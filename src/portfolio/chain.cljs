(ns portfolio.chain
  "Read-only Provenance Blockchain portfolio introspection.

   Runs unchanged in Scittle (browser) and nbb (Node): only js/fetch and
   promesa are used. Takes bech32 addresses only. There is no signing code and
   nothing is ever broadcast.

   Amounts are BigInt in base units. Under SCI, + - * / < = work on BigInt,
   but quot, max, compare and zero? do NOT, so use the helpers below."
  (:require [clojure.string :as str]
            [promesa.core :as p]))

;; ============================================================ BigInt helpers

(def ZERO (js/BigInt 0))
(def ONE (js/BigInt 1))
(defn big-zero? [b] (= b ZERO))
(defn big-pos? [b] (< ZERO b))
(defn big-max [a b] (if (< a b) b a))

(defn pow10 [n]
  (loop [r ONE i 0] (if (< i n) (recur (* r (js/BigInt 10)) (inc i)) r)))

(defn dec->big
  "Decimal string (e.g. \"123.456\") -> BigInt scaled by 10^scale, truncated."
  [s scale]
  (let [s   (str (or s "0"))
        neg (str/starts-with? s "-")
        s   (if neg (subs s 1) s)
        [i f] (str/split s #"\." 2)
        f   (subs (str f (apply str (repeat scale "0"))) 0 scale)
        v   (js/BigInt (str (if (str/blank? i) "0" i) f))]
    (if neg (- ZERO v) v)))

(defn big->dec
  "BigInt carrying `scale` implied decimals -> decimal string.
   Trailing zeros trimmed; optional max-dp truncates the fraction."
  ([b scale] (big->dec b scale nil))
  ([b scale max-dp]
   (let [neg (< b ZERO)
         b   (if neg (- ZERO b) b)
         s   (.toString b)
         s   (if (<= (count s) scale)
               (str (apply str (repeat (- (inc scale) (count s)) "0")) s)
               s)
         ip  (subs s 0 (- (count s) scale))
         fp  (subs s (- (count s) scale))
         fp  (if max-dp (subs fp 0 (min max-dp (count fp))) fp)
         fp  (str/replace fp #"0+$" "")]
     (str (when neg "-") ip (when (seq fp) (str "." fp))))))

(defn group3
  "Thousands separators on the integer part of a decimal string."
  [dec-str]
  (let [[i f] (str/split dec-str #"\." 2)
        neg (str/starts-with? i "-")
        i   (if neg (subs i 1) i)
        g   (->> (reverse i) (partition-all 3) (map #(apply str (reverse %))) reverse (str/join ","))]
    (str (when neg "-") g (when f (str "." f)))))

;; coin maps: {denom BigInt}
(defn add-coin [m denom amt] (update m denom #(+ (or % ZERO) amt)))
(defn coins->map
  "Cosmos coins (or DecCoins) -> {denom BigInt}; DecCoins are truncated to base units."
  [coins]
  (reduce (fn [m {:keys [denom amount]}] (add-coin m denom (dec->big amount 0))) {} coins))
(defn merge-coins [& ms] (apply merge-with + {} ms))
(defn sub-coins [a b] (merge-with - a b))
(defn non-zero [m] (into {} (remove (fn [[_ v]] (big-zero? v))) m))

;; ============================================================ HTTP layer

(def max-in-flight 8)
(defonce ^:private limiter (atom {:active 0 :queue []}))

(defn- run-next! []
  (let [{:keys [active queue]} @limiter]
    (when (and (< active max-in-flight) (seq queue))
      (swap! limiter (fn [s] (-> s (update :active inc) (update :queue subvec 1))))
      ((first queue))
      (recur))))

(defn- limited
  "Run thunk (returning a JS promise) with at most max-in-flight concurrent."
  [thunk]
  (js/Promise.
   (fn [resolve reject]
     (swap! limiter update :queue conj
            (fn []
              (-> (thunk)
                  (.then resolve reject)
                  (.finally (fn [] (swap! limiter update :active dec) (run-next!))))))
     (run-next!))))

(defn- error-response
  "Non-200 body -> ::none (allowed not-found) or throw ex-info with :retry?.
   Cosmos gRPC-gateway reports some not-found cases as HTTP 500
   (e.g. \"codespace sdk code 38: not found: group policy\")."
  [st path t allow-404]
  (let [not-found? (boolean (re-find #"(?i)not found" t))
        msg        (-> t (str/replace #"<[^>]*>" " ") (str/replace #"\s+" " ") str/trim)]
    (if (and allow-404 not-found?)
      ::none
      (throw (ex-info (str "HTTP " st " " path ": " (subs msg 0 (min 160 (count msg))))
                      {:retry? (and (not not-found?) (or (= 429 st) (<= 500 st)))})))))

(defn- fetch-once
  "One GET against one endpoint. Resolves to parsed JSON (keywordized), or ::none
   for an allowed 404/not-found. Rejects with ex-data {:retry? bool}."
  [base path headers allow-404]
  (limited
   (fn []
     (-> (js/fetch (str base path) (clj->js {:headers (merge {"Accept" "application/json"} headers)}))
         (.then (fn [resp]
                  (let [st (.-status resp)]
                    (cond
                      (= 200 st) (.then (.json resp) #(js->clj % :keywordize-keys true))
                      (and allow-404 (= 404 st)) ::none
                      :else (.then (.text resp) #(error-response st path % allow-404))))))))))

(defn- sleep [ms] (js/Promise. (fn [resolve] (js/setTimeout resolve ms))))

(defn get-json
  "GET path from ctx's endpoints in order. A 429 is retried on the same endpoint
   with backoff (up to 3 times); network errors and 5xx fall through to the next
   endpoint; other 4xx are final. Returns nil for an allowed 404/not-found."
  ([ctx path] (get-json ctx path {}))
  ([ctx path {:keys [allow-404 headers]}]
   (letfn [(attempt [eps tries errs]
             (if-let [ep (first eps)]
               (-> (fetch-once (:rest ep) path headers allow-404)
                   (p/then #(if (= ::none %) nil %))
                   (p/catch (fn [e]
                              (cond
                                (and (str/includes? (str (ex-message e)) "HTTP 429") (< tries 3))
                                (p/then (sleep (* 1500 (inc tries))) (fn [_] (attempt eps (inc tries) errs)))

                                (false? (:retry? (ex-data e)))
                                (p/rejected e)

                                :else
                                (attempt (rest eps) 0 (conj errs (str (:id ep) ": " (ex-message e))))))))
               (p/rejected (ex-info (str "All endpoints failed: " (str/join " | " errs)) {:retry? false}))))]
     (attempt (:endpoints ctx) 0 []))))

(defn get-all
  "Follow Cosmos pagination; concat the k collection."
  [ctx path k]
  (let [sep (if (str/includes? path "?") "&" "?")]
    (letfn [(step [acc next-key]
              (p/let [r   (get-json ctx (str path sep "pagination.limit=200"
                                             (when next-key (str "&pagination.key=" (js/encodeURIComponent next-key)))))
                      acc (into acc (get r k))
                      nk  (get-in r [:pagination :next_key])]
                (if (str/blank? nk) acc (step acc nk))))]
      (step [] nil))))

(defn smart
  "CosmWasm smart query (read-only). Returns :data."
  [ctx contract msg]
  (p/let [b64 (js/btoa (js/JSON.stringify (clj->js msg)))
          r   (get-json ctx (str "/cosmwasm/wasm/v1/contract/" contract "/smart/" (js/encodeURIComponent b64)))]
    (:data r)))

;; ============================================================ denominations

(def fallback-denoms {"nhash" {:symbol "HASH" :exponent 9 :name "Hash"}})
(defonce ^:private denom-cache (atom {}))

(defn denom-info
  "Display info for a denom from on-chain metadata: {:symbol :exponent :name :description}.
   IBC denoms resolve to their trace (no exponent known). Unknown -> nil."
  [ctx denom]
  (or (get @denom-cache denom)
      (let [pr (cond
                 (fallback-denoms denom)
                 (p/resolved (fallback-denoms denom))

                 (str/starts-with? denom "ibc/")
                 (p/let [r (get-json ctx (str "/ibc/apps/transfer/v1/denoms/" (subs denom 4)) {:allow-404 true})
                         d (:denom r)
                         ;; Chain at the other end of the channel this voucher arrived on.
                         hop (first (:trace d))
                         from (when hop
                                (p/catch
                                 (p/let [c (get-json ctx (str "/ibc/core/channel/v1/channels/" (:channel_id hop)
                                                              "/ports/" (:port_id hop) "/client_state"))]
                                   (get-in c [:identified_client_state :client_state :chain_id]))
                                 (fn [_] nil)))]
                   (when d
                     ;; No on-chain decimals for IBC vouchers; use the Cosmos base-denom
                     ;; prefix convention (u = 10^-6, n = 10^-9, a = 10^-18).
                     (let [base (str (:base d))
                           [exp sym] (if-let [[_ pfx s] (re-matches #"^([una])([a-z][a-z0-9]*)$" base)]
                                       [({"u" 6 "n" 9 "a" 18} pfx) (str/upper-case s)]
                                       [nil base])]
                       {:symbol sym :exponent exp :ibc? true
                        :ibc-from from :ibc-hops (count (:trace d))
                        :name (str base " via IBC "
                                   (str/join " <- " (map #(str (:port_id %) "/" (:channel_id %)) (:trace d)))
                                   (when from (str " from " from)))})))

                 (str/includes? denom "/") (p/resolved nil)

                 :else
                 (p/let [r  (get-json ctx (str "/cosmos/bank/v1beta1/denoms_metadata/" denom) {:allow-404 true})
                         md (:metadata r)
                         u  (some #(when (= (:display md) (:denom %)) %) (:denom_units md))]
                   (when md
                     {:symbol (if (str/blank? (:symbol md)) (:display md) (:symbol md))
                      :exponent (or (:exponent u) 0)
                      :name (:name md)
                      :description (:description md)})))
            pr (p/catch pr (fn [_] (swap! denom-cache dissoc denom) nil))]
        (swap! denom-cache assoc denom pr)
        pr)))

(defn resolve-denoms
  "Fetch display info for every denom in the given coin maps. -> {denom info}"
  [ctx & coin-maps]
  (let [ds (distinct (mapcat keys coin-maps))]
    (p/let [infos (p/all (map #(denom-info ctx %) ds))]
      (zipmap ds infos))))

(defn fmt-amount
  "Format base-unit BigInt using denom info. -> {:amount \"1,234.5\" :symbol \"HASH\" :base denom}"
  [infos denom amt & [max-dp]]
  (let [{:keys [symbol exponent]} (get infos denom)]
    (if exponent
      {:amount (group3 (big->dec amt exponent (or max-dp exponent))) :symbol symbol :base denom}
      {:amount (group3 (.toString amt)) :symbol (or symbol denom) :base denom :raw? true})))

;; ============================================================ account structure

(defn- type-of [m] (str (get m (keyword "@type"))))

(defn account-kind [acct]
  (let [t   (type-of acct)
        pk  (or (:pub_key acct) (get-in acct [:base_vesting_account :base_account :pub_key]))
        pkt (type-of pk)]
    (cond
      (nil? acct)                                  :unused
      (str/includes? t "ContinuousVestingAccount") :vesting-continuous
      (str/includes? t "DelayedVestingAccount")    :vesting-delayed
      (str/includes? t "PeriodicVestingAccount")   :vesting-periodic
      (str/includes? t "MarkerAccount")            :marker
      (str/includes? pkt "ModuleCredential")       (if (= "group" (:module_name pk)) :group-policy :module)
      (str/includes? pkt "secp256k1")              :key
      (str/includes? pkt "ed25519")                :key
      :else                                        :no-pubkey)))

(defn now-s [] (js/Math.floor (/ (js/Date.now) 1000)))

(defn still-vesting
  "Coins not yet vested at now (unix seconds), Cosmos SDK semantics. nil if not a vesting account."
  [acct now]
  (let [t    (type-of acct)
        bva  (:base_vesting_account acct)
        orig (coins->map (:original_vesting bva))
        end  (some-> (:end_time bva) js/parseInt)
        start (some-> (:start_time acct) js/parseInt)]
    (cond
      (str/includes? t "DelayedVestingAccount") (if (< now end) orig {})
      (str/includes? t "ContinuousVestingAccount")
      (cond (<= now start) orig
            (>= now end) {}
            :else (let [num (js/BigInt (- now start)) den (js/BigInt (- end start))]
                    (into {} (map (fn [[d o]] [d (- o (/ (* o num) den))])) orig)))
      (str/includes? t "PeriodicVestingAccount")
      (let [vested (loop [t start ps (:vesting_periods acct) m {}]
                     (if-let [prd (first ps)]
                       (let [t2 (+ t (js/parseInt (:length prd)))]
                         (if (<= t2 now) (recur t2 (rest ps) (merge-coins m (coins->map (:amount prd)))) m))
                       m))]
        (non-zero (sub-coins orig vested)))
      :else nil)))

(defn vesting-summary [acct]
  (when-let [sv (still-vesting acct (now-s))]
    (let [bva (:base_vesting_account acct)
          dv  (coins->map (:delegated_vesting bva))]
      {:type      (last (str/split (type-of acct) #"\."))
       :start     (some-> (:start_time acct) js/parseInt (* 1000) js/Date. .toISOString)
       :end       (some-> (:end_time bva) js/parseInt (* 1000) js/Date. .toISOString)
       :original  (coins->map (:original_vesting bva))
       :still-vesting sv
       :delegated-vesting dv
       ;; Cosmos SDK: locked in bank = max(still-vesting - delegated-vesting, 0)
       :locked-in-bank (non-zero (into {} (map (fn [[d v]] [d (big-max ZERO (- v (get dv d ZERO)))])) sv))})))

;; ============================================================ groups

(defn group-policy-info
  "If addr is an x/group policy account: {:group-id :threshold :members [{:address :weight}]}."
  [ctx addr]
  (p/let [r (get-json ctx (str "/cosmos/group/v1/group_policy_info/" addr) {:allow-404 true})]
    (when-let [info (:info r)]
      (p/let [ms (get-all ctx (str "/cosmos/group/v1/group_members/" (:group_id info)) :members)]
        {:group-id  (:group_id info)
         :threshold (get-in info [:decision_policy :threshold])
         :policy-type (last (str/split (type-of (:decision_policy info)) #"\."))
         :members   (mapv (fn [m] {:address (get-in m [:member :address])
                                   :weight  (get-in m [:member :weight])})
                          ms)}))))

(defn controlled-group-accounts
  "Group policy accounts where addr is a member. -> [{:address :group-id :alone?}]
   :alone? is true when addr's weight alone meets the threshold."
  [ctx addr]
  (p/let [groups (get-all ctx (str "/cosmos/group/v1/groups_by_member/" addr) :groups)
          per    (p/all
                  (for [g groups]
                    (p/let [ms   (get-all ctx (str "/cosmos/group/v1/group_members/" (:id g)) :members)
                            pols (get-all ctx (str "/cosmos/group/v1/group_policies_by_group/" (:id g)) :group_policies)
                            w    (some #(when (= addr (get-in % [:member :address])) (js/parseFloat (get-in % [:member :weight]))) ms)]
                      (for [pl pols]
                        (let [thr (some-> (get-in pl [:decision_policy :threshold]) js/parseFloat)]
                          {:address (:address pl) :group-id (:id g)
                           :members (count ms)
                           :alone?  (boolean (and w thr (>= w thr)))})))))]
    (vec (apply concat per))))

;; ============================================================ Democratized Prime

(defonce ^:private prime-cache (atom nil))

(defn prime-pools
  "Discover lending pools by CosmWasm code id (config :prime-code-ids) with live
   reserve data. Cached for the session; call (reset-caches!) to refresh."
  [ctx]
  (or @prime-cache
      (let [pr (p/let [addrs (p/all (map #(get-all ctx (str "/cosmwasm/wasm/v1/code/" % "/contracts") :contracts)
                                         (:prime-code-ids ctx)))
                       pools (p/all
                              (for [a (apply concat addrs)]
                                (-> (p/let [info (get-json ctx (str "/cosmwasm/wasm/v1/contract/" a))
                                            st   (smart ctx a {:get_state {}})
                                            rs   (smart ctx a {:get_reserve {}})
                                            tok  (get-in st [:contract :atca])
                                            ti   (when tok (smart ctx tok {:token_info {}}))]
                                      {:pool a
                                       :label (get-in info [:contract_info :label])
                                       :token tok
                                       :token-symbol (:symbol ti)
                                       :underlying (get-in st [:contract :ld :n])
                                       :liquidity-index (get-in rs [:reserve :liquidity_index])
                                       :lender-rate (:current_lender_rate rs)
                                       :borrower-rate (:current_borrower_rate rs)
                                       :utilization (:utilization rs)
                                       :kink (get-in st [:contract :rp :kink])
                                       :deficit (get-in rs [:reserve :deficit_underlying])
                                       :bad-debt (get-in st [:contract :bdla])})
                                    (p/catch (fn [e] {:pool a :error (ex-message e)})))))]
                 (vec pools))
            pr (p/catch pr (fn [e] (reset! prime-cache nil) (p/rejected e)))]
        (reset! prime-cache pr)
        pr)))

(defn prime-positions
  "Receipt-share balances of addr in each pool, valued at the live liquidity index.
   -> [{pool fields... :shares BigInt :value BigInt (underlying base units)}]"
  [ctx addr]
  (p/let [pools (prime-pools ctx)
          rows  (p/all
                 (for [pl pools :when (:token pl)]
                   (p/let [b (smart ctx (:token pl) {:balance {:address addr}})]
                     (let [shares (js/BigInt (or (:balance b) "0"))]
                       (when (big-pos? shares)
                         (assoc pl :shares shares
                                   :value (/ (* shares (dec->big (:liquidity-index pl) 18)) (pow10 18))))))))]
    (vec (remove nil? rows))))

;; ============================================================ per-address report

(defn- settle
  "Never reject: -> {:ok v} or {:error msg}."
  [pr]
  (-> pr (p/then (fn [v] {:ok v})) (p/catch (fn [e] {:error (ex-message e)}))))

(defonce ^:private bond-denom (atom nil))
(defn- get-bond-denom [ctx]
  (or @bond-denom
      (let [pr (p/let [r (get-json ctx "/cosmos/staking/v1beta1/params")] (get-in r [:params :bond_denom]))]
        (reset! bond-denom pr) pr)))

(defn address-report
  "Full read-only report for one address. Each section settles independently;
   failed sections appear under :errors and are excluded from :total."
  [ctx addr]
  (p/let [sections
          (p/all
           (map (fn [[k pr]] (p/then (settle pr) (fn [r] [k r])))
                {:account     (p/let [r (get-json ctx (str "/cosmos/auth/v1beta1/accounts/" addr) {:allow-404 true})] (:account r))
                 :bank        (p/let [b (get-all ctx (str "/cosmos/bank/v1beta1/balances/" addr) :balances)] (coins->map b))
                 :spendable   (p/let [b (get-all ctx (str "/cosmos/bank/v1beta1/spendable_balances/" addr) :balances)] (coins->map b))
                 :held        (p/let [r (get-json ctx (str "/provenance/hold/v1/funds/" addr) {:allow-404 true})] (coins->map (:amount r)))
                 :committed   (p/let [r (get-json ctx (str "/provenance/exchange/v1/commitments/account/" addr) {:allow-404 true})]
                                (mapv (fn [c] {:market-id (:market_id c) :coins (coins->map (:amount c))}) (:commitments r)))
                 :staked      (p/let [ds (get-all ctx (str "/cosmos/staking/v1beta1/delegations/" addr) :delegation_responses)]
                                {:total (coins->map (map :balance ds))
                                 :by-validator (mapv (fn [d] {:validator (get-in d [:delegation :validator_address])
                                                              :coins (coins->map [(:balance d)])}) ds)})
                 :unbonding   (p/let [d  (get-bond-denom ctx)
                                      us (get-all ctx (str "/cosmos/staking/v1beta1/delegators/" addr "/unbonding_delegations") :unbonding_responses)
                                      es (for [u us e (:entries u)] {:validator (:validator_address u) :completes (:completion_time e) :amount (js/BigInt (:balance e))})]
                                {:total (reduce (fn [m e] (add-coin m d (:amount e))) {} es)
                                 :entries (vec (sort-by :completes es)) :denom d})
                 :rewards     (p/let [r (get-json ctx (str "/cosmos/distribution/v1beta1/delegators/" addr "/rewards"))] (coins->map (:total r)))
                 :grants      (p/let [gs (get-all ctx (str "/cosmos/authz/v1beta1/grants/granter/" addr) :grants)]
                                (mapv (fn [g] {:grantee (:grantee g)
                                               :type (last (str/split (type-of (:authorization g)) #"\."))
                                               :msg (or (get-in g [:authorization :msg]) (type-of (:authorization g)))
                                               :limit (some-> (get-in g [:authorization :transfer_limit]) coins->map)
                                               :expiration (:expiration g)})
                                      gs))
                 :group       (group-policy-info ctx addr)
                 :attributes  (p/let [r (get-json ctx (str "/provenance/attribute/v1/attributes/" addr) {:allow-404 true})]
                                (vec (distinct (map :name (:attributes r)))))
                 :prime       (prime-positions ctx addr)}))
          s      (into {} sections)
          ok     (fn [k] (:ok (s k)))
          acct   (ok :account)
          errors (into {} (keep (fn [[k v]] (when-let [e (:error v)] [k e]))) s)
          total  (merge-coins (or (ok :bank) {})
                              (or (:total (ok :staked)) {})
                              (or (:total (ok :unbonding)) {})
                              (or (ok :rewards) {}))]
    {:address addr
     :kind (account-kind acct)
     :account-number (or (:account_number acct) (get-in acct [:base_vesting_account :base_account :account_number]))
     :bank (ok :bank) :spendable (ok :spendable) :held (ok :held) :committed (ok :committed)
     :staked (ok :staked) :unbonding (ok :unbonding) :rewards (ok :rewards)
     :vesting (when acct (vesting-summary acct))
     :grants (ok :grants) :group (ok :group) :attributes (ok :attributes)
     :prime (ok :prime)
     :prime-total (reduce (fn [m pos] (add-coin m (:underlying pos) (:value pos))) {} (ok :prime))
     :total (non-zero total)
     :errors errors}))

(defn empty-report?
  "No assets besides metadata scope NFTs (e.g. KYC passport records) and no Prime positions."
  [r]
  (and (empty? (remove (fn [[d _]] (str/starts-with? d "nft/")) (non-zero (or (:total r) {}))))
       (empty? (:prime r))))

;; ============================================================ whole portfolio

(defn chain-head [ctx]
  (p/let [r (get-json ctx "/cosmos/base/tendermint/v1beta1/blocks/latest")]
    {:height (get-in r [:block :header :height])
     :time   (get-in r [:block :header :time])
     :chain-id (get-in r [:block :header :chain_id])}))

(defn portfolio
  "Reports for the given addresses. With :include-groups? also discovers group
   policy accounts the addresses are members of and keeps the non-empty ones.
   -> {:head :reports [...] :denoms {denom info} :totals {:holdings :prime}}"
  [ctx addrs {:keys [include-groups?]}]
  (p/let [head     (chain-head ctx)
          pools    (p/catch (prime-pools ctx) (fn [_] []))
          direct   (p/all (map #(address-report ctx %) addrs))
          found    (if include-groups?
                     (p/all (map #(settle (controlled-group-accounts ctx %)) addrs))
                     [])
          known    (set addrs)
          cands    (->> (map vector addrs found)
                        (mapcat (fn [[a r]] (map #(assoc % :member a) (:ok r))))
                        (remove #(known (:address %)))
                        (reduce (fn [m c] (if (m (:address c)) m (assoc m (:address c) c))) {})
                        vals)
          extra    (p/all (map (fn [c] (p/then (address-report ctx (:address c)) #(assoc % :discovered-via c))) cands))
          reports  (vec (concat direct (remove empty-report? extra)))
          denoms   (apply resolve-denoms ctx
                          (concat (map :total reports) (map :held reports) (map :prime-total reports)
                                  (keep (comp :still-vesting :vesting) reports)))]
    {:head head
     :pools pools
     :reports reports
     :hidden-empty (count (filter empty-report? extra))
     :denoms denoms
     :totals {:holdings (apply merge-coins (map :total reports))
              :prime (apply merge-coins (map :prime-total reports))}}))

(defn reset-caches! []
  (reset! prime-cache nil) (reset! bond-denom nil) (reset! denom-cache {}))

;; ============================================================ USD prices (Figure Markets)

(defn fetch-prices
  "Prices in USD by asset symbol from the configured markets API.
   -> {:prices {sym {:usd \"0.008\" :source \"HASH-USD\"}} :pegs [...] :fetched-at iso}
   X-USD markets price directly; X-YLDS markets convert via the YLDS price
   (fixed if configured). :fixed-usd overrides markets; :peg-check reports how far
   the market price of a fixed symbol is from its fixed value."
  [{:keys [url field fixed-usd assume-usd peg-check peg-tolerance]}]
  (-> (js/fetch url (clj->js {:headers {"Accept" "application/json"}}))
      (.then (fn [resp]
               (if (= 200 (.-status resp))
                 (.json resp)
                 (throw (ex-info (str "Price API HTTP " (.-status resp)) {})))))
      (.then
       (fn [j]
         (let [ms       (:data (js->clj j :keywordize-keys true))
               field    (or field :lastTradedPrice)
               px       (fn [m] (some-> (get m field) str))
               quote-of (fn [m] (last (str/split (str (:displayName m)) #"-")))
               market   (into {} (for [m ms :when (and (= "USD" (quote-of m)) (px m))]
                                   [(:denom m) {:usd (px m) :source (:displayName m)}]))
               ylds     (or (get fixed-usd "YLDS") (get-in market ["YLDS" :usd]))
               via-y    (when ylds
                          (into {} (for [m ms :when (and (= "YLDS" (quote-of m)) (px m) (not (market (:denom m))))]
                                     [(:denom m) {:usd (big->dec (/ (* (dec->big (px m) 18) (dec->big ylds 18)) (pow10 18)) 18)
                                                  :source (str (:displayName m) " × YLDS $" (big->dec (dec->big ylds 18) 18))}])))
               fixed    (into {} (for [[s v] fixed-usd] [s {:usd v :source "fixed"}]))
               pegs     (vec (for [s peg-check
                                   :let [mk (get-in market [s :usd]) fx (get fixed-usd s)]
                                   :when (and mk fx)]
                               (let [dev (js/Math.abs (- (/ (js/parseFloat mk) (js/parseFloat fx)) 1))]
                                 {:symbol s :fixed fx :market mk :deviation dev
                                  :alarm? (> dev (or peg-tolerance 0.005))})))]
           {:prices (merge (into {} (for [s assume-usd] [s {:usd "1" :source "assumed $1" :assumed? true}]))
                           market via-y fixed)
            :pegs pegs
            :fetched-at (.toISOString (js/Date.))})))))

(defn usd-value
  "USD value (BigInt with 18 implied decimals) of a base-unit amount, or nil if unpriced."
  [infos prices denom amt]
  (let [{:keys [symbol exponent]} (get infos denom)
        p (get-in prices [symbol :usd])]
    (when (and p exponent)
      (/ (* amt (dec->big p 18)) (pow10 exponent)))))

(defn fetch-estimates
  "USD estimates from an external spot-price API, for symbols Figure Markets
   doesn't price. :url contains {SYM}; :path is where the price sits in the JSON.
   -> {sym {:usd :source :estimated? true}}; symbols that fail are left out."
  [{:keys [url path source]} syms]
  (p/let [rs (p/all
              (for [s syms]
                (-> (js/fetch (str/replace url "{SYM}" (js/encodeURIComponent s)))
                    (.then (fn [r] (when (= 200 (.-status r)) (.json r))))
                    (.then (fn [j]
                             (when-let [a (some-> j (js->clj :keywordize-keys true) (get-in path))]
                               [s {:usd (str a) :source (str source ", " s "-USD") :estimated? true}])))
                    (.catch (fn [_] nil)))))]
    (into {} (remove nil? rs))))

;; ============================================================ endpoint self-test

(defn test-endpoint
  "Check one endpoint from wherever this runs (in a browser this also proves CORS).
   -> {:ok? :chain-id :version :height :ms :height-pin} where :height-pin is
   :honored, :ignored or :unsupported (CORS/preflight refused the header)."
  [ep probe-pool]
  (let [t0  (js/Date.now)
        ctx {:endpoints [ep]}]
    (-> (p/let [head (chain-head ctx)
                ni   (get-json ctx "/cosmos/base/tendermint/v1beta1/node_info")
                ms   (- (js/Date.now) t0)
                pin  (if-not probe-pool
                       :untested
                       (-> (p/let [old-h (str (- (js/parseInt (:height head)) 200))
                                   path  (str "/cosmwasm/wasm/v1/contract/" probe-pool "/smart/"
                                              (js/encodeURIComponent (js/btoa "{\"get_reserve\":{}}")))
                                   now   (get-json ctx path {:headers {"x-cosmos-block-height" (:height head)}})
                                   old   (get-json ctx path {:headers {"x-cosmos-block-height" old-h}})]
                             (if (= (get-in now [:data :reserve :liquidity_index])
                                    (get-in old [:data :reserve :liquidity_index]))
                               :ignored :honored))
                           (p/catch (fn [_] :unsupported))))]
          {:ok? true :chain-id (:chain-id head) :height (:height head) :ms ms
           :version (get-in ni [:application_version :version]) :height-pin pin})
        (p/catch (fn [e] {:ok? false :error (ex-message e) :ms (- (js/Date.now) t0)})))))
