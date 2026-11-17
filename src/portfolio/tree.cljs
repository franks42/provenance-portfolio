(ns portfolio.tree
  "Turns portfolio reports into an indented tree of nodes for the UI.
   Pure data, no rendering: runs unchanged in Scittle and nbb.

   A node is a map:
     :id        stable string, used for expand/collapse state
     :label     text
     :amount    [denom BigInt] (optional)
     :usd       BigInt with 18 implied decimals, or nil
     :note      short muted text (optional)
     :overlay?  true for views that restate other numbers (vesting, free to move);
                they are never added into a parent's value
     :children  vector of nodes (optional)
     :content   keyword for a special panel the UI renders when open (optional)"
  (:require [clojure.string :as str]
            [portfolio.chain :as c]
            [portfolio.activity :as act]))

(def class-order
  [[:stocks     "Stocks"]
   [:stable     "Stable / fiat"]
   [:crypto     "Tradable crypto"]
   [:untradable "Untradable crypto"]])

(defn- last4 [a] (if (and a (> (count a) 4)) (str "…" (subs a (- (count a) 4))) (str a)))

(defn- nft? [d] (str/starts-with? d "nft/"))

(defn- big-min [a b] (if (< a b) a b))
(defn- big-abs [a] (if (< a c/ZERO) (- c/ZERO a) a))

(defn- usd [{:keys [infos prices]} d v]
  (when (and v (c/big-pos? v)) (c/usd-value infos prices d v)))

(defn- sum-usd
  "Sum of the children's :usd, ignoring overlays. nil when nothing is priced."
  [nodes]
  (let [xs (keep :usd (remove :overlay? nodes))]
    (when (seq xs) (reduce + c/ZERO xs))))

(defn- by-usd
  "Comparator: highest USD first, unpriced last, otherwise keep order."
  [a b]
  (let [x (:usd a) y (:usd b)]
    (cond (and x y) (cond (> x y) -1 (< x y) 1 :else 0)
          x -1
          y 1
          :else 0)))

(defn sort-nodes [nodes] (vec (sort by-usd nodes)))

(defn- line [cx id label d v & [opts]]
  (when (and v (c/big-pos? v))
    (merge {:id id :label label :amount [d v] :usd (usd cx d v)} opts)))

(defn- group [id label children & [opts]]
  (let [ch (vec (remove nil? children))]
    (when (seq ch)
      (merge {:id id :label label :children ch :usd (sum-usd ch)} opts))))

(defn classify
  "Asset class of a denom: configured stock/stable symbols first, otherwise
   tradable when Figure Markets has a price for it, untradable when not."
  [{:keys [infos prices classes]} d]
  (let [sym (or (:symbol (get infos d)) d)]
    (cond (contains? (set (:stocks classes)) sym) :stocks
          (contains? (set (:stable classes)) sym) :stable
          (let [px (get prices sym)] (and px (not (:estimated? px)))) :crypto
          :else :untradable)))

;; ============================================================ one asset in one account

(defn- vesting-node [cx id d v]
  (let [orig (get (:original v) d c/ZERO)
        sv   (get (:still-vesting v) d c/ZERO)
        end  (some-> (:end v) (subs 0 10))
        dur  (when (and (:start v) (:end v) (str/includes? (str (:type v)) "Continuous"))
               (js/Math.round (/ (- (js/Date.parse (:end v)) (js/Date.parse (:start v))) 1000)))]
    (when (c/big-pos? orig)
      (group id "vesting schedule"
             [(line cx (str id "/orig") "original grant" d orig)
              (line cx (str id "/vested") "vested so far" d (- orig sv))
              (line cx (str id "/unvested") "unvested (locked)" d sv {:note (when end (str "until " end))})
              (when (and dur (pos? dur))
                (line cx (str id "/day") "vests per day" d (/ (* orig (js/BigInt 86400)) (js/BigInt dur))))]
             {:overlay? true :usd nil :note (str (:type v) (when end (str ", ends " end)))}))))

(defn asset-node
  "One denom in one account report, broken down by where it sits."
  [cx rep d id]
  (let [g      (fn [m] (get m d c/ZERO))
        total  (g (:total rep))
        bank   (g (:bank rep))
        sp     (g (:spendable rep))
        held   (g (:held rep))
        comm   (reduce + c/ZERO (keep #(get (:coins %) d) (:committed rep)))
        staked (g (:total (:staked rep)))
        unb    (g (:total (:unbonding rep)))
        rew    (g (:rewards rep))
        v      (:vesting rep)
        sv     (get (:still-vesting v) d c/ZERO)
        locked (get (:locked-in-bank v) d c/ZERO)
        resid  (- bank sp locked held)
        ;; The SDK applies the vesting lock to delegated tokens first.
        st-unv (big-min sv staked)
        staked-node
        (if (c/big-pos? st-unv)
          (group (str id "/staked") "staked"
                 [(line cx (str id "/staked/unv") "unvested (locked)" d st-unv)
                  (line cx (str id "/staked/v") "vested" d (- staked st-unv)
                        {:note "free once unbonded (21 days)"})]
                 {:amount [d staked]})
          (line cx (str id "/staked") "staked" d staked))
        unb-node
        (when (and (c/big-pos? unb) (= d (:denom (:unbonding rep))))
          (group (str id "/unb") "unbonding"
                 (for [[i e] (map-indexed vector (:entries (:unbonding rep)))]
                   (line cx (str id "/unb/" i) (str "completes " (subs (str (:completes e)) 0 10)) d (:amount e)))
                 {:amount [d unb]}))
        delegated
        (group (str id "/del") "delegated"
               [staked-node unb-node
                (line cx (str id "/rew") "rewards (unclaimed)" d rew {:note "not locked; claim to move"})])
        wallet
        (group (str id "/wallet") "in wallet"
               [(line cx (str id "/avail") "available" d sp {:note "can be sent or traded now"})
                (line cx (str id "/locked") "locked (unvested)" d locked)
                (line cx (str id "/held") "held" d (c/big-max c/ZERO (- held comm)))
                (line cx (str id "/comm") "committed to Figure Markets" d comm)
                (when-not (c/big-zero? resid)
                  (line cx (str id "/resid") "unreconciled" d (big-abs resid)
                        {:warn? true :note "wallet − available − locked − held ≠ 0; please report"}))]
               {:amount [d bank]})
        ;; Committed funds and unclaimed rewards can also be moved, each after one
        ;; extra step. Only worth a separate view when there are at least two sources.
        free-sum (+ sp comm rew)
        free
        (when (< 1 (count (filter c/big-pos? [sp comm rew])))
          (group (str id "/free") "free to move"
                 [(line cx (str id "/free/now") "available now" d sp)
                  (line cx (str id "/free/comm") "+ committed" d comm
                        {:note "release from Figure Markets first; cancels open orders on these funds (orders are kept off-chain by Figure)"})
                  (line cx (str id "/free/rew") "+ rewards" d rew {:note "claim first"})
                  (line cx (str id "/free/sum") "= free after these steps" d free-sum)]
                 {:overlay? true :amount [d free-sum] :usd (usd cx d free-sum)}))
        vest   (when v (vesting-node cx (str id "/vest") d v))
        kids   (vec (remove nil? [delegated wallet free vest]))
        ;; Nothing worth expanding: the whole amount sits in one wallet state.
        only     (when (and (= kids [wallet]) (= 1 (count (:children wallet))))
                   (first (:children wallet)))]
    (cond-> {:id id
             :label (or (:symbol (get (:infos cx) d)) d)
             :amount [d total]
             :usd (usd cx d total)}
      (nil? only) (assoc :children kids)
      ;; "available" is the plain case and needs no note
      (and only (not= (str id "/avail") (:id only)))
      (assoc :note (str "all " (:label only)) :warn? (:warn? only)))))

;; ============================================================ one account

(defn variant-label
  "How one form of an asset differs from others with the same symbol."
  [infos d]
  (let [i (get infos d)]
    (cond (:ibc? i) (str "via IBC" (when-let [f (:ibc-from i)] (str " from " f))
                         (when (> (or (:ibc-hops i) 0) 1) " (multi-hop)"))
          (str/ends-with? d ".figure.se") "Figure entitlement (SE)"
          (str/ends-with? d ".trading") "Figure Markets cash"
          :else (str (or (:symbol i) d) " (" d ")"))))

(defn- merge-same-symbol
  "Nodes for the same symbol in different forms (e.g. USDC via IBC and as a
   Figure entitlement) go under one parent row with the combined value."
  [{:keys [infos classes]} id pairs]
  (vec (for [[sym ps] (group-by (fn [[d n]] (get (:group-as classes) (:label n) (:label n))) pairs)]
         (if (= 1 (count ps))
           (second (first ps))
           (let [kids (sort-nodes (for [[d n] ps] (assoc n :label (variant-label infos d))))
                 ;; amounts only add up when every form has the same symbol and decimals
                 exps (set (map (fn [[d _]] [(:symbol (get infos d)) (:exponent (get infos d))]) ps))
                 tot  (reduce + c/ZERO (map (fn [[_ n]] (second (:amount n))) ps))]
             (cond-> {:id (str id "/sym/" sym) :label sym :children kids :usd (sum-usd kids)
                      :note (str (count ps) " forms")}
               (and (= 1 (count exps)) (second (first exps)))
               (assoc :amount-text (:amount (c/fmt-amount infos (ffirst ps) tot 2)) :amount-sym sym)))))))

(defn- category-nodes
  "Asset-class groups, given [denom node] pairs."
  [cx id pairs]
  (let [by (group-by (fn [[d _]] (classify cx d)) pairs)]
    (vec (for [[k lbl] class-order
               :let [ns (merge-same-symbol cx (str id "/" (name k)) (get by k))]
               :when (seq ns)]
           {:id (str id "/" (name k)) :label lbl
            :children (sort-nodes ns) :usd (sum-usd ns)
            :note (when (= k :untradable)
                    (if (some :usd ns)
                      "no market on Figure; valued at an external spot price (estimate)"
                      "no market on Figure; not in totals"))}))))

(defn- pct [s] (when s (str (.toFixed (* 100 (js/parseFloat s)) 2) "%")))

(defn- prime-note [x]
  (str "lender rate " (:lender-rate-pct x) " · utilization " (:utilization-pct x)
       (when (:above-kink? x)
         (str " · above " (str/replace (pct (or (:kink x) "1")) #"\.00%" "%") " kink: high rate, limited withdrawal liquidity"))))

(defn- decorate-prime [x]
  (assoc x
         :lender-rate-pct (pct (:lender-rate x))
         :utilization-pct (pct (:utilization x))
         :above-kink? (> (js/parseFloat (:utilization x)) (js/parseFloat (or (:kink x) "1")))))

(defn- prime-node [cx id positions]
  (group id "Prime lending"
         (sort-nodes
          (for [x positions]
            (line cx (str id "/" (:pool x)) (str (:label x) " " (last4 (:pool x)))
                  (:underlying x) (:value x)
                  {:note (prime-note x)
                   :warn? (:above-kink? x)})))))

(defn- activity-node
  "Recent transactions of an account, from cx :activity {addr {:status ...}}."
  [cx id addr]
  (when-let [{:keys [status events since error failed]} (get (:activity cx) addr)]
    (let [pools   (into {} (map (juxt :pool :label)) (:pools cx))
          name-fn (fn [a] (if-let [l (pools a)] (str l " " (last4 a)) ((:name-fn cx) a)))
          since-s (when-let [t (:time since)] (str "node history since " (subs t 0 10)))
          base    {:id id :label "activity" :overlay? true}]
      (case status
        :loading (assoc base :note "loading…")
        :error   (assoc base :note (str "unavailable: " error) :warn? true)
        (if (empty? events)
          (assoc base :note (str/join " · " (remove nil? ["no transactions found" since-s])))
          (assoc base
                 :note (str/join " · " (remove nil? [(str (count events) " most recent transactions") since-s
                                                     (when (pos? (or failed 0)) (str failed " searches failed"))]))
                 :children (vec (map-indexed
                                 (fn [i e]
                                   {:id (str id "/" i) :overlay? true :event e
                                    :label (str (subs (str (:time e)) 0 10) " · " (act/describe (:infos cx) name-fn e))})
                                 events))))))))

(defn account-node [cx rep]
  (let [a     (:address rep)
        id    (str "acct/" a)
        ds    (remove nft? (keys (c/non-zero (or (:total rep) {}))))
        cats  (category-nodes cx id (for [d ds] [d (asset-node cx rep d (str id "/" d))]))
        prime (prime-node cx (str id "/prime") (map decorate-prime (:prime rep)))
        kids  (sort-nodes (remove nil? (conj cats prime)))
        access (when (or (seq (:grants rep)) (:group rep))
                 {:id (str id "/access") :label "access & permissions" :overlay? true
                  :content :access :address a})
        actv   (activity-node cx (str id "/activity") a)]
    {:id id
     :label ((:name-fn cx) a)
     :address a
     :kind (:kind rep)
     :controlled-by (some-> rep :discovered-via :member)
     :errors (:errors rep)
     :usd (sum-usd kids)
     :children (cond-> kids access (conj access) actv (conj actv))}))

;; ============================================================ all accounts

(defn all-accounts-node [cx reports]
  (let [id     "all"
        name-fn (:name-fn cx)
        ds     (distinct (remove nft? (mapcat (comp keys c/non-zero #(or % {}) :total) reports)))
        assets (for [d ds]
                 (let [per (sort-nodes
                            (for [r reports :let [v (get (:total r) d)] :when (and v (c/big-pos? v))]
                              (line cx (str id "/" d "/" (:address r)) (name-fn (:address r)) d v)))
                       tot (reduce + c/ZERO (map (comp second :amount) per))]
                   [d {:id (str id "/" d)
                       :label (or (:symbol (get (:infos cx) d)) d)
                       :amount [d tot] :usd (usd cx d tot)
                       :children (when (> (count per) 1) per)}]))
        cats   (category-nodes cx id assets)
        pools  (group-by :pool (for [r reports x (:prime r)] (assoc x :holder (:address r))))
        prime  (group (str id "/prime") "Prime lending"
                      (sort-nodes
                       (for [[pool xs] pools]
                         (let [d   (:underlying (first xs))
                               tot (reduce + c/ZERO (map :value xs))
                               x   (decorate-prime (first xs))
                               per (sort-nodes (for [y xs] (line cx (str id "/prime/" pool "/" (:holder y)) (name-fn (:holder y)) d (:value y))))]
                           (line cx (str id "/prime/" pool) (str (:label x) " " (last4 pool)) d tot
                                 {:note (prime-note x)
                                  :warn? (:above-kink? x)
                                  :children (when (> (count per) 1) per)})))))
        kids   (sort-nodes (remove nil? (conj cats prime)))]
    {:id id :label "All accounts" :usd (sum-usd kids) :children kids :overlay? true}))

;; ============================================================ whole portfolio

(defn portfolio-tree
  "cx: {:infos denom-info :prices {sym {:usd ..}} :classes {:stocks #{} :stable #{}} :name-fn (fn [addr] label)}"
  [cx result]
  (let [reports (:reports result)
        accts   (sort-nodes (map #(account-node cx %) reports))
        usd-tot (sum-usd accts)]
    {:id "pf" :label "Portfolio" :usd usd-tot
     :children (conj accts (all-accounts-node cx reports))}))
