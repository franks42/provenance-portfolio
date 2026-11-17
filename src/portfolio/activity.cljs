(ns portfolio.activity
  "Recent on-chain activity of an account, interpreted in plain terms
   (trades on Figure Markets, Prime deposits, commitments, transfers, staking).
   Uses the node's tx search, so it needs an endpoint that keeps tx history.
   Runs unchanged in Scittle and nbb. Read-only."
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [portfolio.chain :as c]))

;; ============================================================ fetching

(defn- search [ctx q limit]
  (p/let [r (c/get-json ctx (str "/cosmos/tx/v1beta1/txs?query=" (js/encodeURIComponent q)
                                 "&limit=" limit "&order_by=ORDER_BY_DESC"))]
    (:tx_responses r)))

(defn queries
  "Tx-search queries that together find the transactions touching addr.
   Provenance's typed exchange events carry JSON-quoted values."
  [addr]
  [(str "transfer.sender='" addr "'")
   (str "transfer.recipient='" addr "'")
   (str "message.sender='" addr "'")
   (str "provenance.exchange.v1.EventFundsCommitted.account='\"" addr "\"'")
   (str "provenance.exchange.v1.EventCommitmentReleased.account='\"" addr "\"'")])

(defn fetch-txs
  "Most recent txs touching addr, newest first. -> {:txs [...] :failed n}"
  [ctx addr limit]
  (p/let [rs (p/all (for [q (queries addr)] (p/catch (search ctx q limit) (fn [_] ::failed))))]
    {:failed (count (filter #(= ::failed %) rs))
     :txs (->> rs
               (remove #(= ::failed %))
               (apply concat)
               (reduce (fn [m t] (assoc m (:txhash t) t)) {})
               vals
               (sort-by #(js/parseInt (:height %)) >)
               (take limit)
               vec)}))

(defn history-start
  "Oldest block the first endpoint still has: {:height :time}, or nil.
   Nodes answer a request for block 1 with \"lowest height is N\"."
  [ctx]
  (let [base (:rest (first (:endpoints ctx)))]
    (-> (js/fetch (str base "/cosmos/base/tendermint/v1beta1/blocks/1"))
        (.then #(.text %))
        (.then (fn [txt]
                 (if-let [[_ h] (re-find #"lowest height is (\d+)" txt)]
                   (p/let [b (c/get-json ctx (str "/cosmos/base/tendermint/v1beta1/blocks/" h))]
                     {:height h :time (get-in b [:block :header :time])})
                   {:height "1"})))
        (.catch (fn [_] nil)))))

;; ============================================================ interpreting

(defn- unq [v] (str/replace (str v) #"^\"|\"$" ""))

(defn parse-coins
  "\"12nhash,5uylds.fcc\" (possibly JSON-quoted) -> {denom BigInt}"
  [s]
  (reduce (fn [m [_ n d]] (c/add-coin m d (js/BigInt n)))
          {}
          (re-seq #"(\d+)([a-zA-Z][a-zA-Z0-9/._:-]*)" (unq s))))

(defn- attrs [e] (into {} (map (fn [a] [(keyword (:key a)) (:value a)])) (:attributes e)))

(defn- msg-type [m] (last (str/split (str (get m (keyword "@type"))) #"\.")))

(defn- all-msgs
  "Messages of a tx, including those wrapped in authz MsgExec."
  [tx]
  (mapcat (fn [m] (cons m (:msgs m))) (get-in tx [:tx :body :messages])))

(defn interpret
  "One tx from addr's point of view.
   pools: [{:pool addr :label}] for naming Prime deposits.
   -> {:height :time :hash :kind ...} with kind one of
      :trade :lend :prime-withdraw :receive :send :commit :release
      :stake :unstake :redelegate :claim :group-proposal :other"
  [addr pools tx]
  (let [events  (:events tx)
        msgs    (all-msgs tx)
        types   (set (map msg-type msgs))
        fee     (str/join "," (map #(str (:amount %) (:denom %)) (get-in tx [:tx :auth_info :fee :amount])))
        xfers   (for [e events :when (= "transfer" (:type e))
                      :let [a (attrs e)]]
                  {:from (:sender a) :to (:recipient a) :amount (:amount a)})
        ;; drop the fee payment (paid by addr) once
        xfers   (let [[a b] (split-with #(not (and (= addr (:from %)) (= fee (:amount %)))) xfers)]
                  (concat a (rest b)))
        ins     (filter #(and (= addr (:to %)) (not= addr (:from %))) xfers)
        outs    (filter #(and (= addr (:from %)) (not= addr (:to %))) xfers)
        sum     (fn [xs] (apply c/merge-coins (map (comp parse-coins :amount) xs)))
        exch    (fn [t] (apply c/merge-coins
                               (for [e events :when (= t (:type e))
                                     :let [a (attrs e)] :when (= addr (unq (:account a)))]
                                 (parse-coins (:amount a)))))
        commits (exch "provenance.exchange.v1.EventFundsCommitted")
        rels    (exch "provenance.exchange.v1.EventCommitmentReleased")
        pool-of (into {} (map (juxt :pool :label)) pools)
        lent    (filter #(pool-of (:to %)) outs)
        back    (filter #(pool-of (:from %)) ins)
        mine    (fn [k] (filter #(= addr (k %)) msgs))
        base    {:height (:height tx) :time (:timestamp tx) :hash (:txhash tx)}]
    (merge
     base
     (cond
       (and (types "MsgMarketCommitmentSettleRequest") (seq ins) (seq outs))
       {:kind :trade :out (sum outs) :in (sum ins)
        :with (vec (distinct (map :to outs)))}

       (seq lent)
       {:kind :lend :coins (sum lent) :pool (pool-of (:to (first lent))) :pool-address (:to (first lent))}

       (seq back)
       {:kind :prime-withdraw :coins (sum back) :pool (pool-of (:from (first back))) :pool-address (:from (first back))}

       (seq (mine :delegator_address))
       (let [m (first (mine :delegator_address)) t (msg-type m)
             amt (when-let [a (:amount m)] (when (:denom a) {(:denom a) (js/BigInt (:amount a))}))]
         (case t
           "MsgDelegate" {:kind :stake :coins amt}
           "MsgUndelegate" {:kind :unstake :coins amt}
           "MsgBeginRedelegate" {:kind :redelegate :coins amt}
           "MsgWithdrawDelegatorReward" {:kind :claim :coins (sum ins)}
           {:kind :other :types (vec (sort types))}))

       (seq (filter #(and (= "MsgSubmitProposal" (msg-type %)) (some #{addr} (:proposers %))) msgs))
       (let [m (first (filter #(= "MsgSubmitProposal" (msg-type %)) msgs))]
         {:kind :group-proposal :group-account (:group_policy_address m)
          :inner (vec (for [im (:messages m)]
                        (cond-> {:type (msg-type im)}
                          (:grantee im) (assoc :grantee (:grantee im)))))})

       (seq ins)
       {:kind :receive :coins (sum ins) :from (vec (distinct (map :from ins)))
        :committed (not-empty commits)}

       (seq outs)
       {:kind :send :coins (sum outs) :to (vec (distinct (map :to outs)))}

       (seq commits) {:kind :commit :coins commits}
       (seq rels)    {:kind :release :coins rels}
       :else         {:kind :other :types (vec (sort types))}))))

(defn account-activity
  "Interpreted recent activity of addr. -> {:events [...] :failed n :denoms #{...}}"
  [ctx addr pools {:keys [limit] :or {limit 15}}]
  (p/let [{:keys [txs failed]} (fetch-txs ctx addr limit)]
    (let [evs (mapv #(interpret addr pools %) txs)]
      {:events evs
       :failed failed
       :denoms (set (mapcat (fn [e] (mapcat keys (keep e [:coins :in :out]))) evs))})))

;; ============================================================ describing

(defn- last4 [a] (if (and a (> (count a) 4)) (str "…" (subs a (- (count a) 4))) (str a)))

(defn coins-str [infos m]
  (str/join ", " (for [[d v] m :let [f (c/fmt-amount infos d v 2)]] (str (:amount f) " " (:symbol f)))))

(defn describe
  "One-line description of an interpreted event. name-fn names known addresses."
  [infos name-fn ev]
  (let [cs #(coins-str infos %)
        nm #(str/join ", " (map name-fn %))]
    (case (:kind ev)
      :trade          (str "traded " (cs (:out ev)) " → " (cs (:in ev)) " on Figure Markets")
      :lend           (str "lent " (cs (:coins ev)) " to " (:pool ev) " " (last4 (:pool-address ev)))
      :prime-withdraw (str "withdrew " (cs (:coins ev)) " from " (:pool ev) " " (last4 (:pool-address ev)))
      :receive        (str "received " (cs (:coins ev)) " from " (nm (:from ev))
                           (when (:committed ev) ", committed to Figure Markets"))
      :send           (str "sent " (cs (:coins ev)) " to " (nm (:to ev)))
      :commit         (str "committed " (cs (:coins ev)) " to Figure Markets")
      :release        (str "released " (cs (:coins ev)) " from Figure Markets")
      :stake          (str "staked " (cs (:coins ev)))
      :unstake        (str "started unstaking " (cs (:coins ev)) " (21 days)")
      :redelegate     (str "moved stake " (cs (:coins ev)) " to another validator")
      :claim          (str "claimed rewards " (cs (:coins ev)))
      :group-proposal (str "proposed for group account " (name-fn (:group-account ev)) ": "
                           (str/join ", " (for [i (:inner ev)]
                                            (str ({"MsgGrant" "authorization grant" "MsgRevoke" "revoke authorization"
                                                   "MsgExec" "execute" "MsgSend" "send"} (:type i) (:type i))
                                                 (when (:grantee i) (str " to " (name-fn (:grantee i))))))))
      (str "other: " (str/join ", " (:types ev))))))
