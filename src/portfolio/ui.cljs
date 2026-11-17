(ns portfolio.ui
  "Browser UI for the read-only Provenance portfolio viewer (Scittle + Reagent)."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [promesa.core :as p]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [portfolio.chain :as c]
            [portfolio.activity :as act]
            [portfolio.tree :as t]))

;; ============================================================ state

(defonce app
  (r/atom {:config nil
           :endpoints []
           :ep-tests {}
           :accounts []          ; [{:address :label}]
           :storage :session     ; :session | :browser | :link
           :include-groups? true
           :loading? false
           :result nil
           :prices nil           ; {:prices {sym {:usd :source :assumed?}} :fetched-at} or {:error}
           :error nil
           :show-settings? false
           :view :tree           ; :tree | :table
           :open #{"pf"}         ; ids of expanded tree rows
           :activity {}          ; addr -> {:status :loading|:ok|:error :events :since}
           :form {:address "" :label ""}}))

(def address-re #"^(pb|tp)1[02-9ac-hj-np-z]{38,58}$")
(def ls-endpoints "pp.endpoints")
(def ls-accounts "pp.accounts")
(def ls-open "pp.open")

;; ============================================================ localStorage (wrapped: it can throw)

(defn ls-get [k] (try (some-> (.getItem js/localStorage k) edn/read-string) (catch :default _ nil)))
(defn ls-set! [k v] (try (.setItem js/localStorage k (pr-str v)) (catch :default _ nil)))
(defn ls-del! [k] (try (.removeItem js/localStorage k) (catch :default _ nil)))

;; ============================================================ URL fragment (#accounts=addr~label,addr~label&groups=1)

(defn parse-hash []
  (let [h (subs (.-hash js/location) 1)
        kv (into {} (for [part (str/split h #"&") :let [[k v] (str/split part #"=" 2)] :when (seq k)] [k v]))]
    (when-let [a (get kv "accounts")]
      {:accounts (vec (for [item (str/split a #",") :when (seq item)
                            :let [[addr lbl] (str/split item #"~" 2)]]
                        {:address (js/decodeURIComponent addr) :label (some-> lbl js/decodeURIComponent)}))
       :include-groups? (not= "0" (get kv "groups"))})))

(defn hash-string [{:keys [accounts include-groups?]}]
  (str "#accounts="
       (str/join "," (for [{:keys [address label]} accounts]
                       (str address (when (seq label) (str "~" (js/encodeURIComponent label))))))
       "&groups=" (if include-groups? "1" "0")))

(defn set-hash! [s]
  (.replaceState js/history nil "" (str (.-pathname js/location) (.-search js/location) s)))

;; ============================================================ persistence of the user's choices

(defn persist-accounts! []
  (let [{:keys [storage accounts include-groups?] :as st} @app]
    (case storage
      :browser (do (ls-set! ls-accounts {:accounts accounts :include-groups? include-groups?})
                   (ls-set! ls-open (:open st)) (set-hash! ""))
      :link    (do (ls-del! ls-accounts) (ls-del! ls-open) (set-hash! (hash-string st)))
      :session (do (ls-del! ls-accounts) (ls-del! ls-open) (set-hash! "")))))

(defn toggle-open!
  "Expand/collapse a tree row. Remembered across visits only in browser storage mode,
   because row ids contain addresses."
  [id]
  (swap! app update :open #(if (contains? % id) (disj % id) (conj % id)))
  (when (= :browser (:storage @app)) (ls-set! ls-open (:open @app))))

(defn persist-endpoints! [] (ls-set! ls-endpoints (:endpoints @app)))

(defn forget-everything! []
  (ls-del! ls-accounts) (ls-del! ls-endpoints) (ls-del! ls-open) (set-hash! "")
  (swap! app assoc :accounts [] :storage :session :result nil :open #{"pf"}
         :endpoints (:endpoints (:config @app)) :ep-tests {}))

;; ============================================================ helpers

(defn last4 [a] (if (and a (> (count a) 4)) (str "…" (subs a (- (count a) 4))) (str a)))

(defn account-name [addr]
  (let [lbl (some #(when (= addr (:address %)) (:label %)) (:accounts @app))]
    (if (seq lbl) (str lbl " " (last4 addr)) (last4 addr))))

(defn known-name
  "Name for an address seen in grants/groups: own account label, Prime pool label, or …last4."
  [addr pools]
  (or (some #(when (= addr (:pool %)) (str "Prime: " (:label %) " " (last4 addr))) pools)
      (account-name addr)))

(defn ctx []
  (let [{:keys [config endpoints]} @app]
    {:endpoints (filterv :enabled? endpoints)
     :prime-code-ids (:prime-code-ids config)}))

(defn pct [s] (when s (str (.toFixed (* 100 (js/parseFloat s)) 2) "%")))

(defn amt
  ([infos denom v] (amt infos denom v 6))
  ([infos denom v dp]
   (let [ibc? (:ibc? (get infos denom))
         f   (c/fmt-amount infos denom v dp)
         ;; a non-zero amount that rounds to 0 is shown with full precision
         f   (if (and (= "0" (:amount f)) (c/big-pos? v)) (c/fmt-amount infos denom v) f)
         {:keys [amount symbol base raw?]} f
         se? (str/ends-with? base ".figure.se")]
     [:span.amt {:title base}
      amount " " [:span.sym symbol]
      (when se? [:span.badge {:title "Security entitlement: a claim held at Figure, not the native coin"} "SE"])
      (when ibc? [:span.badge {:title (str (:name (get infos denom)) ". Decimals inferred from the denom prefix.")} "IBC"])
      (when raw? [:span.badge {:title "No on-chain metadata: shown in raw base units"} "raw"])])))

(defn denom-order [infos ds]
  (sort-by (fn [d] [(if (= d "nhash") 0 1) (str/lower-case (or (:symbol (get infos d)) d))]) ds))

;; ============================================================ USD valuation

(defn usd-str [v18]
  (let [s (c/big->dec v18 18 2)
        [i f] (str/split s #"\." 2)
        s2 (str i "." (subs (str f "00") 0 2))]
    (if (and (= "0" s) (c/big-pos? v18)) "<$0.01" (str "$" (c/group3 s2)))))

(defn price-str [p] (str "$" (c/group3 (c/big->dec (c/dec->big p 18) 18 6))))

(defn usd-of [infos denom v]
  (c/usd-value infos (get-in @app [:prices :prices]) denom v))

(defn usd-sum
  "Sum USD over a coin map. -> {:usd BigInt18 :unpriced #{symbols}}"
  [infos coins]
  (reduce (fn [acc [d v]]
            (if (str/starts-with? d "nft/")
              acc
              (if-let [x (usd-of infos d v)]
                (update acc :usd + x)
                (update acc :unpriced conj (or (:symbol (get infos d)) d)))))
          {:usd c/ZERO :unpriced #{}}
          coins))

;; ============================================================ actions

(defn estimate-missing!
  "Once both the portfolio and Figure prices are in: fetch external estimates for
   held symbols Figure Markets doesn't price, and merge them in marked :estimated?."
  []
  (let [{:keys [result prices config]} @app
        ec (:estimate-prices config)]
    (when (and ec result (:prices prices) (not (:estimated-at prices)))
      (let [syms (:unpriced (usd-sum (:denoms result) (get-in result [:totals :holdings])))]
        (swap! app assoc-in [:prices :estimated-at] :pending)
        (-> (c/fetch-estimates ec syms)
            (p/then (fn [est] (swap! app update :prices
                                     #(-> % (update :prices merge est)
                                          (assoc :estimated-at (.toISOString (js/Date.)))))))
            (p/catch (fn [_] nil)))))))

(defn activity-ctx
  "Endpoints for tx history: the configured ids, in the user's endpoint list;
   falls back to the enabled endpoints."
  []
  (let [ids (get-in @app [:config :activity :endpoints])
        eps (:endpoints @app)
        sel (keep (fn [i] (some #(when (= i (:id %)) %) eps)) ids)]
    {:endpoints (vec (if (seq sel) sel (filter :enabled? eps)))}))

(defn load-activity!
  "Recent transactions of every account in res, in the background."
  [res]
  (let [actx  (activity-ctx)
        limit (or (get-in @app [:config :activity :limit]) 20)
        since (act/history-start actx)]
    (doseq [{:keys [address]} (:reports res)]
      (swap! app assoc-in [:activity address] {:status :loading})
      (-> (p/let [r  (act/account-activity actx address (:pools res) {:limit limit})
                  ;; names/decimals for denoms not in the holdings
                  missing (remove #(contains? (get-in @app [:result :denoms]) %) (:denoms r))
                  ds (when (seq missing) (c/resolve-denoms (ctx) (zipmap missing (repeat 1))))
                  s  since]
            (when (seq ds) (swap! app update-in [:result :denoms] merge ds))
            (swap! app assoc-in [:activity address] (assoc r :status :ok :since s)))
          (p/catch (fn [e] (swap! app assoc-in [:activity address] {:status :error :error (ex-message e)})))))))

(defn load-portfolio! []
  (let [{:keys [accounts include-groups?]} @app]
    (when (seq accounts)
      (swap! app assoc :loading? true :error nil)
      (when-let [pc (get-in @app [:config :prices])]
        (-> (c/fetch-prices pc)
            (p/then #(do (swap! app assoc :prices %) (estimate-missing!)))
            (p/catch #(swap! app assoc :prices {:error (ex-message %)}))))
      (-> (c/portfolio (ctx) (mapv :address accounts) {:include-groups? include-groups?})
          (p/then (fn [res] (swap! app assoc :result res :loading? false :activity {})
                    (estimate-missing!) (load-activity! res)))
          (p/catch (fn [e] (swap! app assoc :error (ex-message e) :loading? false)))))))

(defn refresh! [] (c/reset-caches!) (load-portfolio!))

(defn add-account! []
  (let [{:keys [address label]} (:form @app)
        address (str/trim address)]
    (cond
      (not (re-matches address-re address))
      (swap! app assoc-in [:form :error] "Not a Provenance address (pb1…).")

      (some #(= address (:address %)) (:accounts @app))
      (swap! app assoc-in [:form :error] "Already in the list.")

      :else
      (do (swap! app #(-> % (update :accounts conj {:address address :label (str/trim label)})
                          (assoc :form {:address "" :label ""})))
          (persist-accounts!)))))

(defn remove-account! [addr]
  (swap! app update :accounts (fn [as] (vec (remove #(= addr (:address %)) as))))
  (persist-accounts!))

(defn set-storage! [mode] (swap! app assoc :storage mode) (persist-accounts!))

(defn test-endpoint! [ep]
  (swap! app assoc-in [:ep-tests (:id ep)] {:running? true})
  (p/then (c/test-endpoint ep (get-in @app [:config :probe-pool]))
          #(swap! app assoc-in [:ep-tests (:id ep)] %)))

(defn update-endpoints! [f] (swap! app update :endpoints f) (persist-endpoints!))

(defn move-endpoint! [i delta]
  (update-endpoints!
   (fn [eps]
     (let [j (+ i delta)]
       (if (and (>= j 0) (< j (count eps)))
         (assoc eps i (eps j) j (eps i))
         eps)))))

;; ============================================================ settings panel

(defn endpoint-row [i ep]
  (let [t (get-in @app [:ep-tests (:id ep)])]
    [:tr
     [:td [:input {:type "checkbox" :checked (boolean (:enabled? ep))
                   :on-change #(update-endpoints! (fn [eps] (update-in eps [i :enabled?] not)))}]]
     [:td [:b (:label ep)] [:div.muted (:rest ep)] (when (:notes ep) [:div.note (:notes ep)])]
     [:td.nowrap
      [:button.small {:on-click #(move-endpoint! i -1)} "↑"]
      [:button.small {:on-click #(move-endpoint! i 1)} "↓"]
      (when (:custom? ep)
        [:button.small {:on-click #(update-endpoints! (fn [eps] (vec (concat (subvec eps 0 i) (subvec eps (inc i))))))} "✕"])]
     [:td [:button.small {:on-click #(test-endpoint! ep)} "Test"]]
     [:td.small
      (cond
        (nil? t) [:span.muted "—"]
        (:running? t) "testing…"
        (:ok? t) [:span
                  [:span.ok "✓ reachable"] " " (:version t) ", block " (:height t) ", " (:ms t) " ms"
                  [:br]
                  (case (:height-pin t)
                    :honored [:span.ok "height pinning honored"]
                    :ignored [:span.warn "ignores height pinning (latest only)"]
                    :unsupported [:span.warn "height pinning not allowed (CORS)"]
                    [:span.muted "height pinning not tested"])
                  (when (not= (:chain-id t) (get-in @app [:config :chain-id]))
                    [:div.bad "wrong chain: " (:chain-id t)])]
        :else [:span.bad "✗ " (:error t)])]]))

(defn settings-panel []
  (let [{:keys [endpoints]} @app
        custom (r/atom {:label "" :rest ""})]
    (fn []
      [:section.card
       [:h3 "Data sources"]
       [:p.muted "Queried in order; the next enabled endpoint is used if one fails. "
        "Endpoints must allow browser (CORS) access. All run the same Provenance software; "
        "they differ in history retention and limits. \"Test\" runs from your browser."]
       [:table.eps
        [:tbody (doall (map-indexed (fn [i ep] ^{:key (str (:id ep))} [endpoint-row i ep]) (:endpoints @app)))]]
       [:div.row
        [:input {:placeholder "Label" :value (:label @custom) :on-change #(swap! custom assoc :label (.. % -target -value))}]
        [:input.wide {:placeholder "https://… REST endpoint" :value (:rest @custom)
                      :on-change #(swap! custom assoc :rest (.. % -target -value))}]
        [:button {:on-click (fn []
                              (let [{:keys [label rest]} @custom]
                                (when (str/starts-with? rest "https://")
                                  (update-endpoints! #(conj % {:id (keyword (str "custom-" (js/Date.now)))
                                                               :label (if (seq label) label rest)
                                                               :rest (str/replace rest #"/+$" "")
                                                               :enabled? true :custom? true}))
                                  (reset! custom {:label "" :rest ""}))))}
         "Add endpoint"]]
       [:div.row
        [:button.small {:on-click (fn [] (ls-del! ls-endpoints)
                                    (swap! app assoc :endpoints (:endpoints (:config @app)) :ep-tests {}))}
         "Reset to defaults"]]])))

;; ============================================================ accounts panel

(defn accounts-panel []
  (let [{:keys [accounts form storage include-groups? loading?]} @app]
    [:section.card
     [:h3 "Accounts"]
     (if (empty? accounts)
       [:p.muted "Add one or more Provenance addresses (pb1…). Public addresses only: no keys are ever needed or asked for."]
       [:ul.accounts
        (for [{:keys [address label]} accounts]
          ^{:key address}
          [:li [:b (if (seq label) label "(no label)")] " " [:code (last4 address)]
           [:button.small.link {:on-click #(remove-account! address)} "remove"]])])
     [:div.row
      [:input.wide {:placeholder "pb1… address" :value (:address form)
                    :on-change #(swap! app assoc-in [:form :address] (.. % -target -value))}]
      [:input {:placeholder "Label (optional)" :value (:label form)
               :on-change #(swap! app assoc-in [:form :label] (.. % -target -value))
               :on-key-down #(when (= "Enter" (.-key %)) (add-account!))}]
      [:button {:on-click add-account!} "Add"]]
     (when-let [e (:error form)] [:div.bad e])
     [:label.row [:input {:type "checkbox" :checked include-groups?
                          :on-change (fn [_] (swap! app update :include-groups? not) (persist-accounts!))}]
      " Also find group accounts these addresses control (e.g. Figure Markets accounts)"]

     [:fieldset
      [:legend "Remember these addresses?"]
      [:label [:input {:type "radio" :checked (= storage :session) :on-change #(set-storage! :session)}]
       " Only for this tab: forgotten when you close it"]
      [:label [:input {:type "radio" :checked (= storage :browser) :on-change #(set-storage! :browser)}]
       " On this browser (localStorage)"]
      [:label [:input {:type "radio" :checked (= storage :link) :on-change #(set-storage! :link)}]
       " In this page's link: bookmark or share it (the part after # is never sent to any server)"]
      (when (= storage :browser)
        [:p.note "Note: on a shared host such as username.github.io, every page on that host can read this browser storage."])
      [:button.small {:on-click forget-everything!} "Forget everything on this browser"]]

     [:div.row
      [:button.primary {:disabled (or loading? (empty? accounts)) :on-click refresh!}
       (if loading? "Loading…" (if (:result @app) "Refresh" "Load portfolio"))]
      [:button.small {:on-click #(swap! app update :show-settings? not)}
       (if (:show-settings? @app) "Hide data sources" "Data sources…")]]]))

;; ============================================================ portfolio views

(defn coins-table [infos rep]
  (let [{:keys [total spendable held staked unbonding rewards]} rep
        ds (->> (concat (keys total) (keys held) (keys spendable))
                distinct
                (remove #(str/starts-with? % "nft/"))
                (denom-order infos))
        cell (fn [m d] (let [v (get m d)] (if (and v (c/big-pos? v)) [amt infos d v] [:span.muted "—"])))]
    (when (seq ds)
      [:table.coins
       [:thead [:tr [:th "Asset"] [:th "Total"] [:th "USD"] [:th {:title "Freely transferable: not vesting-locked and not held/committed"} "Unrestricted"] [:th "Held/committed"] [:th "Staked"] [:th "Unbonding"] [:th "Rewards"]]]
       [:tbody
        (for [d ds]
          ^{:key d}
          [:tr [:td (or (:symbol (get infos d)) d)]
           [:td (cell total d)]
           [:td (if-let [x (some->> (get total d) (usd-of infos d))] (usd-str x) [:span.muted "—"])]
           [:td (cell spendable d)] [:td (cell held d)]
           [:td (cell (:total staked) d)] [:td (cell (:total unbonding) d)] [:td (cell rewards d)]])]])))

(defn vesting-box [infos v]
  [:div.sub
   [:h4 "Vesting (" (:type v) ")"]
   [:p (subs (or (:start v) "?") 0 10) " → " (subs (or (:end v) "?") 0 10)]
   [:table.kv
    [:tbody
     (for [[k lbl] [[:original "Original grant"] [:still-vesting "Still vesting (unspendable)"]
                    [:delegated-vesting "Delegated while vesting"] [:locked-in-bank "Locked in bank"]]]
       ^{:key k}
       [:tr [:td lbl] [:td (if (seq (get v k))
                             (for [[d x] (get v k)] ^{:key d} [:div [amt infos d x 0]])
                             [:span.muted "none"])]])]]
   [:p.note "Unvested tokens stay locked even after undelegating (21-day unbonding)."]])

(defn prime-table [infos positions]
  [:div.sub
   [:h4 "Democratized Prime lending"]
   [:table.coins
    [:thead [:tr [:th "Pool"] [:th "Receipt shares"] [:th "Value"] [:th "Lender rate"] [:th "Utilization"]]]
    [:tbody
     (for [x positions]
       ^{:key (:pool x)}
       [:tr [:td (:label x) [:div.muted (last4 (:pool x))]]
        [:td (c/group3 (c/big->dec (:shares x) 6 2)) " " (:token-symbol x)]
        [:td [amt infos (:underlying x) (:value x) 2]]
        [:td (pct (:lender-rate x))]
        [:td (let [u (js/parseFloat (:utilization x)) k (js/parseFloat (or (:kink x) "1"))]
               [:span {:class (when (> u k) "warn")
                       :title (when (> u k) "Above the rate-model kink: withdrawals limited to unborrowed liquidity")}
                (pct (:utilization x))])]])]]
   [:p.note "Value = shares × the pool's live liquidity index. Interest accrues in the index; "
    "losses beyond collateral become a lender-borne deficit."]])

(defn control-box [rep pools]
  (let [{:keys [grants group]} rep]
    (when (or (seq grants) group)
      [:div.sub
       [:h4 "Who else can act on this account"]
       (when group
         [:p "Group account (group " (:group-id group) ", threshold " (:threshold group) "). Members: "
          (interpose ", " (for [m (:members group)]
                            ^{:key (:address m)} [:span (known-name (:address m) pools) " (weight " (:weight m) ")"]))])
       (when (seq grants)
         [:table.coins
          [:thead [:tr [:th "Grantee"] [:th "Permission"] [:th "Limit"] [:th "Expires"]]]
          [:tbody
           (for [[i g] (map-indexed vector grants)]
             ^{:key i}
             [:tr [:td (known-name (:grantee g) pools)]
              [:td (last (str/split (str (:msg g)) #"\.")) [:div.muted (:type g)]]
              [:td (if (seq (:limit g))
                     (for [[d x] (:limit g)] ^{:key d} [:div [amt (get-in @app [:result :denoms]) d x]])
                     "—")]
              [:td (or (some-> (:expiration g) (subs 0 10)) [:span.warn "never"])]])]])])))

(defn account-card [infos pools rep]
  (let [{:keys [address kind discovered-via vesting prime errors attributes unbonding total]} rep
        nfts (count (filter #(str/starts-with? % "nft/") (keys total)))]
    [:section.card
     [:h3 (account-name address) " "
      [:span.badge (case kind
                     :key "key account" :group-policy "group account"
                     (:vesting-continuous :vesting-delayed :vesting-periodic) "vesting account"
                     :unused "unused" (name kind))]]
     (let [{h :usd} (usd-sum infos total)
           {p :usd} (usd-sum infos (:prime-total rep))]
       (when (get-in @app [:prices :prices])
         [:p "Value: " [:b (usd-str (+ h p))]
          (when (c/big-pos? p) [:span.muted " (incl. " (usd-str p) " lent in Prime)"])]))
     (when discovered-via
       [:p.muted "Found via group " (:group-id discovered-via) " (member " (last4 (:member discovered-via)) ")"])
     [coins-table infos rep]
     (when (seq (:entries unbonding))
       [:p.small "Unbonding: "
        (interpose "; " (for [e (:entries unbonding)]
                          ^{:key (:completes e)} [:span [amt infos (:denom unbonding) (:amount e) 0] " on " (subs (:completes e) 0 10)]))])
     (when vesting [vesting-box infos vesting])
     (when (seq prime) [prime-table infos prime])
     [control-box rep pools]
     (when (or (seq attributes) (pos? nfts))
       [:details [:summary "Attributes & records"]
        [:p.small (str/join ", " attributes)]
        (when (pos? nfts) [:p.small nfts " metadata scope record(s), e.g. KYC passport"])])
     (when (seq errors)
       [:div.bad "Some sections failed and are excluded from totals: "
        (str/join "; " (map (fn [[k e]] (str (name k) ": " e)) errors))])]))

(defn price-notes [pr unpriced]
  [:div
   (for [{:keys [symbol market fixed deviation alarm?]} (:pegs pr) :when alarm?]
     ^{:key symbol}
     [:p.bad "⚠ " symbol " trades at $" market " on Figure Markets, " (.toFixed (* 100 deviation) 2)
      "% away from its $" fixed " value. Something may be wrong; values here still use $" fixed "."])
   (cond
     (:error pr) [:p.warn "Prices unavailable: " (:error pr)]
     (:prices pr) [:p.note "Prices: Figure Markets last trades, " (subs (:fetched-at pr) 0 16) " UTC. "
                   (when (seq unpriced) (str "No price (not included in USD): " (str/join ", " (sort unpriced)) ". "))
                   (let [est (sort (keep (fn [[s v]] (when (:estimated? v) (str s " $" (:usd v)))) (:prices pr)))]
                     (when (seq est)
                       (str "Estimated from " (get-in @app [:config :estimate-prices :source]) " (no Figure market): "
                            (str/join ", " est) ". ")))
                   "SE tokens are valued only if Figure still trades them."])
   [:p.note "Not visible on chain: claims kept in Figure's own books (e.g. auction-rate/margin pool shares), "
    "and the backing of SE tokens."]])

;; ============================================================ tree view

(defn tree-cx [res]
  {:infos (:denoms res)
   :prices (get-in @app [:prices :prices])
   :classes (get-in @app [:config :asset-classes])
   :pools (:pools res)
   :activity (:activity @app)
   :name-fn account-name})

(defn tree-label [pools node]
  [:span
   (:label node)
   (when (= :group-policy (:kind node)) [:span.badge "group"])
   (when (#{:vesting-continuous :vesting-delayed :vesting-periodic} (:kind node)) [:span.badge "vesting"])
   (when-let [m (:controlled-by node)] [:span.badge {:title "Found because this account is a member of its group"} "via " (last4 m)])
   (when (seq (:errors node))
     [:span.badge.bad {:title (str/join "; " (map (fn [[k e]] (str (name k) ": " e)) (:errors node)))}
      "partial data"])])

(defn tree-node [infos pools rep-by-addr node depth]
  (let [open?  (contains? (:open @app) (:id node))
        kids   (:children node)
        can?   (or (seq kids) (:content node))
        [d v]  (:amount node)]
    [:<>
     [:div.trow {:class [(when (:overlay? node) "overlay") (when (zero? depth) "root") (when (:warn? node) "warn") (when can? "can")]
                 :style {:padding-left (str (+ 0.4 (* 1.3 depth)) "em")}
                 :on-click (when can? #(toggle-open! (:id node)))}
      [:span.tlabel
       [:span.caret (cond (not can?) "" open? "▾" :else "▸")]
       [tree-label pools node]
       (when (:note node) [:span.tnote (:note node)])]
      [:span.tamt (cond d [amt infos d v 2]
                        (:amount-text node) [:span.amt (:amount-text node) " " [:span.sym (:amount-sym node)]])]
      [:span.tusd (when (and d (get-in @app [:prices :prices (:symbol (get infos d)) :estimated?]))
                    [:span.badge {:title (get-in @app [:prices :prices (:symbol (get infos d)) :source])} "est."])
                  (cond (:usd node) (usd-str (:usd node))
                        (and d (not (:overlay? node))) [:span.muted "no market"]
                        :else "")]]
     (when open?
       (for [ch kids] ^{:key (:id ch)} [tree-node infos pools rep-by-addr ch (inc depth)]))
     (when (and open? (= :access (:content node)))
       [:div.tcontent {:style {:margin-left (str (+ 1.7 (* 1.3 depth)) "em")}}
        [control-box (get rep-by-addr (:address node)) pools]])]))

(defn tree-card [res]
  (let [tree  (t/portfolio-tree (tree-cx res) res)
        infos (:denoms res)
        reps  (into {} (map (juxt :address identity) (:reports res)))
        {unpriced :unpriced} (usd-sum infos (get-in res [:totals :holdings]))]
    [:section.card.tree
     [tree-node infos (:pools res) reps tree 0]
     [price-notes (:prices @app) unpriced]]))

(defn totals-card [res]
  (let [infos (:denoms res)
        {:keys [holdings prime]} (:totals res)
        ds (denom-order infos (remove #(str/starts-with? % "nft/") (keys holdings)))
        pr (:prices @app)
        {hu :usd unpriced :unpriced} (usd-sum infos holdings)
        {pu :usd} (usd-sum infos prime)]
    [:section.card.totals
     [:h3 "Totals"]
     (when (:prices pr)
       [:p.big "≈ " [:b (usd-str (+ hu pu))]
        [:span.muted " (holdings " (usd-str hu) (when (c/big-pos? pu) (str " + Prime " (usd-str pu))) ")"]])
     [:table.coins
      [:thead [:tr [:th "Asset"] [:th "Amount"] [:th "Price"] [:th "USD"]]]
      [:tbody
       (for [d ds :let [v (get holdings d) sym (or (:symbol (get infos d)) d)
                        px (get-in pr [:prices sym]) x (usd-of infos d v)]]
         ^{:key d}
         [:tr [:td sym] [:td [amt infos d v 2]]
          [:td (if px [:span {:title (:source px)} (price-str (:usd px)) (when (:assumed? px) [:span.badge "assumed"]) (when (:estimated? px) [:span.badge "est."])] [:span.muted "no market"])]
          [:td (if x (usd-str x) [:span.muted "—"])]])
       (for [[d x] prime]
         ^{:key (str "prime-" d)}
         [:tr [:td "Lent in Prime"] [:td [amt infos d x 2]] [:td ""] [:td (or (some->> (usd-of infos d x) usd-str) "—")]])]]
     [price-notes pr unpriced]]))

(defn portfolio-view []
  (let [{:keys [result error loading?]} @app]
    [:div
     (when error [:section.card [:div.bad error]])
     (when result
       (let [infos (:denoms result)
             pools (:pools result)]
         [:div
          [:p.muted "Block " (get-in result [:head :height]) " · " (some-> (get-in result [:head :time]) (subs 0 19) (str/replace "T" " ")) " UTC · "
           (get-in result [:head :chain-id]) " · data unverified (REST, from " (str/join ", " (map :label (:endpoints (ctx)))) ")"
           (when (pos? (:hidden-empty result)) (str " · " (:hidden-empty result) " empty group account(s) hidden"))]
          [:div.row
           [:button.small {:class (when (= :tree (:view @app)) "on") :on-click #(swap! app assoc :view :tree)} "Tree view"]
           [:button.small {:class (when (= :table (:view @app)) "on") :on-click #(swap! app assoc :view :table)} "Table view"]]
          (if (= :table (:view @app))
            [:div
             [totals-card result]
             (for [rep (:reports result)] ^{:key (:address rep)} [account-card infos pools rep])]
            [tree-card result])]))
     (when loading? [:p.muted "Loading…"])]))

(defn root []
  [:div.wrap
   [:header [:h1 "Provenance portfolio"] [:p.muted "Read-only view of public chain data. No keys, no signing, no server."]]
   (if-not (:config @app)
     [:p "Loading configuration…"]
     [:div
      [accounts-panel]
      (when (:show-settings? @app) [settings-panel])
      [portfolio-view]])])

;; ============================================================ boot

(defn init! []
  (rdom/render [root] (js/document.getElementById "app"))
  (-> (js/fetch "endpoints.edn" (clj->js {:cache "no-cache"}))
      (.then #(.text %))
      (.then (fn [txt]
               (let [cfg (edn/read-string txt)
                     saved-eps (ls-get ls-endpoints)
                     from-hash (parse-hash)
                     from-ls (ls-get ls-accounts)]
                 (swap! app merge
                        {:config cfg
                         :endpoints (vec (or saved-eps (:endpoints cfg)))}
                        (cond
                          from-hash {:accounts (:accounts from-hash) :include-groups? (:include-groups? from-hash) :storage :link}
                          from-ls   {:accounts (vec (:accounts from-ls)) :include-groups? (:include-groups? from-ls true) :storage :browser
                                     :open (set (or (ls-get ls-open) #{"pf"}))}
                          :else     {}))
                 (when (seq (:accounts @app)) (load-portfolio!)))))
      (.catch (fn [e] (swap! app assoc :error (str "Could not load endpoints.edn: " e) :config {:endpoints []})))))

(init!)
