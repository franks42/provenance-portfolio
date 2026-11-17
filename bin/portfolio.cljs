;; Command-line / agent access to the same read-only portfolio view as the web page.
;; Reuses src/portfolio/chain.cljs and tree.cljs unchanged (they run under nbb).
;;
;;   npx nbb -cp src bin/portfolio.cljs [options] ADDRESS[~label] ...
;;
;; Options:
;;   --format tree|json|edn   output (default tree: indented text)
;;   --no-groups              don't look for group accounts the addresses control
;;   --no-prices              skip USD valuation
;;   --config PATH            endpoints file (default: endpoints.edn next to src/)
;;   --help
;;
;; Read-only: plain HTTP GETs and CosmWasm smart queries. No keys, no signing.
(ns portfolio-cli
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [promesa.core :as p]
            [portfolio.chain :as c]
            [portfolio.tree :as t]))

(def config (atom nil))

(def usage
  (str "Usage: npx nbb -cp src bin/portfolio.cljs [--format tree|json|edn] [--no-groups] [--no-prices]\n"
       "                                          [--config endpoints.edn] ADDRESS[~label] ...\n"))

;; ============================================================ args & config

(def address-re #"^(pb|tp)1[02-9ac-hj-np-z]{38,58}$")

(defn parse-args [args]
  (loop [[a & more :as as] args
         o {:format "tree" :groups? true :prices? true :accounts []}]
    (cond
      (empty? as) o
      (= a "--help") (assoc o :help? true)
      (= a "--format") (recur (rest more) (assoc o :format (first more)))
      (= a "--config") (recur (rest more) (assoc o :config (first more)))
      (= a "--no-groups") (recur more (assoc o :groups? false))
      (= a "--no-prices") (recur more (assoc o :prices? false))
      (str/starts-with? a "--") (assoc o :error (str "Unknown option " a))
      :else (let [[addr lbl] (str/split a #"~" 2)]
              (if (re-matches address-re addr)
                (recur more (update o :accounts conj {:address addr :label lbl}))
                (assoc o :error (str "Not a Provenance address: " addr)))))))

(def script-file *file*)

(defn default-config-path []
  ;; bin/portfolio.cljs -> repo root
  (path/join (path/dirname (path/dirname (path/resolve script-file))) "endpoints.edn"))

(defn load-config [p]
  (edn/read-string (fs/readFileSync p "utf8")))

;; ============================================================ helpers

(defn last4 [a] (str "…" (subs a (- (count a) 4))))

(defn usd-str [v18]
  (let [s (c/big->dec v18 18 2)
        [i f] (str/split s #"\." 2)]
    (if (and (= "0" s) (c/big-pos? v18))
      "<$0.01"
      (str "$" (c/group3 (str i "." (subs (str f "00") 0 2)))))))

(defn unpriced-symbols [infos prices holdings]
  (set (for [[d v] holdings
             :when (and (not (str/starts-with? d "nft/")) (c/big-pos? v)
                        (nil? (c/usd-value infos prices d v)))]
         (or (:symbol (get infos d)) d))))

;; ============================================================ load

(defn load-all [cfg {:keys [accounts groups? prices?]}]
  (let [ctx {:endpoints (filterv :enabled? (:endpoints cfg))
             :prime-code-ids (:prime-code-ids cfg)}]
    (p/let [res    (c/portfolio ctx (mapv :address accounts) {:include-groups? groups?})
            pr     (when prices?
                     (p/catch (c/fetch-prices (:prices cfg)) (fn [e] {:error (ex-message e)})))
            miss   (when (:prices pr)
                     (unpriced-symbols (:denoms res) (:prices pr) (get-in res [:totals :holdings])))
            est    (if (and (seq miss) (:estimate-prices cfg))
                     (p/catch (c/fetch-estimates (:estimate-prices cfg) miss) (fn [_] {}))
                     {})]
      {:ctx ctx :result res
       :prices (cond-> pr (:prices pr) (update :prices merge est))})))

;; ============================================================ output

(defn amount-map [infos [d v]]
  (let [{:keys [symbol exponent]} (get infos d)]
    {:denom d
     :symbol (or symbol d)
     :amount (if exponent (c/big->dec v exponent) (str v))
     :base-units (str v)}))

(defn node->data
  "Tree node as plain data: BigInts become decimal strings, report details are
   attached to accounts (kind, grants, group) for agents that need them."
  [infos prices reps node]
  (let [[d _ :as a] (:amount node)
        sym (when d (:symbol (get infos d)))
        px  (when sym (get prices sym))
        rep (when (str/starts-with? (str (:id node)) "acct/") (get reps (:address node)))]
    (cond-> {:label (:label node)}
      a                    (assoc :amount (amount-map infos a))
      (:amount-text node)  (assoc :amount {:symbol (:amount-sym node) :amount (:amount-text node)})
      (:usd node)          (assoc :usd (c/big->dec (:usd node) 18 2))
      (and a (nil? (:usd node)) (not (:overlay? node))) (assoc :usd nil :unpriced true)
      (:estimated? px)     (assoc :price-estimated-from (:source px))
      (:assumed? px)       (assoc :price-assumed true)
      (:note node)         (assoc :note (:note node))
      (:overlay? node)     (assoc :overlay true)
      (:warn? node)        (assoc :warning true)
      (:address node)      (assoc :address (:address node))
      (and rep (= (:id node) (str "acct/" (:address node))))
      (assoc :kind (some-> (:kind rep) name)
             :controlled-by (:controlled-by node)
             :errors (not-empty (:errors rep)))
      (= :access (:content node))
      (assoc :grants (mapv (fn [g] {:grantee (:grantee g) :type (:type g) :msg (str (:msg g))
                                    :expiration (:expiration g)})
                           (:grants (get reps (:address node))))
             :group (:group (get reps (:address node))))
      (seq (:children node))
      (assoc :children (mapv #(node->data infos prices reps %) (:children node))))))

(defn tree-text [infos prices node depth]
  (let [[d v] (:amount node)
        sym (when d (:symbol (get infos d)))
        amt (cond d (let [f (c/fmt-amount infos d v 2)] (str (:amount f) " " (:symbol f)))
                  (:amount-text node) (str (:amount-text node) " " (:amount-sym node)))
        usd (cond (:usd node) (str (usd-str (:usd node))
                                   (when (get-in prices [sym :estimated?]) " est.")
                                   (when (get-in prices [sym :assumed?]) " assumed"))
                  (and d (not (:overlay? node))) "no market")
        line (str (apply str (repeat depth "  "))
                  (when (:warn? node) "! ")
                  (:label node)
                  (when amt (str "  " amt))
                  (when usd (str "  " usd))
                  (when (:note node) (str "  — " (:note node))))]
    (cons line (mapcat #(tree-text infos prices % (inc depth)) (:children node)))))

(defn report [{:keys [result prices]} names fmt]
  (let [infos (:denoms result)
        px    (:prices prices)
        name-fn (fn [a] (if-let [l (get names a)] (str l " " (last4 a)) (last4 a)))
        tree  (t/portfolio-tree {:infos infos :prices px
                                 :classes (:asset-classes @config)
                                 :name-fn name-fn}
                                result)
        reps  (into {} (map (juxt :address identity) (:reports result)))
        head  (:head result)
        alarms (filter :alarm? (:pegs prices))
        meta  {:chain-id (:chain-id head) :height (:height head) :time (:time head)
               :data "unverified (REST)"
               :prices (cond (:error prices) {:error (:error prices)}
                             px {:source "Figure Markets last trades" :fetched-at (:fetched-at prices)
                                 :estimated (into {} (for [[s v] px :when (:estimated? v)] [s (:usd v)]))
                                 :assumed (vec (sort (for [[s v] px :when (:assumed? v)] s)))}
                             :else nil)
               :hidden-empty-group-accounts (:hidden-empty result)
               :warnings (vec (for [{:keys [symbol market fixed]} alarms]
                                (str symbol " trades at $" market ", away from its $" fixed " value")))}]
    (case fmt
      "json" (js/JSON.stringify (clj->js (assoc meta :tree (node->data infos px reps tree))) nil 2)
      "edn"  (pr-str (assoc meta :tree (node->data infos px reps tree)))
      (str/join "\n"
                (concat [(str "Block " (:height head) " · " (:time head) " · " (:chain-id head) " · data unverified (REST)")]
                        (tree-text infos px tree 0)
                        (when-let [e (not-empty (get-in meta [:prices :estimated]))]
                          [(str "Estimated prices (no Figure market): "
                                (str/join ", " (for [[s v] (sort e)] (str s " $" v))))])
                        (when (:error prices) [(str "Prices unavailable: " (:error prices))])
                        (:warnings meta))))))

;; ============================================================ main

(defn main [args]
  (let [o (parse-args args)]
    (cond
      (:help? o) (do (print usage) (js/process.exit 0))
      (:error o) (do (binding [*print-fn* *print-err-fn*] (println (:error o)) (print usage)) (js/process.exit 2))
      (empty? (:accounts o)) (do (binding [*print-fn* *print-err-fn*] (print usage)) (js/process.exit 2))
      (not (#{"tree" "json" "edn"} (:format o)))
      (do (binding [*print-fn* *print-err-fn*] (println "Unknown format" (:format o))) (js/process.exit 2))
      :else
      (let [cfg (load-config (or (:config o) (default-config-path)))]
        (reset! config cfg)
        (-> (load-all cfg o)
            (p/then (fn [data]
                      (println (report data (into {} (keep (fn [{:keys [address label]}] (when label [address label]))) (:accounts o))
                                       (:format o)))))
            (p/catch (fn [e]
                       (binding [*print-fn* *print-err-fn*] (println "Failed:" (ex-message e)))
                       (js/process.exit 1))))))))

(main (vec *command-line-args*))
