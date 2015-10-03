(ns json-m.core
    (:require  [clojure.algo.monads :refer :all])
    (:require [clojure.string :as str]))

(defn- value-not-in-msg [vals]
  (str "value is not one of [" (str/join ", " (map pr-str vals)) "]"))

(defn- map-every-nth [f coll n]
  (map-indexed #(if (zero? (mod (inc %1) n)) (f %2) %2) coll))

(defn- as-integral [v]
  (if (number? v)
    (if (integer? v)
      v
      (if (= (Math/floor v) v)
        (long v)
        nil))
    nil))

(defmonad json-m
   [m-result  (fn m-result-json [v]
                (fn [path _kf ks] (ks v)))
    m-bind    (fn m-bind-json [mv f]
                (fn [path kf ks]
                  (let [ks' (fn [a] ((f a) path kf ks))]
                    (mv path kf ks'))))
    m-plus    (fn m-plus-json
                ([a b]
                 (fn [path kf ks]
                   (let [kf' (fn [_ _] (b path kf ks))]
                     (a path kf' ks))))
                ([a b & mvs]
                 (loop [mv (m-plus-json a b)
                        mvs mvs]
                   (if (first mvs)
                     (recur (m-plus-json mv (first mvs))
                            (rest mvs))
                     mv))))])

(defn fail [msg]
  (fn [path kf _ks] (kf path msg)))

; for convenience, introduce some monomorphic aliases.
; this way we don't have have to wrap so much stuff in `with-monad`.
(with-monad json-m
  (def ^:private return m-result)
  (def ^:private bind   m-bind)
  (def ^:private mplus  m-plus)
  (defn- fmap [f m] (m-fmap f m))
  (defn- mjoin [m]  (m-join m))
  (defn- mseq [ms]  (m-seq ms)))

(defn format-path [path]
  (loop [elems path
         pfx   "$"]
    (let [{idx :index, key :key} (first elems)]
      (cond
        idx    (recur (rest elems) (str pfx "[" idx "]"))
        key    (recur (rest elems) (str pfx "." (name key)))
        :else  pfx))))

(defn- format-error [path msg]
  (str "Error in " (format-path path) ": " msg))

(defn run-parser [p]
  (p []
     (fn [path msg] (throw (Exception. (format-error path msg))))
     (fn [v] v)))

(defn get-path [path kf ks]
  (ks path))

(defn modify-failure [f p]
  (fn [path kf ks]
    (p path (fn [p' m] (kf p' (f m))) ks)))

(defn- add-path [p path-elem]
  (fn [path kf ks]
    (p (conj path path-elem) kf ks)))

(defn add-path-key [p key]
  (add-path p {:key key}))

(defn add-path-index [p index]
  (add-path p {:index index}))

(defn field [obj key pf & [pf-k p-def]]
  (if (instance? clojure.lang.IPersistentMap obj)
    (if (contains? obj key)
      (let [val (get obj key)]
        (bind (modify-failure (fn [msg] (str "failed to parse field " (name key) ": " msg))
                              (add-path-key (pf val) key))
              (or pf-k return)))
      (or p-def (fail (str "key " (name key) " not present"))))
    (fail "value is not an object")))

(defn field-if-present [obj key pf & [p-def]]
  (field obj key pf nil (or p-def (return nil))))

(defmacro field-case-if-present [obj key pf p-def & clauses]
  (let [preds         (take-nth 2 clauses)
        [clauses def] (if (even? (count clauses))
                        [clauses
                         `(fail ~(value-not-in-msg preds))]
                        [(drop-last 1 clauses)
                         `(~return ~(last clauses))])
        clauses       (map-every-nth (fn [expr] `(~return ~expr)) clauses 2)]

    `(field
       ~obj
       ~key
       (fn [v#]
         (~bind (~pf v#) (fn [v'#]
                           (condp = v'# ~@clauses ~def))))
       identity
       ~p-def)))

(defmacro field-case [obj key pf & clauses]
  `(field-case-if-present ~obj ~key ~pf nil ~@clauses))

(defn elem [ary idx pf & [pf-k p-def]]
  (if (vector? ary)
    (if (contains? ary idx)
      (let [val (get ary idx)]
        (bind (modify-failure (fn [msg] (str "failed to parse element " idx ": " msg))
                        (add-path-index (pf val) idx))
              (or pf-k return)))
      (or p-def (fail (str "element " idx " not present"))))
    (fail "value is not an array")))

(defn elem-if-present [ary idx pf & [p-def]]
  (elem ary idx pf nil (or p-def (return nil))))

(defmacro elem-case-if-present [ary idx pf p-def & clauses]
  (let [preds         (take-nth 2 clauses)
        [clauses def] (if (even? (count clauses))
                        [clauses
                         `(fail ~(value-not-in-msg preds))]
                        [(drop-last 1 clauses)
                         `(~return ~(last clauses))])
        clauses       (map-every-nth (fn [expr] `(~return ~expr)) clauses 2)]

    `(elem
       ~ary
       ~idx
       (fn [v#]
         (~bind (~pf v#) (fn [v'#]
                           (condp = v'# ~@clauses ~def))))
       identity
       ~p-def)))

(defmacro elem-case [ary idx pf & clauses]
  `(elem-case-if-present ~ary ~idx ~pf nil ~@clauses))

(defn- check [f msg]
  (fn [v]
    (if (f v)
      (return v)
      (fail msg))))

(defn- coerce [f msg]
  (fn [v]
    (let [val (f v)]
      (if val
        (return val)
        (fail msg)))))

(def parse-string   (check string? "value is not a string"))
(def parse-number   (check number? "value is not a number"))
(def parse-integral (coerce as-integral "value is not integral"))
(def parse-map      (check #(instance? clojure.lang.IPersistentMap %) "value is not a map"))
(def parse-vec      (check vector? "value is not a vector"))

(defn parse-vec-of-indexed [pf]
  (fn [items]
    (bind (parse-vec items) ; only for effects: assert that items is a JavaScript Array
          (constantly
            (mseq
              (map-indexed
                (fn [idx itm]
                  (modify-failure (fn [msg] (str "failed to parse element " idx ": " msg))
                                  (add-path-index (pf idx itm) idx)))
                items))))))

(defn parse-vec-of [pf]
  (parse-vec-of-indexed (fn [_ itm] (pf itm))))

(defn parse-one-of [expected]
  (fn [v]
    (let [matches (filter #(= v %) expected)]
      (if (seq matches)
        (return (first matches))
        (fail (str "value is not one of [" (str/join ", " (map pr-str expected)) "]"))))))

(defn if-truthy [p pf-then & [p-else]]
  (bind p (fn [v]
            (if v (pf-then v) (or p-else (return nil))))))

(defn if-success [p pf-then & [p-else]]
  (let [success (fmap #(pf-then %) p)
        failure (return (or p-else (return nil)))]
    (mjoin (mplus success failure))))

(defmacro parse-case [& clauses]
  (let [preds         (take-nth 2 clauses)
        [clauses def] (if (even? (count clauses))
                        [clauses
                         `(fail ~(value-not-in-msg preds))]
                        [(drop-last 1 clauses)
                         (last clauses)])]
    `(fn [v#]
       (condp = v# ~@clauses ~def))))
