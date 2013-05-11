(ns com.thelastcitadel.cps)

(defrecord Fn [kont bodies name])
(defrecord FnBody [args body])
(defrecord Constant [kont value])
(defrecord App [kont value])
(defrecord Binding [kont name])
(defrecord Yield [kont value])
(defrecord Cond [value then else])
(defrecord LetFn [kont fns])

(defprotocol Finished
  (finished? [_]))

;; a Continuation and a value to be delivered unto it
(defrecord Computation [value kont]
  Finished
  (finished? [_] false))

(extend-protocol Finished
  Object
  (finished? [_] true)
  nil
  (finished? [_] true))

(def continueate nil)
(def continueate-seq nil)

(declare c)
(defmulti continueate (fn [e k] (type e)))
(defmulti continueate-seq (fn [e k] (first e)))
(defmethod continueate clojure.lang.Symbol [s k]
  (->Constant k s))
(defmethod continueate nil [s k]
  (->Constant k s))
(defmethod continueate Number [s k]
  (->Constant k s))
(defmethod continueate Boolean [s k]
  (->Constant k s))
(defmethod continueate clojure.lang.ISeq [s k]
  (continueate-seq s k))
(defmethod continueate-seq 'fn* [form k]
  (let [[name bodies] (if (symbol? (second form))
                        [(second form) (rest (rest form))]
                        [nil (rest form)])]
    (->Fn k (doall (for [[args & body] bodies]
                     (->FnBody args (c (cons 'do body) k))))
          name)))
(defmethod continueate-seq 'do [[_ & body] k]
  (if (> (count body) 1)
    (let [end (last body)
          body (butlast body)]
      (c
       `(let ~(vec (apply concat (for [b body] ['_ b])))
          ~end)
       k))
    (c (first body) k)))
(defmethod continueate-seq 'let* [[_ bindings & body] k]
  (if (seq bindings)
    (let [bindings (partition-all 2 bindings)]
      (reduce
       (fn [cont [n v]]
         (c v (->Binding cont n)))
       (c (cons 'do body) k)
       (reverse bindings)))
    (c `(cons 'do body) k)))
(defmethod continueate-seq 'if [[_ test then else] k]
  (if (coll? test)
    (c
     `(let [t# ~test]
        (if t# ~then ~else))
     k)
    (->Cond test
            (c then k)
            (c else k))))
(defmethod continueate-seq 'yield [[_ v] k]
  (if (coll? v)
    (c
     `(let [n# ~v]
        (~'yield n#))
     k)
    (->Yield k v)))
(defmethod continueate-seq 'letfn* [[_ fns & body] k]
  (->LetFn
   (c (cons 'do body) k)
   (doall (for [[n f] (partition-all 2 fns)]
            [n (c f 'foo)]))))
(defmethod continueate-seq :default [app k]
  (if (every? (complement coll?) app)
    (->App k app)
    (let [bindings (for [e app]
                     (if (coll? e)
                       [(gensym) e]
                       [e e]))]
      (c
       `(let ~(vec (apply concat (remove #(= (first %) (second %)) bindings)))
          ~(doall (map first bindings)))
       k))))

(defn c [form k]
  (continueate (macroexpand form) k))

(defn generate [f]
  (cond
   (instance? Fn f) `(fn ~@(when (:name f)
                             [(:name f)])
                       ~@(doall (map generate (:bodies f))))
   (instance? FnBody f) `(~(:args f) ~(generate (:body f)))
   (instance? LetFn f)
   `(letfn* ~(vec (apply concat (for [[n v] (:fns f)]
                                  [n (generate v)])))
            ~(generate (:kont f)))
   (instance? Cond f)
   `(if ~(:value f)
      ~(generate (:then f))
      ~(generate (:else f)))
   (and (instance? App f)
        (instance? Binding (:kont f)))
   `(let [~(:name (:kont f)) ~(:value f)]
      ~(generate (:kont (:kont f))))
   (and (instance? App f)
        (symbol? (:kont f)))
   (:value f)
   (and (instance? Yield f)
        (instance? Binding (:kont f)))
   `(->Computation ~(:value f) (fn [~(:name (:kont f))] ~(generate (:kont (:kont f)))))
   (and (instance? Constant f)
        (instance? Binding (:kont f)))
   `(let [~(:name (:kont f)) ~(:value f)]
      ~(generate (:kont (:kont f))))
   (and (instance? Constant f)
        (symbol? (:kont f)))
   (:value f)
   :else
   (throw (Exception. (str "unknown " (pr-str f))))))

(defmacro Y [form]
  (generate (c form 'foo)))

(defn done? [x]
  (finished? x))

(defn step
  "steps a computation forward"
  [x]
  (if-not (done? x)
    (if (done? (:value x))
      ((:kont x) (:value x))
      (let [new-x (step (:value x))]
        (->Computation new-x (:kont x))))
    x))

(defn run [x]
  (if (done? x)
    x
    (recur (step x))))

(defmacro defnY [& params]
  (let [[_ name value] (macroexpand (cons 'clojure.core/defn params))]
    `(def ~name (Y ~value))))
