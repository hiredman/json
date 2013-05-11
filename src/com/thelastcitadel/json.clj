(ns com.thelastcitadel.json
  (:require [com.thelastcitadel.cps :refer [Y defnY run done?]]))

(declare encode)

(defnY encode-string* [form buffer idx]
  (when (> (count form) idx)
    (.put buffer (nth form idx))
    (encode-string* form buffer (yield (inc idx)))))

(defnY encode-string [form buffer]
  (yield (.put buffer (byte (int \"))))
  ;; calling getBytes on strings is gross, but I need utf8 bytes
  (yield (encode-string* (.getBytes form "utf8") buffer 0))
  (.put buffer (byte (int \"))))

(defnY encode-pairs [pairs buffer]
  (if (seq pairs)
    (let [[k v] (first pairs)]
      (yield (encode k buffer))
      (yield (.put buffer (byte (int \:))))
      (yield (encode v buffer))
      (when (seq (rest pairs))
        (yield (.put buffer (byte (int \,)))))
      (encode-pairs (yield (rest pairs)) buffer))))

(defnY encode-map [form buffer]
  (yield (.put buffer (byte (int \{))))
  (yield (encode-pairs (seq form) buffer))
  (.put buffer (byte (int \}))))

(defnY encode-number [form buffer]
  (encode-string* (.getBytes (.toString form) "utf8") buffer 0))

(defnY encode-bool [form buffer]
  (encode-string* (.getBytes (.toString form) "utf8") buffer 0))

(defnY encode-nil [form buffer]
  (encode-string* (.getBytes "nil" "utf8") buffer 0))

(defnY encode-array* [form buffer]
  (when (seq form)
    (yield (encode (first form) buffer))
    (when (seq (rest form))
      (yield (.put buffer (byte (int \,)))))
    (encode-array* (yield (rest form)) buffer)))

(defnY encode-array [form buffer]
  (yield (.put buffer (byte (int \[))))
  (yield (encode-array* form buffer))
  (.put buffer (byte (int \]))))

(defnY encode [form buffer]
  (cond
   (nil? form)
   (yield (encode-nil form buffer))
   (string? form)
   (yield (encode-string form buffer))
   (map? form)
   (yield (encode-map form buffer))
   (or (keyword? form) (symbol? form))
   (yield (encode-string (name form) buffer))
   (number? form)
   (yield (encode-number (double form) buffer))
   (coll? form)
   (yield (encode-array (seq form) buffer))
   ;; (instance? Character form)
   ;; (yield (encode-string (str form) buffer))
   (instance? Boolean form)
   (yield (encode-bool form buffer))
   :else (throw (Exception. "unknown")))
  false)


;;;;;;;;;;;;

(defprotocol Encode
  (enc [_ form])
  (step [_]))

(defn encoder [buffer]
  (let [r (atom nil)]
    (reify
      Encode
      (enc [_ form]
        (reset! r (encode form buffer))
        nil)
      (step [_]
        (if (done? @r)
          false
          (loop [p (.position buffer)]
            (if-not (zero? (.remaining buffer))
              (let [new-r (com.thelastcitadel.cps/step @r)]
                (reset! r new-r)
                (if (done? new-r)
                  false
                  (if (= p (.position buffer))
                    true
                    (recur (.position buffer)))))
              true)))))))


(comment

  (let [bb (java.nio.ByteBuffer/allocate 10)
        e (encoder bb)]
    (enc e {"a" "b"})
    (step e)
    (step e)
    (step e)
    (step e)
    (step e)
    (step e)
    (step e)
    (step e)
    (step e)
    (String. (.array bb)))

  )
