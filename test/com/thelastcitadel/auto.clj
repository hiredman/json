(ns com.thelastcitadel.auto
  (:require [com.thelastcitadel.json :refer :all]
            [clojure.data.generators :as gen]
            [clojure.test.generative :refer :all]
            [cheshire.core :as json]))

(defn encode-to-string [data]
  (let [bb (java.nio.ByteBuffer/allocate 10)
        baos (java.io.ByteArrayOutputStream.)
        e (encoder bb)]
    (enc e data)
    (while (step e)
      (step e)
      (.flip bb)
      (.write baos
              (.array bb)
              (.position bb)
              (.limit bb))
      (.clear bb))
    (String. (.toByteArray baos))))

(defn collections [data]
  (if (zero? (gen/uniform 0 3))
    (into gen/collections
          [[gen/vec [(data)]]
           [gen/set [(data)]]
           [gen/hash-map [gen/scalars (data)]]])
    gen/collections))

(defn coll []
  (let [[coll args] (rand-nth (collections coll))]
    (apply coll (map (comp rand-nth seq) args))))

(defn data []
  (gen/one-of coll gen/scalar))

(defn comparable-json [data]
  {:cheshire (try
               [(json/generate-string data)]
               (catch Throwable e
                 [nil e]))
   :json (try
           [(encode-to-string data)]
           (catch Throwable e
             [nil e]))})

(defspec json
  comparable-json
  [^{:tag `data} coll]
  (let [{[cheshire ch-e] :cheshire
         [json json-e] :json} %]
    (is (or (= (try
                 (json/parse-string cheshire)
                 (catch Exception e
                   :a))
               (try
                 (json/parse-string cheshire)
                 (catch Exception e
                   :b)))
            (and ch-e json-e)))))
