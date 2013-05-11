(ns com.thelastcitadel.json-test
  (:use clojure.test
        com.thelastcitadel.json)
  (:require [clojure.test.generative.runner :as runner]))

(deftest t-simple-string-test
  (let [bb (java.nio.ByteBuffer/allocate 10)
        baos (java.io.ByteArrayOutputStream.)
        e (encoder bb)]
    (enc e "foo bar baz")
    (while (step e)
      (step e)
      (.flip bb)
      (.write baos
              (.array bb)
              (.position bb)
              (.limit bb))
      (.clear bb))
    (is (= "\"foo bar baz\""
           (String. (.toByteArray baos))))))

(deftest t-string-with-quotes-test
  (let [bb (java.nio.ByteBuffer/allocate 10)
        baos (java.io.ByteArrayOutputStream.)
        e (encoder bb)]
    (enc e "foo \"bar\" baz")
    (while (step e)
      (step e)
      (.flip bb)
      (.write baos
              (.array bb)
              (.position bb)
              (.limit bb))
      (.clear bb))
    (is (= "\"foo \"bar\" baz\""
           (String. (.toByteArray baos))))))

(deftest t-simple-map-test
  (let [bb (java.nio.ByteBuffer/allocate 10)
        baos (java.io.ByteArrayOutputStream.)
        e (encoder bb)]
    (enc e {"foo" "bar"})
    (while (step e)
      (step e)
      (.flip bb)
      (.write baos
              (.array bb)
              (.position bb)
              (.limit bb))
      (.clear bb))
    (.flip bb)
    (.write baos
            (.array bb)
            (.position bb)
            (.limit bb))
    (is (= "{\"foo\":\"bar\"}"
           (String. (.toByteArray baos)))))
  (let [bb (java.nio.ByteBuffer/allocate 10)
        baos (java.io.ByteArrayOutputStream.)
        e (encoder bb)]
    (enc e {:foo "bar"})
    (while (step e)
      (step e)
      (.flip bb)
      (.write baos
              (.array bb)
              (.position bb)
              (.limit bb))
      (.clear bb))
    (.flip bb)
    (.write baos
            (.array bb)
            (.position bb)
            (.limit bb))
    (is (= "{\"foo\":\"bar\"}"
           (String. (.toByteArray baos)))))
  (let [bb (java.nio.ByteBuffer/allocate 10)
        baos (java.io.ByteArrayOutputStream.)
        e (encoder bb)]
    (enc e {:foo "bar"
            :baz "gg"})
    (while (step e)
      (step e)
      (.flip bb)
      (.write baos
              (.array bb)
              (.position bb)
              (.limit bb))
      (.clear bb))
    (.flip bb)
    (.write baos
            (.array bb)
            (.position bb)
            (.limit bb))
    (is (= "{\"foo\":\"bar\",\"baz\":\"gg\"}"
           (String. (.toByteArray baos))))))

(deftest t-simple-number-test
  (let [bb (java.nio.ByteBuffer/allocate 10)
        baos (java.io.ByteArrayOutputStream.)
        e (encoder bb)]
    (enc e 123456)
    (while (step e)
      (step e)
      (.flip bb)
      (.write baos
              (.array bb)
              (.position bb)
              (.limit bb))
      (.clear bb))
    (is (= "123456.0"
           (String. (.toByteArray baos))))))

(def ^:dynamic *gen* false)

(deftest t-gen
  (when-not *gen*
    (binding [*gen* true]
      (runner/-main "test/"))))
