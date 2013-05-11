(ns com.thelastcitadel.cps-test
  (:require [com.thelastcitadel.cps :refer :all]
            [clojure.test :refer :all]))

(deftest t-Y
  (let [h (Y (fn [x]
               (+ 10 (yield x))))
        g (Y (fn [a b]
               (+ (yield (h 10))
                  (yield a)
                  (yield b))))
        f (Y (fn [x y z]
               (+ (yield (g x y))
                  (yield x)
                  (yield y)
                  (yield z))))]
    (is (= 29 (run (f 1 2 3))))))

(deftest t-Y-if
  (let [f (Y (fn [x]
               (let [x (if x
                         (yield x)
                         (yield 1))]
                 x)))]
    (is (= 1 (run (f 1))))
    (is (= 1 (run (f nil))))))

(deftest t-Y-letfn
  (let [f (Y (let [f (letfn [(h [x]
                               (+ 10 (yield x)))
                             (g [a b]
                               (+ (yield (h 10))
                                   (yield a)
                                   (yield b)))
                             (f [x y z]
                               (+ (yield (g x y))
                                  (yield x)
                                  (yield y)
                                  (yield z)))]
                       f)]
               f))]
    (is (= 29 (run (f 1 2 3)))))
  (let [f (Y (letfn [(h [x]
                       (+ 10 (yield x)))
                     (g [a b]
                       (+ (yield (h 10))
                          (yield a)
                          (yield b)))
                     (f [x y z]
                       (+ (yield (g x y))
                          (yield x)
                          (yield y)
                          (yield z)))]
               f))]
    (is (= 29 (run (f 1 2 3))))))
