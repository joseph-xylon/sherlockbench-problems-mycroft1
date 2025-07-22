(ns mycroft1-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [mycroft1 :refer [problems]]))

;; we need some helper functions to extract the function and call it
(defn find-map-by-key-value [key value coll]
  (some #(when (= (key %) value) %) coll))

(defn eval-fn [fn-name & args]
  (apply (:function (find-map-by-key-value :name- fn-name problems)) args))

(deftest test-subtract-number-of-unique-values-from-fixed-value
  (is (= 7 (eval-fn "subtract number of unique values from fixed value" 1 2 3))) ; 3 unique, 10-3=7
  (is (= 9 (eval-fn "subtract number of unique values from fixed value" 5 5 5))) ; 1 unique, 10-1=9
  (is (= 8 (eval-fn "subtract number of unique values from fixed value" 2 2 3)))) ; 2 unique, 10-2=8

(deftest test-count-identical
  (is (= 2 (eval-fn "count identical" 1 1 2))) ; max occurrence is 2 (for 1)
  (is (= 3 (eval-fn "count identical" 3 3 3))) ; max occurrence is 3 (for 3)
  (is (= 1 (eval-fn "count identical" 1 2 3)))) ; max occurrence is 1 (all different)

(deftest test-fizzbuzz-but-with-different-words
  (is (= "1" (eval-fn "fizzbuzz but with different words" 1)))
  (is (= "Boom" (eval-fn "fizzbuzz but with different words" 3)))
  (is (= "Crash" (eval-fn "fizzbuzz but with different words" 5)))
  (is (= "BoomCrash" (eval-fn "fizzbuzz but with different words" 15))))

(comment
  (run-tests)
  )