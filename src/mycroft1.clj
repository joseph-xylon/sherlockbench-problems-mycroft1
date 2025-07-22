(ns mycroft1
  (:require [clojure.string :as string]))

(def namespace-name "Mycroft1")

;; problem-sets are defined by tag and name
(def tag-names
  {::all "All"
   ::string "String"
   ::math "Math"
   ::logic "Logic"})

(def problems
  [{:name- "subtract number of unique values from fixed value"
    :args ["integer" "integer" "integer"]
    :function (fn [a b c]
                (let [unique-count (count (set [a b c]))
                      fixed-value 10]
                  (- fixed-value unique-count)))
    :verifications [[1 2 3]
                    [5 5 5]
                    [2 2 3]]
    :output-type "integer"
    :test-limit 20
    :tags #{::all ::math}}
   
   {:name- "count identical"
    :args ["integer" "integer" "integer"]
    :function (fn [a b c]
                (let [values [a b c]
                      freq (frequencies values)]
                  (apply max (vals freq))))
    :verifications [[1 1 2]
                    [3 3 3]
                    [1 2 3]]
    :output-type "integer"
    :test-limit 20
    :tags #{::all ::logic}}

   {:name- "fizzbuzz but with different words"
    :args ["integer"]
    :function (fn [n]
                (cond
                  (zero? (mod n 15)) "BoomCrash"
                  (zero? (mod n 3)) "Boom"
                  (zero? (mod n 5)) "Crash"
                  :else (str n)))
    :verifications [[1]
                    [3]
                    [5]
                    [15]]
    :output-type "string"
    :test-limit 20
    :tags #{::all ::logic}}])