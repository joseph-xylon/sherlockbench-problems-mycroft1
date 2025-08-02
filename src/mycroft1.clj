(ns mycroft1
  (:require [clojure.string :as string]))

(def namespace-name "Mycroft1")

;; problem-sets are defined by tag and name
(def tag-names
  {::all "All"
   ::string "String"
   ::math "Math"
   ::logic "Logic"
   ::shape "Shape"
   ::new "New"})

(defn prime?
  [n]
  (cond
    (<= n 1) false  ; Numbers less than or equal to 1 are not prime
    (= n 2) true    ; 2 is the only even prime number
    (even? n) false ; Other even numbers are not prime
    :else
    (let [sqrt-n (Math/sqrt n)]
      (not-any? #(zero? (mod n %))
                (range 3 (inc (int sqrt-n)) 2)))))

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
    :tags #{::all ::logic}}

   {:name- "valley or mountain"
    :args ["integer" "integer" "integer"]
    :function (fn [a b c]
                (cond
                  (and (= a b) (= b c)) "plateau"
                  (and (< b a) (< b c)) "valley"
                  (and (> b a) (> b c)) "mountain"
                  :else "neither"))
    :verifications [[2 1 3]
                    [1 5 2]
                    [4 4 4]
                    [1 2 3]]
    :output-type "string"
    :test-limit 20
    :tags #{::all ::shape}}

   {:name- "peak side"
    :args ["integer" "integer" "integer"]
    :function (fn [a b c]
                (if (and (> b a) (> b c))
                  (let [left-rise (- b a)
                        right-drop (- b c)]
                    (cond
                      (> left-rise right-drop) "left steeper"
                      (> right-drop left-rise) "right steeper"
                      :else "balanced"))
                  "not a peak"))
    :verifications [[1 6 4]
                    [2 8 1]
                    [3 9 3]
                    [5 2 7]]
    :output-type "string"
    :test-limit 20
    :tags #{::all ::shape}}

   {:name- "prime, even"
    :args ["integer" "integer"]
    :function (fn [a b]
                (+
                 (if (prime? a) 1 0)
                 (if (even? b) 1 0)))
    :verifications [[19 4]
                    [10 32]
                    [13 9]
                    [16 29]]
    :output-type "integer"
    :test-limit 30
    :tags #{::all ::logic ::new}}

   {:name- "multiple3, bigger2"
    :args ["integer" "integer"]
    :function (fn [a b]
                (case
                    (+
                     (if (zero? (mod a 3)) 1 0)
                     (if (> b 2) 1 0))
                  0 "luminous"
                  1 "lion"
                  2 "sunset"))
    :verifications [[19 4]
                    [10 32]
                    [13 9]
                    [16 29]]
    :output-type "integer"
    :test-limit 30
    :tags #{::all ::logic ::new}}])

