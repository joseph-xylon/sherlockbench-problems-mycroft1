(ns mycroft1
  (:require [clojure.string :as string]))

(def namespace-name "Mycroft1")

;; problem-sets are defined by tag and name
(def tag-names
  {::all "All"
   ::math "Math"
   ::logic "Logic"
   ::shape "Shape"})

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
  [{:name- "fizzbuzz with different words"
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

   {:name- "is descending"
    :args ["integer" "integer" "integer"]
    :function (fn [a b c]
                (> a b c))
    :verifications [[5, 6, 3], [12, 5, 4], [5, 7, 90], [15, 2, 7]]
    :output-type "boolean"
    :test-limit 20
    :tags #{::all}}

   {:name- "multiple3, bigger"
    :args ["integer" "integer"]
    :function (fn [a b]
                (+
                 (if (zero? (mod a 3)) 1 0)
                 (if (> a b) 0 1)))
    :verifications [[19 4]
                    [10 32]
                    [13 9]
                    [16 29]]
    :output-type "integer"
    :test-limit 30
    :tags #{::all ::logic}}

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

   {:name- "subtract number of unique values from fixed value"
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
    :tags #{::all ::logic}}

   {:name- "crack lock"
    :args ["integer" "integer" "integer"]
    ;; return 3-D Manhattan-distance until they guess right
    :function (fn [a b c]
                (let [code [6 7 1]
                      attempts [a b c]
                      acc (reduce + (map #(Math/abs (- %1 %2)) code attempts))]
                  acc))
    :verifications [[5 3 8]]
    :output-type "string"
    :test-limit 20
    :tags #{::all ::investigation ::crack-lock}}

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

   {:name- "set heading"
    :args ["integer" "integer"]
    :function (fn [a b]
                (let [lat  66
                      long 110
                      vdiff (- lat a)
                      hdiff (- long b)]
                  (cond
                    (and (zero? vdiff) (zero? hdiff))
                    "secret575"

                    (> (Math/abs vdiff) (Math/abs hdiff))
                    (if (pos? vdiff) "North" "South")

                    :else
                    (if (pos? hdiff) "East" "West"))))
    :verifications [[36, 140]
                    [54, 144]
                    [30, 150]]
    :output-type "string"
    :test-limit 30
    :tags #{::all ::investigation}}
])
