(ns day2.solution
  (:require [clojure.math :as math]))

(def input (clojure.string/split (slurp "src/day2/input") #","))

(defn integer-range [[a b]]
  [(Long. a) (Long. b)])

(defn invalid-id [id p]
  "for the first part of the puzzle"
  (if (> p (math/pow 10 (/ (math/ceil  (math/log10 id)) 2)))
    false
    (if (= (rem id p) (quot id p))
        true
        (recur id (* 10 p)))))

(defn digits [p]
  (if (< 0 p 10)
    1
    (inc (digits (quot p 10)))))

(defn invalid-id-second [id shift max]
  "for the second part of the puzzle"
  (if (> shift max)
      false
    (let [p (math/pow 10 shift)
          r (rem id p)
          q (quot id p)]
      (or (= id (long (+ (* r (math/pow 10 (digits q))) q)))
          (invalid-id-second id (inc shift) max)))))

(defn solution-second [ranges sum]
  (if (empty? ranges)
     sum
     (let [[left right] (first ranges)
           invalid-ids (filter #(invalid-id-second % 1 (quot (int (math/ceil (math/log10 %))) 2)) (range left (inc right)))]
       (recur (rest ranges) (reduce + sum invalid-ids)))))

(defn solution-first [ranges sum]
  (if (empty? ranges)
     sum
     (let [[left right] (first ranges)
           invalid-ids (filter #(invalid-id % 10) (range left (inc right)))]
       (recur (rest ranges) (reduce + sum invalid-ids)))))

(defn process-first [input]
  (let [ranges-strings (map #(clojure.string/split % #"-") input)
        ranges (map integer-range ranges-strings)]
    (solution-first ranges 0)))

(defn process-second [input]
  (let [ranges-strings (map #(clojure.string/split % #"-") input)
        ranges (map integer-range ranges-strings)]
    (solution-second ranges 0)))

(defn -main [] (println (process-second input)))
