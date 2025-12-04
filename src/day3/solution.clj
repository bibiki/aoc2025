(ns day3.solution
  (:require [clojure.string :as s]))

(def input (s/split (slurp "src/day3/input") #"\n"))

(defn line-to-ints [line]
  (map #(Integer. %) line))

(defn line-max [line]
  (let [tens (reduce max (drop-last line))
        ones (reduce max (rest (drop-while #(< % tens) line)))]
    (+ (* 10 tens) ones)))

(defn solution-first [input]
  (let [lines (map #(s/split % #"") input)
        line-ints (map line-to-ints lines)
        maxes (map line-max line-ints)]
    (reduce + 0 maxes)))

(defn line-max-twelve [line target]
  (cond
    (= target 0) '()
    (= target (count line)) line
    :else
          (let [left (take (- (count line) target -1) line)
                m (reduce max 0 left)
                next-line (rest (drop-while #(not= % m) line))]
      (cons m (line-max-twelve next-line (dec target))))))

(defn calc-line [line total]
  (if (empty? line)
      total
      (recur (rest line) (+ (first line) (*' total 10)))))

(defn solution-second [input]
  (let [lines (map #(s/split % #"") input)
        line-ints (map line-to-ints lines)
        lines-of-twelve (map #(line-max-twelve % 12) line-ints)
        calc-lines (map #(calc-line % 0) lines-of-twelve)]
    (reduce + 0 calc-lines)))

(defn -main []
  (println (solution-second input)))
