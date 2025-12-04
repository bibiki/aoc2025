(ns day4.solution
  (:require [clojure.string :as s]))

(def input (map #(s/split % #"") (s/split (slurp "src/day4/input") #"\n")))

(def map-length (count input))
(def map-width (count (first input)))

(defn valid-coord [[a b]]
  (and (>= a 0) (>= b 0)
       (< a map-width) (< b map-length)))

(defn get-neighbors [x y]
  (filter valid-coord [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
               [(dec x) y] [(inc x) y]
               [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]))

(defn is-paper-roll? [[a b] input]
  (let [row (first (drop a input))]
       (= "@" (first (drop b row)))))

(defn is-movable? [[a b] input]
  (if (not (is-paper-roll? [a b] input))
      false
    (let [n (get-neighbors a b)
          paper-rolls (map #(is-paper-roll? % input) n)]
      (< (count (filter #(= true %) paper-rolls)) 4))))

(defn get-movables [input]
  (for [a (range map-width)
        b (range map-length)
        :when (is-movable? [a b] input)]
  [a b]))

(defn solution-first [input]
  (count (get-movables input)))

(defn cnt [[a b] input]
  (let [r (first (drop a input))]
    (first (drop b r))))

(defn remove-movables [input]
  (partition (count input)
             (for [a (range (count input))
                   b (range (count (first input)))]
               (if (is-movable? [a b] input)
                 "."
                 (cnt [a b] input)))))

(defn keep-removing [input]
  (let [new-map (remove-movables input)]
    (if (= input new-map)
      new-map
      (keep-removing new-map))))

(defn count-removed [input new-map]
  (for [a (range (count input))
        b (range (count (first input)))
        :when (and (is-paper-roll? [a b] input)
                   (not (is-paper-roll? [a b] new-map)))]
    1))

(defn solution-second [input]
  (let [new-map (keep-removing input)]
    (reduce + 0 (count-removed input new-map))))

(defn -main []
  (println (solution-first input))
  (println (solution-second input)))
