(ns day8.solution
  (:require [clojure.set :as s]))

(defn numbers [l]
  (map #(Integer. %) l))

(def sample-input (map numbers (mapv #(clojure.string/split % #",") (clojure.string/split (slurp "src/day8/sample-input") #"\n"))))
(def input        (map numbers (mapv #(clojure.string/split % #",") (clojure.string/split (slurp "src/day8/input") #"\n"))))

(defn distance [v1 v2]
  (Math/sqrt (reduce + (map #(* (- %1 %2) (- %1 %2)) v1 v2))))

(defn nearest [one list other]
  (cond
    (empty? list) other
    (= one other) (recur one list (first list))
    :else (nearest one
                 (rest list) 
                 (if (< (distance one other) (distance one (first list)))
                     other
                     (first list)))))

(defn my-remove [s es]
  (if (or (empty? es) (empty? s))
      s
      (recur (filter #(not= % (first es)) s) (rest es))))

(defn my-remove [s es]
  (if (empty? es)
    s
    (recur (filter #(not= (first es) %1) s) (rest es))))

(defn my-reducer [groups res]
  (if (empty? groups)
    res
    (let [f (first groups)
          circles (filter #(not= #{} (s/intersection %1 f)) res)
          u (reduce s/union f circles)]
   (recur (rest groups) (conj (my-remove res circles) u)))))

(defn all-pairs [input res]
  (if (= 1 (count input))
    res
    (let [x (first input)
          r (rest input)
          temp (map #(list (distance x %) x %) r)]
      (recur r (concat res temp)))))

(defn solution-first [input len]
  (let [distance-pairs (all-pairs input '())
        sorted-pairs (sort-by first <= distance-pairs)
        pairs (map #(set (rest %)) sorted-pairs)]
    (reduce * (take 3 (sort > (map count (my-reducer (take len pairs) #{})))))))

(defn find-last-necessary-connection [all-pairs res len target]
  (let [next-res (my-reducer (take len all-pairs) res)]
        (if (= target (count (first next-res)))
          (take 1 (drop (dec len) all-pairs))
          (recur all-pairs next-res (inc len) target))))

(defn solution-second [input]
  (let [distance-pairs (all-pairs input '())
        sorted-pairs (sort-by first <= distance-pairs)
        pairs (map #(set (rest %)) sorted-pairs)]
    (find-last-necessary-connection pairs #{} 1 (count input))))

(defn -main []
  (solution-first input)
  (solution-second input))
