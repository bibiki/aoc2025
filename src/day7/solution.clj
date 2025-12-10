(ns day7.solution)
           
(def input (mapv #(clojure.string/split % #"") (clojure.string/split (slurp "src/day7/input") #"\n")))

(defn numbers [l]
  (mapv #(if (= % "0") 0 (if (= "^" %) -1 1)) l))

(def input-numbers (mapv numbers (mapv #(clojure.string/split % #"") (clojure.string/split (slurp "src/day7/input-numbers") #"\n"))))

(defn merge-lines [l1 l2 index limit res]
  (if (= index limit)
    res
    (let [p1 (nth l1 index)
          p2 (nth l2 index)]
      (cond
        (and (= "|" p1) (= "^" p2)) (recur l1 l2 (inc index) limit (-> res
                                                                       (update (dec index) (fn [_] "|"))
                                                                       (update index (fn [_] p2))
                                                                       (update (inc index) (fn [_] "|"))))
        (and (= "|" p1) (= "." p2)) (recur l1 l2 (inc index) limit (update res index (fn [_] "|")))
        :else (recur l1 l2 (inc index) limit (update res index (fn [x] (if (not= x "x") x p2))))))))

(defn split-beam [input index]
  (if (= index (dec (count input)))
         input
         (recur (update input (inc index) #(merge-lines (nth input index) % 0 (dec (count %)) (vec (repeat (count %) "x")))) (inc index))))

(defn count-splits [space]
  (reduce + (for [y (range (dec (count space)))
                  x (range (count (first space)))
                  :when (and (= "|" (nth (nth space y) x))
                             (= "^" (nth (nth space (inc y)) x)))]
                 1)))
(defn solution-first [input]
  (count-splits (split-beam input 0)))

(defn merge-two [l1 l2 res index]
  (if (= index (count l1))
    res
    (cond
      (< (nth l1 index) 1) (recur l1 l2 (update res index (fn [_] (nth res index))) (inc index))
      (and (< 0 (nth l1 index)) (= 0 (nth l2 index))) (recur l1 l2 (update res index (fn [x] (+ x (nth l1 index)))) (inc index))
      (and (< 0 (nth l1 index)) (= -1 (nth l2 index))) (recur l1 l2 (update (update res (dec index) (fn [x] (+ x (nth l1 index)))) (inc index) (fn [x] (+ x (nth l1 index)))) (inc index)))))

(defn merge-lines-numbers [f r]
  (if (empty? r)
    (reduce + f)
    (recur (merge-two f (first r) (first r) 0) (rest r))))

(defn solution-second [input]
  (merge-lines-numbers (first input) (rest input)))

(defn -main []
  (println (solution-first input))
  (println (solution-second input-numbers)))
