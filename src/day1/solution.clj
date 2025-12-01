(ns day1.solution)

(defn process-first [ins count current-point]
  (println count current-point (first ins))
  (if (empty? ins) count
  (let [in (first ins)
        op (if (clojure.string/starts-with? in "L") - +)
        num (Integer. (subs in 1))
        next-point (rem (+ 100000 (op current-point num)) 100)]
   (recur (rest ins) (if (= 0 next-point) (inc count) count) next-point))))

(defn count-zeros [op current num sum]
  (if (= num 0) sum
    (let [next (op current 1)
          next-sum (if (= (rem next 100) 0) (inc sum) sum)]
    (recur op next (dec num) next-sum))))

(defn process-second [ins count current-point]
  (println count current-point (first ins))
  (if (empty? ins) count
  (let [in (first ins)
        op (if (clojure.string/starts-with? in "L") - +)
        num (Integer. (subs in 1))
        next-point (op current-point num)]
    (recur (rest ins)
           (+ count (count-zeros op current-point num 0))
           (rem (+ 1000000 next-point) 100)))))

(defn solution-first []
	(let [input (clojure.string/split (slurp "src/day1/input") #"\n")]
	(process-first input 0 50)))

(defn solution-second []
	(let [input (clojure.string/split (slurp "src/day1/input") #"\n")]
	(process-second input 0 50)))

(defn -main [] (println (solution-second)))
