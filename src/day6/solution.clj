(ns day6.solution)

(def input (clojure.string/split (slurp "src/day6/input") #"\n"))

(defn list-of-nums [strings]
  (map #(Integer. %) strings))

(defn string-to-operator [s]
  (cond
    (= "*" s) *'
    :else +))

(defn solution-first [input]
  (let [rows (map #(clojure.string/split (clojure.string/trim %) #" ") input)
        nums (map #(list-of-nums %) (take 4 rows))
        operators (map string-to-operator (last rows))]
    (reduce + (map #(apply %1 (list %2 %3 %4 %5)) operators (first nums) (second nums) (second (rest nums)) (last nums)))))

(defn calc-column [operator nums rez]
  (if (empty? nums)
    rez
    (let [to-calc (Integer. (clojure.string/trim (reduce str "" (map #(subs % 0 1) nums))))
          new-nums (filter #(not= % "") (map #(subs % 1) nums))]
      (recur operator new-nums (operator rez to-calc)))))

(defn line-to-list [line rez col-widths]
  (if (= 1 (count col-widths))
      (cons line rez)
      (cons (subs line 0 (first col-widths)) (line-to-list (subs line (inc (first col-widths))) rez (rest col-widths)))))

(defn width [s]
  (-> s
      (.trim)
      (.length)))

(defn col-widths [input]
  (let [rows (map #(clojure.string/split (.trim %) #"\s+") input)
        a (first rows)
        b (second rows)
        c (nth rows 2)
        d (last (drop-last rows))
        col-widths (map #(max (width %1) (width %2) (width %3) (width %4)) a b c d)]
    col-widths))

(defn columns [input col-widths]
  (let [my-rows (map #(line-to-list % '() col-widths) input)
        aa (first my-rows)
        bb (second my-rows)
        cc (nth my-rows 2)
        ee (last my-rows)
        dd (last (drop-last my-rows))]
    (map #(list %1 %2 %3 %4 %5) ee aa bb cc dd)))

(defn solution-second [input]
  (let [col-widths (col-widths input)
        columns (columns input col-widths)
        cols-calced (map #(calc-column (string-to-operator (clojure.string/trim (first %))) (rest %) (if (= "+" (clojure.string/trim (first %))) 0 1)) columns)]
    (reduce + cols-calced)))

(defn -main []
  (println (solution-second input)))
