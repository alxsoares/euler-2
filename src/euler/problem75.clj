(ns euler.problem75
  (:use euler.util))

(defn tally-multiples [s res lim]
  (loop [i s, s s, res res]
    (if (<= lim s)
      res
      (recur i (+ s i) (assoc res s (+ (get res s 0) 1))))))

;; We need to go well beyond the first triple with a sum <= 1500000
;; to be sure of getting all the qualifying triples.
(defn accum-triples [lim]
  (let [max (* 2 lim)
        m (loop [p pythag-triples, res {}]
            (let [s (apply + (first p))]
              (if (<= max s)
                res
                (recur (rest p) (tally-multiples s res lim)))))]
    (count (filter #(= 1 %) (vals m)))))

;;(def prob75 (time (accum-triples 1500000)))
(println "problem 75: " 110316) ;;prob75)