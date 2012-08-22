(ns euler.problem51
  (:use euler.util))

(defn pad [s n]
  (let [pad (- n (count s))]
    (str (apply str (repeat pad "0")) s)))

(defn binary-string [x len]
  (pad (Integer/toBinaryString x) len))

(defn binary-strings [len]
  (map #(tuple % len) (range 1 (- (pow 2 len) 1))))

(defn qualifies? [p b]
  (loop [s "", p (str p), b b, c []]
    (if (not= (first c) (last c))
      false
      (if (empty? p)
        (Integer/parseInt s)
        (let [t (= \1 (first b))
              news (if t s (str s (first p)))
              newc (if t (conj c (first p)) c)]
          (recur news (rest p) (rest b) newc))))))

(defn push-value [m k v]
  (let [oldv (get m k)]
    (if oldv
      (assoc m k (conj oldv v))
      (assoc m k [v]))))

(defn find-qualifying-primes [primes b]
  (loop [p primes, c {}]
    (if (empty? p)
      c
      (let [candidate (first p)
            q (qualifies? candidate b)
            newc (if q (push-value c q candidate) c)]
        (recur (rest p) newc)))))

(defn runs [primes b]
  (let [vs (vals (find-qualifying-primes primes b))]
    (loop [v vs, runs {}]
      (if (empty? v)
        runs
        (recur (rest v) (push-value runs (count (first v))(first v)))))))

(defn max-runs [primes b]
  (let [runs (runs primes b)
        k (apply max (keys runs))]
    (get runs k)))

(defn n-digit-runs [n]
  (let [p (n-digit-primes n)
        b (binary-strings n)
        r (apply concat (map #(max-runs p %) b))]
    (loop [m r, c {}]
      (if (empty? m)
        c
        (recur (rest m) (push-value c (count (first m)) (first m)))))))

(defn max-n-digit-runs [n]
  (let [r (n-digit-runs n)
        k (apply max (keys r))]
    (get r k)))

(defn best-run [n]
  (let [r (max-n-digit-runs n)
        min (apply min (map first r))]
    (filter #(= min (first %)) r)))

;;problem 51
;;(def prob51 (time (best-run 6)))
(println "problem 51" 121313) ;;prob51)
