(ns euler.util
  (:require [clojure.contrib.lazy-seqs :as lz]
            [clojure.contrib.string :as s]))

(defn abs
  "Returns the absolute value of the number n."
  [n]
  (if (neg? n)
    (- n)
    n))

(defn non-neg?
  "Returns true if n >= 0, false otherwise."
  [n]
  (not (neg? n)))

(defn ismul?
  "Returns true if y is an integer multiple of x, false otherwise."
  [x y]
  (= 0 (rem y x)))

(defn extract-factor
  "Divides the integer n by the given factor as often as possible.  Returns a map containing
   the factor, its exponent in the original integer and the quotient upon extraction of the factor."
  ([fac n]
     (extract-factor {:fac fac :exp 0 :quot n}))
  ([{:keys [fac exp quot]}]
     (if (ismul? fac quot)
       (extract-factor {:fac fac :exp (inc exp) :quot (/ quot fac)})
       {:fac fac :exp exp :quot quot})))

(defn factor-decompose
  "For a given integer, returns a list of pairs of integers, each of which consists of a prime
   factor and the exponent of that factor."
  [n]
  (let [primes lz/primes]
    (loop [facs []
           p primes
           m (abs n)
           ex (extract-factor (first p) (abs n))]
      (if (= 1 m)
        (if (pos? n)
          facs
          (cons [-1 1] facs))
        (recur (if (= 0 (:exp ex))
                 facs
                 (conj facs [(:fac ex) (:exp ex)]))
               (next p)
               (:quot ex)
               (extract-factor (fnext p) (:quot ex)))))))

(defn palindrome?
  "Returns true if the string representation of x is a palindrome (disregarding the sign if x is a number)."
  [x]
  (let [s (if (number? x) (str (abs x)) (str x))]
    (= s (apply str (reverse s)))))

;; QQQQ could be generalised - doesn't deal with infinite sequences
(defn diagonal-product [x y]
  (let [x (vec x), y (vec y), xmax (dec (count x)), ymax (dec (count y)), ndiags (inc (+ xmax ymax))]
    (for [i (range ndiags)
          j (range (max 0 (- i ymax)) (inc (min xmax i)))]
      [(x j) (y (- i j))])))

(defn gcd
  "Returns the greatest common divisor of the integers m and n."
  ([x] x)
  ([x y] (let [r (rem x y)]
           (if (zero? r)
             (abs y)
             (gcd y r))))
  ([x y & more] (let [g (gcd x y)]
                  (if (= g 1)
                    1
                    (apply gcd (cons g more))))))

(defn lcm
  "Returns the least common multiple of the integers m and n."
  ([x y]
     (let [p (*' x y)
           g (gcd x y)]
       (abs (/ p g))))
  ([x y & more] (let [l (lcm x y)]
                  (apply lcm (cons l more)))))

(defn factorial
  "Returns the factorial of n, i.e. n*(n-1)*(n-2)*...*2*1"
  [n]
  (when (non-neg? n) (apply *' (range 2 (+ n 1)))))

(defn pow [n p]
  "Returns n raised to an integer power p.  p may be negative."
  [n p]
  (if (neg? p)
    (/ 1 (nth (iterate #(*' n %) 1) (abs p)))
    (nth (iterate #(*' n %) 1) p)))

(defn stirling-approximation
  "Returns Stirling's constant-time approximation to the factorial function.
   The relative error decreases as n increases."
  [n]
  (*' (Math/sqrt (* 2 n Math/PI)) (pow (/ n Math/E) n)))

(defn sum-of-n-ints
  "Returns the sum of the first n natural numbers."
  [n]
  (when (pos? n)
    (/ (*' n (+' n 1)) 2)))

(defn sum-of-n-squares
  "Returns the sum of the squares of the first n natural numbers."
  [n]
  (when (pos? n)
    (/ (*' n (+' n 1) (+' (*' 2 n) 1)) 6)))

(defn sum-of-n-cubes
  "Returns the sum of the cubes of the first n natural numbers."
  [n]
  (when (pos? n)
    (/ (*' n n (+' n 1) (+' n 1)) 4)))

(defn phi
  "Calculate Euler's totient function, phi(n), which is the number of integers less than n and coprime to n."
  [n]
  (when (pos? n)
    (let [facs (map first (factor-decompose n))]
      (apply * (cons n (map #(- 1 (/ 1 %)) facs))))))

(defn lt-and-coprime
  "Returns a sequence of all integers less than n and coprime to n."
  [n]
  (filter #(= 1 (gcd % n)) (range 1 n)))

(def pythag-triples
  "Returns a sequence of Pythagorean triples.  This is not a complete list, as
   integer multiples of these triples are also Pythagorean."
  (for [m (iterate inc 1) n (lt-and-coprime m) :when (odd? (- m n))]
    [(- (* m m) (* n n)) (* 2 m n) (+ (* m m) (* n n))]))

(defn digits
  "Returns a sequence of all the digits of n."
  [n]
  (map #(- (int %) 48) (seq (str (abs n)))))

(def natural-numbers
  "Returns a lazy sequence of all the natural numbers."
  (iterate inc 1))

(def even-numbers
  "Returns a lazy sequence of all the even natural numbers."
  (filter even? natural-numbers))

(def odd-numbers
  "Returns a lazy sequence of all the odd natural numbers."
  (filter odd? natural-numbers))

(def triangle-numbers
  "Returns a lazy sequence of all the triangle numbers."
  (reductions + natural-numbers))

(def square-numbers
  "Returns a lazy sequence of all the square numbers."
  (reductions + odd-numbers))

(defn r-gonal-number [n r]
  (/ (* n (- (* n (- r 2)) (- r 4))) 2))

(defn num-divisors
  "Returns the number of distinct divisors of the integer n."
  [n]
  (let [facs (factor-decompose (abs n))
        exps (map #(inc (last %)) facs)]
    (apply * exps)))

(defn int-powers
  "Returns a lazy sequence of all the integer powers of n, starting at 0."
  [n]
  (iterate #(* n %) 1))

(defn divisors
  "Returns a sequence of the divisors of the integer n."
  [n]
  (let [decomp (factor-decompose (abs n))
        facs (vec (map first decomp))
        names (vec (repeatedly (count facs) gensym))
        ranges (map #(vec (range 0 (inc (second %)))) decomp)
        iter (vec (interleave names ranges))]
    (sort (map #(apply * %) (eval `(for ~iter (map pow ~facs ~names)))))))
        
(defn boolean-tuples
  "Returns a lazy ordered sequence of all length-n tuples of boolean values."
  [n]
  (let [names (vec (repeatedly n gensym))
        iter (vec (interleave names (repeat n [false true])))]
    (eval `(for ~iter ~names))))

(defn hailstone
  "Function of n used to calculate hailstone sequences:
   hailstone(n) = 3n + 1 (n odd) | n/2 (n even)"
  [n]
  (cond (odd? n) (+ 1 (* 3 n))
        (even? n) (/ n 2)))

(defn hailstone-seq
  "Returns the hailstone sequence for n (as described in the Collatz conjecture)."
  [n]
  (when (pos? n) (seq (conj (vec (take-while #(not= 1 %) (iterate hailstone n))) 1))))

(defn n-digit?
  "Is the argument an n-digit integer?"
  [n]
  (fn [x] (and (integer? x) (< (dec (pow 10 (dec n))) x (pow 10 n)))))

(defn take-range
  "Returns a lazy sequence of the first range of members of coll for which pred evaluates to true."
  [pred coll]
  (take-while pred (drop-while #(not (pred %)) coll)))

(defn binomial-coefficient [n r]
  "Returns the binomial coefficient nCr."
  (/ (factorial n) (*' (factorial r) (factorial (- n r)))))

(defn binomial-coefficients
  "Returns a list of the binomial coefficients for n (i.e. nCr where 0 <= r <= n)."
  [n]
  (map binomial-coefficient (repeat (inc n) n) (range (inc n))))

(defn n-digit-members
  "Returns a list of the n-digit members of coll."
  [coll n]
  (take-range #((n-digit? n) %) coll))

(defn n-digit-primes
  "Returns a list of n-digit primes."
  [n]
  (n-digit-members lz/primes n))
    
(defn find-runs
  "Find runs of duplicates in a sequence.  Return a vector of pairs
   [n elt] containing the multiplicity and the element."
  [coll]
  (loop [x coll, count 1, res[]]
    (let [prev (first x), curr (second x), match (= prev curr)]
           (if (not x)
             res
             (recur (next x)
                    (if match (inc count) 1)
                    (if match res (conj res [count prev])))))))

(defn- p [k n]
  (cond
   (> k n) 0
   (= k n) 1
   :else (+ (p (inc k) n) (p k (- n k)))))

(defn- count-partitions [n]
  (p 1 n))

(defn dot
  "Find the dot product of a sequence of vectors"
  [& vecs]
  (apply + (map #(apply * %) (apply map vector vecs))))




