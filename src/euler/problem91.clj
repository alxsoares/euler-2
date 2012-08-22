(ns euler.problem91
  (:use euler.util)
  (:use clojure.math.combinatorics))

(def tuples
  (rest (for [x (range 51) y (range 51)] [x y])))

(defn distance-squared
  "Find the euclidean distance between two points"
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (+ (* dx dx) (* dy dy))))

(defn is-right
  "Determine whether or not a triangle is right-angled"
  [[x1 y1] [x2 y2]]
  (let [s1 (distance-squared [0 0] [x1 y1])
        s2 (distance-squared [0 0] [x2 y2])
        s3 (distance-squared [x1 y1] [x2 y2])
        sides (sort [s1 s2 s3])]
    (= (last sides) (+ (first sides) (second sides)))))

(def triangles (combinations tuples 2))