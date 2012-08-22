(ns euler.turtle)

(defn create-turtle
  []
  {:position [0 0]
   :direction 0
   :history '()
   :pen-down false
   :pen-style {:thickness 1 :colour "#f00"}})

(defn pen-down
  [turtle]
  (assoc turtle
    :pen-down true
    :history (cons (:history turtle) [turtle])))