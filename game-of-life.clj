#!/usr/bin/env bb

(require '[clojure.pprint :refer [pprint]])
(defn neighbours
  [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ x dx) (+ y dy)]))

(defn populate
  "Turns :on each of the cells specified as [y, x] coordinates."
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(defn empty-board
  "Creates a rectangular empty board of the specified width and height."
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn stepper
  "Returns a step function for Life-like cell automata
   neighbours takes a location and return a sequential collection
   of locations. survive? and birth? are predicates on the number of
   living neighbours"
  [neighbours birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (if (cells loc) (survive? n) (birth? n))]
         loc))))

(def step (stepper neighbours #{3} #{2 3}))

(->> (iterate step #{[2 0] [2 1] [2 2] [1 2] [0 1]})
     (drop 8)
     first
     (populate (empty-board 6 6))
     pprint)
