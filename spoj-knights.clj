#!/usr/bin/env bb
(ns spoj.knight
  (:require (clojure [string :as str]
                     [data :as data])))

(defn full-graph [n]
  (apply merge-with
    (comp set concat)
    (for [x (range 1 n) y (range (+ 1 x) (+ 1 n)) :when (not (= y x))]
      {x #{y} y #{x}})))

(defn longest-circuit
  "Returns the longest simple circuit in a graph. Using DFS"
  [nodes succs]
  (letfn
    [(dfs [env node]
       (if ((:visited env) node)
         env
         (let [depth (+ 1 (:depth env))]
           #_(println "dfs: " env node)
           (-> env
               (assoc :depth depth)
               (assoc-in [:visited node] depth)
               ((partial reduce
                  (fn [env succ]
                    (if-let [depth-succ ((:visited env) succ)]
                      (let [new-circuit (+ 1 (- depth depth-succ))]
                        (if (odd? new-circuit)
                          (assoc env :circuit (max (:circuit env) (+ 1 (- depth depth-succ))))
                          env))
                      (dfs env succ))))
                (succs node))))))]
    (:circuit (reduce dfs {:circuit 0 :visited {} :depth 0} nodes))))

(defn work [n m]
  #_(println "Read " n " & " m)
  (let [e-origin (apply merge-with (comp set concat)
                        (for [_ (range m)]
                          (let [[s-x s-y & _] (str/split (read-line) #" ")
                                 x (Integer. s-x)
                                 y (Integer. s-y)]
                            {x #{y} y #{x}})))
        e-comp (merge-with ; Complement Graph
                 (comp set (partial filter #(not= % nil)) first data/diff)
                 (full-graph n) e-origin)]
    #_(println (range 1 (+ 1 n)) "\n"
               (full-graph n) "\n"
               e-origin "\n"
               e-comp)
    (- n (longest-circuit (range 1 (+ 1 n)) e-comp))))

(defn main []
  (loop [[s-n s-m & _] (str/split (read-line) #" ")]
    (let [n (Integer. s-n) m (Integer. s-m)]
      (when (and (not= n 0) (not= m 0))
        (println (work n m))
        (recur (str/split (read-line) #" "))))))

(main)
