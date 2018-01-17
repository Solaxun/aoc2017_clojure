(ns aoc2017-clj.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]))

(def data (slurp "resources/day5.txt"))
(def nums (mapv read-string (str/split data #"\n")))

(defn jump-around [jumps]
  (loop [j jumps steps 0 pos 0]
    (if (get j pos)
      (recur (update j pos inc) (inc steps) (+ pos (get j pos)))
      steps)))

(defn jump-around-2 [jumps]
  (loop [j jumps steps 0 pos 0]
    (if (get j pos)
      (recur (update j pos #(if (>= % 3) (dec %) (inc %)))
             (inc steps)
             (+ pos (get j pos)))
      steps)))


(jump-around nums)
(jump-around-2 nums)
