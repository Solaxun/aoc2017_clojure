(ns aoc2017-clj.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]))

(def data (slurp "resources/day6.txt"))
(def nums (map read-string (str/split data #"\t")))

(defn re-distribute [nums]
  (loop [val (apply max nums)
         ix (.indexOf nums val)
         nums (assoc nums ix 0)]
    (if (zero? val)
      nums
      (recur (dec val)
             (mod (inc ix) (count nums))
             (update nums
                     (mod (inc ix) (count nums))
                     inc)))))

(defn find-repeat-banks [banks]
  (loop [b banks seen #{}]
    (if (seen b) (count seen)
        (recur (re-distribute b) (conj seen b)))))

(defn repeat-cycle-size [banks]
  (loop [b banks seen {} cnt 0]
    (if (seen b) (- cnt (seen b))
        (recur (re-distribute b) (assoc seen b cnt) (inc cnt)))))

(find-repeat-banks (vec nums))
(repeat-cycle-size (vec nums))
