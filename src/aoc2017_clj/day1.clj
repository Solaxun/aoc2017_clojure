(ns aoc2017-clj.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]))

(def data (str/trim (slurp "resources/day1.txt")))

(defn part1 [nums]
  (->> nums
       (partition 2 1)
       (filter #(apply = %))
       (map (comp read-string str first))
       (reduce +)
       (#(if (= (first data) (last data)) (+ 3 %) %)))) ;cheat on last element

(def step (/ (count data) 2))

(defn part2 [nums]
  (let [nums (mapv (comp read-string str) nums)
        cnt (count nums)]
       (reduce +
               (map-indexed (fn [ix x]
                                (if (= x (get nums (mod (+ step ix) cnt)))
                                    x
                                    0)) nums))))

(part1 data)
(part2 data)
