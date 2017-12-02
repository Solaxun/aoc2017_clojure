(ns aoc2017-clj.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]))

(def data (slurp "resources/day2.txt"))

(defn to-numbers [numstring]
  (->> (str/split numstring #"\n")
       (map #(str/split % #"\t"))
       (map (fn [row]
              (map #(-> % str read-string) row)))))

(->> (to-numbers data)
     (map (juxt #(apply max %) #(apply min %)))
     (map (fn [[x y]] (- x y)))
     (reduce +))

(->> (to-numbers data)
     (map #(combs/combinations % 2))
     (mapcat (fn [combpairs]
            (filter (fn [[x y]]
                      (zero? (mod (max x y) (min x y)))) combpairs)))
     (map (fn [[x y]] (/ (max x y) (min x y))))
     (reduce +))
