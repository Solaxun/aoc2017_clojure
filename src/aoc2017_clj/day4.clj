(ns aoc2017-clj.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]))

(def data (str/trim (slurp "resources/day4.txt")))

(def words (str/split data  #"\n"))

(count (filter #(= 1 (apply max  %))
               (map (comp vals frequencies)
                    (map #(str/split % #" ") words))))

(count (filter #(= 1 (apply max  %))
               (map (comp vals frequencies)
                    (map (fn [phrases] (map sort phrases))
                         (map #(str/split % #" ") words)))))
