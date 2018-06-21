(ns aoc2017-clj.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]))

(def data (slurp "resources/day8.txt"))
(def instructions (str/split data #"\n"))

(defn parse-instr [instr]
  ((comp rest first)
   (re-seq #"(\w+) (inc|dec) (-*\d+) if (.+)" instr)))

(defn eval-single-infix-expr [[val1 op val2] regs]
  (let [op (get {"==" "=" "!=" "not="} op op)]
    (eval (read-string (str "(" op " " (regs val1) " " val2 ")")))))

(def registers
  (let [regs (map (comp first parse-instr) instructions)
        zeros (repeat 0)]
    (zipmap regs zeros)))

(defn validate-expr [regs instr]
  (let [[val1 op val2 expr] (parse-instr instr)]
    (if (eval-single-infix-expr (str/split expr #" ") regs)
      (assoc regs val1 (if (= op "inc") (+ (regs val1) (read-string val2))
                           (- (regs val1) (read-string val2))))
      regs)))

(defn process-instructions [instructions regs]
  (reduce validate-expr regs instructions))

;; part 1
(apply max (vals (process-instructions instructions registers)))

;; part 2
(defn process-instructions-2 [instructions regs]
  (reductions validate-expr regs instructions))

(apply max (mapcat vals (process-instructions-2 instructions registers)))
(map (comp read-string str) (str 432))
