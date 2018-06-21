(ns scrabble
  (:require [clojure.string :as str]))

;;;; my version
(def score-nums (str/split "1 3 3 2 1 4 2 4 1 8 5 1 3 1 1 3 10 1 1 1 1 4 4 8 4 10" #" "))
(def scores (zipmap (map char (range 97 123)) (map read-string score-nums)))

(def words
  (-> "resources/words4k.txt"
   slurp
   (str/split  #"\r\n")
   (->> (map clojure.string/lower-case ))))

(defn word-fits? [tiles word]
  (-> tiles
      frequencies
      ((partial merge-with min (select-keys (frequencies word) tiles)))
      (select-keys word)
      vals
      (->> (reduce + (get (frequencies tiles) \* 0))
           (<= (count word)))))

(defn score-word [word] (- (reduce + (map scores word))))

(defn get-best-plays [tiles words]
  (->> (filter (partial word-fits? tiles) words)
       (sort-by score-word)))

(get-best-plays "xboedif" words)

;;;; other dude's version
(defn scramboni [rack]
  "Find all playable words in a given Scrabble rack.  Use asterisks for blanks."
  (letfn [(every-but-n? [n pred coll]
            (->> coll
                 (map (comp (fn [x] (if x :true :false)) pred))
                 frequencies
                 (merge {:false 0})
                 :false
                 (>= n)))]
    (let [scrabble-words (->>  "resources/words4k.txt"
                              slurp
                              clojure.string/split-lines
                              (map clojure.string/lower-case))
          rack-frequencies (frequencies rack)
          template (zipmap "abcdefghijklmnopqrstuvwxyz" (repeat 26 0))
          points {\a 1 \b 3 \c 3 \d 2 \e 1 \f 4 \g 2 \h 4 \i 1 \j 8 \k 5 \l 1 \m 3
                  \n 1 \o 1 \p 3 \q 10 \r 1 \s 1 \t 1 \u 1 \v 4 \w 4 \x 8 \y 4 \z 10}
          num-asterisks (if-let [n (rack-frequencies \*)] n 0)
          frequencies-to-use (merge template rack-frequencies)
          filtered-words (filter
                           (fn [word] (every-but-n? num-asterisks
                                                    #(<= ((frequencies word) %)
                                                         (frequencies-to-use %))
                                                    (seq word)))
                           scrabble-words)
         filtered-word-scores (map #(reduce + (map points (seq %))) filtered-words)]
      (->> (zipmap filtered-words filtered-word-scores)
           (sort-by val)
           reverse))))
(scramboni "libernty")


;;;; add polys - golfed version, not working yet ;;;;
(defn add-polynomials [p1 p2]
  (str/join " + "
            (map (fn [[k v]] (apply str [(apply + (map #(-> % coefficient str read-string) v)) (apply str k)]))
                 (group-by power (str/split (str p2 " + " p1)  #" \+ ")))))


(group-by power (str/split (str poly2 " + " poly1)  #" \+ "))
(add-polynomials poly1 poly2)
(reduce add-polynomials [poly1 poly2 poly2])
(take-last 2 "5x")
(first "5x2")

(def p1 "3x2")
(def p2 "3x")
(def p3 "x2")
(def p4 "x")
(def p5 "5")


;;;;;; add polys complete ;;;;;;
(def poly1  "15x3 + 7x + 2x2 + 8x + 9x3 + 5 + 7")
(def poly2  "5x3 + 7x + 2x2 + 8x + 9x3 + 5")

(defn string-is-number [s] (boolean (re-matches #"\d+" (str s))))

(defn operand->number [operand]
  (let [[left right] (str/split operand #"x")]
    [(read-string left) 0 (read-string right)]))

(defn normalize-operand [x]
  (case (count x)
    2 (if (string-is-number (last x)) (str "0" x) (str x "1"))
    1 (if (string-is-number x) (str x "x0") (str "0" x "1"))
     x))

(defn add-poly [p1 p2]
  (->> (str/split (str p1 " + " p2) #" \+ " )
       (map normalize-operand)
       (map operand->number)
       (group-by last)
       (map (fn [[power operands]]
              [(reduce + (first (apply map vector operands))) "x" power ]))
       (map #(apply str %))
       (str/join " + ")))

(add-poly poly1 poly2)

(defn add-polys [& polys] (reduce add-poly polys))
(add-polys poly1 poly2 poly2 poly2 poly1 poly1)
