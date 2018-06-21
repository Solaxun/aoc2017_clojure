(def virus
"#.##.###.#.#.##.###.##.##
.##.#.#.#..####.###.....#
...##.....#..###.#..#.##.
##.###.#...###.#.##..##.#
###.#.###..#.#.##.#.###.#
.###..#.#.####..##..#..##
..###.##..###.#..#...###.
........##..##..###......
######...###...###...#...
.######.##.###.#.#...###.
###.##.###..##..#..##.##.
.#.....#.#.#.#.##........
#..#..#.#...##......#.###
#######.#...#..###..#..##
#..#.###...#.#.#.#.#....#
#.#####...#.##.##..###.##
..#..#..#.....#...#.#...#
###.###.#...###.#.##.####
.....###.#..##.##.#.###.#
#..#...######.....##.##.#
###.#.#.#.#.###.##..###.#
..####.###.##.#.###..#.##
#.#....###....##...#.##.#
###..##.##.#.#.##..##...#
#.####.###.#...#.#.....##")


#_(def virus
"..#
#..
...")


(defn virus-map [text]
  (map-indexed #(when (= %2 \#) %1) text))

(def grid (mapv vec (clojure.string/split virus #"\n")))

(defn node-map [grid]
  (into {} (apply concat (map-indexed (fn [i row]
                                        (map-indexed
                                         (fn [j col] (if (= col \#) [[i j] :I]
                                                        [[i j] :C])
                                           )row)
                                        )grid))))

(defn infections [grid]
  (->> grid
       get-infected-locs
       (remove nil?)
       set))

(defn middle [grid]
  (repeat 2 (int (/ (count grid) 2))))

(defn transition [facing turn]
   (let [d {:U {:L :L :R :R}
            :L {:L :D :R :U}
            :D {:L :R :R :L}
            :R {:L :U :R :D}}]
          (get-in d [facing turn])))

(defn propagate-virus [grid n]
  (loop [loc (middle grid)
        facing :U
        dirmap {:U [-1 0] :L [0 -1] :D [1 0] :R [0 1]}
        infected? (infections grid)
        infected-count 0
        n n]
      (cond ;; case 1
            (zero? n)
            infected-count
            ;; case 2
            (infected? loc)
            (recur (map + loc (dirmap (transition facing :R)))
             (transition facing :R)
             dirmap
             (disj infected? loc)
             infected-count
             (dec n))
           ;; case 3
           :else
           (recur (map + loc (dirmap (transition facing :L)))
             (transition facing :L)
             dirmap
             (conj infected? loc)
             (inc infected-count)
             (dec n)))))

; (propagate-virus grid 10000)

(defn propagate-virus-2 [grid n]
  (loop [loc (middle grid)
        facing :U
        current-status :I
        nodes (node-map grid)
        infected-count 0
        n n]
      (if (zero? n)
          infected-count
          (let [dirmap {:U [-1 0] :L [0 -1] :D [1 0] :R [0 1]}
                status-map {:C :W :W :I :I :F :F :C}
                reverse-dirs {:U :D :D :U :L :R :R :L}
                new-status (status-map current-status)
                turns {:C (transition facing :L) :W facing :I (transition facing :R) :F (reverse-dirs facing)}
                facing (turns current-status)]
              ; (println facing  )
          (recur (map + loc (dirmap facing))
                facing
                (nodes (map + loc (dirmap facing)) :C)
                (assoc nodes loc new-status)
                (if (= new-status :I)
                    (inc infected-count)
                    infected-count)
                (dec n))))))

;(propagate-virus-2 grid 100)
(propagate-virus-2 grid 10000000)
