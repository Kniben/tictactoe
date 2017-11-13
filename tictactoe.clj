(ns tictactoe.core)

(defn init-game []
  {:turn (rand-nth [:X :O])
   :board [[:_ :_ :_]
           [:_ :_ :_]
           [:_ :_ :_]]})

(defn cell-empty? [x y board]
  (= :_ (get-in board [y x])))

(defn transpose [board]
  (for [x (range 0 3)] (mapv #(% x) board)))

(defn diagonals [board]
  [(mapv (comp eval list) board (range 0 3))
   (mapv (comp eval list) board (reverse (range 0 3)))])

(defn lines [board]
  (concat board (transpose board) (diagonals board)))

(defn victory? [marker board]
  (->> (lines board)
       (map (fn [line] (every? #(= % marker) line)))
       (some true?)
       some?))

(defn prompt-marker [marker]
  (print (str (name marker) "'s turn to pick cell (1-9): "))
  (flush)
  (let [n (dec (read-string (read-line)))]
    (println)
    [(mod n 3) (quot n 3)]))

(defn print-board [board]
  (dorun (for [row (reverse board)]
           (println (mapv name row)))))

(defn hand-over-turn [game]
  (update-in game [:turn] (fn [old] (next-marker old))))

(defn next-marker [old-marker]
  (cond (= old-marker :O) :X
        (= old-marker :X) :O))

(defn mark [x y game]
  (update-in game [:board y x] (fn [old] (:turn game))))

(defn update-game [game]
  (print-board (:board game))
  (let [[x y] (prompt-marker (:turn game))]
    (if (cell-empty? x y (:board game))
      (->> game
           (mark x y)
           hand-over-turn)
      (do (println x y "is occupied!")
          game))))

(defn game-loop [game]
  (if (or (victory? :O (:board game))
          (victory? :X (:board game)))
    (print-board (:board game))
    (recur (update-game game))))

(defn run []
  (game-loop (init-game)))

;; Tests
(defn assert-for [pred & items]
  (for [item items] (assert (pred item))))

(defn assert-cells [board pred cells]
  (for [[x y] cells] (assert (apply pred [x y board]))))

(let [int-board [[0 1 2]
                 [3 4 5]
                 [6 7 8]]
      empty (:board (init-game))
      diagonal [[:O :_ :_]
                [:_ :O :_]
                [:_ :_ :O]]
      horizontal [[:_ :_ :_]
                  [:_ :_ :_]
                  [:O :O :O]]
      vertical [[:_ :_ :O]
                [:_ :_ :O]
                [:_ :_ :O]]
      corners [[:O :_ :O]
               [:_ :_ :_]
               [:O :_ :O]]
      draw [[:O :X :O]
            [:X :O :X]
            [:O :O :X]]]
  (assert-for #(victory? :O %) diagonal horizontal vertical)
  (assert-for #(not (victory? :O %)) empty corners draw)
  (assert-cells corners cell-empty? [[1 0] [0 1] [1 1] [2 1] [1 2]])
  (assert-cells empty cell-empty? (for [x (range 0 3) y (range 0 3)] [x y])))
