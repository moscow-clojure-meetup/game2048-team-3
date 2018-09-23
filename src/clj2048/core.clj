(ns clj2048.core
  (:gen-class))

(defn transpose
  [v]
  (apply (partial mapv vector) v))

(defn mirror
  [v]
  (mapv (fn [x] (vec (reverse x))) v))

(defn move-row
  [xs]
  (cond
    (empty? xs) []
    (= 1 (count xs)) xs
    :else
    (let [[x y & rest] xs]
      (if (= x y)
        (cons (+ x y) (move-row rest))
        (cons x (move-row (cons y rest)))))))

(defn move-left
  [xs]
  (mapv
   (fn [row]
     (let [l (count row)
           pad (fn [xs] (take l (concat xs (repeat l 0))))]
       (->> row
            (filter (partial not= 0))
            seq
            move-row
            pad
            vec)))
   xs))

(defn select
  [pred coll]
  (filter (fn [[_ x]] (pred x))
          (map vector (iterate inc 0) coll)))

(defn inject
  [board]
  (let [rows (select
              (fn [r] (not (empty? (filter zero? r)))) board)]
    (if (empty? rows)
      board
      (let [[y row] (rand-nth rows)
            [x _] (rand-nth (select zero? row))
            val (if (= 0 (rand-int 11)) 4 2)]
        (assoc-in board [y x] val)))))

(defn move-dir
  [dir board]
  (case dir
    :left
    (move-left board)

    :right
    (-> board
        mirror
        move-left
        mirror)
    :up
    (-> board
        transpose
        move-left
        transpose)

    :down
    (-> board
        transpose
        mirror
        move-left
        mirror
        transpose)

    board))

(defn game-over?
  [board]
  (every?
   (partial = board)
   (map (fn [d] (move-dir d board))
        [:up :down :left :right])))

(defn solved?
  [board]
  (boolean (some (partial = 2048) (apply concat board))))

(defn new-game
  []
  (-> (vec (repeat 4 (vec (repeat 4 0))))
      inject
      inject))

(defn move
  [dir board]
  (let [new (let [moved (move-dir dir board)]
              (if (= board moved)
                board
                (inject moved)))]
    [new (cond
           (solved? new) :win
           (game-over? new) :lose
           :else :continue)]))

(defn draw-board
  [board]
  (let [bar (fn [] (println "+----+----+----+----+"))]
    (bar)
    (doseq [row board]
      (doseq [c row]
        (if (zero? c)
          (print "|    ")
          (printf "|%4d" c)))
      (println "|")
      (bar))))

(defn play
  [board]
  (draw-board board)
  (println "[u/d/l/r or q]")
  (let [cmd (read-line)
        continue-with
        (fn [dir]
          (let [[new state] (move dir board)]
            (case state
              :lose #(println "You lose!")
              :win #(println "You WIN!")
              #(play new))))]
    (case cmd
      "u" (continue-with :up)
      "d" (continue-with :down)
      "l" (continue-with :left)
      "r" (continue-with :right)
      "q" #(println "Bye!")
      #(play board))))

(defn -main
  [& _]
  (trampoline play (new-game)))
