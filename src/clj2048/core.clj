(ns clj2048.core)

(defn transpose
  [v]
  (apply (partial mapv vector) v))

(defn mirror
  [v]
  (mapv (fn [x] (vec (reverse x))) v))

(defn go
  [xs]
  (cond
    (empty? xs) []
    (= 1 (count xs)) xs
    :else
    (let [[x y & rest] xs]
      (if (= x y)
        (cons (+ x y) (go rest))
        (cons x (go (cons y rest)))))))

(defn move-left
  [xs]
  (mapv
   (fn [row]
     (let [l (count row)
           pad (fn [xs] (take l (concat xs (repeat l 0))))]
       (->> row
            (filter (partial not= 0))
            seq
            go
            pad
            vec)))
   xs))

(defn select
  [pred coll]
  (filter (fn [[_ x]] (pred x))
          (map vector (iterate inc 0) coll)))

(defn inject
  [board]
  (let [rows (select (fn [r] (not (empty? (filter zero? r)))) board)]
    (if (empty? rows)
      board
      (let [[y row] (rand-nth rows)
            [x _] (rand-nth (select zero? row))
            val (if (= 0 (rand-int 11)) 4 2)]
        (assoc-in board [y x] val)))))

(defn move-dir
  [dir board]
  (condp = dir
    :left (move-left board)
    :right (-> board
               mirror
               move-left
               mirror)
    :up (-> board
            transpose
            move-left
            transpose)
    :down (-> board
              transpose
              mirror
              move-left
              mirror
              transpose)
    board))

(defn move
  [dir board]
  (let [new (move-dir dir board)]
    (if (= board new)
      board
      (inject new))))
