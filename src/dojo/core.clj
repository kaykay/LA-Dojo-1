(ns dojo.core
  :require [clojure.contrib.string :as string])

(def card-values {"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "10" 10 "J" 11 "Q" 12 "K" 13 "A" 14})

(defn grok-card [s]
  (let [card-parts (rest (re-matches #"(^\d+|^\w)(\w+)$" s))]
    [(card-values (first card-parts)) (last card-parts)]))

(defn split-grok-hand [hand-string]
  (take 2 (map #(map grok-card (string/split #" " %)) hand-string)))

(defn load-hands [file-name]
  (map split-grok-hand
       (partition 3 (string/split-lines (slurp file-name)))))

(defn four-of-a-kind [hand]
  (some #(cond (= 4 (count %)) %) (vals (group-by #(first %) hand))))

(defn pairs [hand]
  (let [grouped-vals (vals (group-by #(first %) hand))]
    (reduce #(if (> (count %2) 1) (concat %1 %2) %1) [] grouped-vals)))

(defn straight? [hand]
  (reduce #(cond (nil? %1) nil
                 (= (inc (first %1)) (first %2)) %2)
          hand))

(defn flush? [hand]
  (every? #(= (last (first hand)) (last %)) (rest hand)))

(defn best-hand [hand]
  (let [pairs-all (pairs hand)]
  (cond (and (flush? hand) (straight? hand)) hand
        (four-of-a-kind hand) pairs-all
        (flush? hand) hand
        (straight? hand) hand
        (not-empty pairs-all) pairs-all
        true (merge `() (last hand)))))

(defn check-hands-sorted [hand-pair-lines]
  (= (last hand-pair-lines) (sort (first hand-pair-lines))))

(defn check-hands-best [hand-pair-lines]
  (= (last hand-pair-lines) (best-hand (sort (first hand-pair-lines)))))

(every? check-hands-sorted (load-hands "resources/poker_hands-sorted.txt"))
(every? check-hands-best (load-hands "resources/poker_hands-best.txt"))

