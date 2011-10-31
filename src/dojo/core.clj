;;including clojure.contrib.string for split, split-lines functions
(ns dojo.core
  :require [clojure.contrib.string :as string])

(def card-values {"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "10" 10 "J" 11 "Q" 12 "K" 13 "A" 14})

(defn grok-card [s]
  "Takes a card as original representation and splits it into ['number', 'suite'],
so (first card) gives the number and (last card) gives the suite, first and last statements
appear in a lot of places, perhaps a proper structure instead of vector would have made it
more readable"
  (let [card-parts (rest (re-matches #"(^\d+|^\w)(\w+)$" s))]
    [(card-values (first card-parts)) (last card-parts)]))

(defn split-grok-hand [hand-string]
  (take 2 (map #(map grok-card (string/split #" " %)) hand-string)))

;;Loads the hands from lines and splits it up into card structure (eg:
;;[4 "Spades"] ), the structure returned is a list of (input,
;;expected) pairs
(defn load-hands [file-name]
  (map split-grok-hand
       (partition 3 (string/split-lines (slurp file-name)))))

(defn four-of-a-kind [sorted-hand]
  (some #(cond (= 4 (count %)) %) (vals (group-by #(first %) sorted-hand))))

(defn pairs [sorted-hand]
  (let [grouped-vals (vals (group-by #(first %) sorted-hand))]
    (reduce #(if (> (count %2) 1) (concat %1 %2) %1) [] grouped-vals)))

;;Use reduce to Check if adjacent card "number" (first card) increments by 1
(defn straight? [sorted-hand]
  (reduce #(cond (nil? %1) nil
                 (= (inc (first %1)) (first %2)) %2)
          sorted-hand))

(defn flush? [sorted-hand]
  (every? #(= (last (first sorted-hand)) (last %)) (rest sorted-hand)))

(defn best-hand [sorted-hand]
  (let [pairs-all (pairs sorted-hand)]
  (cond (and (flush? sorted-hand) (straight? sorted-hand)) sorted-hand
        (four-of-a-kind sorted-hand) pairs-all
        (flush? sorted-hand) sorted-hand
        (straight? sorted-hand) sorted-hand
        (not-empty pairs-all) pairs-all
        true (merge `() (last sorted-hand)))))

;;Takes the given, expected hands and checks if the sort / best-hand
;;implementation matches the expected output.
;; Making use of the fact that sort happens over values of the list.
(defn check-hands-sorted [hand-pair-lines]
  (= (last hand-pair-lines) (sort (first hand-pair-lines))))

(defn check-hands-best [hand-pair-lines]
  (= (last hand-pair-lines) (best-hand (sort (first hand-pair-lines)))))

;; Check whether every output matches with the given output.
(every? check-hands-sorted (load-hands "resources/poker_hands-sorted.txt"))
(every? check-hands-best (load-hands "resources/poker_hands-best.txt"))

