(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        f (str fst)]
    (if (Character/isDigit fst)
      (Integer/valueOf f)
      (get symbol_ranks f))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn n-of-a-kind? [n hand]
  (== n (apply max (rank-frequencies hand))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (rank-frequencies hand))))

(defn two-pairs? [hand]
  (let [rr (sort (rank-frequencies hand))]
    (cond
      (= [1 4] rr) true
      (= [1 2 2] rr) true
      :else false)))

(def range_5 (range 5))

(defn shifted_range_5? [x]
  (= [5] (vals (frequencies (map - x range_5)))))

(defn straight? [hand]
  (let [f (sort (map rank hand))]
    (if (shifted_range_5? f)
      true
        (shifted_range_5? (sort (replace {14 1} f))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))  

(defn filter-values [res]
  (filter (fn [[r v]] r) res))

(defn value [hand]
  (apply max (map (fn [[r v]] v)
                  (filter-values
                    (map (fn [x] (test_hand_value hand x))
                         values)))))

(defn test-hand-value [hand [f v]]
  [(f hand) v])

(defn high-card? [hand]
  true)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(def symbol-ranks {
                   "T" 10
                   "J" 11
                   "Q" 12
                   "K" 13
                   "A" 14
                   })

(def values {
             high-card?             0
             pair?                  1
             two-pairs?             2
             three-of-a-kind?       3
             straight?              4
             flush?                 5
             full-house?            6
             four-of-a-kind?        7
             straight-flush?        8
             })

; For testing purpose.
(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

