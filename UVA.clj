(ns UVA)

;; UVA Problem 167
;; The eight queens problem involves placing eight queens on a chess board in
;; such a manner that no queen is attacking any other queen. That is that no
;; queen shares the same row, column or diagonal as any other queen.

;; +---+---+---+---+---+---+---+---+    Here is one of the 92 different
;; |   |   |   | Q |   |   |   |   |    solutions to the puzzle for a board in
;; +---+---+---+---+---+---+---+---+    a particular orientation; there are 12
;; | Q |   |   |   |   |   |   |   |    unique configurations, each of which
;; +---+---+---+---+---+---+---+---+    can be rotated or reflected in various.
;; |   |   |   |   | Q |   |   |   |    ways to give the others.
;; +---+---+---+---+---+---+---+---+
;; |   |   |   |   |   |   |   | Q |    A solution can be represented by a
;; +---+---+---+---+---+---+---+---+    vector of length eight, representing
;; |   |   |   |   |   | Q |   |   |    the eight rows, with each element
;; +---+---+---+---+---+---+---+---+    being the offset of the queen in that
;; |   |   | Q |   |   |   |   |   |    row.
;; +---+---+---+---+---+---+---+---+
;; |   |   |   |   |   |   | Q |   |    For example, the solution shown here
;; +---+---+---+---+---+---+---+---+    would be represented by the the vector
;; |   | Q |   |   |   |   |   |   |    [3 0 4 7 5 2 6 1].
;; +---+---+---+---+---+---+---+---+

;; The actual problem to solve is, given as input an array of 64 numeric
;; values which can be mapped to the squares of a chess board, determine
;; which sum of the values taken from the eight squares corresponding to all
;; solutions of the eight queens problem, is the maximum such value.

;; The eight queens puzzle can be solved by considering the board as eight
;; rows and adding a single queen to each row in turn whilst ensuring that
;; the puzzle constraints are maintained. Once a situation is reached where
;; it is impossible to add an additional queen, recursively backtrack by
;; removing one or more queens from previous rows until a queen can be placed
;; in a new unchecked position. Once all rows are filled, that comprises one
;; possible solution.

;; A helper function that checks that adding a queen in the "col" column of
;; the next row of a partial solution does not violate the constraints of the
;; queens puzzle.

;; +---+---+---+---+---+---+---+---+    Here "rows" is '(1 4 2) and "cols" is
;; |   |   | Q |   |   |   |   |   |    6, meaning we're testing if a queen
;; +---+---+---+---+---+---+---+---+    is valid at the indicated square.
;; |   |   |   |   | Q |   |   |   |
;; +---+---+---+---+---+---+---+---+    Note how "rows" is ordered with the
;; |   | Q |   |   |   |   |   |   |    offset of the most recently added
;; +---+---+---+---+---+---+---+---+    queen at the front of the list.
;; |   |   |   |   |   |   | ? |   |
;; +---+---+---+---+---+---+---+---+

(defn valid? [rows col]
  (letfn [
          (satisfies? [[r c]]   ;; Existing queen at row r, column c
            (and (not= c col) (not= col (dec (- c r))) (not= col (inc (+ c r)))))]

    (every? satisfies? (map-indexed vector rows))))

;; Generate all 92 solutions to the puzzle as a lazy sequence.

(defn solve [rows col]
  (when (not (and (= col 8) (empty? rows)))
    (if (= col 8)
      (recur (rest rows) (inc (first rows)))
      (if (= (count rows) 8)
        (lazy-seq (cons
                    (vec (reverse rows))
                    (solve (rest rows) (inc (first rows)))))
        (if (valid? rows col)
          (recur (conj rows col) 0)
          (recur rows (inc col)))))))

;; The same algorithm rewritten using the "cond" function.

(defn solve [rows col]
  (when (not (and (= col 8) (empty? rows)))
    (cond
      (= col 8)
      (recur (rest rows) (inc (first rows)))

      (= (count rows) 8)
      (lazy-seq (cons
                  (vec (reverse rows))
                  (solve (rest rows) (inc (first rows)))))

      (valid? rows col)
      (recur (conj rows col) 0)

      :else
      (recur rows (inc col)))))

;; All 92 solutions as a lazy sequence.

(def solutions (solve () 0))

;; Take a single solution and transform it to a sequence of eight, zero based
;; offsets into a board represented as a vector of length 64.

;; (solution-to-offsets [6 4 2 0 5 7 1 3]) => (6 12 18 24 37 47 49 59)

;; 00 01 02 03 04 05 06 07    _ _ _ _ _ _ Q _
;; 08 09 10 11 12 13 14 15    _ _ _ _ Q _ _ _
;; 16 17 18 19 20 21 22 23    _ _ Q _ _ _ _ _
;; 24 25 26 27 28 29 30 31    Q _ _ _ _ _ _ _
;; 32 33 34 35 36 37 38 39    _ _ _ _ _ Q _ _
;; 40 41 42 43 44 45 46 47    _ _ _ _ _ _ _ Q
;; 48 49 50 51 52 53 54 55    _ Q _ _ _ _ _ _
;; 56 57 58 59 60 61 62 63    _ _ _ Q _ _ _ _

(defn solution-to-offsets [solution]
  (map (fn [[r c]] (+ (* 8 r) c)) (map-indexed vector solution)))

;; Given a board and a solution, return the sum of the board values at offsets
;; corresponding to the solution.

(defn solution-sum [board solution]
  (reduce + (map board (solution-to-offsets solution))))

;; For a given board, determine the maximum value of the solution sums as
;; described above, for all possible solutions.

(defn max-solutions-sum [board]
  (apply max (map (partial solution-sum board) solutions)))

;; The board given as an example.

(def board [
            1  2  3  4  5  6  7  8    ;; This board has the unusual property that the
            9 10 11 12 13 14 15 16    ;; sum of the values of those squares corresponding
            17 18 19 20 21 22 23 24    ;; to any of the 92 possible solutions to the eight
            25 26 27 28 29 30 31 32    ;; queens problems, all have the same sum of 260.
            33 34 35 36 37 38 39 40
            41 42 43 44 45 46 47 48
            49 50 51 52 53 54 55 56
            57 58 59 60 61 62 63 64
            ])

(assert (= (max-solutions-sum board) 260))

(def twos-board [
                 1  1  1  1  1  1  2  1    ;; This board has the value 2 at those offsets
                 1  1  1  1  2  1  1  1    ;; which correspond to [6 4 2 0 5 7 1 3] as shown
                 1  1  2  1  1  1  1  1    ;; above. The maximum sum found should therefore
                 2  1  1  1  1  1  1  1    ;; be for this solution and have value 16.
                 1  1  1  1  1  2  1  1
                 1  1  1  1  1  1  1  2
                 1  2  1  1  1  1  1  1
                 1  1  1  2  1  1  1  1])

(assert (= (max-solutions-sum twos-board) 16))

;; Further testing will be split into two parts; showing that the solutions
;; generated to the eight queens problem are in fact solutions, and testing
;; that the algorithm to determine the maximum sum is working.

;; To show that the generated solutions are correct, take each solution in
;; turn and show that each of the eight squares is unique by row, column and
;; both diagonals.

(def lines [
            ;; Rows
            #{ 0  1  2  3  4  5  6  7}
            #{ 8  9 10 11 12 13 14 15}
            #{16 17 18 19 20 21 22 23}
            #{24 25 26 27 28 29 30 31}
            #{32 33 34 35 36 37 38 39}
            #{40 41 42 43 44 45 46 47}
            #{48 49 50 51 52 53 54 55}
            #{56 57 58 59 60 61 62 63}

            ;; Columns
            #{ 0  8 16 24 32 40 48 56}
            #{ 1  9 17 25 33 41 49 57}
            #{ 2 10 18 26 34 42 50 58}
            #{ 3 11 19 27 35 43 51 59}
            #{ 4 12 20 28 36 44 52 60}
            #{ 5 13 21 29 37 45 53 61}
            #{ 6 14 22 30 38 46 54 62}
            #{ 7 15 23 31 39 47 55 63}

            ;; Top Left to Bottom Right Diagonals
            #{ 0  9 18 27 36 45 54 63}
            #{ 1 10 19 28 37 46 55}
            #{ 2 11 20 29 38 47}
            #{ 3 12 21 30 39}
            #{ 4 13 22 31}
            #{ 5 14 23}
            #{ 6 15}
            #{ 8 17 26 35 44 53 62}
            #{16 25 34 43 52 61}
            #{24 33 42 51 60}
            #{32 41 50 59}
            #{40 49 58}
            #{48 57}

            ;; Top Right to Bottom Left Diagonals
            #{ 1  8}
            #{ 2  9 16}
            #{ 3 10 17 24}
            #{ 4 11 18 25 32}
            #{ 5 12 19 26 33 40}
            #{ 6 13 20 27 34 41 48}
            #{ 7 14 21 28 35 42 49 56}
            #{15 22 29 36 43 50 57}
            #{23 30 37 44 51 58}
            #{31 38 45 52 59}
            #{39 46 53 60}
            #{47 54 61}
            #{55 62}])

;; Take the solutions in the standard form that has been used so far and
;; convert them to a sequence of 92 sets of the offsets of the squares in
;; each solution.

(def solution-sets (map (comp set solution-to-offsets) solutions))

;; Now we can lazily generate a sequence that comprises the intersections of
;; the cartesian product of the 'lines' and 'solution-sets' sequences and
;; assert that each such intersection can have at most a single element.

;; This in effect proves that the solutions generated to the eight queens
;; problem are all valid as per the definition of the problem.

(assert
  (every? (fn [s] (<= (count s) 1))
          (for [i lines, j solution-sets] (clojure.set/intersection i j))))

;; It remains to test that the 'max-solutions-sum' function gives the same
;; value for randomly generated boards as does using the 92 solutions to
;; determine this value directly.

(def solutions-as-offsets (map solution-to-offsets solutions))

(defn max-sum-using-solutions [board]
  (apply max
         (map (fn [offsets] (reduce + (map board offsets))) solutions-as-offsets)))

;; Notice here the use of functions with no arguments to force the evaluation
;; of expressions containing side effects,

(defn random-board [] (vec (repeatedly 64 (fn [] (inc (rand-int 99))))))

;; Generate 1000 random boards and check that the maximum sum value given by
;; the solution function corresponds to that calculated directly.

(def test-count 1000)

(loop [count test-count]
  (when (pos? count)
    (let [
          board (random-board)
          s (max-solutions-sum board)
          t (max-sum-using-solutions board)]

      (assert (= s t))
      (recur (dec count)))))


