(defn parse-input
  []
  (map keyword
    (flatten
      (map #(re-seq #"Ko|kO|Kudah|kudah|Kukarek|Kud|kud" %) (line-seq (java.io.BufferedReader. *in*))))))

(defn make-commands
  [commands [current-input :as input]]
  (if (or (empty? input) (= :kud current-input))
    {:commands commands :input input}
    (if (= :Kud current-input)
      (let [{c :commands i :input} (make-commands [] (rest input))]
        (recur (conj commands c) (rest i)))
      (recur (conj commands current-input) (rest input)))))

(defmulti _interpret #(class %2))

(defmethod _interpret clojure.lang.Keyword
  [{:keys [cells current-cell] :as cells-state} command]
  (condp = command
    :Ko {:cells (update-in cells [current-cell] inc) :current-cell current-cell}
    :kO {:cells (update-in cells [current-cell] dec) :current-cell current-cell}
    :Kudah {:cells (if (= (inc current-cell) (count cells)) (conj cells 0) cells) :current-cell (inc current-cell)}
    :kudah {:cells cells :current-cell (dec current-cell)}
    :Kukarek (do
               (print (char (cells current-cell)))
               cells-state)))

(defn interpret
  [cells-state command]
  (reduce _interpret (cons cells-state command)))

(defmethod _interpret clojure.lang.IPersistentVector
  [{:keys [cells current-cell] :as cells-state} command]
  (if (zero? (cells current-cell))
    cells-state
    (recur (interpret cells-state command) command)))

(interpret {:cells [0] :current-cell 0} (:commands (make-commands [] (parse-input))))
