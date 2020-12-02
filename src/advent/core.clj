(ns advent.core)

(def input
  (with-open [r (clojure.java.io/reader "/Users/matt/src/github/mattford63/clojure/advent-clj/src/advent/input")]
    (->> r
         line-seq
         (map #(Integer/parseInt %) )
         set
         doall)))

;; recursive building up an accumulator and separate state
(defn expense
  ([s t]
   (expense s t #{} []))

  ([s t state acc]
   (if (empty? s)
     acc
     (let [f (first s)
           d (- t f)
           acc (if (contains? state d)
                 (conj acc (* f d))
                 acc)]
       (recur (rest s) t (conj state f) acc)))))


;; a natural lazy'ish approach - breaks the stack, but not entirely sure why, rewritten with step below
(defn lazy-expense
  ([s t]
   (lazy-expense s t #{}))
  ([s t state]
   (remove nil? (lazy-seq
                 (when-let [f (first s)]
                   (cons
                    (let [d (- t f)]
                      (when (contains? state d)
                        (* d f)))
                    (lazy-expense (rest s) t (conj state f))))))))


;; use a step function to recur to the next stage when cons isn't what we want
(defn lazy-expense'
  ([s t]
   (lazy-expense' s t #{}))
  ([s t state]
   (let [step (fn [s t state]
                (when-let [f (first s)]
                  (let [d (- t f)]
                    (if (contains? state d)
                      (cons (* d f) (lazy-expense' (rest s) t (conj state f)))
                      (recur (rest s) t (conj state f))))))]
     (lazy-seq (step s t state)))))


;; via reduce and a complex accumulator to manage state and result
(defn reduce-expense [s t]
  (reduce (fn [acc x]
                (let [{:keys [result state]} acc
                      d (- t x)]
                  {:result (if (contains? state d)
                             (conj result (* x d))
                             result)
                   :state (conj state x)}))
              {:result []
               :state #{}}
              s))

;; use a short-circuit
(defn reduce-expense-sc [s t]
  (reduce (fn [acc x]
                (let [{:keys [result state]} acc
                      d (- t x)]
                  (if (contains? state d)
                    (reduced (* x d))
                    {:result result
                     :state (conj state x)})))
              {:result []
               :state #{}}
              s))

;; Run
(expense input 2020)
(reduce-expense input 2020)
(reduce-expense-sc (range) 2020) ;; has a short-circuit - but only 1 result
(take 800 (lazy-expense (range) 2020)) ;; stack-overflow WHY?
(take 800 (lazy-expense' (range) 2020))
(lazy-expense' input 2020)
