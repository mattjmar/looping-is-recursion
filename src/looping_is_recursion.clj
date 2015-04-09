(ns looping-is-recursion)

(defn power [base exp]
  (loop [result 1
         b base
         e exp]
    (if (<= e 0)
      result
      (recur (* result b) b (dec e)))))


(defn last-element [a-seq]
  (if (<=(count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (empty? seq1) (empty? seq2)
   (empty? seq2) (empty? seq1)
   (not(= (first seq1)(first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         p pred
         l-seq a-seq]
   (cond
     (empty? l-seq) nil
     (pred (first l-seq)) index
     :else (recur (inc index) p (rest l-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         l-seq a-seq]
    (if (empty? l-seq)
      (/ sum count)
      (recur (+ sum (first l-seq)) (inc count) (rest l-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (loop [l-set #{}
         l-seq a-seq]
    (if (empty? l-seq)
      l-set
      (recur (toggle l-set (first l-seq)) (rest l-seq)))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         ind (- n 2)]
    (cond
     (== n 0) 0
     (== n 1) 1
     (== ind 0) (+ a b)
     :else (recur b (+ a b) (dec ind)))))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn cut-at-repetition [a-seq]
  (loop [result []
         l-seq a-seq]
    (cond
     (empty? l-seq) result
     (in? result (first l-seq)) result
     :else (recur (conj result (first l-seq)) (rest l-seq)))))

