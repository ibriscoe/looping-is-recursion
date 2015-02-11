(ns looping-is-recursion)

(defn power-helper [acc base exp]
  (if (zero? exp)
    acc
    (recur (* acc base) base (dec exp))))

(defn power [base exp]
  (power-helper 1 base exp))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn last-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (= 1 (count a-seq))
      (first a-seq)
    :else
      (recur (rest a-seq))))

(last-element [])      ;=> nil
(last-element [1 2 3]) ;=> 3
(last-element [2 5])   ;=> 5

(defn seq=helper [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))
      true
    (not= (first seq1) (first seq2))
      false
    :else
      (recur (rest seq1) (rest seq2))))

(defn seq= [seq1 seq2]
  (if (not= (count seq1) (count seq2))
    false
    (seq=helper seq1 seq2)))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [] [])             ;=> true
(seq= [1 2 nil] [1 2])   ;=> false
(seq= [1 4 2] [1 2 4])   ;=> false
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false)

(defn find-first-index [pred a-seq]
  (loop [acc 0
         the-seq a-seq]
    (cond
      (empty? the-seq)
        nil
      (pred (first the-seq))
        acc
      :else
        (recur (inc acc) (rest the-seq)))))

(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])                                    ;=> nil

(defn avg [a-seq]
  (loop [total 0
         items 0
         the-seq a-seq]
    (cond
      (empty? the-seq)
        (if (zero? items)
          nil
          (/ total items))
      :else
        (recur (+ total (first the-seq)) (inc items) (rest the-seq)))))


(avg [])
(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         the-seq a-seq]
    (if (empty? the-seq)
      acc
      (recur (toggle acc (first the-seq)) (rest the-seq)))))

(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}

(defn fast-fibo [n]
  (loop [f-n+1 1
         f-n 0
         i 0]
    (cond
      (= i n)
        f-n
      :else
        (recur (+ f-n f-n+1) f-n+1 (inc i)))))


(fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=>
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8

(defn cut-at-repetition [a-seq]
  (loop [the-seq a-seq
         items #{}
         acc []]
    (cond
      (empty? the-seq)
        acc
      (contains? items (first the-seq))
        acc
      :else
        (recur (rest the-seq) (conj items (first the-seq)) (conj acc (first the-seq))))))

(cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]
