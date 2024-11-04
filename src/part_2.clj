(ns part-2)

;; precision

(let [a 1.0e50 
      b -1.0e50 
      c 17.0e00]
  [(+ a (+ b c)) (+ (+ a b) c)]) ; [0.0 17.0], precision error

(let [a (rationalize 1.0e50 )
      b (rationalize -1.0e50 )
      c (rationalize 17.0e00)]
  [(+ a (+ b c)) (+ (+ a b) c)]) ; [17N 17N], N is postfix for BigInt

;; metadata and equality

(let [x (with-meta 'goat {:ornery true})
      y (with-meta 'goat {:ornery false})]
  [(= x y) (identical? x y) (meta x) (meta y)]) 
; [true false {:ornery true} {:ornery false}]

;; collection data types: sequentials, maps, sets

; collection itself = composite data

; sequential = ordered collection
; [1 2] (1 2)

; sequence = sequential collection that may/may not exist (lazy) 
; (map inc [1 2])

; seq, api abstraction for collections, stateless iterators
; api: first and rest, and nil as terminating cond

(= [1 2 3] '(1 2 3)) ; true, both sequences w/ same values in same order

(= [1 2 3] #{1 2 3}) ; false, both are not sequences

; seq abstraction is prevalent across clojure

(class {:a 1}) ; clojure.lang.PersistentArrayMap

(class (seq {:a 1})) ; clojure.lang.PersistentArrayMap$Seq

(class (val {:a 1})) ; clojure.lang.APersistentMap$KeySeq

; the changes in classes are implementation details
; they all implement/extend IPersistentMap and ISeq

;; vectors

(into [1 2 3] (range 4 11)) ; [1 2 3 4 5 6 7 8 9 10]

(vector-of :int Math/TAU 2 1.3) ; [6 2 1]

(vector-of :char 97 98 99) ; [\a \b \c]

(vector-of :int 2147483648) ; ArithmeticException: integer overflow

(rseq (mapv char (range 97 123))) ; vectors are fast to reverse

(replace {2 :a, 4 :b} [1 2 3 2 3 4]) ; [1 :a 3 :a 3 :b]

;; stack operations, prefer over assoc, last, dissoc in stacks

(let [stack [1 2 3]]
  [(peek stack)   ; 3
   (pop stack)    ; [1 2]
   (conj stack 4) ; [1 2 3 4]
   stack])        ; [1 2 3]

(time (last (vec (range 100000)))) ; 8.040167 msecs

(time (peek (vec (range 100000)))) ; 1.034417 msecs, doesn't walk through the full sequence

(subvec (mapv char (range 97 123)) 0 5) ; [\a \b \c \d \e], creating is fast due to persistence

;; iterating over hashmaps yields vectors

(for [me {:width 10, :height 20, :depth 15}]
  (vector? me)) ; (true true true), type are clojure.lang.MapEntrys

;; lists

(next '()) ; nil

(rest '()) ; ()

(pop '()) ; IllegalStateException: Can't pop empty list

;; persistent queues

(defn queue [& args]
  (apply conj clojure.lang.PersistentQueue/EMPTY args))

(queue :wake-up :shower :brush-teeth) ; #object[clojure.lang.PersistentQueue 0x7b24fc67 "clojure.lang.PersistentQueue@26810cc8"]

; overload printer multimethod for persistent queues

(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

clojure.lang.PersistentQueue/EMPTY ; <-nil-<

(peek (queue :a :b :c)) ; :a

(pop (queue :a :b :c)) ; <-(:b :c)-<

;; persistent sets

; mathematical sets, collections of unsorted, unique values

(into #{[] #{} {}} [()]) ; #{[] #{} {}}, [] and () fall under the same equality partition and are de-duped

(some #{1 :b} [:a 1 :b 2]) ; 1, not nil, idiomatic contains search

(sorted-set :b 2 :c :a 3 1) ; ClassCastException: class clojure.lang.Keyword cannot be cast to class java.lang.Number

(contains? #{1 2 4 3} 4) ; true

(contains? [1 2 4 3] 4) ; false, contains? expects given key exisits

;; maps

(let [m {:a 1 
         2 :b
         [4 5 6] "7 8 9"}]
  [(m :a) (m 2) (m [4 5 6])]) ; [1 :b "7 8 9"], heterogeneous keys


(seq {:a 1, :b 2}) ; ([:a 1] [:b 2]), returns sequence of vectors 

(into {} [[:a 1] [:b 2]]) ; {:a 1, :b 2}

(zipmap [:a :b] [1 2]) ; {:a 1, :b 2}, ordering is not guaranteed

;; sorted maps

(sorted-map "a" 1, "b" 2) ; {"a" 1, "b" 2}, sorted by initial type of keys

(sorted-map "a" 1, :b 2) ; ClassCastException: class java.lang.String cannot be cast to class clojure.lang.Keyword

(sorted-map "bac" 2 "abc" 9) ; {"abc" 9, "bac" 2}

(sorted-map-by 
  (fn [a b] (compare (subs a 1) (subs b 1)))
  "bac" 2
  "abc" 9) ; {"bac" 2, "abc" 9}

(assoc (sorted-map 1 :int) 1.0 :float) ; {1 :float}, when keys equal, only one is kept

;; array maps

(seq (hash-map :a 1, :b 2, :c 3)) ; ([:c 3] [:b 2] [:a 1])

(seq (array-map :a 1, :b 2, :c 3)) ; ([:a 1] [:b 2] [:c 3]), ensures insertion order

;; pos fn

; work on any collection type
; find and return indicies of some passed-in value
  ; numerical index for sequential collections
  ; key for associated collections
; return nil for not found

; first cut

(defn pos [e coll]
  (let [cmp (if (map? coll)
              #(= (second %1) %2)
              #(= %1 %2))]
    (loop [s coll idx 0]
      (when (seq s)
        (if (cmp (first s) e)
          (if (map? coll)
            (first (first s)) idx)
          (recur (next s) (inc idx)))))))

(pos 3 [:a 1 :b 2 :c 3 :d 4]) ; 5
(pos 3 {:a 1 :b 2 :c 3 :d 4}) ; :c
(pos \3 ":a 1 :b 2 :c 3 :d 4") ; 13

; second cut

(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll))) ; or (map-indexed vector coll)

(index [:a 1 :b 2 :c 3]) ; ([0 :a] [1 1] [2 :b] [3 2] [4 :c] [5 3])

(index {:a 1 :b 2 :c 3}) ; ([:a 1] [:b 2] [:c 3])

(index #{:a 1 :b 2 :c 3}) ; ([1 1] [:c :c] [3 3] [2 2] [:b :b] [:a :a])

(defn pos [e coll]
  (for [[i v] (index coll)
        :when (= e v)]
    i))

(pos 3 [:a 1 :b 2 :c 3 :d 4]) ; (5)
(pos 3 {:a 1, :b 2, :c 3, :d 4}) ; (:c)
(pos 3 [:a 3 :b 3 :c 3 :d 4]) ; (1 3 5)
(pos \3 ":a 1 :b 2 :c 3 :d 4") ; (13)

; final cut, add predicate param to allow for any dimensional checks

(defn pos [pred coll]
  (for [[i v] (index coll)
        :when (pred v)]
    i))

(pos even? [2 3 6 7]) ; (0 2)
(pos #{3 4} {:a 1 :b 2 :c 3 :d 4}) ; (:c :d)
(pos #(= % 3) [:a 3 :b 3 :c 3 :d 4]) ; (1 3 5), works as before
