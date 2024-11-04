(ns part-3
  (:require [clojure.test :as test]))

(def base (list :barnabas :adam))

(def list-1 (conj base :willie)) ; (:willie :barnabas :adam)

(def list-2 (conj base :phoenix)) ; (:phoenix :barnabas :adam)

(= (next list-1) (next list-2)) ; true, values shared

(identical? (next list-1) (next list-2)) ; true, structurally shared

;; tree 

{:val 5, :L nil, :R nil} ; simple tree

(defn xconj [t v]
  (cond
    (nil? t) {:val v :L nil :R nil})) ; base case

(xconj nil 5) ; {:val 5, :L nil, :R nil}

(defn xconj [t v]
  (cond
    (nil? t) {:val v :L nil :R nil}
    (< v (:val t)) {:val (:val t) 
                    :L (xconj (:L t) v)
                    :R (:R t)})) ; push left

(def tree-1 (xconj nil 5))

(def tree-2 (xconj tree-1 3)) ; {:val 5, :L {:val 3, :L nil, :R nil}, :R nil}

(def tree-3 (xconj tree-2 2)) ; {:val 5, :L {:val 3, :L {:val 2, :L nil, :R nil}, :R nil}, :R nil}

(defn xseq [t]
  (when t
    (concat 
      (xseq (:L t)) 
      [(:val t)] 
      (xseq (:R t)))))

(xseq tree-3) ; (2 3 5)

(defn xconj [t v]
  (cond
    (nil? t) {:val v :L nil :R nil}
    (< v (:val t)) {:val (:val t) 
                    :L (xconj (:L t) v)
                    :R (:R t)}
    :else {:val (:val t)
           :L (:L t)
           :R (xconj (:R t) v)})) ; push right

(def tree-4 (xconj tree-1 7))

(identical? (:L tree-1) (:L tree-4)) ; true

;; laziness

(and () 42 true :all-truthy) ; :all-truthy

(and 214 [] false (println "Send missiles") :world-destroyed) ; false

(defn rec-steps [[x & xs]]
  (if x
    [x (rec-steps xs)]
    [])) ; recursive version

(rec-steps [1 2 3 4]) ; [1 [2 [3 [4 []]]]]

(rec-steps (range 100000)) ; java.lang.StackOverflowError

(defn lazy-steps [s]
  (lazy-seq
    (if (seq s)
      [(first s) (lazy-steps (rest s))]
      [])))

(lazy-steps [1 2 3 4]) ; (1 (2 (3 (4 ()))))

(lazy-steps (range 100000)) ; works

; rules of thumb for lazy seqs
  ; use the lazy-seq macro at the outermost level of your forms
  ; when consuming another sequence, use rest not next
  ; prefer higher-order functions during processing
  ; don't hold on to the head of your sequences

; holding a head

(let [r (range 1e9)]
  (first r)
  (last r)) ; 999999999

(let [r (range 1e9)]
  (last r)
  (first r)) ; OutOfMemoryError: Java heap space

; infinite sequences

(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(triangle 4) ; 10, (+ 1 2 3 4) or
             ;  1
            ; 2   3
           ; 4  5  6 
          ; 7  8  9 10

(def triangle-nums (map triangle (iterate inc 1)))

(take 10 triangle-nums) ; (1 3 6 10 15 21 28 36 45 55)

(take 10 (filter even? triangle-nums)) ; (6 10 28 36 66 78 120 136 190 210)

(nth triangle-nums 99) ; 5050

(double (reduce +  
          (take 1000 (map / triangle-nums)))) ; 1.998001998001998, approachs 2

(take 2 (drop-while #(< % 10000) triangle-nums)) ; (10011 10153)

; delay and force

(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))

(defer-expensive 
  (delay :cheap)
  (delay (do (Thread/sleep 5000) :expensive))) ; :cheap, instantly

(defer-expensive 
  (delay false)
  (delay (do (Thread/sleep 5000) :expensive))) ; :expensive, after 5s

; lazy linked-lists

(defn tree-triangle-nums [n]
  {:head (triangle n)
   :tail (delay (tree-triangle-nums (inc n)))})

(defn head [l] (:head l))

(defn tail [l] (force (:tail l)))

(head (tail (tail (tree-triangle-nums 1)))) ; 6

; lazy quicksort

(defn rand-ints [n]
  (take n (repeatedly #(rand-int n))))

(rand-ints 10) ; (2 5 3 6 8 8 4 9 3 5)

(list* 1 '(2 3)) ; (1 2 3), same as list but last arg must be a sequence

(defn sort-parts [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          (recur 
            (list* 
              (filter smaller? xs)
              pivot
              (remove smaller? xs)
              parts)))
        (when-let [[x & parts] parts]
          (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))

(qsort (rand-ints 20)) ; (1 5 5 6 7 8 9 10 11 11 12 14 15 15 15 16 17 17 19 19)

(take 10 (qsort (rand-ints 10000))) ; (1 1 2 2 4 4 4 4 6 6), still fast due to laziness

;; functional programming

; function composition

(def fifth (comp first rest rest rest rest))

(fifth [1 2 3 4 5]) ; 5

(defn fnth [n]
  (apply comp
    (cons first (repeat (dec n) rest))))

((fnth 7) [1 2 3 4 5 6 7]) ; 7

(map (comp
       keyword
       #(.toLowerCase %)
       name)
  '(a B C)) ; (:a :b :c)


; partial application

((partial + 5) 100 200) ; 305

(#(apply + 5 %&) 100 200) ; 305

; functions returning functions

((complement even?) 2) ; false

(repl/source complement)

; (defn complement
;   "Takes a fn f and returns a fn that takes the same arguments as f,
;   has the same effects, if any, and returns the opposite truth value."
;   {:added "1.0"
;    :static true}
;   [f] 
;   (fn 
;     ([] (not (f)))
;     ([x] (not (f x)))
;     ([x y] (not (f x y)))
;     ([x y & zs] (not (apply f x y zs)))))

; functions as data

(defn join
  {:test (fn [] (assert (= (join "," [1 2 3]) "1,3,3")))} ; bad test
  [sep s]
  (apply str (interpose sep s)))

(test/run-tests) ; {:test 1, :pass 0, :fail 0, :error 1, :type :summary}

; higher-order functions

; either function that returns a function
; or function that accepts functions as args

(def plays 
  [{:band "Burial", :plays 979, :loved 9}
   {:band "Eno", :plays 2333, :loved 15}
   {:band "Bill Evans", :plays 979, :loved 9}
   {:band "Magma", :plays 2665, :loved 31}])

(def sort-by-loved-ratio 
  (partial 
    sort-by 
    #(/ (:plays %) 
       (:loved %))))

(map :band (sort-by-loved-ratio plays)) ; ("Magma" "Burial" "Bill Evans" "Eno")

(defn columns [column-names]
  (fn [row]
    (vec (map row column-names))))

((columns [:plays :loved :band])
 {:band "Burial", :plays 979, :loved 9}) ; [979 9 "Burial"]

; named arguments

(defn slope 
  [& {:keys [p1 p2]
      :or {p1 [0 0]
           p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(slope) ; 1.0

(slope :p2 [2 1]) ; 0.5

(slope :p1 [4 15] :p2 [3 21]) ; -6.0

; pre and post conditions

(defn slope 
  [& {:keys [p1 p2]
      :or {p1 [0 0]
           p2 [1 1]}}]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(slope :p1 [10 10] :p2 [10 10]) ; AssertionError: Assert failed: (not= p1 p2)

; turn off checks via (set! *assert* false) after ns declaration

; decouple assertions from functions

(defn into-grocery-list [m]
  (into m {:meat "beef" :veggie "broccoli"}))

(defn vegan-list [f m]
  {:pre [(:veggie m)]
   :post [(:veggie %) (nil? (:meat %))]}
  (f m))

(vegan-list into-grocery-list {:veggie "carrot"}) ; AssertionError: Assert failed: (nil? (:meat %))

; pulling assertions into a wrapper function allows you detach 
; domain-specific requirements from potentially globally useful
; functions and siolate them in _aspects_ (Laddad 2003)

; closures

(def add-and-get
  (let [a (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [n] (.addAndGet a n))))

(add-and-get 1) ; 1
(add-and-get 1) ; 2

(defn divisible? [n denom]
  (zero? (rem n denom)))

(filter divisible? (range 10)) ; ArityException: Wrong number of args (1) passed to: joy/divisible?

(defn divisible-by [denom] ; single arg via closures
  (fn [n] (zero? (rem n denom))))

(filter (divisible-by 4) (range 1 10)) ; (4 8)

; sharing closure context

(def bearings [{:x 0, :y 1}    ; north
               {:x 1, :y 0}    ; east
               {:x 0, :y -1}   ; south
               {:x -1, :y 0}]) ; west

(defn forward [x y bearing-idx]
  (let [bearing (bearings bearing-idx)]
    [(+ x (:x bearing))
     (+ y (:y bearing))]))

(forward 5 5 0) ; [5 6]

(defn bot [x y bearing-idx]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-idx)
   :forward (fn []
              (bot (+ x (:x (bearings bearing-idx)))
                   (+ y (:y (bearings bearing-idx)))
                   bearing-idx))})

(:coords ((:forward (bot 5 5 0)))) ; [5 6]

; recursion

; mundane "linear" version

(defn pow [base exp]
  (if (zero? exp)
    1
    (* base (pow base (dec exp)))))

(pow 2 10) ; 1024

(pow 1.01 925) ; 9937.353723241924

(pow 2 10000) ; java.lang.StackOverflowError

(defn pow [base exp]
  (letfn [(kapow [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (kapow base exp 1)))

(pow 2N 1000) ; 10715086071...N

(def simple-metric 
  {:meter 1
   :km 1000
   :cm 1/100
   :mm [1/10 :cm]})

(defn convert [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
              (let [val (get context unit)]
                (if (vector? val)
                  (* mag (convert context val))
                  (* mag val)))))
    0
    (partition 2 descriptor)))

(convert simple-metric [50 :cm]) ; 1/2

(convert simple-metric [100 :mm]) ; 1/10

(float 
  (convert simple-metric 
    [3 :km 10 :meter 80 :cm 10 :mm])) ; 3010.81

(-> (* 3 (:km simple-metric))
  (+ (* 10 (:meter simple-metric)))
  (+ (* 80 (:cm simple-metric)))
  (+ (* (:cm simple-metric)
       (* 10 (first (:mm simple-metric)))))
  float) ; 3010.81

(convert {:bit 1, :byte 8, :nibble [1/2 :byte]} [32 :nibble]) ; 128N

; trampoline finite state machine

(defn elevator [commands]
  (letfn 
    [(ff-open [[_ & r]]
       "When the elevator is ope non the 1st floor
        it can either close or be done."
       #(case _
          :close (ff-closed r)
          :done true
          false))
     (ff-closed [[_ & r]]
       "When the elevator is closed on the 1st floor
        it can either open or go up."
       #(case _
          :open (ff-open r)
          :up (sf-closed r)
          false))
     (sf-closed [[_ & r]]
       "When the elevator is closed on the 2nd floor
        it can either open or go up"
       #(case _
          :down (ff-closed r)
          :open (sf-open r)
          false))
     (sf-open [[_ & r]]
       "When the elevator is open on the 2nd floor
        it can either close or be done"
       #(case _
          :close (sf-closed r)
          :done true
          false))]
    (trampoline ff-open commands)))

(elevator [:open :open]) ; false, invalid state

(elevator [:close :up :open :done]) ; true, valid state

; continuation-passing style

; generalize a computation by creating 3 functions
  ; accept, when should computation terminiate
  ; return, wrap return values of computation
  ; continuation, next step in computation

(defn factorial-cps [n k]
  (letfn [(next [v] (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) next))))

(defn factorial [n]
  (factorial-cps n identity))

(factorial 10) ; 3628800

; generic cps maker

(defn make-cps [accept? kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v] (k ((partial kont v) n)))]
         (if (accept? n)
           (k 1)
           (recur (dec n) cont))))
     n kend)))

(def factorial
  (make-cps 
    zero?
    identity
    #(* %1 %2)))

(factorial 10) ; 3628800

(def triangle
  (make-cps
    #(= 1 %)
    identity
    #(+ %1 %2)))

(triangle 10) ; 55

;; a* pathfinding

(def world [[ 1 1 1 1 1]
            [999 999 999 999 1]
            [ 1 1 1 1 1]
            [ 1 999 999 999 999]
            [ 1 1 1 1 1]]) ; cost function map, 1 = flat ground, 999 mountain

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
               size
               yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
     (map #(vec (map + yx %))
       deltas))))

(neighbors 5 [0 0]) ; ([1 0] [0 1])


(defn estimate-cost 
  [step-cost-est size y x]
  (* step-cost-est 
    (- (* size 2) y x 2)))

(estimate-cost 900 5 0 0) ; 7200

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost 
    (or (:cost cheapest-nbr) 0)))

(path-cost 900 {:cost 1}) ; 901

(defn total-cost
  [new-cost step-cost-est size y x]
  (+ new-cost
    (estimate-cost step-cost-est size y x)))

(total-cost 0 900 5 0 0) ; 7200

(total-cost (path-cost 900 {:cost 1}) 900 5 3 4) ; 1801

(defn min-by [f coll]
  (when (seq coll)
    (reduce
      (fn [min other]
        (if (> (f min) (f other))
          other
          min))
      coll)))

(min-by :cost [{:cost 100} {:cost 36} {:cost 9}]) ; {:cost 9}

; a* algo

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (repeat size 
                         (vec (repeat size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                             (keep 
                               #(get-in routes %)
                               nbr-yxs))
              new-cost (path-cost 
                         (get-in cell-costs yx)
                         cheapest-nbr)
              old-cost (:cost (get-in routes yx))]
          (if (and old-cost (>= new-cost old-cost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps) 
              (assoc-in routes yx
                {:cost new-cost
                 :yxs (conj (:yxs cheapest-nbr []) yx)})
              (into rest-work-todo
                (map 
                  (fn [w]
                    (let [[y x] w]
                      [(total-cost new-cost step-est size y x) w]))
                  nbr-yxs)))))))))

(astar 
  [0 0] 
  900
  [[ 1 1 1 2 1]
   [ 1 1 1 999 1]
   [ 1 1 1 999 1]
   [ 1 1 1 999 1]
   [ 1 1 1 1 1]])

; [{:cost 9, 
;   :yxs [[0 0] [0 1] [0 2] [1 2] [2 2] [3 2] [4 2] [4 3] [4 4]]} 
;   :steps 134]

(astar [0 0] 900 world)

; [{:cost 17, 
;   :yxs [[0 0] [0 1] [0 2] [0 3] [0 4] [1 4] [2 4] [2 3] [2 2] [2 1] [2 0] [3 0] [4 0] [4 1] [4 2] [4 3] [4 4]]} 
;   :steps 94]
