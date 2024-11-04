(ns part-1
  (:require [clojure.repl :as repl]
            [clojure.java.javadoc :refer [javadoc]]
            [clojure.core.match :refer [match]]))

;; scalar values

"foo
 bar
 baz" ; "foo\n bar\n baz"  

[127 0x7F 0177 32r3V 2r01111111] ; [127 127 127 127 127]

-103/4 ; clojure.lang.Ratio

\u30DE ; \マ

;; vars

(def pi-ish 22/7) ; define root value of symbol pi-ish

(def y) ; #object[clojure.lang.Var$Unbound 0x273ab9d2 "Unbound: #'joy/y"]

;; first class functions

(fn []) ; #object[joy$eval2815$fn__2816 0x89a7d3c "joy$eval2815$fn__2816@89a7d3c"]

(map (fn [f] (f)) 
  [(fn [] 1) 
   (fn [] 2) 
   (fn [] 3)]) ; (1 2 3)

;; lambdas

(macroexpand '#(%1 %2)) ; (fn* [p1__2892# p2__2893#] (p1__2892# p2__2893#))

(macroexpand '#(%&)) ; (fn* [& rest__2899#] (rest__2899#))

;; loop via recursion

(loop [sum 0, x 100]
  (if (pos? x)
    (recur (+ sum x) (dec x))
    sum)) ; 5050

;; quote/splicing

`map ; clojure.core/map

`non-existent ; joy/non-existent

`(+ ~(* 3 2)) ; (clojure.core/+ 6)

`(~@'(1 2 3)) ; (1 2 3)

`foo# ; foo__2896__auto__, auto gen-sym

;; java interop

(java.util.Date.) ; #inst "2024-11-02T16:11:30.394-00:00"

(doto (java.util.HashMap.)
  (.put "home" "/home/me")
  (.put "bin" "src")) ; {"bin" "src", "home" "/home/me"}

(.. (java.util.Date.) toString (endsWith "2024")) ; true

java.lang.Object/.toString ; static class method access 

clojure.core/str ; works with namespace symbols

java.lang.Object ; works

clojure.core ; but not with namespace symbols, ClassNotFoundException: clojure.core

;; truthiness

(if true :truthy :falsey) ; :truthy
(if [] :truthy :falsey) ; :truthy 
(if nil :truthy :falsey) ; :falsey
(if false :truthy :falsey) ; :falsey

; nil punning (and idiomatic empty coll checking)

(defn print-seq [s]
  (when (seq s)
    (prn (first s))
    (recur (rest s))))

(print-seq []) ; returns nil

(print-seq [1 2]) ; prints 1 and 2 then returns nil

;; destructuring

(let [v (vec (range 1 11)) 
      [a b c & more :as all] v]
  {:a a
   :b b
   :c c
   :more more
   :all all}) ; {:a 1, :b 2, :c 3, :more (4 5 6 7 8 9 10), :all [1 2 3 4 5 6 7 8 9 10]}

(let [{:keys [f-name m-name l-name]} 
      {:f-name "Guy"
       :l-name "Steele"
       :m-name "Lewis"}]
(str l-name ", " f-name " " m-name)) ; "Steele, Guy Lewis"

; use {:strs [...]} for string keys or :syms for symbols
; works with vectors too

(let [{first-thing 0, last-thing 3} [1 2 3 4]] ; works with vectors
  [first-thing last-thing]) ; [1 4]

;; repl workflow demo

(range 5) ; (0 1 2 3 4)

(for [x (range 2)
      y (range 2)]
  [x y]) ; ([0 0] [0 1] [1 0] [1 1])

(xor 1 2) ; RuntimeException: Unable to resolve symbol: xor in this context 

(repl/find-doc "xor")

; -------------------------
; clojure.core/bit-xor
; ([x y]) ([x y & more])
; Bitwise exclusive or

(bit-xor 1 2) ; 3

(for [x (range 2)
      y (range 2)]
  [x y (bit-xor x y)]) ; ([0 0 0] [0 1 1] [1 0 1] [1 1 0])

(def frame (java.awt.Frame.))

(for [methods (.getMethods java.awt.Frame) ; use .getDeclaredFields for static fields 
      :let [name (.getName methods)]]
      name)

(.isVisible frame) ; false

(.setVisible frame true) ; window shows up now

(.setSize frame (java.awt.Dimension. 400 400)) ; window resized

(javadoc 'java.awt.Frame) ; opens url

(def gfx (.getGraphics frame))

(.fillRect gfx 100 100 50 75)

(.setColor gfx (java.awt.Color. 255 128 0))

(.fillRect gfx 100 150 75 50)

(defn xors [max-x max-y]
  (for [x (range max-x)
        y (range max-y)]
  [x y (bit-xor x y)]))

(doseq [[x y xor] (xors 200 200)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))

(doseq [[x y xor] (xors 500 500)]
(.setColor gfx (java.awt.Color. xor xor xor))
(.fillRect gfx x y 1 1)) ; xor produces a wild pattern

(rem (bit-xor 200 500) 256) ; 60

(defn xors [max-x max-y]
  (for [x (range max-x)
        y (range max-y)]
  [x y (rem (bit-xor x y) 256)]))

(defn clear [g] (.clearRect g 0 0 500 500))

(clear gfx)

(defn f-values [f max-x max-y]
  (for [x (range max-x)
        y (range max-y)]
    [x y (rem (f x y) 256)]))

(defn draw-values [f xs ys]
  (clear gfx)
  (.setSize frame (java.awt.Dimension. xs ys))
  (doseq [[x y v] (f-values f xs ys)]
    (.setColor gfx (java.awt.Color. v v v))
    (.fillRect gfx x y 1 1)))

(draw-values * 256 256) ; the craziest pattern, by far

;; fizzbuzz with core.match

(doseq [n (range 1 101)]
  (println
    (match [(mod n 3) (mod n 5)]
      [0 0] "FizzBuzz"
      [0 _] "Fizz"
      [_ 0] "Buzz"
      :else n))) ; prints 1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz...
