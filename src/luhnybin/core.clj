(ns luhnybin.core
  (:use [clojure.java.io :only [make-reader]]
        [clojure.string  :only [join]])
  (:gen-class))

(set! *warn-on-reflection* true)

;; First define a few small helpers.

(def ^:private digits #{ \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 })
(def ^:private extras #{ \- \space })
(def ^:private ccchars (apply conj digits extras))

(def ^:private c->i
  "Map of characters to their single digit integer values."
  { \0 0, \1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9 })
(def ^:private c->i*
  "Map of characters to their doubled and digit-summed integer values."
  { \0 0, \1 2, \2 4, \3 6, \4 8, \5 1, \6 3, \7 5, \8 7, \9 9, })

;; 1. ignoring non-digits and working from the right;
;; 2. double every second digit and sum the resulting digits;
;; 3. sum all of the resulting digits; and
;; 4. divide by 10 and take the remainder.
;; 
;; The candidate passes if the result is 0.
(defn luhn?
  "Does a candidate char-seq pass the Luhn test?"
  [chars]
  (->> (filter digits chars)
    (reverse)
    (map #(%1 %2) (cycle [c->i c->i*]))
    (reduce + 0)
    (#(rem % 10))
    (#(if (zero? %) chars nil))))

;; A potential credit card must have 14 to 16 digits (inclusive).
(defn- correct-len? [chars]
  "Are there [14..16] chars in a seq?"
  (let [n (count (filter digits chars))]
    (and (>= n 14) (<= n 16))))

(defn- candidates
  "See if there are any candidates in a string or char-seq."
  [chars]
  (->> (take-while ccchars chars)
    (reductions conj [])
    (filter correct-len?)
    (sort #(compare (count %2) (count %1)))))

(defn process-line
  ""
  [line]
  (loop [pos 0 in (seq line) to-mask #{}]
    (if in
      (let [c (first in)]
        (if (digits c)
          (let [candidates (candidates in)]
            (if-let [cc (some luhn? candidates)]
              (recur (inc pos) (next in)
                     (into to-mask (range pos (+ pos (count cc)))))
              (recur (inc pos) (next in) to-mask)))
          (recur (inc pos) (next in) to-mask)))
      (->> (seq line)
        (map-indexed #(if (and (to-mask %1) (digits %2)) \X %2))
        (apply str)))))

(defn -main [& args]
  (dorun
    (->> nil
      (make-reader (or (first args) *in*))
      (line-seq)
      (map process-line)
      (map println))))

