;; In answer to the [challenge][CHL] set by Bob Lee from [Square][SQR].
;; [CHL]: http://corner.squareup.com/2011/11/luhny-bin.html
;; [SQR]: http://squareup.com/

(ns luhnybin.core
  (:use [clojure.java.io :only [make-reader]]
        [clojure.string  :only [join]])
  (:gen-class))

;; First define a few small helpers…

(def ^:private digits #{ \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 })
(def ^:private extras #{ \- \space })
(def ^:private ccchars (apply conj digits extras))

;; A map of characters to their single digit integer values.
(def ^:private c->i
  { \0 0, \1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9 })

;; A map of characters to their doubled and digit-summed integer values.
(def ^:private c->i*
  { \0 0, \1 2, \2 4, \3 6, \4 8, \5 1, \6 3, \7 5, \8 7, \9 9, })

;; ### The Luhn Algorithm
;;
;; See [Wikipedia][WIKI] for the gory details, but basically:
;; 
;; 1. ignoring non-digits and working from the right;
;; 2. double every second digit and sum the resulting digits;
;; 3. sum all of the resulting digits; and
;; 4. divide by 10 and take the remainder.
;; 
;; The candidate passes if the result is 0.
;; [WIKI]: http://en.wikipedia.org/wiki/Luhn_algorithm
(defn luhn?
  "Does a candidate char-seq pass the Luhn test?"
  [chars]
  (->> (filter digits chars)
    (reverse)
    (map #(%1 %2) (cycle [c->i c->i*]))
    (reduce + 0)
    (#(rem % 10))
    (#(if (zero? %) chars nil))))

;; ### Searching for Potential CCNs

;; A potential credit card must have 14 to 16 digits (inclusive).
(defn- correct-len? [chars]
  (let [n (count (filter digits chars))]
    (and (>= n 14) (<= n 16))))

;; Find any candidates at the ste start of a string or char seq,
;; sorted longest to shortest.
(defn- leading-candidates [chars]
  (loop [cs (vec (take-while ccchars chars)) acc []]
    (let [len (count (filter digits cs))]
      (cond
        (>  len 16) (recur (pop cs) acc)
        (>= len 14) (recur (pop cs) (conj acc cs))
        :else acc))))

;; ### The Main Public API: Process Strings, Files, or Streams

(defn process-line
  "Process a line, returning the indices of any potential CC numbers."
  [line]
  (loop [pos 0 in (seq line) to-mask #{}]
    (if in
      (let [c (first in)]
        (if (digits c)
          (let [candidates (leading-candidates in)]
            (if-let [cc (some luhn? candidates)]
              (recur (inc pos) (next in)
                     (into to-mask (range pos (+ pos (count cc)))))
              (recur (inc pos) (next in) to-mask)))
          (recur (inc pos) (next in) to-mask)))
      to-mask)))

(defn mask-line
  "Mask out any digits occurring at the marked positions."
  [line to-mask mask-char]
  (->> (seq line)
    (map-indexed #(if (and (to-mask %1) (digits %2)) mask-char %2))
    (apply str)))

;; Read from the file named in the first argument, or *stdin*.
(defn -main [& args]
  (dorun
    (->> nil
      (make-reader (or (first args) *in*))
      (line-seq)
      (map #(mask-line % (process-line %) \X))
      (map println))))

