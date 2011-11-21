;; In answer to the [challenge][CHL] set by Bob Lee from [Square][SQR].
;; [CHL]: http://corner.squareup.com/2011/11/luhny-bin.html
;; [SQR]: http://squareup.com/

(ns luhnybin.core
  (:use [clojure.java.io :only [make-reader]])
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

(defn- num-digits [chars]
  (reduce #(if (digits %2) (inc %1) %1) 0 chars))

;; Find any candidates at the ste start of a string or char seq,
;; sorted longest to shortest.

(defn- leading-candidates [chars]
  (loop [[head & tail] (take-while ccchars chars) seen [] acc '()]
    (let [seen (if head (conj seen head) seen)
          len  (num-digits seen)
          acc  (if (>= len 14) (conj acc seen) acc)]
      (if (or (= len 16) (nil? tail) (empty? tail))
        acc
        (recur tail seen acc)))))

;; ### The Main Public API: Process Strings, Files, or Streams

(defn process-line
  "Process a line, returning the indices of any potential CC numbers."
  [line]
  (let [buf (transient (vec line))]
    (loop [pos 0 in (seq line)]
      (when (digits (first in))
        (when-let [cc (some luhn? (leading-candidates in))]
          (loop [n pos [x & xs] cc]
            (when (digits x) (assoc! buf n \X))
            (when xs (recur (inc n) xs)))))
      (when (not (empty? in))
        (recur (inc pos) (next in))))
    (apply str (persistent! buf))))

;; Read from the file named in the first argument, or *stdin*.
(defn -main [& args]
  (dorun
    (->> (make-reader (or (first args) *in*) nil)
      (line-seq)
      (pmap #(process-line %))
      (map println))))

