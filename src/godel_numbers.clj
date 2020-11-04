(ns godel-numbers
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :as string]))

; primes

(defn lazy-primes
  ([] (lazy-primes 2 []))
  ([current known-primes]
   (let [factors (take-while #(<= (* % %) current) known-primes)
         remainders (map #(mod current %) factors)]
     (if (not-any? zero? remainders)
       (lazy-seq (cons
                   current
                   (lazy-primes (inc current) (conj known-primes current))))
       (recur (inc current) known-primes)))))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def char->num (->> (char-range \a \z)
                    (map-indexed (fn [i x] [x (inc i)]))
                    (into {})))

(def num->char (map-invert char->num))

(def max-num (->> num->char (map first) sort last))

(defn chars->godel-num [chars]
  (->> chars
       (map char->num)
       (map vector (lazy-primes))
       (reduce
         (fn [res [prime num]]
           (* res (Math/pow prime num)))
         1)))

(defn factorize
  "factorization, but optimized for our num->char

  we can cheat a bit by knowing the max exponent we can have"
  [num]
  (loop [num (double num) acc [1] primes (lazy-primes)]
    (if (= num 1.0)
      acc
      (let [factor (first primes)]
        (if (zero? (mod num factor))
          (recur (quot num factor) (conj acc factor) primes)
          (recur num acc (rest primes)))))))

(defn godel-num->chars [godel-num]
  (->> (factorize godel-num)
       (drop 1)
       frequencies
       sort
       (map (comp num->char second))
       string/join))

(comment
  (chars->godel-num "hi")
  (godel-num->chars 5038848))
