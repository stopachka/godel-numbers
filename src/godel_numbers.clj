(ns godel-numbers
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :as string]))

(defn primes
  ([] (primes 2 []))
  ([current known-primes]
   (let [factors (take-while #(<= (* % %) current) known-primes)
         remainders (map #(mod current %) factors)]
     (if (not-any? zero? remainders)
       (lazy-seq (cons
                   current
                   (primes (inc current) (conj known-primes current))))
       (recur (inc current) known-primes)))))

(defn factorize
  [num]
  (loop [num (biginteger num) acc [1] primes (primes)]
    (if (= num 1N)
      acc
      (let [factor (first primes)]
        (if (zero? (mod num factor))
          (recur (quot num factor) (conj acc factor) primes)
          (recur num acc (rest primes)))))))

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
       (map vector (primes))
       (reduce
         (fn [res [prime num]]
           (* (.pow (biginteger prime) num) res))
         (bigint 1))))

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
