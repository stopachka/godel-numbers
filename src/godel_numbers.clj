(ns godel-numbers
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :as string]))

(defn primes
  ([] (primes 2 []))
  ([current known-primes]
   (let [factors (take-while #(<= (* % %) current) known-primes)
         remainders (map #(mod current %) factors)]
     (if (some zero? remainders)
       (recur (inc current) known-primes)
       (lazy-seq (cons
                   current
                   (primes (inc current) (conj known-primes current))))))))

(defn factorize
  [num]
  (loop [num (biginteger num) acc [1] primes (primes)]
    (if (= num 1N)
      acc
      (let [factor (first primes)]
        (if (zero? (mod num factor))
          (recur (quot num factor) (conj acc factor) primes)
          (recur num acc (rest primes)))))))

(def open-bracket (symbol "("))
(def close-bracket (symbol ")"))

(def token->num {open-bracket 1
                 close-bracket 3
                 0 5
                 'next 7
                 '+ 9
                 '* 11
                 '= 13
                 'not 15
                 'or 17
                 'when 19
                 'there-is 21
                 'a 2
                 'b 4
                 'c 6})

(def num->token (map-invert token->num))

(defn parse-tokens
  ([form] (parse-tokens [] form))
  ([res form]
   (if (seq? form)
     (let [res-after-open-bracket
           (conj res open-bracket)

           res-after-seq
           (reduce
             parse-tokens
             res-after-open-bracket
             form)]
       (conj res-after-seq close-bracket))
     (conj res form))))

(defn bigpow [a b]
  (.pow (biginteger a) b))

(defn pm-lisp->godel-num [form]
  (->> (parse-tokens form)
       (map token->num)
       (map vector (primes))
       (map (partial apply bigpow))
       (reduce *)))

(defn godel-num->pm-lisp [godel-num]
  (->> (factorize godel-num)
       rest
       frequencies
       sort
       (map (comp num->token second))
       (string/join #" ")
       read-string))

(comment
  (pm-lisp->godel-num '(next 0))
  (godel-num->pm-lisp 4688381250N))
