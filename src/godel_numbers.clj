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

(defn- advance-prime-token [primes res token]
  [(rest primes)
   (conj res [(first primes) (token->num token)])])

(defn prime-and-token
  ([form] (second (prime-and-token (primes) [] form)))
  ([primes res form]
   (cond
     (seq? form)
     (let [
           [primes-after-open-bracket
            res-after-open-bracket]
           (advance-prime-token primes res open-bracket)

           [primes-after-seq
            res-after-seq]
           (reduce
             (fn [[primes res] form]
               (prime-and-token primes res form))
             [primes-after-open-bracket res-after-open-bracket]
             form)]
       (advance-prime-token primes-after-seq res-after-seq close-bracket))
     :else
     (advance-prime-token primes res form))))

(defn pm-lisp->godel-num [form]
  (->> (prime-and-token form)
       (reduce
         (fn [res [prime num]]
           (* (.pow (biginteger prime) num) res))
         (bigint 1))))

(defn godel-num->pm-lisp [godel-num]
  (->> (factorize godel-num)
       (drop 1)
       frequencies
       sort
       (map (comp num->token second))
       (string/join #" ")
       read-string))

(comment
  (pm-lisp->godel-num '(next 0))
  (godel-num->pm-lisp 4688381250N))
