# godel-numbers 

Accompaniment to the [essay](https://stopa.io/post/269) on Godel Numbers : ) 

Handy functions that encode "PM-Lisp", into Godel Numbers and vice versa. 

This just works for formulas -- most proofs would be too big to even fit in a biginteger! 

```clojure
(pm-lisp->godel-num '(next 0))
(godel-num->pm-lisp 4688381250N)
```
