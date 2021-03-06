# LispLisp
Objectively the best esoteric language ever

Essentially: Each instance of the word `lisp` is the Iota combinator. The language is too stupid to recognize sequences of `lisp`s
that aren't simply representations of the SKI Combinator Calculus.

```
S = (lisp (lisp (lisp (lisp lisp))))
K = (lisp (lisp (lisp lisp)))
I = (lisp lisp)
```

Here's Version 2: Now compiles to LazyK. I included a couple of somewhat-modified LazyK interpreters, along with a Bash script
to automate the interpretation process. The version in C (originally by irori) is much faster than the Haskell one (by dforgeas).
Both of the above, incidentally, are licensed under GPL version 2. I intend to write my own (proper) interpreter in Scheme
at some point,
under my preferred Apache license. Until then, however, note that the lazyk.hs and lazyk.c files are GPL version 2, and the rest
of the stuff is Apache.

In the Bash file, entitled "lisplisp" here, you can change the default Scheme interpreter. I have tested both Guile and Racket,
and they work fine. However, any other proper Scheme should work too. The only one of these programs that can only be run on
Guile is the one with the .guile suffix, obviously.

Stuff to do:

* Write a proper (not lame) interpreter. Probably in Scheme.
* Write a translator from Brainf*** to LispLisp (because why not)

Have fun.
