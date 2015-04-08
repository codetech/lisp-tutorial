# LISP Tutorial


## Hello World

```elisp
(message "Hello World!")
```


## Lisp Expressions

The syntax of Lisp is among the simplest of all programming languages.  Lisp has
only one type of expression--the function call.  Function calls have this form:

```elisp
(function expr1 expr2 ... exprN)
```

Examples:

```elisp
(+ 3 4)  ; => 7
(length "abcd")  ; => 4
(concat "just" "testing")  ; => "justtesting"
(type-of "testing")  ; => string
(buffer-size)  ; => 217
```

When it makes sense for a function to have an arbitrary number of operands, Lisp
typically permits it:

```elisp
(+ 1 2 3)  ; => 6
(* 1 2 3 4 5)  ; => 120
(- 10 1 2 3 4 5 6 7 8 9 10)  ; => -45
(concat "a" "bc" "def" "ghij")  ; => "abcdefghij"
```

Complex expressions are built up by nesting:

```elisp
;; Most languages: (3+4)*(5-3)
(* (+ 3 4) (- 5 3))  ; => 14
(1+ (length "abcd"))  ; => 5
(substring (concat "abc" "def") 1 3)  ; => "bc"
```


## True and False

There are a number of functions that perform comparisons.  They typically return
`t` if successful and `nil` if not:

```elisp
(< 1 2)  ; => t
(= (* 3 4) (+ 4 4 4))  ; => t
(string= "9798" (concat ?a ?b))  ; => t
(numberp "xyz")  ; => nil
```

The `not` function inverts `t` or `nil`:

```elisp
(not nil)  ; => t
```

`not` considers everything except `nil` to be `t`

```elisp
(if nil "yes" "no") ; => "no"
(if t "yes" "no")   ; => "yes"
(if 0 "yes" "no")   ; => "yes"
(if "" "yes" "no")  ; => "yes"

(and t nil) ; => nil
(or t nil)  ; => t
```


## Global and Local Variables

```elisp
(setq x 1)  ; assign 1 to x

(let (a b)
  (setq a 3)
  (setq b 4)
  (+ a b))  ; returns 7

(let ((a 3) (b 4))
  (+ a b))  ; returns 7
```


## Lists

The fundamental data structure in Lisp is the list.  Here are some examples of
lists:

```elisp
(1 2 3 4)
(x y z)
(+ 3 4)
(car ford)
(setq y (* (dot) (dot)))
("just" a ('test) (((here) for) example))
(cdr '(1 2 3))
```

Lists can represent program code or data; the meaning is dependent on context.

Creating a list:

```elisp
(list 1 2 3)  ; => (1 2 3)
(list "butter" "milk" "eggs")  ; => ("butter" "milk" "eggs")
```

Accessing the head:

```elisp
(setq x (list 1 2 3 4))

(car x)  ; => 1
```

Accessing the tail:

```elisp
(cdr x)  ; => (2 3 4)
```

Accessing the Nth element:

```elisp
(car (cdr x))  ; => 2
(cdr (cdr x))  ; => (3 4)
(car (cdr (cdr x)))  ; => 3
(cdr (cdr (cdr x)))  ; => (4)
(car (cdr (cdr (cdr x))))  ; => 4
(cdr (cdr (cdr (cdr x))))  ; => nil
```

Recursive much?  But we rarely actually write out nested `car`s and `cdr`s.  We
use them to iterate over a list.

```elisp
(let ((items (list "butter" "milk" "eggs")))
  (while items
    (message (car items))
    (setq items (cdr items))))
```


## Pairs

```elisp
(cons 10 20)  ; => (10 . 20)
(cons "a" "b")  ; => ("a" . "b")
```

You can represent lots of data items in terms of pairs.  Complex numbers, like
1/2, 7/5 are one example.

```elisp
(defun make-complex-number (numerator denominator)
  (cons numerator denominator))

(defun print-complex-number (number)
  (message "%d/%d" (car number) (cdr number)))

(defun add-complex-numbers (n1 n2)
  (cons (+ (* (car n1) (cdr n2))
           (* (car n2) (cdr n1)))
        (* (cdr n1) (cdr n2))))

(print-complex-number
 (make-complex-number 3 4))  ; => "3/4"

(add-complex-numbers
 (make-complex-number 1 2)
 (make-complex-number 3 4))  ; => "10/8"
```

You can make pairs containing pairs.  A pair containing a pair containing a
pair... etc... is actually a list!

```elisp
(cons 1 nil)  ; => (1)
(cons 1 (cons 2 nil))  ; => (1 2)
(cons 1 (cons 2 (cons 3 nil)))  ; => (1 2 3)
```

## Some built-in list functions

Here is a sampling of the many built-in functions that operate on lists:

```elisp
(length '(a b c))  ; => 3
(nth 1 '(a b c))  ; => b
(member 20 '(10 20 30))  ; => (20 30)
(reverse '(1 2 3))  ; => (3 2 1)
(list '(a b) 1 2 '(10 20 30))  ; => ((a b) 1 2 (10 20 30))
(append '(a b) 1 2 '(10 20 30))  ; => (a b 49 50 10 20 30)
(equal '(1 2 3) (cons 1 '(2 3)))  ; => t
```


## Functions

The special form defun is used to define functions.  The general form is
this:

```elisp
(defun name arguments documentation expr1 expr2 ... exprN)
```

The result of `exprN` is the return value of the function.

A function to calculate the area of a circle:

```elisp
(defun area (radius)
 "Calculates the area of circle with RADIUS"
 (* pi radius radius)) ; 'pi' is a built-in variable
```

Usage:

```elisp
(area 5)  ; => 78.53981633974483
```

`defun` is called a special form because it doesn't evaluate all of its
arguments.

The documentation for a function can be accessed with `describe-function`:

```elisp
(describe-function 'area)
```

Consider a function `linelen` that computes the distance between two points
represented as dotted-pairs:

```elisp
(linelen '(0 . 0) '(1 . 1))  ; => 1.4142135623730951
```

Definition:

```elisp
(defun linelen (p1 p2)
  (let* ((x1 (car p1))
         (y1 (cdr p1))
         (x2 (car p2))
         (y2 (cdr p2))
         (xdiff (- x2 x1))
         (ydiff (- y2 y1)))
    (sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))
```


## `cond`

The special form `cond` provides for conditional execution of expressions.  The
general form is this:

```elisp
(cond clause1 clause2 ... clauseN)
```

Each clause is of the form:

```elisp
(test-expr expr1 expr2 ... exprN)
```

Each clause is processed in turn, first evaluating `test-expr`.  If it yields a
non-`nil` value then `expr1` through `exprN` are executed.  The value of the
last expression is the value of the `cond`.  If the `test-expr` for a clause
produces `nil`, then the next clause is evaluated in the same way.

```elisp
(defun cond-ex1 (N)
  (cond
   ((= N 0) "N is zero")
   ((> N 100) "N > 100")
   ((= (mod N 2) 0) "N is even")
   (t "None of the above")))

(cond-ex1 10)  ; => "N is even"
(cond-ex1 1000)  ; => "N > 100"
(cond-ex1 7)  ; => "None of the above"
```

Imagine a function `(divide L N)` that separates the values in `L` based on
whether the values are smaller or larger than `N`.

```elisp
(divide '(5 2 4 10 3 -3) 5)  ; => ((2 4 3 -3) (10))
```

Implementation:

```elisp
(defun divide (L N)
  (let ((smaller nil) (bigger nil) elem)
    (while L
      (setq elem (car L))
      (setq L (cdr L))
      (cond
       ((< elem N)
        (setq smaller (cons elem smaller)))
       ((> elem N)
        (setq bigger (cons elem bigger)))))
    (list (reverse smaller) (reverse bigger))))
```


## Higher-order functions

### map

`mapcar` applies a function to every element in a list and produces a list of
the results:

```elisp
(mapcar 'length '("a" "test" "of" "mapcar"))  ; => (1 4 2 6)
```

A function in Lisp can be represented with a list whose first element is
`lambda`.

```elisp
(mapcar (lambda (n) (* n 2)) '(10 20 30))  ; => (20 40 60)
```

Here is one way to write `mapcar`:

```elisp
(defun mapcar (f L)
  (cond
   ((consp L)
    (cons
     (apply f (car L) nil)
     (mapcar f (cdr L))))
   (t ())))
```

Common Lisp has a bunch of functions which take other functions as
parameters.

```elisp
(require 'cl-lib)
```

### every

> `(cl-every PREDICATE SEQ...)`
> Return true if PREDICATE is true of every element of SEQ or SEQs.

```elisp
(cl-every 'numberp '(1 2 3))  ; => t
(cl-every 'numberp '(1 2 "c"))  ; => nil, last one is a string
```

### some

> `(cl-some PREDICATE SEQ...)`
> Return true if PREDICATE is true of any element of SEQ or SEQs.
> If so, return the true (non-nil) value returned by PREDICATE.

```elisp
(cl-some 'numberp '(1 2 3))  ; => t
(cl-some 'numberp '("a" "b" "c"))  ; => nil, none where numbers
```

### filter

> `(cl-remove-if-not PREDICATE SEQ [KEYWORD VALUE]...)`
> Remove all items not satisfying PREDICATE in SEQ.

```elisp
(setq servers (list "https://google.com" "http://google.com"))

(defun is-safe (url)
  (string-match "^https" url))

(cl-remove-if-not 'is-safe servers)  ; => ("https://google.com")
```

### sort

> `(cl-sort SEQ PREDICATE [KEYWORD VALUE]...)`
> Sort the argument SEQ according to PREDICATE.

```elisp
(cl-sort (list "George" "Joe" "Aaron")
         (lambda (word1 word2)
           (< (elt (downcase word1) 0)
              (elt (downcase word2) 0))))  ; => ("Aaron" "George" "Joe")
```
