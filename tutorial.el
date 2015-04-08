;;;; LISP Tutorial


;;; Printing

(message "hi")

;; Printing variable values
(message "Her age is: %d" 16)            ; %d is for number
(message "Her name is: %s" "Vicky")      ; %s is for string
(message "My list is: %S" (list 8 2 3))  ; %S is for any lisp expression


;;; Arithmetic Functions

(+ 4 5 1)   ; => 10
(- 9 2)     ; => 7
(- 9 2 3)   ; => 4
(* 2 3)     ; => 6
(* 2 3 2)   ; => 12
(/ 7 2)     ; => 3 (integer part of quotient)
(/ 7 2.0)   ; => 3.5
(% 7 4)     ; => 3 (Remainder)
(expt 2 3)  ; => 8 (power; exponential)


;;; Converting String and Numbers

(string-to-number "3")
(number-to-string 3)


;;; True and False

;; All the following are false. They all evaluate to "nil".
(if nil "yes" "no")     ; => "no"
(if () "yes" "no")      ; => "no"
(if '() "yes" "no")     ; => "no"
(if (list) "yes" "no")  ; => "no", because (list) eval to a empty list, same as ()

(if t "yes" "no")   ; => "yes"
(if 0 "yes" "no")   ; => "yes"
(if "" "yes" "no")  ; => "yes"
(if [] "yes" "no")  ; => "yes". The [] is vector of 0 elements

(and t nil) ; => nil
(or t nil)  ; => t


;;; Comparison Functions

(< 3 4)   ; less than
(> 3 4)   ; greater than
(<= 3 4)  ; less or equal to
(>= 3 4)  ; greater or equal to

(= 3 3)    ; => t
(= 3 3.0)  ; => t

(/= 3 4)  ; not equal. => t

;; Testing if two values have the same datatype and value.
(equal "abc" "abc")  ; => t
(equal 3 3)          ; => t
(equal 3.0 3.0)      ; => t
(equal 3 3.0)        ; => nil. Because datatype doesn't match.

;; Testing equality of lists
(equal '(3 4 5) '(3 4 5))    ; => t
(equal '(3 4 5) '(3 4 "5"))  ; => nil

;; Testing equality of symbols
(equal 'abc 'abc)  ; => t

;; Inequality
(not (equal 3 4))  ; => t. General way to test inequality.


;;; Global and Local Variables

(setq x 1)  ; assign 1 to x

(let (a b)
  (setq a 3)
  (setq b 4)
  (+ a b))  ; returns 7

(let ((a 3) (b 4))
  (+ a b))  ; returns 7


;;; If Then Else

(if (< 3 2)
    (message "yes"))
(if (< 3 2)
    (message "yes")
  (message "no"))

(if nil
    (message "yes")
  (message "no"))  ; prints no


;;; A Block of Expressions

(if t
    ;; Explicit block
    (progn
      (message "foo")
      (message "bar"))
  (message "no"))

(when t
  ;; Implicit block
  (message "foo")
  (message "bar"))


;;; Iteration

;; TODO: Might just want to use `mapcar' and `mapc' instead.
;; (let ((x 32))
;;   (while (< x 127)
;;     (ucs-insert x)
;;     (setq x (+ x 1))))


;;; Lists

;; TODO: http://ergoemacs.org/emacs/elisp_list_vector.html


;;; Defining a function

(defun my-function ()
  "testing"
  (message "Yay!"))

(let ((a-function (lambda ()
                    (message "Yo!"))))
  (funcall a-function))


;; TODO: Practical lambdas
;; TODO: Practical lexical binding
