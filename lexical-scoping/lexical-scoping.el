;; -*- lexical-binding: t; -*-

(defun make-person (name age)
  (lambda (get
           &optional set-name
           &optional set-age)
    (funcall get name age)
    (let ((set-name (or set-name 'identity))
          (set-age (or set-age 'identity)))
      (setq name (funcall set-name name))
      (setq age (funcall set-age age)))))

(defun print-person (person)
  (let (representation)
    (funcall
     person
     (lambda (name age)
       (setq representation (format "Name: %s, Age: %s" name age))))
    (message representation)))

(defun grow-old (person)
  (let (new-age)
    (funcall
     person
     (lambda (_name age)
       (setq new-age (+ age 10)))
     nil
     (lambda (age)
       new-age))))

(let ((john (make-person "John" 33)))
  (print-person john)
  (grow-old john)
  (print-person john))
