(defun bsearch (vec elt)
  "binary search VEC for ELT, returning index, or -1 if not found"
  (bsearch-impl vec elt 0 (- (length vec) 1)))

(defun bsearch-impl (vec target left right)
  (let ((mid (/ (+ left right) 2)))
    (cond ((eq left right)
           (if (eq target (elt vec left))
               left
             -1))
          ((<= target (elt vec mid))
           (bsearch-impl vec target left mid))
          (t
           (bsearch-impl vec target (+ 1 mid) right)))))
