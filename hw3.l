; PART 1
(defun element-count (item lst)
  (cond
    ((null lst) 0) ; if no item, return 0
    ; if match, increment
    ((equal item (car lst)) (+ 1 (element-count item (cdr lst))))
    ; otherwise, no match, simply continue
    (t (element-count item (cdr lst)))
  )
)

(defun min-mean-max (lst)
  (cond
    ((null lst) nil)
    (t (list (find-min lst (car lst))  (find-mean lst 0 0) (find-max lst (car lst))))
  )
)

; helper function to find minimum
(defun find-min (lst min)
  (cond
    ((null lst) min)
    (t (if (< min (car lst))
        (find-min (cdr lst) min)
        (find-min (cdr lst) (car lst))
      )
    )
  )
)

; helper function to find maximum
(defun find-max (lst max)
  (cond
    ((null lst) max)
    (t (if (> max (car lst))
        (find-max (cdr lst) max)
        (find-max (cdr lst) (car lst))
      )
    )
  )
)

; helper function to find mean
(defun find-mean (lst num total)
  (cond
    ((null lst) (/ num total)) ;at the end, divide to get the mean
    (t (find-mean (cdr lst) (+ num (car lst)) (1+ total)))
  )
)

(defun shift (num lst)
  (cond
    ((null lst) nil)
    ; treat num as a counter
    ((> num 0) (shift (- num 1) (append (cdr lst) (list (car lst)))))
    (t lst)
  )
)

(defun pivot (num lst)
  (list (less-than num lst nil) (greater-than num lst nil))
)

; helper function to get a list less than pivot
(defun less-than (num lst result)
  (cond
    ((null lst) result)
    ((< (car lst) num)
      (less-than num (cdr lst) (append result (list (car lst))))
    )
    (t (less-than num (cdr lst) result))
  )
)

; helper function to get a list greater or equal to pivot
(defun greater-than (num lst result)
  (cond
    ((null lst) result)
    ((>= (car lst) num) (greater-than num (cdr lst) (append result (list (car lst)))))
    (t (greater-than num (cdr lst) result))
  )
)

(defun break-list (delim lst)
  (cond
    ((null lst) nil)
    (t (nreverse (break-list-helper delim lst (list) (list))))
  )
)

; helper function to recursively construct all the result
(defun break-list-helper (delim lst temp result)
  (cond
    ((null lst) result)
    ((equal delim (car lst))
      (if (not (null temp))
        (break-list-helper delim (cdr lst) (list) (cons temp result))
        (break-list-helper delim (cdr lst) (list) result)
      )
    )
    ((null (cdr lst)) (break-list-helper delim (cdr lst) (list) (cons (append temp (list (car lst))) result)))
    (t (break-list-helper delim (cdr lst) (append temp (list (car lst))) result))
  )
)

(defun quicksort (lst)
  (cond
    ((null lst) nil)
    (t
      (let* ((piv (car lst)) (progress (pivot piv lst)))
        (cond
          ((and (null (car progress)) (null (cadr progress))) nil)
          ((null (car progress)) (append (list piv) (quicksort (cdadr progress))))
          ((null (cadr progress)) (append (quicksort (car progress)) (list piv)))
          (t (append (quicksort (car progress)) (list piv) (quicksort (cdadr progress))))
        )
      )
    )
  )
)

; PART 2
(defun match (pattern assertion)
  (cond
    ((or (null pattern) (null assertion))
      (equal pattern assertion)
    )
    ((or (equal (car pattern) '?) (equal (car pattern) (car assertion)))
      (match (cdr pattern) (cdr assertion))
    )
    ((equal (car pattern) '!)
      (if (match (cdr pattern) (cdr assertion)) ;try taking ! as one character
        t
        (match pattern (cdr assertion)) ;try taking ! as multiple characters
      )
    )
  )
)

; PART 3
(defun matrix-add (m1 m2)
  (cond
    ((or (null m1) (null m2)) nil)
    (t (cons (addrow (car m1) (car m2)) (matrix-add (cdr m1) (cdr m2))))
  )
)

(defun addrow (r1 r2)
  (cond
    ((or (null r1) (null r2)) nil)
    (t (cons (+ (car r1) (car r2)) (addrow (cdr r1) (cdr r2))))
  )
)

(defun index (matrix x y)
  (getitem (getitem matrix x) y)
)

(defun getitem (row i)
  (cond
    ((null row) nil)
    ((> i 0) (getitem (cdr row) (- i 1)))
    (t (car row))
  )
)

(defun len (lst)
  (cond
    ((null lst) 0) ; if no item, return 0
    ; increment
    (t (1+ (len (cdr lst))))
  )
)

(defun matrix-multiply (m1 m2)
  (cond
    ((or (null m1) (null m2)) nil)
    (t (matrix-multiply-helper m1 (matrix-transpose m2)))
  )
)

(defun matrix-multiply-helper (m1 m2)
  (cond
    ((null m1) nil)
    (t (cons (multiply-row (car m1) m2) (matrix-multiply-helper (cdr m1) m2)))
  )
)

(defun multiply-row (r1 m2)
  (cond
    ((null m2) nil)
    (t (cons (dot-product r1 (car m2)) (multiply-row r1 (cdr m2))))
  )
)

(defun dot-product (r1 r2)
  (cond
    ((or (null r1) (null r2)) 0)
    (t (+ (* (car r1) (car r2)) (dot-product (cdr r1) (cdr r2))))
  )
)

(defun matrix-transpose (m)
  (cond
    ((null m) nil)
    ((null (car m)) nil)
    (t (cons (matrix-cars m) (matrix-transpose (matrix-cdrs m))))
  )
)

(defun matrix-cars (m)
  (cond
    ((null m) nil)
    (t (cons (caar m) (matrix-cars (cdr m))))
  )
)

(defun matrix-cdrs (m)
  (cond
    ((null m) nil)
    (t (cons (cdar m) (matrix-cdrs (cdr m))))
  )
)
