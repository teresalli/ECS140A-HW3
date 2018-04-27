; PART 1
(defun element-count (item lst)
  (cond
    ((null lst) 0) ; if no item, return 0
    ((equal item (car lst)) ; if match, increment
      (+ 1 (element-count item (cdr lst)))
    )
    (t ; otherwise, no match, simply continue
      (element-count item (cdr lst))
    )
  )
)

(defun min-mean-max (lst)
  (cond
    ((null lst) nil)
    (t
      (list (find-min lst (car lst))  (find-mean lst 0 0) (find-max lst (car lst)))
    )
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
    ((null lst) (/ num total))
    (t
      (find-mean (cdr lst) (+ num (car lst)) (1+ total))
    )
  )
)

(defun shift (num lst)
  (cond
    ((null lst) nil)
    ((> num 0)
      (shift (- num 1) (append (cdr lst) (list (car lst))))
    )
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
    (t
      (less-than num (cdr lst) result)
    )
  )
)

; helper function to get a list greater or equal to pivot
(defun greater-than (num lst result)
  (cond
    ((null lst) result)
    ((>= (car lst) num)
      (greater-than num (cdr lst) (append result (list (car lst))))
    )
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
    ((sorted lst) lst)
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

;small helper func for quicksort to check if list is sorted (ascending)
(defun sorted (lst)
    (cond
        ((= (list-length lst) 1) t)
        ((< (car lst) (car (cdr lst))) (sorted (cdr lst)))
        ((null lst) nil)
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
    (t
      (setq matrix (list))
      (dotimes (i (list-length m1))
        (setq row (list))
        (dotimes (j (list-length (car m1)))
          (setq row (append row (list (+ (index m1 i j) (index m2 i j) ))))
        )
        (setq matrix (cons row matrix))
      )
      (nreverse matrix)
    )
  )
)

(defun index (matrix x y)
  (nth y (nth x matrix))
)

(defun matrix-multiply (m1 m2)
  (cond
    ((or (null m1) (null m2)) nil)
    (t
      (setq matrix (list))
      (dotimes (i (list-length m1))
        (setq row (list))
        (dotimes (j (list-length (car m2)))
          (setq result 0)
          (dotimes (k (list-length m2))
            (setq result (+ result (* (index m1 i k) (index m2 k j) )))
          )
          (setq row (append row (list result)))
        )
        (setq matrix (cons row matrix))
      )
      (nreverse matrix)
    )
  )
)

(defun matrix-transpose (m)
  (cond
    ((or (null m)) nil)
    (t
      (setq matrix (list))
      (dotimes (i (list-length (car m)))
        (setq row (list))
        (dotimes (j (list-length m))
          (setq row (append row (list (index m j i) )))
        )
        (setq matrix (cons row matrix))
      )
      (nreverse matrix)
    )
  )
)
