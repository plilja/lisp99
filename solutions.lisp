(defun list-eq (xs ys)
  (if (null xs)
    (null ys)
    (if (null ys)
      nil
      (and (= (car xs) (car ys)) (list-eq (cdr xs) (cdr ys))))))

(assert (list-eq () ()))
(assert (list-eq '(1) '(1)))
(assert (list-eq '(1 2 3 4) '(1 2 3 4)))
(assert (not (list-eq () '(1))))
(assert (not (list-eq '(1) ())))
(assert (not (list-eq '(1 2 3) '(1 2 3 4))))
(assert (not (list-eq '(1 2 3 4) '(1 2 3))))
(assert (not (list-eq '(2 1) '(1 2))))

; Problem 1
(defun my-last (xs)
  (if (null (cdr xs))
    (car xs)
    (my-last (cdr xs))))

(assert (= 4 (my-last '(1 2 3 4))))


(defun init (xs)
  (if (null (cdr xs))
    ()
    (cons (car xs) (init (cdr xs)))))

(assert (list-eq '(1 2 3) (init '(1 2 3 4))))


; Problem 2
(defun my-but-last (xs)
  (my-last (init xs)))

(assert (= 3 (my-but-last '(1 2 3 4))))


; Problem 3
(defun element-at (xs i)
  (if (= 1 i)
    (car xs)
    (element-at (cdr xs) (- i 1))))

(assert (= 3 (element-at '(1 2 3 4 5) 3)))


; Problem 4
(defun len (xs)
  (if (null xs)
    0
    (+ 1 (len (cdr xs)))))

(assert (= 3 (len '(1 2 3))))


; Problem 5
(defun my-reverse (xs)
  (if (null xs)
    ()
    (cons (my-last xs) (my-reverse (init xs)))))

(assert (list-eq '(4 3 2 1) (my-reverse '(1 2 3 4))))


; Problem 6
(defun palindrome? (xs)
  (list-eq xs (my-reverse xs)))

(assert (palindrome? '(1)))
(assert (palindrome? '(1 2 3 2 1)))
(assert (not (palindrome? '(1 2 3))))


; Problem 7
(defun my-flatten (xs)
  (if (null xs)
    ()
    (if (listp (car xs))
      (append (my-flatten (car xs)) (my-flatten (cdr xs)))
      (cons (car xs) (my-flatten (cdr xs))))))

(assert (list-eq '(1 2 3) (my-flatten '(1 2 3)))) 
(assert (list-eq '(1 2 3 4 5) (my-flatten '(1 (2 (3 4) 5))))) 


; Problem 8
(defun compress (xs)
  (if (null (cdr xs))
    xs
    (let ((ys (compress (cdr xs))))
      (if (= (car xs) (car ys))
        ys
        (cons (car xs) ys)))))

(assert (list-eq '(1 2 3 1) (compress '(1 1 1 2 3 1 1))))




