(defun list-eq (xs ys)
  (defun elem-eq? (x y)
    (if (listp x)
      (if (listp y)
        (list-eq x y)
      nil)
    (if (listp y)
      nil
      (= x y))))

  (if (null xs)
    (null ys)
    (if (null ys)
      nil
      (and (elem-eq? (car xs) (car ys)) (list-eq (cdr xs) (cdr ys))))))

(assert (list-eq () ()))
(assert (list-eq '(1) '(1)))
(assert (list-eq '(1 2 3 4) '(1 2 3 4)))
(assert (not (list-eq () '(1))))
(assert (not (list-eq '(1) ())))
(assert (not (list-eq '(1 2 3) '(1 2 3 4))))
(assert (not (list-eq '(1 2 3 4) '(1 2 3))))
(assert (not (list-eq '(2 1) '(1 2))))

; Nested lists
(assert (list-eq '(1 (2 (3 4) 5)) '(1 (2 (3 4) 5))))
(assert (not (list-eq '(1 (2 (3 4) 5)) '(1 (2 5)))))


(defun singleton? (xs)
  (if (null xs)
    nil
    (null (cdr xs))))

(assert (singleton? '(4)))
(assert (not (singleton? '(1 2))))
(assert (not (singleton? ())))


; Problem 1
(defun my-last (xs)
  (if (singleton? xs)
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
  (if (singleton? xs)
    xs
    (let ((ys (compress (cdr xs))))
      (if (= (car xs) (car ys))
        ys
        (cons (car xs) ys)))))

(assert (list-eq '(1 2 3 1) (compress '(1 1 1 2 3 1 1))))


; Problem 9
(defun pack (xs)
  (if (or (null xs) (singleton? xs))
    (list xs)
    (let ((ys (pack (cdr xs))))
      (if (= (car xs) (car (car ys)))
        (cons (cons (car xs) (car ys)) (cdr ys))
        (cons (list (car xs)) ys)))))

(assert (list-eq '((1 1 1 1) (2) (3 3) (1 1) (4) (5 5 5 5)) (pack '(1 1 1 1 2 3 3 1 1 4 5 5 5 5))))


; Problem 10
(defun encode (xs)
  (defun f (ys)
    (cons (len ys) (list (car ys))))
  (mapcar #'f (pack xs)))

(assert (list-eq '((4 1) (1 2) (2 3) (2 1) (1 4) (4 5)) (encode '(1 1 1 1 2 3 3 1 1 4 5 5 5 5))))


; Problem 11
(defun encode-modified (xs)
  (defun f (ys)
    (if (singleton? ys)
      (car ys)
      (cons (len ys) (list (car ys)))))
  (mapcar #'f (pack xs)))

(assert (list-eq '((3 1) 2 3 (2 1)) (encode-modified '(1 1 1 2 3 1 1))))
