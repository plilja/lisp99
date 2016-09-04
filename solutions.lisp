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


(defun assert-eq (expected actual)
  (if (not (list-eq expected actual))
    (error (format nil "Lists are not equal~%expected: ~a~%actual: ~a" expected actual))))


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

(assert-eq '(1 2 3) (init '(1 2 3 4)))


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

(assert-eq '(4 3 2 1) (my-reverse '(1 2 3 4)))


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

(assert-eq '(1 2 3) (my-flatten '(1 2 3)))
(assert-eq '(1 2 3 4 5) (my-flatten '(1 (2 (3 4) 5))))


; Problem 8
(defun compress (xs)
  (if (singleton? xs)
    xs
    (let ((ys (compress (cdr xs))))
      (if (= (car xs) (car ys))
        ys
        (cons (car xs) ys)))))

(assert-eq '(1 2 3 1) (compress '(1 1 1 2 3 1 1)))


; Problem 9
(defun pack (xs)
  (if (or (null xs) (singleton? xs))
    (list xs)
    (let ((ys (pack (cdr xs))))
      (if (= (car xs) (car (car ys)))
        (cons (cons (car xs) (car ys)) (cdr ys))
        (cons (list (car xs)) ys)))))

(assert-eq '((1 1 1 1) (2) (3 3) (1 1) (4) (5 5 5 5)) (pack '(1 1 1 1 2 3 3 1 1 4 5 5 5 5)))


; Problem 10
(defun encode (xs)
  (defun f (ys)
    (cons (len ys) (list (car ys))))
  (mapcar #'f (pack xs)))

(assert-eq '((4 1) (1 2) (2 3) (2 1) (1 4) (4 5)) (encode '(1 1 1 1 2 3 3 1 1 4 5 5 5 5)))


; Problem 11
(defun encode-modified (xs)
  (defun f (ys)
    (if (singleton? ys)
      (car ys)
      (cons (len ys) (list (car ys)))))
  (mapcar #'f (pack xs)))

(assert-eq '((3 1) 2 3 (2 1)) (encode-modified '(1 1 1 2 3 1 1)))


; Problem 12
(defun decode-modified (xs)
  (defun expand (ys)
    (if (= 0 (car ys))
      ()
      (cons (car (cdr ys)) (expand (cons (- (car ys) 1) (cdr ys))))))

  (if (null xs)
    ()
    (let ((x (car xs)))
          (if (listp x)
            (append (expand x) (decode-modified (cdr xs)))
            (cons x (decode-modified (cdr xs)))))))

(assert-eq '(1 1 1 2 3 1 1) (decode-modified '((3 1) 2 3 (2 1))))


; Problem 13
(defun singleton (x) ; TODO look for built in solution
  (cons x '()))
(defun encode-direct (xs)
  (defun encode-direct-helper (xs acc)
    (if (null xs)
      acc
      (let ((x (car xs)) (y (car acc)))
        (if (listp y)
          (if (= x (car (cdr y)))
            (encode-direct-helper (cdr xs) (cons (cons (+ (car y) 1) (singleton x)) (cdr acc)))
            (encode-direct-helper (cdr xs) (cons x acc)))
          (if (= x y)
            (encode-direct-helper (cdr xs) (cons (cons 2 (singleton x)) (cdr acc)))
            (encode-direct-helper (cdr xs) (cons x acc)))))))
  (reverse (encode-direct-helper (cdr xs) (singleton (car xs)))))
        
(assert-eq '((3 1) 2 3 (2 1)) (encode-direct '(1 1 1 2 3 1 1)))


; Problem 14
(defun dupli (xs)
  (if (null xs)
    ()
    (let ((head (car xs)))
      (cons head (cons head (dupli (cdr xs)))))))

(assert-eq '(1 1 2 2 3 3) (dupli '(1 2 3)))


; Problem 15
(defun repli (xs n)
  (defun f (xs m)
    (if (null xs)
      ()
      (if (= 0 m)
        (f (cdr xs) n)
        (cons (car xs) (f xs (- m 1))))))
  (f xs n))

(assert-eq '(1 1 1 2 2 2 3 3 3) (repli '(1 2 3) 3))


; Problem 16
(defun drop-every (xs n)
  (defun f (xs m)
    (if (null xs)
      ()
      (if (= 1 m)
         (f (cdr xs) n)
         (cons (car xs) (f (cdr xs) (- m 1))))))
  (f xs n))

(assert-eq '(1 2 4 5) (drop-every '(1 2 3 4 5 6) 3))


; Problem 17
(defun split (xs n)
  (if (< n 0)
    (error "Negative index"))
  (if (null xs)
    ()
    (if (= 0 n)
      (cons '() (singleton xs))
      (let ((r (split (cdr xs) (- n 1))))
        (cons (cons (car xs) (car r)) (cdr r))))))

; Problem 18
(defun drop (xs n)
  (if (or (= 0 n) (null xs))
    xs
    (drop (cdr xs) (- n 1))))

(assert-eq '(3 4 5) (drop '(1 2 3 4 5) 2))

(defun take (xs n)
  (if (or (= 0 n) (null xs))
    ()
    (cons (car xs) (take (cdr xs) (- n 1)))))

(assert-eq '(1 2 3) (take '(1 2 3 4 5) 3))

(defun splice (xs a b)
  (drop (take xs b) (- a 1)))

(assert-eq '(3 4 5 6 7) (splice '(1 2 3 4 5 6 7 8 9 10) 3 7))


; Problem 19
(defun rotate (xs n)
  (let ((m (len xs)))
      (let ((ys (split xs (mod n m))))
        (append (car (cdr ys)) (car ys)))))

(assert-eq '(4 5 6 7 8 1 2 3) (rotate '(1 2 3 4 5 6 7 8) 3))
(assert-eq '(7 8 1 2 3 4 5 6) (rotate '(1 2 3 4 5 6 7 8) -2))


; Problem 20
(defun remove-at (xs i)
  (cond 
    ((<= i 0) (error "Index must be positive"))
    ((null xs) xs)
    ((= 1 i) (cdr xs))
    (t (cons (car xs) (remove-at (cdr xs) (- i 1))))))

(assert-eq '(1 3 4) (remove-at '(1 2 3 4) 2))
