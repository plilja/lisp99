(setf *random-state* (make-random-state t))


(defun list-eq (xs ys)
  (defun elem-eq? (x y)
    (if (listp x)
      (if (listp y)
        (list-eq x y)
      nil)
    (if (listp y)
      nil
      (eq x y))))

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


(defun sum (xs)
  (if (null xs)
    0
    (+ (car xs) (sum (cdr xs)))))

(assert (= 6 (sum '(1 3 2))))
(assert (= 0 (sum '())))

(defun product (xs)
  (if (null xs)
    1
    (* (car xs) (product (cdr xs)))))

(assert (= 6 (product '(1 3 2))))
(assert (= 1 (product '())))


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


; Problem 21
(defun insert-at (elem xs i)
  (cond 
    ((<= i 0) (error "Index must be positive"))
    ((null xs) (singleton elem))
    ((= 1 i) (cons elem xs))
    (t (cons (car xs) (insert-at elem (cdr xs) (- i 1))))))

(assert-eq '(1 2 3 4) (insert-at 2 '(1 3 4) 2))
(assert-eq '(1 2 3) (insert-at 3 '(1 2) 3))


; Problem 22
(defun range (a b)
  (if (< b a)
    ()
    (cons a (range (+ a 1) b))))

(assert-eq '(4 5 6 7 8 9) (range 4 9))


; Problem 23
(defun rnd-select (xs n)
  (if (= 0 n)
    ()
    (let ((m (len xs)))
      (let ((i (+ 1 (random m))))
        (let ((k (element-at xs i)))
          (let ((left (remove-at xs i)))
            (cons k (rnd-select left (- n 1)))))))))


; Problem 24
(defun lotto-select (n m)
  (rnd-select (range 1 m) n))


; Problem 25
(defun rnd-permu (xs)
  (rnd-select xs (len xs)))


; Problem 26
(defun combination (n xs)
  (cond
    ((= 0 n) '(()))
    ((null xs) '())
    (t (let ((x (car xs)))
         (let ((sub (combination (- n 1) (cdr xs))))
            (let ((including-x (mapcar (lambda (ys) (cons x ys)) sub)))
                (let ((excluding-x (combination n (cdr xs))))
                    (append including-x excluding-x))))))))


(assert-eq '((1) (2)) (combination 1 '(1 2)))
(assert-eq '((1 2)) (combination 2 '(1 2)))
(assert-eq '((1 2) (1 3) (2 3)) (combination 2 '(1 2 3)))
(assert-eq '((1 2 3) (1 2 4) (1 3 4) (2 3 4)) (combination 3 '(1 2 3 4)))


; Problem 27
(defun group (xs sizes)
  (defun g (x groups)
    (cond 
      ((null groups) ())
      ((> (car (car groups)) 0) 
        (cons
          (append (list (append (list (- (car (car groups)) 1) x) (cdr (car groups)))) (cdr groups))
          (mapcar (lambda (y) (cons (car groups) y)) (g x (cdr groups)))))
      (t (mapcar (lambda (y) (cons (car groups) y)) (g x (cdr groups))))
     )) 

  (defun f (xs sizes)
    (cond
      ((null xs) (list sizes))
      (t (mapcan (lambda (y) (g (car xs) y)) (f (cdr xs) sizes)))))

  (let ((ys (f xs (mapcar (lambda (x) (list x)) sizes))))
    (mapcar (lambda (a) (mapcar (lambda (b) (cdr b)) a)) ys)))

(assert-eq '(((1))) (group '(1) '(1)))
(assert-eq '(((2) (1)) ((1) (2))) (group '(1 2) '(1 1)))
(assert-eq '(((3) (1 2)) ((2) (1 3)) ((1) (2 3))) (group '(1 2 3) '(1 2)))
(assert (= 756 (len (group '(1 2 3 4 5 6 7 8 9) '(2 2 5)))))


; Problem 28
(defun lsort (xs)
  (stable-sort xs (lambda (x y) (< (len x) (len y)))))

(assert-eq '((15) (4 5) (4 5) (13 14) (1 2 3) (6 7 8) (9 10 11 12)) (lsort '((1 2 3) (4 5) (6 7 8) (4 5) (9 10 11 12) (13 14) (15))))

(defun lfsort (xs)
  (defun count-freq (y xs)
    (cond
      ((null xs) 0)
      ((= (len (car xs)) (len y)) (+ 1 (count-freq y (cdr xs))))
      (t (count-freq y (cdr xs)))))
  (let ((c (copy-list xs)))
    (stable-sort c (lambda (x y) (< (count-freq x xs) (count-freq y xs))))))

(assert-eq '((9 10 11 12) (15) (1 2 3) (6 7 8) (4 5) (4 5) (13 14)) (lfsort '((1 2 3) (4 5) (6 7 8) (4 5) (9 10 11 12) (13 14) (15))))


; Problem 31
(defun is-prime (x)
  (defun iter (i j)
    (cond
      ((> i j) t)
      ((= 0 (rem x i)) nil)
      (t (iter (+ i 1) j))))
  (if (= 1 x)
    nil
    (iter 2 (isqrt x))))

(assert (not (is-prime 1)))
(assert (not (is-prime 4)))
(assert (not (is-prime 6)))
(assert (is-prime 2))
(assert (is-prime 7))
(assert (is-prime 13))


; Problem 32
(defun my-gcd (a b)
  (if (= 0 b)
    a
    (my-gcd b (mod a b))))

(assert (= 9 (my-gcd 36 63)))
(assert (= 1 (my-gcd 13 4)))


; Problem 33
(defun coprime (a b)
  (= 1 (my-gcd a b)))

(assert (coprime 35 64))
(assert (not (coprime 2 4)))


; Problem 34
(defun totient-phi (m)
  (len (remove-if-not (lambda (i) (coprime i m)) (range 1 m))))

(assert (= 4 (totient-phi 10)))


; Problem 35
(defun prime-factors (m)
  (defun f (i j k)
    (cond
      ((> i j) (list k))
      ((= 0 (rem k i)) (cons i (f i (isqrt (/ k i)) (/ k i))))
      (t (f (+ i 1) j k))))
  (f 2 (isqrt m) m))

(assert-eq '(3 3 5 7) (prime-factors 315))
(assert-eq '(13) (prime-factors 13))


; Problem 36
(defun prime-factors-mult (m)
  (mapcar (lambda (xs) (list (car (cdr xs)) (car xs))) (encode (prime-factors m))))

(assert-eq '((3 2) (5 1) (7 1)) (prime-factors-mult 315))


; Problem 37
(defun totient-phi-imp (m)
  (let ((factors (prime-factors-mult m)))
    (product (mapcar (lambda (x) (* (- (car x) 1) (expt (car x) (- (car (cdr x)) 1)))) factors))))

(assert (= (totient-phi 10) (totient-phi-imp 10)))
(assert (= (totient-phi 18) (totient-phi-imp 18)))
(assert (= (totient-phi 47) (totient-phi-imp 47)))


; Problem 38
(defun primes (a b)
  (remove-if-not (lambda (m) (is-prime m)) (range a b)))

(assert-eq '(2 3 5) (primes 1 5))
(assert-eq '(3 5 7 11 13) (primes 3 16))


; Problem 39
(defun goldbach (m)
  (defun f (i)
    (let ((r (- m i)))
      (cond
        ((and (is-prime i) (is-prime r)) (list i r))
        (t (f (+ i 1))))))
  (if (= (rem m 2) 0)
    (f 2)
    (error "must provide even number to goldbach")))

(assert-eq '(5 23) (goldbach 28))
(assert-eq '(3 97) (goldbach 100))


; Problem 40
(defun goldbach-list (a b)
  (mapcar (lambda (x) (goldbach x)) (remove-if-not (lambda (x) (= (rem x 2) 0)) (range a b))))

(assert-eq '((3 7) (5 7) (3 11) (3 13) (5 13) (3 17)) (goldbach-list 9 20))


; Problem 46
(defun _and (a b)
  (if a
    b
    nil))

(assert (not (_and nil nil)))
(assert (not (_and t nil)))
(assert (not (_and nil t)))
(assert (_and t t))

(defun _or (a b)
  (if a
    t
    b))

(assert (not (_or nil nil)))
(assert (_or t nil))
(assert (_or nil t))
(assert (_or t t))

(defun _nand (a b)
  (not (_and a b)))

(assert (_nand nil nil))
(assert (_nand t nil))
(assert (_nand nil t))
(assert (not (_nand t t)))

(defun _nor (a b)
  (not (_or a b)))

(assert (_nor nil nil))
(assert (not (_nor t nil)))
(assert (not (_nor nil t)))
(assert (not (_nor t t)))

(defun _xor (a b)
  (if a
    (not b)
    b))

(assert (not (_xor nil nil)))
(assert (_xor t nil))
(assert (_xor nil t))
(assert (not (_xor t t)))

(defun _impl (a b)
  (if a
    b
    t))

(assert (_impl nil nil))
(assert (not (_impl t nil)))
(assert (_impl nil t))
(assert (_impl t t))

(defun _equ (a b)
  (if a
    b
    (not b)))

(assert (_equ nil nil))
(assert (not (_equ t nil)))
(assert (not (_equ nil t)))
(assert (_equ t t))

(defun table (pred)
  (list
    (list t t (funcall pred t t))
    (list t nil (funcall pred t nil))
    (list nil t (funcall pred nil t))
    (list nil nil (funcall pred nil nil))
    ))

(assert-eq '((t t t) (t nil nil) (nil t nil) (nil nil nil)) (table #'_and))
(assert-eq '((t t t) (t nil t) (nil t t) (nil nil nil)) (table #'_or))
