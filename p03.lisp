(defun element-at (xs i)
  (if (= 1 i)
    (car xs)
    (element-at (cdr xs) (- i 1))))

(print (element-at '(1 2 3 4) 3))
