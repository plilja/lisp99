(defun len (xs)
  (if (null xs)
    0
    (+ 1 (len (cdr xs)))))

(print (len '(1 2 3 4 5)))


