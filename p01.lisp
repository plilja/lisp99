(defun my-last (xs)
  (if (null (cdr xs))
    (car xs)
    (my-last (cdr xs))))


(print (my-last '(1 2 3 4)))
