(defun my-but-last (xs)
  (if (null (cdr (cdr xs)))
    (car xs)
    (my-but-last (cdr xs))))

(print (my-but-last '(1 2 3 4)))
