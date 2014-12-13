(defun int->nat (n)
  (if (= n 0)
      '(:O)
      `(:S ,(int->nat (- n 1)))))

(defun nat->int (n)
  (match n
    ((:O) 0)
    ((:S m) (+ 1 (nat->int m)))))

(defun ltake (n strm)
  (if (= n 0)
      '()
      (cons (@ hd strm) (ltake (1- n) (@ tl strm)))))

(print
 (mapcar #'nat->int (ltake 10 (@ iterate (@ plus (int->nat 2)) (int->nat 1)))))
