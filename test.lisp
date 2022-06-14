(load "./lisbol.lisp")

(def-main-program 'test)

(in-program 'test)

(display "ok")

(defun tree-to-str (tree)
  (reduce (lambda (acc i)
	    (str:concat acc (string #\NEWLINE)
			(typecase i
			  (string i)
			  (t (reduce (lambda (acc x)
				       (str:concat acc (string #\NEWLINE) x))
				     i)))))
	  (cdr tree)
	  :initial-value (car tree)))

(let ((str (render)))
  (with-open-file (file "./test-rendered.cbl" :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (format file str)))
