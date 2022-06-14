(defun render-identification ()
  (str:concat *shift-spaces* "IDENTIFICATION DIVISION."))

(defun render-program (label)
  (str:concat *shift-spaces* "PROGRAM-ID. " (str:upcase label) "."))

(defun render-source-computer (args)
  (str:concat *shift-spaces* "SOURCE-COMPUTER. " (string (cadr args)) "."))

(defun render-object-computer (args)
  (str:concat *shift-spaces* "OBJECT-COMPUTER. " (string (cadr args)) "."))

(defvar *render-env-table*)
(setq *render-env-table*
      '((:source-computer . render-source-computer)
	(:object-computer . render-object-computer)))

(defun render-environment (args)
  (let* ((init "ENVIRONMENT DIVISION.")
	 (env (cdr args)))
    (cons init (mapcar (render-with-table *render-env-table*) env))))

(defun render-display (args)
  (str:concat *shift-spaces*
	      "DISPLAY "
	      (format nil "\"~a\"" (string (cadr args)))
	      "."))

(defvar *render-proc-table*)
(setq *render-proc-table*
      '((:display . render-display)))

(defun render-procedure (args)
  (let ((program (cdr args)))
    (mapcar (render-with-table *render-proc-table*) program)))

(defun render-end-program (str c)
  (with-slots (name is-initial) c
    (format str "~a~%" (str:concat *shift-spaces* "END PROGRAM " (str:upcase (string name)) "."))))

(defun render-with-table (table)
  (lambda (x)
    (let ((f (assoc (car x) table)))
      (if f
	  (funcall (cdr f) x)
	  (error (format nil "~a ~a" (car x) "function not defined"))))))

(defun render-header (str c)
  (with-slots (name) c
    (format str "~a~%" (render-identification))
    (format str "~a~%" (render-program (string name)))))

(defun render-procedure (str c)
  (with-slots (procedure) c
    (let ((r (render-with-table *render-proc-table*)))
      (format str "~a~%" (str:concat *shift-spaces* "PROCEDURE DIVISION."))
      (reduce (lambda (str x)
		(format str "~a~%" (funcall r x))
		str)
	      procedure
	      :initial-value str))))

(defun render-end (str c)
)

(defun render ()
  (with-output-to-string (str)
     (render-header str *compilation*)
     (render-procedure str *compilation*)
     (render-end-program str *compilation*)))
