(ql:quickload 'str)

(load "./render.lisp")

(defvar *shift-spaces* "       ")

(defclass compilation-unit ()
  ((name :initform nil :initarg :name :accessor name)
   (is-initial :initarg :is-initial :accessor is-initial)
   (environment :initarg :environment :accessor environment :initform nil)
   (procedure :initarg :procedure :accessor procedure :initform nil)))

(defvar *compilations*)
(setq *compilations* nil)

(defvar *compilation*)
(setq *compilation* nil)

(defun in-program (name)
  (setq *compilation*
	(cdr (assoc name *compilations*))))

(defun def-program-tree (name &key (initial nil) (environment nil))
  (let ((inst (make-instance 'compilation-unit
			     :name name
			     :is-initial initial
			     :environment environment)))
    (setq *compilations*
	  (acons name inst *compilation*))))

(defun def-program (name &key environment)
  (def-program-tree name :initial nil :environment environment))

(defun def-main-program (name &key environment)
  (def-program-tree name :initial t :environment environment))

(defun def-environment (&key (source nil) (object nil))
  `((:source-computer ,source)
    (:object-computer ,object)))

(defun display (text)
  (let ((to-render `((:display ,text)))
	(value (slot-value *compilation* 'procedure)))
    (setf (slot-value *compilation* 'procedure)
	  (append value to-render))))
