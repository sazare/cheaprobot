;;; now abandoned 

;; phase plus 1
;; after birth

;; action
(defclass what ())
(defclass baby ())
(defclass inside ())
(defclass outside ())

(defgeneric action ((b baby)))

(defgeneric inside ((b inside)))

(defgeneric outside ((o outside)))

(defgeneric log ((w what)))

(defclass baby () ((omutu :initarg :omutu :accessor omutu)(onaka :initarg :onaka :accessor onaka)))
(defvar b1 (make-instance 'baby :omutu 'new :onaka 'full))
(omutu b1)
(setf (omutu b1) 'new)

