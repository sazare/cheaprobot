;; a model for baby life cycle


;;;;; WHAT NAME IS THIS BABY?

;; objects outside of baby
(defclass chikaku () 
  (
    (what :initform nil :accessor what)
  )
)

(defparameter *chikaku* (make-instance 'chikaku))  ;; nil is nothing near

(defclass omutsu ()
  (
    (kept :initform nil :accessor kept)
  )
)
 
(defparameter *omutsu* (make-instance 'omutsu))   ;; nil is clear

;; object in baby

(defclass inofu ()
  (
    (kept :initform 0 :accessor kept)
  )
)

(defparameter *inofu* (make-instance 'inofu))      ;; 5 hours

(defclass chou ()
  (
    (kept :initform 0 :accessor kept)
  )
)

(defparameter *chou* (make-instance 'chou))     ;; chou can keep  10 units.

;; sensible object outide of baby
(defclass oppai ()())
    
(defparameter *oppai* (make-instance 'oppai))      ;; oppai can keep 6 units.

;; baby's 
(defclass sensor ()
  (
    (target :initarg :target :accessor target)
  )
)

(defclass eyes (sensor) ())
(defclass skin (sensor) ())
(defclass inner (sensor) ())

(defclass baby ()
  (
    (eyes   :initform (make-instance 'eyes :target *chikaku*) :accessor eyes) 
    (skin   :initform (make-instance 'skin :target *omutsu*)  :accessor skin)
    (inner  :initform (make-instance 'inner :target *inofu*)  :accessor inner)

    (body   :initform (make-instance 'crier :target t)        :accessor body) ;; t is baby itself
    (kuchi  :initform (make-instance 'sucker :target *chikaku*) :accessor kuchi)
  )
)

;; actor card
(defclass actor (sensor)
  (
    (towhat :initarg :towhat :accessor towhat)
  )
)

(defclass crier (actor) ())

(defmethod cry ((b baby))
  (format t "baby cry! ~a~%" (body b))
)

(defclass sucker (actor) ())

(defmethod suck ((b baby))
  (incf (inner b))
  (format t "baby suck ~a and baby's stomach is ~a~%" (kuchi b) (inner b))
)

;; おむつ交換
(defun change-omutsu ()
  (setf (kept *omutsu*) 'new)
)

(defmethod do-unchi ((b baby))
  (setf (kept *omutsu*) 'dart)
  (setf (kept *chou*) 0)
)


(defmethod do-sense ((b baby))
  (or 
    (target (eyes b))
    (equal 'dirt (kept (target (skin b))))
    (target (inner b))
  )
)

;; 空腹知覚
(defmethod inner ((b baby))
  (<= (kept *inofu*) 0)
)

;; おっぱい見る
(defmethod sense ((b baby))
  (equal *oppai* (what *chikaku*))  ;;??? *oppai* identical in *chikaku* (maybe oppai is only one for the baby)
)


;;; what is ku and what is kai 
(defclass emotion () ())
(defclass ku (emotion) ())
(defclass kai (emotion) ())

;; emotion t is ku, nil is kai
(defmethod emotion ((s chikaku)) nil)
(defmethod emotion ((s omutsu)) t)
(defmethod emotion ((s inofu)) (equal (kept s) 0))


;;泣く
(defmethod cry ((b baby))
  (when (body b) (format t "cry with ~a~%" (body b)))
)

;; 吸う


;; 赤ちゃんの生命サイクル
(defmethod acycle ((b baby))
  (let (s e)
    (setq s (do-sense b))
    (cond
      (s 
        (setq e (emotion s))
        (cond
          ((and (equal e 'ku) (equal s *oppai*)) (suck b))
          ((equal e 'ku) (cry b))
         (t (format t "i can't act ~a at ~a~%" b e))
        )
      )
      (t (setf (what *chikaku*) nil))
    )
  )
)


;; external action
;; mother's act card
(defun put-oppai ()
  (setf *chikaku* *oppai*)
)

(defun remove-oppai ()
  (setq *chikaku* nil)
)

;; inner body automatic
(defmethod sucked ((b baby))
  (setf (kept *inofu*) (incf (kept *inofu*)))
)

(defun auto-stomach ()
  (when (> (kept *inofu*) 0)
    (setf (kept *inofu*) (decf (kept *inofu*) ))
    (setf (kept *chou*)  (incf (kept *chou*)))
  )
)


(defparameter *baby* (make-instance 'baby))

(defparameter b1 *baby*)




