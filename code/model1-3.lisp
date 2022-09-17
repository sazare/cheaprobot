;; a model for baby life cycle

(defconstant new-omutsu 'new-omutsu)
(defconstant dirt-omutsu 'dirt-omutsu)

;;;;; WHAT NAME IS THIS BABY?

;; objects outside of baby
(defclass chikaku () (
    (what :initform nil :accessor what))
)

(defparameter *chikaku* (make-instance 'chikaku))  ;; nil is nothing near

;
;
(defclass omutsu () (
    (kept :initform new-omutsu :accessor kept))
)
 
(defparameter *omutsu* (make-instance 'omutsu))   

(defclass sessyoku () (
    (what :initform *omutsu* :accessor what))
)
(defparameter *sessyoku* (make-instance 'sessyoku))

;; object in baby's body

(defclass inofu () (
    (kept :initform 5 :accessor kept))
)

(defparameter *inofu* (make-instance 'inofu))      ;; 5 hours

(defclass chou () (
    (kept :initform 0 :accessor kept))
)

(defparameter *chou* (make-instance 'chou))     ;; chou can keep  10 units.

;; sensible object outide of baby
(defclass oppai ()(
  (whos :initform 'mama :accessor whos))
)
    
(defparameter *oppai* (make-instance 'oppai))      ;; oppai can keep 6 units.

;; baby's 
(defclass zensin () (
  (whole :initform t :accessor kept))
)

(defparameter *zensin* (make-instance 'zensin))


;; sensor
(defclass sensor () (
    (target :initarg :target :accessor target))
)

(defclass eyes (sensor) ())
(defclass hip-skin (sensor) ())
(defclass inside (sensor) ())

(defparameter *eyes*     (make-instance 'eyes     :target *chikaku*))
(defparameter *hip-skin* (make-instance 'hip-skin :target *sessyoku*))
(defparameter *inside*   (make-instance 'inside   :target *inofu*))

(defclass baby () (
    (eyes     :initform *eyes*      :accessor eyes) 
    (hip-skin :initform *hip-skin*  :accessor hip-skin)
    (inside   :initform *inside*    :accessor inside)

    (zensin   :initform *zensin*  :accessor zensin) ;; t is baby itself
    (kuchi    :initform *chikaku* :accessor kuchi)
  )
)

;; actor

(defmethod cry ((b baby))
  (format t "cring ~a ~%" (kept (zensin b)))
)

;; おむつ交換
(defun change-omutsu ()
  (setf (kept *omutsu*) new-omutsu)
)

(defun set-chou (d)
  "for debugging"
  (setf (kept *chou*) d)
)

(defmethod do-unchi ((b baby))
  (let ()
    (when (>= (kept *chou*) 10) 
      (setf (kept *omutsu*) dirt-omutsu)
      (setf (kept *chou*) 0)
    )
  )
)

;; sense 関係
(defmethod do-sense ((b baby))
  (cond 
    ((what (target (eyes b)))                                (what (target (eyes b))))
    ((equal dirt-omutsu (kept (what (target (hip-skin b))))) (what (target (hip-skin b))))

    ((eq 0 (kept (target (inside b))))                       (target (inside b))) ;;when empty return zouki itself;;  
  )
)

;; 空腹知覚
(defmethod kufuku ((b baby))
  (<= (kept (target (inside b))) 0)
)

;; おっぱい見る
(defmethod sense ((b baby))
  (equal *oppai* (what *chikaku*))  
)


;;; what is ku and what is rak
(defclass emotion () ())
(defclass ku (emotion) ())
(defclass rak (emotion) ())

;; emotion t is ku, nil is rak
(defparameter *feelnow* nil)

(defmethod feel  ((s chikaku)) nil)
(defmethod feel  ((s omutsu)) (equal dirt-omutsu (kept s)))
(defmethod feel  ((s inofu)) (equal (kept s) 0))
(defmethod feel  ((s oppai)) T)


;;泣く
(defmethod cry ((b baby))
  (when (kept (zensin b)) (format t "cry with ~a~%" (zensin b)))
)

;; 吸う
(defmethod suck ((b baby))
  (incf (kept *inofu*))
)

;; mother's act card
(defun put-oppai ()
  (setf (what *chikaku*) *oppai*)
)

(defun remove-oppai ()
  (setf (what *chikaku*) nil)
)

;; 赤ちゃんの生命サイクル
(defmethod acycle ((b baby))
  (let (s)
    (setq s (do-sense b))
    (cond
      (s 
        (setq *feelnow* (feel s))
        (cond
          ((and *feelnow* (equal s *oppai*)) (suck b))
          (*feelnow* (cry b))
          (t (format t "i can't act ~a of ~a at ~a~%" b s *feelnow*))
        )
      )
      (t (format t "i ca't act ~a of ~a at ~a ~%" b s *feelnow*))
    )
  )
)


;; inside body automatic
(defmethod sucked ((b baby))
  (setf (kept *inofu*) (incf (kept *inofu*)))
)


(defun auto-body()
  (when (> (kept *inofu*) 0)
    (setf (kept *inofu*) (decf (kept *inofu*) ))
    (setf (kept *chou*)  (incf (kept *chou*)))
  )
)


(defparameter *baby* (make-instance 'baby))

(defparameter bb *baby*)

