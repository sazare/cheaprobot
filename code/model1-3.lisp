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
    (kept :initform 0 :accessor kept))
)

(defparameter *inofu* (make-instance 'inofu))

(defclass chou () (
    (kept :initform 0 :accessor kept))
)

(defparameter *chou* (make-instance 'chou))

;; sensible object outide of baby
(defclass oppai ()(
  (whos :initform 'mama :accessor whos))
)
    
(defparameter *oppai* (make-instance 'oppai))

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

(defparameter *baby* (make-instance 'baby))

;; actor
;; おむつ交換
(defun change-omutsu ()
  (setf (kept *omutsu*) new-omutsu)
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
(defmethod kufuku ((i inofu))
  (<= (kept i) 0)
)

;;; what is ku and what is rak
;; emotion t is ku, nil is rak
(defclass emotion () ())
(defclass ku (emotion) ())
(defclass rak (emotion) ())

(defparameter *feelnow* nil)

(defmethod feel ((s omutsu)) (equal dirt-omutsu (kept s)))
(defmethod feel ((s inofu)) (equal (kept s) 0))
(defmethod feel ((s oppai)) (kufuku *inofu*))


;;泣く
(defmethod cry ((b baby))
  (format t "~%CRING!!! ~a ~%~%" (zensin b))
)


;; 吸う
;; mother's act card
(defun put-oppai ()
  (setf (what *chikaku*) *oppai*)
)

(defun remove-oppai ()
  (setf (what *chikaku*) nil)
)

;;吸う
(defmethod suck ((b baby))
  (let ()
    (setf (kept *inofu*) (+ (kept *inofu*) 5))
    (when (>= (kept *inofu*) 10) (remove-oppai))
  )
)


;; inside body automatic
;; 体内の進行
(defun do-digest ()
  (let ()
    (unless (kufuku *inofu*)
      (setf (kept *chou*)  (+ (kept *chou*) 1))
      (setf (kept *inofu*) (- (kept *inofu*) 1))
    )
  )
)

(defun do-unchi ()
  (when (>= (kept *chou*) 10)  
    (setf (kept *omutsu*) dirt-omutsu)
    (setf (kept *chou*) 0)
  )
)


(defun auto-body ()
  (let ()
    (do-digest)
    (do-unchi)
  )
)

;; おっぱい見る
(defmethod see-oppai((c chikaku))
  (equal *oppai* (what c))
)

;; 赤ちゃんの生命サイクル
(defmethod bcycle ((b baby))
  (let (s)
    (auto-body) 
    (setq s (do-sense b))
    (cond
      (s 
        (setf *feelnow* (feel s))
        (cond
          ((and (kufuku *inofu*) (see-oppai *chikaku*))       (suck b))
          ((and (<= (kept *inofu*) 10)(see-oppai *chikaku*))  (suck b))
          (*feelnow*                                          (cry b))
          (t (format t "something unhappy things occured ~a of ~a at ~a~%" b s *feelnow*))
        )
      )
      (t (format t "the baby is good. everything ok. feelnow=~a~%" *feelnow* ))
    )
  )
)

;; for debugging
(defun set-chou (d)
  "for debugging"
  (setf (kept *chou*) d)
)


