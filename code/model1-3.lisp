;; a model for baby life cycle (model1-3.lisp)

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

;; actor do something for our  baby
(defclass actor () ())

;; baby's 
(defclass kuchi (actor) (
  (sita  :initform "kuchi/sita" :accessor sita))
)

(defparameter *kuchi* (make-instance 'kuchi))


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
    ;(sensors  :initform  '(*eyes* *hip-skin* *inside*) :accessor sensors)
    (inside   :initform *inside*    :accessor inside)
    (hip-skin :initform *hip-skin*  :accessor hip-skin)
    (eyes     :initform *eyes*      :accessor eyes) 

    (kuchi    :initform *kuchi* :accessor kuchi)
  )
)

(defparameter *baby* (make-instance 'baby))

;; external action
;; おむつ交換
(defun change-omutsu ()
  (setf (kept *omutsu*) new-omutsu)
)



;; sense 関係
(defmethod do-sense ((b baby))
  (cond 
    ((eq 0 (kept (target (inside b))))                       (target (inside b))) ;;when empty return zouki itself;;  
    ((equal dirt-omutsu (kept (what (target (hip-skin b))))) (what (target (hip-skin b))))

    ((what (target (eyes b)))                                (what (target (eyes b))))
  )
)

;;; actions
(defmethod cry ((k kuchi))
  (format t "cry by kuchi~%")
)


;;; what is ku and what is ka
;; emotion t is ku, nil is ka
(defclass emotion () ())
; ku = 苦
(defclass ku (emotion) ())
(defun isku (e) (equal e :ku))

; ka = kai = 快
(defclass ka (emotion) ())
(defun iska (e) (equal e :ka))

;; and emotion 
(defun eand (e1 e2)
  (cond
    ((eq e1 :ka) :ka)
    ((eq e2 :ka) :ka)
    (t :ku)
  )
)

(defmethod feel ((s omutsu)) 
  (if (equal dirt-omutsu (kept s)) :ku :ka)
)

;; 空腹知覚
(defmethod  kufuku ((i inofu))
  (<= (kept i) 0) 
)

(defmethod feel((i inofu))
  (if (<= (kept i) 0) :ku :ka)
)

(defmethod feel((o oppai))
  (format t "good for oppai~%")
)

(defparameter *feelnow* nil)

;; 吸う
;; mother's act card
(defun put-oppai ()
  (setf (what *chikaku*) *oppai*)
)

(defun remove-oppai ()
  (setf (what *chikaku*) nil)
)

;;泣く
;;泣く
(defmethod cry ((k kuchi))
  (format t "~%CRING!!! with ~a inofu=~a, chou=~a, omutsu=~a~%~%" k (kept *inofu*) (kept *chou*) (kept *omutsu*) )
)

;;吸う
(defmethod suck ((k kuchi))
  (let ()
    (setf (kept *inofu*) (+ (kept *inofu*) 5))
    (when (>= (kept *inofu*) 10) 
      (remove-oppai)
      (format t "reject oppai, enough~%"))
    (format t "suck milk by ~a~%~%" k)
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
          ((and (feel *inofu*) (see-oppai *chikaku*))         (suck *kuchi*))
          ((and (<= (kept *inofu*) 10)(see-oppai *chikaku*))  (suck *kuchi*))
          ((isku *feelnow*)                                   (cry *kuchi*))
          (t (format t "something unhappy things occured ~a of ~a at ~a~%" b s *feelnow*))
        )
      )
      (t (format t "the baby is good. everything ok. inofu=~a, chou=~a, feelnow=~a~%" (kept *inofu*) (kept *chou*) *feelnow* ))
    )
  )
)

;; for debugging
(defun set-chou (d)
  "for debugging"
  (setf (kept *chou*) d)
)


