;; baby  phase1 model1


;; baby
; センサー 
;; 目
;; お腹(体内)
;; お尻(皮膚)
; 動作
;; 舌(乳を吸う)
;; 声(泣く)
;; 全身(泣く)

;;;; 部位         センサー　　　　ActionCard
;;;1 目           おっぱい発見
;;;2 お腹(体内)   空腹
;;;3 お尻(皮膚)   異物接触
;;;4 口(舌)                       吸う
;;;5 口(声)                       声を出す


; お尻のActで排便は不要だと思う。自動的に出てしまう。
;; おしっこやうんちを自覚的に行わせるために教育しているので、教育がなければ勝手に出るのだろう。という仮説
;; 泣くのはうんちがしたいからでなく、うんちでおむつの中が気持ち悪くなるから。
;; 泣く原因が観測できなくなるまで泣き続ける。泣き止むトリガーはない。
;; 世界は命題で認識されている。




; baby
;; loop for sencor 
;;; scan => fact
;;; fact ≡ (部位 状態)
;;  fact → instinct
;;; instinct : (苦 快 静)
;;  instinct ⇒ action
;;; action
;;;; 泣く
;;;; 吸う(乳)
;;
; external
;;;; おむつ交換
;;;; おっぱい→授乳


;; body structure
;;; センサー(身体部位; 目鼻耳舌皮膚)
;;;; 目 - おっぱい
;;;; 触 - おしり濡れ
;;;; 体内 -- 便意、空腹
;;; 
;;; 活動
;;;; 口/声 - 泣く(緊張)
;;;; 口/食 - 吸う(乳)
;;;; 肛門/排便 - うんち(弛緩)

;;; 接触世界
;;;; おむつ
;;;; 


(defparameter *near* nil)

;;;; 部位         センサー　　　　ActionCard
;;;1 目           おっぱい発見
;;;2 お腹(体内)   空腹
;;;3 お尻(皮膚)   異物接触
;;;4 口(舌)                       吸う
;;;5 口(声)                       声を出す、泣く

(defclass baby ()
  (
    (eyes   :initarg  :eyes   :accessor eyes) 
    (inner  :initarg  :inner  :accessor inner)
    (skin   :initarg  :skin   :accessor skin)
    (osiri  :initarg  :osiri  :accessor osiri)
    (kuchi  :initarg  :kuchi  :accessor kuchi)
    (nodo   :initarg  :nodo   :accessor nodo)
  )
)
;pretty print
(defmethod pp ((b baby))
  (format t "eyes: ~a, skin: ~a, inner: ~a, osiri: ~a, kuchi: ~a~%" (eyes b)(skin b)(inner b)(osirib))
)

;; inner/空腹 → 泣く
;; 目/おっぱい →
;; おっぱい/
(defmethod change-omutsu ((b baby))
  (setf (osiri b) 'new)
)

(defmethod do-unchi ((b baby))
  (setf (osiri b) 'dart)
)

(defmethod sence-inner ((b baby))
  (or (eyes b) (skin b)(inner b)(not (equal (osiri b)'new)))
)

(defun sence ()
  (when *near* (list 'eyes *near*))
)

(defun do-action (f i)
  (format t "do ~a with ~a~%" i f)
)

(defun emotion (f) 
  (cond
    (f 'pain)
    (t 'fun)
  )
)

(defmethod do-baby ((b baby))
  (let (f i)
    (setq f (sence-inner b))
    (cond
      (f (setq i (list f (emotion f))))
      (t (setq f 'eyes) (setq i (sence)))
    )
    (do-action f i)
  )
)

(defun oppai ()
  (setf *near* 'oppai)
)

;; mother's action
(defun put-oppai ()
  (setf *near* 'oppai)
)

;; body reaction

(defmethod sucked ((b baby))
  (let ((milk 100))
    (setf (omutsu b) 100)
  )
)
  
(defmethod spend-time ((b body))
  (o(sleep 10))
)


;(defvar b1 (make-instance 'baby :eyes nil :skin nil :inner nil :osiri 'new))



