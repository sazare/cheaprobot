;; baby  phase1 model2
;;;; NOT WORK
; 前提
; 赤ん坊の認識は命題論理のレベルだと思う。
; 監視できるもの
;; 便意でなく、おむつの中の状態を不愉快と感じるのが泣く原因ではないか
;; 空腹は体内の状態であり、特に空腹が引き金ではなく、たんなる苦しみがトリガーではないか。
; 行動の選択肢
;; 泣く
;; おっぱいをみたら吸う
;
;; この段階ではセンサーは、苦/快しか判定できないとする。



;; baby
;; 目(おっぱい)
;; お腹(体内)
;; お尻(皮膚)
; 動作
;; 舌(乳を吸う)
;; 全身(泣く)

;;;; 部位         センサー　　　　ActionCard
;;;1 目           おっぱい発見
;;;2 お腹(体内)   空腹
;;;3 お尻(皮膚)   異物接触
;;;4 口(舌)                       吸う
;;;5 口(声)                       声を出す


; お尻のActで排便は不要だと思う。このphaseでは自動的に出てしまう。
;; おしっこやうんちを自覚的に行わせるために教育しているので、教育がなければ勝手に出るのだろう。という仮説
;; 泣くのはうんちがしたいからでなく、うんちでおむつの中が気持ち悪くなるから。
;; 泣く原因が観測できなくなるまで泣き続ける。泣き止むトリガーはない。
;; 世界は命題で認識されている。



; 時間は30分単位。

; 授乳に30分/ 1単位時間
; ミルクの間隔は4時間ごと/ 8単位時間
; 空腹の間隔が8単位時間
; うんちの間隔は、母乳で12回としよう。2時間に1回か。4単位時間に1回。

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
    (skin   :initarg  :skin   :accessor skin)
    (inner  :initarg  :inner  :accessor inner)

    (body   :initarg  :body   :accessor body)
    (kuchi  :initarg  :kuchi  :accessor kuchi)
  )
)
;pretty print
(defmethod pp ((b baby))
  (format t "eyes: ~a, skin: ~a, inner: ~a, body: ~a~%" (eyes b)(skin b)(inner b)(body b))
)

(defclass sensor ()
  (
    (target :initarg :target :accessor target)
  )
)

(defclass eyes (sensor)
  (
    (near :initarg :near :accssor near )
  )
)

(defclass skin (sensor)
  (
    (feeling :initarg :feeling :accessor feeling)
  )
)

(defclass inner (sensor)
  (
    (empty :initarg :empty :accessor :empty)
  )
)

(defmethod pp ((s sensor))
  (format t "sensor: target: ~a~%" (target s))
)

;; action card
(defclass action ()
  (
    (dowhat :initarg :dowhat :accessor dowhat)
    (target :initarg :target :accessor target)
  )
)

(defmethod pp ((a action))
  (format t "sensor: target: ~a dowhat: ~a~%" (target a)(dowhat a))
)

(defclass cry (action))

(defclass suck (action))

;; おむつ交換
(defun change-omutsu ((b baby))
  (setf (b) 'new)
)

(defmethod do-unchi ((b baby))
  (setf (osiri b) 'dart)
)

(defmethod do-sense ((b baby))
  (or (eyes b) (skin b)(inner b))
)

;; 空腹知覚
(defmethod inner ((b body))
  (inner b)
)

;; おっぱい見る
(defmethod sense ((b baby))
  (equal (eyes b) 'oppai)
)

;; which ?
(defun sence ()
  (when *near* (list 'eyes *near*))
)
  
;;泣く

(defmethod action ((b baby) what target)
  (format t "do ~a with ~a~%" i f)
  (mylog b '泣く)
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

;;; body mech of the baby

; 胃袋 == 0 => 空腹
; 視界 に おっぱい :自動アクション =>吸う
; 吸う-> 胃袋++ <10
; 消化: 胃袋-> 腸
; 胃袋== 0 -> 空腹
; 腸 = 10 -> おむつ==よごれる  & 腸==0

センスの結果
;; 空腹 => 苦
;; おむつ汚れ => 苦
;; 胃袋 == 10 => 快
;; おっぱい => 快




;(defvar b1 (make-instance 'baby :eyes nil :skin nil :inner nil :osiri 'new))



