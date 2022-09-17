; printobject baby

(defmethod po ((b baby))
  (format t "{")
  (po (eyes b))
  (po (hip-skin b))
  (po (inside b))
  (po (zensin b))
  (po (kuchi b))
  (format t "}")
)

(defmethod po ((e eyes))
  (po (target e))
)

(defmethod po ((h hip-skin))
  (po (target h))
)

(defmethod po ((i inside))
  (po (target i))
)

(defmethod po ((c chikaku))
  (format t "eyes: ~a " (what c))
)

(defmethod po ((s sessyoku))
  (format t "hip: ~a " (what s))
)

(defmethod po ((o omutsu))
  (format t "hip-skin: ~a " (kept o))
)

(defmethod po ((i inofu))
  (format t "inofu: ~a " (kept i))
)

(defmethod po ((c chou))
  (format t "chou: ~a " (kept c))
)

(defmethod po ((z zensin))
  (format t "zensin: ~a " (kept z))
)

(defmethod po ((i inofu))
  (format t "inofu: ~a " (kept i))
)


(defun pova ()
  (format t "*chikaku*=~a, *sessyoku*=~a, *omutsu*=~a, *inofu*=~a, *chou*=~a, *oppai*=~a(whos:~a)~%"
             (what *chikaku*) (what *sessyoku*) (kept *omutsu*) (kept *inofu*) (kept *chou*) *oppai* (whos *oppai*))
  (format t "*baby*=")
  (po *baby*)
)


