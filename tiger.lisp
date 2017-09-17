(in-package :vg-tiger)


(defparameter *tiger-parts* nil)

(defstruct part comdata comlen points style end)
;;
;; Our data comes without command vector length.  Since it ends in a 0, calc.
;;


;; Data is an array of 3 lists vecs and a style.
(defun part-make (datum)
  (let ((comlen (1- (length (first datum))))) ;because comlist has a :uchar
    (make-part :comlen comlen
	       :comdata  (malloc:vec1 (first datum))
	       :points    (malloc:vec1 (second datum))
	       :style     (malloc:vec1 (third datum)) 
	       :end (fourth datum))))


(defun tiger-parts ()
  (loop for i from 0
     for datum in *tiger-data*
     do
       (setf (aref *tiger-parts* i)(part-make datum))))


(defparameter *tiger-paths* nil)
(defparameter *tiger-stroke* nil)
(defparameter *tiger-fill* nil)

(defun tiger-paths ()
;;  (vg:with-path (temp (new-path)))
  (vg:set-i vg:matrix-mode vg:matrix-path-user-to-surface)
;;  (vg:translate -200.0 200.0)
  (vg:scale 1.0 -1.0)

;;  (format t "handles: ~A~&" (vg:handles-currently))
  (loop for part across *tiger-parts*
     for i from 0
     do
       (with-slots (comdata comlen points) part
	 (let (( temp (starky::new-path)))
	   (vg:clear-path temp vg:path-capability-all)
	   (vg:append-path-data temp comlen comdata points)

	     #||   (let ((path (vg:create-path vg:path-format-standard
	   vg:path-datatype-f
	   1.0 0.0
	   0 0(ti
	   (+ vg:path-capability-append-to
	   vg:path-capability-transform-to)
	   )))||#
	   ;;	       (vg:clear-path temp vg:path-capability-all)
	   ;;	       (vg:transform-path path temp)
	   (let ((err (vg:get-error) ))
	     (unless (zerop err)
	       (format t "~A ~A~&"i (vg:error-msg err))
	       (format t "appended ~A ~A ~A~&" comlen comdata points)
	        ) )
	   (setf (aref *tiger-paths* i) temp))))
  
;;  (format t "2handles: ~A~&" (vg:handles-currently))
  (setf *tiger-stroke* (vg:create-paint)
	*tiger-fill* (vg:create-paint))
  (vg:set-paint *tiger-stroke* vg:STROKE-PATH)
  (vg:set-paint *tiger-fill* vg:FILL-PATH)
  (vg:load-identity))

;;    "destroy paths.  destroy parts too to dealloc"
(defun tiger-paths-free ()
    (loop for path across *tiger-paths* do
	 (vg:destroy-path path)))


(defun tiger-display-part (i)
  (let* ((p (aref *tiger-paths* i))
	 (part (aref *tiger-parts* i))
	 (style (part-style part)) )
    (vg:set-parameter-fv
     *tiger-stroke* vg:paint-color 4 style)
    (vg:set-parameter-fv
     *tiger-fill* vg:paint-color 4
     (cffi-sys:inc-pointer style 16))
    (vg:set-f vg:stroke-line-width (cffi:mem-ref style :float  (* 8 4)))
    (vg:draw-path p (part-end part)))
  )
(defun tiger-display (x y &optional (scale 1.0) (rot 1.0))
;  (starky::start 1920 1080)
;;  (format t "3handles: ~A~&" (vg:handles-currently))
  (vg:translate x y)
  (vg:rotate rot)
  (vg:translate -100.0 -100.0)

  (vg:scale scale  scale)
  (vg:set-i vg:image-mode vg:draw-image-normal)
  (vg:set-paint *tiger-stroke* vg:stroke-path)
  (vg:set-parameter-i *tiger-stroke* vg:PAINT-TYPE vg:PAINT-TYPE-COLOR )
  (vg:set-paint *tiger-fill* vg:fill-path)
  (vg:set-parameter-i *tiger-fill* vg:PAINT-TYPE vg:PAINT-TYPE-COLOR )

  (loop  for i from 0 to 239 do    (tiger-display-part i) )
;;  (format t "4handles: ~A~&" (vg:handles-currently))
  ;;(tiger-display-part i)

 ;; (vg:flush)
 ;; (egl:swap-buffers native::*surface*)
  )

(defun tiger-in ()
  (setf *tiger-parts* (make-array 240)
	*tiger-paths* (make-array 240))
  (starky::tin)
  (tiger-parts)
  (tiger-paths)
  (mouse:open)
;  (ev-open "/dev/input/event0")
  )

(defun tiger-out ()
;;  (ev-close)
  (vg:handles-free)
  (malloc:free)
  (setf *tiger-paths* nil)
  (setf *tiger-parts* nil)
  (starky::tout)
  (mouse:close)
  )

(defparameter *ra* nil)

(defparameter *stops*
  {0.0  0.15882353 0.2137255 0.16862746 1.0 
  0.5  0.10392157 0.1372549 0.2254902  1.0 
  1.0  0.11764706 0.1882353 0.19607843 1.0 })

#||
(defun tiger-test ()
  (mm)
  (setf *mouse-right* 0)
  (let (;;(v #{10.0 10.0 100.0 200.0})
	
	)
    (loop for q =  (mm)
       for radius = 280.0 then (* radius 0.99)
       until (> *mouse-right* 0)  do
	 (vg:with-handles
	   (malloc:with
	     ;;(let ((handles (vg:handles-currently))))
	     (setf *ra* radius)
	     (starky::start 1920 1080)
	     ;;       (starky::bgr (0.0 0.1 0.1 1.0))

	     (starky::fill-radial-gradient 600.0 300.0 500.0 300.0 radius *stops* 3)
	     (starky::rect 0.0 0.0 1920.0 1080.0)
	     (tiger-display (float  *mouse-x*) (float *mouse-y*) 1.0 (float *mouse-wheel*))
	     (starky::end))
;;	   (format t "cleaned up ~A handles" (- (vg:handles-currently) vg:handles-currently))
	   ;;(vg:handles-free handles)
	   )
	 ))
  )
||#

(let ((x 0)
      (y 0)
      (w 0)
      (bleft nil)
      (bmid nil)
      (bright nil)
      (radius 280.0)
      (scale 1.0))
  
  (defun tiger-iteration ()
    (mouse:handle-events (lambda (xx yy ww ll mm rr)
			   (setf x xx
				 y yy
				 w ww
				 bleft ll
				 bmid mm
				 bright rr
				 )
			   (if (= 1 ll) (setf scale (* scale 1.05)))
			   
			   (if (= 1 rr) (setf scale (* scale 0.95)))
			   ))
    (setf radius (* radius .999))
 ;;   (print y)
   ;; (print bright)
    (vg:with-handles
      (malloc:with
	;;(let ((handles (vg:handles-currently))))
	(starky::start 1920 1080)
	(starky::fill-radial-gradient 600.0 300.0 500.0 300.0 radius *stops* 3)
	(starky::rect 0.0 0.0 1920.0 1080.0)
	(tiger-display (float  x) (float y) scale 180.0)
	(starky::end)))
    bmid))

(defun tiger ()
  (loop while (= 0 (tiger-iteration ))))

;; ))
#||
(defun tiger-test1 ()
  (starky::with-vec (scissor '(0 0 1920 1080))
    (vg:set-i vg:scissoring 1)
    (vg:set-iv vg:scissor-rects 4 scissor))
  (let ((x 600.0)
	(y 600.0)
	(rot 0.0)
	(scale 1.0)
	(inc-x (random 5.0))
	(inc-y (random 5.0))
	(inc-scale (random 0.04))
	(inc-rot (random 2.0)))
    (loop for i from 1 to 300 do
	 (when (or (> x 1500.0)
		 (< x 200.0))
	   (setf inc-x (- inc-x)))
	 (when (or (> y 900.0)
		   (< y 200.0))
	   (setf inc-y (- inc-y)))
	 (when (or (> scale 2.0)
		   (< scale .3))
	   (setf inc-scale (- inc-scale)))
	 (tiger-display x y scale rot)
	 (setf x (+ x inc-x)
	       y (+ y inc-y)
	       rot (+ rot inc-rot)
	       scale (+ scale inc-scale)))))

  ||#
