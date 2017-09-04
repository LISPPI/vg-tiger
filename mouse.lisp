(in-package :vg-tiger)
#||
struct timeval {
	__kernel_time_t		tv_sec;		/* seconds */
	__kernel_suseconds_t	tv_usec;	/* microseconds */
};

struct input_event {
	struct timeval time;
	__u16 type;
	__u16 code;
	__s32 value;
}
(defcstruct event
  (sec :uint)
  (usec :uint)
  (type :uint16)
  (code :uint16)
  (value :uint32))

;#define EV_SYN			0x00
#define EV_KEY			0x01
#define EV_REL			0x02
#define EV_ABS			0x03
#define EV_MSC			0x04
#define EV_SW			0x05
#define EV_LED			0x11
#define EV_SND			0x12
#define EV_REP			0x14
#define EV_FF			0x15
#define EV_PWR			0x16
#define EV_FF_STATUS		0x17
#define EV_MAX			0x1f
#define EV_CNT			(EV_MAX+1)


||#
;; this is a bullshit event handler for mouse
;; events.  We just keep the latest data...


(defparameter *buffer* nil)
(defparameter *file* nil)
(defun ev-open (fname)
  (setf *buffer* (foreign-alloc :uint :count 4))
  (setf *file* (foreign-funcall "open"
			     :string fname
			     :uint #x800	;ro, non-block
			     :int )))
(defcfun ("close" cclose) :int
  (fd :int))
(defun ev-close ()
  (cclose *file*))

(defcfun ("read" cread) :int
  (fd :int)
  (buf :pointer)
  (size :int))

(defun ev-read ()
  (cread *file* *buffer* 16))

(defun z ()
   (print (cread *file* *buffer* 16 ))
;  (fread *q* 16 1 *f*)
  (starky::%print-mem *buffer*))

(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)
(defparameter *mouse-wheel* 180)
;; no threads! what?  Ok, for now just poll
(defparameter *mouse-left* 0)
(defparameter *mouse-right* 0)

(defun mm ()
  (loop for result =  (ev-read)
       until (= -1 result) do
       
       (let ((type (mem-ref *buffer* :uint16 8))
	     (code (mem-ref *buffer* :uint16 10))
	     (value (mem-ref *buffer* :int 12)))
	 ;;   (format t "   ~A ~A~&" code value)
	 ;;   (format t "ev: ~A~&" (mem-ref *q* :uint16 8))
	 (case type
	   (2 (case code ;;rel
		(0 (incf *mouse-x* (* 2 value))
		   (when (< 1920 *mouse-x*)
		     (setf *mouse-x* 1920))
		   (when (> 0 *mouse-x*)
		     (setf *mouse-x* 0)))
		
		(1 (decf *mouse-y* value)
		   (when (< 1080 *mouse-y*)
		     (setf *mouse-y* 1080))
		   (when (> 0 *mouse-y*)
		     (setf *mouse-y* 0)))
		(8  (incf *mouse-wheel* (* 2 value))
		    (when (< 360 *mouse-wheel*)
		      (setf *mouse-wheel* 360))
		    (when (> 0 *mouse-wheel*)
		      (setf *mouse-wheel* 0)))
		(t (format t "Unhandled rel ~A ~A~&" code value))))
	   (1 (format t "press ~A ~A~&"code value) (force-output)
	    (case code
		(272 (setf *mouse-left* value)	; BTN_LEFT
		 )
		(273 (setf *mouse-right* value)		; BTN right
		 )))
	  
	   
	   (0)
	   (t (format t "Unhandled message ~A ~A ~A~&"type code value))))
	 
     ;;  (format t "   ~A ~A~&" *mouse-x* *mouse-y*)
       )
;;  (force-output)
  )



