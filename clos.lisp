(defclass person ()
  (name))

(setf p (make-instance 'person))
(setf (slot-value p 'name) fred)
