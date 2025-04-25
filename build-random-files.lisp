(defun create-random-file (filename size)
  "Generate the random file.
Using the `filename' and the `size'."
  (with-open-file (output-stream filename :element-type '(unsigned-byte 8)
					  :direction :output
					  :if-exists :supersede)
    (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
      (loop for remaining = size then (- remaining chunk)
	    while (> remaining 0)
	    for chunk = (min (length buffer) remaining)
	    do (progn
		 (dotimes (i chunk)
		   (setf (aref buffer i) (random 256)))
		 (write-sequence buffer output-stream :end chunk))))))
