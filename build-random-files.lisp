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

(defun random-string (len-chars)
  "Genera strings aleatorios, dado un tamaño `len-chars'."
  (let ((chars (concatenate 'string "abcdefghijklmnopqrstuvwxyz"
			    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			    "0123456789")))
    (labels ((random-element (lst)
	       (aref lst (random (length lst))))
	     (random-str (len-chars)
	       (if (zerop len-chars)
		   nil
		   (cons (random-element chars)
			 (random-str (1- len-chars))))))
      (concatenate 'string (random-str len-chars)))))

(defun main ()
  (let ((args (si:command-args)))
    (cond
      ((= (length args) 1)
       (progn
	 (format t "Nombre:~%")
	 (format t "random-file - Genera un archivo con data aleatoria del tamaño que el usuario determine.~%")
	 (format t "~%Uso: ~a archivo-a-generar tamaño~%~%" (car args))
	 (format t "Autor:~%")
	 (format t "     Escrito por Charte Erbeth.~%~%")
	 (format t "Informar de errores:~%")
	 (format t "Repositorio: https://github.com/Carht/build-random-files/issues~%")))
      ((= (length args) 3)
       (create-random-file (second args) (parse-integer (third args))))))
  (ext:quit 0))

(main)
(si:top-level
 (lambda ()
   (unwind-protect
	(main)
     (ext:quit 0))))
