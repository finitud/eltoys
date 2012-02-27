(setq *x-viewport-size* 80)
(setq *y-viewport-size* 25)
(setq *char-sequence* ["@" "#" "$" "%" "%" "*" "/" ":" "~" "."])

(defun get-dx (xmin xmax)
  (/ (- xmax xmin) *x-viewport-size*))

(defun get-x (xmin dx iter)
  (+ xmin (* iter dx)))

(defun get-dy (ymin ymax)
  (/ (- ymax ymin) *y-viewport-size*))

(defun get-y (ymin dy iter)
  (+ ymin (* iter dy)))

(defun cabs (r i)
  (+ (* r r) (* i i)))

(defun re-iter (r i)
  (- (* r r) (* i i)))

(defun im-iter (r i)
  (* 2.0 r i))

(defun mandelbrot-char (n)
  (elt *char-sequence* (mod n 10)))

(defun mandelbrot-iter (x y nmax re im n)
  (cond
   ((> (cabs re im) 2.0) (mandelbrot-char n))
   ((< n nmax) (mandelbrot-iter x y nmax
				(+ (re-iter re im) x)
				(+ (im-iter re im) y)
				(1+ n)))
   (t " ")))

(defun mandelbrot (xmin xmax ymin ymax nmax)
  "Draws an ASCII Mandelbrot set"
  (let ((dx (get-dx xmin xmax))
	(dy (get-dy ymin ymax)))
    (dotimes (yn *y-viewport-size*)
      (dotimes (xn *x-viewport-size*)
	(insert
	 (mandelbrot-iter (get-x xmin dx xn)
			  (get-y ymin dy yn)
			  nmax
			  0.0 0.0 0)))
      (insert "\n"))))
