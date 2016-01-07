;plots a few sample plots
(define (refresh device)
 (clear device "white")
 (coord-grid-cart device "black"))

(define plot1 (make-graphics-device 'win32))


(refresh plot1)


(draw-line plot1 "blue" (make-line-cords
			(scale-point-list
			  (zip (range -5 5 .01)
			       (map sin
				    (range -5 5 .01)))
		         .2 .8)))

(draw-vector-list plot1 "red"
	(pair-zip
	  (scale-point-list
	    (zip (range -5 5 .1)
	         (map sin (range -5 5 .1)))
		  .2 .8)
	  (zip (map (lambda (x)
	              (* .2 (sin x)))
	            (range -5 5 .1))
	       (map (lambda (x)
	              (* .2 (cos x)))
	            (range -5 5 .1)))))

(label plot1 "black" "Plot 1" 0 .9)

(define plot2 (make-graphics-device 'win32))

(refresh plot2)

(vector-field-plot plot2 "purple" 
                   (range -1 1 .1) (range -1 1 .1)
		   (lambda (x y) 
		     (cons (* .1 y)
		           (* -.1 x))))

(label plot2 "black" "Plot 2" 0 .9)


