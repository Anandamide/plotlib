README.txt

This is a lightweight plotting utility for MIT/GNU-Scheme for windows users
Allows for plotting of various smooth functions and vector fields. Tutorial
will be added soon.

To try it add these files to the mit-scheme folder, open mit-scheme and enter
(load "plot.scm")

The library offers graphics at levels of abstraction, with make-plot being the
highest level and things like draw-line, draw-vector and draw-seg-list providing
primitives to build with.

Make plot can make any other plots and does so simply. For the simplest plot
use 'simple-plot' or 'simple-scale-plot':

First make a graphics device for output:
(define window (make-graphics-device 'win32'))

Its then as easy as:
(make-plot device 'mode '(args))

For the simple plot use:
(make-plot window 'simple-plot (list "white" "black" "red"
                                     #|bgcolor  grid-color line-color|#
                                     (list (range -1 1 .001) (lambda (x)
                                                               (+ (* x x)
                                                                  (* 2 x)
                                                                  -3)))))
                                    #|     input range       single variable
                                                              function
