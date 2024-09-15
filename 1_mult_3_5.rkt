#lang racket
(require plot)

; create a list of all the multiples and then
; remove the duplicates and sum them together.
(define (sum_with_lists limit)
  (apply +
         (remove-duplicates
          (append
 
           (map (lambda (number) (* 3 number))
                (range 1 (ceiling (/ limit 3))))

           (map (lambda (number) (* 5 number))
                (range 1 (ceiling (/ limit 5))))
           )
          )
         )
)

; just look at each number and figure out if it is a
; multiple of 3 or 5 and sum them up.
(define (sum_count limit)
  (for/fold ([sum 0]) ([number (in-range 1 limit)])
    (if (or (= 0 (modulo number 3))
            (= 0 (modulo number 5)))
        (+ sum number)
        sum)))


(define sum_count_run_totals 
(for/list ([i '(1000 10000 100000 1000000)])

  (define-values (result cpu-time real-time gc-time)
    (time-apply sum_count (list i) )
    )

  cpu-time
  )

)

(define sum_list_run_totals 
(for/list ([i '(1000 10000 100000 1000000 )])

  (define-values (result cpu-time real-time gc-time)
    (time-apply sum_with_lists (list i) )
    )

  cpu-time
  )

)

(define data (list sum_list_run_totals sum_count_run_totals))

; Generate X values as indices for each list of Y values
(define x-values (range (length (first data))))

; Extract Y values from each list
(define y-values1 (first data))  ; First list of Y values
(define y-values2 (second data)) ; Second list of Y values

; Combine X and Y values to create a list of points (for line plots)
(define points1 (map list x-values y-values1))
(define points2 (map list x-values y-values2))

; Create the plot
(parameterize ([plot-x-label "Index"]
               [plot-y-label "Values"]
               [plot-title "Line Plot of Y values"])
  (plot (list
         (lines points1 #:color "red" #:width 2)  ; Line for first list
         (lines points2 #:color "blue" #:width 2) ; Line for second list
         )))
