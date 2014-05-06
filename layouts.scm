(load "misc.scm")
(load "properties.scm")

(define (vertical-box . args)
  (parse-args args contents properties)

  (define (to-html)
    (apply
      make-div
      (cons (list (cons "style" (string-append "height:100%; width:100%;"
                                               (css-handler properties))))
            (map (lambda (c) (string-append (c 'to-html) "<br>")) contents))))

  (lambda (msg)
    (match msg
      ['type 'content]
      ['to-html (to-html)])))

(define (n-columns-box . args)
  (parse-args args contents properties)
  
  (define (to-html)
    (let ([num-cols (length contents)])
      (define columns
        (map (lambda (content)
               (make-div (list (cons "style" 
                                     (format "height:100%; width:~A%; float:left;" 
                                             (quotient 100 num-cols))))
                         (content 'to-html)))
             contents))
      (define clear-float-div
        (make-div (list (cons "style" "float:clear"))))
      
      (apply make-div (cons (list (cons "style" (string-append "height:100%; width:100%"
                                                               (css-handler properties))))
                            (append columns (list clear-float-div))))))
  
  (lambda (msg)
    (match msg
           ['type 'content]
           ['to-html (to-html)])))



(define (n-rows-box . args)
  (parse-args args contents properties)
  
  (define (to-html)
    (let ([num-cols (length contents)])
      (define columns
        (map (lambda (content)
               (make-div (list (cons "style" 
                                     (format "height:~A%; width:100%;" 
                                             (quotient 100 num-cols))))
                         (content 'to-html)))
             contents))
      
      (apply make-div (cons (list (cons "style" (string-append "height:100%; width:100%"
                                                               (css-handler properties))))
                            columns))))
  
  (lambda (msg)
    (match msg
           ['type 'content]
           ['to-html (to-html)])))
