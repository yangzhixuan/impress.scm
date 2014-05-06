(load "misc.scm")

;--------------- property functions------------------------
(define-syntax define-property
  (syntax-rules ()
    [(_ property-name property-key)
     (define (property-name val . additional-args)
       (define (set-val! new-val)
         (set! val new-val))
       (lambda (msg)
         (match msg
            ['key property-key]
            ['val val]
            ['type 'property]
            ['additional additional-args]
            ['set-val! set-val!])))]))

(define (property? obj)
  (or (eq? (obj 'type) 'property)
      (eq? (obj 'type) 'css-property)))


;properties for slides meta information
(define-property title 'title)
(define-property impress.js-path 'impress.js-path)
(define-property css-path 'css-path)
(define-property charset 'charset)
(define-property viewpoint 'viewpoint)
(define-property output-filename 'output-filename)
(define-property slide-flow-style 'slide-flow-style)


;properties for slide
(define-property x-coordinate 'data-x)
(define-property y-coordinate 'data-y)
(define-property z-coordinate 'data-z)
(define-property x-rotate 'data-x-rotate)
(define-property y-rotate 'data-y-rotate)
(define-property z-rotate 'data-z-rotate)
(define-property scale 'data-scale)

;css-properties
(define-syntax define-css-property
  (syntax-rules ()
    ((_ css-name css-key css-transformer)
     (define (css-name . args)
       (lambda (msg)
         (match msg
                ['type 'css-property]
                ['key css-key]
                ['val args]
                ['to-css (apply css-transformer args)]))))))

(define (css-property? p)
  (eq? (p 'type) 'css-property))

(define (css-handler csses)
  (string-join (map (lambda (x) (x 'to-css)) csses) ";"))

(define-css-property font-size 'font-size 
   (lambda (s) 
     (format "font-size: ~A;" s)))

(define-css-property css-style 'css-style
   (lambda (s) s))

(define-css-property background-color 'bg-color 
   (lambda (r g b) 
      (format "background-color: rgb(~A,~A,~A);" r g b)))


(define (property->pair p)
  (cons (p 'key) (p 'val)))

(define (lookup-table table key)
  (cond [(null? table) #f]
        [(eq? ((car table) 'key) key) (car table)]
        [else (lookup-table (cdr table) key)]))


; merge properties set. Use value in old when conflicts
(define (merge-properties old new)
  (foldl (lambda (p l)
           (if (lookup-table l (p 'key))
             l
             (cons p l)))
         old new))

(define (print-properties-list l)
  (printf "~A~N"
          (map (lambda (p) (format "[~A -> ~A] " (p 'key) (p 'val))) l)))
