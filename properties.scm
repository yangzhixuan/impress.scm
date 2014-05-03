(load "misc.scm")

;--------------- property functions------------------------
(define-syntax define-property
  (syntax-rules ()
    [(_ property-name property-key)
     (define (property-name val)
       (define (set-val! new-val)
         (set! val new-val))
       (lambda (msg)
         (match msg
            ['key property-key]
            ['val val]
            ['type 'property]
            ['set-val! set-val!])))]))

(define (property? obj)
  (eq? (obj 'type) 'property))


;properties for slides meta information
(define-property title 'title)
(define-property impress.js-path 'impress.js-path)
(define-property css-path 'css-path)
(define-property charset 'charset)
(define-property viewpoint 'viewpoint)

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
