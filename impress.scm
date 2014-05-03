(load "misc.scm")
(load "properties.scm")


;---------------content functions----------------------

(define (content? obj)
  (eq? (obj 'type) 'content))

(define (impress . args)
  (parse-args contents properties)
  
  ; default value for properties
  (define default-properties
    (list (impress.js-path "js/impress.js")
          (css-path "css/default.css")
          (charset "utf-8")
          (viewpoint "width=device-width, minimum-scale=1, maximum-scale=1, user-scalable=no")))

  ; map property into html tags
  (define (property-handler property)
    (let ([key (property 'key)] 
          [val (property 'val)])
      (match key
        ['title (make-title val)]
        ['css-path (make-css-link val)]
        ['impress.js-path (make-outside-script val)]
        ['charset (make-meta-char-set val)]
        ['viewpoint (make-meta-viewpoint val)]
        [other (error "impress: unknown property " other)])))

  ; use the default value if some property isn't specified
  (set! properties (merge-properties properties default-properties))

  (make-html
    (apply make-head
           (map property-handler properties))
    (apply make-body
           (map (lambda (x) (x 'to-html)) contents))))


(define (slide . args)
  (parse-args contents properties)

  (define (to-html)
    "TODO"
    )

  (lambda (msg)
    (match msg 
      ['to-html  (to-html)]
      ['type 'content]
      [other (error "slide: unknown message " msg)])))

