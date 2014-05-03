;bind contents and properties. frequently used in content functions
(define-syntax parse-args
  (syntax-rules ()
    [(_ contents properties)
     (begin
       (define contents (filter content? args))
       (define properties (filter property? args)))]))

;----------------html generate functions-------------------
(define (make-html head body)
  (format "<html>~N ~A ~N ~A ~N</html>" head body))

(define (make-head . tags)
  (format "<head>~N ~A ~N</head>" (string-join tags "\n")))

(define (make-body . tags)
  (format "<body>~N ~A ~N</body>" (string-join tags "\n")))

(define (make-div properties . tags)
  (define properties-string
    (string-join (map 
                   (lambda (pair) 
                     (format "~A=\"~A\"" (car pair) (cdr pair)))
                   properties)))
  (define tags-string
    (string-join tags "\n"))
  
  (format "<div ~A>~N ~A~N </div>" properties-string tags-string))

(define (make-title title)
  (format "<title>~A<title>" title))

(define (make-css-link src)
  (format "<link rel=\"stylesheet\" src=\"~A\">" src))

(define (make-outside-script src)
  (format "<script style=\"text/javascript\" src=\"~A\"></script>" src))

(define (make-meta-charset val)
  (format "<meta charset=\"~A\">" val))

(define (make-meta-viewpoint val)
  (format "<meta name=\"viewpoint\" content=\"~A\"" val))
