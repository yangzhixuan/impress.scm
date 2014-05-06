(load "impress.scm")

;(printf "~A~N" 
;  (make-html
;    (make-head
;      (make-meta-charset "utf-8") 
;      (make-title "this is a title")
;      (make-css-link "mycss.css")
;      (make-outside-script "this.js"))
;    (make-body
;      (make-div (list (cons "class" "slide")) "hello"))))

(define (mylayout top-left top-right bottom)
  (n-rows-box (n-columns-box top-left top-right)
              bottom))

(impress (slide 
           (text "First text" (font-size "150%")) 
           (text "Second text")) 
         (slide
           (text "new slide"))
         (slide
           (n-columns-box
             (text "col1")
             (text "col2")
             (text "col3"))
           (font-size "30px")
           (background-color 100 200 100))
         (slide
           (n-rows-box
             (text "row1")
             (text "row2")
             (text "row3")))
         (slide
           (mylayout (text "I") (text "love") (text "scheme")))
         
         (slide)
         (slide)
         (slide)
         (slide)
         (slide)
         (slide)
         (slide)
         (slide)
         (slide)

         (title "new title") 
         (output-filename "templates/test.html")
         (slide-flow-style "circle" 5000))

(define prop-list
  (list (title "old-title")
        (css-path "this.css")))

(define new-list
  (list (title "new-title")))

(print-properties-list (merge-properties new-list prop-list))

