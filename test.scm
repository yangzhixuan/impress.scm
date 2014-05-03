(load "impress.scm")

(printf "~A~N" 
  (make-html
    (make-head
      (make-meta-charset "utf-8") 
      (make-title "this is a title")
      (make-css-link "mycss.css")
      (make-outside-script "this.js"))
    (make-body
      (make-div (list (cons "class" "slide")) "hello"))))


(define prop-list
  (list (title "old-title")
        (css-path "this.css")))

(define new-list
  (list (title "new-title")))

(print-properties-list (merge-properties new-list prop-list))
