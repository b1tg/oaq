(import (chicken base)
        (chicken pretty-print)
        (chicken format)
        (chicken irregex)
        (chicken io)
        (clojurian syntax)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        (srfi 69)
        (comparse)
        (sxml-transforms)
        (char-set-literals)
        (html-parser))

(define new-line
  (any-of (preceded-by (is #\newline)
                       (result "\n"))
          (preceded-by (is #\return)
                       (maybe (is #\newline))
                       (result "\n"))))
(define space-chars (char-set #\space #\tab))
(define space-char (preceded-by (in space-chars) (result " ")))
(define space* (as-string (zero-or-more space-char)))
(define atx-inline
  (none-of* new-line (sequence space* (zero-or-more (is #\#)) space* new-line)))
(define space+ (as-string (one-or-more space-char)))
(define codeblock
  (sequence* (
              (_ (any-of (char-seq "\n\n\t") (char-seq "\n\n    ")))
              (pp (as-string (one-or-more (none-of* (char-seq "\n\n")
                                                    item))))
              )
             (result `(CODE ,(string-append "    " pp))))
  )
(define para
  (sequence* ((_ (zero-or-more new-line))
              (pp (as-string (one-or-more (none-of* 
                                           new-line
                                           item))))
              )
             (result `(PARA ,pp)))
  )
(define qa
  (sequence*
   ((_ (zero-or-more new-line))
    (qmark (char-seq "Q: "))
    (question
     (as-string (one-or-more (any-of (none-of* 
                                      new-line
                                      item)))))
    (_ (char-seq "\n"))
    )
   (result `(QA ,question)))
  )

(define (qa->str raw)
  (sxml->html `((p "QA=> " (b ,(car raw))) (p ,(cdr raw))))
  )
(define (qa->str1 raw_list) 
  (sxml->html
   `(
     html
     (@ (lang "zh_CN"))
     (head
      (meta (@ (charset "UTF-8")))
      (link (@ (href "./style.css") (rel "stylesheet") (type "text/css")))
      ;;<link href="https://cdn.bootcdn.net/ajax/libs/github-markdown-css/4.0.0/github-markdown.css" rel="stylesheet">
      (link (@ (href "https://cdn.bootcdn.net/ajax/libs/github-markdown-css/4.0.0/github-markdown.css") (rel "stylesheet") (type "text/css")))
      (title "nice things")
      )
     (body
      (article
       (@ (class "markdown-body"))
       ,@(map (lambda(tmp)
                (cond 
                  ((equal? (car tmp) 'QA) `(h1 ,(cdr tmp)))
                  ((equal? (car tmp) 'PARA) `(p ,(cdr tmp)))
                  ((equal? (car tmp) 'CODE) `(pre (code ,(cdr tmp))))
                  )
                ) raw_list)
       )
      )
     )
   )
  )
(set! case3
      (call-with-input-file "oaq_basic.txt"
        (lambda (port)
          (read-string #f port))))
(set! raw_list (parse (zero-or-more (any-of qa codeblock para)) case3))
;(print "========OUTPUT=======")
;(print (qa->str1 raw_list))
;(print "=====================")
(with-output-to-file "oaq_basic.html"
  (lambda ()
    (print (qa->str1 raw_list))))

(print "Successfully written to oaq_basic.html")
