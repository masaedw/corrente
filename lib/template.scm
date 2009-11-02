;;
;; template engine
;;
;; original code written by Yuumi Yoshida
;; http://www.atmarkit.co.jp/fcoding/index/gauche.html
;; Thanks a lot!
;;
(define-module template
  (use file.util)
  (export render rendering-text rendering-file))
(select-module template)

(define (template->s-exp templ)
  (define (quote-display m)
    (format "(display ~s *p*)" (m 1)))
  (if (not (#/<%/ templ))
    templ
    (let* ((s (regexp-replace-all* templ
                                   #/<%=(.*?)%>/ "<% (display \\1 *p*) %>"
                                   #/%>(.*?)<%/ quote-display))
           (str-s-exp (regexp-replace* s
                                       #/^(.*?)<%/ quote-display
                                       #/%>(.*?)$/ quote-display)))
      (read-from-string (string-append "(call-with-output-string (lambda(*p*)"
                                       str-s-exp
                                       "))")))))

(define (rendering-text templ vars args)
  (apply (eval `(lambda ,vars ,(template->s-exp templ))
               (interaction-environment)) args))

(define (rendering-file templ-file vars args)
  (rendering-text (file->string templ-file) vars args))

(define-macro (render tmpl-file . args)
  `(rendering-file ,tmpl-file (quote ,args) (list ,@args)))

(provide "template")
