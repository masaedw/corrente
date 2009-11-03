#!/usr/bin/env gosh
;; -*- coding: utf-8 mode: scheme -*-

(use gauche.logger)

(use gauche.parameter)
(use rfc.http)
(use rfc.base64)
(use sxml.ssax)
(use sxml.sxpath)
(use file.util)
(use util.list)

(add-load-path "lib")
(use orm)

;;
;; Configuration
;;

(define (read-configuration file)
  (guard (e [(or (<system-error> e)
                 (<read-error> e))
             (exit 1 "Reading config file failed: ~a" (ref e'message))])
    (let1 ss (file->sexp-list file)
      (unless (and (= (length ss) 1) (list? (car ss)))
        (exit 1 "Malformed configuration file: ~a" file))
      (dolist [k '(twitter-username twitter-password db-name)]
        (unless (string? (assoc-ref (car ss) k))
          (exit 1 "Entry for ~a is missing or non-string in ~a" k file)))
      ;; Returns a closure to look up
      (lambda (k) (assoc-ref (car ss) k)))))

;;
;; Twitter access layer
;;

(define (twitter-auth-token config)
  (let1 u:p #`",(config 'twitter-username):,(config 'twitter-password)"
    #`"Basic ,(base64-encode-string u:p)"))

(define (twitter-post config content)
  (log-format "twitter-post: ~a" content)
  (let1 r (make-request 'post "twitter.com" "/statuses/update.xml"
                        (http-compose-query #f `((status ,content)) 'utf-8)
                        :content-type "application/x-www-form-urlencoded"
                        :authorization (twitter-auth-token config))
    ((if-car-sxpath '(// status id *text*)) r)))

(define (twitter-mentions config since-id)
  (let1 r (make-request 'get "twitter.com"
                        (if since-id
                          `("/statuses/mentions.xml" (since_id ,since-id))
                          "/statuses/mentions.xml")
                        #f :authorization (twitter-auth-token config))
    (sort-by (map (lambda (s)
                    (map (cut <> s) `(,(if-car-sxpath '(created_at *text*))
                                      ,(if-car-sxpath '(id *text*))
                                      ,(if-car-sxpath '(text *text*))
                                      ,(if-car-sxpath '(user screen_name *text*))
                                      ,(if-car-sxpath '(user id *text*)))))
                  ((sxpath '(// status)) r))
             (.$ x->integer car)
             >)))

(define (twitter-followers config)
  (let1 r (make-request 'get "twitter.com"
                        `("/followers/ids.xml"
                          (screen_name ,(config'twitter-username)))
                        #f :authorization (twitter-auth-token config))
    ((sxpath '(// id *text*)) r)))

(define (max-status-id mentions)
  (if (null? mentions) #f (car (car mentions))))

;;
;; Models
;;

(define-class <user> (<orm>) ())
(define-class <rate> (<orm>) ())
(define-class <record> (<orm>) ())
(define-class <item> (<orm>) ())


(define (kick-reply-watcher! client config)
  (define (body)
    (guard (e [else (log-format "watcher error: ~a" (ref e'message))])
      (let loop ((since-id (max-status-id (twitter-mentions config #f))))
        (sys-sleep 60)
        (log-format "watcher polling")
        (loop (forward-from-twitter config client since-id))))
    (sys-sleep 60)
    (body))
  (thread-start! (make-thread body)))



(define conf (read-configuration "config.scm"))
(orm-init (conf 'db-name))

(define (main args)
  (let ((config (read-configuration "config.scm")))
    (parameterize ((*db-name* (config 'db-name)))
      0)))

;;
;; Utility
;;

(define (make-request method server request-uri :optional body :rest opts)
  (receive (status header body)
      (case method
        [(get)  (apply http-get server request-uri opts)]
        [(post) (apply http-post server request-uri body opts)])
    (unless (equal? status "200")
      (log-format "~a returned status ~a: ~a" server status body)
      (error "make-request error"))
    (call-with-input-string body (cut ssax:xml->sxml <> '()))))
