#!/usr/bin/env gosh
;; -*- coding: utf-8 mode: scheme -*-

(add-load-path "lib")
(use orm)

(define-class <user> (<orm>) ())
(define-class <rate> (<orm>) ())
(define-class <record> (<orm>) ())
(define-class <item> (<orm>) ())

(*db-name* "dbi:sqlite3:corrente.db")

(define (main args)
  0)
