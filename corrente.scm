#!/usr/bin/env gosh
;; -*- coding: utf-8 mode: scheme -*-

(add-load-path "lib")
(use orm)

(*db-name* "dbi:sqlite3:corrente.db")

(define-orm-class <user>)
(define-orm-class <rate>)
(define-orm-class <record>)
(define-orm-class <item>)


(define (main args)
  0)
