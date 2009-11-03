;;
;; O/R mapper
;;
;; original code written by Yuumi Yoshida
;; http://www.atmarkit.co.jp/fcoding/index/gauche.html
;; Thanks a lot!
;;

(define-module orm
  (use dbi)
  (use gauche.collection)
  (use gauche.parameter)
  (use util.match)
  (export-all))
(select-module orm)

(define (orm-init db-name)
  (*db-name* db-name)
  (db-connect)
  (map (cut setup-slots <>) (class-direct-subclasses <orm>)))


(define *db-name* (make-parameter #f))
(define *db* (make-parameter #f))

(define-class <orm-meta> (<class>) ())

(define-class <orm> ()
  ((initialized  :allocation :each-subclass)
   (table-name :allocation :each-subclass)
   (column-names :allocation :each-subclass))
  :metaclass <orm-meta>)

(define-method setup-slots ((class <orm-meta>))
  (if (class-slot-bound? class 'initialized)
    class
    (let* ((_ (or (*db*) (db-connect)))
           (table-name (class-name->table-name class))
           (column-names (db-column-name-list table-name))
           (new-class #f))
      (redefine-slots class column-names)
      (set! new-class (slot-ref class 'redefined))
      (set! (class-slot-ref new-class 'table-name) table-name) 
      (set! (class-slot-ref new-class 'column-names) column-names)
      (set! (class-slot-ref new-class 'initialized)  #t)
      new-class)))

(define-method class-name->table-name ((class <orm-meta>))
  (string-append (regexp-replace #/<(.+)>/ (symbol->string (class-name class)) "\\1") "s"))

(define-method redefine-slots ((class <orm-meta>) slots-names)
  (let1 slots (map (lambda(n)
		     `(,(string->symbol n) :init-keyword ,(make-keyword n)
		       :accessor ,(string->symbol #`",|n|-of")))
		   slots-names)
    (eval `(define-class ,(class-name class) 
	     ( ,(class-name (car (class-direct-supers class))) )
             ,slots)
	  (interaction-environment))))

(define-method make-with-db-values ((class <orm-meta>) row column-getter)
  (if (not row)
    #f
    (let1 init-values (fold (lambda(c r)
                              (list* (make-keyword c) (column-getter row c) r))
                            ()
                            (class-slot-ref class 'column-names))
      (apply make (cons class init-values)))))

(define-method all ((class <orm-meta>))
  (let* ((class (setup-slots class))
         (result (db-select (class-slot-ref class 'table-name)))
         (column-getter (relation-accessor result)))
    (map (lambda(row) (make-with-db-values class row column-getter))
	 result)))

(define-method find ((class <orm-meta>) (id <integer>))
  (let* ((class (setup-slots class))
         (result (db-select-by-key (class-slot-ref class 'table-name)
				   "id" id))
	 (column-getter (relation-accessor result)))
    (make-with-db-values class (first-element result) column-getter)))

(define-method find ((class <orm-meta>) . restargs)
  (let* ((conditions (get-keyword :conditions restargs #f))
         (class (setup-slots class))
         (result (db-select-by-condition (class-slot-ref class 'table-name) conditions))
         (column-getter (relation-accessor result)))
    (make-with-db-values class (first-element result) column-getter)))


(define-method first-element ((coll <collection>))
  (find (lambda(el) #t) coll))

(define-method save ((object <orm>)) 
  (if (slot-bound? object 'id)
    (db-update (slot-ref object 'table-name)
               (column-slot-hash object)
               "id"
               (slot-ref object 'id))
    (db-insert (slot-ref object 'table-name)
               (column-slot-hash object)))
  #t)

(define-method column-slot-hash ((object <orm>))
  (let1 hash (make-hash-table 'string=?)
    (for-each
     (lambda(col)
       (if (slot-bound? object (string->symbol col))
         (set! (ref hash col)
               (slot-ref object (string->symbol col)))))
     (slot-ref object 'column-names))
    hash))

(define (db-connect)
  (begin0
   (*db* (dbi-connect (*db-name*)))))

(define (db-column-name-list table-name)
  (let1 result (dbi-do (*db*) #`"SELECT * FROM ,table-name LIMIT 1")
    (vector->list (relation-column-names result))))

(define (db-select table-name)
  (dbi-do (*db*) #`"SELECT * FROM ,table-name"))

(define (db-select-by-key table-name key-column key-value)
  (let1 sql #`"SELECT * FROM ,table-name WHERE ,key-column = ?"
    (dbi-do (*db*) sql '() key-value)))

(define (db-select-by-condition table-name conditions)
  (match-let1 (condition restargs ...) conditions
    (let1 sql #`"SELECT * FROM ,table-name WHERE ,condition"
      (apply dbi-do (*db*) sql '() restargs))))

(define (db-update table-name slot-hash key-column key-value)
  (let* ((assign (string-join (map (lambda(c) #`",c = ?") (hash-table-keys slot-hash)) ","))
	 (sql #`"UPDATE ,table-name SET ,assign WHERE ,key-column = ?"))
    (apply dbi-execute (*db*) sql '() (append (hash-table-values slot-hash) (list key-value)))))

(define (db-insert table-name slot-hash)
  (let* ((places (map (lambda(c) "?") (hash-table-keys slot-hash)))
	 (sql #`"INSERT INTO ,table-name (,(string-join (hash-table-keys slot-hash) \",\")) VALUES (,(string-join places \",\"))"))
    (apply dbi-do (*db*) sql '() (hash-table-values slot-hash))))

(provide "orm")
