;; SPDX identifier: GPL-3.0-or-later

;; DB file
(define dbfile "expense.sqlite")

;; GUI handles
(define gui #f)
(define form #f)

;; Function: reload data from DB into UI widgets
(define (reload-from-db)
  (let* ((db (sqlite-open dbfile))
         (rows (sqlite-query db
                "SELECT type, amount, description FROM transactions ORDER BY rowid DESC"))
         (income (sqlite-query db
                  "SELECT SUM(amount) FROM transactions WHERE type='Income'"))
         (expense (sqlite-query db
                   "SELECT SUM(amount) FROM transactions WHERE type='Expense'"))
         (incomeamt (if (and income (car income) (car (car income)))
                        (car (car income)) 0))
         (expenseamt (if (and expense (car expense) (car (car expense)))
                         (car (car expense)) 0))
         (total (- incomeamt expenseamt)))
    (sqlite-close db)
    (glgui-widget-set! gui form 'transactionhistory
      (map (lambda (row)
             (string-append (car row) ": "
                            (number->string (cadr row)) " KES - "
                            (caddr row)))
           rows))
    (glgui-widget-set! gui form 'incomeamount (number->string incomeamt))
    (glgui-widget-set! gui form 'expenseamount (number->string expenseamt))
    (glgui-widget-set! gui form 'totalbalance (number->string total))))

;; UI definition using list->table
(define expense-tracker
  (list->table
   `(
     (main
       "Expense Tracker"
       #f
       #f
       (label text "Total Balance")
       (label id totalbalance text "0")
       (label text "KES")
       (spacer height 20)
       (label text "Income")
       (label id incomeamount text "0")
       (spacer height 10)
       (label text "Expenses")
       (label id expenseamount text "0")
       (spacer height 20)
       (label text "Transaction History")
       (listbox id transactionhistory entries ())
       (spacer height 20)
       (button text "Add" action ,(lambda () 'addtransaction))
     )

     (addtransaction
       "Add Transaction"
       ("Back" main)
       #f
       (dropdown id type label "Type:" entries ("Income" "Expense"))
       (textentry id amount text "Amount:" keypad numint)
       (textentry id description text "Description:")
       (button text "Save"
         action ,(lambda ()
                   (let* ((t (uiget 'type "Income"))
                          (amt (string->number (uiget 'amount "0")))
                          (desc (uiget 'description "")))
                     (if (and amt (> amt 0))
                         (begin
                           (let ((db (sqlite-open dbfile)))
                             (sqlite-query db
                               (string-append
                                "INSERT INTO transactions (type, amount, description) VALUES ('"
                                t "'," (number->string amt) ",'" desc "')"))
                             (sqlite-close db))
                           (reload-from-db)
                           (glgui-widget-set! gui form 'amount "")
                           (glgui-widget-set! gui form 'description "")
                           'main)
                         #f))))
     )
   )))

;; Main entry point
(main
  ;; Initialization
  (lambda (w h)
    (make-window 480 800)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))

    (let ((aw (glgui-width-get))
          (ah (glgui-height-get)))
      (glgui-box gui 0 0 aw ah Blue)
      (set! form (glgui-uiform gui 0 0 aw ah)))

    ;; Ensure the DB and table exist
    (let ((db (sqlite-open dbfile)))
      (sqlite-query db
        "CREATE TABLE IF NOT EXISTS transactions (type TEXT, amount INTEGER, description TEXT)")
      (sqlite-close db))

    (glgui-widget-set! gui form 'sandbox (system-directory))
    (glgui-widget-set! gui form 'uiform expense-tracker)

    ;; Use a default font
    (glgui-widget-set! gui form 'fnt ascii_14.fnt)

    ;; Load initial data
    (reload-from-db))

  ;; Event handler
  (lambda (t x y)
    (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYESCAPE))
        (terminate))
    (glgui-event gui t x y))

  ;; Termination handler
  (lambda ()
    #t)

  ;; Suspend
  (lambda () (glgui-suspend))

  ;; Resume
  (lambda () (glgui-resume)))
