(define dbfile "expense.sqlite")
(define db #f)

;; --- Helper to reload transaction history and balances from DB ---
(define (reload-from-db)
  (let* ((rows (sqlite-query db "SELECT type, amount, description FROM transactions ORDER BY rowid DESC"))
         (income (sqlite-query db "SELECT SUM(amount) FROM transactions WHERE type='Income'"))
         (expense (sqlite-query db "SELECT SUM(amount) FROM transactions WHERE type='Expense'"))
         (incomeamt (if (and income (car income) (car (car income))) (car (car income)) 0))
         (expenseamt (if (and expense (car expense) (car (car expense))) (car (car expense)) 0))
         (total (- incomeamt expenseamt)))

    ;; Manually update widget values instead of dbset!
    (glgui-widget-set! gui form 'transactionhistory
      (map (lambda (row)
             (string-append (car row) ": "
                            (number->string (cadr row)) " KES - "
                            (caddr row)))
           rows))
    (glgui-widget-set! gui form 'incomeamount (number->string incomeamt))
    (glgui-widget-set! gui form 'expenseamount (number->string expenseamt))
    (glgui-widget-set! gui form 'totalbalance (number->string total))))

;; --- UI Definition ---
(define demouiform:expense-tracker
  (list->table
    `(
      (main
        "Expense Tracker"
        (hbox
          (label text "Welcome" align left size header)
          (spacer)
          (button text "Logout" action ,(lambda () (terminate))))
        (spacer height 10)
        (label text "Total Balance" align left size medium)
        (label id totalbalance text "0" align left size large)
        (label text "KES" align left size small)
        (spacer height 20)
        (hbox
          (vbox
            (hbox
              (label text "Income" align left size medium)
              (label text "↑" color Green align left size medium))
            (label id incomeamount text "0" align left size large))
          (spacer width 40)
          (vbox
            (hbox
              (label text "Expenses" align left size medium)
              (label text "↓" color Red align left size medium))
            (label id expenseamount text "0" align left size large)))
        (spacer height 20)
        (label text "Transaction History" align left size medium)
        (listbox id transactionhistory entries ())
        (spacer height 20)
        (button text "Add" size header action ,(lambda () 'addtransaction))
      )

      (addtransaction
        "Add Transaction"
        ("Back" main)
        #f
        (spacer height 10)
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
                            ;; Save to DB
                            (sqlite-query db (string-append
                              "INSERT INTO transactions (type, amount, description) VALUES ('"
                              t "'," (number->string amt) ",'" desc "')"))
                            (reload-from-db)

                            ;; Clear inputs
                            (glgui-widget-set! gui form 'amount "")
                            (glgui-widget-set! gui form 'description "")

                            'main)
                          ;; Invalid input → just ignore or log
                          #f))))))))

;; --- GUI Setup ---
(define gui #f)
(define form #f)

(main
  (lambda (w h)
    (make-window 480 800)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))

    (let ((aw (glgui-width-get))
          (ah (glgui-height-get)))
      (glgui-box gui 0 0 aw ah DarkGreen)
      (set! form (glgui-uiform gui 0 0 aw ah)))

    (set! db (sqlite-open dbfile))
    (sqlite-query db
      "CREATE TABLE IF NOT EXISTS transactions (type TEXT, amount INTEGER, description TEXT)")

    (glgui-widget-set! gui form 'sandbox (system-directory))
    (glgui-widget-set! gui form 'uiform demouiform:expense-tracker)

    ;; Fonts
    (glgui-widget-set! gui form 'fnt ascii_18.fnt)
    (glgui-widget-set! gui form 'smlfnt ascii_14.fnt)
    (glgui-widget-set! gui form 'hdfnt ascii_24.fnt)
    (glgui-widget-set! gui form 'bigfnt ascii_40.fnt)

    ;; Table for widget values
    (glgui-widget-set! gui form 'database (make-table))

    (reload-from-db)
  )
  (lambda (t x y)
    (if (= t EVENT_KEYPRESS)
        (if (= x EVENT_KEYESCAPE) (terminate)))
    (glgui-event gui t x y))
  (lambda () (when db (sqlite-close db)) #t)
  (lambda () (glgui-suspend))
  (lambda () (glgui-resume)))
