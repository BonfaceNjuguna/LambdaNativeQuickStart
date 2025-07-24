;; Expense Tracker with SQLite persistence

;; --- SQLite DB setup ---
(define dbfile (string-append (system-directory) "/expense.sqlite"))
(define db #f)

;; Helper to reload transaction history and balances from DB
(define (reload-from-db)
  (let* ((rows (sqlite-query db "SELECT type, amount, description FROM transactions ORDER BY rowid DESC"))
         (income (sqlite-query db "SELECT SUM(amount) FROM transactions WHERE type='Income'"))
         (expense (sqlite-query db "SELECT SUM(amount) FROM transactions WHERE type='Expense'"))
         (incomeamt (if (and income (car income) (car (car income))) (car (car income)) 0))
         (expenseamt (if (and expense (car expense) (car (car expense))) (car (car expense)) 0)))
    (dbset! 'transactionhistory
      (map (lambda (row)
             (string-append (car row) ": " (number->string (cadr row)) " KES - " (caddr row)))
           rows))
    (dbset! 'incomeamount incomeamt)
    (dbset! 'expenseamount expenseamt)
    (dbset! 'totalbalance (- incomeamt expenseamt))))

(define demouiform:expense-tracker (list->table `(
  (main
    "Expense Tracker"
    ;; Top row: Welcome text (left), Logout button (right)
    (hbox
      (label text "Welcome" align left size header)
      (spacer)
      (button text "Logout" action ,(lambda () (terminate)))
    )
    (spacer height 10)
    ;; Total balance and currency
    (label text "Total Balance" align left size medium)
    (label id totalbalance text "0" align left size large)
    (label text "KES" align left size small)
    (spacer height 20)
    ;; Income and Expenses row
    (hbox
      (vbox
        (hbox
          (label text "Income" align left size medium)
          (label text "↑" color Green align left size medium)
        )
        (label id incomeamount text "0" align left size large)
      )
      (spacer width 40)
      (vbox
        (hbox
          (label text "Expenses" align left size medium)
          (label text "↓" color Red align left size medium)
        )
        (label id expenseamount text "0" align left size large)
      )
    )
    (spacer height 20)
    ;; Transaction history
    (label text "Transaction History" align left size medium)
    (listbox id transactionhistory entries ())
    (spacer height 20)
    ;; Add button
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
    (button text "Save" action ,(lambda ()
      (let* ((t (uiget 'type "Income"))
             (amt (string->number (uiget 'amount "0")))
             (desc (uiget 'description "")))
        (if (and amt (> amt 0))
          (begin
            ;; Insert transaction into SQLite DB
            (sqlite-query db (string-append
              "INSERT INTO transactions (type, amount, description) VALUES ('"
              t "'," (number->string amt) ",'" desc "')"))
            ;; Reload history and balances from DB
            (reload-from-db)
            (dbset! 'amount "")
            (dbset! 'description "")
            'main)
          (begin
            (popup "Please enter a valid amount." '("OK" #f))
            #f)))))
)))

(define gui #f)
(define form #f)

(main
  ;; initialization
  (lambda (w h)
    (make-window 480 800)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))

    (let ((aw (glgui-width-get))
          (ah (glgui-height-get)))
      (glgui-box gui 0 0 aw ah DarkGreen)
      (set! form (glgui-uiform gui 0 0 aw ah)))

    ;; Open SQLite DB and create table if not exists
    (set! db (sqlite-open dbfile))
    (sqlite-query db "CREATE TABLE IF NOT EXISTS transactions (type TEXT, amount INTEGER, description TEXT)")

    ;; Set the sandbox up to be the current directory and use the above example as the script
    (glgui-widget-set! gui form 'sandbox (system-directory))
    (glgui-widget-set! gui form 'uiform demouiform:expense-tracker)

    ;; Set the fonts
    (glgui-widget-set! gui form 'fnt ascii_18.fnt)
    (glgui-widget-set! gui form 'smlfnt ascii_14.fnt)
    (glgui-widget-set! gui form 'hdfnt ascii_24.fnt)
    (glgui-widget-set! gui form 'bigfnt ascii_40.fnt)

    ;; Create the table to store data (default location for widget values)
    (glgui-widget-set! gui form 'database (make-table))

    ;; Load transactions and balances from DB
    (reload-from-db)
  )
  ;; events
  (lambda (t x y)
    (if (= t EVENT_KEYPRESS) (begin
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-event gui t x y))
  ;; termination
  (lambda ()
    (when db (sqlite-close db))
    #t)
  ;; suspend
  (lambda () (glgui-suspend))
  ;; resume
  (lambda () (glgui-resume))
)