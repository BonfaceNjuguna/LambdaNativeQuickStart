(define dbfile (string-append (system-directory) "/expense.sqlite"))
(define db #f)

;; --- UI Definition ---
(define demouiform:expense-tracker
  (list->table
    `(
      (main
        "Expense Tracker"
        #f  ;; No menu
        #f  ;; No icon
        (hbox
          (label text "Welcome" align left fnt 'bigfnt)
          (spacer)
          (button text "Logout" action ,(lambda () (terminate))))
        (spacer height 10)
        (label text "Total Balance" align left fnt 'fnt)
        (hbox
          (label id totalbalance text ,(lambda ()
                                         (let* ((income (or (caar (sqlite-query db "SELECT SUM(amount) FROM transactions WHERE type='Income'")) 0))
                                                (expense (or (caar (sqlite-query db "SELECT SUM(amount) FROM transactions WHERE type='Expense'")) 0))
                                                (total (- income expense)))
                                           (number->string total)))
                 align left fnt 'hdfnt)
          (label text " KES" align left fnt 'smlfnt))
        (spacer height 20)
        (hbox
          (vbox
            (hbox
              (label text "Income" align left fnt 'fnt)
              (label text "↑" color Green align left fnt 'fnt))
            (label id incomeamount text ,(lambda ()
                                           (number->string (or (caar (sqlite-query db "SELECT SUM(amount) FROM transactions WHERE type='Income'")) 0)))
                   align left fnt 'hdfnt))
          (spacer width 40)
          (vbox
            (hbox
              (label text "Expenses" align left fnt 'fnt)
              (label text "↓" color Red align left fnt 'fnt))
            (label id expenseamount text ,(lambda ()
                                            (number->string (or (caar (sqlite-query db "SELECT SUM(amount) FROM transactions WHERE type='Expense'")) 0)))
                   align left fnt 'hdfnt)))
        (spacer height 20)
        (label text "Transaction History" align left fnt 'fnt)
        (listbox id transactionhistory entries ,(lambda ()
                                                  (let ((rows (sqlite-query db "SELECT type, amount, description FROM transactions ORDER BY rowid DESC")))
                                                    (map (lambda (row)
                                                           (string-append (car row) ": "
                                                                          (number->string (cadr row)) " KES - "
                                                                          (caddr row)))
                                                         rows))))
        (spacer height 20)
        (button text "Add" fnt 'bigfnt action ,(lambda () 'addtransaction))
      )

      (addtransaction
        "Add Transaction"
        ("Back" main)
        #f
        (spacer height 10)
        (dropdown id type text "Type:" entries ("Income" "Expense") #t location ui default 0)
        (textentry id amount text "Amount:" keypad numint #t location ui default "0")
        (textentry id description text "Description:" #t location ui default "")
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
                            ;; No reload needed, lambdas will query fresh on main
                            'main)
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

  )
  (lambda (t x y)
    (if (= t EVENT_KEYPRESS)
        (if (= x EVENT_KEYESCAPE) (terminate)))
    (glgui-event gui t x y))
  (lambda () (when db (sqlite-close db)) #t)
  (lambda () (glgui-suspend))
  (lambda () (glgui-resume)))