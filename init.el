;; Packages
(require 'package)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/myModes")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(require 'moe-theme)
;; (setq moe-light-pure-white-background-in-terminal t)
(moe-dark)



;; Helm mode
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-mini)


;; Ne pas activer fuzzy matching parce que trop de candidats 
;; (setq helm-buffers-fuzzy-matching t
      ;; helm-recentf-fuzzy-match    t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

(defun pcomplete/sudo ()
  (let ((prec (pcomplete-arg 'last -1)))
    (cond ((string= "sudo" prec)
           (while (pcomplete-here*
                   (funcall pcomplete-command-completion-function)
                   (pcomplete-arg 'last) t))))))

(helm-mode 1)
(helm-autoresize-mode t)

;; Utiliser projectile et helm-projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile-find-file)
(add-to-list 'projectile-globally-ignored-directories "zz-old")
;; (helm-autoresize-mode 1)


;; Speedbar by default
;; (speedbar 1)

;;;;;;;;;;;;;;;;; MU4E
; Need to add to load path explicitly for git version
(global-set-key (kbd "C-c m") 'mu4e)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)
;; Make it default mail client for C-x m
(setq mail-user-agent 'mu4e-user-agent)
;; top-level Maildir
(setq mu4e-maildir       "~/.mail" 
      mu4e-attachment-dir "~/Downloads"
      mu4e-sent-folder "/svictor/INBOX/.Sent"
      mu4e-refile-folder "/archives/svictor"
      mu4e-drafts-folder "/svictor/INBOX/.Drafts"
      mu4e-trash-folder "/svictor/INBOX/.Trash")

;; Contexts
       (setq mu4e-contexts
         `( ,(make-mu4e-context
     	  :name "svictor"
     	  :enter-func (lambda () (mu4e-message "Switch to the Svictor context"))
     	  ;; leave-func not defined
     	  :match-func (lambda (msg)
     			(when msg
     			  (mu4e-message-contact-field-matches msg
     			    :to "svictor@svictor.net")))
     	  :vars '(  ( user-mail-address . "svictor@svictor.net"  )
		    ( user-full-name . "Victor A. Stoichita" )
		    ( mu4e-sent-folder . "/svictor/INBOX/.Sent" )
		    ( mu4e-drafts-folder . "/svictor/INBOX/.Drafts" )
		    ( mu4e-trash-folder . "/svictor/INBOX/.Trash" )
		    ( mu4e-refile-folder . "/archives/svictor" )
		    ( mu4e-compose-signature .
		      (concat
		       "Victor A. Stoichita"))))
            ,(make-mu4e-context
     	  :name "prof"
     	  :enter-func (lambda () (mu4e-message "Switch to the Perso CNRS context"))
     	  :match-func (lambda (msg)
     			(when msg
     			  (mu4e-message-contact-field-matches msg
     			    :to "victor.stoichita@cnrs.fr")))
     	  :vars '(  ( user-mail-address	. "victor.stoichita@cnrs.fr" )
		    ( user-full-name . "Victor A. Stoichita" )
		    ( mu4e-sent-folder . "/cnrs/Sent Items" )
		    ( mu4e-drafts-folder . "/cnrs/Drafts" )
		    ( mu4e-trash-folder . "/cnrs/Trash" )
		    ( mu4e-refile-folder . "/cnrs/Archives" )
		    ( mu4e-compose-signature .
					     (concat
					      "Centre de Recherche en Ethnomusicologie - LESC - UMR 7186 - CNRS\n"
					      "M.A.E. - Université Paris Ouest - Nanterre La Défense\n"
					      "21 Allée de l'Université - 92023 Nanterre Cedex\n"
					      "http://crem-cnrs.fr/\n"
					      "Tel: 06 51 21 35 76 | http://svictor.net\n"))))

	    ;; Désactivé !!!
	    ;; ,(make-mu4e-context
	    ;;   :name "crem"
	    ;;   :enter-func (lambda () (mu4e-message "Switch to the CREM context"))
	    ;;   :match-func (lambda (msg)
	    ;; 		    (when msg
	    ;; 		      (mu4e-message-contact-field-matches msg
	    ;; 							  :to "crem.lesc@cnrs.fr")))
	    ;;   :vars '(  ( user-mail-address	. "crem.lesc@cnrs.fr" )
	    ;; 		( user-full-name . "Centre de recherche en ethnomusicologie (CREM-LESC, UMR7186, CNRS)" )
	    ;; 		( mu4e-sent-folder . "/crem/Sent Items" )
	    ;; 		( mu4e-drafts-folder . "/crem/Drafts" )
	    ;; 		( mu4e-trash-folder . "/crem/Deleted Items" )
	    ;; 		( mu4e-refile-folder . "/crem/Archive" )
	    ;; 		( mu4e-compose-signature .
	    ;; 	          (concat
	    ;; 		   "Victor A. Stoichita\n\n"
	    ;; 		   "Centre de Recherche en Ethnomusicologie - LESC - UMR 7186 - CNRS\n"
	    ;; 		   "M.A.E. - Université Paris Ouest - Nanterre La Défense\n"
	    ;; 		   "21 Allée de l'Université - 92023 Nanterre Cedex\n"
	    ;; 		   "Tel: 01 46 69 26 68 - Fax: 01 46 69 25 91\n"
	    ;; 		   "http://crem-cnrs.fr | http://archives.crem-cnrs.fr"))))

	    ,(make-mu4e-context
	      :name "listes"
	      :enter-func (lambda () (mu4e-message "Switch to Lists context"))
	      ;; leave-func not defined
	      :match-func (lambda (msg)
			    (when msg
			      (mu4e-message-contact-field-matches msg
								  :to "victor@svictor.net")))
	      :vars '(  ( user-mail-address . "victor@svictor.net"  )
			( user-full-name . "Victor A. Stoichita" )
			( mu4e-sent-folder . "/victor/INBOX/.Sent" )
			( mu4e-drafts-folder . "/victor/INBOX/.Drafts" )
			( mu4e-trash-folder . "/victor/INBOX/.Trash" )
			( mu4e-refile-folder . "/victor/INBOX/.Archive" )
			( mu4e-compose-signature .
		      (concat
		       "Victor A. Stoichita"))))

	    ,(make-mu4e-context
	      :name "rubbish"
	      :enter-func (lambda () (mu4e-message "Switch to vs context"))
	      ;; leave-func not defined
	      :match-func (lambda (msg)
			    (when msg
			      (mu4e-message-contact-field-matches msg
								  :to "vs@svictor.net")))
	      :vars '(  ( user-mail-address . "vs@svictor.net"  )
			( user-full-name . "Victor" )
			( mu4e-sent-folder . "/vs/INBOX/.Sent" )
			( mu4e-drafts-folder . "/vs/INBOX/.Drafts" )
			( mu4e-trash-folder . "/vs/INBOX/.Trash" )
			( mu4e-refile-folder . "/vs/INBOX/.Archive" )
			( mu4e-compose-signature .
		      (concat
		       "Victor A. Stoichita"))))
	    ))

       ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
       ;; guess or ask the correct context, e.g.

       ;; start with the first (default) context;
       ;; default is to ask-if-none (ask when there's no context yet, and none match)
       ;; (setq mu4e-context-policy 'pick-first)

       ;; compose with the current context is no context matches;
       ;; default is to ask
       ;; '(setq mu4e-compose-context-policy nil)
    
;; No auto signature, use C-c C-w to include one
(setq mu4e-compose-signature-auto-include nil)

(setq mu4e-user-mail-address-list
      '("crem.lesc@cnrs.fr"
	"victor.stoichita@cnrs.fr"
	"svictor@svictor.net"
	"vs@svictor.net"
	"victor@svictor.net"))

;; Limit address autocomplete
(setq mu4e-compose-complete-ignore-address-regexp "no-?reply")
(setq mu4e-compose-complete-only-after "2012-01-01")
; (setq mu4e-compose-complete-only-personal t)


(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;;  message-mode derived modes, such as mu4e-compose-mode.
;; mark the file(s) in dired you would like to attach and press
;; C-c RET C-a, and you’ll be asked whether to attach them to an existing
;; message, or create a new one.
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
     	(set-buffer buffer)
     	(when (and (derived-mode-p 'message-mode)
		   (null message-sent-message-via))
     	  (push (buffer-name buffer) buffers))))
    (nreverse buffers)))
(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; "Jour littéral, jour mois année, nom+email a écrit :"
(setq message-citation-line-format "Le %d %b %Y, %f a écrit :\n")
(setq mu4e-view-show-addresses t)

;; Show fancy chars instead of letters
(setq mu4e-use-fancy-chars t)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Compose messages in new frame
(setq mu4e-compose-in-new-frame t)

;; add format=flowed for outgoing mail.
;; Use only M-q or fill-paragraph in paragraphs
;; (add-hook 'mu4e-compose-mode-hook
;;           (defun cpb-compose-setup ()
;;             "Outgoing mails get format=flowed."
;;             (use-hard-newlines t 'guess)))

;; (add-hook 'mu4e-compose-mode-hook
;;   (defun my-add-flowed ()
;;     "Add format=flowed."
;;     (use-hard-newlines t 'guess))
    ;; (message-replace-header "Content-Type" "text/plain; format=flowed" )))
;; (defun my-add-bcc ()
;; (message-add-header "Bcc: vs@svictor.net\n")))

;; See https://www.emacswiki.org/emacs/GnusFormatFlowed
;; harden-newlines helper is needed for yanked bits of text which would otherwise lack the hard newline breaks (according to Yuri d’Elia on mu4e discussion list 15/02/16 

;; (defun harden-newlines ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (search-forward "\n" nil t)
;;       (put-text-property (1- (point)) (point) 'hard t))))

;; (add-hook 'message-setup-hook
;;   (lambda ()
;;     (setq truncate-lines nil ; optional
;; 	  word-wrap t        ; optional
;; 	  use-hard-newlines t)))

;; (add-hook 'message-send-hook
;;   (lambda ()
;;     (when use-hard-newlines
;;       (harden-newlines))))

;; the maildirs you use frequently; access them with 'j' ('jump')
(setq   mu4e-maildir-shortcuts
    '(("/svictor/INBOX"     . ?v)
      ("/victor/INBOX"       . ?i)
      ("/vs/INBOX"        . ?s)
      ("/cnrs/INBOX"        . ?c)
      ("/crem/INBOX"        . ?e)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-change-filenames-when-moving t ; to keep mbsync happy
      mu4e-index-update-error-continue t
      mu4e-index-update-error-warning t
      mu4e-hide-index-messages t)

;; Pour aller chercher le pain dans emacs. 
(setq mu4e-update-interval 600 ;update toutes les 10 minutes
       mu4e-get-mail-command "timeout 60s mbsync -a") ; Timeout to avoid getting stuck after suspend/resume

; Mu4e Bookmarks
(add-to-list 'mu4e-bookmarks '("( contact:svictor@svictor.net OR contact:victor@svictor.net OR contact:vs@svictor.net OR contact:victor.stoichita@cnrs.fr OR contact:victor.stoichita@mae.u-paris10.fr OR contact:crem.lesc@cnrs.fr)"       "For me"     ?v))
;; (add-to-list 'mu4e-bookmarks '("( m:/svictor/INBOX OR m:/cnrs/INBOX OR m:/vs/INBOX OR m:/crem/INBOX)"       "For me"     ?v))
(add-to-list 'mu4e-bookmarks '("g:d"       "Drafts"     ?d))
(add-to-list 'mu4e-bookmarks '("g:f"       "Flagged"     ?f))
;; (add-to-list 'mu4e-bookmarks '("g:unread AND NOT g:trashed AND NOT m:/svictor/INBOX/.Archive AND NOT m:/victor/INBOX/.Archive AND NOT m:/crem/Archive AND NOT m:/cnrs/Archives" "Unread (and not archived)"     ?u))
(add-to-list 'mu4e-bookmarks '("list:yoshimi.freelists.org" "Yoshimi" ?y))
(add-to-list 'mu4e-bookmarks '("list:ardour-users-ardour.org)" "Ardour" ?a))
(add-to-list 'mu4e-bookmarks '("list:linux-audio-user.lists.linuxaudio.org)" "LAU" ?l))
(add-to-list 'mu4e-bookmarks '("list:mu-discuss.googlegroups.com)" "LAU" ?m))

(add-to-list 'mu4e-bookmarks '("d:today..now AND NOT g:trashed AND NOT m:/svictor/INBOX/.Archive AND NOT m:/victor/INBOX/.Archive AND NOT m:/crem/Archive AND NOT m:/cnrs/Archives" "Today (and not archived)"     ?t))

; tell mu4e to use w3m for html rendering
;; (setq mu4e-html2text-command "w3m -T text/html")
;; (setq mu4e-html2text-command "html2text")

;; New in emacs 24 : shr, part of eww
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
;; Emulate some of the shr bindings
(add-hook 'mu4e-view-mode-hook
  (lambda()
    ;; try to emulate some of the eww key-bindings
    (local-set-key (kbd "<tab>") 'shr-next-link)
    (local-set-key (kbd "<backtab>") 'shr-previous-link)))
;; Make shr text more readable on dark themes
(setq shr-color-visible-luminance-min 80)

;; new action on key x works only for emacs with xwidget support
(defun my-mu4e-action-view-with-xwidget (msg)
  "View the body of the message inside xwidget-webkit."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (mu4e-error "No xwidget support available"))
  (let* ((html (mu4e-message-field msg :body-html))
	  (txt (mu4e-message-field msg :body-txt))
	  (tmpfile (format "%s%x.html" temporary-file-directory (random t))))
    (unless (or html txt)
      (mu4e-error "No body part for this message"))
    (with-temp-buffer
      ;; simplistic -- but note that it's only an example...
      (insert (or html (concat "<pre>" txt "</pre>")))
      (write-file tmpfile)
      (xwidget-webkit-browse-url (concat "file://" tmpfile) t))))

(add-to-list 'mu4e-view-actions
  '("xViewXWidget" . my-mu4e-action-view-with-xwidget) t)

;; mu4e-action-view-in-browser is built into mu4e
;; by adding it to these lists of custom actions
;; it can be invoked by first pressing a, then selecting
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Same with message tagging
(add-to-list 'mu4e-view-actions
  '("retag message" . mu4e-action-retag-message) t)

; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
(setq mu4e-headers-fields
   '( (:human-date . 12)
     (:flags . 6)
     (:mailing-list . 10)
     (:from-or-to . 22)
     (:thread-subject . nil)))

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
   (imagemagick-register-types))

;; From http://ionrock.org/emacs-email-and-mu.html
;; Choose account label to feed msmtp -a option based on From header
;; in Message buffer; This function must be added to
;; message-send-mail-hook for on-the-fly change of From address before
;; sending message since message-send-mail-hook is processed right
;; before sending message.
(defun choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "svictor@svictor.net" from) "svictor")
               ((string-match "victor@svictor.net" from) "victor")
               ((string-match "vs@svictor.net" from) "vs")
               ((string-match "victor.stoichita@cnrs.fr" from) "cnrs")
               ((string-match "crem.lesc@cnrs.fr" from) "crem"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'choose-msmtp-account)


(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "~/.mail/rc-bin/msmtpq"
;; (setq sendmail-program "msmtp"
      mail-specify-envelope-from t
;; needed for debians message.el cf. README.Debian.gz 
      message-sendmail-f-is-evil nil                
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

;; Support for transform message to task. From http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
;;store org-mode links to messages
;;Problem (void-function org-element-update-syntax).
(require 'org-mu4e)


;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

;; Org mode keys in message mode. Could work in other text modes
(add-hook 'message-mode-hook 'turn-on-orgstruct)
;; When you use orgstruct++-mode, Org will also export indentation and
;; autofill settings into that mode, and detect item context after the
;; first line of an item.
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;;;;;;;;;;;;;;; End Mu4E
;; Show entire parenthesis block (alternative: mixed: default: parenthesis)
(setq show-paren-style 'mixed)

;; Autofill
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'fill-nobreak-predicate 'fill-french-nobreak-p)
;; (add-hook 'fill-nobreak-predicate 'fill-single-word-nobreak-p)

;; Remove auto-fill on text modes (rely on wrapping modes)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(setq sentence-end-double-space nil)

;;;;;;;;;;;;;;; OrgMode

;; From http://pragmaticemacs.com/emacs/org-mode-basics-vi-a-simple-todo-list/
;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote other-frame))
(setq org-agenda-restore-windows-after-quit )				;


;; From http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
;; warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
;; (setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
;; (setq org-agenda-skip-scheduled-if-deadline-is-shown t) ;
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
;; (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
;; (setq org-agenda-todo-ignore-deadlines (quote all))
;; (setq org-agenda-todo-ignore-scheduled (quote all))
(setq org-agenda-start-on-weekday 1) 


(setq org-agenda-custom-commands
      '(("v" "Mon agenda" agenda ""
         ((org-agenda-ndays 7)          ;; agenda will start in week view
          (org-agenda-repeating-timestamp-show-all t)   ;; ensures that repeating events appear on all relevant dates
	  (org-agenda-files '("~/org/vic.org")) ;; Only my own agenda
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))) ;; limits agenda view to timestamped items
	("e" "Avec Estelle et Arthur" agenda ""
         ((org-agenda-ndays 7)          ;; agenda will start in week view
          (org-agenda-repeating-timestamp-show-all t)   ;; ensures that repeating events appear on all relevant dates
	  (org-agenda-files '("~/org/")) ;; All agenda files
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
	
	("v" "Agenda just vic" agenda ""
	 ((org-agenda-entry-types '(vic)) ))
	("n" "Agenda and all TODOs" ((agenda "" nil) (alltodo "" nil)) nil)
	("c" "Calendar view" vic/cfw:open-org-calendar "" nil)
	))
;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      '(("e" "event" entry (file "vic.org")
	 "* %^{Heading} %i %?\n %^t \n %a\n")
	("t" "todo" entry (file+headline "~/org/todo.org" "En général")
	 "* TODO [#B] %i %?\nSCHEDULED: %^t\n %a\n")
	("a" "todo audio" entry (file+headline "~/org/todo.org" "Audio")
	 "* TODO [#B] %i %a\n")
	;; For mu4e. From http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/ (see above)
	("m" "todo mails" entry (file+headline "~/org/todo.org" "Mails")
	 "* TODO [#A] %? :mails: \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
	("r" "todo terrain" entry (file+headline "~/org/todo.org" "Terrain")
	 "* TODO [#B] %i %?\nSCHEDULED: %^t\n %a\n")
	("l" "todo site lesc" entry (file+headline "~/org/sitelesc_todo.org" "New")
	 "* TODO [#B] %i %? :site: \nSCHEDULED: %^t\n %a\n")
	("c" "todo crem" entry (file+headline "~/org/todo.org" "Crem")
	 "* TODO [#B] %i %? :crem: \nSCHEDULED: %^t\n %a\n")))


;; You can insert and follow links that have Org syntax not only in Org,
;; but in any Emacs buffer.  For this, you should create two global
;; commands, like this (please select suitable global keys yourself):

(global-set-key "\C-c L" 'org-insert-link-global)
(global-set-key "\C-c o" 'org-open-at-point-global)

;; Pour écrire des mails en html depuis org
(require 'org-mime)
(setq org-mime-library 'mml)


;sort tasks in order of when they are due and then by priority
;(setq org-agenda-sorting-strategy
;  (quote
;   ((agenda time-up priority-down category-keep deadline-up)
;    (todo priority-down category-keep)
;    (tags priority-down category-keep)
;    (search category-keep))))

(setq
 org-agenda-files '("~/org")
 org-icalendar-include-todo (quote t)
 ;; org-icalendar-store-UID t
 org-icalendar-timezone "GMT+2 CEST"
 org-icalendar-use-scheduled (quote (event-if-not-todo todo-start))
 ;; org-icalendar-use-scheduled (quote (nil))
 org-log-done (quote time)
 org-replace-disputed-keys nil)

;; this hook saves an ics file once an org-buffer is saved
(defun my-icalendar-agenda-export()
  (when (string= (buffer-file-name) "/home/vic/org/vic.org")
    (org-icalendar-export-to-ics)
    (shell-command "cp /home/vic/org/vic.ics /var/lib/radicale/collections/vic/vic.ics")))

(add-hook 'after-save-hook 'my-icalendar-agenda-export)



;;;;;;;;;;;;;; End Org-mode

;; Calfw provides a new calendar view
(require 'calfw)
(require 'calfw-org)
(setq cfw:org-overwrite-default-keybinding t)
;; From https://github.com/kiwanami/emacs-calfw/issues/18#issuecomment-9134716
(defun vic/cfw:open-org-calendar (&rest args)
  (interactive)
  (let (
	;; do not duplicate deadlines
	(org-deadline-warning-days 0)
	)
    (cfw:open-org-calendar)
    )
  ;; set the org variables to remember
  (set (make-variable-buffer-local 'org-agenda-skip-function)
       org-agenda-skip-function)
  (set (make-variable-buffer-local 'org-deadline-warning-days) org-deadline-warning-days))


;; Never ask long confirmations
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;; Custom keybindings

;; Make windmove work in org-mode:
          (add-hook 'org-shiftup-final-hook 'windmove-up)
          (add-hook 'org-shiftleft-final-hook 'windmove-left)
          (add-hook 'org-shiftdown-final-hook 'windmove-down)
          (add-hook 'org-shiftright-final-hook 'windmove-right)


;;Assign recent files menu to C-x f
;; Superseded by helm
;; (global-set-key "\C-xf" 'recentf-open-files)

(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward) ; single key, useful
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
 )

;; Some shortcuts for window management
(global-set-key (kbd "<f7>") 'split-window-vertically)
(global-set-key (kbd "\C-c<f7>") 'split-window-horizontally)
(global-set-key (kbd "<f8>") 'delete-other-windows)
(global-set-key (kbd "\C-c<f8>") 'kill-buffer-and-window)



;; Ability to revert splite pane config
(winner-mode 1)
(global-set-key (kbd "<f5>") 'winner-undo)
(global-set-key (kbd "<f6>") 'winner-redo)

(global-set-key "\C-xf" 'recentf-open-files)
;; (defun close-and-kill-next-pane ()
;;   "If there are multiple windows, then close the other pane and kill the buffer in it also."
   ;; (interactive)
   ;; (other-window 1)
   ;; (kill-this-buffer)
   ;; (if (not (one-window-p))
       ;; (delete-window)))
 ;; (global-set-key (kbd "C-~") 'close-and-kill-next-pane)

;; (defun comment-or-uncomment-region-or-line ()
;;     "Comments or uncomments the region or the current line if there's no active region."
;;     (interactive)
;;     (let (beg end)
;;         (if (region-active-p)
;;             (setq beg (region-beginning) end (region-end))
;;             (setq beg (line-beginning-position) end (line-end-position)))
;;         (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-ç") 'comment-dwim-2)


(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-ê") 'helm-company)
     (define-key company-active-map (kbd "C-ê") 'helm-company)))




;; (require 'yasnippet)
;; (yas-reload-all)
;; (add-hook 'prog-mode-hook 'yas-minor-mode)
;; (add-hook 'web-mode-hook 'yas-minor-mode)
;; (add-hook 'text-mode-hook 'yas-minor-mode)
;; (add-hook 'message-mode-hook 'yas-minor-mode)


;;;;;;;;;;;;; Elpy mode for python
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)

;; Enable flycheck for elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(setq elpy-rpc-backend "jedi")

;; Autopep8 (mais pas pratique pour ipdb sur même ligne)
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)



;; -si for human readable size time-style for yyyy-mm-dd
(setq dired-listing-switches "-Al --si --time-style long-iso")
(setq delete-by-moving-to-trash t)

;; Put these auto-generated files in a folder ignored by git
(setq recentf-save-file "~/.emacs.d/.autogeneres")
(setq tramp-persistency-file-name "~/.emacs.d/.autogeneres")
(setq backup-directory-alist ((".*" . "~/.emacs.d/.autogeneres/backups"))
(setq ido-save-directory-list-file "~/.emacs.d/.autogeneres/ido-last")
(setq auto-save-list-file-prefix "~/.emacs.d/.autogeneres/auto-save-list/saves")
(setq projectile-cache-file "~/.emacs.d/.autogeneres/projectile.cache")
(setq projectile-known-projects-file "~/.emacs.d/.autogeneres/projectile-bookmarks.eld")
(setq eshell-history-file-name "~/.emacs.d/.autogeneres/eshell/history")
(setq eshell-last-dir-ring-file-name "~/.emacs.d/.autogeneres/eshell/lastdir")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LilyPond-pdf-command "evince")
 '(ange-ftp-try-passive-mode t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(backup-directory-alist (quote ((".*bufni.*" . "/tmp"))))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "c4e6fe8f5728a5d5fd0e92538f68c3b4e8b218bcfb5e07d8afff8731cc5f3df0" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(delete-selection-mode t)
 '(dired-dwim-target t)
 '(eww-search-prefix "https://www.google.com/search?q=")
 '(fci-rule-color "#424242")
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(nxml-slash-auto-complete-flag t)
 '(projectile-globally-ignored-file-suffixes (quote ("~" "#" ".bak")))
 '(shift-select-mode nil)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;; Web-mode 
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


(defun my-setup-php ()
  ;; enable web mode
  (web-mode)
  
  ;; ;; Flycheck redefined for web mode
  (require 'flycheck)
  
  (flycheck-define-checker web-mode-php
			   "This is the same as the default php checker except just for web-mode.
It continues checking for javascript errors if there are no more PHP errors."
			   :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
				     "-d" "log_errors=0" source)
			   :error-patterns
			   ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
				   (message) " in " (file-name) " on line " line line-end))
			   :modes (web-mode))
  
  (flycheck-define-checker my-php
			   "A PHP syntax checker using the PHP command line interpreter. See URL `http://php.net/manual/en/features.commandline.php'."
			   :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
				     "-d" "log_errors=0" source)
			   :error-patterns
			   ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
				   (message) " in " (file-name) " on line " line line-end))
			   :modes (php-mode php+-mode web-mode))
  
  ;; Enable flycheck
  (flycheck-select-checker my-php)
  (flycheck-mode t))
  
  ;; make these variables local
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)

  ;; set indentation, can set different indentation level for different code type
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)




;; Html5-el Adds html5 schemas to nxml
;; (eval-after-load "rng-loc"
;;       '(add-to-list 'rng-schema-locating-files "~/.emacs.d/myModes/html5-el/schemas.xml"))
;;     (require 'whattf-dt)


;; Add path to buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse) ; path after file name to ease autocompletion
;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; path after in < > 


;;The following code either switches to the term buffer used last or creates a new one, if a term buffer is already selected. 
(defun last-term-buffer (l)
      "Return most recently used term buffer."
      (when l
	(if (eq 'term-mode (with-current-buffer (car l) major-mode))
	    (car l) (last-term-buffer (cdr l)))))
(defun get-term ()
      "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
      (interactive)
      (let ((b (last-term-buffer (buffer-list))))
	(if (or (not b) (eq 'term-mode major-mode))
	    (multi-term)
	  (switch-to-buffer b))))


;;;; Custom keyboard bindings


; For root on localhost through tramp. Avoid being asked for password on restart
;; (setq recentf-auto-cleanup 'never)
(setq tramp-verbose 6)
(setq tramp-default-method "ssh")

;;;; Default theme
;; (load-theme 'sanityinc-solarized-dark t)
;; (load-theme 'sanityinc-tomorrow-bright t)


;;; Highlight matching paranthesis
(show-paren-mode 1) ; turn on paren match highlighting
;; (setq show-paren-style 'expression) ; highlight entire bracket expression

;; Windmove allows switching windows with shift+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; Get the newest version of ll-debug.el via
;;
;; http://www.cbrunzema.de/software.html#ll-debug
;; (require 'll-debug)

;; Now you can bind ll-debug commands to keystrokes yourself or just
;; call `ll-debug-install-suggested-keybindings'. It clobbers C-v,
;; which may not be completely emacs-political-correct, but it happens
;; to be the stuff I use daily, it is only a suggestion, blah, if you
;; don't like it, don't use it blah blah, do it your own way blah bla
;; blah and don't flame me....
;; `ll-debug-install-suggested-keybindings' installs the following
;; keybindings:
;;
;; C-v C-v   ll-debug-toggle-comment-region-or-line
;; C-v v     ll-debug-uncomment-region-or-line
;; C-v C-y   ll-debug-copy-and-comment-region-or-line
;; C-v C-d   ll-debug-insert

;; (ll-debug-install-suggested-keybindings)

(put 'narrow-to-region 'disabled nil)
