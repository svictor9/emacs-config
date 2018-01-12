;; Packages
   (require 'package)
   (add-to-list 'load-path "~/.emacs.d/myModes")
   (add-to-list 'load-path "~/.emacs.d/elpa/")
   (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			    ;; ("marmalade" . "https://marmalade-repo.org/packages/")
			    ("melpa" . "https://melpa.org/packages/")
			    ("org" . "http://orgmode.org/elpa/")))
   (package-initialize)

;; Theme
   (load-theme 'sanityinc-tomorrow-eighties t)

;; Helm mode
   (require 'helm-config)
   (global-set-key (kbd "M-x") 'helm-M-x)
   (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
   (global-set-key (kbd "C-x C-f") 'helm-find-files)
   (global-set-key (kbd "C-x f") 'helm-for-files)
   (global-set-key (kbd "C-x C-b") 'helm-mini)
   (global-set-key (kbd "M-y") 'helm-show-kill-ring)
   (setq helm-ff-skip-boring-files t)

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

   ;; Add attachments to emails with helm-locate
   (helm-add-action-to-source "Attach to Email" #'mml-attach-file
                           helm-source-locate)

;; Projectile et helm-projectile
   (projectile-mode)
   (setq projectile-completion-system 'helm)
   (helm-projectile-on)
   (setq projectile-switch-project-action 'helm-projectile-find-file)
   (add-to-list 'projectile-globally-ignored-directories "zz-old")

;;;;;;;;;;;;;;;;; MU4E
   ;; Need to add to load path explicitly for git version
   (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
   (require 'mu4e)
   (global-set-key (kbd "C-c m") 'mu4e)
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


	  ;; start with the first (default) context;
	  ;; default is to ask-if-none (ask when there's no context yet, and none match)
	  (setq mu4e-context-policy 'pick-first)
	  ;; compose with the current context is no context matches;
	  ;; default is to ask
	  ;; '(setq mu4e-compose-context-policy nil)

   ;; No auto signature, use C-c C-w to include one
   (setq mu4e-compose-signature-auto-include nil)

   (setq mu4e-user-mail-address-list
	 '("victor.stoichita@cnrs.fr"
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

   ;; the maildirs you use frequently; access them with 'j' ('jump')
   (setq   mu4e-maildir-shortcuts
       '(("/svictor/INBOX"     . ?v)
	 ("/victor/INBOX"       . ?i)
	 ("/vs/INBOX"        . ?s)
	 ("/cnrs/INBOX"        . ?c)
	 ("/crem/INBOX"        . ?e)))

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
   (add-to-list 'mu4e-bookmarks '("list:yoshimi.freelists.org" "Yoshimi" ?y))
   (add-to-list 'mu4e-bookmarks '("list:ardour-users-ardour.org)" "Ardour" ?a))
   (add-to-list 'mu4e-bookmarks '("list:linux-audio-user.lists.linuxaudio.org)" "LAU" ?l))
   (add-to-list 'mu4e-bookmarks '("list:mu-discuss.googlegroups.com)" "LAU" ?m))
   (add-to-list 'mu4e-bookmarks '("d:today..now AND NOT g:trashed AND NOT m:/svictor/INBOX/.Archive AND NOT m:/victor/INBOX/.Archive AND NOT m:/crem/Archive AND NOT m:/cnrs/Archives" "Today (and not archived)"     ?t))

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
   (setq sendmail-program "/home/vic/bin/msmtp-enqueue.sh"
   ;; (setq sendmail-program "msmtp"
	 mail-specify-envelope-from t
   ;; needed for debians message.el cf. README.Debian.gz 
	 message-sendmail-f-is-evil nil                
	 mail-envelope-from 'header
	 message-sendmail-envelope-from 'header)
;;;;;;;;;;;;;;; End Mu4E

;; Mu4e et org
   (require 'org-mu4e)
   ;;store link to message if in header view, not to header query
   (setq org-mu4e-link-query-in-headers-mode nil)
   ;; Org mode keys in message mode. Could work in other text modes
   (add-hook 'message-mode-hook 'turn-on-orgstruct)
   ;; When you use orgstruct++-mode, Org will also export indentation and
   ;; autofill settings into that mode, and detect item context after the
   ;; first line of an item.
   (add-hook 'message-mode-hook 'turn-on-orgstruct++)


;; Show entire parenthesis block (alternative: mixed: default: parenthesis)
   (setq show-paren-style 'mixed)

;; Autofill
   ;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
   (add-hook 'fill-nobreak-predicate 'fill-french-nobreak-p)
   (add-hook 'fill-nobreak-predicate 'fill-single-word-nobreak-p)

   ;; Remove auto-fill on text modes (rely on wrapping modes)
   ;; (remove-hook 'text-mode-hook #'turn-on-auto-fill)
   ;; (setq sentence-end-double-space nil)

(defvar V-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?’ "w" table)
    (modify-syntax-entry ?« "($" table)
    (modify-syntax-entry ?» ")^" table)
    table))

(add-hook 'text-mode-hook (lambda ()
			    (set-syntax-table V-syntax-table)))
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)


;; <RET> or Ctrl-o to insert hard new lines (never filled)
   (setq-default use-hard-newlines t)
   (setq mu4e-compose-format-flowed t)

;;;;;;;;;;;;;;; Start OrgMode

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
   (setq org-agenda-restore-windows-after-quit t)				;

   ;; warn me of any deadlines in next 7 days
   (setq org-deadline-warning-days 7)
(setq org-agenda-start-on-weekday 1)

;; Localize calendar for French. Cf https://www.emacswiki.org/emacs/CalendarLocalization#toc9
  (setq calendar-week-start-day 1
          calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi"
                                   "Jeudi" "Vendredi" "Samedi"]
          calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai"
                                     "Juin" "Juillet" "Août" "Septembre"
                                        "Octobre" "Novembre" "Décembre"])

   (setq org-agenda-custom-commands
	 '(("v" "Mon agenda" agenda ""
	    ((org-agenda-ndays 7)          ;; agenda will start in week view
	     (org-agenda-repeating-timestamp-show-all t)   ;; ensures that repeating events appear on all relevant dates
	     (org-agenda-files '("vic_cal.org", "vic_mobile.org")) ;; Only my own agenda
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
	 '(("e" "event" entry (file "victor.org")
	    "* %^{Heading} %i %?\n %^t \n %a\n")
	   ("t" "todo" entry (file+headline "general.org" "À faire")
	    "* TODO [#B] %i %?\nSCHEDULED: %^t\n %a\n")
	   ("n" "notes" entry (file+headline "notes.org" "Notes")
	    "* %i \n %a\n")
	   ("m" "todo mails" entry (file+headline "mails.org" "Mails")
	    "* TODO %? \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%a\n")
	   ("a" "anthro" entry (file+headline "anthro.org" "Anthro")
	    "* TODO %i %?\n\nSCHEDULED: %^t\n %a\n")
	   ("A" "Audio")
	   ("an" "audio notes" entry (file+headline "audio.org" "Audio notes")
	    "* %i %a\n")
	   ("at" "audio todo" entry (file+headline "audio.org" "Audio todo")
	    "* TODO [#B] %i %?\nSCHEDULED: %^t\n %a\n")
	   ;; For mu4e. From http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/ (see above)
	   ("g" "Trucs à geek")
	   ("gn" "Geek" entry (file+headline "geek.org" "Geek notes")
	    "* %i %? \n%a\n")
	   ("gt" "geek todo" entry (file+headline "geek.org" "Geek notes")
	    "* TODO [#B] %i %?\nSCHEDULED: %^t\n %a\n")))


   ;; Insert and follow links that have Org syntax not only in Org but in any Emacs buffer. 
   (global-set-key "\C-c L" 'org-insert-link-global)
   (global-set-key "\C-c o" 'org-open-at-point-global)

   (setq
    org-agenda-files '("~/org")
    org-icalendar-include-todo (quote t)
    ;; org-icalendar-store-UID t
    org-icalendar-timezone "GMT+2 CEST"
    org-icalendar-use-scheduled (quote (event-if-not-todo todo-start))
    ;; org-icalendar-use-scheduled (quote (nil))
    org-log-done (quote time)
    org-replace-disputed-keys nil
    org-export-html-postamble nil)

   (add-hook 'org-mode-hook #'org-indent-mode)

    ;; Org-caldav configuration
(require 'org-caldav)
(setq
 org-caldav-url "https://framagenda.org/remote.php/dav/calendars/svictor"
 org-caldav-calendar-id "personal"
 ;; Todo : remplacer ~/org par concat org-directory
 ;; Todo : vérifier si :inbox et :files sont tous deux nécessaires
 org-caldav-inbox "/home/vic/org/victor_mobile.org"
 org-caldav-files '("/home/vic/org/victor.org")
 org-caldav-days-in-past "30"
 org-caldav-calendars '((:calendar-id "personal")
			(:calendar-id "personal_shared_by_ebreteque" :inbox "~/org/estelle.org" :files ("~/org/estelle.org"))
			(:calendar-id "arthurics_shared_by_ebreteque" :inbox "~/org/kids.org" :files ("~/org/kids.org"))
			) 
				      
 
 org-caldav-backup-file "/home/vic/org/autogeneres/org-caldav-backup.org"
 org-caldav-save-directory "~/org/autogeneres"

 org-caldav-delete-calendar-entries 'ask
 org-caldav-delete-org-entries 'ask
 )
   ;; this hook saves an ics file once an org-buffer is saved
   ;; (defun my-icalendar-agenda-export()
   ;;   (when (string= (buffer-file-name) "/home/vic/org/vic.org")
   ;;     (org-icalendar-export-to-ics)
   ;;     (shell-command "cp /home/vic/org/vic.ics /var/lib/radicale/collections/vic/vic.ics")))
   ;; (add-hook 'after-save-hook 'my-icalendar-agenda-export)

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

(setq org-html-postamble nil)

   ;; Make windmove work in org-mode:
          (add-hook 'org-shiftup-final-hook 'windmove-up)
          (add-hook 'org-shiftleft-final-hook 'windmove-left)
          (add-hook 'org-shiftdown-final-hook 'windmove-down)
          (add-hook 'org-shiftright-final-hook 'windmove-right)
(setq org-archive-location "~/org/archives/::%s_archive")

;;;;;;;;;;;;;; End Org-mode

;; Never ask long confirmations
(defalias 'yes-or-no-p 'y-or-n-p)

;; Alternate PDF renderer
(pdf-tools-install)
 
;;;;;;; Some custom keybindings

   (progn
     ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
     (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
     (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
     (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward) ; single key, useful
     (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
    )

   ;; Ability to revert splite pane config
   (winner-mode 1)
   (global-set-key (kbd "<f5>") 'winner-undo)
   (global-set-key (kbd "<f6>") 'winner-redo)

   (global-set-key (kbd "C-ç") 'comment-dwim-2)

   ;; Rebind M-q to the org function which seems to work better in mails etc.
   (global-set-key (kbd "M-q") 'org-fill-paragraph)
		
   (global-set-key "\M-n" 'forward-paragraph)
   (global-set-key "\M-p" 'backward-paragraph)

   (add-hook 'after-init-hook 'global-company-mode)
   (eval-after-load 'company
     '(progn
	(define-key company-mode-map (kbd "C-ê") 'helm-company)
	(define-key company-active-map (kbd "C-ê") 'helm-company)))

   ;; Windmove allows switching windows with shift+arrows
   (when (fboundp 'windmove-default-keybindings)
     (windmove-default-keybindings))

   (global-set-key (kbd "C-x g") 'magit-status)

;;;;; Start Python customizations

   (require 'elpy)
   (elpy-enable)
   ;; Really need Ipython?
   ;; (elpy-use-ipython)
   ;; If ipython, also set the following for ipython ≥ 5 
   ;; (setq python-shell-interpreter "ipython"
   ;;    python-shell-interpreter-args "--simple-prompt -i")

   ;; Enable flycheck for elpy
   (when (require 'flycheck nil t)
     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
     (add-hook 'elpy-mode-hook 'flycheck-mode))

   (setq elpy-rpc-backend "jedi")

   ;; Autopep8 (mais pas pratique pour ipdb sur même ligne)
   ;; (require 'py-autopep8)
   ;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;;;; End Python customizations

;;;;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(global-set-key (kbd "<f8>") 'multi-term-dedicated-toggle)
(setq multi-term-dedicated-select-after-open-p t)

;;;;; Dired
   ;; -si for human readable size time-style for yyyy-mm-dd
   (setq dired-listing-switches "-Al --si --time-style long-iso")
   (setq delete-by-moving-to-trash t)

;;;; Web-mode 
   (require 'web-mode)
   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
   ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.php$" . my-setup-php))
   (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

   ;; Auto-complete in web-mode is now context-aware. 
   (setq web-mode-ac-sources-alist
      '(("css" . (ac-source-words-in-buffer ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))
        ("php" . (ac-source-words-in-buffer
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))))

   (defun my-setup-php ()
     (web-mode)
     ;; make these variables local
     (make-local-variable 'web-mode-code-indent-offset)
     (make-local-variable 'web-mode-markup-indent-offset)
     (make-local-variable 'web-mode-css-indent-offset)

     ;; set indentation, can set different indentation level for different code type
     (setq web-mode-code-indent-offset 4)
     (setq web-mode-css-indent-offset 2)
     (setq web-mode-markup-indent-offset 2)

     ;; Enable flycheck
     (flycheck-select-checker my-php)
     (flycheck-mode t))

      ;; Flycheck redefined for web mode
      (require 'flycheck)
      (flycheck-define-checker my-php
	"A PHP syntax checker using the PHP command line interpreter."
	:command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
		  "-d" "log_errors=0" source)
	:error-patterns
	((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
		(message) " in " (file-name) " on line " line line-end))
	:modes (php-mode php+-mode web-mode))

;; Add path to buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse) ; path after file name to ease autocompletion

; For root on localhost through tramp. Avoid being asked for password on restart
;; (setq recentf-auto-cleanup 'never)
(setq tramp-verbose 6)
(setq tramp-default-method "ssh")

;; Highlight matching paranthesis
(show-paren-mode 1) ; turn on paren match highlighting
;; (setq show-paren-style 'expression) ; highlight entire bracket expression

;; Enable narrow-to region. Bound to C-x n n.
(put 'narrow-to-region 'disabled nil)

;; Custom set variables
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
 '(auto-save-file-name-transforms
   (quote
    ((".*" "~/.emacs.d/.autogeneres/auto-save-files/" t))))
 '(auto-save-list-file-prefix "~/.emacs.d/.autogeneres/auto-save-list/saves-")
 '(backup-directory-alist (quote ((".*" . "/home/vic/.emacs.d/backups/"))))
 '(beacon-color "#f2777a")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(delete-selection-mode t)
 '(dired-dwim-target t)
 '(eww-search-prefix "https://www.google.com/search?q=")
 '(fci-rule-color "#424242")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(nxml-slash-auto-complete-flag t)
 '(org-export-backends (quote (ascii html icalendar latex odt)))
 '(package-selected-packages
   (quote
    (org-caldav org-plus-contrib pdf-tools org-mime magit comment-dwim-2 web-mode undo-tree swiper realgud python-environment py-autopep8 php-mode multi-term less-css-mode helm-projectile helm-company flycheck elpy company-quickhelp color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized calfw auto-complete)))
 '(projectile-globally-ignored-directories
   (quote
    ("zz-old" ".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "__pycache__")))
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
