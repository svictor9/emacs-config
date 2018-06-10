;;;;;;;;;;;;;;;;; MU4E
;; Need to add to load path explicitly for git version
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
	   :enter-func (lambda () (mu4e-message "Switch to the CNRS context"))
	   :match-func (lambda (msg)
			 (when msg
			   (mu4e-message-contact-field-matches msg
							       :to "victor.stoichita@cnrs.fr")))
	   :vars '(  ( user-mail-address	. "victor.stoichita@cnrs.fr" )
		     ( user-full-name . "Victor A. Stoichita" )
		     ( mu4e-sent-folder . "/cnrs/Sent Items" )
		     ( mu4e-drafts-folder . "/cnrs/Drafts" )
		     ( mu4e-trash-folder . "/cnrs/Deleted Items" )
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
(setq message-citation-line-function (quote message-insert-formatted-citation-line))
(setq message-citation-line-format "Le %d %b %Y, %f a écrit :\n")
(setq mu4e-view-show-addresses t)

;; Show fancy chars instead of letters
(setq mu4e-use-fancy-chars t)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Compose messages in new frame
(setq mu4e-compose-in-new-frame t)

;; Send mails in html also
(setq org-mu4e-convert-to-html t)
(defun htmlize-and-send ()
  "When in an org-mu4e-compose-org-mode message, htmlize and send it."
  (interactive)
  (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
    (org-mime-htmlize)
    (org-mu4e-compose-org-mode)
    (mu4e-compose-mode)
    (message-send-and-exit)))

;; the maildirs you use frequently; access them with 'j' ('jump')
(setq   mu4e-maildir-shortcuts
	'(("/svictor/INBOX"     . ?v)
	  ("/victor/INBOX"       . ?i)
	  ("/vs/INBOX"        . ?s)
	  ("/cnrs/INBOX"        . ?c)))

(setq mu4e-change-filenames-when-moving t ; to keep mbsync happy
      mu4e-index-update-error-continue t
      mu4e-index-update-error-warning t
      mu4e-hide-index-messages t)

;; Pour aller chercher le pain dans emacs. 
(setq mu4e-update-interval 600 ;update toutes les 10 minutes
      mu4e-get-mail-command "timeout 60s mbsync -a") ; Timeout to avoid getting stuck after suspend/resume

					; Mu4e Bookmarks
(add-to-list 'mu4e-bookmarks '("( contact:svictor@svictor.net OR contact:victor@svictor.net OR contact:vs@svictor.net OR contact:victor.stoichita@cnrs.fr OR contact:victor.stoichita@mae.u-paris10.fr)"       "For me"     ?v))
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

;; the headers to show in the headers list -- a pair of a field
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
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))

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
	       ((string-match "victor.stoichita@cnrs.fr" from) "cnrs"))))
	  (setq message-sendmail-extra-arguments (list '"-a" account))))))
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'choose-msmtp-account)


(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/home/vic/bin/msmtp-enqueue.sh"
      mail-specify-envelope-from t
      ;; needed for debians message.el cf. README.Debian.gz 
      message-sendmail-f-is-evil nil                
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

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
