(require 'org)
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
   (setq org-default-notes-file (concat org-directory "/trucs.org"))

   (setq org-capture-templates
	 '(("e" "Événement" entry (file "victor.org")
	    "* %^{Heading} %i %?\n %^t \n %a\n")
	   ("t" "Trucs")
	   ("tt" "Trucs todo" entry (file+headline "trucs.org" "À faire")
	    "* TODO [#B] %i %?\nSCHEDULED: %^t\n ")
	   ("tn" "Trucs notes" entry (file+headline "trucs.org" "Notes")
	    "* %i \n")
	   ("tm" "Répondre aux mails" entry (file+headline "trucs.org" "Répondre aux mails")
    	   ;; For mu4e. From http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/
	    "* TODO %a\n :mail: \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n")
	   ("a" "Anthro")
	   ("at" "Anthro todo" entry (file+headline "anthro.org" "À faire")
	    "* TODO %i %?\n\nSCHEDULED: %^t\n")
	   ("an" "Anthro notes" entry (file+headline "anthro.org" "Notes")
	    "* %i \n")
	   ("s" "Son")
	   ("st" "Son todo" entry (file+headline "son.org" "À faire")
	    "* TODO [#B] %i %?\nSCHEDULED: %^t\n")
	   ("sn" "Son notes" entry (file+headline "son.org" "Notes")
	    "* %? %i\n")
	   ("o" "Ordis")
	   ("ot" "Ordis todo" entry (file+headline "ordis.org" "À faire")
	    "* TODO [#B] %i %?\nSCHEDULED: %^t\n")
	   ("on" "Ordis notes" entry (file+headline "ordis.org" "Notes")
	    "* %i %? \n")))


   ;; Insert and follow links that have Org syntax not only in Org but in any Emacs buffer. 
   (global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)
(global-set-key "\C-cl" 'org-store-link)

   (setq
    org-agenda-files '("~/org")
    org-icalendar-include-todo (quote t)
    ;; org-icalendar-store-UID t
    org-icalendar-timezone "GMT+2 CEST"
    org-icalendar-use-scheduled (quote (event-if-not-todo todo-start))
    ;; org-icalendar-use-scheduled (quote (nil))
    org-log-done (quote time)
    org-replace-disputed-keys nil
    org-export-html-postamble nil
    org-export-backends (quote (ascii html icalendar latex odt))
    org-refile-targets (quote ((org-agenda-files :level . 1))))

(add-hook 'org-mode-hook #'org-indent-mode)

    ;; Org-caldav configuration
(require 'auth-password-store)
(auth-pass-enable)

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
(setq org-archive-location "~/org/archive::datetree/")

(require 'org-mime)
(setq org-mime-library 'mml)
