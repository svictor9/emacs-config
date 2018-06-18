;; Packages
(require 'package)
(add-to-list 'load-path "~/.emacs.d/my/")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ;; ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(load-file "~/.emacs.d/orgmode_init.el")

(autoload 'web-mode "web-mode" "Web Mode for html and mixed files" t)
(autoload 'php-mode "~/.emacs.d/php_init.el" "Php-mode for editing PHP code" t)
(autoload 'helm-mu "helm-mu" "Use Helm with mu" t)
(autoload 'helm-mu-contacts "helm-mu" "Helm-mu for contact search" t)
					;(autoload 'python-mode "python-mode" "Elpy mode for Python" t)
(autoload 'mu4e "~/.emacs.d/mu4e_init.el" "Mu email config" t)
(global-set-key (kbd "C-c m") 'mu4e)
(add-hook 'python-mode-hook 'elpy-mode)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Theme
(load-theme 'sanityinc-tomorrow-night t)

;; Helm mode
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-for-files)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(setq helm-ff-skip-boring-files t)

;; in Helm-mu only contacts who sent me email
(setq helm-mu-contacts-personal t)

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
(helm-add-action-to-source "Attach to Email" #'mml-attach-file helm-source-locate)

;; Projectile et helm-projectile
;; (projectile-mode)
(eval-after-load 'projectile-mode 'helm-projectile-on)
;; (helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile-find-file)
(setq projectile-globally-ignored-directories (quote
					       ("zz-old" ".git" ".hg" ".bzr" ".svn" ".stack-work" "__pycache__")))
(setq projectile-globally-ignored-file-suffixes (quote ("~" "#" ".bak")))


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

(setq sentence-end-double-space nil)

;; <RET> or Ctrl-o to insert hard new lines (never filled)
(setq-default use-hard-newlines t)
(setq mu4e-compose-format-flowed t)

;; Auto-complete in web-mode is context-aware
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-words-in-buffer ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))
        ("php" . (ac-source-words-in-buffer
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))))


;; prefer flycheck over flymake in elpy
(add-hook 'elpy-mode-hook 'flycheck-mode)

(setq elpy-rpc-backend "jedi")

;; Never ask long confirmations
(defalias 'yes-or-no-p 'y-or-n-p)

;; Alternate PDF rederer
;; initialise
(pdf-tools-install)
;; open pdfs scaled to fit page
(setq-default pdf-view-display-size 'fit-page)
;; automatically annotate highlights
(setq pdf-annot-activate-created-annotations t)
;; use normal isearch
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

;; Use pdf-tools to render Lilypond pdfs
(setq LilyPond-pdf-command "ec")


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

   (global-set-key (kbd "C-%") 'other-window)

   ;; Rebind M-q to the org function which seems to work better in mails etc.
   (global-set-key (kbd "M-q") 'org-fill-paragraph)
		
   (global-set-key "\M-n" 'forward-paragraph)
   (global-set-key "\M-p" 'backward-paragraph)

(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)

;; 'dabbrev-expand is one of the functions in hippie-expand-try-functions-list anyway
(global-set-key "\M-/" 'hippie-expand)

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-ê") 'helm-company)
     (define-key company-active-map (kbd "C-ê") 'helm-company)))

;; Windmove allows switching windows with shift+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-x g") 'magit-status)


;;;;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(global-set-key (kbd "<f8>") 'multi-term-dedicated-toggle)
(setq multi-term-dedicated-select-after-open-p t)
(setq term-scroll-to-bottom-on-output t)

;;;;; Dired
;; -si for human readable size time-style for yyyy-mm-dd
(setq dired-listing-switches "-Al --si --time-style long-iso")
(setq delete-by-moving-to-trash t)

(setq nxml-slash-auto-complete-flag t)
;; Add path to buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse) ; path after file name to ease autocompletion

					; For root on localhost through tramp. Avoid being asked for password on restart
(setq tramp-verbose 6)
(setq tramp-default-method "ssh")

(show-paren-mode 1) ; turn on paren match highlighting
;; (setq show-paren-style 'expression) ; highlight entire bracket expression
(delete-selection-mode)
(setq dired-dwim-target t)
(setq shift-select-mode t)
(tool-bar-mode -1)
(setq transient-mark-mode t)


(setq eww-search-prefix "https://duckduckgo.com/?q=")

;; Enable narrow-to region. Bound to C-x n n.
(put 'narrow-to-region 'disabled nil)

;; Put autogenerated files aside
(setq auto-save-file-name-transforms
      (quote
       ((".*" "~/.emacs.d/.autogeneres/auto-save-files/" t))))
(setq auto-save-list-file-prefix "~/.emacs.d/.autogeneres/auto-save-list/saves-")
(setq backup-directory-alist (quote ((".*" . "/home/vic/.emacs.d/backups/"))))

;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#d6d6d6" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#4d4d4c"))
 '(beacon-color "#c82829")
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(fci-rule-color "#d6d6d6")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (json-mode dumb-jump web-mode undo-tree swiper realgud python-environment py-autopep8 pdf-tools org-mime multi-term magit less-css-mode iedit helm-projectile helm-mu helm-company flycheck elpy company-quickhelp company-php comment-dwim-2 color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized calfw-org calfw auto-complete auth-password-store)))
 '(projectile-mode t nil (projectile))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
