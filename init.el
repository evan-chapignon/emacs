(setq use-package-always-ensure t)
(condition-case nil
    (require 'use-package)
  (file-error
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(delete-selection-mode t)
(display-battery-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-font-lock-mode 1)
(menu-bar-mode -1)
(pixel-scroll-precision-mode -1)
(recentf-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(which-key-mode 1)

(set-face-attribute 'default nil :font "Iosevka" :height 110)
(require 'mu4e)
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height 110)
(set-window-buffer nil (current-buffer))
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq evil-want-keybinding nil)
(setq history-length 25)
(setq frame-resize-pixelwise t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq bookmark-save-flag 1)

(setq mu4e-maildir "~/Mail/univ")
(setq olivetti-body-width 200)
(setq-default auto-fill-function 'do-auto-fill)
(setq org-default-notes-file "~/org/capture.org")
(setq org-directory "~/org")
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(setq ring-bell-function 'ignore)
(setq scroll-margin 5)
(setq user-mail-address "evan.delepine-gengembre.etu@univ-lille.fr" user-full-name "Evan Delepine")
(setq visible-bell t)
(setq-default left-margin-width 1 right-margin-width 1)
(setq-default line-spacing 0.2)
(setq-default tab-width 4)

(defun ouvrir-config-emacs ()
  "Ouvre... la config emacs"
  (interactive)
  (find-file "~/.config/emacs/init.org"))
(global-set-key (kbd "C-c e") 'ouvrir-config-emacs)
(global-set-key (kbd "C-o") 'avy-goto-char)
(global-set-key (kbd "C-c l") 'avy-copy-line)
(global-set-key (kbd "C-c s") 'avy-goto-line)

(defun other-other-window ()
  "Passe à la fenêtre précédente."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-<iso-lefttab>") 'other-other-window)

  (defun shuffle-lines (beg end)
    (interactive "r")
    (let ((lines (split-string (buffer-substring beg end) "\n" t)))
  	(setq lines (shuffle-vector (vconcat lines)))
  	(delete-region beg end)
  	(goto-char beg)
  	(insert (mapconcat 'identity (append lines nil) "\n"))))

  ;; Je vous PROMETS que c'est utile


  (global-set-key (kbd "C-c C-r") 'recentf-open-files)

  (global-set-key (kbd "C-:") 'er/expand-region)
  (global-set-key (kbd "C-<tab>") 'other-window)
  (global-set-key (kbd "C-x C-y") 'compile)


  ;; Pas vraiment une fonction, mais ouvre vterm dans un buffer sous le
  ;;  buffer actuel, un peu comme vscode
  (add-to-list 'display-buffer-alist
  			 '("\\*vterm\\*"
                 (display-buffer-reuse-window
  				display-buffer-in-direction)
                 (direction . bottom)
                 (window-height . 0.3)))

(use-package org
  :hook ((org-mode . org-indent-mode)
		 (org-mode . visual-line-mode)
		 (org-mode . variable-pitch-mode)
		 (org-mode . org-modern-mode))
  :config
  (setq org-hide-leading-stars t     
		org-startup-indented t       
		org-ellipsis "…")
  (require 'ob)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
	 (C . t)
	 (latex . t)
	 (java . t)
	 (org . t))))


(use-package org-modern
  :after org
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "◆")
        org-modern-hide-stars t      
        org-modern-table nil         
        org-modern-checkbox '((?X . "☑") (?- . "❍") (?\s . "☐"))))

(add-hook 'org-mode-hook
          (lambda ()
            (variable-pitch-mode 1)
            (set-face-attribute 'variable-pitch nil :font "Iosevka" :height 110)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-<return>") #'org-meta-return))

(defun imp-edt ()
  (interactive)
  (let* ((url "https://edt-iut.univ-lille.fr/Telechargements/ical/Edt_DELEPINE_GENGEMBRE.ics?version=2018.0.3.6&idICal=F44073DA6A5D3F2604325CF447C258C5&param=643d5b312e2e36325d2666683d3126663d31")
		 (local-file "/tmp/edt.ics")
		 (org-file (expand-file-name "edt_backup.org" org-directory))
		 (converted-file (expand-file-name "edt.org" org-directory)))
	(url-copy-file url local-file t)
	(when (file-exists-p org-file)
      (delete-file org-file))
	(icalendar-import-file local-file org-file)
	(message "Emploi du temps importé dans %s" org-file)

	(with-temp-buffer
      (insert-file-contents org-file)
      (goto-char (point-min))
      (let ((lines '())
			(output ""))
		(while (not (eobp))
          (let ((line (string-trim (thing-at-point 'line t))))
			(unless (string-empty-p line)
              (push line lines)))
          (forward-line 1))
		(setq lines (nreverse lines))
		(while lines
          (let ((line (car lines)))
			(when (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) \\([0-9:]+\\)-\\([0-9:]+\\) \\(.*\\)" line)
              (let* ((month (match-string 1 line))
					 (day (match-string 2 line))
					 (year (match-string 3 line))
					 (start (match-string 4 line))
					 (end (match-string 5 line))
					 (title (match-string 6 line))
					 (timestamp (format "<%s-%02d-%02d %s-%s>"
										year (string-to-number month) (string-to-number day)
										start end)))
				(setq output (concat output "* " title "\n  " timestamp "\n\n")))))
          (setq lines (cdr lines)))
		(with-temp-file converted-file
          (insert output))
		(message "Emploi du temps converti : %s" converted-file)))))

(global-set-key (kbd "<f12>") #'imp-edt)

(use-package vertico
  :init
  (vertico-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ("C-c m" . consult-imenu)))

(use-package company
  :hook (after-init . global-company-mode))



(use-package yasnippet
  :config
  (setq yas-snippet-dirs
		'("~/.config/emacs/snippets"))
  (yas-global-mode 1))

(use-package mood-line
  :config
  (mood-line-mode)
  
  (display-battery-mode 1)
  (display-time-mode 1)
  
  (setq display-time-format "%H:%M" 
		display-time-default-load-average nil)
  
  (setq battery-mode-line-format " [BAT%p%%]"))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create
										"*dashboard*")))
(add-hook 'server-after-make-frame-hook 'dashboard-refresh-buffer)

(setq dashboard-startup-banner "~/Pictures/player1.png")

(setq org-agenda-files '("~/org/TODO/todo.org" "~/org/edt.org"))
(setq org-agenda-span 30)

(setq org-capture-templates
      '(("t" "Tâche" entry
		 (file+headline "~/org/TODO/todo.org" "Tâches")
		 "* TODO %?\nSCHEDULED: %^t\n%u\n%a"
		 :empty-lines 1)))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(load "~/.config/emacs/theme.el")
(load "~/.config/emacs/latex.el")
(load "~/.config/emacs/ia.el")
