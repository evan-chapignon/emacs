(setq use-package-always-ensure t)
(condition-case nil
    (require 'use-package)
  (file-error
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))


  (require 'package)
  (add-to-list 'package-archives '("melpa" "https://melpa.org/packages/") t)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(add-hook 'emacs-startup-hook #'recentf-open-files)

(delete-selection-mode t)
  (display-battery-mode 1)
  (electric-pair-mode 1)
  (global-display-line-numbers-mode t)
  (global-font-lock-mode 1)
  (menu-bar-mode -1)
  (olivetti-mode 1)
;;  (pixel-scroll-precision-mode 1)
  (recentf-mode 1)
  (save-place-mode 1)
  (savehist-mode 1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (company-mode 1)
  (which-key-mode 1)

(set-face-attribute 'default nil :font "Iosevka" :height 110)
(require 'mu4e)
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height 110)
(set-window-buffer nil (current-buffer))
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq custom-file (concat user-emacs-directory "custom.el"))
(setq evil-want-keybinding nil)
(setq history-length 25)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
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
  (interactive)
  (find-file "~/.config/emacs/README.org"))
(global-set-key (kbd "C-c e") 'ouvrir-config-emacs)
(global-set-key (kbd "C-o") 'avy-goto-char)
(global-set-key (kbd "C-c l") 'avy-copy-line)
(global-set-key (kbd "C-c s") 'avy-goto-line)

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
        org-ellipsis "…"))           

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

(use-package vertico
:init
(vertico-mode))

(use-package magit
:ensure t
:bind (("C-x g" . magit-status)))

(mood-line-mode 1)

(display-battery-mode 1)
(display-time-mode 1)

(setq display-time-format "%H:%M" 
      display-time-default-load-average nil)

(setq battery-mode-line-format " [BAT%p%%]")

(setq org-agenda-files '("~/org/TODO/todo.org" "~/org/edt.org"))
(setq org-agenda-span 30)

(setq org-capture-templates
      '(("t" "Tâche" entry
		 (file+headline "~/org/todo.org" "Tâches")
		 "* TODO %?\nSCHEDULED: %^t\n%u\n%a"
		 :empty-lines 1)))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(load "~/.config/emacs/theme.el")
(load "~/.config/emacs/latex.el")
