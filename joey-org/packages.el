;;; packages.el --- joey-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Joey.ZhangXiaodong <joey@GALAXY>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `joey-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `joey-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `joey-org/pre-init-PACKAGE' and/or
;;   `joey-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst joey-org-packages
  '(
    org
    org-pomodoro
    ))

(defun joey-org/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook '(lambda () (zilongshanren/growl-notification "Pomodoro Finished" "☕️ Have a break!" t)))
    (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (zilongshanren/growl-notification "Short Break" "🐝 Ready to Go?" t)))
    (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (zilongshanren/growl-notification "Long Break" " 💪 Ready to Go?" t)))
    ))

(defun joey-org/post-init-org ()
  (add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append)
  (with-eval-after-load 'org
    (progn
      (spacemacs|disable-company org-mode)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "," 'org-priority)
      (require 'org-compat)
      (require 'org)
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)
      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)

      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-inhibit-startup t)
      (setq org-agenda-span 'day)
      (setq org-agenda-use-tag-inheritance nil)
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)

      (add-to-list 'auto-mode-alist '("\.org\\'" . org-mode))

      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                    (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))

      (setq org-clock-in-switch-to-state "STARTED")

      (setq org-clock-into-drawer t)

      (setq org-clock-out-remove-zero-time-clocks t)

      (setq org-tags-match-list-sublevels nil)

      (add-hook 'org-mode-hook '(lambda ()
                                  (local-set-key (kbd "C-c i s")
                                                 'joey/org-insert-src-block)))

      (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

      (setq org-plantuml-jar-path "~/.emacs.d/private/plantuml.jar")
      (setq org-ditaa-jar-path "~/.emacs.d/private/ditaa.jar")
      (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
      (setq org-babel-results-keyword "results")
      (defun bh/display-inline-images ()
        (condition-case nil
            (org-display-inline-images)
          (error nil)))

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         (ruby . t)
         (ledger . t)
         (dot . t)
         (js . t)
         (latex . t)
         (python . t)
         (emacs-lisp . t)
         (plantuml . t)
         (C . t)
         (ditaa . t)))

      (setq org-confirm-babel-evaluate nil)

      (require 'ox-md nil t)

      (setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-someday (expand-file-name "someday.org" org-agenda-dir))
      (setq org-agenda-file-tickler (expand-file-name "tickler.org" org-agenda-dir))
      (setq org-agenda-files (list org-agenda-file-inbox
                                   org-agenda-file-gtd
                                   org-agenda-file-tickler))

      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
          "." 'spacemacs/org-agenda-transient-state/body)
        )
      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates
            '(("t" "Todo [inbox]" entry (file+headline org-agenda-file-inbox "Tasks")
               "* TODO %i%?"
               :empty-lines 1)
              ("T" "Tickler" entry (file+headline org-agenda-file-tickler "Tickler")
               "* %i%? \n %U")))

      (setq org-refile-targets '((org-agenda-file-gtd :maxlevel . 3)
                                 (org-agenda-file-someday :level . 1)
                                 (org-agenda-file-tickler :maxlevel . 2)))
      (setq org-agenda-custom-commands
            '(("g" . "GTD contexts")
              ("go" "Office" tags-todo "office")
              ("gc" "Computer" tags-todo "computer")
              ("gp" "Phone" tags-todo "phone")
              ("gh" "Home" tags-todo "home")
              ("ge" "Errands" tags-todo "errands")
              ("G" "GTD Block Agenda"
               ((tags-todo "office")
                (tags-todo "computer")
                (tags-todo "phone")
                (tags-todo "home")
                (tags-todo "errands"))
               nil
               ("~/next-actions.html"))
              )
            ))))
;;; packages.el ends here
