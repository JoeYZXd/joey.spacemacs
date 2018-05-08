;;; packages.el --- joey-programming layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Joey.ZhangXiaodong <joy.zhangxiaodong@gmail.com>
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
;; added to `joey-programming-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `joey-programming/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `joey-programming/pre-init-PACKAGE' and/or
;;   `joey-programming/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(defconst joey-programming-packages
  '(
    company
    elpy
    flycheck
    vue-mode
    lsp-mode
    lsp-vue
    company-lsp)
  "The list of Lisp packages required by the joey-programming layer.
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun joey-programming/init-elpy ()
      (use-package elpy
        :diminish elpy-mode
        :config

        (defun elpy-modules-remove-modeline-lighter (mode-name))

        (setq elpy-modules '(elpy-module-sane-defaults
                             elpy-module-eldoc
                             elpy-module-pyvenv))

        (when (configuration-layer/layer-usedp 'auto-completion)
          (add-to-list 'elpy-modules 'elpy-module-company)
          (add-to-list 'elpy-modules 'elpy-module-yasnippet))

        (elpy-enable)))

(defun joey-programming/post-init-company ()
  (spacemacs|add-company-hook inferior-python-mode)
  (push 'company-capf company-backends-inferior-python-mode)
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 0)
              (setq-local company-idle-delay 0.5))))

(defun joey-programming/post-init-flycheck ()
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(defun joey-programming/init-lsp-mode ()
  (use-package lsp-mode))

(defun joey-programming/init-lsp-vue ()
  (use-package lsp-vue))

(defun joey-programming/init-company-lsp ()
  (use-package company-lsp))

(defun joey-programming/init-vue-mode ()
  "Initialize vue mode package"
  (with-eval-after-load 'vue-mode
    (progn
      (add-to-list 'vue-mode-hook #'smartparens-mode)
      (require 'lsp-mode)
      (require 'lsp-vue)
      (add-hook 'vue-mode-hook #'lsp-vue-mmm-enable)
      (with-eval-after-load 'lsp-mode
        (require 'lsp-flycheck))

      (require 'company-lsp)
      (push 'company-lsp company-backends))))
;;; packages.el ends here
