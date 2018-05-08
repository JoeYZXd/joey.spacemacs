;; 设置剪切板
(setq x-select-enable-clipboard t)
(unless window-system
  (when (getenv "DISPLAY")
    (defun xsel-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    (defun xsel-paste-function ()
      (let ((xsel-output (shell-command-to-string "xsel --clipoard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output)))
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)))
;; 设置剪切板结束

;; Org Mode配置参数
;; 在org mode中使用python3.5执行python代码
(setq org-babel-python-command "/usr/local/apps/python3/bin/python3.5")

(defvar org-agenda-dir "" "gtd org files location")
(setq-default org-agenda-dir "/home/joey/Jooooooey/gtd")
;; Org Mode配置参数结束
