;; load the package system so we can get new packages
(require 'package)
(add-to-list 'package-archives  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize) 

;; install all the libs used if they aren't already there
(dolist (lib '(puppet-mode cider ace-jump-mode magit expand-region quickrun yasnippet))
  (unless (package-installed-p lib) (progn
				      (package-refresh-contents)
				      (package-install lib) )))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(truncate-lines nil)
 '(speedbar-show-unkown-files t)
 '(speedbar-update-flag nil)
 )


;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (python . t)
   (clojure . t)
   (sh . t)
   (ruby . t)
   ))

;; disable backups
(setq make-backup-files nil) 
(setq auto-save-default nil)

;;
;;ace jump mode major function
;;
;; https://github.com/winterTTr/ace-jump-mode
;; C-c SPC <char>  jump to first character
;; C-u C-c SPC jump to any character
;; C-u C-u C-c SPC jump to line
(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
;;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-1") 'ace-jump-mode)
(define-key global-map (kbd "C-2") 'ace-jump-line-mode)


;; expand region
;; continue to expand region until you have what you need
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; quick run
;; run the contents of the buffer
;; https://github.com/syohex/emacs-quickrun
(require 'quickrun)

(quickrun-add-command "python"
                      '((:command . "python")
                        (:exec    . "%c %s"))
                      :mode 'python-mode)

(global-set-key (kbd "<f12>") 'quickrun)

(global-set-key (kbd "<f1>") 'speedbar-get-focus)

(speedbar)
(speedbar-toggle-show-all-files)

;; set common speedbar paths, maybe save some typing
(defun sb-sk (k)
  (setq default-directory k)
  (speedbar-update-contents)
  (speedbar-get-focus)
  )

(global-set-key (kbd "C-c C-1") (lambda () (interactive) (sb-sk "c:/users/analog/Desktop/") ))
(global-set-key (kbd "C-c C-2") (lambda () (interactive) (sb-sk "c:/users/analog/Dropbox/") ))

;; turn capslock into ctrl
;; install AutoHotkey
;; Save this to script.ahk
;; double click to install
;;
;; #IfWinActive emacs  ; if in emacs
;;    +Capslock::Capslock ; make shift+Caps-Lock the Caps Lock toggle
;;    Capslock::Control   ; make Caps Lock the control button
;;    #IfWinActive        ; end if in emacs



;; enable snipits
;; https://github.com/capitaomorte/yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/"))
(yas-global-mode 1)


;; spelling
;; http://www.johndcook.com/blog/emacs_windows/#aspell
;; http://aspell.net/win32/
(setq-default ispell-program-name "C:/progra~2/Aspell/bin/aspell.exe")
;;(setq text-mode-hook '(lambda() (flyspell-mode t) ))

;; create a macro that allow setting anonymous functions easier
;; expands to (lambda () (interactive) (command))
(defmacro ## (var) (list 'lambda '() '(interactive) var))

;; Various keyboard bindings
(global-set-key (kbd "<end>")   (## (end-of-buffer)))
(global-set-key (kbd "<home>")  (## (beginning-of-buffer)))
(global-set-key (kbd "C-s")     (## (save-buffer)))
(global-set-key (kbd "C-x C-s") (## (isearch-forward)))




