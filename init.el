;; create a macro that allow setting anonymous functions easier
;; expands to (lambda () (interactive) (command))
(defmacro ## (var) (list 'lambda '() '(interactive) var))


;; load paths
(load "~/.emacs.d/elisp/toggleCase.el")
;; (load "~/.emacs.d/elisp/lineup.el")


;; Configure per-OS stuff here
(defun configureLinux () )

(defun configureMac   () )

(defun configureWindows ()
	;; configure termainal to work
	;(setq explicit-shell-file-name "C:/Program Files (x86)/Git/bin/bash.exe")
	;(setq shell-file-name explicit-shell-file-name)
	;(add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")
	(setq gnugo-program "C:/bin/gnugo-3.8/gnugo.exe")

	;; configure aspell to work
	;; spelling
	;; http://www.johndcook.com/blog/emacs_windows/#aspell
	;; http://aspell.net/win32/
	(setq-default ispell-program-name "c:/progra~2/Aspell/bin/aspell.exe")
	(setq processing-location "c:/bin/processing-2.2.1/processing-java.exe")
	(setq processing-application-dir "c:/bin/processing-2.2.1")
	(setq processing-sketchbook-dir "c:/data/processingSketches")
	)

(cond ((string= system-type "windows-nt") (configureWindows))
			((string= system-type "gnu/linux")  (configureLinux))
			((string= system-type "darwin")     (configureMac))
			)



;; load the package system so we can get new packages
(require 'package)
(setq package-archives  '( ("melpa" . "http://melpa.org/packages/")
                            ("org" . "http://orgmode.org/elpa/")
														))

(package-initialize)

;; install all the libs used if they aren't already there
(dolist (lib '(puppet-mode cider ace-jump-mode magit expand-region quickrun yasnippet helm gnugo rainbow-delimiters paredit company processing-mode ace-window logstash-conf multiple-cursors helm-swoop))
  (unless (package-installed-p lib) (progn
				      (package-refresh-contents)
				      (package-install lib) )))


;; Make sure all buffers save with unix line endings and not ^m
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; delete backwards
(global-set-key (kbd "C-,") 'delete-backward-char)
(global-set-key (kbd "M-,") 'backward-kill-word)
;; C-Delete delete word forwards

;; Logstash conf mode configuration
(setq logstash-indent 2)

;; multiple cursors setup
;; https://github.com/magnars/multiple-cursors.el
(global-set-key (kbd "C-c m") 'mc/edit-lines)

;;helm-swoop
;;https://github.com/ShingoFukuyama/helm-swoop

;; magit configuration
;; for windows run: git config --global credential.helper wincred

;; cider configuration
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer nil)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)

;; Various keyboard bindings
(global-set-key (kbd "<end>")   (## (end-of-buffer)))
(global-set-key (kbd "<home>")  (## (beginning-of-buffer)))
(global-set-key (kbd "C-s")     (## (save-buffer)))
(global-set-key (kbd "C-x C-s") (## (isearch-forward)))
(global-set-key (kbd "<f12>")  'bookmark-jump)
(global-set-key (kbd "<f11>")  'bookmark-set)
(global-set-key (kbd "<f1>")   'flyspell-buffer)


(setq auto-revert-verbose nil)


(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "<f2>")   'switch-to-previous-buffer)

(custom-set-variables
 '(custom-enabled-themes (quote (wombat)))
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(truncate-lines nil)
 '(coffee-tab-width 2)
 )

;; when you highlight text, and type it'll delete it
(delete-selection-mode 1)

;; toggle case
(global-set-key (kbd "C-`") 'toggleCase)
(global-set-key (kbd "C-1") 'toggleCaseWord)

;; Align characters
(global-set-key (kbd "C-+") 'lineup)

;; set the windows size
(when window-system (set-frame-size (selected-frame) 150 50))

;; show me where my parens don't add up
(show-paren-mode t)

;; show lines and cols
(setq line-number-mode t)
(setq column-number-mode t)
(setq linenum-mode t)

;; configure tabs
(setq indent-tabs-mode nil)
(setq-default tab-width 2)
(add-hook 'python-mode-hook (## (setq python-indent 2)))

(defun timestamp ()
   (interactive)
   (insert (format-time-string "%Y-%m-%d %R")))

(global-set-key (kbd "C-; d") 'timestamp)

;; orgmode options
;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
;; http://lists.gnu.org/archive/html/emacs-orgmode/2011-02/msg00465.html

(require 'ox-md)
(require 'ox-odt)
(setq org-startup-folded "showall")
(setq org-log-done t) ;; when you close a task it time stampes it
(setq org-list-allow-alphabetical t)
(setq org-timestamp-translate t)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (python . t)
   (clojure . t)
   (sh . t)
   (ruby . t)
	 (ditaa . t)
   ))

(setq org-confirm-babel-evaluate nil)
;; disable backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;;keep a list of recent files history previous
(recentf-mode 1)
(global-set-key (kbd "<f9>") 'recentf-open-files)

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

(global-set-key (kbd "C-; C-l") 'ace-jump-mode)
(global-set-key (kbd "C-; C-;") 'ace-jump-char-mode)
(global-set-key (kbd "C-; C-'") 'ace-jump-line-mode)
(global-set-key (kbd "M-p") 'ace-window)
;; expand region
;; continue to expand region until you have what you need
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; enable snippets
;; https://github.com/capitaomorte/yasnippet
;; http://capitaomorte.github.io/yasnippet/
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/"))
(yas-global-mode 1)


(global-set-key (kbd "C-z") 'undo)
;; M-; will comment/uncomment a selected region

;; http://tuhdo.github.io/helm-intro.html
(helm-mode t)
(global-set-key (kbd "M-x")         'helm-M-x)
(global-set-key (kbd "M-y")         'helm-show-kill-ring)
(global-set-key (kbd "C-x b")       'helm-buffers-list)
(global-set-key (kbd "C-x C-b")     'helm-buffers-list)
(global-set-key (kbd "C-x C-f")     'helm-find-files)
(global-set-key (kbd "C-; C-f")         'helm-occur)
(global-set-key (kbd "C-c C-c")   'helm-colors)
(global-set-key (kbd "C-c alc")     'helm-calcul-expression)
(setq helm-buffers-fuzzy-matching t)


(global-visual-line-mode 1)
(setq-default fill-column 80  whitespace-line-column 80)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq show-paren-style 'parenthesis)
