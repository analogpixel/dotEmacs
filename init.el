;; create a macro that allow setting anonymous functions easier
;; expands to (lambda () (interactive) (command))
(defmacro ## (var) (list 'lambda '() '(interactive) var))

;; load paths
(load "~/.emacs.d/elisp/toggleCase.el")
(load "~/.emacs.d/elisp/any-ini-mode.el")

;; startup python jedi mode stuff
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;; Make sure all buffers save with unix line endings and not ^m
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Configure per-OS stuff here
(defun configureLinux ()
	(setq tramp-default-method "ssh")
	)

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
	(setq org-babel-sh-command "cmd /k")
	(set-default 'tramp-auto-save-directory "c:/data/tmp")
	(set-default 'tramp-default-method "plinkx")
	(setq password-cache-expiry nil)
	(setenv "JAVA_CMD" "c:/Program Files/Java/jdk1.8.0_65/bin/java.exe" )
	(setq scad-command "e:/bin/openscad/openscad.exe")

	(setenv "PATH" (concat "E:/bin/PortableApps/SWI-PrologPortable/App/SWI-Prolog/bin" ";" (getenv "PATH")))
	(setq exec-path (append exec-path '("E:/bin/PortableApps/SWI-PrologPortable/App/SWI-Prolog/bin")))
	)

(cond ((string= system-type "windows-nt") (configureWindows))
			((string= system-type "gnu/linux")  (configureLinux))
			((string= system-type "darwin")     (configureMac))
			)

(menu-bar-mode -1) ;; turn off that menu bar

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 5) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq mouse-autoselect-window t) ;; buffer/window is selected on hover not click

;; middle mouse button paste system clipboard
(global-set-key (kbd "<mouse-2>") 'clipboard-yank)

;; resize the windows with shift control arrow
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(package-initialize)

;; load the package system so we can get new packages
(require 'package)


(setq package-archives  '( ("melpa" . "http://melpa.org/packages/")
													 ("marmalade" . "https://marmalade-repo.org/packages/")
													 ("org" . "http://orgmode.org/elpa/")
													 ("gnu" . "http://elpa.gnu.org/packages/")
													 ))

;; install all the libs used if they aren't already there
;; (dolist (lib '(puppet-mode cider ace-jump-mode magit expand-region  yasnippet helm gnugo rainbow-delimiters paredit  processing-mode ace-window htmlize logstash-conf multiple-cursors   yaml-mode web-mode org-download key-chord scad-mode))
;;   (unless (package-installed-p lib) (progn
;; 																			(package-refresh-contents)
;; 																			(package-install lib) )))


;; Logstash conf mode configuration
(setq logstash-indent 2)

;; prolog setup
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))


;; multiple cursors setup
;; https://github.com/magnars/multiple-cursors.el
;; mark a rectangular region with C-x space and then C-c m to edit it
(global-set-key (kbd "C-c m") 'mc/edit-lines)

;; magit configuration
;; for windows run: git config --global credential.helper wincred
;; also for windows .gitconfig needs to move from c:\users\<uid> to
;; c:\users\<uid>appdata\roaming
(global-set-key (kbd "C--") 'magit-status)
(setq magit-commit-all-when-nothing-staged t) ;; stage all unstaged files

;; cider configuration
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer nil)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl t)

;; Various keyboard bindings
(global-set-key (kbd "<M-wheel-down>")   (## (end-of-buffer)))
(global-set-key (kbd "<M-wheel-up>")  (## (beginning-of-buffer)))

;; (global-set-key (kbd "C-x C-s") (## (isearch-forward)))
(global-set-key (kbd "<f12>")  'bookmark-jump)
(global-set-key (kbd "<f11>")  'bookmark-set)
(global-set-key (kbd "<f1>")   'flyspell-buffer)

(setq rainbow-delimiters-depth-1-face '((t (:foreground, purple))))
(setq rainbow-delimiters-depth-2-face '((t (:foreground, blue))))

(setq auto-revert-verbose nil)

(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "<f2>")   'switch-to-previous-buffer)

(global-set-key (kbd "<f5>") 'align-regexp )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (wombat)))
 '(inhibit-startup-screen t)
 '(magit-diff-options nil)
 '(tool-bar-mode nil)
 '(truncate-lines nil))


;; Configure actual rainbow parens with rainbow mode
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((((background dark)) (:foreground "purple"))))
 '(rainbow-delimiters-depth-2-face ((((background dark)) (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((((background dark)) (:foreground "darkcyan"))))
 '(rainbow-delimiters-depth-4-face ((((background dark)) (:foreground "red"))))
 '(rainbow-delimiters-depth-5-face ((((background dark)) (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((((background dark)) (:foreground "brown"))))
 '(rainbow-delimiters-depth-7-face ((((background dark)) (:foreground "darkgreen"))))
 '(rainbow-delimiters-depth-8-face ((((background dark)) (:foreground "blue"))))
 '(rainbow-delimiters-depth-9-face ((((background dark)) (:foreground "red"))))
 '(tooltip ((t (:background "white" :foreground "blue" :foundry "fixed")))))

;; when you highlight text, and type it'll delete it
(delete-selection-mode 1)

;; toggle case
(global-set-key (kbd "C-2") 'toggleCase)
(global-set-key (kbd "C-1") 'toggleCaseWord)

;; set the windows size
;; (when window-system (set-frame-size (selected-frame) 150 50))

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
;; (setq org-edit-src-auto-save-idle-delay 1) ;;autosave is kind of anoying
(setq org-startup-folded "showall") ;; don't colapse everythign
(setq org-log-done t) ;; when you close a task it time stampes it
(setq org-list-allow-alphabetical t)
(setq org-timestamp-translate t)
(setq org-startup-with-inline-images t)
(setq org-src-fontify-natively t)
(setq org-export-with-section-numbers nil)
;;(setq org-src-tab-acts-natively t)



;; When in a tangle buffer edit, save the file, and tangle it
(defun saveAndTangle ()
	(interactive)
	(org-edit-src-save)
	(switch-to-buffer (other-buffer (current-buffer) 1))
	(org-babel-tangle)
	(switch-to-buffer (other-buffer (current-buffer) 1))
	)

(global-set-key (kbd "C-c t") 'saveAndTangle)
(global-set-key (kbd "C-c C-t") 'saveAndTangle)
(global-set-key (kbd "M-<f4>") 'org-babel-tangle) ;; wtf right?

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

(setq org-confirm-babel-evaluate nil)  ;; don't ask to run code blocks

;; create directories if they don't exist when you tangle
(setq org-babel-default-header-args (cons '(:mkdirp . "yes")  (assq-delete-all :mkdirp org-babel-default-header-args)))

;; don't add a new line at the begining of each file when you tangle
(setq org-babel-default-header-args (cons '(:padline . "no") (assq-delete-all :padline org-babel-default-header-args)))


;; disable backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;; keep a list of recent files history previous
;; use helm-recentf to list them
(recentf-mode 1)

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
(global-set-key (kbd "<f9>")        'helm-recentf)
(global-set-key (kbd "M-y")         'helm-show-kill-ring)
(global-set-key (kbd "C-x b")       'helm-buffers-list)
(global-set-key (kbd "C-x C-b")     'helm-buffers-list)
(global-set-key (kbd "C-x C-f")     'helm-find-files)
(global-set-key (kbd "C-; C-f")     'helm-occur)
(global-set-key (kbd "C-c C-c")			'helm-colors)
(global-set-key (kbd "C-c alc")     'helm-calcul-expression)
(define-key helm-map (kbd "<tab>")	'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i")		'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")		'helm-select-action) ; list actions using C-z

(setq helm-buffers-fuzzy-matching t)

(global-visual-line-mode 1)
(setq-default fill-column 80  whitespace-line-column 80)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq show-paren-style 'parenthesis)


(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(global-set-key (kbd "M-t") 'transpose-windows)


;; to create a keyboard macro
;; f3 record f4
;; name-last-kbd-macro
;; open init.el
;; insert-kbd-macro
(fset 'quotepaste
			"\"\C-y\"")

(fset 'saveline
			"\C-a\C-k\C-y")

(fset 'dupeline
   [?\C-a ?\C-k ?\C-y return ?\C-y])

;; enable workgroups2
;; C-c z
;; c: create a: rename k: kill v: switch C-s: save C-f: load
;;(workgroups-mode 1)
;;(global-set-key (kbd "C-c C-\\")         'wg-switch-to-previous-workgroup)

(defun procsaverun ()
	(interactive)
	(save-buffer)
	(processing-sketch-run)
	)

;; load up some key chords
;; http://www.emacswiki.org/emacs/key-chord.el
(key-chord-define-global "FF" 'helm-find-files)
(key-chord-define-global "BB" 'helm-buffers-list)
(key-chord-define-global "hj" 'undo)
(key-chord-define-global "qb" 'backward-kill-word)
(key-chord-define-global "qc" 'delete-backward-char)
(key-chord-define-global "qp" 'helm-projectile)
(key-chord-define-global "ML" 'mc/edit-lines)
(key-chord-define-global "MS" 'magit-status)
(key-chord-define-global "qj" 'cider-jack-in)
(key-chord-define-global "SS" (## (save-buffer)))
(key-chord-define-global "UU" 'toggleCase)
(key-chord-define-global "ST" 'saveAndTangle)
(key-chord-define-global "TT" 'org-babel-tangle)
(key-chord-define-global "TW" 'transpose-windows)
(key-chord-define-global "PN" 'persp-next)
(key-chord-define-global "PM" 'persp-prev)
(key-chord-define-global "PC" 'persp-switch)
(key-chord-define-global "??" 'mark-whole-buffer)
(key-chord-define-global "PG" 'gnugo)
(key-chord-define-global "PK" 'gomoku)
(key-chord-define-global "GG" 'goto-line)


;; (message "%s" major-mode)
(key-chord-define emacs-lisp-mode-map  "EE" 'eval-last-sexp)
(key-chord-define-global "QQ" 'kill-ring-save)
(key-chord-define-global "WW" 'yank)
(key-chord-define-global "RR" 'quotepaste)
(key-chord-define-global "AA" 'saveline)
(key-chord-define-global "DD" 'dupeline)

(add-hook 'processing-mode-hook (##
(key-chord-define processing-mode-map "PR" 'procsaverun)))

(require 'cider)
(key-chord-define cider-mode-map "EE" 'cider-eval-last-sexp)

(key-chord-define-global "z[" (## ( point-to-register 1)))
(key-chord-define-global "x[" (## ( jump-to-register 1)))
(key-chord-define-global "z]" (## ( point-to-register 2)))
(key-chord-define-global "x]" (## ( jump-to-register 2)))
(key-chord-define-global "z'" (## ( point-to-register 3)))
(key-chord-define-global "x'" (## ( jump-to-register 3)))

(key-chord-mode 1)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; allow other programs to connect to emacs
(require 'server)
;; don't ask if i'm sure when closing a buffer
(unless (server-running-p) (server-start))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
