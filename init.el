;; ######################################## INSTALL PACKAGES ######################################
(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p `better-defaults) (package-install `better-defaults))
(unless (package-installed-p `material-theme) (package-install `material-theme))
(unless (package-installed-p `auto-complete) (package-install `auto-complete))


;; ##################################### BASIC CUSTOMIZATION ######################################
(setq inhibit-startup-message t) ;; hide the startup message
;; (load-theme 'material t) ;; load material theme
;; (global-linum-mode t) ;; enable line numbers globally
(set-default 'truncate-lines t) ;; do not wrap
(prefer-coding-system 'utf-8) ;; use UTF-8


;;### Shell configuration
(use-package exec-path-from-shell
  :init
  (setenv "SHELL" "/bin/zsh")
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))


;;### Faked full screen
(use-package maxframe
     :ensure t)
(defvar my-fullscreen-p t "Check if fullscreen is on or off")
(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (restore-frame)
	(maximize-frame)))
;; (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen) ;; conflicts with an auctex command to insert an \item in a list.

;;### Turn on font-locking or syntax highlighting
(global-font-lock-mode t)

;;### font size in the modeline
(set-face-attribute 'mode-line nil  :height 240)


;;### hippie-expand
(global-set-key [remap dabbrev-expand]  'hippie-expand)


; ;; Set copy+paste
;  (cua-mode t)
;     (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;     (transient-mark-mode 1) ;; No region when it is not highlighted
;     (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; REMOVE THE SCRATCH BUFFER AT STARTUP
;; Makes *scratch* empty.
;; (setq initial-scratch-message "")
;; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)


;; dired-icon-mode

(add-hook 'dired-mode-hook 'dired-icon-mode)

;; replace dired with dirvish
(dirvish-override-dired-mode)


;; Revert Dired and other buffers after changes to files in directories on disk.
;; Source: [[https://www.youtube.com/watch?v=51eSeqcaikM&list=PLEoMzSkcN8oNmd98m_6FoaJseUsa6QGm2&index=2][Dave Wilson]]
(setq global-auto-revert-non-file-buffers t)


;; customize powerline
;; (line above the command line at the bottom of the screen)
(use-package powerline
  :ensure t)
(require 'powerline)
(powerline-default-theme)

;; highlight current line
(global-hl-line-mode +1)
(set-face-background hl-line-face "#1c1f26")
(set-face-attribute 'mode-line nil  :height 360)

;; List recently opened files.
(recentf-mode 1)


;; Revert buffers when the underlying file has changed.
(global-auto-revert-mode 1)


;; Save history going back 25 commands.
;; Use M-p to get previous command used in the minibuffer.
;; Use M-n to move to next command.
(setq history-length 25)
(savehist-mode 1)


;; Save place in a file.
(save-place-mode 1)




;; These settings enables using the same configuration file on multiple platforms.
;; Note that windows-nt includes [[https://www.gnu.org/software/emacs/manual/html_node/elisp/System-Environment.html][windows 10]].
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-cygwin* (eq system-type 'cygwin))
(defconst *is-unix* (not *is-windows*))


;; See this [[http://ergoemacs.org/emacs/emacs_hyper_super_keys.html][ for more information.]]
;; set keys for Apple keyboard, for emacs in OS X 
;; Source http://xahlee.info/emacs/emacs/emacs_hyper_super_keys.html
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make option key do Super. 
(setq mac-control-modifier 'control) ; make Control key do Control
(setq mac-function-modifier 'hyper)  ; make Fn key do Hyper. Only works on Apple produced keyboards.  
(setq mac-right-command-modifier 'hyper)


;; Switch to previous buffer
(define-key global-map (kbd "s-<left>") 'previous-buffer)
;; Switch to next buffer
(define-key global-map (kbd "s-<right>") 'next-buffer)


;; Minibuffer history keybindings
;; The calling up of a previously issued command in the minibuffer with ~M-p~ saves times.
(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)
(define-key minibuffer-local-map (kbd "M-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-complete-history-element)
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)


;; Bibtex configuration
(defconst blaine/bib-libraries (list "/Users/blaine/Documents/global.bib")) 

;; Combined with emacs-mac, this gives good PDF quality for [[https://www.aidanscannell.com/post/setting-up-an-emacs-playground-on-mac/][retina display]].
(setq pdf-view-use-scaling t)


;; PDF default page width behavior
(setq-default pdf-view-display-size 'fit-page)


;; Set delay in the matching parenthesis to zero.
(setq show-paren-delay 0)
(show-paren-mode t)


;; rainbow-delimiters
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
            :foreground "magenta"
            :inherit 'error
            :box t)


;; ############################## Package Configurations ################################

;;## A

;;### auto-complete
;; do default config for auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)



;;;### auto-complete-auctex.el --- auto-completion for auctex

;; Copyright (C) 2012 Christopher Monsanto
     
;; Author: Christopher Monsanto <chris@monsan.to>
;; Version: 1.0
;; Package-Requires: ((yasnippet "0.6.1") (auto-complete "1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; You can install this by (require 'auto-complete-auctex).
;; Feel free to contribute better documentation!

;;;#### Code:

(require 'tex)
(require 'latex)

(eval-when-compile
  (require 'auto-complete)
  (require 'yasnippet))

(defvar ac-auctex-arg-lookup-table
  '((TeX-arg-define-macro . ("\\MacroName"))
    (TeX-arg-counter . ("Counter"))
    (TeX-arg-define-counter . ("\\CounterName"))
    (TeX-arg-file . ("Filename"))
    (TeX-arg-bibliography . ("Filename"))
    (TeX-arg-bibstyle . ("Style"))
    (TeX-arg-environment . ("Environment"))
    (TeX-arg-define-environment . ("EnvironmentName"))
    (TeX-arg-size . ("(w, h)"))
    (TeX-arg-ref . ("Name"))
    (TeX-arg-index . ("Index"))
    (TeX-arg-define-label . ("Label"))
    (LaTeX-arg-usepackage . (["opt1,..."] "Package"))
    (LaTeX-env-label . nil)
    (LaTeX-amsmath-env-aligned . (["htbp!"]))
    (LaTeX-amsmath-env-alignat . (["# Columns"]))
    (LaTeX-env-array . (["bct"] "lcrpmb|"))
    (LaTeX-env-item . nil)
    (LaTeX-env-document . nil)
    (LaTeX-env-figure . (["htbp!"]))
    (LaTeX-env-contents . ("Filename"))
    (LaTeX-env-minipage . (["htbp!"] "Width"))
    (LaTeX-env-list . ("Label" "\\itemsep,\\labelsep,..."))
    (LaTeX-env-picture . ("(w, h)" "(x, y)"))
    (LaTeX-env-tabular* . ("Width" ["htbp!"] "lcrpmb|><"))
    (LaTeX-env-bib . ("WidestLabel"))
    (TeX-arg-conditional . ([""]))
    (2 . ("" ""))
    (3 . ("" "" ""))
    (4 . ("" "" "" ""))
    (5 . ("" "" "" "" ""))
    (6 . ("" "" "" "" "" ""))
    (7 . ("" "" "" "" "" "" ""))
    (8 . ("" "" "" "" "" "" "" ""))
    (9 . ("" "" "" "" "" "" "" "" "")))
  "Anything not in this table defaults to '(\"\")")

(defun ac-auctex-expand-arg-info (arg-info)
  (loop for item in arg-info
	append (cond
		((or (stringp item) (and (vectorp item) (stringp (elt item 0))))
		 (list item))
		((vectorp item)
		 (loop for item-2 in (or (assoc-default (or (car-safe (elt item 0)) (elt item 0))
							ac-auctex-arg-lookup-table 'equal) '(""))
		       collect [item-2]))
		(t
		 (or (assoc-default (or (car-safe item) item) ac-auctex-arg-lookup-table) '(""))))))

(defun ac-auctex-snippet-arg (n arg)
  (let* ((opt (vectorp arg))
	 (item (if opt (elt arg 0) arg))
	 (m (if (vectorp arg) (1+ n) n))
	 (var (format "${%s}" item)))
    (list (1+ m)
	  (if opt
	      (concat (format "${[") var "]}")
	    (concat "{" var "}")))))

;;;#### Macros
;;

(defun ac-auctex-expand-args (str env)
  (yas/expand-snippet (ac-auctex-macro-snippet (assoc-default str env))))

(defun ac-auctex-macro-snippet (arg-info)
  (let ((count 1))
    (apply 'concat (loop for item in (ac-auctex-expand-arg-info arg-info)
			 collect (destructuring-bind (n val)
				     (ac-auctex-snippet-arg count item)
				   (setq count n)
				   val)))))

(defun ac-auctex-macro-candidates ()
   (let ((comlist (if TeX-symbol-list
		      (mapcar (lambda (item)
			        (or (car-safe (car item)) (car item)))
			    TeX-symbol-list))))
    (all-completions ac-prefix comlist)))

(defun ac-auctex-macro-action ()
  (yas/expand-snippet (ac-auctex-macro-snippet (assoc-default candidate TeX-symbol-list)))) 

(ac-define-source auctex-macros
  '((init . TeX-symbol-list)
    (candidates . ac-auctex-macro-candidates)
    (action . ac-auctex-macro-action)
    (requires . 0)
    (symbol . "m")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))

;;;#### Symbols

(defun ac-auctex-symbol-candidates ()
  (all-completions ac-prefix (mapcar 'cadr LaTeX-math-default)))

(defun ac-auctex-symbol-action ()
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (if (texmathp)
      (progn
	(insert "\\" candidate)
	(yas/expand-snippet (ac-auctex-macro-snippet (assoc-default candidate TeX-symbol-list))))
    (insert "$\\" candidate "$")
    (backward-char)
    (yas/expand-snippet (ac-auctex-macro-snippet (assoc-default candidate TeX-symbol-list)))))

(defun ac-auctex-symbol-document (c)
  (let* ((cl (assoc c (mapcar 'cdr LaTeX-math-default)))
         (decode (if (nth 2 cl) (char-to-string (decode-char 'ucs (nth 2 cl))) ""))
         (st (nth 1 cl))
         (hs (if (listp st) (mapconcat 'identity st " ") st)))
    (and decode (concat hs " == " decode))))

(ac-define-source auctex-symbols
  '((init . LaTeX-math-mode)
    (candidates . ac-auctex-symbol-candidates)
    (document . ac-auctex-symbol-document)
    (action . ac-auctex-symbol-action)
    (requires . 0)
    (symbol . "s")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))


;;;#### Environments

(defvar ac-auctex-environment-prefix "beg")

(defun ac-auctex-environment-candidates ()
  (let ((envlist (mapcar (lambda (item) (concat ac-auctex-environment-prefix (car item)))
			 LaTeX-environment-list)))
    (all-completions ac-prefix envlist)))

(defun ac-auctex-environment-action ()
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (let ((candidate (substring candidate (length ac-auctex-environment-prefix))))
    (yas/expand-snippet (format "\\begin{%s}%s\n$0\n\\end{%s}"
				candidate
				(ac-auctex-macro-snippet (assoc-default candidate LaTeX-environment-list))
				candidate)))) 

(ac-define-source auctex-environments
  '((init . LaTeX-environment-list)
    (candidates . ac-auctex-environment-candidates)
    (action .  ac-auctex-environment-action)
    (requires . 0)
    (symbol . "e")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))


;;;#### Refs

(defun ac-auctex-label-candidates ()
  (all-completions ac-prefix (mapcar 'car LaTeX-label-list)))

(ac-define-source auctex-labels
  '((init . LaTeX-label-list)
    (candidates . ac-auctex-label-candidates)
    (requires . 0)
    (symbol . "r")
    (prefix . "\\\\ref{\\([^}]*\\)\\=")))


;;;#### Bibs

(defun ac-auctex-bib-candidates ()
  (all-completions ac-prefix (mapcar 'car LaTeX-bibitem-list)))

(ac-define-source auctex-bibs
  `((init . LaTeX-bibitem-list)
    (candidates . ac-auctex-bib-candidates)
    (requires . 0)
    (symbol . "b")
    (prefix . ,(concat "\\\\cite"
		       "\\(?:"
		         "\\[[^]]*\\]"
		       "\\)?"
		       "{\\([^},]*\\)"
		       "\\="))))

;;;#### Setup

(defun ac-auctex-setup ()
  (setq ac-sources (append
                      '(ac-source-auctex-symbols
                        ac-source-auctex-macros
			ac-source-auctex-environments
			ac-source-auctex-labels
			ac-source-auctex-bibs)
                      ac-sources)))

(add-to-list 'ac-modes 'latex-mode)
(add-hook 'LaTeX-mode-hook 'ac-auctex-setup)

(provide 'auto-complete-auctex)

;;; auto-complete-auctex.el ends here

;; indent with spaces instead of tabs for pep8 compatibility
(setq tab-width 4)
(setq-default indent-tabs-mode nil)



;;## B


;;### bibtex-mode related
;; Fetch bibtex for the given DOI. Insert at point, which should be in your global.bib file.
;; Needs code to reformat the bibtex key.
;;
;; https://www.anghyflawn.net/blog/2014/emacs-give-a-doi-get-a-bibtex-entry/

(defun get-bibtex-from-doi (doi)
 "Get a BibTeX entry from the DOI"
 (interactive "MDOI: ")
 (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
   (with-current-buffer 
     (url-retrieve-synchronously 
       (format "http://dx.doi.org/%s" 
       	(replace-regexp-in-string "http://dx.doi.org/" "" doi)))
     (switch-to-buffer (current-buffer))
     (goto-char (point-max))
     (setq bibtex-entry 
     	  (buffer-substring 
          	(string-match "@" (buffer-string))
              (point)))
     (kill-buffer (current-buffer))))
 (insert (decode-coding-string bibtex-entry 'utf-8))
 (define-key bibtex-mode-map (kbd "C-c b") 'get-bibtex-from-doi)
 (bibtex-fill-entry))
;; I want run the above function to define it upon entry into a Bibtex file. 
(add-hook
   'bibtex-mode-hook
   (lambda ()
       (get-bibtex-from-doi nil)))

;; Hook to add imenu to menubar in bibtex mode
;; http://www.jonathanleroux.org/bibtex-mode.html
(add-hook
  'bibtex-mode-hook
  (lambda ()
    (imenu-add-to-menubar "Imenu")))


;;## Dired related
(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))


;;## E

;;### ef-theme

(add-to-list 'load-path "~/latex-emacs/manual-packages/ef-themes")

(require 'ef-themes)
;; If you like two specific themes and want to switch between them, you
;; can specify them in `ef-themes-to-toggle' and then invoke the command
;; `ef-themes-toggle'.  All the themes are included in the variable
;; `ef-themes-collection'.
(setq ef-themes-to-toggle '(ef-summer ef-winter))

;; Make customisations that affect Emacs faces BEFORE loading a theme
;; (any change needs a theme re-load to take effect).

(setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 . (variable-pitch light 1.9))
        (1 . (variable-pitch light 1.8))
        (2 . (variable-pitch regular 1.7))
        (3 . (variable-pitch regular 1.6))
        (4 . (variable-pitch regular 1.5))
        (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
        (6 . (variable-pitch 1.3))
        (7 . (variable-pitch 1.2))
        (t . (variable-pitch 1.1))))

;; They are nil by default...
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the theme of choice:
(load-theme 'ef-spring :no-confirm)

;; The themes we provide:
;;
;; Light: `ef-day', `ef-light', `ef-spring', `ef-summer'.
;; Dark:  `ef-autumn', `ef-dark', `ef-night', `ef-winter'.
;;
;; Also those which are optimized for deuteranopia (red-green color
;; deficiency): `ef-deuteranopia-dark', `ef-deuteranopia-light'.

;; We also provide these commands, but do not assign them to any key:
;;
;; - `ef-themes-toggle'
;; - `ef-themes-select'
;; - `ef-themes-load-random'
;; - `ef-themes-preview-colors'
;; - `ef-themes-preview-colors-current'#+END_SRC


;; atomic-chrome, used to interact with GhostText extension for Google Chrome.
(use-package atomic-chrome)
(atomic-chrome-start-server)
(setq atomic-chrome-default-major-mode 'python-mode)
(setq atomic-chrome-extension-type-list '(ghost-text))
;;(atomic-chrome-start-httpd)
(setq atomic-chrome-server-ghost-text-port 4001)
(setq atomic-chrome-url-major-mode-alist
      '(("github\\.com" . gfm-mode)
        ("overleaf.com" . latex-mode)
        ("750words.com" . latex-mode)))
; Select the style of opening the editing buffer by atomic-chrome-buffer-open-style.
; full: Open in the selected window.
; split: Open in the new window by splitting the selected window (default).
; frame: Create a new frame and window in it. Must be using some windowing pacakge.
(setq atomic-chrome-buffer-open-style 'split)


;; awesome-tab
(use-package awesome-tab
  :load-path "~/.emacs.default/elisp/awesome-tab"
  :config
  (awesome-tab-mode t))


;; *** Electric-pair mode. Add matching pairs of quotes and parentheses.
(electric-pair-mode)


;; *** electric-spacing
;; An emacs minor-mode to automatically add spacing around [[https://github.com/xwl/electric-spacing][operators] in math expressions.].
;; Backspace over the whitespaces to remove them when none are permitted.
(use-package electric-spacing
      :ensure t)

;; git clone https://github.com/walmes/electric-spacing.git into .emacs.default/elisp
;; byte-compile with (byte-compile-file "~/ess-emacs/elisp/electric-spacing/electric-spacing.el")
;; byte-compile with (byte-compile-file "~/ess-emacs/elisp/electric-spacing/electric-spacing-r.el")
(add-to-list 'load-path "~/ess-emacs/elisp/electric-spacing")
(require 'electric-spacing-r)
(add-hook 'ess-mode-hook #'electric-spacing-mode)
;; restrict to limited number of modes to keep it out of the minibuffer
(defvar my-electic-pair-modes '(python-mode julia-mode org-mode latex-mode))
(defun my-inhibit-electric-pair-mode (char)
  (not (member major-mode my-electic-pair-modes)))
(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)


;;## I

;;### ivy
(use-package ivy
 :ensure t
 :init
 (ivy-mode 1)
 (global-set-key "\C-s" 'swiper)
 (unbind-key "S-SPC" ivy-minibuffer-map)
 (setq ivy-height 15
       ivy-use-virtual-buffers t
       ivy-count-format "(%d/%d) "
       ivy-use-selectable-prompt t))
(use-package ivy-bibtex
   :ensure t)



;;## L


;;### LanguageTool

;; I downloaded Language Tool and installed it in ~/.languagetool.
;; source: https://github.com/PillFall/languagetool.el


(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
        languagetool-server-command "~/.languagetool/languagetool-server.jar"))

(setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"))

(setq languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
      languagetool-server-command "~/.languagetool/languagetool-server.jar")

;;### LaTeX helpher functions
;;#### M-x description
;; Converts a selected list into a description list.
;; The elements of the list must begin with a dash.
;; The terms to be inserted into the square brackets
;; have to be added after running the function.
(defun description (beg end) 
 "wrap the active region in an 'itemize' environment,
  converting hyphens at the beginning of a line to \item"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (beginning-of-buffer)
    (insert "\\begin{description}\n")
    (while (re-search-forward "^- " nil t)
      (replace-match "\\\\item[ ]"))
    (end-of-buffer)
    (insert "\\end{description}\n")))


;;#### M-x enumerate
;; Converts a selected list into an enumerated list.
;; The elements of the list must begin with a dash.
(defun enumerate (beg end) 
 "wrap the active region in an 'itemize' environment,
  converting hyphens at the beginning of a line to \item"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (beginning-of-buffer)
    (insert "\\begin{enumerate}\n")
    (while (re-search-forward "^- " nil t)
      (replace-match "\\\\item "))
    (end-of-buffer)
    (insert "\\end{enumerate}\n")))


;;#### M-x itemize
;; Converts a selected list into an itemized list.
;; The elements of the list must begin with a dash.
;; A similar function could be made to make an enumerated list
;; and a description list.
;; Source: \url{https://tex.stackexchange.com/questions/118958/emacsauctex-prevent-region-filling-when-inserting-itemize}
(defun itemize (beg end) 
 "wrap the active region in an 'itemize' environment,
  converting hyphens at the beginning of a line to \item"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (beginning-of-buffer)
    (insert "\\begin{itemize}\n")
    (while (re-search-forward "^- " nil t)
      (replace-match "\\\\item "))
    (end-of-buffer)
    (insert "\\end{itemize}\n")))


;;#### LaTeX related
(unless (package-installed-p `auctex) (package-install `auctex))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq doc-view-continuous t) ;; scroll over all pages in doc view 

;; Settings for minted package issue
(eval-after-load "tex" 
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
    )
  )

; Outline-minor-mode key map Source: https://www.emacswiki.org/emacs/OutlineMinorMode
(define-prefix-command 'cm-map nil "Outline-")
; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key "\M-o" cm-map)



;;## M

;;### Configured for GitHub Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc -t html5"))
;;Install simple-httpd and impatient-mode packages.
(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-port 7070)
  (setq httpd-host (system-name)))
;; The impatient-mode package takes the content of your buffer, passes it through a filter, and serves the result via simple-httpd HTTP server.
(use-package impatient-mode
  :ensure t
  :commands impatient-mode)
;; Create a filter function to process the Markdown buffer. 
;; The function my-markdown-filter uses github-markdown-css to mimic the look of GitHub.
(defun my-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))
;; Create the function my-markdown-preview to show the preview. 
(defun my-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))
;; Run my-markdown-preview in any Markdown buffer.
;; It will open a new window in your browser and update it as you type.


; ;; Let's try an org-mode previewer
; (require 'org-mode
;   :mode ("\\.org\\'" . gfm-mode)
;   :commands (org-mode gfm-mode)
;   :config
;   (setq org-command "pandoc -t html5"))
;
;   (defun my-org-filter (buffer)
;     (princ
;      (with-temp-buffer
;        (let ((tmp (buffer-name)))
;          (set-buffer buffer)
;          (set-buffer (org tmp))
;          (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
;   <body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
;      (current-buffer)))
;   ;; Create the function my-org-preview to show the preview.
;
; (defun my-org-preview ()
;   "Preview org."
;   (interactive)
;   (unless (process-status "httpd")
;     (httpd-start))
;   (impatient-mode)
;   (imp-set-user-filter 'my-og-filter)
;   (imp-visit-buffer))


;;### Move selected regions up or down
;; It is commands like these one that enable rapid reorganization of your prose when writing one sentence per row.
;; Thank you to DivineDomain for the suggested upgrade.
;; Source: https://www.emacswiki.org/emacs/MoveText 
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-line-region-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-line-region-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "M-<down>") 'move-line-region-down)
(global-set-key (kbd "M-<up>") 'move-line-region-up)



;; ;;### Move lines up an down
;; It is commands like these one that enable rapid reorganization of your prose when writing one sentence per row.
;; Retained for those who have not mastered regions.
;; (defun move-line (n)
;;   "Move the current line up or down by N lines."
;;   (interactive "p")
;;   (setq col (current-column))
;;   (beginning-of-line) (setq start (point))
;;   (end-of-line) (forward-char) (setq end (point))
;;   (let ((line-text (delete-and-extract-region start end)))
;;     (forward-line n)
;;     (insert line-text)
;;     ;; restore point to original column in moved line
;;     (forward-line -1)
;;     (forward-char col)))

;; (defun move-line-up (n)
;;   "Move the current line up by N lines."
;;   (interactive "p")
;;   (move-line (if (null n) -1 (- n))))

;; (defun move-line-down (n)
;;   "Move the current line down by N lines."
;;   (interactive "p")
;;   (move-line (if (null n) 1 n)))

;; (global-set-key (kbd "M-<up>") 'move-line-up)
;; (global-set-key (kbd "M-<down>") 'move-line-down)


;;## O

;; <<<<<<< BEGINNING of org-agenda >>>>>>>>>>>>>>
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; org-capture
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

(setq org-agenda-files '("/Users/blaine/gtd/tasks/JournalArticles.org"
 "/Users/blaine/gtd/tasks/Proposals.org"
 "/Users/blaine/gtd/tasks/Books.org"
 "/Users/blaine/gtd/tasks/Talks.org"
 "/Users/blaine/gtd/tasks/Posters.org"
 "/Users/blaine/gtd/tasks/ManuscriptReviews.org"
 "/Users/blaine/gtd/tasks/Private.org"
 "/Users/blaine/gtd/tasks/Service.org"
 "/Users/blaine/gtd/tasks/Teaching.org"
 "/Users/blaine/gtd/tasks/Workshops.org"
 "/Users/blaine/gtd/tasks/grasscatchers.org"))

;; Cycle through these keywords with shift right or left arrows.
(setq org-todo-keywords
        '((sequence "TODO(t)" "INITIATED(i!)" "WAITING(w!)" "CAL(a)" "SOMEDAY(s!)" "PROJ(j)" "|" "DONE(d!)" "CANCELLED(c!)")))

(setq org-refile-targets '(("/Users/blaine/gtd/tasks/JournalArticles.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/Proposals.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/Books.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/Talks.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/Posters.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/ManuscriptReviews.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/Private.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/Service.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/Teaching.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/grasscatcer.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/Workshops.org" :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)

;; ***** customized agenda views
;; 
;; These are my customized agenda views by project.
;; The letter is the last parameter.
;; For example, enter ~C-c a b~ and then enter 402 at the prompt to list all active tasks related to 402 tasks.
;; 
;; I learned about this approach [[https://tlestang.github.io/blog/keeping-track-of-tasks-and-projects-using-emacs-and-org-mode.html][here]].
;; 
;; The CATEGORY keyword resides inside of a Properties drawer.
;; The drawers are usually closed.
;; I am having trouble opening my drawers in may org files.
;; In addition, I do not want to have to add a drawer to each TODO.
;; 
;; I am loving Tags now.
;; I may switch to using Tags because they are visible in org files.
;; I tried and they are not leading to the expect list of TODOs in org-agenda.
;; I am stumped.
;; 
;; In the meantime, enter ~C-c \~ inside JournalArticles.org to narrow the focus to the list of TODOs or enter ~C-c i b~ to get an indirect buffer.
;; 

(setq org-agenda-custom-commands
      '(
	("b"
             "List of all active 402 tasks."
             tags-todo
             "402\"/TODO|INITIATED|WAITING")
	("c"
             "List of all active 523 RNA-drug crystallization review paper tasks."
             tags-todo
             "CATEGORY=\"523\"/TODO|INITIATED|WAITING")
	("d"
             "List of all active 485PyMOLscGUI tasks."
             tags-todo
             "CATEGORY=\"485\"/TODO|INITIATED|WAITING")
	("e"
             "List of all active 2104 Emacs tasks"
             tags-todo
             "2104+CATEGORY=\"2104\"/NEXT|TODO|INITIATED|WAITING")
	("n"
             "List of all active 651 ENAX2 tasks"
             tags-todo
             "651+CATEGORY=\"651\"/NEXT|TODO|INITIATED|WAITING")
	("q"
             "List of all active 561 charge density review"
             tags
             "561+CATEGORY=\"211\"/NEXT|TODO|INITIATED|WAITING")
	("r"
             "List of all active 211 rcl/dnph tasks"
             tags-todo
             "211+CATEGORY=\"211\"/NEXT|TODO|INITIATED|WAITING")
	("P"
         "List of all projects"
         tags
         "LEVEL=2/PROJ")))


;; I usually know the project to which I want to assign a task.
;; I loathe having to come back latter to refile my tasks.
;; I want to do the filing at the time of capture.
;; I found a solution [[https://stackoverflow.com/questions/9005843/interactively-enter-headline-under-which-to-place-an-entry-using-capture][here]].
;; 
;; A project has two or more tasks.
;; I believe that the 10,000 projects is the upper limit for a 30 year academic career.
;; There are about 10,000 workdays in a 30 year career if you work six days a week.
;; Of course, most academics work seven a week and many work longer than 30 years, some even reach 60 years.
;; 
;; I have my projects split into ten org files.
;; Each org file has a limit of 1000 projects for ease of scrolling.
;; 
;; It is best to let Emacs insert new task because it is easy to accidently delete sectons in an org file, especially when sections are folded.
;; (I know that many love folded sections.
;; There is a strong appeal to being able to collapse secitons of text.
;; However, folded section are not for me; I have experienced too many catastrophes.
;; I open all of my org files with all sections fully open.
;; I can use swiper to navigate if I do not want to scroll.)
;; Enter ~C-c c~ to start the capture menu.
;; The settings below show a single letter option for selecting the appropriate org-file.
;; After entering the single-letter code, you are prompted for the headline name.
;; You do not have to include the TODO keyword.
;; However, I changed "Headline" to "Tag" because I have the project ID was one of the tags on the same line as the project headline.
;; I am now prompted for the tag.
;; After entering the tag, I fill out the task entry.
;; I then enter ~C-c C-c~ to save the capture.
;; 
;;This protocol can be executed from inside the target org file or from a different buffer.
;;
;;I learned about the following function, which I modified by changing "Headline " to "Tag", from
;;[[https://stackoverflow.com/questions/9005843/interactively-enter-headline-under-which-to-place-an-entry-using-capture][Lionel Henry]] with the modification by Phil on July 1, 2018.
;;
(defun org-ask-location ()
  (let* ((org-refile-targets '((nil :maxlevel . 9)))
         (hd (condition-case nil
                 (car (org-refile-get-location "Tag" nil t))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n")))
  (end-of-line))

(setq org-capture-templates
 '(
   ("j" "JournalArticles" entry
    (file+function "/Users/blaine/gtd/tasks/JournalArticles.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("g" "GrantProposals" entry
    (file+function "/Users/blaine/gtd/tasks/Proposals.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("b" "Books" entry
    (file+function "/Users/blaine/gtd/tasks/Books.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("t" "Talks" entry
    (file+function "/Users/blaine/gtd/tasks/Talks.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("p" "Posters" entry
    (file+function "/Users/blaine/gtd/tasks/Posters.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("r" "ManuscriptReviews" entry
    (file+function "/Users/blaine/gtd/tasks/ManuscriptReviews.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("v" "Private" entry
    (file+function "/Users/blaine/gtd/tasks/Private.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("S" "Service" entry
    (file+function "/Users/blaine/gtd/tasks/Service.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("T" "Teaching" entry
    (file+function "/Users/blaine/gtd/tasks/Teaching.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("w" "Workshop" entry
    (file+function "/Users/blaine/gtd/tasks/Workshops.org" org-ask-location)
    "\n\n*** TODO %?\n<%<%Y-%m-%d %a %T>>"
    :empty-lines 1)
    ("s" "Slipbox" entry  (file "/User/org-roam/inbox.org")
           "* %?\n")
    ))


(defun jethro/org-capture-slipbox ()
    (interactive)
    (org-capture nil "s"))


;; <<<<<<< END of org-agenda >>>>>>>>>>>>>>


;; org-drill for spaced repetition learning in org-mode
;; You have to install org-drill from MELPA.;; This is a YouTube video about how to use org-drill[[https://www.youtube.com/watch?v=uraPXeLfWcM][to learn Chinese]].
;;You can use org tables to generate [[https://github.com/chrisbarrett/org-drill-table][flashcards]].
(use-package org-drill
             :ensure t
             :config (progn
                          (add-to-list 'org-modules 'org-drill)
                          (setq org-drill-add-random-noise-to-intervals-p t)
                          (setq org-drill-hint-separator "||")
                          (setq org-drill-left-cloze-delimiter "<[")
                          (setq org-drill-right-cloze-delimiter "]>")
                          (setq org-drill-learn-fraction 0.25)
             )
)


(use-package org-pomodoro
    :ensure t
    :commands (org-pomodoro)
    :config
    (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))

;; (use-package sound-wav)
;; (setq org-pomodoro-ticking-sound-p nil)
;; ; (setq org-pomodoro-ticking-sound-states '(:pomodoro :short-break :long-break))
;; (setq org-pomodoro-ticking-sound-states '(:pomodoro))
;; (setq org-pomodoro-ticking-frequency 1)
;; (setq org-pomodoro-audio-player "mplayer")
;; (setq org-pomodoro-finished-sound-args "-volume 0.9")
;; (setq org-pomodoro-long-break-sound-args "-volume 0.9")
;; (setq org-pomodoro-short-break-sound-args "-volume 0.9")
;; (setq org-pomodoro-ticking-sound-args "-volume 0.3")

(global-set-key (kbd "C-c o") 'org-pomodoro)



;; <<<<<<< BEGIN org-ref >>>>>>>>>>>>>>
(require 'org-ref)
(require 'org-ref-ivy)
(require 'bibtex)

(setq bibtex-completion-bibliography '("/Users/blaine/Documents/global.bib")
	bibtex-completion-library-path '("/Users/blaine/0papersLabeled/")
	bibtex-completion-notes-path "/Users/blaine/Documents/notes/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil fpath)))
      
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

;; H is the hyper key. I have bound H to Fn. For the MacAlly keyboard, it is bound to right-command.
(define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
(define-key org-mode-map (kbd "H-c") org-ref-insert-cite-function)
(define-key org-mode-map (kbd "H-r") org-ref-insert-ref-function)
(define-key org-mode-map (kbd "H-l") org-ref-insert-label-function)
(define-key org-mode-map (kbd "H-d") 'doi-add-bibtex-entry)


;; <<<<<<< END org-ref >>>>>>>>>>>>>>




;; <<<<<<< BEGIN org-roam >>>>>>>>>>>>>>

;;## Basic org-roam config
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/Users/blaine/org-roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))



;;## Basic org-roam config
;; Following https://jethrokuan.github.io/org-roam-guide/

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))


(setq org-roam-node-display-template
    (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))


(defun jethro/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))


(defun jethro/tag-new-node-as-draft ()
  (org-roam-tag-add '("draft")))
(add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)


;; ;;### Create the property “type” on my nodes.
;; 
;; (cl-defmethod org-roam-node-type ((node org-roam-node))
;;   "Return the TYPE of NODE."
;;   (condition-case nil
;;       (file-name-nondirectory
;;        (directory-file-name
;;         (file-name-directory
;;          (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;     (error "")))


;; (setq org-roam-node-display-template
;;           (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))




;;### org-roam-bibtex config

(use-package org-roam-bibtex
      :hook (org-roam-mode . org-roam-bibtex-mode))

    (setq orb-preformat-keywords
          '("citekey" "title" "url" "author-or-editor" "keywords" "file")
          orb-process-file-keyword t
          orb-file-field-extensions '("pdf"))

    (setq orb-templates
          '(("r" "ref" plain(function org-roam-capture--get-point)
             ""
             :file-name "${citekey}"
             :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}
  - tags ::
  - keywords :: ${keywords}

  *Notes
  :PROPERTIES:
  :Custom_ID: ${citekey}
  :URL: ${url}
  :AUTHOR: ${author-or-editor}
  :NOTER_DOCUMENT: ${file}
  :NOTER_PAGE:
  :END:")))

;; <<<<<<< END org-roam >>>>>>>>>>>>>>





;;## P

;;###  Move to cursor to previously visited window
;; From the book Writing GNU Emacs Extensions by Bill Glickstein.
(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key "\C-xp" 'other-window-backward)


;;###  pdb-tools

;;Marcin Magnus's updated fork of pdb-tools by Charlie Bond and David Love.
;;[[https://github.com/mmagnus/emacs-pdb-mode][Gitub repo]]]


;; pdb.el
(load-file "~/.emacs.default/plugins/emacs-pdb-mode/pdb-mode.el")
(setq pdb-rasmol-name "/Applications/PyMOL.app/Contents/bin/pymol")
(setq auto-mode-alist
     (cons (cons "pdb$" 'pdb-mode)
           auto-mode-alist ) )
(autoload 'pdb-mode "PDB")




;;### pdf-tools

;; This is an alternative to the built-in DocView package.
;; I allows smooth scrolling and it superior in general.
;; I could load several PDFs, including 500 pages books.
;; 
;; 
;; The pdf-tools package runs on top of pdf-view package.
;; This making capturing text from PDFs much easier.
;; 
;; I followed a [[http://pragmaticemacs.com/emacs/view-and-annotate-pdfs-in-emacs-with-pdf-tools][blog post]].
;; You enter highlights by selecting with the mouse and entering C-c C-a h.
;; An annotation menu opens in the minibuffer.
;; Enter ~C-c C-c~ to save the annotation.
;; Enter ~C-c C-a t~ to enter text notes.
;; Enter the note and enter ~C-c C-c~ to save.
;; Right-click the mouse to get a menu of more options.


(use-package pdf-tools
  ;;:pin manual ;; manually update
  :config
  ;; initialise
  (pdf-tools-install)

  ;; This means that pdfs are fitted to width by default when you open them
  (setq-default pdf-view-display-size 'fit-width)
  ;; open pdfs scaled to fit page
  ;;  (setq-default pdf-view-display-size 'fit-page)
   ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
  ;; Setting for sharper images with Macs with Retina displays
  (setq pdf-view-use-scaling t)



;; **** Useful keybindings for viewing PDFs
;; |------------------------------------------+-----------------|
;; | Display                                  |                 |
;; |------------------------------------------+-----------------|
;; | Zoom in / Zoom out                       | ~+~ / ~-~       |
;; | Fit height / Fit width / Fit page        | ~H~ / ~W~ / ~P~ |
;; | Trim margins (set slice to bounding box) | ~s b~           |
;; | Reset margins                            | ~s r~           |
;; | Reset z oom                              | ~0~             |
;; |------------------------------------------+-----------------|
;; 
;; **** Useful keybindings for navigating PDFs
;; 
;; |-----------------------------------------------+-----------------------|
;; | Navigation                                    |                       |
;; |-----------------------------------------------+-----------------------|
;; | Scroll Up / Down by Page-full                 | ~space~ / ~backspace~ |
;; | Scroll Up / Down by Line                      | ~C-n~ / ~C-p~         |
;; | Scroll Right / Left                           | ~C-f~ / ~C-b~         |
;; | First Page / Last Page                        | ~<~ / ~>~             |
;; | Next Page / Previous Page                     | ~n~ / ~p~             |
;; | First Page / Last Page                        | ~M-<~ / ~M->~         |
;; | Incremental Search Forward / Backward         | ~C-s~ / ~C-r~         |
;; | Occur (list all lines containing a phrase)    | ~M-s o~               |
;; | Jump to Occur Line                            | ~RETURN~              |
;; | Pick a Link and Jump                          | ~F~                   |
;; | Incremental Search in Links                   | ~f~                   |
;; | History Back / Forwards                       | ~l~ / ~r~             |
;; | Display Outline                               | ~o~                   |
;; | Jump to Section from Outline                  | ~RETURN~              |
;; | Jump to Page                                  | ~M-g g~               |
;; | Store position / Jump to position in register | ~m~ / ~'~             |
;; |-----------------------------------------------+-----------------------|
;; 


;;## S


;;### Straight
;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;### pdf-drop-mode
(straight-use-package
  '(pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode"))


(require 'pdf-drop-mode)

(defun my/pdf-process (file doi)
  (message "%s : %s" file doi))

(setq pdf-drop-search-hook #'my/pdf-process)
(pdf-drop-mode)






;;### Swiper related confg from https://github.com/zamansky/using-emacs/blob/lesson-6-swiper/init.el
(use-package try
	:ensure t)

(use-package which-key
	:ensure t 
	:config
	(which-key-mode))


;; Org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defalias 'list-buffers 'ibuffer)
;; (defalias 'list-buffers 'ibuffer-other-window)

; If you like a tabbar 
;(use-package tabbar
;  :ensure t
;  :config
;  (tabbar-mode 1))


(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
    ))


;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))



;;## T
;;### TeXcount setup for TeXcount version 2.3 and later
;; See https://app.uio.no/ifi/texcount/howto.html to use from the command line.
(defun texcount ()
  (interactive)
  (let*
    ( (this-file (buffer-file-name))
      (enc-str (symbol-name buffer-file-coding-system))
      (enc-opt
        (cond
          ((string-match "utf-8" enc-str) "-utf8")
          ((string-match "latin" enc-str) "-latin1")
          ("-encoding=guess")
      ) )
      (word-count
        (with-output-to-string
          (with-current-buffer standard-output
            (call-process "/opt/local/bin/texcount" nil t nil "-0" enc-opt this-file)
    ) ) ) )
    (message word-count)
) )
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map "\C-cw" 'texcount)))
(add-hook 'latex-mode-hook (lambda () (define-key latex-mode-map "\C-cw" 'texcount)))



;;### activate word count mode
;; This mode will count the LaTeX markup, but it does give the count of incrementally added words.
(use-package wc-mode)
(add-hook 'text-mode-hook 'wc-mode)
;; Suggested setting
(global-set-key "\C-cw" 'wc-mode)

;;### Do NOT MESS with the code below
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-menu-height 15)
 '(ivy-height 20)
 '(org-agenda-files
   '("~/430PSADwaveOpt/430gh/main.org" "/Users/blaine/gtd/tasks/JournalArticles.org" "/Users/blaine/gtd/tasks/Proposals.org" "/Users/blaine/gtd/tasks/Books.org" "/Users/blaine/gtd/tasks/Talks.org" "/Users/blaine/gtd/tasks/Posters.org" "/Users/blaine/gtd/tasks/ManuscriptReviews.org" "/Users/blaine/gtd/tasks/Private.org" "/Users/blaine/gtd/tasks/Service.org" "/Users/blaine/gtd/tasks/Teaching.org" "/Users/blaine/gtd/tasks/Workshops.org"))
 '(package-selected-packages
   '(counsel ace-window org-bullets which-key try languagetool org-ref org-pomodoro pdf-tools dirvish dired-icon 0xc org-pdftools multiple-cursors 0blayout dired-subtree org-roam-timestamps org-roam-bibtex org-roam-ui org-roam org-preview-html impatient-mode ef-themes yasnippet wc-mode use-package rainbow-delimiters powerline maxframe material-theme exec-path-from-shell electric-spacing better-defaults auto-complete auctex atomic-chrome))
 '(warning-suppress-log-types '(((tar link)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))


 ;;### Tramp
 ;; Set default connection mode to SSH
 (setq tramp-default-method "ssh")


;;## Y 
;;### yasnippet related
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
(global-set-key "\C-o" 'yas-expand)
(use-package popup
  :ensure t)
(require 'yasnippet)
;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))
(setq yas/prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))

(defun complete-if-yas-field (&rest _)
  (let ((field (yas-current-field)))
    (when (and field
               (not (yas--field-modified-p field)))
      (company-manual-begin))))

(advice-add 'company-complete-selection :after 'complete-if-yas-field)
(advice-add 'yas-next-field :after 'complete-if-yas-field)


