;;; inf-lua.el --- Run a Lua repl in an inferior process -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/inf-lua
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Created:  9 December 2023
;; Keywords: lua repl languages

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Inf-lua provides a REPL buffer for interacting with a Lua process.
;;
;; Features:
;;  - completion-at-point: For globals, methods, functions, table keys, and
;;    filenames in strings.
;;  - font-locking using `lua-ts-mode' or `lua-mode' for input
;;  - compilation errors
;;
;; TODO:
;;  - Add filename, line number info to regions loaded into repl
;;    (`lua-send-region') so tracebacks work
;;  - setup debugger tracking
;;    possible to switch into `gud-lua' somehow?
;;    or something like `python-pdbtrack-setup-tracking'
;;  - switch to inf-lua during compilation when execution enters interactive
;;    mode - `inf-ruby-switch-from-compilation'
;;
;;; Code:

(require 'lua-ts-mode)                  ; syntax-table
(require 'comint)

(defgroup inf-lua nil
  "Run Lua process in a buffer."
  :group 'languages
  :prefix "inf-lua-")

(defcustom inf-lua-command "lua"
  "Command to run inferior Lua process."
  :type 'string
  :risky t)

(defcustom inf-lua-arguments '("-i")
  "Command line arguments for `inf-lua-command'."
  :type '(repeat string))

(defcustom inf-lua-buffer-name "Lua"
  "Default buffer name for the Lua interpreter."
  :type 'string
  :safe 'stringp)

(defcustom inf-lua-prompt "> "
  "Regexp matching the top-level prompt used by the inferior Lua process."
  :type 'regexp
  :safe 'stringp)

(defcustom inf-lua-prompt-continue ">> "
  "Regexp matching the continuation prompt used by the inferior Lua process."
  :type 'regexp
  :safe 'stringp)

(defcustom inf-lua-debug-prompt (rx (or "lua_debug" "debugger.lua") "> ")
  "Regexp matching debugger prompts used by the inferior Lua process."
  :type 'regexp
  :safe 'stringp)

(defcustom inf-lua-history-filename nil
  "File used to save command history of the inferior Lua process."
  :type '(choice (const :tag "None" nil) file)
  :safe 'string-or-null-p)

(defcustom inf-lua-startfile nil
  "File to load into the inferior Lua process at startup."
  :type '(choice (const :tag "None" nil) (file :must-match t)))

(defcustom inf-lua-font-lock-enable t
  "Non-nil to enable font-locking in the repl buffer."
  :type 'boolean)

(defcustom inf-lua-completion-enabled t
  "Enable/disable inferior lua completion at point."
  :type 'boolean)


(defvar inf-lua--internal-error-re
  (rx (or "[string \"REPL\"]" "stdin") ":" (+ digit) ":")
  "Regexp to match useless internal repl error locations.
For rep.lua, \\='[string \"REPL\"]:*:', and Lua \\='stdin:1:'.")

(defvar inf-lua-repl-compilation-regexp-alist
  `(;; debugger.lua
    ("^[>]?\\s-*break via dbg.+=> \\([^:]+\\):\\([0-9]+\\)" 1 2)
    ("^\\s-*[0-9]+\\(?: =>\\)?\\s-*\\([^:]+\\):\\([0-9]+\\)" 1 2)
    ;; first line of traceback
    (,(rx-to-string
       `(seq bol (or (regexp ,inf-lua--internal-error-re) "Error:")
             (regexp " \\([^:]+\\):\\([0-9]+\\):")))
     1 2)
    ;; `lua-traceback-line-re'
    ("^\\(?:[\t ]*\\|.*>[\t ]+\\)\\(?:[^\n\t ]+:[0-9]+:[\t ]*\\)*\\(?:\\([^\n\t ]+\\):\\([0-9]+\\):\\)" 1 2)))


(defun inf-lua-calculate-command (&optional prompt default)
  "Calculate command to start repl.
If PROMPT is non-nil, read command interactively using DEFAULT if non-nil."
  (unless default
    (setq default (concat inf-lua-command " "
                          (mapconcat 'identity inf-lua-arguments " "))))
  (if prompt (read-shell-command "Run Lua: " default) default))

(defun inf-lua-buffer ()
  "Return inferior Lua buffer for current buffer."
  (if (derived-mode-p 'inf-lua-mode)
      (current-buffer)
    (let* ((proc-name inf-lua-buffer-name)
           (buffer-name (format "*%s*" proc-name)))
      (when (comint-check-proc buffer-name)
        buffer-name))))

(defun inf-lua-process ()
  "Return inferior Lua process for current buffer."
  (get-buffer-process (inf-lua-buffer)))

;;;###autoload
(defun inf-lua-run (&optional prompt cmd startfile show)
  "Run a Lua interpreter in an inferior process.
With prefix, PROMPT, read command.
If CMD is non-nil, use it to start repl.
STARTFILE overrides `inf-lua-startfile' when present.
When called interactively, or with SHOW, show the repl buffer after starting."
  (interactive (list current-prefix-arg nil nil t))
  (let* ((cmd (inf-lua-calculate-command prompt cmd))
         (buffer (inf-lua-make-comint
                  cmd
                  inf-lua-buffer-name
                  (or startfile inf-lua-startfile)
                  show)))
    (get-buffer-process buffer)))

(defun inf-lua--write-history (process _)
  "Write history file for inferior Lua PROCESS."
  (when inf-lua-history-filename
    (let ((buffer (process-buffer process)))
      (when (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer (comint-write-input-ring))))))

(defun inf-lua-make-comint (cmd proc-name &optional startfile show)
  "Create a Lua comint buffer.
CMD is the Lua command to be executed and PROC-NAME is the process name
that will be given to the comint buffer.
If STARTFILE is non-nil, use that instead of `inf-lua-startfile'
which is used by default. See `make-comint' for details of STARTFILE.
If SHOW is non-nil, display the Lua comint buffer after it is created.
Returns the name of the created comint buffer."
  (let ((proc-buff-name (format "*%s*" proc-name)))
    (unless (comint-check-proc proc-buff-name)
      (let* ((cmdlist (split-string-and-unquote cmd))
             (program (car cmdlist))
             (args (cdr cmdlist))
             (buffer (apply #'make-comint-in-buffer proc-name
                            proc-buff-name
                            program
                            startfile
                            args)))
        (set-process-sentinel
         (get-buffer-process buffer) #'inf-lua--write-history)
        (with-current-buffer buffer
          (inf-lua-mode)
          (when inf-lua-completion-enabled
            (inf-lua-setup-completion)))))
    (when show
      (pop-to-buffer proc-buff-name))
    proc-buff-name))

(defvar-local inf-lua--prompt-internal nil)

(defun inf-lua--calculate-prompt-regexps ()
  "Calculate regexp to match prompt."
  (setq inf-lua--prompt-internal
        (rx-to-string `(: (or (regexp ,inf-lua-debug-prompt)
                              (regexp ,inf-lua-prompt-continue)
                              (regexp ,inf-lua-prompt))))))

(defun inf-lua--preoutput-filter (string)
  "Filter for `comint-preoutput-filter-functions' to process STRING.
Filters extra prompt characters that accumulate in the output when
sending regions to the inferior process."
  (setq string (replace-regexp-in-string
                (rx-to-string
                 `(: bol
                     (* (regexp ,inf-lua--prompt-internal))
                     (group (regexp ,inf-lua--prompt-internal)
                            (* nonl))))
                "\\1" string))
  (when (and (not (bolp))
             (string-match-p
              (concat "\\`" inf-lua--prompt-internal) string))
    ;; Stop prompts from stacking up when sending regions: > > >
    (setq string (concat "\n" string)))
  ;; Replace useless error locations
  (replace-regexp-in-string
   (concat "^" inf-lua--internal-error-re) "Error:"
   (replace-regexp-in-string
    (concat "\\(\\s-+\\)" inf-lua--internal-error-re) "\\1[REPL]:" string)))


;;; TODO(5/21/24): send string function to convert strings to literals
;; and load in Lua repl for source locations in errors (see `lua-send-region')
(defun inf-lua-string-to-literal (string)
  "Convert STRING to Lua literal."
  (concat "'" (cl-reduce
               (lambda (string rep)
                 (replace-regexp-in-string (car rep) (cdr rep) string))
               '(("[\"'\\]" . "\\\\\\&")
                 ("\t" . "\\\\t")
                 ("\n" . "\\\\n"))
               :initial-value string)
          "'"))

;;; TODO(5/31/24): buffer tracking if lua repl enters debug
;; see `python-pdbtrack-setup-tracking' for inspiration
(defun inf-lua-dbg-setup-tracking ()
  "Setup lua debugger tracking in current buffer.")


;;; Font-locking
(defvar inf-lua-font-lock-keywords
  `(;; Tracebacks (Lua and rep.lua)
    (,(rx-to-string `(seq bol (group "Error") ": " (group (* nonl))))
     (1 'font-lock-warning-face append)
     (2 '(:inherit font-lock-warning-face :slant italic :weight normal) append))
    (,(rx-to-string `(seq ": " (group  "in function ") "'" (group (+ (not "'"))) "'"))
     (1 'font-lock-comment-face)
     (2 'font-lock-function-name-face prepend))
    (": \\(in \\(?:main chunk\\|?\\)\\)" (1 'font-lock-comment-face))
    (,(rx-to-string `(seq (group "(...") (group "tail calls") (group "...)")))
     (1 'font-lock-comment-face)
     (2 'font-lock-doc-markup-face)
     (3 'font-lock-comment-face))
    (,(rx-to-string `(seq bol "stack traceback")) . font-lock-preprocessor-face)
    ("\\[\\(?:C\\|REPL\\)\\]" . font-lock-constant-face)
    ("\\[string \"REPL\"\\]:.*" . font-lock-comment-face))
  "Additional font-locking keywords in `inf-lua-mode'.")


;; -------------------------------------------------------------------
;;; Completion

(defvar inf-lua--completion-code "
function __REPL_complete(line, initial_scope)
  local base, sep, rest = string.match(line, '^(.*)([.:])(.*)')
  if not base then rest = line end
  local prefix = string.match(rest, '^[%a_][%a%d_]*')
  if prefix and prefix ~= rest then return print() end
  local function get_scope(scope)
    if not scope or scope == '' then return _ENV or _G end
    local f = load('return ' .. scope)
    if f then
      local ok
      ok, scope = pcall(f)
      if ok then
        local typ = type(scope)
        if typ == 'string' then return string end
        if typ == 'thread' then return coroutine end
        return scope
      end
    end
    return {}
  end
  local scope = get_scope(initial_scope) --- @type table
  if not scope then return print() end
  local meth = sep == ':'
  if base then
    if meth and type(scope[base]) == 'string' then
      scope = string
    elseif meth and type(scope[base]) == 'thread' then
      scope = coroutine
    else
      local f = load('return ' .. base, nil, nil, scope)
      if not f then return print() end
      local ok
      ok, scope = pcall(f)
      if not ok then return print() end
    end
  else
    base = ''
    sep = ''
    scope = scope
  end
  local matches = {}
  local prop = sep ~= ':'
  while type(scope) == 'table' do
    for key, value in pairs(scope) do
      if (prop or (type(value) == 'function')) and
           ((not prefix) or (string.match(key, '^' .. prefix))) then
        matches[key] = true
      end
    end
    scope = getmetatable(scope)
    scope = scope and scope.__index
  end
  local items = {}
  for key in pairs(matches) do
    items[#items + 1] = key
  end
  table.sort(items)
  return print(table.concat(items, '\\n'))
end
")                            ; leave trailing newline so initial prompt is '> '

(defun inf-lua-setup-completion ()
  "Setup completion in Lua repl."
  (interactive)
  (if-let ((process (inf-lua-process)))
      (progn (comint-send-string process (concat inf-lua--completion-code "\n"))
             (setq inf-lua-completion-enabled t))
    (user-error "Start a lua process first")))

(defun inf-lua--get-completions-from-process (process input &optional scope)
  "Get completions for prefix INPUT in SCOPE from PROCESS."
  (cl-flet ((escape-string (s) (replace-regexp-in-string "\\\"" "\\\\\"" s)))
    (with-current-buffer (process-buffer process)
      ;; FIXME: use temp buffer eventually
      (let ((redirect-buffer (get-buffer-create "* Lua completions redirect*"))
            (input-to-send
             (format "__REPL_complete(\"%s\", %s)"
                     (escape-string input)
                     (and scope (concat "\"" (escape-string scope) "\"")))))
        (with-current-buffer redirect-buffer
          (erase-buffer)
          (with-current-buffer (process-buffer process)
            (comint-redirect-send-command
             input-to-send redirect-buffer nil t)
            (while (and (null comint-redirect-completed)
                        (accept-process-output process 1)))
            (comint-redirect-cleanup))
          (split-string
           (buffer-substring-no-properties
            (line-beginning-position) (point-min))
           "[ \f\t\n\r\v()]+" t))))))

(defvar-local inf-lua--capf-cache nil)

(defun inf-lua--beginning-of-sexp (lim &optional include-scope)
  "Move point to beginning of preceding Lua sexp, bounded by LIM.
When INCLUDE-SCOPE is non-nil, move to beginning of preceding chained
command, eg. move to beginning of \"t[i].fn().prefix\"."
  (while (and (> (point) lim)
              (or (not (zerop (skip-syntax-backward "w_")))
                  (and include-scope
                       (pcase (char-before)
                         ((or ?. ?:) (forward-char -1) t)
                         ;; XXX: includes function calls eg. "fn().prefix"
                         ((or ?\) ?\] ) (backward-sexp) t)
                         (_ nil))))))
  (point))

(defun inf-lua--completion-at-point (&optional process)
  "Generate completions at point from PROCESS."
  (setq process (or process (get-buffer-process (current-buffer))))
  (let* ((repl-buffer-p (derived-mode-p 'inf-lua-mode))
         (line-start (if repl-buffer-p
                         (cdr (bound-and-true-p comint-last-prompt))
                       (line-beginning-position)))
         (in-string-p (nth 3 (syntax-ppss)))
         (scope)
         (sep)
         (end (point))
         (start (if (< (point) line-start)
                    (point)
                  (save-excursion
                    (if (not in-string-p)
                        (prog1 (inf-lua--beginning-of-sexp line-start)
                          (when (memq (char-before) '(?. ?:))
                            (forward-char -1)
                            (setq sep (char-to-string (char-after))
                                  scope (buffer-substring-no-properties
                                         (save-excursion
                                           (inf-lua--beginning-of-sexp
                                            line-start 'scope))
                                         (point)))))
                      (search-backward (string in-string-p) line-start t)
                      (prog1 (1+ (point))
                        ;; table key, eg _G["str
                        (when (eq ?\[ (char-before))
                          (forward-char -1)
                          (let ((scope-end (point)))
                            (setq scope (buffer-substring-no-properties
                                         (inf-lua--beginning-of-sexp line-start)
                                         scope-end)))))))))
         (filename-p (and in-string-p (not scope)))
         (proc-buff (process-buffer process))
         (prompt)
         (prompt-boundaries (with-current-buffer proc-buff
                              (when comint-last-prompt
                                (setq prompt (buffer-substring-no-properties
                                              (car comint-last-prompt)
                                              (cdr comint-last-prompt)))
                                comint-last-prompt)))
         (completion-fn
          (with-current-buffer proc-buff
            (cond ((or (null prompt)
                       (and repl-buffer-p
                            (or (< (point) (cdr prompt-boundaries))
                                (string-match-p inf-lua-debug-prompt prompt))))
                   #'ignore)
                  (filename-p #'ignore)
                  (t #'inf-lua--get-completions-from-process))))
         (prev-prompt (car inf-lua--capf-cache))
         (re (or (cadr inf-lua--capf-cache) regexp-unmatchable))
         (prefix (and (>= end start)
                      (buffer-substring-no-properties start end)))
         (cache-prefix (concat scope sep prefix)))
    (unless (and cache-prefix
                 (equal prev-prompt (car prompt-boundaries))
                 (string-match re cache-prefix))
      (setq inf-lua--capf-cache
            `(,(car prompt-boundaries)
              ,(if (string-empty-p cache-prefix)
                   regexp-unmatchable
                 (concat
                  "\\`" (regexp-quote cache-prefix) "\\(?:\\sw\\|\\s_\\)*\\'"))
              ,@(funcall completion-fn process prefix scope))))
    (when (>= end start)
      (list start end (if filename-p
                          #'completion-file-name-table
                        (cddr inf-lua--capf-cache))
            :company-prefix-length (and (memq (char-before) '(?: ?.)) t)))))

(defun inf-lua-completion-at-point ()
  "Completion at point function for Lua repl."
  (when (and inf-lua-completion-enabled
             (derived-mode-p 'inf-lua-mode)
             (comint-after-pmark-p))
    (inf-lua--completion-at-point (get-buffer-process (current-buffer)))))


(defvar-keymap inf-lua-mode-map
  :doc "Keymap in inferior Lua buffer."
  "TAB" #'completion-at-point)

;;;###autoload
(define-derived-mode inf-lua-mode comint-mode "Lua"
  "Major mode for lua repl.
\\<inf-lua-mode-map>"
  :syntax-table lua-ts--syntax-table
  (setq-local mode-line-process '(":%s")
              comment-start "--"
              comment-end ""
              comment-start-skip "--+ *"
              parse-sexp-ignore-comments t
              parse-sexp-lookup-properties t)
  (inf-lua--calculate-prompt-regexps)
  (setq-local comint-input-ignoredups t
              comint-input-ring-file-name inf-lua-history-filename
              comint-prompt-read-only t
              comint-prompt-regexp inf-lua--prompt-internal
              comint-output-filter-functions '(ansi-color-process-output)
              comint-highlight-input nil)
  (add-hook 'comint-preoutput-filter-functions #'inf-lua--preoutput-filter nil t)
  (setq-local scroll-conservatively 1)

  ;; Font-locking
  (setq-local font-lock-defaults '(inf-lua-font-lock-keywords nil nil))
  (when (and (null comint-use-prompt-regexp)
             inf-lua-font-lock-enable
             (or (require 'lua-ts-mode nil t)
                 (require 'lua-mode nil t)))
    (comint-fontify-input-mode))

  (setq comint-indirect-setup-function
        (lambda ()
          (let ((inhibit-message t)
                (message-log-max nil))
            (cond ((fboundp 'lua-ts-mode) (lua-ts-mode))
                  ((fboundp 'lua-mode) (lua-mode))
                  (t nil)))))

  ;; Compilation
  (setq-local compilation-error-regexp-alist inf-lua-repl-compilation-regexp-alist)
  (compilation-shell-minor-mode t)
  ;; XXX: probably dont do this
  (face-remap-add-relative 'compilation-error 'link)

  (add-hook 'completion-at-point-functions #'inf-lua-completion-at-point nil t)
  (inf-lua-dbg-setup-tracking))

(provide 'inf-lua)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; inf-lua.el ends here
