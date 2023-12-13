;;; inf-lua.el --- Lua repl mode -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/inf-lua
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created:  9 December 2023
;; Keywords: lua languages

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
;;  - font-locking using `lua-ts-mode' for input
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

(require 'comint)
(require 'lua-ts-mode)

(defgroup inf-lua nil
  "Run Lua process in a buffer."
  :group 'languages)

(defcustom inf-lua-buffer-name "Lua"
  "Default buffer name for the Lua interpreter."
  :type 'string
  :safe 'stringp
  :group 'inf-lua)

(defcustom inf-lua-font-lock-enable t
  "Non-nil to enable font-locking in the repl buffer."
  :type 'boolean
  :group 'inf-lua)

(defcustom inf-lua-completion-enabled t
  "Enable/disable inferior lua completion at point."
  :type 'boolean
  :group 'inf-lua)

(defvar inf-lua-prompt-debugger-regexp "debugger.lua>")

(defvar inf-lua-repl-compilation-regexp-alist
  '(;; debugger.lua
    ("^[>]?\\s-*break via dbg.+=> \\([^:]+\\):\\([0-9]+\\)" 1 2)
    ("^\\s-*[0-9]+\\(?: =>\\)?\\s-*\\([^:]+\\):\\([0-9]+\\)" 1 2)
    ;; `lua-traceback-line-re'
    ("^\\(?:[\t ]*\\|.*>[\t ]+\\)\\(?:[^\n\t ]+:[0-9]+:[\t ]*\\)*\\(?:\\([^\n\t ]+\\):\\([0-9]+\\):\\)" 1 2)))

(defun inf-lua-calculate-command (&optional prompt default)
  (setq default (or default (concat
                             lua-ts-inferior-program " "
                             (mapconcat 'identity lua-ts-inferior-options " "))))
  (if prompt (read-shell-command "Run Lua: " default) default))
;; Dont use '-e' in case repl doesn't support it, eg. rep.lua
;; (when inf-lua-completion-enabled
;;   (format " -e \"%s\"" (replace-regexp-in-string
;;                         "\\\\" "\\\\\\\\" inf-lua--completion-code)))

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
  "Run a Lua interpreter in an inferior process."
  (interactive (list current-prefix-arg nil nil t))
  (let* ((cmd (inf-lua-calculate-command prompt cmd))
         (buffer (inf-lua-make-comint
                  cmd
                  inf-lua-buffer-name
                  (or startfile lua-ts-inferior-startfile)
                  show)))
    (get-buffer-process buffer)))

(defun inf-lua-make-comint (cmd proc-name &optional startfile show)
  "Create a Lua comint buffer.
CMD is the Lua command to be executed and PROC-NAME is the process name
that will be given to the comint buffer.
If STARTFILE is non-nil, use that instead of `lua-ts-inferior-startfile'
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
        (when lua-ts-inferior-history
          (set-process-sentinel
           (get-buffer-process buffer) #'lua-ts-inferior--write-history))
        (with-current-buffer buffer
          (inf-lua-mode)
          (when inf-lua-completion-enabled
            (inf-lua-setup-completion)))))
    (when show
      (pop-to-buffer proc-buff-name))
    proc-buff-name))

;; Defined in `lua-ts-inferior-lua'
(defun inf-lua--preoutput-filter (string)
  (if (or (not (equal (buffer-name) lua-ts-inferior-buffer))
          (equal string (concat lua-ts-inferior-prompt-continue " ")))
      string
    (concat
     ;; Filter out the extra prompt characters that
     ;; accumulate in the output when sending regions
     ;; to the inferior process.
     (replace-regexp-in-string (rx-to-string
                                `(: bol
                                    (* ,lua-ts-inferior-prompt
                                       (? ,lua-ts-inferior-prompt)
                                       (1+ space))
                                    (group (* nonl))))
                               "\\1" string)
     ;; Re-add the prompt for the next line.
     lua-ts-inferior-prompt " ")))

;; (defvar inf-lua-output-filter-buffer nil)
;; (defun inf-lua-output-filter (string)
;;   (setq inf-lua-output-filter-buffer
;;         (concat inf-lua-output-filter-buffer string)))

;; (defun inf-lua-send-string-no-output (string &optional process)
;;   (or process (setq process (or (inf-lua-process)
;;                                 (user-error "No lua process found"))))
;;   (cl-letf* (((process-filter process)
;;               (lambda (_proc str)
;;                 (with-current-buffer (process-buffer process)
;;                   (inf-lua--preoutput-filter str))))
;;              (inhibit-quit t)
;;              (buffer (process-buffer process))
;;              (last-prompt 'comint-last-prompt)
;;              (last-prompt-value (buffer-local-value last-prompt buffer)))
;;     (or
;;      (with-local-quit
;;        (unwind-protect
;;            (comint-send-string process string))))))

;; -------------------------------------------------------------------
;;; Completion

(defvar inf-lua--completion-code "
function __REPL_complete(line, initial_scope)
  local base, sep, rest = string.match(line, '^(.*)([.:])(.*)')
  if not base then rest = line end
  local prefix = string.match(rest, '^[%a_][%a%d_]*')
  if prefix and prefix ~= rest then return print() end
  local function get_scope(scope)
    if not scope or scope == '' then return _G end
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
  if #items == 1 then
    return print(items[1])
  elseif #items > 1 then
    return print(table.concat(items, '\\n'))
  end
end")

(defun inf-lua-setup-completion ()
  "Setup completion in Lua repl."
  (interactive)
  (if-let ((process (inf-lua-process)))
      (progn (comint-send-string process inf-lua--completion-code)
             (setq inf-lua-completion-enabled t))
    (user-error "Start a lua process first")))

(defun inf-lua--get-completions-from-process (process input &optional scope)
  "Get completions for prefix INPUT in SCOPE from PROCESS."
  (cl-flet ((escape-string (s) (replace-regexp-in-string "\\\"" "\\\\\"" s)))
    (with-current-buffer (process-buffer process)
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
  (while (and (> (point) lim)
              (or (not (zerop (skip-syntax-backward "w_")))
                  (and include-scope
                       (pcase (char-before)
                         ((or ?. ?:) (forward-char -1) t)
                         ;; XXX: include function calls? eg. "fn().prefix"
                         ((or ?\) ?\] ) (backward-sexp) t)
                         (_ nil))))))
  (point))

(defun inf-lua--completion-at-point (&optional process)
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
                            (< (point) (cdr prompt-boundaries)))
                       (and (not repl-buffer-p)
                            (string-match-p
                             inf-lua-prompt-debugger-regexp prompt)))
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
                        (cddr inf-lua--capf-cache))))))

(defun inf-lua-completion-at-point ()
  "Completion at point function for Lua repl."
  (when (and inf-lua-completion-enabled
             (derived-mode-p 'inf-lua-mode)
             (comint-after-pmark-p))
    (inf-lua--completion-at-point (get-buffer-process (current-buffer)))))

;; -------------------------------------------------------------------
;;; Font-lock
;; Stolen from `python.el'

(defvar-local inf-lua--font-lock-buffer nil)

(defmacro inf-lua-with-shell-buffer (&rest body)
  "Execute the forms in BODY with the shell buffer temporarily current.
Signals an error if no shell buffer is available for current buffer."
  (declare (indent 0) (debug t))
  (let ((shell-process (make-symbol "shell-process")))
    `(let ((,shell-process (get-buffer-process lua-ts-inferior-buffer)))
       (unless ,shell-process
         (user-error "Start a Lua process first"))
       (with-current-buffer (process-buffer ,shell-process)
         ,@body))))

(defmacro inf-lua-font-lock-with-font-lock-buffer (&rest body)
  "Execute the forms in BODY in the font-lock buffer.
The value returned is the value of the last form in BODY.  See
also `with-current-buffer'."
  (declare (indent 0) (debug t))
  `(inf-lua-with-shell-buffer
     (save-current-buffer
       (when (not (and inf-lua--font-lock-buffer
                       (get-buffer inf-lua--font-lock-buffer)))
         (setq inf-lua--font-lock-buffer
               (inf-lua-font-lock-get-or-create-buffer)))
       (set-buffer inf-lua--font-lock-buffer)
       (when (not font-lock-mode)
         (font-lock-mode 1))
       (setq-local delay-mode-hooks t)
       (when (not (derived-mode-p 'lua-ts-mode))
         (lua-ts-mode))
       ,@body)))

(defun inf-lua-font-lock-kill-buffer ()
  "Kill the font-lock buffer safely."
  (when (and inf-lua--font-lock-buffer
             (buffer-live-p inf-lua--font-lock-buffer))
    (kill-buffer inf-lua--font-lock-buffer)
    (when (derived-mode-p 'inf-lua-mode)
      (setq inf-lua--font-lock-buffer nil))))

(defun inf-lua-font-lock-get-or-create-buffer ()
  "Get or create a font-lock buffer for current inferior process."
  (inf-lua-with-shell-buffer
    (if inf-lua--font-lock-buffer
        inf-lua--font-lock-buffer
      (let ((process-name
             (process-name (get-buffer-process (current-buffer)))))
        (generate-new-buffer
         (format " *%s-font-lock*" process-name))))))

(defun inf-lua-font-lock-post-command-hook ()
  "Fontifies current line in inferior lua buffer."
  (let ((prompt-end (cdr comint-last-prompt)))
    (when (and prompt-end (> (point) prompt-end)
               (process-live-p (get-buffer-process (current-buffer))))
      (let* ((input (buffer-substring-no-properties
                     prompt-end (point-max)))
             (deactivate-mark nil)
             (start-pos prompt-end)
             (buffer-undo-list t)
             (replacement
              (inf-lua-font-lock-with-font-lock-buffer
                (delete-region (point-min) (point-max))
                (insert input)
                (font-lock-ensure)
                (buffer-string)))
             (replacement-length (length replacement))
             (i 0))
        ;; Inject text properties to get input fontified.
        (while (not (= i replacement-length))
          (let* ((plist (text-properties-at i replacement))
                 (next-change (or (next-property-change i replacement)
                                  replacement-length))
                 (plist (let ((face (plist-get plist 'face)))
                          (if (not face)
                              plist
                            ;; Replace FACE text properties with
                            ;; FONT-LOCK-FACE so input is fontified.
                            (plist-put plist 'face nil)
                            (plist-put plist 'font-lock-face face)))))
            (set-text-properties
             (+ start-pos i) (+ start-pos next-change) plist)
            (setq i next-change)))))))

(defun inf-lua-font-lock-cleanup-buffer ()
  "Cleanup the font-lock buffer.
Provided as a command because this might be handy if something
goes wrong and syntax highlighting in the shell gets messed up."
  (interactive)
  (inf-lua-with-shell-buffer
    (inf-lua-font-lock-with-font-lock-buffer
      (erase-buffer))))

(defun inf-lua-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT ends with input prompt."
  (string-match
   ;; XXX: It seems on macOS an extra carriage return is attached
   ;; at the end of output, this handles that too.
   (concat
    "\r?\n?"
    ;; Remove initial caret from calculated regexp
    (replace-regexp-in-string
     (rx string-start ?^) ""
     lua-ts-inferior-prompt)
    (rx eos))
   output))

(defun inf-lua-font-lock-comint-output-filter-function (output)
  "Clean up the font-lock buffer after any OUTPUT."
  (unless (string= output "")
    (if (let ((output (ansi-color-filter-apply output)))
          (and (inf-lua-comint-end-of-output-p output)
               (not (string-match lua-ts-inferior-prompt-continue output))))
        ;; If output ends with an initial (not continuation) input prompt
        ;; then the font-lock buffer must be cleaned up.
        (inf-lua-font-lock-cleanup-buffer)
      ;; Otherwise just add a newline.
      (inf-lua-font-lock-with-font-lock-buffer
        (goto-char (point-max))
        (newline)))
    output))

(defun inf-lua-font-lock-turn-on (&optional msg)
  (interactive "p")
  (inf-lua-with-shell-buffer
    (inf-lua-font-lock-kill-buffer)
    (setq-local inf-lua--font-lock-buffer nil)
    (add-hook 'post-command-hook
              #'inf-lua-font-lock-post-command-hook nil t)
    (add-hook 'kill-buffer-hook #'inf-lua-font-lock-kill-buffer nil t)
    (add-hook 'comint-output-filter-functions
              #'inf-lua-font-lock-comint-output-filter-function 'append))
  (when msg
    (message "Inf lua font-lock enabled")))


(defvar inf-lua-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'completion-at-point)
    map))

;;;###autoload
(define-derived-mode inf-lua-mode comint-mode "Lua"
  "Major mode for lua repl.
\\<inf-lua-mode-map>"
  (setq-local comment-start "--"
              comment-end ""
              comment-start-skip "--+ *"
              parse-sexp-ignore-comments t
              parse-sexp-lookup-properties t)

  (setq-local mode-line-process '(":%s"))

  (setq-local comint-input-ignoredups t
              comint-input-ring-file-name lua-ts-inferior-history
              comint-prompt-read-only t
              comint-prompt-regexp (rx-to-string
                                    `(: bol ,lua-ts-inferior-prompt
                                        (1+ space)))
              comint-output-filter-functions '(ansi-color-process-output
                                               comint-watch-for-password-prompt)
              comint-highlight-input nil)
  (add-hook 'comint-preoutput-filter-functions #'inf-lua--preoutput-filter nil t)  

  ;; compilation
  (setq-local compilation-error-regexp-alist inf-lua-repl-compilation-regexp-alist)
  (compilation-shell-minor-mode t)

  (when inf-lua-font-lock-enable
    (inf-lua-font-lock-turn-on))
  (add-hook 'completion-at-point-functions #'inf-lua-completion-at-point nil t))

(provide 'inf-lua)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; inf-lua.el ends here
