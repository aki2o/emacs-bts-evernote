;;; bts-evernote.el --- A plugin of bts.el for Evernote -*- coding: utf-8; -*-

;; Copyright (C) 2015  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: convenience, evernote
;; URL: https://github.com/aki2o/emacs-bts-evernote
;; Version: 0.0.1
;; Package-Requires: ((bts "0.1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; Nothing.

;;; Dependencies:
;; 
;; - bts.el ( see <https://github.com/aki2o/emacs-bts> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'bts-evernote)

;;; Configuration:
;;
;; ;; About config item, see Customization or eval the following sexp.
;; ;; (customize-group "bts-evernote")

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "bts-evernote:[^:]" :docstring t)
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "bts-evernote:[^:]" :docstring t)
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.4.1 (i686-pc-linux-gnu, GTK+ Version 2.24.10) of 2015-10-22 on hiroaki-vm
;; - bts.el ... Version 0.1.0


;; Enjoy!!!


;;; Code:
(require 'cl-lib)
(require 'server)
(require 'bts)
(require 'auto-complete nil t)


(defgroup bts-evernote nil
  "A plugin of bts for Evernote."
  :group 'bts
  :prefix "bts-evernote:")

(defcustom bts-evernote:geeknote-command "geeknote"
  "Path for geeknote command."
  :type 'string
  :group 'bts-evernote)

(defcustom bts-evernote:find-note-query-completion 'auto-complete
  "Feature for completion in minibuffer to find note."
  :type '(choice (const auto-complete)
                 (const nil))
  :group 'bts-evernote)


(defvar bts-evernote::guid-regexp "a-zA-Z0-9-")

(defvar bts-evernote::msg-now-loading
  (propertize "Now loading..." 'face 'font-lock-keyword-face))


;;;;;;;;;;;;;
;; Utility

(defun bts-evernote::hash-exists-p (key table)
  (let ((novalue (cl-gensym)))
    (not (eq (gethash key table novalue) novalue))))

(defun bts-evernote::build-geeknote-command (&rest args)
  (let ((ret (format "%s %s"
                     bts-evernote:geeknote-command
                     (mapconcat 'shell-quote-argument (delete nil args) " "))))
    (bts--debug "evernote built command : %s" ret)
    ret))
  
(defmacro bts-evernote::with-prefer-cache (cmd-args tasknm cache-var force cb &rest body)
  (declare (indent 1))
  `(or (when (and (not ,force) ,cache-var)
         (gethash (bts-evernote::get-current-account) ,cache-var))
       (when (or (not ,cache-var)
                 (not (bts-evernote::hash-exists-p (bts-evernote::get-current-account) ,cache-var)))
         (lexical-let* ((cmd (apply 'bts-evernote::build-geeknote-command ,cmd-args))
                        (bufnm (format " *bts-evernote %s*" ,tasknm))
                        (procnm (format "bts-evernote %s" ,tasknm))
                        (proc (get-process procnm))
                        (cb ,cb))
           (cond
            (cb
             (if (and (processp proc) (process-live-p proc))
                 (bts--info "evernote not start delayed work[%s] : already started" ,tasknm)
               (bts--debug "evernote start delayed work[%s] : %s" ,tasknm cmd)
               (set-process-sentinel
                (start-process-shell-command procnm bufnm cmd)
                (lambda (proc msg)
                  (yaxception:$~
                    (yaxception:try
                      (let ((ret (with-current-buffer bufnm
                                   (bts--trace* "evernote got result of %s ...\n%s" cmd (buffer-string))
                                   (save-match-data ,@body))))
                        (if (not ,cache-var)
                            (funcall cb ret)
                          (puthash (bts-evernote::get-current-account) ret ,cache-var)
                          (bts--debug* "evernote finished delayed work[%s] ...\n%s" ,tasknm ret)
                          (funcall cb))))
                    (yaxception:catch 'error e
                      (bts--error "evernote failed delayed work[%s] : %s" ,tasknm (yaxception:get-text e))
                      (bts:show-message "Failed delayed work for %s : %s" ,tasknm (yaxception:get-text e)))
                    (yaxception:finally
                      (ignore-errors (kill-buffer bufnm)))))))
             'scheduled)
            (t
             (bts--debug "evernote start no-delayed work[%s] : %s" ,tasknm cmd)
             (let ((ret (with-temp-buffer
                          (insert (shell-command-to-string cmd))
                          (bts--trace* "evernote got result of %s ...\n%s" cmd (buffer-string))
                          (save-match-data ,@body))))
               (when ,cache-var
                 (puthash (bts-evernote::get-current-account) ret ,cache-var))
               (bts--debug* "evernote finished no-delayed work[%s] ...\n%s" ,tasknm ret)
               ret)))))))

(defmacro bts-evernote::clear-cache (cache-var)
  (declare (indent 0))
  `(progn
     (remhash (bts-evernote::get-current-account) ,cache-var)
     (bts--info "evernote cleared cache of %s for %s"
                ',cache-var (bts-evernote::get-current-account))))


;;;;;;;;;;;;;
;; Account

(defvar bts-evernote::current-account nil)

(defun bts-evernote::get-current-account ()
  (or bts-evernote::current-account
      (bts-evernote::with-prefer-cache '("settings")
        "settings" nil t nil
        (goto-char (point-min))
        (re-search-forward "^Username: +")
        (setq bts-evernote::current-account
              (buffer-substring-no-properties (point) (point-at-eol)))
        (bts--info "evernote get current account : %s" bts-evernote::current-account)
        bts-evernote::current-account)))


;;;;;;;;;;;;;;
;; Notebook

(defvar bts-evernote::books-cache-hash (make-hash-table :test 'equal))

(defvar bts-evernote::notebook-regexp
  (rx-to-string `(and bos (* space)
                      (group (+ (any ,bts-evernote::guid-regexp))) (+ space)
                      ":" (+ space)
                      (group (+ not-newline))
                      eol)))

(defun* bts-evernote::fetch-notebooks (&key force cb)
  (bts-evernote::with-prefer-cache '("notebook-list" "--guid")
    "books" bts-evernote::books-cache-hash force cb
    (cl-loop initially (goto-char (point-min))
             while (not (eobp))
             for line = (thing-at-point 'line)
             do (bts--trace* "evernote got result line for notebook : %s" line)
             if (string-match bts-evernote::notebook-regexp line)
             collect (progn
                       (bts--debug "evernote found notebook line : %s" line)
                       (cons (match-string-no-properties 1 line)
                             (match-string-no-properties 2 line)))
             do (forward-line 1))))

(defun* bts-evernote::fetch-notebook-names (&rest args)
  (let ((books (apply 'bts-evernote::fetch-notebooks args)))
    (if (symbolp books) books (mapcar 'cdr books))))

(defun bts-evernote::get-notebook-name (guid)
  (assoc-default guid (bts-evernote::fetch-notebooks)))

(defun bts-evernote::get-notebook-guid (name)
  (cl-loop for e in (bts-evernote::fetch-notebooks)
           if (string= (cdr e) name) return (car e)))

(defun bts-evernote::clear-notebook-cache ()
  (bts-evernote::clear-cache bts-evernote::books-cache-hash))


;;;;;;;;;
;; Tag

(defvar bts-evernote::tags-cache-hash (make-hash-table :test 'equal))

(defvar bts-evernote::tag-regexp
  (rx-to-string `(and bos (* space)
                      (group (+ (any ,bts-evernote::guid-regexp))) (+ space)
                      ":" (+ space)
                      (group (+ not-newline))
                      eol)))

(defun* bts-evernote::fetch-tags (&key force cb)
  (bts-evernote::with-prefer-cache '("tag-list" "--guid")
    "tags" bts-evernote::tags-cache-hash force cb
    (cl-loop initially (goto-char (point-min))
             while (not (eobp))
             for line = (thing-at-point 'line)
             do (bts--trace* "evernote got result line for tag : %s" line)
             if (string-match bts-evernote::tag-regexp line)
             collect (progn
                       (bts--debug "evernote found tag line : %s" line)
                       (cons (match-string-no-properties 1 line)
                             (match-string-no-properties 2 line)))
             do (forward-line 1))))

(defun* bts-evernote::fetch-tag-names (&rest args)
  (let ((tags (apply 'bts-evernote::fetch-tags args)))
    (if (symbolp tags) tags (mapcar 'cdr tags))))

(defun bts-evernote::get-tag-name (guid)
  (assoc-default guid (bts-evernote::fetch-tags)))

(defun bts-evernote::get-tag-guid (name)
  (cl-loop for e in (bts-evernote::fetch-tags)
           if (string= (cdr e) name) return (car e)))

(defun bts-evernote::clear-tag-cache ()
  (bts-evernote::clear-cache bts-evernote::tags-cache-hash))


;;;;;;;;;;;;;
;; Project

(defun bts-evernote::submit-project (mdl)
  (bts--debug "evernote start submit project.")
  `(,@(bts:make-plist-from-alist mdl :includes '(account))
    :secret-passwd ,(assoc-default 'passwd mdl)))

(defun bts-evernote:make-project-view (project)
  "Function for `bts:system-project-view'."
  (let* ((lo `((:type text :name account :label "YourName" :size 30 :require t)
               BR (:type text :name passwd :label "Password" :size 30 :require t :secret t)))
         (defs (when project
                 (bts:make-alist-from-plist project
                                            :includes '(account passwd)))))
    `(:layout ,lo :defaults ,defs :submit-action bts-evernote::submit-project)))


;;;;;;;;;;;
;; Query

(defun bts-evernote::refresh-books-layout (mdl)
  (bts-evernote::clear-notebook-cache)
  (bts:widget-update-buffer :showp t))

(defun bts-evernote::gen-books-layout (value-is-id &rest args)
  (let* ((func (if value-is-id
                   'bts-evernote::fetch-notebooks
                 'bts-evernote::fetch-notebook-names))
         (books (funcall func
                 :cb `(lambda ()
                        (bts:widget-update-buffer :buffer-or-name ,(buffer-name) :showp t)))))
    (if (eq books 'scheduled)
        `((:type const ,@args :value "") bts-evernote::msg-now-loading)
      `((:type select :options ,books ,@args)
        " " (:type button :name clear-books :title " Clear Cache "
                   :action bts-evernote::refresh-books-layout)))))

(defun bts-evernote::gen-query-books-layout ()
  (when (memq (bts:widget-get-value 'book-filter) '(on-query))
    `(BR ,@(bts-evernote::gen-books-layout
            t :name 'filtered-books :label "" :label-face 'default :multiple t))))

(defun bts-evernote::query-target-help (mdl)
  (mapconcat 'identity
             (mapcar #'(lambda (e) (format "%-8s ... %s" (car e) (cdr e)))
                     '(("title" . "filter only by note title")
                       ("content" . "filter only by note content")
                       ("on-fetch" . "ask this option value when fetching notes")))
             "\n"))

(defun bts-evernote::query-book-help (mdl)
  (mapconcat 'identity
             (mapcar #'(lambda (e) (format "%-8s ... %s" (car e) (cdr e)))
                     '(("none" . "not filtering at this timing but able to filter when fetching notes")
                       ("on-query" . "add filtered notebooks in this query")))
             "\n"))

(defun bts-evernote:make-query-view (project query)
  "Function for `bts:system-query-view'."
  (let* ((lo `((:type select :name target :label "Search Target" :options (title content on-fetch)
                      :require t :radio t :horizontal t :value title)
               " " (:type link :tip bts-evernote::query-target-help)
               BR (:type select :name book-filter :label "Notebook Filter" :options (none on-query)
                         :require t :radio t :horizontal t :value none :action on-update)
               " " (:type link :tip bts-evernote::query-book-help)
               (bts-evernote::gen-query-books-layout)))
         (defs (when query
                 (bts:make-alist-from-plist query))))
    `(:layout ,lo :defaults ,defs)))


;;;;;;;;;;;;;;;;;
;; View Ticket

(defun bts-evernote::get-tags-from (s)
  (cl-loop for e in (split-string s ",")
           for e = (replace-regexp-in-string "\\`\\s-+" "" e)
           for e = (replace-regexp-in-string "\\s-+\\'" "" e)
           collect e))

(defvar bts-evernote::show-note-title-regexp
  (rx-to-string `(and bos (+ "#") (+ space) "TITLE" (+ space) (+ "#") eos)))

(defvar bts-evernote::show-note-meta-regexp
  (rx-to-string `(and bos (+ "=") (+ space) "META" (+ space) (+ "=") eos)))

(defvar bts-evernote::show-note-content-regexp
  (rx-to-string `(and bos (+ "-") (+ space) "CONTENT" (+ space) (+ "-") eos)))

(defun bts-evernote::piece-out-ticket (ticket)
  (let ((guid (plist-get ticket :guid))
        bodies)
    (bts-evernote::with-prefer-cache `("show" ,guid)
      guid nil t nil
      (bts--debug "evernote start piece out ticket : %s" guid)
      (cl-loop initially (goto-char (point-min))
               with curr-prop = nil
               while (not (eobp))
               for line = (replace-regexp-in-string "\r?\n" "" (thing-at-point 'line))
               do (cond
                   ;; Found previous of title
                   ((string-match bts-evernote::show-note-title-regexp line)
                    (setq curr-prop 'title))
                   ;; Found start of meta info
                   ((string-match bts-evernote::show-note-meta-regexp line)
                    (setq curr-prop 'meta))
                   ;; Found start of body
                   ((string-match bts-evernote::show-note-content-regexp line)
                    (setq curr-prop 'body))
                   ;; Found title
                   ((eq curr-prop 'title)
                    (plist-put ticket :title line)
                    (bts--debug "evernote pieced out ticket title : %s" (plist-get ticket :title)))
                   ;; Found author
                   ((and (eq curr-prop 'meta)
                         (string-match "\\`author: +\\(.+\\)\\'" line))
                    (plist-put ticket :author (match-string-no-properties 1 line))
                    (bts--debug "evernote pieced out ticket author : %s" (plist-get ticket :author)))
                   ;; Found tags
                   ((and (eq curr-prop 'body)
                         (not bodies)
                         (string-match "\\`Tags: +\\(.+\\)\\'" line))
                    (plist-put ticket :tags (match-string-no-properties 1 line))
                    (bts--debug "evernote pieced out ticket tags : %s" (plist-get ticket :tags)))
                   ;; Found body
                   ((eq curr-prop 'body)
                    (push line bodies)))
               do (forward-line 1)))
    (when bodies
      (plist-put ticket :body (mapconcat 'identity (reverse bodies) "\n"))
      (bts--debug "evernote pieced out ticket body : %s" (plist-get ticket :body)))))

(defun bts-evernote::refresh-tags-layout (mdl)
  (bts-evernote::clear-tag-cache)
  (bts:widget-update-buffer :showp t))

(defun bts-evernote::gen-ticket-books-layout ()
  (bts-evernote::gen-books-layout nil :name 'book :label "Notebook"))

(bts:regist-message 'bts-evernote-tags-input
  t         "input tag name with separating comma"
  'Japanese "タグ名をカンマ区切りで入力")

(defvar bts-evernote::ticket-tags-help (bts:get-message 'bts-evernote-tags-input))

(defun bts-evernote:make-ticket-single-view (project ticket)
  "Function for `bts:system-ticket-single-view'."
  (let* ((lo `((:type const :name author :label "Author" :size 50)
               BR (:type text :name title :label "Title" :size 50)
               BR (bts-evernote::gen-ticket-books-layout)
               BR (:type text :name tags :label "Tags" :size 50)
               " " (:type link :tip bts-evernote::ticket-tags-help)
               " " (:type button :name clear-tags :title " Clear Cache "
                          :action bts-evernote::refresh-tags-layout)
               BR (:type text :name body :label "Content" :area t)))
         (defs (when ticket
                 (bts-evernote::piece-out-ticket ticket)
                 (bts:make-alist-from-plist ticket)))
         (attrs `((project . ,project)
                  (ticket  . ,ticket))))
    `(:layout ,lo :defaults ,defs :attributes ,attrs)))


;;;;;;;;;;;;;;;;;;
;; Fetch Ticket

(defvar bts-evernote::find-note-query-book-regexp
  (rx-to-string `(and (or bos (+ space)) "@" (group (* (not space))))))

(defvar bts-evernote::find-note-query-tag-regexp
  (rx-to-string `(and (or bos (+ space)) "#" (group (* (not space))))))

(defvar bts-evernote::find-note-query-date-regexp
  (rx-to-string `(and (or bos (+ space)) ":" (group (+ (any "0-9/-"))))))

(defvar bts-evernote::note-line-regexp
  (rx-to-string `(and bos (* space)
                      (group (+ (any ,bts-evernote::guid-regexp))) (+ space)
                      ":" (+ space)
                      (+ (any "0-9.")) (+ space)
                      (group (+ not-newline))
                      eol)))

(defvar bts-evernote::note-line-book-regexp
  (rx-to-string `(and (+ space) "@" (group (+ (any ,bts-evernote::guid-regexp))) eos)))

(defvar bts-evernote::note-line-tag-regexp
  (rx-to-string `(and (+ space) "#" (group (+ (any ,bts-evernote::guid-regexp))) eos)))

(defvar bts-evernote::find-note-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "<f1>") 'bts-evernote:find-note-popup-help)
    (define-key map [remap minibuffer-keyboard-quit] 'bts-evernote:find-note-quit)
    ;; (ignore-errors
    ;;   (define-key map (nth 0 (where-is-internal 'keyboard-quit)) 'bts-evernote:find-note-quit))
    map))

(defun bts-evernote:find-note-popup-help ()
  (interactive)
  (let ((help "Search value is structured by words,tags,notebooks and date.

Example:
some filter words #TAG1 #TAG2 @NOTEBOOK1 @NOTEBOOK2 :YYYY/MM/DD-MM/DD
"))
    (pos-tip-show help 'bts:widget-tip-face nil nil 300)))

(defvar bts-evernote::find-note-query-in-minibuffer nil)

(defun bts-evernote:find-note-quit ()
  (interactive)
  (bts:ticket-fetch-complete bts-evernote::find-note-query-in-minibuffer nil)
  (call-interactively 'minibuffer-keyboard-quit))

(defvar bts-evernote:ac-source-notebook
  `((candidates . bts-evernote::fetch-notebook-names)
    (prefix . ,bts-evernote::find-note-query-book-regexp)
    (symbol . "b")
    (requires . 0)))

(defvar bts-evernote:ac-source-tag
  `((candidates . bts-evernote::fetch-tag-names)
    (prefix . ,bts-evernote::find-note-query-tag-regexp)
    (symbol . "t")
    (requires . 0)))

(defvar bts-evernote:find-note-query nil)

(defun bts-evernote::find-note-read-query (query)
  (let* ((prompt "Search value ( <f1>:Help  ): ")
         (buf (bts::summary-find-buffer (list query)))
         (init-v (when buf
                   (buffer-local-value 'bts-evernote:find-note-query buf)))
         (ret (minibuffer-with-setup-hook
                  (lambda ()
                    (set (make-local-variable 'bts-evernote::find-note-query-in-minibuffer) query)
                    (when (and (eq bts-evernote:find-note-query-completion 'auto-complete)
                               (featurep 'auto-complete))
                      (add-to-list 'ac-modes 'minibuffer-inactive-mode)
                      (setq ac-sources '(bts-evernote:ac-source-notebook
                                         bts-evernote:ac-source-tag))
                      (auto-complete-mode 1)))
                (read-from-minibuffer prompt init-v bts-evernote::find-note-keymap))))
    (when buf
      (with-current-buffer buf
        (set (make-local-variable 'bts-evernote:find-note-query) ret)))
    ret))

(defun bts-evernote::build-date-option (s)
  ;; TODO:
  nil)

(defun bts-evernote:fetch-issue (project query)
  "Function for `bts:system-ticket-fetcher'."
  (yaxception:$~
    (yaxception:try
      (let* ((query-v (bts-evernote::find-note-read-query query))
             (tags (cl-loop while (string-match bts-evernote::find-note-query-tag-regexp query-v)
                            collect (match-string-no-properties 1 query-v)
                            do (setq query-v (concat (substring query-v 0 (match-beginning 0))
                                                     (substring query-v (match-end 0))))))
             (books (append
                     (when (eq (plist-get query :book-filter) 'on-query)
                       (mapcar 'bts-evernote::get-notebook-name (plist-get query :filtered-books)))
                     (cl-loop while (string-match bts-evernote::find-note-query-book-regexp query-v)
                              collect (match-string-no-properties 1 query-v)
                              do (setq query-v (concat (substring query-v 0 (match-beginning 0))
                                                       (substring query-v (match-end 0)))))))
             (date (when (string-match bts-evernote::find-note-query-date-regexp query-v)
                     (prog1 (bts-evernote::build-date-option (match-string-no-properties 1 query-v))
                       (setq query-v (concat (substring query-v 0 (match-beginning 0))
                                             (substring query-v (match-end 0)))))))
             (on-content (or (eq (plist-get query :target) 'content)
                             (and (eq (plist-get query :target) 'on-fetch)
                                  (y-or-n-p "Do content search? "))))
             (args (append `("find" "--search" ,query-v "--guid" "--with-tags" "--with-notebook")
                           (when tags `("--tags" ,(mapconcat 'identity tags ",")))
                           (when books `("--notebooks" ,(mapconcat 'identity books ",")))
                           (when date `("--date" ,date))
                           (when on-content '("--content-search"))))
             (tasknm (format "find %s" (bts:query-get-unique-string query))))
        (bts:ticket-fetch-complete
         query
         (bts-evernote::with-prefer-cache args
           tasknm nil t nil
           (cl-loop initially (goto-char (point-min))
                    while (not (eobp))
                    for line = (thing-at-point 'line)
                    do (bts--trace* "evernote got result line for note : %s" line)
                    if (string-match bts-evernote::note-line-regexp line)
                    collect (let* ((guid (match-string-no-properties 1 line))
                                   (title (match-string-no-properties 2 line))
                                   book tags)
                              (bts--debug "evernote found note[%s] title : %s" guid title)
                              (when (string-match bts-evernote::note-line-book-regexp title)
                                (bts--debug "evernote found notebook in title : %s"
                                            (setq book (bts-evernote::get-notebook-name
                                                        (match-string-no-properties 1 title))))
                                (bts--debug "evernote remained title : %s"
                                            (setq title (substring title 0 (match-beginning 0)))))
                              (while (string-match bts-evernote::note-line-tag-regexp title)
                                (bts--debug "evernote found tag in note line : %s"
                                            (pushnew (bts-evernote::get-tag-name
                                                      (match-string-no-properties 1 title))
                                                     tags))
                                (bts--debug "evernote remained title : %s"
                                            (setq title (substring title 0 (match-beginning 0)))))
                              `(:guid ,guid :title ,title :tags ,(mapconcat 'identity tags ", ") :book ,book))
                    do (forward-line 1))))))
    (yaxception:catch 'error e
      (bts--error "evernote failed fetch issue from %s of %s : %s"
                  (bts:query-get-config-name query)
                  (bts:project-get-config-name project)
                  (yaxception:get-text e))
      (bts:ticket-fetch-failed query))))


;;;;;;;;;;;;;;;;;;;
;; Regist Ticket

(defvar bts-evernote::next-note-body nil)

(defun bts-evernote::note-body-editor ()
  (bts:awhen bts-evernote::next-note-body
    (yaxception:$~
      (yaxception:try
        (bts--debug "evernote start note body editor")
        (erase-buffer)
        (insert it)
        (save-buffer)
        (server-edit)
        (bts--info "evernote finished note body editor"))
      (yaxception:catch 'error e
        (bts--error "evernote failed note body editor : %s" (yaxception:get-text e))
        (bts:show-message "Failed to regist note to evernote : %s" (yaxception:get-text e)))
      (yaxception:finally
        (setq bts-evernote::next-note-body nil)))))

(add-hook 'server-switch-hook 'bts-evernote::note-body-editor t)

(defun bts-evernote:regist-issue (project ticket diff)
  "Function for `bts:system-ticket-register'."
  (yaxception:$
    (yaxception:try
      (setq bts-evernote::next-note-body (plist-get ticket :body))
      (let* ((guid (plist-get ticket :guid))
             (tag-opt (bts:awhen (plist-get ticket :tags) `("--tags" ,it)))
             (book-opt (bts:awhen (plist-get ticket :book) `("--notebook" ,it)))
             (title-opt (when (or (not diff)
                                  (assoc-default 'title diff))
                          `("--title" ,(plist-get ticket :title))))
             (body-opt '("--content" "WRITE"))
             (cmd-args (cond ((not diff)
                              (append '("create") body-opt title-opt tag-opt book-opt))
                             (t
                              (append `("edit" "--note" ,guid) body-opt title-opt tag-opt book-opt))))
             (procnm (format "bts-evernote regist %s" (or guid "new")))
             (proc (get-process procnm))
             (buf (with-current-buffer (get-buffer-create " *bts-evernote regist*")
                    (erase-buffer)
                    (current-buffer)))
             (cmd (apply 'bts-evernote::build-geeknote-command cmd-args)))
        (when (and (processp proc) (process-live-p proc))
          (error "Old registration is not yet finished."))
        (start-process-shell-command procnm buf cmd)
        (bts--info "evernote called regist note : %s" cmd)
        (copy-sequence ticket)))
    (yaxception:catch 'error e
      (bts--error "evernote failed regist issue : %s" (yaxception:get-text e))
      (yaxception:throw e))))


;;;;;;;;;;;;;;;;;;;
;; Delete Ticket

(defun bts-evernote:delete-issue (project ticket)
  "Function for `bts:system-ticket-eraser'."
  (yaxception:$
    (yaxception:try
      (let* ((guid (plist-get ticket :guid))
             (tasknm (format "remove %s" guid))
             (args `("remove" "--note" ,guid "--force")))
        (bts-evernote::with-prefer-cache args
          tasknm nil t nil
          (goto-char (point-min))
          (if (re-search-forward "^Note has been successful deleted\\." nil t)
              t
            (bts--error "evernote result of delete note[%s] ...\n%s"
                        guid (buffer-string))
            (error "try '%s' in terminal" (apply 'bts-evernote::build-geeknote-command args))))))
    (yaxception:catch 'error e
      (bts--error "evernote failed delete issue : %s" (yaxception:get-text e))
      (yaxception:throw e))))


;;;;;;;;;;;;;;;;;;;;
;; Ticket Summary

(defun bts-evernote:make-summary-format (project query)
  "Function for `bts:system-summary-format'."
  `((:name title :label "Title" :size 40 :sort t)
    (:name book :label "Notebook" :size 20)
    (:name tags :label "Tags")))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Other BTS Function

(defun bts-evernote:make-ticket-unique-string (ticket)
  "Function for `bts:system-ticket-unique-string'."
  (plist-get ticket :guid))


;;;;;;;;;;;;;;;;;;;
;; Regist to BTS

(bts:system-regist
 (make-bts:system
  :name                 'evernote
  :project-view         'bts-evernote:make-project-view
  :query-view           'bts-evernote:make-query-view
  :ticket-single-view   'bts-evernote:make-ticket-single-view
  :ticket-fetcher       'bts-evernote:fetch-issue
  :ticket-register      'bts-evernote:regist-issue
  :ticket-eraser        'bts-evernote:delete-issue
  :ticket-unique-string 'bts-evernote:make-ticket-unique-string
  :summary-format       'bts-evernote:make-summary-format))


(provide 'bts-evernote)
;;; bts-evernote.el ends here
