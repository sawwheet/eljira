;;; eljira.el --- A Jira interface -*- lexical-binding: t; -*-

;; Author: Chris Quinn
;; Version: 0.1.0
;; Package-Requires: (jiralib2 (ox-jira "0.1-SNAPSHOT") (transient "0.4.0") language-detection s)
;; Keywords: tooling, productivity
;; URL: https://github.com/sawwheet/eljira

;;; Commentary:

;; This package provides an interface to Jira

(require 'vtable)
(require 'cl-lib)
(require 'jiralib2)
(require 'ox-jira)
(require 'transient)
(require 'eljira-parser)

(defgroup eljira ()
  "eljira Customization")

(defcustom eljira-token ""
  "Jira token"
  :group 'eljira)

(defcustom eljira-username ""
  "Jira username"
  :group 'eljira)

(defcustom eljira-url ""
  "The jira base url.
     It should be in the format: https://<your-domain>.atlassian.net"
  :group 'eljira)

(defcustom eljira-queries '((:name "My Issues"
                                   :jql "resolution = Unresolved ORDER BY created DESC"
                                   :fields ((:name "Key" :formatter (lambda (value) (propertize value 'face 'bold)))
                                            (:name "Type" :field "issuetype")
                                            (:name "Status" :field "status")
                                            (:name "Summary" :field "summary")
                                            (:name "Reporter" :field "reporter")
                                            (:name "Comments" :field "comment" :hide t)
                                            (:name "Parent" :field "parent" :getter (lambda (issue table)
                                                                                      (let-alist (eljira-issue-fields issue)
                                                                                        .parent.key)))
                                            (:name "Description" :field "description" :hide t))))
  "Each item is a plist with the following properties:

     NAME - string - The name of the query
     JQL - string - The JQL used to fetch jira issues
     FIELDS - plist - The fields that will be fetched, and optionally displayed in the user interface. See below FIELDS section for more information.

     FIELDS
     The fields property is a plist. It extends the vtable columns spec: (info \"(vtable) Making A Table\").
     There are two additional properties to be aware of:
     FIELD - string - required - The name of the jira field to get. Supports custom fields e.g. customfield_10000
     HIDE - boolean - if 't', the field will not be shown in the user-interface, but it will be fetched with the query.

     Example
     (:name \"My Issues\"
      :jql \"project = \"SRE\" AND assignee = currentUser() AND resolution = Unresolved ORDER BY created DESC\"
      :fields (
         (:name \"Key\" :formatter (lambda (value)
                   (propertize value 'face 'bold)))
         (:name \"Type\" :field \"issuetype\")
         (:name \"Status\" :field \"status\")
         (:name \"Summary\" :field \"summary\")
         (:name \"Reporter\" :field \"reporter\")
         (:name \"Points\" :field \"customfield_10000\" :min-width 7
          :formatter (lambda (value)
                 (number-to-string (if value (round value) 0))))
         (:name \"Comments\" :field \"comment\" :hide t)
         (:name \"Parent\" :field \"parent\" :getter (lambda (issue table)
                   (let-alist (eljira-issue-fields issue)
                        .parent.key)))
         (:name \"Description\" :field \"description\" :hide t)))

     "

  )


(defvar-keymap eljira-mode-map
  "<remap> <next-line>" #'eljira-next-line
  "<remap> <previous-line>" #'eljira-previous-line
  "n" #'eljira-next-line
  "p" #'eljira-previous-line
  "h" #'eljira-dispatch
  "C" #'eljira-create-issue
  "G" #'eljira
  "c" #'eljira-dispatch-copy
  "e" #'eljira-dispatch-edit
  "v" #'eljira-dispatch-view
  "b" #'eljira-switch-to-buffer)

(define-derived-mode eljira-mode fundamental-mode "eljira"
  "Major mode for eljira"
  (setq buffer-read-only t))

;;;; Issue

(cl-defstruct eljira-issue
  "Represents a jira issue."
  key
  fields)

(defcustom eljira-issue-types '("Story" "Epic" "Task")
  "A list of possible issue types to create.")

(defcustom eljira-project ""
  "The default project to create an issue in.")

;;;; Helpers
;; Various helper functions that help with wrangling issues

(defalias 'eljira-current-issue 'vtable-current-object)

(defun eljira-current-issues ()
  (vtable-objects (vtable-current-table)))

(defvar eljira--assignee-cache nil)

(defun eljira-assignees (key)
  (if eljira--assignee-cache
      eljira--assignee-cache
    (message "Fetching assignees...")
    (let ((assignees (jiralib2-get-assignable-users key)))
      ;; Only cache accountId and displayName. There is a alot of
      ;; other stuff we don't care about and just slows down the cache
      (setq eljira--assignee-cache
            (seq-map (lambda (assignee)
                       (let-alist assignee
                         `((displayName . ,.displayName)
                           (accountId . ,.accountId))))
                     assignees)))
    eljira--assignee-cache))

(cl-defun eljira--find-assignee (&key name id)
  (seq-find (lambda (a)
              (or (and name (equal (alist-get 'displayName a) name))
                  (and id (equal (alist-get 'accountId a) id))))
            eljira--assignee-cache))

(defun eljira--assignees-names (key)
  (seq-map (lambda (a) (alist-get 'displayName a))
           (eljira-assignees key)))

;; TODO This function is ugly
(defun eljira--update-issue-locally (issue update)
  (pcase (type-of (car update))
    ('string (setf (alist-get (intern-soft (car update)) (eljira-issue-fields issue)) (cdr update)))
    ('symbol (setf (alist-get (car update) (eljira-issue-fields issue)) (cdr update))))
  (vtable-update-object
   (vtable-current-table)
   issue
   (vtable-current-object)))

(defun eljira--update-issue (issue update)
  "update is alist of field to change"
  (let ((key (eljira-issue-key issue)))
    (jiralib2-update-issue key update)
    (eljira--update-issue-locally issue update)
    (message "Updated %s for %s" (car update) key)))

(cl-defun eljira--alist-completing-read (prompt collection &key read return)
  "Given an alist, READ these keys and RETURN this key from chosen"
  (let* ((opts (mapcar (lambda (al) (alist-get read al)) collection))
         (chosen (completing-read prompt opts))
         (got (alist-get return
                         (seq-find (lambda (al) (equal (alist-get read al) chosen))
                                   collection))))
    got))

;;;; Capture

(defun eljira--get-org-heading-content (buf &rest headings)
  "Return an alist of heading and content pairs from org BUF"
  (with-current-buffer buf
    (if (seq-contains headings "*")
        (cons "*" (buffer-string)))
    (org-map-entries
     #'(lambda ()
         (when (seq-contains headings (org-get-heading))
           (cons (substring-no-properties (org-get-heading))
                 (substring-no-properties (org-get-entry))))))))

(defvar-keymap eljira-capture-mode-map
  "C-c C-c" #'eljira-capture-submit
  "C-c C-k" #'eljira-capture-abort)

(define-minor-mode eljira-capture-mode
  "Minor mode for special key bindings in a capture buffer."
  :lighter " EljiraCap"
  :keymap eljira-capture-mode-map)

(defvar-local eljira--capture-submit-fn nil)

(defvar-local eljira--capture-abort-fn nil)

(defun eljira-capture-submit ()
  (interactive)
  (funcall eljira--capture-submit-fn))

(defun eljira-capture-abort ()
  (interactive)
  (funcall eljira--capture-abort-fn))

(defun eljira--make-capture-fn (grab fn then win-config)
  (lambda ()
    (let ((grabbed (apply #'eljira--get-org-heading-content
                          (current-buffer)
                          grab)))
      (funcall fn grabbed)
      (set-window-configuration win-config)
      (funcall then last-input-event))))

(cl-defun eljira--capture (&key name with grab submit abort then)
  "Create a buffer to capture some content from the user

    It handles creating a buffer and restoring the previous window config

    NAME is the name of the buffer to be created
    WITH is a plist: (:header :content :pos)
    :POS is where to put the point in the capture bufer
    :CONTENT is a string that will be put in the buffer
    THEN is called after the user calls an action.
    It is mainly used to be called with the table as the current buffer"
  (let* ((win-config (current-window-configuration))
         (buf (get-buffer-create (format "*eljira capture: %s*" name)))
         (submit-fn (eljira--make-capture-fn grab submit then win-config))
         (abort-fn (eljira--make-capture-fn grab abort then win-config)))
    (with-current-buffer buf
      (org-mode)
      (eljira-capture-mode)
      (erase-buffer)
      (setq-local header-line-format
                  (format "%s | %s to submit %s to abort."
                          (plist-get with :header)
                          (propertize "C-c C-c" 'face 'org-code)
                          (propertize "C-c C-k" 'face 'org-code)))
      (insert (plist-get with :content))
      (if-let* ((p (plist-get with :pos)))
          (goto-char p)
        (goto-char (point-min)))

      (setq-local eljira--capture-submit-fn submit-fn)
      (setq-local eljira--capture-abort-fn abort-fn))

    (display-buffer buf
                    '((display-buffer-full-frame)
                      (inhibit-same-window . t)))))

;;;; Customfields

(defcustom eljira-customfields '()
  "A list of plists. Each plist represents a customfield.

     The format for the plist is:
     NAME - The human readable name of the customfield
     KEY - The single letter that will be used in transient menus
     FIELD - The customfield id. e.g. customfield_10000
     TYPE - The type of customfield. See eljira--customfield-types for supported types

     Customfields show up in transient menus when both create, and edit an
     issue. They are prefixed with ';'.

     If your customfield type is not supported, see eljira-static-customfields.

     ")

(defvar eljira--customfield-types '((string . nil)
                                    (integer . transient-read-number-N+)
                                    (date . transient-read-date)))

(defcustom eljira-static-customfields '()
  "Customfields that do not show up in the transient menus when creating and editing issues.

     Jira supports many customfield types, and some of them are difficult to
     support in eljira, such as cascading fields. For customfield types that are not supported, you can hardcode them here.
     For example, here is a cascading field type:
     (setq eljira-static-customfields `((\"customfield_10000\" . ((\"value\" . \"Internal\")))))")

(defun eljira--find-customfield (field)
  (seq-find (lambda (f) (equal field (plist-get f :field)))
            eljira-customfields))

(defun eljira--parse-customfields (args)
  "Given transient args, returns the customfields.

  example: ((\"customfield_3322\" \"foo\"))"
  (cl-flet* ((customfield-p (field)
               (string-match (rx "customfield_" (one-or-more digit)) field))
             (type (customfield)
               (plist-get (eljira--find-customfield customfield) :type))
             ;; Return ("customfield_3213" "value")
             (parse (field)
               (when (customfield-p field)
                 (let* ((key (match-string 0 field))
                        (value-raw (car (cdr (split-string field "="))))
                        (value (pcase (type key)
                                 ('integer (string-to-number value-raw))
                                 ('date value-raw)
                                 (_ value-raw))))
                   (cons key value)))))
    (remq nil (seq-map #'parse args))))

(defun eljira--build-customfield-suffix (cf)
  "Generate the command to be called when executing customfield as a suffix
  This returns a closure"

  (let* ((field (plist-get cf :field))
         (name (plist-get cf :name))
         ;; See eljira--customfield-types for possibilities
         (reader (pcase (plist-get cf :type)
                   ('string `(read-string (format "%s: " ,name)))
                   ('integer `(read-number (format "%s: " ,name)))
                   ('date '(org-read-date)))))

    ;; Return a closure
    #'(lambda ()
        (interactive)
        (let* ((issue (eljira-current-issue))
               (key (eljira-issue-key issue))
               (choice (eval reader)))
          (eljira--update-issue issue `(,field . ,choice))))))

(defun eljira--build-customfield-transient-group (fn)
  "FN should take in a customfield and return a transient *fix"
  (list (transient-parse-suffix 'eljira-create-issue-options
                                (vconcat (mapcar fn eljira-customfields)))))

;;;; Create

;; TODO: Be smarter about whether it is a child issue or not.
;; TODO: Create custom transient class for maps such as : (("value" . "internal"))

(defvar eljira-create-issue-header
  (format "Creating issue. %s to submit. %s to cancel"
          (propertize "C-c C-c" 'face 'help-key-binding)
          (propertize "C-c C-k" 'face 'help-key-binding))
  "The header for the create issue buffer")

(defun eljira--create-issue-abort ()
  (set-window-configuration eljira--previous-window-config))

(defun eljira--create-issue-confirm ()
  (eljira-create-issue-options))

(defun eljira--create-issue-buffer ()
  "Creates a buffer suited for creating an issue and returns it"
  (let ((buf (get-buffer-create "*eljira create issue*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (setq-local header-line-format eljira-create-issue-header)
      (insert "* Summary\n* Description"))
    buf))

(defun eljira--parse-create-issue-buffer (buf)
  "Parses BUF as if it were eljira--create-issue-buffer"
  (with-current-buffer buf
    (org-map-entries
     #'(lambda ()
         (cons (substring-no-properties (org-get-heading))
               (substring-no-properties (org-get-entry)))))))

(defun eljira--validate-create-issue-content (content)
  (cl-flet ((sanitize-and-validate (content)
              (with-temp-buffer
                (insert (cdr content))
                (goto-char (point-min))
                (while (re-search-forward "\n" nil t)
                  (replace-match ""))
                (when (string= (buffer-string) "")
                  (user-error
                   (format "%s is required for creating an issue" (car content)))))))

    (mapc #'sanitize-and-validate content)))

;; HACK: Creating an issue is a hack
;; I hook in to eljira-capture-mode to set the keybindings.
;; I tried eljira-capture to work with the whole transient thing but I'm too stupid
(defun eljira-create-issue ()
  "Create a new Jira issue"
  (interactive)
  (let ((win-config (current-window-configuration))
        (buf (eljira--create-issue-buffer)))
    (with-current-buffer buf
      (eljira-capture-mode)
      (setq-local eljira--capture-submit-fn #'eljira--create-issue-confirm)
      (setq-local eljira--capture-abort-fn #'eljira--create-issue-abort))
    (display-buffer buf '((display-buffer-full-frame)
                          (inhibit-same-window . t)))
    (pop-to-buffer buf)
    (setq eljira--previous-window-config win-config)
    (goto-char (point-min))
    (end-of-line)
    (newline)))

(transient-define-suffix eljira-submit-issue (args)
  "Finalizes the create issue buffer"
  (interactive (list (transient-args transient-current-command)))
  (let* ((content (eljira--parse-create-issue-buffer (current-buffer)))
         (project (transient-arg-value "--project=" args))
         (type (transient-arg-value "--type=" args))
         (assignee (alist-get 'accountId
                              (eljira--find-assignee :name (transient-arg-value "--assignee=" args))))
         (customfields (append eljira-static-customfields
                               (list `("assignee" . (("id" . ,assignee))))
                               (eljira--parse-customfields args))))

    (eljira--validate-create-issue-content content)

    (let ((i (apply 'jiralib2-create-issue
                    project
                    type
                    (cdr (assoc "Summary" content))
                    (cdr (assoc "Description" content))
                    customfields
                    )))
      (message "Created: %s/browse/%s" eljira-url (alist-get 'key i)))

    (set-window-configuration eljira--previous-window-config)
    (kill-buffer "*eljira create issue*")))

(transient-define-prefix eljira-create-issue-options (issue)
  :value (list
          (format "--type=%s" (car eljira-issue-types))
          (format "--project=%s" eljira-project))

  [["Overview"
    ("t" "Issue Type" "--type="
     :class transient-option
     :prompt "Issue Type: "
     :choices (lambda () eljira-issue-create-issue-types)
     :always-read t)
    ("p" "Project" "--project="
     :class transient-option
     :always-read t
     :prompt "Project: ")
    ("a" "Assignee" "--assignee="
     :class transient-option
     :prompt "Assignee: "
     :choices (lambda () (seq-map (lambda (assignee)
                                    (alist-get 'displayName assignee))
                                  eljira--assignee-cache)))]]

  ["Custom Fields"
   :class transient-columns
   :setup-children (lambda (_)
                     (eljira--build-customfield-transient-group
                      (lambda (cf)
                        (list (format ";%s" (plist-get cf :key))
                              (plist-get cf :name)
                              (format "--%s=" (plist-get cf :field))
                              :prompt (format "%s: " (plist-get cf :name))
                              :reader (alist-get (plist-get cf :type) eljira--customfield-types)))))]

  ["Submit"
   [("RET" "Create Issue" eljira-submit-issue)
    ("e" "Args" transient-echo-arguments)]])

;;;; Actions

(defvar eljira--actions '())

(defun eljira--make-action (group name spec)
  (list group (list name spec)))

(defun eljira--find-action (action)
  "Use eljira--make-action to generate ACTION"
  (alist-get (caadr action) (alist-get (car action) eljira--actions)))

(defun eljira--add-action (action)
  "Updates eljira--actions with action
  Does not update in place. Use eljira--set-action"
  (cl-flet ((current-action-p (a)
              (eq (car a) (caadr action))))
    (cons (cadr action)
          (cl-remove-if #'current-action-p (alist-get (car action) eljira--actions)))))

(defun eljira--set-action (action)
  "Update eljira--actions in place"
  (setf (alist-get (car action) eljira--actions)
        (eljira--add-action action)))

;; TODO: Document
;; TODO: Handle docstrings better. rn if there is no docstring macro does not work
(defmacro eljira-defaction (action spec docstring &rest body)
  "Create a new function to act on issues. Use SPEC to dynamically bind to
  transient menus.
  ACTION will be appeneded to 'eljira-action-' and become a function
  SPEC is the form (group key description)
  DOCSTRING is the doc string for the new function
  BODY is what the function does
  "
  (declare (indent 2) (debug t) (doc-string 3))
  `(progn
     (defun ,(intern (format "eljira-action-%s" (symbol-name action))) ()
       ,docstring
       (interactive)
       (let* ((issue (eljira-current-issue))
              (key (eljira-issue-key issue)))
         ,@body
         (when (vtable-current-table)
           (eljira-create-context eljira-current-context))
         ))
     (eljira--set-action
      (eljira--make-action ,(car spec)
                           (quote ,action)
                           (list ,(nth 1 spec)
                                 ,(nth 2 spec)
                                 (quote ,(intern-soft (format "eljira-action-%s" (symbol-name action)))))))))

(eljira-defaction add-parent ('edit "p" "Parent")
  "Add a parent"
  (let ((parent (completing-read "Parent: "
                                 (mapcar (lambda (issue) (eljira-issue-key issue))
                                         (eljira-current-issues)))))
    (jiralib2-session-call (format "/rest/api/2/issue/%s" key)
                           :type "PUT"
                           :data (json-encode `(("fields" . (("parent" . (("key" . ,parent))))))))
    (message "Made %s a child of %s" key parent)))

(eljira-defaction link-issue ('edit "l" "Link")
  "Link issue"
  (let* ((to (completing-read "Link to: " (mapcar (lambda (issue)
                                                    (eljira-issue-key issue))
                                                  (eljira-current-issues))))
         (link-type (eljira--alist-completing-read "Link type: "
                                                   (alist-get 'issueLinkTypes example)
                                                   :read 'inward
                                                   :return 'name)))
    (jiralib2-session-call (format "/rest/api/2/issueLink")
                           :type "POST"
                           :data (json-encode `(("inwardIssue" . (("key" . ,to)))
                                                ("outwardIssue" . (("key" . ,key)))
                                                ("type" . (("name" . ,link-type))))))))

(defun eljira-issue-link-types ()
  (jiralib2-session-call (format "/rest/api/2/issueLinkType")))

(eljira-defaction edit-summary ('edit "s" "Summary")
  "Edit the summary of issue at point"
  (let* ((current-summary (eljira--field-display-value issue "summary"))
         (summary (read-string "Summary: " current-summary)))
    (eljira--update-issue issue `(summary . ,summary))))

;; jiralib2 version doesn't work because we can only use accountId now
(defun eljira-issue-assign (key account-id)
  (jiralib2-session-call (format "/rest/api/2/issue/%s/assignee" key)
                         :type "PUT"
                         :data (json-encode `(("accountId" . ,account-id)))))

(eljira-defaction edit-assignee ('edit "a" "Assignee")
  "Edit the assignee of issue at point"
  (let* ((assignee (completing-read "Assignee: " (eljira--assignees-names key)))
         (assignee-id (alist-get 'accountId (eljira--find-assignee :name assignee))))
    (eljira-issue-assign key assignee-id)
    (eljira--update-issue-locally issue `(assignee . ,assignee))
    (message "%s assigned to: %s" key assignee)))

(eljira-defaction add-comment ('edit "c" "Comment")
  "Add comment to issue at point"
  (let ((comments (let-alist (eljira-issue-fields issue) .comment.comments))
        (header (format "Commenting on %s" key))
        (cb (lambda (called)
              (unless (eq called 107) ;; 107 is c-c c-k
                (eljira--update-issue-locally
                 (eljira-current-issue)
                 `(comment . ,(let-alist (jiralib2-get-issue key) .fields.comment))))
              (eljira-create-context eljira-current-context)))
        (content)
        (pos))

    ;; Get content
    (with-temp-buffer
      (insert (concat "* New comment\n\n"
                      (when comments (eljira--jira-comments-to-org comments))))
      (goto-char (point-min))
      (next-line)
      (setq pos (point))
      (setq content (buffer-string)))

    (eljira--capture :name (format "comment on %s" key)
                     :with `(:header ,header :content ,content :pos ,pos)
                     :grab '("New comment")
                     :submit (lambda (grabbed)
                               (let ((cmt (eljira-parser-org-to-jira (cdar grabbed))))
                                 (jiralib2-add-comment key cmt)
                                 (message "Added comment to %s" key)))
                     :abort (lambda (_) (message "Aborted comment"))
                     :then cb)))

(eljira-defaction edit-description ('edit "d" "Description")
  "Edit the issue description"
  (let ((header (format "Editing description on" key))
        (content (alist-get 'description (eljira-issue-fields issue)))
        (cb (lambda (called)
              (unless (eq called 107) ;; 107 is c-c c-k
                (eljira--update-issue-locally
                 (eljira-current-issue)
                 `(description . ,(let-alist (jiralib2-get-issue key) .fields.description))))
              (eljira-create-context eljira-current-context))))


    (eljira--capture :name (format "editing description on %s" key)
                     :with `(:header ,header :content ,content)
                     :submit (lambda (grabbed)
                               (let ((desc (eljira-parser-org-to-jira (buffer-string))))
                                 (jiralib2-update-issue key `(description . ,desc))
                                 (message "Edited description on %s" key)
                                 `(description . ,desc)))
                     :abort (lambda (_) (message "Aborted description"))
                     :then cb)))

(eljira-defaction transition-issue ('edit "m" "Status [m]ove")
  "Transition 'issue' to a new state."
  (let* ((transitions (jiralib2-get-actions key))
         (transition-names (mapcar #'cdr transitions))
         (choice (completing-read "Transition: " transition-names))
         (chosen-transition (rassoc choice transitions)))
    (message "Transitioning...")
    (jiralib2-do-action key (car chosen-transition))
    (let ((new-status (let-alist (jiralib2-get-issue key) .fields.status.name)))
      (eljira--update-issue-locally issue `(status . ,new-status)))))

(eljira-defaction transition-issue-continuously ('edit "M" "Status [M]ove Loop")
  "Tranisition issue until the user quits with C-g"
  (cl-loop
   (message "Fetching transitions for %s" key)
   (let* ((transitions (jiralib2-get-actions key))
          (transition-names (mapcar #'cdr transitions))
          (choice (completing-read "Transition: " transition-names))
          (chosen-transition (rassoc choice transitions)))
     (message "Transitioning...")
     (jiralib2-do-action key (car chosen-transition))
     (let ((new-status (let-alist (jiralib2-get-issue key) .fields.status.name)))
       (eljira--update-issue-locally issue `(status . ,new-status))
       (message "Transistioned %s to %s" key choice)))))

(eljira-defaction sort-table ('view "s" "Sort Table")
  "Sort the table by a column"
  (let* ((columns (vtable-columns (vtable-current-table)))
         (column-names (seq-map (lambda (col)
                                  (vtable-column-name col))
                                columns))
         (choice (completing-read "Sort By: " column-names)))
    (beginning-of-line)
    (while (and (not (eolp))
                (not
                 (equal (vtable-column
                         (vtable-current-table)
                         (vtable-current-column))
                        choice)))
      (vtable-next-column))
    (vtable-sort-by-current-column)
    (beginning-of-line)
    (beginning-of-buffer)))

(eljira-defaction filter-table ('view "f" "Filter table")
  "Filter the table by a column"
  (let* ((columns (vtable-columns (vtable-current-table)))
         (column-names (seq-map (lambda (col) (vtable-column-name col)) columns))
         (filtered-by (completing-read "Filter by: " column-names))
         (filter (read-string "Filter: ")))

    (cl-loop for issue in (vtable-objects (vtable-current-table))
             for colname = (downcase filtered-by)
             for col-jira-field-name = (eljira--jira-field-name-from-column colname)
             for content = (eljira--field-display-value issue col-jira-field-name)
             unless (if content (string-match-p filter content) nil)
             do (vtable-remove-object (vtable-current-table) issue))))

(eljira-defaction browse-issue ('view "o" "Open Issue")
  "Open issue in your external browser"
  (browse-url (format "%s/browse/%s" eljira-url key)))

;; TODO: revisist the name and buffer created
(eljira-defaction get-linked-issues ('view "l" "Linked Issues")
  "Run a query to get linked issues to issue at point"
  (let* ((query (make-eljira-query
                 :name (format "linked %s" key)
                 :jql (format "issue in linkedIssues(\"%s\") OR issue in portfolioChildIssuesOf(\"%s\")" key key)
                 :fields (eljira-query-fields eljira--current-query)
                 :buffer (get-buffer-create (format "*eljira query - linked %s*" key)))))
    (eljira-display-query query)))

(eljira-defaction copy-issue-key ('copy "k" "Issue Key")
  "Copy issue key"
  (kill-new key)
  (message "Copied \"%s\" to kill ring" key))

(eljira-defaction copy-issue-url ('copy "u" "Issue URL")
  "Copy issue url"
  (let ((url (format "%s/browse/%s" eljira-url key)))
    (kill-new url)
    (message "Copied \"%s\" to kill ring" url)))

(defun eljira--setup-actions-children (prefix group)
  "Create children for PREFIX for GROUP"
  (list (transient-parse-suffix
         prefix
         (vconcat (mapcar #'cadr (alist-get group eljira--actions))))))

(transient-define-prefix eljira-dispatch-edit (issue)
  ["Field"
   :class transient-columns
   :setup-children (lambda (_) (eljira--setup-actions-children 'eljira-dispatch-edit 'edit))]
  ["Custom Fields"
   :class transient-columns
   :setup-children (lambda (_)
                     (eljira--build-customfield-transient-group
                      (lambda (cf)
                        (list (format ";%s" (plist-get cf :key))
                              (plist-get cf :name)
                              (eljira--build-customfield-suffix cf)))))])

(transient-define-prefix eljira-dispatch-copy (issue)
  ["Copy"
   :class transient-columns
   :setup-children (lambda (_) (eljira--setup-actions-children 'eljira-dispatch-copy 'copy))])

(transient-define-prefix eljira-dispatch-view (issue)
  ["View"
   :class transient-columns
   :setup-children (lambda (_) (eljira--setup-actions-children 'eljira-dispatch-view 'view))]
  ["Context"
   ("c" "Context" eljira-select-context)
   ])

(transient-define-prefix eljira-dispatch (issue)
  [["Edit" ("e" "Edit" eljira-dispatch-edit)]
   ["View" ("v" "View" eljira-dispatch-view)]
   ["Copy" ("c" "Copy" eljira-dispatch-copy)]]
  ["Create" ("C" "Create Issue" eljira-create-issue)]
  )

;;;; Query

;; This is the core component of eljira. The user constructs a query,
;; eljira takes that query and presents an emacs buffer with a table
;; representing said query

(cl-defstruct eljira-query
  name
  jql
  fields
  buffer)

(defun eljira-queries ()
  "Convert the user facing eljira-queries to eljira-query structs."
  (mapcar (lambda (query)
            (make-eljira-query
             :name (plist-get query :name)
             :jql (plist-get query :jql)
             :fields (plist-get query :fields)
             :buffer (get-buffer-create (format "*eljira query - %s*" (plist-get query :name)))))
          eljira-queries))

(defun eljira-query-jira-fields (query)
  "Given QUERY return the jira fields.

  The jira fields are part of the columns. So we have to find them ourselves"
  (mapcar (lambda (column) (plist-get column :field)) (eljira-query-fields query)))

(defun eljira-select-query ()
  "Prompt the user to select a query based on query names"
  (completing-read
   "Query: "
   (seq-map (lambda (query)
              (plist-get query :name))
            eljira-queries)))

(defun eljira-find-query (name)
  "Find query matching NAME"
  (seq-find (lambda (query)
              (equal (eljira-query-name query) name))
            (eljira-queries)))

(defun eljira-submit-query (query)
  "Get issues returned by the 'jql' slot in QUERY"
  (let* ((fields (eljira-query-jira-fields query))
         (jql (eljira-query-jql query))
         (issues (apply 'jiralib2-jql-search jql fields)))
    (seq-map (lambda (issue)
               (make-eljira-issue
                :fields (alist-get 'fields issue)
                :key (alist-get 'key issue)))
             issues)))

(defun eljira-display-query (query)
  "Create a vtable to display QUERY.

  QUERY is an eljira-query."
  (let* ((inhibit-read-only t)
         (buf (eljira-query-buffer query))
         (columns (eljira-query-fields query))
         (vtable-columns (cl-remove-if #'null
                                       (mapcar #'eljira--sanitize-column-for-vtable columns))))

    (with-current-buffer buf
      (eljira-mode)
      (setq-local eljira--current-query query)
      (erase-buffer)
      (hl-line-mode)
      (make-vtable
       :columns vtable-columns
       :separator-width 5
       :face 'default
       :objects-function (lambda ()
                           (eljira-submit-query eljira--current-query))
       :getter #'eljira--vtable-getter))
    (display-buffer (eljira-query-buffer query)
                    '((display-buffer-full-frame)
                      (inhibit-same-window . t)))))

;;;; Utility Functions

;; Various utility functions used by eljira

;; TODO: This can be better

(defun eljira--field-display-value (issue field)
  "Return the display name of FIELD for the given ISSUE.

  Many fields are alists. However when constructing a table, we usually
  want what jira calls a displayName, or sometimes just name"

  (let* ((fields (eljira-issue-fields issue))
         (field-obj (alist-get (intern field) fields)))
    (pcase field-obj
      ((or
        (pred stringp)
        (pred numberp))
       field-obj)
      ((pred (alist-get 'displayName))
       (alist-get 'displayName field-obj))
      ((pred (alist-get 'name))
       (alist-get 'name field-obj))
      (_ field-obj))))

(defun eljira--jira-field-name-from-column (colname)
  "Given COLNAME return the associated jira field name

  When a user constructs a query, we ask for them to specify the name of
  the columns, and the jira field they want to be associated with that
  column name.  This function returns that association when given COLNAME"
  (plist-get
   (seq-find
    (lambda (f)
      (equal (downcase (plist-get f :name)) colname))
    (eljira-query-fields eljira--current-query))
   :field))

(defun eljira--vtable-getter (issue column vtable)
  "Returns the value that should be displayed in COLUMN

  - Get the name of the column at index COLUMN
  - Return what should be displayed for the issue in said column

  ISSUE is an eljira-issue
  COLUMN is the current column index
  VTABLE is the current vtable"
  (let ((colname (downcase (vtable-column vtable column))))
    (pcase colname
      ("key" (eljira-issue-key issue))
      (column-name (eljira--field-display-value
                    issue
                    (eljira--jira-field-name-from-column column-name))))))

(defun eljira--sanitize-column-for-vtable (column)
  "Return a plist compatible with vtable.

  Because we extend the vtable column struct, we have to have a method of
  putting it back to something vtable understands."
  (when (not (plist-get column :hide))
    (let ((copy (copy-sequence column)))
      (map-delete copy :field))))

;;;; Context

;; The context window displays a single piece of information related
;; to the issue at point in the main window

(defun eljira--jira-comments-to-org (comments)
  "Return comments as an Org mode string"
  (if comments 
      (mapconcat (lambda (comment)
                   (let-alist comment
                     (format "* %s -- %s\n%s"
                             .author.displayName
                             (format-time-string "%Y-%m-%d" (date-to-time .created))
                             (format "%s\n" (eljira-parser-jira-to-org .body)))))
                 (reverse comments) "")
    nil))

(defvar eljira-context-buffer "*eljira context*")

(defvar eljira-current-context nil)

(cl-defstruct eljira-context
  name
  header
  body)

(defcustom eljira-contexts `(,(make-eljira-context
                               :name "Description"
                               :header '(("Key" . "key") ("Reporter" . "reporter") ("Assignee" . "assignee"))
                               :body (lambda (issue)
                                       (or (let-alist (eljira-issue-fields issue)
                                             (eljira-parser-jira-to-org .description))
                                           "No description for issue")))
                             ,(make-eljira-context
                               :name "Comments"
                               :header '(("Key" . "key") ("Reporter" . "reporter") ("Assignee" . "assignee"))
                               :body (lambda (issue)
                                       (or (let-alist (eljira-issue-fields issue)
                                             (eljira--jira-comments-to-org .comment.comments))
                                           "No comments for issue"))))
  "Contexts that can be set for the issue at point")

(defun eljira--format-context-header (issue ctx)
  (cl-loop for (k . v) in (eljira-context-header ctx)
           concat (format "%s: %s | " (propertize k 'face 'success)
                          (if (equal v "key")
                              (eljira-issue-key issue)
                            (eljira--field-display-value issue v)))))

(cl-defmethod eljira-create-context (ctx)
  (let* ((issue (vtable-current-object))
         (header (when (eljira-context-header ctx)
                   (eljira--format-context-header issue ctx))))
    (with-current-buffer (get-buffer-create eljira-context-buffer)
      (org-mode)
      (erase-buffer)
      (when header (setq-local header-line-format header))
      (if-let* ((content (funcall (eljira-context-body ctx) issue)))
          (insert content)
        (insert "Not found")))))

(cl-defmethod eljira-create-context :after (ctx)
  (display-buffer eljira-context-buffer
                  '((display-buffer-below-selected)
                    (window-height . 0.4)))
  (setq eljira-current-context ctx))

(defun eljira-find-context (name)
  "Find the context matching NAME."
  (cl-find name eljira-contexts :key #'eljira-context-name :test #'equal))

(defun eljira--set-context (ctx)
  (setq eljira-current-context ctx))

(defun eljira-select-context ()
  (interactive)
  (let ((ctx (completing-read "Context: " (mapcar #'eljira-context-name eljira-contexts) nil t)))
    (-> ctx
        (eljira-find-context)
        (eljira--set-context)
        (eljira-create-context))))

;;;; Navigation

(defun eljira--eotp ()
  "Returns t if at the end of the table"
  (let ((pos (point))
        (eot (save-excursion
               (vtable-end-of-table)
               (previous-line)
               (point))))

    (when (equal (point) eot)
      t)))

(defun eljira-next-line ()
  (interactive)
  (if (eljira--eotp)
      (message "End of table")
    (next-line)
    (eljira-create-context eljira-current-context)))

(defun eljira-previous-line ()
  (interactive)
  (previous-line)
  (eljira-create-context eljira-current-context))

(defun eljira-switch-to-buffer ()
  "Switch to an eljira buffer"
  (interactive)
  (let* ((eljira-buffers (cl-remove-if-not
                          (lambda (buf)
                            (string-prefix-p "*eljira query" (buffer-name buf)))
                          (buffer-list)))
         (selected-buffer (completing-read
                           "Buffer: " 
                           (mapcar #'buffer-name eljira-buffers))))
    (switch-to-buffer selected-buffer)
    (eljira-create-context eljira-current-context)))

;;;; Program

(defun eljira--init ()
  (unless eljira-token
    (user-error "Must set eljira auth information"))
  (setq jiralib2-url eljira-url)
  (setq jiralib2-auth 'basic)
  (jiralib2-session-login eljira-username eljira-token))

;;;###autoload
(defun eljira ()
  (interactive)
  (eljira--init)
  (eljira-display-query (eljira-find-query (eljira-select-query)))
  (setq eljira-current-context (car eljira-contexts))
  (eljira-create-context eljira-current-context))

(provide 'eljira)
