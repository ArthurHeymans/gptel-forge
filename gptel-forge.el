;;; gptel-forge.el --- Generate PR descriptions for forge using gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Authors
;; SPDX-License-Identifier: Apache-2.0

;; Author: Arthur Heymans
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (forge "0.4") (gptel "0.9.8"))
;; Keywords: vc, convenience
;; URL: https://github.com/ArthurHeymans/gptel-forge

;;; Commentary:

;; This package uses the gptel library to add LLM integration into
;; forge.  Currently, it adds functionality for generating pull-request
;; descriptions when creating PRs via `forge-create-pullreq'.

;;; Code:

(require 'gptel)
(require 'forge)
(require 'forge-post)

(defgroup gptel-forge nil
  "Generate PR descriptions using gptel."
  :group 'forge
  :group 'gptel)

(defconst gptel-forge-prompt-default
  "You are an expert at writing pull request descriptions. Your job is to write a clear, concise PR description that summarizes the changes.

The PR description should include:
- A brief summary of what the changes do (1-2 sentences)
- Key changes or features added
- Any important implementation details worth noting

Guidelines:
- Be concise and to the point
- Focus on the \"why\" and \"what\", not just the \"how\"
- Use bullet points for listing multiple changes
- Do not include the raw diff output in the description
- Only return the PR description in your response, no meta-commentary

Format:
- Start with a title line (without a # prefix)
- Follow with a blank line
- Then the body of the description"
  "Default prompt for generating PR descriptions.")

(defconst gptel-forge-prompt-conventional
  "You are an expert at writing pull request descriptions following conventional commit style. Your job is to write a clear, concise PR description.

The PR title should be structured as:
    <type>(<optional scope>): <description>

Types: build, chore, ci, docs, feat, fix, perf, refactor, style, test

The body should include:
- A brief summary of what the changes do
- Key changes or features (as bullet points if multiple)
- Any breaking changes or important notes

Guidelines:
- Keep the title under 72 characters
- Be concise and focus on the \"why\"
- Do not include raw diff output
- Only return the PR description, no meta-commentary

Format:
- First line is the title (type(scope): description)
- Blank line
- Body with summary and details"
  "Conventional commits style prompt for generating PR descriptions.")

(defcustom gptel-forge-pr-prompt gptel-forge-prompt-default
  "The prompt to use for generating a PR description.
The prompt should consider that the input will be a diff of changes
between the source and target branches."
  :type 'string
  :group 'gptel-forge)

(custom-declare-variable
 'gptel-forge-model nil
 "The gptel model to use, defaults to `gptel-model` if nil.

See `gptel-model` for documentation.

If set to a model that uses a different backend than
`gptel-backend`, also requires `gptel-forge-backend' to be set to
the correct backend."
 :type (get 'gptel-model 'custom-type)
 :group 'gptel-forge)

(custom-declare-variable
 'gptel-forge-backend nil
 "The gptel backend to use, defaults to `gptel-backend` if nil.

See `gptel-backend` for documentation."
 :type (get 'gptel-backend 'custom-type)
 :group 'gptel-forge)

(defvar gptel-forge-rationale-buffer "*gptel-forge Rationale*"
  "Buffer name for entering rationale for PR description generation.")

(defvar-local gptel-forge--current-post-buffer nil
  "Buffer where PR description is being generated.")

(defvar-local gptel-forge--source-branch nil
  "Source branch for PR generation.")

(defvar-local gptel-forge--target-branch nil
  "Target branch for PR generation.")

(defun gptel-forge--request (&rest args)
  "Call `gptel-request` with ARGS.
Respects configured model/backend options."
  (declare (indent 1))
  (let* ((gptel-backend (or gptel-forge-backend gptel-backend))
         (gptel-model (or gptel-forge-model gptel-model)))
    (apply #'gptel-request args)))

(defun gptel-forge--get-diff (source target)
  "Get the diff between SOURCE and TARGET branches for PR."
  (when (and source target)
    (let ((diff (magit-git-output "diff" (format "%s...%s" target source))))
      (if (or (null diff) (string-empty-p diff))
          (error "No diff found between %s and %s" target source)
        diff))))

(defun gptel-forge--generate (source target callback &optional rationale)
  "Generate a PR description for SOURCE to TARGET branches.
Invokes CALLBACK with the generated description when done.
Optional RATIONALE provides context for why the changes were made."
  (let* ((diff (gptel-forge--get-diff source target))
         (prompt (if (and rationale (not (string-empty-p rationale)))
                     (format "Why these changes were made: %s\n\nCode changes:\n%s"
                             rationale diff)
                   diff)))
    (gptel-forge--request prompt
      :system gptel-forge-pr-prompt
      :context nil
      :callback (lambda (response info)
                  (cond
                   (response
                    (funcall callback response))
                   ((plist-get info :error)
                    (message "gptel-forge: Error: %s"
                             (or (plist-get (plist-get info :error) :message)
                                 (plist-get info :error))))
                   (t
                    (message "gptel-forge: Failed to generate PR description (status: %s)"
                             (plist-get info :status))))))))

(defun gptel-forge-generate-description ()
  "Generate a PR description when in the forge post buffer.
This command is available when editing a new pull-request."
  (interactive)
  (unless (and (derived-mode-p 'forge-post-mode)
               (eq forge-edit-post-action 'new-pullreq))
    (user-error "Not in a new pull-request buffer"))
  (let ((source forge--buffer-head-branch)
        (target forge--buffer-base-branch)
        (buf (current-buffer)))
    (unless (and source target)
      (user-error "Source or target branch not set"))
    (message "gptel-forge: Generating PR description...")
    (gptel-forge--generate
     source target
     (lambda (description)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (erase-buffer)
           (insert "# ")
           (save-excursion
             (insert description))))))))

(define-derived-mode gptel-forge-rationale-mode text-mode "gptel-forge-Rationale"
  "Mode for entering PR rationale before generating description."
  (local-set-key (kbd "C-c C-c") #'gptel-forge--submit-rationale)
  (local-set-key (kbd "C-c C-k") #'gptel-forge--cancel-rationale))

(defun gptel-forge--setup-rationale-buffer ()
  "Setup the rationale buffer with proper guidance."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert ";;; WHY are you making these changes? (optional)\n")
    (insert ";;; Press C-c C-c to generate PR description, C-c C-k to cancel\n")
    (insert ";;; Leave empty to generate without rationale\n")
    (insert ";;; ────────────────────────────────────────────────────────\n")
    (add-text-properties (point-min) (point)
                         '(face font-lock-comment-face read-only t))
    (insert "\n")
    (goto-char (point-max))))

(defun gptel-forge--submit-rationale ()
  "Submit the rationale buffer content and proceed with PR description generation."
  (interactive)
  (let ((rationale (string-trim
                    (buffer-substring-no-properties
                     (save-excursion
                       (goto-char (point-min))
                       (while (and (not (eobp))
                                   (get-text-property (point) 'read-only))
                         (forward-char))
                       (point))
                     (point-max))))
        (source gptel-forge--source-branch)
        (target gptel-forge--target-branch)
        (buf gptel-forge--current-post-buffer))
    (quit-window t)
    (message "gptel-forge: Generating PR description with rationale...")
    (gptel-forge--generate
     source target
     (lambda (description)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (erase-buffer)
           (insert "# ")
           (save-excursion
             (insert description)))))
     rationale)))

(defun gptel-forge--cancel-rationale ()
  "Cancel rationale input and abort PR description generation."
  (interactive)
  (quit-window t)
  (message "PR description generation canceled."))

(defun gptel-forge-generate-description-with-rationale ()
  "Generate a PR description with rationale when in the forge post buffer.
This opens a buffer to enter context about why the changes were made,
which helps the LLM generate a better description."
  (interactive)
  (unless (and (derived-mode-p 'forge-post-mode)
               (eq forge-edit-post-action 'new-pullreq))
    (user-error "Not in a new pull-request buffer"))
  (let ((post-buffer (current-buffer))
        (source forge--buffer-head-branch)
        (target forge--buffer-base-branch))
    (unless (and source target)
      (user-error "Source or target branch not set"))
    (let ((buffer (get-buffer-create gptel-forge-rationale-buffer)))
      (with-current-buffer buffer
        (gptel-forge-rationale-mode)
        (gptel-forge--setup-rationale-buffer)
        (setq gptel-forge--current-post-buffer post-buffer)
        (setq gptel-forge--source-branch source)
        (setq gptel-forge--target-branch target))
      (pop-to-buffer buffer))))

;;;###autoload
(defun gptel-forge-install ()
  "Install gptel-forge functionality.
This adds keybindings to `forge-post-mode-map' for generating PR descriptions."
  (define-key forge-post-mode-map (kbd "M-g") #'gptel-forge-generate-description)
  (define-key forge-post-mode-map (kbd "M-r") #'gptel-forge-generate-description-with-rationale))

;;;###autoload
(defun gptel-forge-uninstall ()
  "Uninstall gptel-forge functionality."
  (define-key forge-post-mode-map (kbd "M-g") nil)
  (define-key forge-post-mode-map (kbd "M-r") nil))

(provide 'gptel-forge)
;;; gptel-forge.el ends here
