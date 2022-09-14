;;; org-attach-extra.el --- Useful tools for dealing with org-attach files  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun org-attach-extra-open-in-treemacs ()
  "Show the `org-attach-dir' in treemacs."
  (interactive)
  (require 'treemacs)
  (if-let* ((att-dir (org-attach-dir)))
      (progn
        (treemacs-select-window)
        (treemacs-goto-file-node att-dir))
    (message "No attachment folder for node.")))

(defun org-attach-extra-get-links-for-dir (dir)
  "Find all attachment links in the visible buffer whose `org-attach-dir's are equal to DIR."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward "\\[\\[attachment:" nil t)
             if (string= (org-attach-dir) dir)
             collect (save-excursion
                       (goto-char (match-beginning 0))
                       (cadr (org-element-link-parser))))))

(defun org-attach-extra-delete-unlinked ()
  "Opens a CRM prompt for deleting attachments in the current `org-attach-dir' that are not referenced by any links in the widened buffer."
  (interactive)
  (if-let* ((att-dir (org-attach-dir))
            (used (mapcar
                   (lambda (x) (plist-get x :path))
                   (save-restriction
                     (widen)
                     (org-attach-extra-get-links-for-dir att-dir)))))
      (if-let* ((all (org-attach-file-list att-dir))
                (unused (cl-set-difference all used :test #'string=)))
          (dolist (file (completing-read-multiple "Unlinked attachments" unused))
            (let ((path (expand-file-name file att-dir)))
              (delete-file path)))
        (message "There are no unlinked attachments in the context at point."))
    (message "There is no attachment folder for the heading at point.")))

(defun org-attach-extra--refile-to (att-dir all file pos)
  (with-current-buffer (find-file-noselect file t)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (or pos (point-min)))
        (if-let* ((target-att-dir (org-attach-dir)))
            (dolist (file all)
              (let ((path (expand-file-name file att-dir))
                    (target-path (expand-file-name file target-att-dir)))
                (rename-file path target-path)))
          (message "Refile target does not have an org-attach dir."))))))

(defun org-attach-extra-refile ()
  "Opens a CRM prompt for refiling attachments in the `org-attach' context at point."
  (interactive)
  (if-let ((att-dir (org-attach-dir)))
      (let* ((all (completing-read-multiple
                   "Attachments to refile to this target:"
                   (org-attach-file-list att-dir)))
             (target (org-refile-get-location))
             (file (nth 1 target))
             (pos (nth 3 target)))
        (org-attach-extra--refile-to att-dir all file pos))
    (message "There is no attachment folder for the heading at point.")))

(defun org-attach-extra-refile-to-roam-node ()
  "Opens a CRM prompt for refiling attachments in the `org-attach' context at point, with an org-roam node as target."
  (interactive)
  (require 'org-roam)
  (if-let ((att-dir (org-attach-dir)))
      (let* ((all (completing-read-multiple
                   "Attachments to refile to this target:"
                   (org-attach-file-list att-dir)))
             (target (org-roam-node-read))
             (file (org-roam-node-file target))
             (pos (org-roam-node-point target)))
        (org-attach-extra--refile-to att-dir all file pos))
    (message "There is no attachment folder for the heading at point.")))

(defun org-attach-extra-add-id ()
  "Add ID for current heading and move all previously inherited attachments into the new attachment directory of current heading."
  (interactive)
  (when (org-id-get) (error "Current heading already has ID"))
  (if-let ((att-dir (org-attach-dir)))
      (let ((all (save-restriction
                  (org-narrow-to-subtree)
                  (mapcar
                   (lambda (x) (plist-get x :path))
                   (org-attach-extra-get-links-for-dir att-dir)))))
        (when (yes-or-no-p
               (format "The following attachments will be moved:\n%s\nProceed?"
                       (string-join all "\n")))
          (org-id-get-create)
          (org-attach-dir-get-create)
          (let* ((file (buffer-file-name))
                 (pos (point)))
            (org-attach-extra--refile-to att-dir all file pos))))
    (message "There is no attachment folder for the heading at point.")))

(defun org-attach-extra-remove-id ()
  "Remove ID for current heading and move all previously inherited attachments into the new attachment directory of current heading.

After moving, also deletes the previous attachment directory when it is empty."
  (interactive)
  (unless (org-id-get) (error "Cannot find the existent ID of current heading"))
  (if-let* ((att-dir (org-attach-dir)))
      (let ((all (org-attach-file-list att-dir)))
        (when (yes-or-no-p
               (format "The following attachments will be moved:\n%s\nProceed?"
                       (string-join all "\n")))
          (when (org-entry-delete (point) "ID")
            (org-id-update-id-locations nil 'silent))
          (let* ((file (buffer-file-name))
                 (pos (point)))
            (org-attach-extra--refile-to att-dir all file pos))
          (when (directory-empty-p att-dir)
            (delete-directory att-dir))))
    (message "There is no attachment folder for the heading at point.")))

(defun org-attach-extra-change-id ()
  "Modify the ID for current heading and move attachments accordingly."
  (interactive)
  (if-let ((att-dir (org-attach-dir)))
      (let* ((all (org-attach-file-list att-dir))
             (new-id (read-string "New ID:")))
        (when (yes-or-no-p
               (format "The following attachments will be moved:\n%s\nProceed?"
                       (string-join all "\n")))
          (when (org-entry-put (point) "ID" new-id)
            (org-id-update-id-locations nil 'silent))
          (org-attach-dir-get-create)
          (let* ((file (buffer-file-name))
                 (pos (point)))
            (org-attach-extra--refile-to att-dir all file pos))
          (when (directory-empty-p att-dir)
            (delete-directory att-dir))))
    (message "There is no attachment folder for the heading at point.")))

(provide 'org-attach-extra)

;;; org-attach-extra.el ends here
