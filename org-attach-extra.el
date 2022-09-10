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
            (all (org-attach-file-list att-dir))
            (used (mapcar
                   (lambda (x) (plist-get x :path))
                   (save-restriction
                     (widen)
                     (org-attach-extra-get-links-for-dir att-dir)))))
      (if-let* ((unused (cl-set-difference all used :test #'string=)))
          (dolist (file (completing-read-multiple "Unlinked attachments" unused))
            (let ((path (expand-file-name file att-dir)))
              (delete-file path)))
        (message "There are no unlinked attachments in the context at point."))
    (message "There is no attachment folder for the heading at point.")))

(defun org-attach-extra--refile-to (att-dir file pos)
  (let ((all (org-attach-file-list att-dir)))
    (with-current-buffer (find-file-noselect file t)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (or pos (point-min)))
          (if-let* ((target-att-dir (org-attach-dir)))
              (dolist (file (completing-read-multiple "Attachments to refile to this target:" all))
                (let ((path (expand-file-name file att-dir))
                      (target-path (expand-file-name file target-att-dir)))
                  (rename-file path target-path)))
            (message "Refile target does not have an org-attach dir.")))))))

(defun org-attach-extra-refile ()
  "Opens a CRM prompt for refiling attachments in the `org-attach' context at point."
  (interactive)
  (if-let ((att-dir (org-attach-dir)))
      (let* ((target (org-refile-get-location))
             (file (nth 1 target))
             (pos (nth 3 target)))
        (org-attach-extra--refile-to att-dir file pos))
    (message "There is no attachment folder for the heading at point.")))

(defun org-attach-extra-refile-to-roam-node ()
  "Opens a CRM prompt for refiling attachments in the `org-attach' context at point, with an org-roam node as target."
  (interactive)
  (require 'org-roam)
  (if-let ((att-dir (org-attach-dir)))
      (let* ((target (org-roam-node-read))
             (file (org-roam-node-file target))
             (pos (org-roam-node-point target)))
        (org-attach-extra--refile-to att-dir file pos))
    (message "There is no attachment folder for the heading at point.")))

(provide 'org-attach-extra)

;;; org-attach-extra.el ends here
