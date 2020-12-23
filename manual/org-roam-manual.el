;;; org-roam-manual.el — Build a PDF manual based on an org roam directory

;;; Commentary:

;; This provides a simple way to build a PDF based on the org files
;; collected in ‘org-roam-directory’.  The contents and order are
;; controlled by ‘files-to-combine’.  (Sample content for the exp2exp
;; wiki is provided.)

;;; Code:

(defun downsample ()
  "Process an Org Roam buffer for inclusion in a standard Org file.
Changes title to header, and increase indentation of existing headers.
Changes file links to internal links."
  (if (looking-at "^#\\+TITLE:")
      (replace-match "*"))
  (forward-line 1)
  (if (looking-at "^#\\+roam_tags:\\(.*\\)")
      (replace-match ":PROPERTIES:
  :tag:\\1
  :END:"))
  (while (re-search-forward "^\\*" nil t)
    (replace-match "**"))
  (goto-char (point-min))
  (while (re-search-forward "\\[\\[file:\\([^]]*\\)\\]\\[\\([^]]*\\)\\]\\]" nil t)
    (replace-match "[[*\\2][\\2]]"))
  (buffer-substring-no-properties (point-min) (point-max)))

(defun combine-org-roam-files (&rest args)
"Combine a list of files, specified as ARGs.
The files are to be found in `org-roam-directory'."
  (apply #'concat
         (mapcar (lambda (file)
                   (save-window-excursion
                     (find-file (concat org-roam-directory file))
                     (let ((contents (buffer-substring-no-properties (point-min)
                                                                     (point-max))))
                       (with-temp-buffer (insert contents)
                                         (goto-char (point-min))
                                         (downsample)))))
                 (or (car args) (nthcdr 5 command-line-args)))))

(defvar files-to-combine
'(
"introduction.org"
"approach.org"
"Index_of_Ethics_Paper.org"
"background.org"
"background_concepts.org"
"background_hccc.org"
"background_other.org"
"survey_introduction.org"
"1.0.org"
"1_a.org"
"1_b.org"
"2.0.org"
"2_a.org"
"2_b.org"
"3.0.org"
"3_a.org"
"3_b.org"
"4_0.org"
"4_a.org"
"4_b.org"
"5.0.org"
"case_studies.org"
"logseq_and_friends.org"
"how_ai_can_be_a_force_for_good.org"
"mathematical_creativity.org"
"discussion.org"
"mind.org"
"premises.org"
"scope.org"
"main_references.org"
)
"An ordered list of files to combine in our export.
This is where the order of presentation in the downstream org file
and derived PDF is defined.")

(defun indent-org-roam-export ()
  "Utility function to increase indention for selected trees."
  (org-map-entries (lambda ()
                     ;; don’t demote the top level items and their sub-items
                     (let ((tag (org-entry-get nil "tag")))
                       (if (and tag (string= (car (split-string tag)) "HL"))
                           (progn (org-end-of-subtree)
                                  (setq org-map-continue-from (point)))
                         (org-do-demote))))
                   nil 'file))

(defun rebuild-org-roam-pdf ()
  "Build an org file and PDF compiling `files-to-combine'."
  (interactive)
  (save-excursion (find-file (concat org-roam-directory
                                     "../manual/combined.org"))
    (goto-char (point-min))
    (search-forward "# IMPORT")
    (let ((beg (point)))
      (delete-region (point) (point-max))
      (insert "\n" (combine-org-roam-files files-to-combine))
      (goto-char beg)
      (indent-org-roam-export)
      (org-latex-export-to-pdf))))

;;; org-roam-manual.el ends here
