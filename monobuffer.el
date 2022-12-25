(defgroup monobuffer nil
  "Monobuffer customization group."
  :group 'convenience)

(defcustom monobuffer--exclude-folder-regex
  "node_modules"
  "Regex representing the folders to excludes while searching for
all subprojects"
  :type 'string
  :group 'monobuffer)

(defcustom monobuffer--root-folder-regex ".git\\|\.project"
  "Regex used to find root directory"
  :type 'string
  :group 'monobuffer)

(defcustom monobuffer--subproject-regex "package.json"
  "Regex representing the pattern to find all subprojects"
  :type 'string
  :group 'monobuffer)

(defun monobuffer--file-regex-exist-p (regex dir)
  "Return t if REGEX matches any file in DIR, nil otherwise."
  (let ((files (directory-files dir)))
    (catch 'found
      (dolist (file files)
        (when (string-match-p regex file)
          (throw 'found t)))
      nil)))

(defun monobuffer--get-root-project ()
  (locate-dominating-file
   default-directory
   (lambda (f) (monobuffer--file-regex-exist-p monobuffer--root-folder-regex f))))

;; TODO remove root folder from the list
(defun monobuffer--get-all-subprojects ()
  (let ((root (monobuffer--get-root-project)))
	(mapcar #'file-name-directory
	 (directory-files-recursively root monobuffer--subproject-regex nil
								   (lambda (d) (not (string-match monobuffer--exclude-folder-regex d)))))))

(defun monobuffer--add-trailing-slash (dir)
  (if (string-suffix-p "/" dir)
	  dir
    (concat dir "/")))

(defun monobuffer ()
  (interactive)
  (when-let ((new-content (monobuffer--get-new-path (minibuffer-contents))))
	(delete-minibuffer-contents)
	(insert-and-inherit new-content)))

(defun monobuffer--get-new-path (current-path)
  (let ((root (monobuffer--get-root-project)) 
		(project (locate-dominating-file current-path "package.json"))
		(current (monobuffer--add-trailing-slash current-path))
		(subprojects (monobuffer--get-all-subprojects)))
	(if (and (string= current root) subprojects)
		(format "%s%s" root (completing-read "Project: " subprojects))
	  (if (string= current project)
		  (locate-dominating-file current-path ".git")
		project))))

(define-key vertico-map (kbd "M-u") 'monobuffer)

(provide 'monobuffer)





