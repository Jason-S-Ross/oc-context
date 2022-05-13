;;; oc-context.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jason Ross
;;
;; Author: Jason Ross <https://github.com/jason>
;; Maintainer: Jason Ross <jasonross1024@gmail.com>
;; Created: September 24, 2021
;; Modified: September 24, 2021
;; Version: 0.0.1
;; Keywords: bib tools
;; Homepage: https://github.com/jason/oc-context
;; Package-Requires: ((emacs "25.6") (org "9.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description:
;;
;;  Provides citation export for ConTeXt.
;;; Code:

(require 'oc)
(require 'ox-context)

(defun oc-context-export-bibliography
    (_keys _files style props &rest _)
  "Print references from bibliography.
PROPS is the local properties of the bibliography, as a plist."
  ;; TODO Handle PROPS.
  (let ((numbering
         (pcase style
           ((or "numeric" "nb") "num")
           (_ "no")))
        (sorttype
         (pcase style
           ((or "numeric" "nb") "index")
           (_ "default"))))
    (let (args elem)
      (dolist (datum props)
        (message "datum: %S %S" datum (keywordp datum))

        (cond
         ((keywordp datum)
          (when elem (push elem args) (message "Pushing"))
          (setq elem (substring (symbol-name datum) 1))
          (message "elem: %S" elem))

         (t
          ;; Comma-separated values are associated to the same keyword
          (when elem
            (push (cons elem datum) args))
          (setq elem nil)))
        (message "args: %S" args))
      (push (cons "numbering" numbering) args)
      (push (cons "sorttype" sorttype) args)
      (format
       "\\placelistofpublications[%s]"
       (org-context--format-arguments args)))))

(defun oc-context-export-citation
    (citation style _ info)
  "Export CITATION object.
STYLE is the citation style, as a string or nil. INFO is the export state,
as a plist."
  (let* ((alternative
          (pcase style
            ;; TODO handle capitalized stuff
            ;; TODO noauthor
            ;; TODO nocite
            ;; TODO note
            ;;
            (`(,(or "author" "a") . ,_) "author")
            (`(,(or "text" "t") . ,_) "authornum")
            (`(,(or "numeric" "nb") . ,_) "num")
            (`(,(or "nocite" "n") . ,_) nil)
            (_ nil)))
         (caps
          (pcase style
            (`(,_ . ,(or "caps" "c")) t)
            (_ nil)))
         (references (org-cite-get-references citation))
         (origin (pcase references
                   (`(,reference) reference)
                   (`(,reference . ,_) (org-element-property :parent reference))))
         (prefix (org-export-data (org-element-property :prefix origin) info))
         (suffix (org-export-data (org-element-property :suffix origin) info))
         (keys
          (mapconcat
           (lambda (r) (org-element-property :key r))
           references
           ","))
         (options
          (org-context--format-arguments
             (list (cons "alternative" alternative)
                   (cons "lefttext"
                         (when (org-string-nw-p prefix)
                           (format "{%s}" prefix)))
                   (cons "righttext"
                         (when (org-string-nw-p suffix)
                           (format "{%s}" suffix))))))
         (cmd (format "\\cite%s[%s]"
            (if (org-string-nw-p options)
              (format "[%s]" options)
              "")
            keys)))
    (if caps (format "\\WORDS{%s}" cmd) cmd)))

(defun oc-context-prepare-preamble (output _keys files style &rest _)
  "Prepare document preamble for bibliography usage.

OUTPUT is the final output of the export process. FILES is the list
of file names used as the bibliography.

This function adds resources to the document, and set styles."
  (with-temp-buffer
    (save-excursion (insert output))
    (when (search-forward "\\starttext\n" nil t)
      (goto-char (match-beginning 0))
      (insert
       (mapconcat
        (lambda (f)
          (format "\\usebtxdataset[%s]" (expand-file-name f)))
        files
        "\n")
       "\n"))
    (buffer-string)))

(org-cite-register-processor 'context
  :export-bibliography #'oc-context-export-bibliography
  :export-citation #'oc-context-export-citation
  :export-finalizer #'oc-context-prepare-preamble
  :cite-styles
  '((("author" "a") ("caps" "c"))
    (("nocite" "n"))
    (("numeric" "nb"))
    (("text" "t") ("caps" "c"))))


(provide 'oc-context)
;;; oc-context.el ends here
