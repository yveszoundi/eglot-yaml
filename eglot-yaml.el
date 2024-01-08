;;; eglot-yaml.el --- YAML extension for the eglot LSP client  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Yves Zoundi

;; Version: 1.3
;; Author: Yves Zoundi <yz at spam.me>
;; Maintainer: Yves Zoundi <yz at spam.me>
;; URL: https://github.com/yveszoundi/eglot-yaml
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.1") (eglot "1.0") (yaml-mode "0.0.15"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; YAML auto-completion helper for eglot based on few schemas.

;;
;;; Code:

(require 'eglot)

(defgroup eglot-yaml nil
  "Interaction with a YAML language server via eglot."
  :prefix "eglot-yaml-"
  :group 'eglot)

(defcustom eglot-yaml-schema-store-uri "https://www.schemastore.org/api/json/catalog.json"
  "Cache folder for YAML JSON schemas"
  :type 'string
  :group 'eglot-yaml)

(defcustom eglot-yaml-schema-cache-directory "~/.emacs.d/etc/cache/eglot-yaml"
  "Cache folder for YAML JSON schemas."
  :type 'string
  :group 'eglot-yaml)

(defvar eglot-yaml-schema-by-name #s(hash-table size 30 test equal) "YAML schema by name.")

(declare-function 'json-read-from-string "json" (text))

(defun eglot-yaml--file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun eglot-yaml--schema-store-cache-list (schema-index-file)
  "Cache schemas URL for all available schema for a given catalog file."
  (when (hash-table-empty-p eglot-yaml-schema-by-name)
    (let* ((json-object-type 'hash-table)
           (json-array-type  'list)
           (json-key-type    'string)
           (eglot-yaml-schema-catalog-text (eglot-yaml--file-to-string schema-index-file))
           (yaml-schemas (gethash "schemas" (json-read-from-string eglot-yaml-schema-catalog-text))))
      (dolist (yaml-schema yaml-schemas)
        (let ((schema-name (gethash "name" yaml-schema))
              (schema-url  (gethash "url" yaml-schema)))
          (puthash schema-name schema-url eglot-yaml-schema-by-name))))))

(defun eglot-yaml--buffer-whole-string (buffer)
  "Retrieve the text contents from an HTTP response BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (re-search-forward "^$")
      (buffer-substring-no-properties (point) (point-max)))))

;;;###autoload
(defun eglot-yaml-schema-for-buffer ()
  "Select a JSON schema for the current YAML buffer."
  (interactive)
  (if (or (derived-mode-p 'yaml-mode)
          (derived-mode-p 'yaml-ts-mode))
      (progn
        (let ((eglot-yaml-schema-index (expand-file-name "index" eglot-yaml-schema-cache-directory)))
          (unless (file-exists-p eglot-yaml-schema-cache-directory)
            (mkdir eglot-yaml-schema-cache-directory t))

          (unless (file-exists-p eglot-yaml-schema-index)
            (url-copy-file eglot-yaml-schema-store-uri eglot-yaml-schema-index t))

          (unless (hash-table-empty-p eglot-yaml-schema-by-name)
            (eglot-yaml--schema-store-cache-list eglot-yaml-schema-index))

          (let* ((selected-schema-name (completing-read "Select schema: "
                                                        (hash-table-keys eglot-yaml-schema-by-name)
                                                        nil
                                                        t))
                 (selected-schema-uri  (gethash selected-schema-name eglot-yaml-schema-by-name))
                 (schema-uri-local-path (expand-file-name
                                         selected-schema-name
                                         eglot-yaml-schema-cache-directory)))

            (unless (file-exists-p schema-uri-local-path)
              (url-copy-file selected-schema-uri schema-uri-local-path t))

            (let ((eglot-yaml-schemas (list
                                      (list :yaml
                                            :schemas  (list (intern (format ":%s" (eglot--path-to-uri schema-uri-local-path)))
                                                            (buffer-file-name))))))
            (setq eglot-workspace-configuration eglot-yaml-schemas)
            (call-interactively 'eglot-signal-didChangeConfiguration)))))
    (user-error "Cannot only set YAML schema in yaml-mode or yaml-ts-mode.")))

(provide 'eglot-yaml)
;;; eglot-yaml.el ends here
