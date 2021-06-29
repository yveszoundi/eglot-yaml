;;; eglot-yaml.el --- YAML extension for the eglot LSP client  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Yves Zoundi

;; Version: 1.2
;; Package-Version: 20210619.2149
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

;; YAML extension for eglot

;;
;;; Code:

(require 'eglot)

(defgroup eglot-yaml nil
  "Interaction with a YAML language server via eglot."
  :prefix "eglot-yaml-"
  :group 'eglot)

(defcustom eglot-yaml-language-server-executable "yaml-language-server"
  "YAML language executable."
  :type 'file
  :group 'eglot-yaml)

(defcustom eglot-yaml-language-server-arguments
  '("--stdio")
  "YAML language server startup arguments."
  :type '(repeat string)
  :group 'eglot-yaml)

(defcustom eglot-yaml-schema-store-uri "https://www.schemastore.org/api/json/catalog.json"
  "Cache folder for YAML JSON schemas"
  :type 'string
  :group 'eglot-yaml)

(defcustom eglot-yaml-schema-cache-directory "~/.emacs.d/etc/cache/eglot-yaml"
  "Cache folder for YAML JSON schemas."
  :type 'string
  :group 'eglot-yaml)

(defvar eglot-yaml-schemas  '((:yaml :schemas ())) "LSP YAML schemas settings.")

(defvar eglot-yaml-schema-by-name #s(hash-table size 30 test equal) "YAML schema by name.")

(declare-function 'json-read-from-string "json" (text))

(defun eglot-yaml--file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun eglot-yaml--schema-store-cache-list (schema-index-file)
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

(defun eglot-yaml-schema-for-buffer ()
  "Select a JSON schema for the current YAML buffer."
  (interactive)
  (if (derived-mode-p 'yaml-mode)
      (progn
        (let ((eglot-yaml-schema-index (expand-file-name "index" eglot-yaml-schema-cache-directory)))
          (unless (file-exists-p eglot-yaml-schema-index)
            (url-copy-file eglot-yaml-schema-store-uri eglot-yaml-schema-index t))

          (eglot-yaml--schema-store-cache-list eglot-yaml-schema-index)

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

            (setq eglot-yaml-schemas (list
                                      (list :yaml
                                            :schemas (nconc
                                                      (list (intern (format ":%s" (eglot--path-to-uri schema-uri-local-path)))
                                                            (buffer-file-name))
                                                      (caddr (assoc :yaml eglot-yaml-schemas))))))

            (setq-default eglot-workspace-configuration eglot-yaml-schemas)
            (call-interactively 'eglot-signal-didChangeConfiguration))))
    (user-error "Cannot only set YAML schema in yaml-mode.")))

;;;###autoload
(defun eglot-yaml-init ()
  "Initialize the library for use with the Eclipse JDT language server."
  (let ((yaml-lsp-server-path (executable-find eglot-yaml-language-server-executable)))
    (if yaml-lsp-server-path
        (progn
          (unless (file-exists-p (expand-file-name eglot-yaml-schema-cache-directory))
            (make-directory (expand-file-name eglot-yaml-schema-cache-directory) t))

          (let ((yaml-language-server-command (cons eglot-yaml-language-server-executable
                                                    eglot-yaml-language-server-arguments)))
            (add-to-list 'eglot-server-programs `(yaml-mode . ,yaml-language-server-command))
            (setq-default eglot-workspace-configuration eglot-yaml-schemas)
            (add-hook 'yaml-mode-hook 'eglot-ensure)))
      (user-error "Cannot find YAML LSP server! Please customize the variable eglot-yaml-language-server-executable."))))

(provide 'eglot-yaml)
;;; eglot-yaml.el ends here
