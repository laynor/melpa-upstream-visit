;;; melpa-upstream-visit.el --- A set of kludges to visit a melpa-hosted package's homepage

;; Copyright (C) 2013  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((s 1.6.0))

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

;; This package provides an interactive command to `browse-url' a
;; melpa-hosted package's homepage.
;;
;; To (try to) visit a package's homepage, just
;;   M-x muv RET package-name RET
;; and the package's homepage (actually, its repo page) will be hopefully
;; opened in your browser.
;;
;; The list of kludges used to guess the package's homepage is stored
;; in `muv:url-kludges', a variable that you can customize with your
;; own functions.
;;
;; These functions are applied to a melpa recipe and return a string
;; containing the URL of the package - note that here applied refers
;; to `apply'.  See `muv::github-kludge' for an example.
;;
;; The kludges in the list are applied in order until one of them
;; returns non-nil. The first non-nil result is then interpreted
;; as the URL to be visited.

;;; Code:
(eval-when-compile
  (require 'cl))

(require 's)

(defgroup melpa-upstream-visit nil
  "A set of kludges to visit a melpa-installed package's homepage."
  :prefix "muv:"
  :group 'package)

(defun muv::recipe-url (package)
  "Returns the melpa recipe URL (github) for PACKAGE."
   (format "http://raw.github.com/milkypostman/melpa/master/recipes/%s" package))

(defun muv::fetch-recipe (package)
  "Returns the melpa recipe (as a list) for PACKAGE."
  (with-current-buffer (url-retrieve-synchronously
                        (muv::recipe-url package))
    (search-forward "(")
    (backward-char)
    (sexp-at-point)))


;;; Recipe -> URL kludges

(defun* muv::github-kludge (package-name &key fetcher repo &allow-other-keys)
  (and (eq fetcher 'github) (format "https://github.com/%s" repo)))

(defun* muv::wiki-kludge (package-name &key fetcher &allow-other-keys)
  (and (eq fetcher 'github) (format "http://www.emacswiki.org/%s.el" package-name)))

(defun* muv::savannah-git-kludge (package-name &key fetcher url &allow-other-keys)
  (let ((matches (s-match "savannah\\.nongnu\\.org/\\([^/\\.]+\\)\\.git" url)))
    (and matches (format "http://savannah.nongnu.org/projects/%s/" (second matches)))))

(defun* muv::google-code-hg-kludge (package-name &key fetcher url &allow-other-keys)
  (let ((matches (s-match "^https?://code\\.google\\.com/p/[^/]+/" url)))
    (first matches)))

(defun* muv::google-code-kludge (package-name &key url &allow-other-keys)
  (let ((matches (s-match "^https?://[^/]+\\.googlecode\\.com/" url)))
    (first matches)))

(defun* muv::gitorious-kludge (package-name &key url &allow-other-keys)
  (let ((matches (s-match "gitorious\\.org/[^/]+/[^\\.]+" url)))
    (and matches (format "https://%s" (first matches)))))

(defun* muv::bitbucket-kludge (package-name &key url &allow-other-keys)
  (let ((matches (s-match "bitbucket\\.org/[^/]+/[^/\\?]+" url)))
    (and matches (format "https://%s" (first matches)))))

(defun* muv::launchpad-kludge (package-name &key url &allow-other-keys)
  (and (s-starts-with-p "lp:" url)
       (s-replace "lp:" "https://launchpad.net/" url)))

(defun* muv::sourceforge-svn-kludge (package-name &key url &allow-other-keys)
  (let ((matches (s-match "svn\\.sourceforge\\.\\([^/]+\\)/svnroot/\\([^/]+\\)" url)))
    (and matches (format "http://%s.sourceforge.%s/" (third matches) (second matches)))))

;; TODO: sourceforge git kludge

(defun* muv::svn-common-kludge (package-name &key fetcher url &allow-other-keys)
  (and (eq fetcher 'svn) (replace-regexp-in-string "svn/.*$" "" url)))

(defun* muv::plain-url-kludge (package-name &key url &allow-other-keys)
  (read-from-minibuffer "Verify url: " url))


(defcustom muv:url-kludges '(muv::github-kludge
                             muv::wiki-kludge
                             muv::savannah-git-kludge
                             muv::google-code-hg-kludge
                             muv::google-code-kludge
                             muv::gitorious-kludge
                             muv::bitbucket-kludge
                             muv::launchpad-kludge
                             muv::svn-common-kludge
                             muv::plain-url-kludge)
  "Recipe to homepage url translation functions, applied in order."
  :group 'melpa-upstream-visit
  :type '(repeat function))


(defun muv::first-non-nil-result (function-list &rest args)
  "Applies the functions in FUNCTION-LIST to ARGS in order,
returning the first non nil result."
  (or (apply (car function-list) args)
      (apply 'muv::first-non-nil-result (cdr function-list) args)))

(defun muv::url-from-recipe(recipe)
  "Tries to guess the homepage URL of the package described by
RECIPE."
  (apply 'muv::first-non-nil-result muv:url-kludges recipe))

;;;###autoload;
(defun muv (package-name)
  "`browse-url's (or at least tries to) the PACKAGE-NAME's homepage."
  (interactive (list (ido-completing-read "Visit package upstream: "
                                          (mapcar (lambda (el)
                                                    (symbol-name (car el)))
                                                  package-archive-contents))))
  (let ((url (muv::url-from-recipe
              (muv::fetch-recipe package-name))))
    (if url
        (browse-url url)
      (error "No package named '%s' can be found in MELPA." package-name))))

(provide 'melpa-upstream-visit)

;;; melpa-upstream-visit.el ends here
