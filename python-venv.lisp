;;; python-venv.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2024  Anthony Green <green@moxielogic.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;

(in-package :python-venv)

(defclass python-venv ()
  ((directory :initarg :directory)
   (python :initarg :python :initform "python")
   (pip :initarg :pip :initform "pip")))

(defun make-python-venv (directory &key (python "python") (pip "pip"))
  "Make a python virtual environment in DIRECTORY, optionally
 providing the PYTHON and PIP program names."
  (let* ((directory-path (if (typep directory 'pathname) directory
                             (parse-namestring directory)))
         (breadcrumb-filename (namestring (uiop:merge-pathnames* ".CL-PYTHON-VENV" (uiop:ensure-directory-pathname directory-path))))
         (venv (make-instance 'python-venv :directory directory-path :python python :pip pip)))
      ;; Check if the breadcrumb file does not exist
      (unless (probe-file breadcrumb-filename)
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program `(,python "-m" "venv" ,(namestring directory-path))
                              :ignore-error-status t :error-output :string)
          (declare (ignore output))
          (unless (zerop exit-code)
            (error "Failed to create venv: ~A" error-output))
          (with-open-file (stream breadcrumb-filename
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (write-line "Virtual environment created by cl-python-venv." stream))))
      venv))

(defun delete-python-venv (venv)
  "Delete a python virtual environment directory."
  (let* ((directory (slot-value venv 'directory))
         (directory-path (if (typep directory 'pathname) directory
                             (parse-namestring directory)))
         (breadcrumb-filename (namestring (uiop:merge-pathnames* ".CL-PYTHON-VENV" (uiop:ensure-directory-pathname directory-path)))))
    (if (probe-file breadcrumb-filename)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname directory-path) :validate t)
        (error "Directory does not contain a MAKE-PYTHON-VENV generated virtual environment: ~A" directory))))

(defun install-packages-in-venv (venv packages &key (output uiop:*stdout*) (error-output uiop:*stderr*))
  "Install the list PACKAGES in VENV. OUTPUT and ERROR-OUTPUT managed as per UIOP:RUN-PROGRAM."
  (assert (typep venv 'python-venv) (venv)
          "The object ~A is not an instance of VENV." venv)
  (assert (listp packages) (packages)
          "The object ~A is not an list of packages." packages)
  (let ((dir (slot-value venv 'directory)))
    (loop for package in packages
          collect (let ((args (list "bash" "-c" (format nil "source ~A && ~A install ~A"
                                                        (uiop:merge-pathnames* "bin/activate" (uiop:ensure-directory-pathname dir))
                                                        (slot-value venv 'pip)
                                                        package))))
                    (uiop:run-program args :ignore-error-status t :output output :error-output error-output)))))

(defun get-packages-in-venv (venv)
  "Get the list PACKAGES in VENV. OUTPUT and ERROR-OUTPUT managed as per UIOP:RUN-PROGRAM."
  (assert (typep venv 'python-venv) (venv)
          "The object ~A is not an instance of VENV." venv)
  (let* ((dir (slot-value venv 'directory))
         (output (uiop:run-program (list "bash" "-c" (format nil "source ~A && ~A list --format json"
                                                             (uiop:merge-pathnames* "bin/activate" (uiop:ensure-directory-pathname dir))
                                                             (slot-value venv 'pip)))
                                   :output :string)))
    (loop for p in (json:decode-json-from-string output)
          collect (cons (cdr (assoc :name p)) (cdr (assoc :version p))))))

(defun run-python-program-in-venv (venv python-source-file args &key (ignore-error-status t) (output uiop:*stdout*) (error-output uiop:*stderr*))
  "Run the PYTHON-SOURCE-FILE and ARGS in a python VENV. OUTPUT and
ERROR-OUTPUT managed as per UIOP:RUN-PROGRAM."
  (assert (typep venv 'python-venv) (venv)
          "The object ~A is not an instance of VENV." venv)
  (let ((dir (slot-value venv 'directory)))
    (let ((run-program-args (list "bash" "-c" (format nil "source ~A && ~A ~A ~{~A~^ ~}"
                                                      (uiop:merge-pathnames* "bin/activate" (uiop:ensure-directory-pathname dir))
                                                      (slot-value venv 'python)
                                                      python-source-file
                                                      args))))
      (uiop:run-program run-program-args :ignore-error-status ignore-error-status :output output :error-output error-output))))

(defun run-python-source-in-venv (venv python-source args &key (ignore-error-status t) (output uiop:*stdout*) (error-output uiop:*stderr*))
  "Run the PYTHON-SOURCE string along with ARGS in a python VENV. OUTPUT
and ERROR-OUTPUT managed as per UIOP:RUN-PROGRAM."
  (assert (typep venv 'python-venv) (venv)
          "The object ~A is not an instance of VENV." venv)
  (uiop:with-temporary-file (:stream stream :pathname python-source-file :keep nil)
    (write python-source :stream stream)
    :close-stream
    (let ((dir (slot-value venv 'directory)))
      (let ((run-program-args (list "bash" "-c" (format nil "source ~A && ~A ~A ~{~A~^ ~}"
                                                        (uiop:merge-pathnames* "bin/activate" (uiop:ensure-directory-pathname dir))
                                                        (slot-value venv 'python)
                                                        python-source-file
                                                        args))))
        (uiop:run-program run-program-args :ignore-error-status ignore-error-status :output output :error-output error-output)))))
