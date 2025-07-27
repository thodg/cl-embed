;;  cl-embed - ERB-style template engine for Common Lisp
;;  Copyright 2025 kmx.io <contact@kmx.io>
;;
;;  Permission to use, copy, modify, and distribute this software for
;;  any purpose with or without fee is hereby granted, provided that the
;;  above copyright notice and this permission notice appear in all
;;  copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;;  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;;  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;;  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;;  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :embed)

(defun str (&rest parts)
  (apply #'concatenate 'string parts))

(defun parse-template-from-stream (input-stream)
  (let ((template '("(str "))
        (state 'raw)
        (token-stream (make-string-output-stream))
        (token-position (the unsigned-byte 0)))
    (loop
     (let ((c (read-char input-stream nil)))
       (unless c
         (if (eq 'raw state)
             (push (with-output-to-string (string-stream)
                     (prin1 (get-output-stream-string token-stream)
                            string-stream))
                   template)
             (push (list state
                         (get-output-stream-string token-stream))
                   template))
         (return
           (read-from-string
            (apply #'str
                   (reverse (cons ")" template))))))
       (case state
         ((raw)
          (case token-position
            ((0)
             (if (eq c #\<)
                 (setf token-position 1)
                 (write-char c token-stream)))
            ((1)
             (if (eq c #\%)
                 (setf token-position 2)
                 (progn
                   (setf token-position 0)
                   (write-char #\< token-stream))))
            ((2)
             (push (with-output-to-string (string-stream)
                     (prin1 (get-output-stream-string token-stream)
                            string-stream))
                   template)
             (if (eq c #\=)
                 (setf state 'verbose
                       token-position 0
                       token-stream (make-string-output-stream))
                 (progn
                   (push (get-output-stream-string token-stream)
                         template)
                   (setf state 'silent
                         token-position 0
                         token-stream (make-string-output-stream))
                   (write-char c token-stream))))))
         ((silent verbose)
          (case token-position
            ((0)
             (if (eq c #\%)
                 (setf token-position 1)
                 (write-char c token-stream)))
            ((1)
             (if (eq c #\>)
                 (let ((token (get-output-stream-string token-stream)))
                   (push (case state
                           ((silent)
                            (str "(progn " token " \"\")"))
                           ((verbose)
                            token))
                         template)
                   (setf state 'raw
                         token-position 0
                         token-stream (make-string-output-stream)))
                 (progn
                   (write-char c token-stream)
                   (setf token-position 0)))))))))))

(defun parse-template-from-file (input-pathname)
  (with-open-file (input
                   input-pathname
                   :direction :input
                   :element-type 'character
                   :external-format :utf-8)
    (parse-template-from-stream input)))

(defun parse-template-from-string (input-string)
  (with-input-from-string (input
                           input-string)
    (parse-template-from-stream input)))

#+nil
(progn
  (parse-template-from-string "Hello world !")
  (parse-template-from-string "Start<%= (* 1024 1024) %>End"))

(defun render-template-to-stream (bindings template output-stream)
  (assert (eq 'str (car template)))
  (dolist (item (rest template))
    (if (stringp item)
        (write-string item output-stream)
        (write-string (with-output-to-string (string-stream)
                        (prin1 (eval `(let ,bindings
                                        ,item))
                               string-stream))
                      output-stream))))

(defun render-template-to-file (bindings template output-pathname)
  (with-open-file (output
                   output-pathname
                   :direction :output
                   :element-type 'character
                   :external-format :utf-8)
    (render-template-to-stream bindings template output)))

(defun render-template-to-string (bindings template)
  (with-output-to-string (output)
    (render-template-to-stream bindings template output)))

(defun template (bindings input-string)
  (render-template-to-string
   bindings
   (parse-template-from-string input-string)))

#+nil
(template '((a 21)) "Start<%= (* 2 a) %>End")
