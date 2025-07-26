;;  cl-embed  -  ERB-style template engine for Common Lisp
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

(in-package :common-lisp-user)

(defpackage :cl-embed.system
  (:use :common-lisp :asdf))

(in-package :cl-embed.system)

(defsystem :cl-embed
  :name "cl-embed"
  :author "kmx.io <contact@kmx.io>"
  :version "0.1"
  :description "ERB-style template engine"
  :components
  ((:file "package")
   (:file "embed" :depends-on ("package"))))
