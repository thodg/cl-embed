# cl-embed v0.1

ERB-style template engine in portable Common Lisp. No dependencies.
BSD/ISC license.

## Installation

```Common-Lisp
(repo:install :cl-template)
```

## Usage

Templates are text files that can contain embedded Common Lisp code
(s-exp).

The parsing starts as raw text input, until it matches a `<%=` or a
`<%`. In these two cases the parser emits the parsed raw text and
proceeds to reads a code chunk until a matching `%>` is found.
Raw text is converted to a Common Lisp string. Verbose code blocks
are left untouched and should evaluate to a string. Silent code
blocks are extended to return the empty string. The end result is a list
of strings and code, ready to evaluate and concatenate. No parenthesis
magic is done. The Common Lisp code is in plain text and the raw text
blocks are to be treated as strings.

Example template with Common Lisp code embedding :

```Common-Lisp
<!doctype html>
<html>
  <head>
    <title><%= title %></title>
  </head>
  <body>
    <h1><%= title %></h1>
    <ul>
      <%= (mapcar (lambda (item) %>
      <li>
        <%= item %>
      </li>
      <%= ) items-list) %>
    </ul>
  </body>
</html>
```

## Documentation

There are two operations on templates : **parse** and **render**.
Each operation can be from or to a **file**, a **stream**, or a
**string**. The whole API is 6 functions.

### Function (parse-template-from-file *input-pathname*)

Template parser that opens the file *input-pathname* and calls
`parse-template-from-stream` on the opened stream.

### Function (parse-template-from-stream *input-stream*)

Template parser that consumes input from *input-stream* and produces a list
of strings and s-expressions ready to be evaluated and concatenated.

### Function (parse-template-from-string *input-string*)

Template parser that opens the *input-string* as an input stream and
calls `parse-template-from-stream` on the opened stream.

### Function (render-template-to-file *template* *output-pathname*)

Renders *template* with output to a new file at path *output-pathname*.
Calls `render-template-to-stream` under the hood.

### Function (render-template-to-stream *template* *output-stream*)

Renders *template* with output to the *output-stream* stream.

### Function (render-template-to-string *template*)

Renders *template* into a string.
