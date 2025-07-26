# cl-embed v0.1

ERB-style template engine for Common Lisp.

## Installation

```Common-Lisp
(repo:install :cl-template)
```

## Usage

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
