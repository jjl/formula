[![Clojars Project](https://img.shields.io/clojars/v/irresponsible/formula.svg)](https://clojars.org/irresponsible/formula)

The Irresponsible Clojure Guild presents...

# Formula

webforms, meet spec

## Status: alpha

This library is still under active development and there will be bugs and possibly minor breakage.

A release to clojars will come when we've had time to put it through its paces in some real-world software.

Clojurescript is not yet supported.

## Usage

If you're using clojure 1.8, you will need [clojure-future-spec](https://github.com/tonsky/clojure-future-spec)

For this example, we assume you are using ring with wrap-params, but ring is not
required, we just take a map. We do not use the wrap-keyword-params middleware
here so our param keys are strings, but we also support keyword keys.

```clojure
(ns my-app.routes
 (:require [clojure.spec.alpha :as s]
           [irresponsible.formula :as f]
           [irresponsible.formula.conformers :as c]))

;;;;;;;; field declarations

;;; the ::username field reads the parameter "username" and checks it is 8 chars+
(s/def ::username
 (f/field "username"
   :conform (c/min-len 8)
   :error "Must be at least 8 characters long"))

;;; the ::password field reads the parameter "password" and checks it is 8 chars+
(s/def ::password
 (f/field "password"
   :conform (c/min-len 8)
   :error "Must be at least 8 characters long"))

;;;;;;;;; form declaration

(s/def ::login-form
  (f/form :req [::username ::password]
          :error "There was a problem with your input data"))

;;;;;;;;; usage is just like spec!

(defn read-login-form
  "This is a quick helper to demonstrate conforming a form.
   Its main purpose is to return nil when the form data is invalid
   returns: map of field names to values"
  [params]
  (let [r (s/conform ::login-form params)]
    (when-not (f/invalid? r) ;; invalid? just checks if it's ::s/invalid
      r)))

(defn login-post
  "This is a standard ring handler function called in response to a POST request"
  [{:keys [params]}]
  (if-let [{:strs [username password]} (read-login-form params)]
    (let [errors (f/errors (s/explain-data ::login-form params))]
      (render-template "template.html" {:params params :errors errors}))
    (if-let [u (lookup-user {:username username password})]
      (do-login u)
      (render-template "template.html" {:params params :errors {:form "Your username and/or password were wrong. Please try again"}}))))
```

And here's a html-based template for the form. It uses thymeleaf, but you
should be able to readily translate it to whatever you're using:

```html
<!doctype html>
<html lang="en">
<head>
</head>
<body>
  <form action="/login" method="post">
    <div class="form-error" th:if="${errors.form}" th:text="${errors.form}">error message will be written here if there were errors</div>
    <div class="field">
      <label for="username">Username:</label>
      <input type="text" th:value="${params.username}" id="username">
      <span class="error" th:if="${errors.username}" th:text="${errors.username}"/>error message will be written here if the value is invalid</span>
    </div>
    <div class="field">
      <label for="password">Password:</label>
      <input type="password" id="password">
      <span class="error" th:if="${errors.password}" th:text="${errors.password}"/>error message will be written here if the value is invalid</span>
    </div>
    <div class="field">
      <button type="submit" id="submit">Log in!</button>
    </div>
  </form>
</body>
</html>
```

Security note: Please remember to add some sort of xsrf token scheme, which we have omitted for clarity.

## A note about Conformers

Though you are used to defining specs by predicates, internally, the conformer
function is the basic unit of operation. Where a predicate would return false or
nil in the case of failure, a conformer returns `:clojure.spec.alpha/invalid`

Example:

```clojure
(s/def ::foo string?)
;; internally, this expands to something like this:
(defn string-conformer [s]
  (if (string? s)
    s
    ::s/invalid))
```

Because form values are received as strings and typically validated in the attempt
to parse them, this library uses conformers instead of predicates, like spec
does internally and like it may yet end up doing in the public API.

## Copyright and License

MIT LICENSE

Copyright (c) 2017 James Laver

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

