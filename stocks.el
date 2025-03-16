;;; stocks.el --- Fetch stock prices anywhere -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Krishna Gopalakrishnan
;;
;; Author: Krishna Gopalakrishnan <krishnakg@gmail.com>
;; Created: February 27, 2025
;; Modified: February 27, 2025
;; Version: 0.0.1
;; Keywords: finance news tools
;; Homepage: https://github.com/krishnakg/stocks.el
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(provide 'stocks)

(require 'gptel)
(require 'json)
(require 'url)

(defcustom stocks-api-base-url "https://finnhub.io/api/v1"
  "The base URL for the stock API."
  :group 'stocks
  :type 'string)

(defun stocks-ticker-at-point ()
  "Fetches the stock information for the company at point, and outputs the stock"
  (interactive)
  (stocks-ticker (thing-at-point 'word t)))

(defun stocks-ticker (symbol)
  "Fetches the stock information for the provided symbol, and outputs the stock
ticker for the company in the mini-buffer."
  (interactive "sCompany name/symbol: ")
  (gptel-request symbol
    :system "You are a helpful assistant. If this is a name of a company return the stock ticker of the company, no other text.\n
             If this is not a company name, or if there is no ticker, just return an empty string."
    :callback #'stocks-symbol-response))

(gptel-make-tool
 :name "get-stock-quote"
 :function #'stocks-get-quote
 :description "Get the stock quote as well as change in price for a company"
 :args (list '(:name "symbol"
               :type "string"
               :description "The company stock ticker symbol for which to get the stock quote"))
 :category "tools")

(defun stocks-symbol-response (response info)
  "This is the function called by gptel with the stock symbol for our query.

RESPONSE is the response from the LLM.
INFO is additional information from the LLM.
"
  (let ((data (stocks-get-quote response)))
    (message (stocks-minibuffer-message response data) )))

(defun stocks-get-quote (symbol)
  "Fetch data from the finnhub.io url and parse the json response.

The JSON response is a hash-table with the following keys:
c - current price
d - change in price
dp - percent change in price
h - high price of the day
l - low price of the day
o - open price of the day
pc - previous close price
"
  (let* ((api-key (stocks-secret-from-auth-source "finnhub.io"))
         (url (concat stocks-api-base-url "/quote?symbol=" symbol "&token=" api-key))
         (data (stocks-fetch-json-from-api url)))
    data))

(defun stocks-minibuffer-message (symbol json-data)
  "Create the output string for the minibuffer.
In the JSON-DATA, c is current price
                  d is change in price
                  dp is percent change in price
The change value is nil for non-existing symbols.
"
  (if-let ((change (gethash 'd json-data)))
      (let* ((percent-change (gethash 'dp json-data))
             (percent-change-str (stocks-format-change percent-change t)))
        (concat  (propertize symbol 'face '(:weight bold))
                 (format " %s" (stocks-format-price (gethash 'c json-data)))
                 (format " %s" (stocks-format-change change nil))
                 (format " %s" percent-change-str)))
    (user-error "Not a valid symbol")))

(defun stocks-fetch-json-from-api (url)
  "Fetch JSON from URL and return it as a parsed Lisp object."
  (let* ((url-buffer (url-retrieve-synchronously url t))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (json (with-current-buffer url-buffer
                 (goto-char url-http-end-of-headers)
                 (json-read))))
    (kill-buffer url-buffer)
    json))

(defun stocks-format-change (change percent)
  "Formats how the stock change numbers are displayed.
if PERCENT is true, then a % sign and enclosing parenthesis are added
to the display string.
if CHANGE is positive, the display string is green, else it;s red.
"
  (let ((color (if (> change 0)
                   "green"
                 "red"))
        (format-str (if percent
                        "%.2f%%%%"
                      "%.2f")))
    (propertize (format format-str change) 'face `(:foreground ,color))))

(defun stocks-format-price (price)
  "Format's the current PRICE part of the output"
  (propertize (format "%.2f" price) 'face '(:weight bold)))

(defun stocks-secret-from-auth-source (host)
  "Fetch the secret from the auth-source for the HOST."
  (funcall (plist-get (car (auth-source-search :host host :require '(:secret))) :secret)))

;; Test strings below to verify functionality
;; Google
;; Hello
;; Apple
;; Reliance
;; Tesla
;; Palantir

;;; stocks.el ends here
