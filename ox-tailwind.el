;; ox-tailwind.el --- Tailwind.css Back-End for Org Export Engine -*- lexical-binding: t -*-

;; Author: Vasco Ferreira <vasco_mmf@hotmail.com>
;; Maintainer: Vasco Ferreira <vasco_mmf@hotmail.com>
;; Created: 07 Mar 2020
;; Keywords: tailwind.css org-mode html-export
;; Homepage: TODO: github page
;; Package-Requires: ((dash "2.17.0") (ox-html))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This html export backend uses tailwind.css classes to export all the org-mode
;; elements. This way it is you will be able to quickly and effortlessly change
;; the layout, colors and look of the website.
;;
;; The html document has the following default layout:
;;
;; ---------------------------------------------------
;; |                      Header                     |
;; ---------------------------------------------------
;; |Table of Contents|                               |
;; |                 |                               |
;; |     Sidebar     |              Body             |
;; |                 |                               |
;; |                 |                               |
;; ---------------------------------------------------
;; |                      Footer                     |
;; ---------------------------------------------------
;;
;; HACK: You have to replace the class `.table' in Tailwind.css
;; as it collides with the `.table' class in Prims.js.
;; For example, replace `.table' with `.ttable', there is only one occurence.
;;
;; You can change the look of the exported HTML by redefining the values of the
;; classes. All the classes start with `org-tailwind-class-'. To change the
;; contents of the elements you can change the following variables:
;; - `org-tailwind-title'
;; - `org-tailwind-head-files'
;; - `org-tailwind-header'
;; - `org-tailwind-footer'
;; - `org-tailwind-sidebar'
;; - `org-tailwind-bottom-files'
;;
;; Prism.js needs the following plugins:
;; - line highlight
;; - line numbers
;; - autolinker
;; - file highlight
;; - show language
;; - jsonp highlight
;; - inline colors
;; - previewers
;; - autoloader
;; - command-line
;; - toolbar
;; - copy to clipboard button
;; - download button
;; - match braces
;; - diff highlight

;;; Code:

;;; Dependencies:

(require 'ox-html)
(require 'dash)

;;; Define backend

(org-export-define-backend 'tailwind
  '((bold . org-tailwind-bold)
    (center-block . org-html-center-block)
    (clock . org-html-clock)
    (code . org-tailwind-code)
    (drawer . org-html-drawer)
    (dynamic-block . org-html-dynamic-block)
    (entity . org-html-entity)
    (example-block . org-tailwind-example-block)
    (export-block . org-html-export-block)
    (export-snippet . org-html-export-snippet)
    (fixed-width . org-html-fixed-width)
    (footnote-definition . org-html-footnote-definition)
    (footnote-reference . org-html-footnote-reference)
    (headline . org-tailwind-headline)
    (horizontal-rule . org-html-horizontal-rule)
    (inline-src-block . org-html-inline-src-block)
    (inlinetask . org-html-inlinetask)
    (inner-template . org-tailwind-inner-template)
    (italic . org-tailwind-italic)
    (item . org-tailwind-item)
    (keyword . org-html-keyword)
    (latex-environment . org-html-latex-environment)
    (latex-fragment . org-html-latex-fragment)
    (line-break . org-html-line-break)
    (link . org-tailwind-link)
    (node-property . org-html-node-property)
    (paragraph . org-tailwind-paragraph)
    (plain-list . org-tailwind-plain-list)
    (plain-text . org-tailwind-plain-text)
    (planning . org-html-planning)
    (property-drawer . org-html-property-drawer)
    (quote-block . org-tailwind-blockquote)
    (radio-target . org-html-radio-target)
    (section . org-tailwind-section)
    (special-block . org-tailwind-special-block)
    (src-block . org-tailwind-src-block-select)
    (statistics-cookie . org-html-statistics-cookie)
    (strike-through . org-tailwind-strike-through)
    (subscript . org-html-subscript)
    (superscript . org-html-superscript)
    (table . org-tailwind-table)
    (table-cell . org-tailwind-table-cell)
    (table-row . org-tailwind-table-row)
    (target . org-html-target)
    (template . org-tailwind-template)
    (timestamp . org-html-timestamp)
    (underline . org-tailwind-underlined)
    (verbatim . org-tailwind-verbatim)
    (verse-block . org-html-verse-block))
  :menu-entry
  '(?x "Export to HTML with ox-tailwind Back-End"
       ((?H "As HTML buffer" org-tailwind-export-as-html)
        (?h "As HTML file" org-tailwind-export-to-html)
        (?o "As HTML file and open"
            (lambda (a s v b)
              (if a (org-tailwind-export-to-html t s v b)
                (org-open-file (org-tailwind-export-to-html nil s v b)))))))
  :options-alist
  ;; Define keywords like this:
  ;; - (:key-name "PROPERTY-NAME" CONDITION DEFAULT)
  ;; Get the keywords like this:
  ;; - (plist-get (org-export-get-environment 'tailwind) :key-name)
  '((:title "TITLE" nil "Notes & Guides")
    (:html-title "HTML-TITLE:" nil "Notes & Guides")))

;;; Element Classes

;; Headings

(defcustom org-tailwind-class-h1
  "mt-32 mb-6 text-6xl text-gray-700"
  "Tailwind.css classes for Heading 1")

(defcustom org-tailwind-class-h2
  "mt-20 mb-6 text-5xl text-gray-700"
  "Tailwind.css classes for Heading 2")

(defcustom org-tailwind-class-h3
  "mt-12 mb-6 text-4xl text-gray-700"
  "Tailwind.css classes for Heading 3")

(defcustom org-tailwind-class-h4
  "mt-8 mb-6 text-3xl text-gray-700"
  "Tailwind.css classes for Heading 4")

(defcustom org-tailwind-class-h5
  "mt-6 mb-6 text-2xl text-gray-700"
  "Tailwind.css classes for Heading 5")

(defcustom org-tailwind-class-h6
  "mt-4 mb-6 text-xl text-gray-700"
  "Tailwind.css classes for Heading 6")

(defcustom org-tailwind-class-h7
  "mt-2 mb-6 text-lg text-gray-700"
  "Tailwind.css classes for Heading 7 and beyond.")

;; Text elements

(defcustom org-tailwind-class-bold
  ""
  "Tailwind.css classes for the HTML BOLD attribute")

(defcustom org-tailwind-class-italic
  ""
  "Tailwind.css classes for the HTML ITALIC attribute.")

(defcustom org-tailwind-class-underlined
  "underline"
  "Tailwind.css classes for the HTML UNDERLINE attribute.")

(defcustom org-tailwind-class-code
  "m-1 px-2 border-solid border rounded-md border-green-500 text-green-500 bg-gray-300"
  "Tailwind.css classes for the HTML UNDERLINE attribute.")

(defcustom org-tailwind-class-verbatim
  "m-1 px-4 border-solid border rounded-md border-red-500 text-red-500 bg-gray-300"
  "Tailwind.css classes for the HTML VERBATIM attribute.")

(defcustom org-tailwind-class-link
  "text-green-500 hover:text-green-900"
  "Tailwind.css classes for the HTML LINK attribute.")

(defcustom org-tailwind-class-paragraph
  "my-2"
  "Tailwind.css classes for the HTML PARAGRAPH.")

(defcustom org-tailwind-class-image-div
  "my-12"
  "Tailwind.css classes for the HTML image DIV.")

(defcustom org-tailwind-class-image
  "mx-auto max-w-full max-h-full border-solid border-2 rounded-md border-green-500"
  "Tailwind.css classes for the HTML IMAGE.")

(defcustom org-tailwind-class-image-description
  "mx-20 text-center italic"
  "Tailwind.css classes for the HTML image DESCRIPTION.")

(defcustom org-tailwind-class-video
  "border-solid border-2 rounded-md border-green-500"
  "Tailwind.css classes for the HTML VIDEO.")

(defcustom org-tailwind-class-toc-items
  "text-sm hover:bg-green-300"
  "Tailwind.css classes for the HTML Table of Contents items.")

(defcustom org-tailwind-class-top-button
  "float-right -mt-8 underline text-green-500 font-bold"
  "Tailwind.css classes for the HTML go to TOP button.
There are already some prefixed classes:
- p-2
- block
- mt-2
- ml-0")

;; Page Divs

(defcustom org-tailwind-class-body
  "flex flex-col h-screen"
  "Tailwind.css classes for the HTML BODY.")

(defcustom org-tailwind-class-header
  "w-full border-b-2 border-gray-500 bg-gray-900 text-gray-400 shadow-2xl items-center h-16"
  "Tailwind.css classes for the HTML HEADER.")

(defcustom org-tailwind-class-sidebar
  "px-24 pt-20 lg:border-r-2 lg:border-gray-900 lg:fixed lg:w-64 lg:p-4 lg:overflow-y-auto lg:inset-y-0 lg:mt-16 lg:mb-6"
  "Tailwind.css classes for the HTML SIDEBAR.")

(defcustom org-tailwind-class-content
  "flex flex-col lg:flex-row overflow-y-auto"
  "Tailwind.css classes for the HTML CONTENT.")

(defcustom org-tailwind-class-content-container
  "flex-grow px-4 mb-8 sm:px-10 md:px-20 lg:ml-64 lg:px-64 lg:overflow-x-auto"
  "Tailwind.css classes for the HTML contents CONTAINER.")

(defcustom org-tailwind-class-footer
  "fixed bottom-0 w-full h-8 bg-gray-900 text-gray-400 text-center"
  "Tailwind.css classes for the HTML FOOTER.")

;; Lists

(defcustom org-tailwind-class-ordered-list
  "list-decimal my-8"
  "Tailwind.css classes for the HTML ORDERED list.")

(defcustom org-tailwind-class-ordered-list-item
  "ml-10"
  "Tailwind.css classes for the HTML ordered list ITEM.")

(defcustom org-tailwind-class-unordered-list
  "list-disc my-8"
  "Tailwind.css classes for the HTML UNORDERED list.")

(defcustom org-tailwind-class-unordered-list-item
  "ml-10"
  "Tailwind.css classes for the HTML unordered list ITEM.")

(defcustom org-tailwind-class-description-list
  "my-8"
  "Tailwind.css classes for the HTML DESCRIPTION list.")

(defcustom org-tailwind-class-description-list-title
  "font-bold"
  "Tailwind.css classes for the HTML DESCRIPTION title.")

(defcustom org-tailwind-class-description-list-item
  "ml-10"
  "Tailwind.css classes for the HTML description list ITEM.")

;; Table

(defcustom org-tailwind-class-table-name
  "mx-20 text-center italic"
  "Tailwind.css classes for the HTML table NAME.")

(defcustom org-tailwind-class-table
  "table-auto m-auto"
  "Tailwind.css classes for the HTML TABLE.")

(defcustom org-tailwind-class-table-header-row
  "text-gray-600 border"
  "Tailwind.css classes for the HTML table HEADER-ROW.")

(defcustom org-tailwind-class-table-header-cell
  "px-4 py-2 font-bold text-center"
  "Tailwind.css classes for the HTML table HEADER-CELL.")

(defcustom org-tailwind-class-table-body-row
  "hover:bg-green-200"
  "Tailwind.css classes for the HTML table BODY-ROW.")

(defcustom org-tailwind-class-table-body-cell
  "border px-4 py-2"
  "Tailwind.css classes for the HTML table BODY-CELL.")

;; Blocks

(defcustom org-tailwind-class-example
  "my-12 px-4 border-solid border-2 rounded-md border-green-500 text-green-500 bg-gray-300"
  "Tailwind.css classes for the HTML EXAMPLE-BLOCK.")

(defcustom org-tailwind-class-src-container
  "my-12 border-solid border-2 rounded-md border-green-500"
  "Tailwind.css classes for the HTML SRC-BLOCK.")

(defcustom org-tailwind-class-pre
  "rainbow-braces -m-2"
  "Tailwind.css classes for the HTML SRC-BLOCK.")

(defcustom org-tailwind-class-blockquote
  "my-12 px-4 border-solid border-l-8 border-2 rounded-md border-gray-500 bg-gray-300"
  "Tailwind.css classes for the HTML BLOCKQUOTE block.")

;; Special Blocks

(defcustom org-tailwind-class-mermaid-block
  "my-12 p-4 max-w-full max-h-full"
  "Tailwind.css classes for the HTML MERMAID block.")

(defcustom org-tailwind-class-mermaid-block-description
  "mx-20 text-center italic"
  "Tailwind.css classes for the HTML MERMAID block.")

(defcustom org-tailwind-class-details-block
  "my-12 p-8 border-solid border-l-8 border-2 rounded-md border-purple-500"
  "Tailwind.css classes for the HTML DETAILS block.")

(defcustom org-tailwind-class-details-title
  "text-purple-500 font-bold"
  "Tailwind.css classes for the HTML details block TITLE.")

(defcustom org-tailwind-class-tip-block
  "my-12 p-8 border-solid border-l-8 border-2 rounded-md border-teal-500"
  "Tailwind.css classes for the HTML TIP block.")

(defcustom org-tailwind-class-tip-title
  "text-teal-500 font-bold"
  "Tailwind.css classes for the HTML tip block TITLE.")

(defcustom org-tailwind-class-warning-block
  "my-12 p-8 border-solid border-l-8 border-2 rounded-md border-yellow-500"
  "Tailwind.css classes for the HTML WARNING block.")

(defcustom org-tailwind-class-warning-title
  "text-yellow-500 font-bold"
  "Tailwind.css classes for the HTML warning block TITLE.")

(defcustom org-tailwind-class-danger-block
  "my-12 p-8 border-solid border-l-8 border-2 rounded-md border-red-500"
  "Tailwind.css classes for the HTML DANGER block.")

(defcustom org-tailwind-class-danger-title
  "text-red-500 font-bold"
  "Tailwind.css classes for the HTML danger block TITLE.")

;;; Templates

(defcustom org-tailwind-head-files
  "<!-- Tailwind CSS -->
<link href=\"./css/tailwind.min.css\" rel=\"stylesheet\"/>

<!-- Prism Css -->
<link href=\"./css/prism_tomorrow.css\" rel=\"stylesheet\" />

<!-- Mathjax -->
<script>
MathJax = {
  tex: {
  	processEscapes: true
  }
}
</script>
<script type=\"text/javascript\" async
  src=\"./mathjax/tex-mml-chtml.js\">
</script>

<!-- Other Settings -->
<style>
.mermaid > svg {margin: auto;}
.line-highlight {
    background: linear-gradient(to right, hsla(0, 0%, 100%, 0.1) 70%, hsla(157.4, 100%, 51%, 0.04));
}
</style>

<!-- Your CSS file should come here -->
<link href=\"./css/style.css\" rel=\"stylesheet\" />
"
  "Links to be imported on the head of the HTML file.")

(defcustom org-tailwind-header
  "<nav class=\"flex items-center justify-between flex-wrap p-4\">
  <div class=\"flex items-center flex-no-shrink mr-6\">
    <span class=\"font-semibold text-xl tracking-tight\">
      <a href=\"./index.html\">Notes</a>
    </span>
  </div>
  <div class=\"block flex-grow lg:flex lg:items-center lg:w-auto\">
    <div class=\"text-sm lg:flex-grow\">
      <a href=\"#top\" class=\"font-bold block mt-4 mr-4 float-right lg:inline-block lg:mt-0\">
        Top
      </a>
    </div>
    <div>
    </div>
  </div>
</nav>"
  "Contents of header in HTML.")

(defcustom org-tailwind-sidebar
  "<div id=\"toc\"></div>"
  "Contents of sidebar in HTML.")

(defcustom org-tailwind-footer
  "<p>Made by Vasco Ferreira</p>"
  "Contents of footer in HTML.")

(defcustom org-tailwind-bottom-files
  "<script src=\"./js/prism.js\"></script>
<script src=\"./js/mermaid.min.js\"></script>
<script>mermaid.initialize({startOnLoad:true});</script>"
  "Javascript files to be imported at the bottom of the HTML file.")

(defcustom org-tailwind-javascript
   "function getElementsByTagNames(list,obj) {
	if (!obj) var obj = document;
	var tagNames = list.split(',');
	var resultArray = new Array();
	for (var i=0;i < tagNames.length;i++) {
		var tags = obj.getElementsByTagName(tagNames[i]);
		for (var j=0;j < tags.length;j++) {
			resultArray.push(tags[j]);
		}
	}
	var testNode = resultArray[0];
	if (!testNode) return [];
	if (testNode.sourceIndex) {
		resultArray.sort(function (a,b) {
				return a.sourceIndex - b.sourceIndex;
		});
	}
	else if (testNode.compareDocumentPosition) {
		resultArray.sort(function (a,b) {
				return 3 - (a.compareDocumentPosition(b) & 6);
		});
	}
	return resultArray;
}

function createTOC() {
	var z = document.getElementById('toc');;
	var toBeTOCced = getElementsByTagNames('h1,h2,h3,h4,h5');
	if (toBeTOCced.length < 2) return false;

	// Add go to top button
	var top = document.createElement('a');
	top.innerHTML = 'Top';
	top.href = '#top';
	top.className += 'top %s';
	// z.appendChild(top);

	for (var i=0;i < toBeTOCced.length;i++) {
		var tmp = document.createElement('a');
		tmp.innerHTML = toBeTOCced[i].innerHTML;
		tmp.className = 'p-2';
		z.appendChild(tmp);
		if (toBeTOCced[i].nodeName == 'H1')
			tmp.className += ' block mt-2 ml-0';
		if (toBeTOCced[i].nodeName == 'H2')
			tmp.className += ' block ml-4';
		if (toBeTOCced[i].nodeName == 'H3')
			tmp.className += ' block ml-8';
		if (toBeTOCced[i].nodeName == 'H4')
			tmp.className += ' block ml-12';
		if (toBeTOCced[i].nodeName == 'H5')
			tmp.className += ' block ml-16';
		if (toBeTOCced[i].nodeName == 'H6')
			tmp.className += ' block ml-20';
    tmp.className += ' %s';

		var headerId = toBeTOCced[i].id || 'link' + i;
		tmp.href = '#' + headerId;
		toBeTOCced[i].id = headerId;
	}
}

createTOC();"
  "Javascript code needed in the HTML file.")

(defcustom org-tailwind-html-template
      "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <title>%s</title>
  %s
</head>
<body class=\"%s\">

<div id=\"header\" class=\"%s\">
%s
</div>

<div id=\"content\" class=\"top %s\">
  <div id=\"sidebar\" class=\"%s\">
  %s
  </div>

  <div id=\"content-container\" class=\"%s\">
    <div id=\"top\"></div>
    %s
  </div>
</div>

<div id=\"footer\" class=\"%s\">
%s
</div>

</script>
<script>
%s
</script>

%s

</body>
</html>
"
      "Default HTML template.
The default template needs the format place for the following
values, in this order:

- page title
- head files
- body classes
- header classes
- header contents
- content classes
- sidebar classes
- sidebar contents
- content-container classes
- contents
- footer classes
- footer contents
- bottom page javascript
- bottom page files.")

;;; Transcode Elements

(defun org-tailwind-inner-template (contents info)
  "Define the template for the CONTENTS inside Headings.
By not doing anything to the contents, it exports the elements at the root level."
  contents)

(defun org-tailwind-template (contents info)
  "Format the HTML Template and add the CONTENTS of the export."
  (format org-tailwind-html-template
          (plist-get (org-export-get-environment 'tailwind) :html-title)
          org-tailwind-head-files
          org-tailwind-class-body
          org-tailwind-class-header
          org-tailwind-header
          org-tailwind-class-content
          org-tailwind-class-sidebar
          org-tailwind-sidebar
          org-tailwind-class-content-container
          contents
          org-tailwind-class-footer
          org-tailwind-footer
          (format org-tailwind-javascript
                  org-tailwind-class-top-button
                  org-tailwind-class-toc-items)
          org-tailwind-bottom-files))

(defun org-tailwind-bold (bold contents info)
  "Transcode BOLD from Org to HTML."
  (format "<strong class=\"%s\">%s</strong>" org-tailwind-class-bold contents))

(defun org-tailwind-italic (italic contents info)
  "Transcode ITALIC from Org to HTML."
  (format "<em class=\"%s\">%s</em>" org-tailwind-class-italic contents))

(defun org-tailwind-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to HTML."
  (format "<del class=\"%s\">%s</del>" org-tailwind-class-italic contents))

(defun org-tailwind-underlined (underlined contents info)
  "Transcode UNDERLINED from Org to HTML."
  (format "<span class=\"%s\">%s</span>" org-tailwind-class-underlined contents))

(defun org-tailwind-code (code contents info)
  "Transcode CODE from Org to HTML."
  (let ((code-text (org-element-property :value code)))
    (format "<code class=\"%s\">%s</code>" org-tailwind-class-code code-text)))

(defun org-tailwind-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to HTML."
  (let ((code-text (org-element-property :value verbatim)))
    (format "<code class=\"%s\">%s</code>" org-tailwind-class-verbatim code-text)))

(defun org-tailwind-plain-text (plain-text info)
  "Transcode PLAIN-TEXT from Org to HTML."
  plain-text)

(defun org-tailwind-checkbox (checkbox)
  "Format a checkbox item into the corresponding HTML tag."
  (let ((checkbox-tag "<input type=\"checkbox\" disabled %s>"))
    (pcase checkbox
      (`on (format checkbox-tag "checked"))
      (`off (format checkbox-tag "unchecked"))
      (_ (format checkbox-tag "unchecked")))))

(defun org-tailwind-paragraph (paragraph contents info)
  "Transcode PARAGRAPH from Org to HTML."
  (let* ((parent (org-element-property :parent paragraph))
         (parent-type (org-element-property :type parent))
         (ancestor (org-element-property :parent parent))
         (ancestor-type (org-element-property :type ancestor))
         (checkbox (org-element-property :checkbox parent))
         (is-checkbox-p (unless (equalp checkbox nil) t))
         (is-image-p (string-match-p "<img " contents)))
    (cond
     ;; For Mermaid.js return raw text
     ((equalp parent-type "mermaid") contents)
     ;; For Descriptive types, surround with `dd' tag
     ((equalp ancestor-type 'descriptive)
      (format "<dd class=\"%s\">%s</dd>"
              org-tailwind-class-description-list-item
              contents))
     ;; Put image inside a div with description
     (is-image-p (format "<div class=\"%s\">%s</div>"
                         org-tailwind-class-image-div
                         contents))
     ;; If is a checkbox
     (is-checkbox-p
      (format "<p class=\"%s\">%s %s</p>"
              org-tailwind-class-paragraph
              (org-tailwind-checkbox checkbox)
              (and (org-string-nw-p contents) (org-trim contents))))
     (t (format "<p class=\"%s\">%s</p>"
                org-tailwind-class-paragraph
                contents)))))

(defun org-tailwind-link (link contents info)
  "Transcode LINK from Org to HTML."
  (let* ((path (org-element-property :path link))
         (type (org-element-property :type link))
         (parent (org-element-property :parent link))
         (description (org-element-property :name parent))
         (file-extension (file-name-extension path))
         (link-tag "<a class=\"%s\" href=\"%s\">%s</a>")
         (video-tag "<video class=\"%s\" controls> <source src=\"%s\" type=\"video/%s\"/> </video>")
         (image-description "<p class=\"%s\">%s</p>")
         (image-tag "<img class=\"%s\" src=\"%s\"/>%s"))
    (cond
     ;; Is the link a video
     ((or (equalp file-extension "mp4")
          (equalp file-extension "avi")
          (equalp file-extension "mkv")
          (equalp file-extension "mpeg4")
          (equalp file-extension "3gp"))
      (format video-tag
              org-tailwind-class-video
              path
              file-extension))
     ;; Is the link an image
     ((or (equalp file-extension "png")
          (equalp file-extension "svg")
          (equalp file-extension "tiff")
          (equalp file-extension "tif")
          (equalp file-extension "jpg")
          (equalp file-extension "jpeg")
          (equalp file-extension "gif")
          (equalp file-extension "bmp"))
      (format image-tag
              org-tailwind-class-image
              path
              (if description
                  (format image-description
                          org-tailwind-class-image-description
                          description)
                "")))
     ;; Is the link an org file
     ((equalp file-extension "org")
      (format link-tag
              org-tailwind-class-link
              (concat type ":" (replace-regexp-in-string "\.org" "\.html" path))
              contents))
     ;; Any other link
     (t (format link-tag
                org-tailwind-class-link
                (concat type ":" path)
                contents)))))

(defun org-tailwind-blockquote (blockquote contents info)
  "Transcode BLOCKQUOTE from Org to HTML."
  (format "<blockquote class=\"%s\">%s</blockquote>" org-tailwind-class-blockquote contents))

(defvar org-tailwind--src-block-open
  "<div class=\"%s\"><pre class=\"%s\"  %s>"
  "Opening tag of code block for Prism.js
It has two format places:
- Tailwind.css classes
- other attribute.")

(defvar org-tailwind--src-block-close
  "<code class=\"language-%s\">%s</code></pre></div>"
  "Closing tag of code block for Prism.js
It has two format places:
- language
- code text")

(defun org-tailwind--get-attribute (attribute block)
  "Get the ATTRIBUTE from an org BLOCK element."
  (car (org-element-property attribute block)))

(defun org-tailwind-command-line-block (src-block contents info)
  "Transcode SRC-BLOCK with command line language to a custom Prism.js code block."
  (let* ((code-text (org-element-property :value src-block))
         (language (org-element-property :language src-block))
         (filepath (org-tailwind--get-attribute :attr_filepath src-block))
         (username (org-tailwind--get-attribute :attr_username src-block))
         (hostname (org-tailwind--get-attribute :attr_hostname src-block))
         (highlight-lines (org-tailwind--get-attribute :attr_highlight src-block)))
    (concat
     (format org-tailwind--src-block-open
             org-tailwind-class-src-container
             (concat "command-line " )
             (concat
              "data-filter-output=\"(out)\" "
              ;; Add user and host names
              (format " data-user=\"%s\" data-host=\"%s\""
                      (if username username "user")
                      (if hostname hostname "localhost"))
              (if highlight-lines
                  (format " data-line=\"%s\"" highlight-lines)
                "")
              ;; Add file src
              (if filepath
                  (format " data-src=\"%s\" data-download-link" filepath)
                "")
              ))
     (format org-tailwind--src-block-close
             (if (equalp language "ps") "powershell" language)
             code-text))))

(defun org-tailwind-src-block (src-block contents info)
  "Transcode SRC-BLOCK from Org to HTML."
  (let* ((code-text (org-element-property :value src-block))
         (language (org-element-property :language src-block))
         (filepath (org-tailwind--get-attribute :attr_filepath src-block))
         (highlight-lines (org-tailwind--get-attribute :attr_highlight src-block))
         (fetch-code (org-tailwind--get-attribute :attr_fetch src-block)))
    (concat
     (format org-tailwind--src-block-open
             org-tailwind-class-src-container
             (concat "line-numbers " org-tailwind-class-pre)
             (concat " "
                     (if highlight-lines
                         (format " data-line=\"%s\"" highlight-lines)
                       "")
                     (if fetch-code
                         (format " data-jsonp=\"%s\" language-%s"
                                 fetch-code language)
                       "")
                     ;; Add file src
                     (if filepath
                         (format " data-src=\"%s\" data-download-link" filepath)
                       "")))
     (format org-tailwind--src-block-close
             language
             code-text))))

(defun org-tailwind-src-block-select (src-block contents info)
  "Transcode SRC-BLOCK from Org to HTML."
  (let* ((language (org-element-property :language src-block)))
    (cond
     ;; If it is a command line language,
     ((or (equalp language "sh")
          (equalp language "shell")
          (equalp language "bash")
          (equalp language "ps")
          (equalp language "powershell"))
      (org-tailwind-command-line-block src-block contents info))
     ;; If it is a programming language
     (t (org-tailwind-src-block src-block contents info)))))

(defun org-tailwind-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK from Org to HTML."
  (let ((code (org-html-format-code example-block info))
        (code-text (org-element-property :value example-block))
        (language (org-element-property :language example-block))
        (highlight-lines (org-element-property :highlight example-block)))
    (format "<pre class=\"%s\"><code class=\"language-%s\">%s</code></pre>"
            org-tailwind-class-example language code-text)))

(defun org-tailwind-special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK from Org to HTML.
There are 4 types of blocks:
- Details
- Tip
- Warning
- Danger"
  ;; HACK: In order not to add the p tags in paragraph if the special-block's
  ;; type is `mermaid', change the paragraph block directly
  (let* ((type (org-element-property :type special-block))
         (name (org-element-property :name special-block)))
    (cond ((equalp type "mermaid")
           (format "<div class=\"%s\"><div class=\"mermaid\">%s</div><p class=\"%s\">%s</p></div>"
                   org-tailwind-class-mermaid-block
                   contents
                   org-tailwind-class-mermaid-block-description
                   name))
          ;; TODO: process title as summary
          ((equalp type "details")
           (format "<details class=\"%s\"><summary class=\"%s\">%s</summary>%s</details>"
                   org-tailwind-class-details-block
                   org-tailwind-class-paragraph
                   (if name name "Details")
                   contents))
          ((equalp type "tip")
           (format "<div class=\"tip %s\"><p class=\"%s\">%s</p>%s</div>"
                   org-tailwind-class-tip-block
                   org-tailwind-class-tip-title
                   (if name name "")
                   contents))
          ((equalp type "warning")
           (format "<div class=\"warning %s\"><p class=\"%s\">%s</p>%s</div>"
                   org-tailwind-class-warning-block
                   org-tailwind-class-warning-title
                   (if name name "")
                   contents))
          ((equalp type "danger")
           (format "<div class=\"danger %s\"><p class=\"%s\">%s</p>%s</div>"
                   org-tailwind-class-danger-block
                   org-tailwind-class-danger-title
                   (if name name "")
                   contents))
          (t (org-html-special-block special-block contents info)))))

(defun org-tailwind-format-header (level text contents)
  "Return the corresponding HTML heading level of the Org LEVEL."
  (let ((header-contents (if contents contents ""))
        (header "<h%s class=\"%s\">%s</h%s>%s" ))
    (cond
     ((eq level 1) (format header level org-tailwind-class-h1
                           text level header-contents))
     ((eq level 2) (format header level org-tailwind-class-h2
                           text level header-contents))
     ((eq level 3) (format header level org-tailwind-class-h3
                           text level header-contents))
     ((eq level 4) (format header level org-tailwind-class-h4
                           text level header-contents))
     ((eq level 5) (format header level org-tailwind-class-h5
                           text level header-contents))
     ((eq level 6) (format header level org-tailwind-class-h6
                           text level header-contents))
     (t (format header level org-tailwind-class-h7 text level
                header-contents)))))

(defun org-tailwind-headline (headline contents info)
  "Transcode HEADLINE from Org to HTML."
  (let* ((text (org-element-property :raw-value headline))
         (level (org-export-get-relative-level headline info)))
    (org-tailwind-format-header level text contents)))

(defun org-tailwind-table-cell (table-cell contents info)
  "Transcode TABLE-CELL from Org to HTML.
If TABLE-CELL is part of the table header, return the HTML table
cell with `th'. Return `td' otherwise."
  (if (org-export-table-row-in-header-p (org-export-get-parent table-cell) info)
      (format "<th class=\"%s\">%s</th>" org-tailwind-class-table-header-cell contents)
    (format "<td class=\"%s\">%s</td>" org-tailwind-class-table-body-cell contents)))

(defun org-tailwind-table-row (table-row contents info)
  "Transcode TABLE-ROW from Org to HTML.
First, check the group of the TABLE-ROW, in this case it will be
either `thead' or `tbody'. Second, check if it is the first row
of the group. If it is, add the corresponding opening HTML tag.
In the case of being the last row of the group, add the closing
tag. If it is neither, return the row without group tags."
  (let* ((header '("<thead>" . "</thead>"))
         (body '("<tbody>" . "</tbody>"))
         (row '("<tr class=\"%s\">" . "</tr>"))
         (row-group (org-export-table-row-group table-row info))
         (is-first-row-p (org-export-table-row-starts-rowgroup-p table-row info))
         (is-last-row-p (org-export-table-row-ends-rowgroup-p table-row info)))
    ;; As there are multiple types of rows, e.g., separator rows,
    ;; check if the row is a normal row
    (when (eq (org-element-property :type table-row) 'standard)
      ;; If row is in the table header
      (if (eq row-group 1)
          (cond
           ;; Is the first row of the header
           ((not (eq is-first-row-p nil))
            (concat (car header)
                    (format (car row) org-tailwind-class-table-header-row)
                    contents
                    (cdr row)))
           ;; Is the last row of the header
           ((not (eq is-last-row-p nil))
            (concat (format (car row) org-tailwind-class-table-header-row)
                    contents
                    (cdr row)
                    (cdr header)))
           ;; Is in the middle of header
           (t (concat (format (car row) org-tailwind-class-table-header-row)
                      contents
                      (cdr row))))
        ;; If row is in the table body
        (cond
         ;; Is the first row of the body
         ((not (eq is-first-row-p nil))
          (concat (car body)
                  (format (car row) org-tailwind-class-table-body-row)
                  contents
                  (cdr row)))
         ;; Is the last row of the body
         ((not (eq is-last-row-p nil))
          (concat (format (car row) org-tailwind-class-table-body-row)
                  contents
                  (cdr row)
                  (cdr body)))
         ;; Is in the middle of the body
         (t (concat (format (car row) org-tailwind-class-table-body-row)
                    contents
                    (cdr row))))))))

(defun org-tailwind-table (table contents info)
  "Transcode TABLE from Org to HTML."
  (let ((name (org-element-property :name table)))
    (format "<table class=\"%s\">%s</table>%s"
            org-tailwind-class-table
            contents
            (if name
                (format "<p class=\"%s\">%s</p>"
                        org-tailwind-class-table-name
                        name)
              ""))))

(defun org-tailwind-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST from Org to HTML.
There are three types of lists:
- Ordered Lists;
- Unordered Lists and
- Description Lists."
  (let* ((type (pcase (org-element-property :type plain-list)
                 (`ordered "ol")
                 (`unordered "ul")
                 (`descriptive "dl")
                 (other (error "Unknown HTML list type: %s" other)))))
    (format "<%s class=\"%s\">%s</%s>"
            type
            (cond ((equalp type "ol") org-tailwind-class-ordered-list)
                  ((equalp type "ul") org-tailwind-class-unordered-list)
                  ((equalp type "dl") org-tailwind-class-description-list))
            contents
            type)))

(defun org-tailwind-format-list-item (contents type checkbox info
					   &optional term-counter-id headline)
  "Format a list item into the corresponding HTML tag."
  (cond
   ((equalp type `ordered)
    (format "<li class=\"%s\">%s</li>"
            org-tailwind-class-ordered-list-item contents))
   ((equalp type `unordered)
    (format "<li class=\"%s\">%s</li>"
            org-tailwind-class-unordered-list-item contents))
   ((equalp type `descriptive)
    (format "<dt class=\"%s\">%s</dt>%s"
            org-tailwind-class-description-list-title
            term-counter-id
            contents))))

(defun org-tailwind-item (item contents info)
  "Transcode an ITEM element from Org to HTML."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (counter (org-element-property :counter item))
         (checkbox (org-element-property :checkbox item))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info)))))
    (org-tailwind-format-list-item
     contents type checkbox info (or tag counter))))

(defun org-tailwind-section (section contents info)
  "Return the contents of SECTION as is."
  contents)

;;;###autoload
(defun org-tailwind-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a TAILWIND HTML buffer.
Export as `org-html-export-as-html' does, with slimhtml
org-export-backend.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Export is done in a buffer named \"*Org TAILWIND export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'tailwind "*Org TAILWIND Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-tailwind-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML file.
Export as `org-html-export-as-html' does, with slimhtml
org-export-backend.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Return output file's name."
  (interactive)
  (let* ((extension (concat
                     (when (> (length org-html-extension) 0) ".")
                     (or (plist-get ext-plist :html-extension)
                         org-html-extension
                         "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'tailwind file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-tailwind-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'tailwind filename
                      (concat (when (> (length org-html-extension) 0) ".")
                              (or (plist-get plist :html-extension)
                                  org-html-extension
                                  "html"))
                      plist pub-dir))

(provide 'ox-tailwind)

;;; ox-tailwind.el ends here
