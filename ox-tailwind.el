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
;; classes. All the classes start with `org-tailwind-class-', check this file
;; to know which tailwind.css classes are being used and change them any way
;; you want.
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

(require 's)
(require 'cl)
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
  '(?x "Export to HTML with org-tailwind Back-End"
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


;; tailwind group

(defgroup org-tailwind nil
  "Classes for the html elements."
  :group 'classes)


;;; Element Classes

;; Headings

(defcustom org-tailwind-class-h1
  "mt-32 mb-6 text-6xl text-gray-700 border-b hover:text-green-500
border-gray-500"
  "Tailwind.css classes for Heading 1"
  :type '(string))

(defcustom org-tailwind-class-h2
  "mt-20 mb-6 text-5xl text-gray-700 border-b hover:text-green-500
border-gray-500"
  "Tailwind.css classes for Heading 2"
  :type '(string))

(defcustom org-tailwind-class-h3
  "mt-12 mb-6 text-4xl text-gray-700 border-b hover:text-green-500
border-gray-500"
  "Tailwind.css classes for Heading 3"
  :type '(string))

(defcustom org-tailwind-class-h4
  "mt-8 mb-6 text-3xl text-gray-700 border-b hover:text-green-500
border-gray-500"
  "Tailwind.css classes for Heading 4"
  :type '(string))

(defcustom org-tailwind-class-h5
  "mt-6 mb-6 text-2xl text-gray-700 border-b hover:text-green-500
border-gray-500"
  "Tailwind.css classes for Heading 5"
  :type '(string))

(defcustom org-tailwind-class-h6
  "mt-4 mb-6 text-xl text-gray-700 border-b hover:text-green-500
border-gray-500"
  "Tailwind.css classes for Heading 6"
  :type '(string))

(defcustom org-tailwind-class-h7
  "mt-2 mb-6 text-lg text-gray-700 border-b hover:text-green-500
border-gray-500"
  "Tailwind.css classes for Heading 7 and beyond."
  :type '(string))


;; Text elements

(defcustom org-tailwind-class-bold
  ""
  "Tailwind.css classes for the HTML BOLD attribute"
  :type '(string))

(defcustom org-tailwind-class-italic
  ""
  "Tailwind.css classes for the HTML ITALIC attribute."
  :type '(string))

(defcustom org-tailwind-class-underlined
  "underline"
  "Tailwind.css classes for the HTML UNDERLINE attribute."
  :type '(string))

(defcustom org-tailwind-class-code
  "px-2 rounded-md text-green-600 bg-gray-400"
  "Tailwind.css classes for the HTML UNDERLINE attribute."
  :type '(string))

(defcustom org-tailwind-class-verbatim
  "px-2 rounded-md text-red-600 bg-gray-400"
  "Tailwind.css classes for the HTML VERBATIM attribute."
  :type '(string))

(defcustom org-tailwind-class-link
  "text-green-500 hover:text-green-900"
  "Tailwind.css classes for the HTML LINK attribute."
  :type '(string))

(defcustom org-tailwind-class-paragraph
  "my-2"
  "Tailwind.css classes for the HTML PARAGRAPH."
  :type '(string))

(defcustom org-tailwind-class-image-div
  "my-12 max-w-full max-h-full"
  "Tailwind.css classes for the HTML image DIV."
  :type '(string))

(defcustom org-tailwind-class-image
  "mx-auto mb-2 max-w-full max-h-full rounded-md border-2
border-gray-500"
  "Tailwind.css classes for the HTML IMAGE."
  :type '(string))

(defcustom org-tailwind-class-image-description
  "mx-48 text-center italic border-t border-gray-500"
  "Tailwind.css classes for the HTML image DESCRIPTION."
  :type '(string))

(defcustom org-tailwind-class-video-div
  "my-12 max-w-full max-h-full"
  "Tailwind.css classes for the HTML video DIV."
  :type '(string))

(defcustom org-tailwind-class-video
  "mx-auto mb-2 rounded-md border-2 border-gray-500"
  "Tailwind.css classes for the HTML VIDEO."
  :type '(string))

(defcustom org-tailwind-class-video-description
  "mx-48 text-center italic border-t border-gray-500"
  "Tailwind.css classes for the HTML video DESCRIPTION."
  :type '(string))

(defcustom org-tailwind-class-toc-items
  "text-sm border-b border-l-4 border-gray-200 hover:bg-green-300"
  "Tailwind.css classes for the HTML Table of Contents items.
Do not break the line with while inserting a `newline'. Use `\' at
the end."
  :type '(string))

(defcustom org-tailwind-class-current-toc
  "border-green-500"
  "Tailwind.css classes for the CURRENT HTML Table of Contents item."
  :type '(string))


;; Lists

(defcustom org-tailwind-class-ordered-list
  "list-decimal my-8"
  "Tailwind.css classes for the HTML ORDERED list."
  :type '(string))

(defcustom org-tailwind-class-ordered-list-item
  "ml-10"
  "Tailwind.css classes for the HTML ordered list ITEM."
  :type '(string))

(defcustom org-tailwind-class-unordered-list
  "list-disc my-8"
  "Tailwind.css classes for the HTML UNORDERED list."
  :type '(string))

(defcustom org-tailwind-class-unordered-list-item
  "ml-10"
  "Tailwind.css classes for the HTML unordered list ITEM."
  :type '(string))

(defcustom org-tailwind-class-description-list
  "my-8"
  "Tailwind.css classes for the HTML DESCRIPTION list."
  :type '(string))

(defcustom org-tailwind-class-description-list-title
  "font-bold"
  "Tailwind.css classes for the HTML DESCRIPTION title."
  :type '(string))

(defcustom org-tailwind-class-description-list-item
  "ml-10"
  "Tailwind.css classes for the HTML description list ITEM."
  :type '(string))


;; Table

(defcustom org-tailwind-class-table-container
  "overflow-x-auto my-12"
  "Tailwind.css classes for the HTML table NAME."
  :type '(string))

(defcustom org-tailwind-class-table
  "table-auto m-auto my-2"
  "Tailwind.css classes for the HTML TABLE."
  :type '(string))

(defcustom org-tailwind-class-table-description
  "mx-48 text-center italic border-t border-gray-500"
  "Tailwind.css classes for the HTML table NAME."
  :type '(string))

(defcustom org-tailwind-class-table-header-row
  "text-gray-600 border-b-2 border-gray-400"
  "Tailwind.css classes for the HTML table HEADER-ROW."
  :type '(string))

(defcustom org-tailwind-class-table-header-cell
  "px-4 py-2 font-bold text-center"
  "Tailwind.css classes for the HTML table HEADER-CELL."
  :type '(string))

(defcustom org-tailwind-class-table-body-row
  "hover:bg-green-200"
  "Tailwind.css classes for the HTML table BODY-ROW."
  :type '(string))

(defcustom org-tailwind-class-table-last-body-row
  "border-b-2 border-gray-400 hover:bg-green-200"
  "Tailwind.css classes for the HTML table BODY-ROW."
  :type '(string))

(defcustom org-tailwind-class-table-body-cell
  "px-4 py-2"
  "Tailwind.css classes for the HTML table BODY-CELL."
  :type '(string))

(defcustom org-tailwind-class-table-empty-body-cell
  "px-2 py-2"
  "Tailwind.css classes for the HTML table EMPTY BODY-CELL."
  :type '(string))


;; Blocks

(defcustom org-tailwind-class-example-container
  "my-12 rounded-md border border-gray-800 shadow-xl"
  "Tailwind.css classes for the HTML EXAMPLE-BLOCK CONTAINER."
  :type '(string))

(defcustom org-tailwind-class-example
  "rounded rainbow-braces"
  "Tailwind.css classes for the HTML EXAMPLE-BLOCK."
  :type '(string))

(defcustom org-tailwind-class-src-container
  "my-12 shadow-xl"
  "Tailwind.css classes for the HTML SRC-BLOCK CONTAINER."
  :type '(string))

(defcustom org-tailwind-class-pre
  "rainbow-braces match-braces rounded"
  "Tailwind.css classes for the HTML SRC-BLOCK."
  :type '(string))

(defcustom org-tailwind-class-blockquote-container
  "my-12"
  "Tailwind.css classes for the HTML BLOCKQUOTE container."
  :type '(string))

(defcustom org-tailwind-class-blockquote
  "my-2 mx-20 px-4 border-l-8 rounded-md border border-gray-500
bg-gray-300"
  "Tailwind.css classes for the HTML BLOCKQUOTE block."
  :type '(string))

(defcustom org-tailwind-class-blockquote-author
  "mx-48 text-right italic border-t border-gray-500"
  "Tailwind.css classes for the HTML blockquote AUTHOR."
  :type '(string))


;; Special Blocks

(defcustom org-tailwind-class-mermaid-container
  "my-12"
  "Tailwind.css classes for the HTML MERMAID container."
  :type '(string))

(defcustom org-tailwind-class-mermaid-block
  "my-2 p-4 max-w-full max-h-full bg-white rounded-md"
  "Tailwind.css classes for the HTML MERMAID block."
  :type '(string))

(defcustom org-tailwind-class-mermaid-block-title
  "mx-20 text-center italic border-t border-gray-500"
  "Tailwind.css classes for the HTML MERMAID block."
  :type '(string))

(defcustom org-tailwind-class-details-block
  "my-12 p-8 rounded-md border-l-8 border border-purple-500
shadow-xl"
  "Tailwind.css classes for the HTML DETAILS block."
  :type '(string))

(defcustom org-tailwind-class-details-title
  "text-purple-500 font-bold"
  "Tailwind.css classes for the HTML details block TITLE."
  :type '(string))

(defcustom org-tailwind-class-tip-block
  "my-12 p-8 rounded-md border-l-8 border border-teal-500
shadow-xl"
  "Tailwind.css classes for the HTML TIP block."
  :type '(string))

(defcustom org-tailwind-class-tip-title
  "text-teal-500 font-bold"
  "Tailwind.css classes for the HTML tip block TITLE."
  :type '(string))

(defcustom org-tailwind-class-warning-block
  "my-12 p-8 rounded-md border-l-8 border border-yellow-500
shadow-xl"
  "Tailwind.css classes for the HTML WARNING block."
  :type '(string))

(defcustom org-tailwind-class-warning-title
  "text-yellow-500 font-bold"
  "Tailwind.css classes for the HTML warning block TITLE."
  :type '(string))

(defcustom org-tailwind-class-danger-block
  "my-12 p-8 rounded-md border-l-8 border border-red-500
shadow-xl"
  "Tailwind.css classes for the HTML DANGER block."
  :type '(string))

(defcustom org-tailwind-class-danger-title
  "text-red-500 font-bold"
  "Tailwind.css classes for the HTML danger block TITLE."
  :type '(string))


;; Page Divs

(defcustom org-tailwind-class-body
  "flex flex-col h-screen text-gray-700"
  "Tailwind.css classes for the HTML BODY."
  :type '(string))

(defcustom org-tailwind-class-header
  "w-full border-b border-gray-500
shadow-md items-center h-16"
  "Tailwind.css classes for the HTML HEADER."
  :type '(string))

(defcustom org-tailwind-class-sidebar
  "px-24 py-12 bg-gray-200 lg:border-r lg:border-gray-500
lg:fixed lg:pt-2 lg:w-64 lg:px-2 lg:overflow-y-auto lg:inset-y-0
lg:mt-16 lg:mb-8"
  "Tailwind.css classes for the HTML SIDEBAR."
  :type '(string))

(defcustom org-tailwind-class-content
  "flex flex-col lg:flex-row overflow-y-auto"
  "Tailwind.css classes for the HTML CONTENT."
  :type '(string))

(defcustom org-tailwind-class-content-container
  "flex-grow px-4 py-12 mb-8 sm:px-8 md:px-12 lg:ml-64 lg:px-12
lg:overflow-x-auto xl:px-32"
  "Tailwind.css classes for the HTML contents CONTAINER."
  :type '(string))

(defcustom org-tailwind-class-inner-container
  "px-20 py-12 pb-32 mb-12 shadow-2xl rounded-md"
  "Tailwind.css classes for the HTML inner container."
  :type '(string))

(defcustom org-tailwind-class-footer
  "fixed bottom-0 w-full border-t border-solid border-gray-500
h-8 text-center bg-white"
  "Tailwind.css classes for the HTML FOOTER."
  :type '(string))

(defcustom org-tailwind-class-top-button
  "float-right tracking-tight font-bold mt-1"
  "Tailwind.css classes for the HTML go to TOP button.
There are already some prefixed classes:
- p-2
- block
- mt-2
- ml-0"
  :type '(string))

(defcustom org-tailwind-class-search-bar
  "float-right mx-4 w-1/6 rounded-lg px-4 py-1 border-solid
border-2 border-gray-700 text-gray-400 focus:border-green-500
focus:text-gray-700"
  "Tailwind.css classes for the HTML SEARCH BAR."
  :type '(string))

(defcustom org-tailwind-class-search-bar-results-list
  "z-50 absolute w-5/6 sm:w-4/6 md:w-3/6 lg:w-2/6 xl:w-1/6
right-0 mt-12 mr-20 bg-white p-4 shadow-lg border
border-solid border-gray-500 rounded-md"
  "Tailwind.css classes for the HTML RESULTS LIST."
  :type '(string))

(defcustom org-tailwind-class-search-bar-results-item
  "p-2 block rounded-md hover:bg-green-300"
  "Tailwind.css classes for the HTML RESULTS ITEM.
Do not break the line with while inserting a `newline'. Use `\' at
the end."
  :type '(string))


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

<!-- Your CSS file should come here -->
<link href=\"./css/style.css\" rel=\"stylesheet\" />

<!-- Toc tree file -->
<script src=\"./js/toc_tree.js\"></script>
"
  "Links to be imported on the head of the HTML file."
  :type '(string))

(defcustom org-tailwind-header
  "<nav class=\"flex items-center justify-between flex-wrap p-4\">
  <div class=\"flex items-center flex-no-shrink mr-6\">
    <span class=\"font-semibold text-xl tracking-tight\">
      <a href=\"./index.html\">Notes</a>
    </span>
  </div>
  <div class=\"block flex-grow lg:flex lg:items-center lg:w-auto\">
    <div class=\"text-sm lg:flex-grow\">
      <a href=\"#top\" class=\"%s\">
        Top
      </a>
      <input id=\"search-bar\" onkeyup=\"search()\"
      onfocusin='showResults()'
      class=\"%s\" placeholder=\"Search...\"/>
      <ul id=\"search-bar-results\"
      class=\"%s\" style=\"display: none;\"></ul>
    </div>
  </div>
</nav>"
  "Contents of header in HTML."
  :type '(string))

(defcustom org-tailwind-sidebar
  "<div id=\"toc\"></div>"
  "Contents of sidebar in HTML."
  :type '(string))

(defcustom org-tailwind-footer
  "<p>Exported with org-tailwind</p>"
  "Contents of footer in HTML."
  :type '(string))

(defcustom org-tailwind-bottom-files
  "<script src=\"./js/prism.js\"></script>
<script src=\"./js/mermaid.min.js\"></script>
<script>mermaid.initialize({startOnLoad:true});</script>"
  "Javascript files to be imported at the bottom of the HTML file."
  :type '(string))

(defcustom org-tailwind-headlines
  "h1,h2,h3"
  "The level of the headlines to be included in the toc."
  :type '(string))

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
	// Add go to top button
	let top = document.createElement('a');
	top.innerHTML = 'Top';
	top.href = '#top';
	top.className += 'top %s';

    // The tags with the headlines
	let headlines = getElementsByTagNames('%s');

	if (headlines.length < 2) return false;

    // Populate the #toc div
	let toc = document.getElementById('toc');

    // Header counts for numbering
    let header_1 = 0;
    let header_2 = 0;
    let header_3 = 0;
    let header_4 = 0;
    let header_5 = 0;
    let header_6 = 0;

	for (var i=0;i < headlines.length;i++) {

		let tocHeader = document.createElement('a');

        // Tailwind.css classes
		tocHeader.className = 'rounded px-2 py-1';
        tocHeader.className += ' %s';

        // Make the first header active
        // This will be used for the scroll spy
        if (i == 0) {
            tocHeader.className += ' %s';
        }
	
        // Add the header to the table of contents
        toc.appendChild(tocHeader);

        switch (headlines[i].nodeName) {
          case 'H1':
            tocHeader.className += ' block mt-2 ml-0';
            header_1 += 1;
            header_2 = 0;
            header_3 = 0;
            header_4 = 0;
            header_5 = 0;
            header_6 = 0;

          	// Numbering
          	tocHeader.innerHTML += '<b>' + header_1 + '.</b> ';
            
            break;

          case 'H2':
            tocHeader.className += ' block ml-5';
            header_2 += 1;
            header_3 = 0;
            header_4 = 0;
            header_5 = 0;
            header_6 = 0;

          	// Numbering
          	tocHeader.innerHTML += '<b>' + header_1 + '.' + header_2 + '.</b> ';
            break;

          case 'H3':
            tocHeader.className += ' block ml-12';
            header_3 += 1;
            header_4 = 0;
            header_5 = 0;
            header_6 = 0;

          	// Numbering
          	tocHeader.innerHTML += '<b>' + header_1 + '.' + header_2 + '.' + header_3 + '.</b> ';
            break;

          case 'H4':
            tocHeader.className += ' block ml-20';
            header_4 += 1;
            header_5 = 0;
            header_6 = 0;

          	// Numbering
          	tocHeader.innerHTML += '<b>' + header_1 + '.' + header_2 + '.' + header_3 + '.' + header_4 + '.</b> ';
            break;

          case 'H5':
            tocHeader.className += ' block ml-32';
            header_5 += 1;
            header_6 = 0;

          	// Numbering
          	tocHeader.innerHTML += '<b>' + header_1 + '.' + header_2 + '.' + header_3 + '.' + header_4 + '.' + header_5 + '.</b> ';
            break;

          case 'H6':
            tocHeader.className += ' block ml-48';
            header_6 += 1;

          	// Numbering
          	tocHeader.innerHTML += '<b>' + header_1 + '.' + header_2 + '.' + header_3 + '.' + header_4 + '.' + header_5 + '.' + header_6 + '.</b> ';
            break;
        }       
        
        // Header title
        tocHeader.innerHTML += headlines[i].innerHTML;
		
        // Add a link to the header
		let headerId = headlines[i].id || 'toc-link-' + i;
		headlines[i].id = headerId;
        tocHeader.id = \"goto-\" + headerId;
		tocHeader.href = '#' + headerId;
	}
}

createTOC();

// Populate search bar
let searchBar = document.getElementById('search-bar')
let searchBarResults = document.getElementById('search-bar-results')
for(let i = 0; i < tocTree.length; i++) {
    let heading = tocTree[i]
    let item = document.createElement('li')
    let link = document.createElement('a')
    link.href = heading.file + '#toc-link-' + heading.index
    link.className = \"%s\"
    link.style.display = 'none'

    if (heading.name === heading.parent) {
        link.innerText = heading.name
    } else {
        link.innerText = heading.name + ' > ' + heading.parent
    }

    item.appendChild(link)
    searchBarResults.appendChild(item)
}

// Show results on search bar focus
function showResults(){
    searchBarResults.style.display = ''
}

function hideResults(){
    searchBarResults.style.display = 'none'
    searchBar.value = ''
}

// Search Bar
function search(){
    let searchBar = document.getElementById('search-bar')
    let filter = searchBar.value.toUpperCase()
    let count = 0

    let searchResults = searchBarResults.getElementsByTagName('li')

    for(let i = 0; i < searchResults.length; i++) {
        let link = searchResults[i].getElementsByTagName('a')[0]
        let txtValue = link.textContent || link.innerText

        if (txtValue.toUpperCase().indexOf(filter) > -1 && count < 10) {
            link.style.display = ''
            count += 1
        } else {
            link.style.display = 'none'
        }
    }
}


// Check if an element is visible
function isElementVisible (el, parent) {
    let rect = el.getBoundingClientRect();

    return (
        rect.top >= parent.getBoundingClientRect().top &&
        rect.left >= 0 &&
        rect.bottom <= (parent.clientHeight + parent.getBoundingClientRect().top) &&
        rect.right <= (parent.clientWidth)
    );
}


// Scroll Spy
// Everything is run inside the function because offsets
// are created due to tailwind.css classes as it changes
// the height/position of the elements after the page is loaded
function scrollSpy () {
  let headings = document.querySelectorAll('[id^=\"toc-link-\"]');
  let headingsTopOffsets = {};
  let i = 0;

  let selectedClassName = \"%s\";

  Array.prototype.forEach.call(headings, function(s) {
    headingsTopOffsets[s.id] = s.offsetTop;
  });
  
  
  let scrollEl = document.getElementById(\"content-container\");
  let scrollPosition = scrollEl.scrollTop + 150;

  let sidebar = document.getElementById('sidebar');

  for (i in headingsTopOffsets) {
    if (headingsTopOffsets[i] <= scrollPosition) {
      document.querySelector(`div#sidebar a.${selectedClassName}`).classList.remove(selectedClassName);
      document.querySelector(`a[id^=\"goto-${i}\"]`).classList.add(selectedClassName);

      // Scroll the sidebar to the current heading if not visible
      let tocItem = document.getElementById(`goto-${i}`);
      if (!isElementVisible(tocItem, sidebar)) {
          tocItem.scrollIntoView();
      }
    }
  }

};
"
  "Javascript code needed in the HTML file."
  :type '(string))

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

<div id=\"content\" class=\"top %s\" onclick=\"hideResults()\">
  <div id=\"sidebar\" class=\"%s\">
  %s
  </div>

  <div id=\"content-container\" class=\"%s\" onscroll=\"scrollSpy()\">
    <div id=\"top\"></div>
    <div id=\"inner-container\" class=\"%s\">
      %s
    </div>
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
- inner-container classes
- contents
- footer classes
- footer contents
- bottom page javascript
- bottom page files."
      :type '(string))


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
          (format org-tailwind-header
                  org-tailwind-class-top-button
                  org-tailwind-class-search-bar
                  org-tailwind-class-search-bar-results-list)
          org-tailwind-class-content
          org-tailwind-class-sidebar
          org-tailwind-sidebar
          org-tailwind-class-content-container
          org-tailwind-class-inner-container
          contents
          org-tailwind-class-footer
          org-tailwind-footer
          (format org-tailwind-javascript
                  org-tailwind-class-top-button
                  org-tailwind-headlines
                  org-tailwind-class-toc-items
                  org-tailwind-class-current-toc
                  org-tailwind-class-search-bar-results-item
                  org-tailwind-class-current-toc)
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
  (let* ((code-text (org-element-property :value code))
        (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                             (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                        r2)))
    (format "<code class=\"%s\">%s</code>" org-tailwind-class-code escaped-text)))

(defun org-tailwind-verbatim (verbatim contents info)
  "Transcode VERBATIM from Org to HTML."
  (let* ((code-text (org-element-property :value verbatim))
        (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                             (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                        r2)))
    (format "<code class=\"%s\">%s</code>" org-tailwind-class-verbatim escaped-text)))

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
         (is-image-p (string-match-p "<img " contents))
		 (is-video-p (string-match-p "<video " contents)))
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
	 ;; Put video inside a div with description
     (is-video-p (format "<div class=\"%s\">%s</div>"
                         org-tailwind-class-video-div
                         contents))
     ;; If is a checkbox
     (is-checkbox-p
      (format "<p class=\"%s\">%s %s</p>"
              org-tailwind-class-paragraph
              (org-tailwind-checkbox checkbox)
              contents))
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
         (video-description "<p class=\"%s\">%s</p>")
         (video-tag "<video class=\"%s\" controls><source src=\"%s\" type=\"video/%s\"/></video>%s")
         (image-description "<p class=\"%s\">%s</p>")
         (image-tag "<img class=\"%s\" src=\"%s\"/>%s"))
    (cond
     ;; Is the link a video
     ((or (equalp file-extension "mp4")
          (equalp file-extension "avi")
          (equalp file-extension "mkv")
          (equalp file-extension "webm")
          (equalp file-extension "mpeg4")
          (equalp file-extension "3gp"))
      (format video-tag
              org-tailwind-class-video
              path
              file-extension
              (if description
                  (format image-description
                          org-tailwind-class-video-description
                          description)
                "")))
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
              (concat type ":" (replace-regexp-in-string "\\.org" ".html" path))
              contents))
     ;; Any other link
     (t (format link-tag
                org-tailwind-class-link
                (concat type ":" path)
                contents)))))

(defun org-tailwind-blockquote (blockquote contents info)
  "Transcode BLOCKQUOTE from Org to HTML."
  (format
   "<div class=\"%s\"><blockquote class=\"%s\">%s</blockquote><p class=\"%s\">%s</p></div>"
   org-tailwind-class-blockquote-container
   org-tailwind-class-blockquote
   contents
   org-tailwind-class-blockquote-author
   (org-element-property :name blockquote)))

(defvar org-tailwind--src-block-open
  "<div class=\"%s\"><pre class=\"%s\" %s>"
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
             (concat "command-line " org-tailwind-class-pre)
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
         (escaped-text (let* ((r1 (replace-regexp-in-string "<" "&lt;" code-text))
                              (r2 (replace-regexp-in-string ">" "&gt;" r1)))
                         r2))
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
             escaped-text))))

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
    (format "<div class=\"%s\">
<pre class=\"%s\">
<code class=\"language-%s\">%s</code>
</pre>
</div>"
            org-tailwind-class-example-container
            org-tailwind-class-example
            (if language language "none")
            code-text)))

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
           (format "<div class=\"%s\"><div class=\"mermaid %s\">%s</div><p class=\"%s\">%s</p></div>"
                   org-tailwind-class-mermaid-container
                   org-tailwind-class-mermaid-block
                   contents
                   org-tailwind-class-mermaid-block-title
                   (if name name "")))
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
  (let* ((header-p (org-export-table-row-in-header-p
                   (org-export-get-parent table-cell) info))
         (text-alignment (org-export-table-cell-alignment
                          table-cell
                          info)))
    (if header-p
        (format "<th class=\"%s\">%s</th>" org-tailwind-class-table-header-cell contents)
      (if contents
          (format "<td class=\"%s %s\">%s</td>"
                  org-tailwind-class-table-body-cell
                  (concat "text-"
                          (cond
                           ((equal text-alignment 'left) "left")
                           ((equal text-alignment 'center) "center")
                           ((equal text-alignment 'right) "right")))
                  contents)
        (format "<td class=\"%s\"></td>"
                org-tailwind-class-table-empty-body-cell)))))


;; (message "%s" (org-export-table-cell-alignment table-cell info))

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
          (concat (format (car row) org-tailwind-class-table-last-body-row)
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
    (format "<div class=\"%s\"><table class=\"%s\">%s</table>%s</div>"
            org-tailwind-class-table-container
            org-tailwind-class-table
            contents
            (if name
                (format "<p class=\"%s\">%s</p>"
                        org-tailwind-class-table-description
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


;;; Search bar toc tree

(defun org-tailwind--json-toc-item (file title heading index)
  "Transform HEADING and TITLE to a json object."
  (format
   "\t{\n\t\t\"name\": \"%s\", \n\t\t\"index\": \"%s\", \n\t\t\"parent\": \"%s\", \n\t\t\"file\": \"%s\"\n\t},"
   (replace-regexp-in-string "\"" "'" heading)
   index
   title
   (replace-regexp-in-string "\\.org$" ".html" file)))


(defun org-tailwind--json-toc-all-items (headings title file)
  "Transform all the HEADINGS into an object with the heading and
the TITLE."
  (string-join
   (-map
    (lambda (heading)
      (org-tailwind--json-toc-item
       file
       title
       (-first-item heading)
       (-last-item heading)))
    headings)
   "\n"))


(defun org-tailwind--json-toc (filename file-tree output-directory)
  "Read all the exported files and search for headings.
The JSON format will"

  ;; Create an empty list to append all the headings
  (setq headings '())

  (setq headings-max-level (+ 1 (s-count-matches "," org-tailwind-headlines)))

  ;; Create a temporary buffer to store the text of the file
  (with-temp-buffer
    ;; Paste all the text inside the file to the temp buffer
    (insert-file-contents filename)
    ;; Start the index count
    ;; this will be used to target the element position in html
    (setq count 0)
    ;; Search for the org headings
    (while (search-forward-regexp
            (format "^\\*\\{1,%s\\}\s.*$" headings-max-level)
            nil
            t)
      ;; Split the org headings and text
      (let ((cur-line (string-join (cdr (split-string (match-string 0) "\s")) " ")))
        (setq headings (append headings `((,cur-line ,count))))
        (setq count (+ count 1)))))

  (setq title (-first-item (-first-item headings)))
  (setq json-headings
        (org-tailwind--json-toc-all-items headings title filename))

  ;; Write the json toc tree to file
  (write-region json-headings nil file-tree 'append))


(defun org-tailwind--json-toc-all-files (notes-directory output-directory)
  "Read all the files in the notes directory and create a toc tree."
 
  (setq files (directory-files notes-directory nil ".org"))
 
  (setq file-tree (concat output-directory "/js/" "toc_tree.js"))

  ;; Clear the file
  (write-region "" nil file-tree)

  ;; Open the js object
  (write-region "const tocTree = [\n" nil file-tree 'append)

  ;; Append toc
  (-map
   (lambda (file)
     (let ((file-path (concat "./" file)))
       (org-tailwind--json-toc file-path file-tree output-directory))) files)

  ;; Close the js object
  (write-region "\n];" nil file-tree 'append))

;;(org-tailwind--json-toc-all-files "./resources" "./out")


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
  (org-tailwind--json-toc-all-files (file-name-directory filename) pub-dir)
  (org-publish-org-to 'tailwind filename
                      (concat (when (> (length org-html-extension) 0) ".")
                              (or (plist-get plist :html-extension)
                                  org-html-extension
                                  "html"))
                      plist pub-dir))

(provide 'ox-tailwind)

;;; ox-tailwind.el ends here
