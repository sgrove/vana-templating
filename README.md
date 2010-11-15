Vana Templating
========

An utterly sensible templating system written in Common Lisp. One of the guiding principles is "lowest-friction", which says that the smallest unit that a designer is likely to work in cannot have any more friction to use than they're already useful. That means that opening a tag with (<: isn't acceptable, since it's two characters longer than HTML's <.

Read about it more [here][1]

This is a support package for the [Vana web framework][2]

Installation
------------------
Dependencies:
`parenscript`
`vana-utils`


Installation
------------------

    > (ql:quickload 'vana-templating)

Usage
-----------
The syntax is meant to follow the flexibility of html, keeping it close to what an html designer is used to while allowing for pieces to be easily abstracted away.

A full call would be `(tag-name '(attribute-name attribute-value) "content")`, but everything outside of tag-name is optional. Also, tags can be nested, just as in html:

    > (div "A div tag "
           (span "surrounding")
           " a span tag")

will output `<div>A div tag <span>surrounding</span> a span tag</div>`

Different tag cases:

    (span)
    (p '(id \#body))
    (div '(id \#body) "Some content")
    (div '(id \#body) (p "some other content))
    (div "hello world")

Because it's just plain lisp, we can actually start siloing pieces apart, making it more modular. For example:

    ;; example-header-widget
    > (div '(id "header" class "header")
           (h1 '(style "float:left") "Example of templating done in Common Lisp")
           (ul '(id "navbar" class "horizontal_list")
               (li (a '(href "/home")    (img '(src "/images/nav_item.png")) " Home"))
               (li (a '(href "/pricing") (img '(src "/images/nav_item.png")) " Pricing"))
               (li (a '(href "/about")   (img '(src "/images/nav_item.png")) " About"))
               (li (a '(href "/blog")    (img '(src "/images/nav_item.png")) " Blog"))
               (li (a '(href "/contact") (img '(src "/images/nav_item.png")) " Contact"))))

Will output (correct, though admittedly not so pretty)

    <div id=\"header\" class=\"header\"><h1 style=\"float:left\">Example of templating done in Common Lisp</h1><ul id=\"navbar\" class=\"horizontal_list\"><li><a href=\"/home\"><img src=\"/images/nav_item.png\" /> Home</a></li><li><a href=\"/pricing\"><img src=\"/images/nav_item.png\" /> Pricing</a></li><li><a href=\"/about\"><img src=\"/images/nav_item.png\" /> About</a></li><li><a href=\"/blog\"><img src=\"/images/nav_item.png\" /> Blog</a></li><li><a href=\"/contact\"><img src=\"/images/nav_item.png\" /> Contact</a></li></ul></div>



TODO
-------
 * Variables/string manipulation in the attributes section causes *a lot* of extra code. May call for a proper macro.
 * Where an abstracted function tries to return two sibling tags not contained in a parent tag, on the latter of the two will be returned. It's lisp symantics, and there are a few ways to get around this, right now with list->string. The challenge is to follow the guiding principle for it.

License
---------------

Released under the MIT license, please see `LICENSE` for more details

Thanks
-------------

  [1]: http://trapm.com/vana-templating-an-utterly-sensible-templatin
  [2]: https://github.com/sgrove/vana

