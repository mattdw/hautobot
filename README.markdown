**Don't use this, it's slow, broken, and incomplete.**

hautobot is a Haskell approximation of Christophe Grand's Enlive
(<https://github.com/cgrand/enlive>).

It depends on the Text.XmlHtml package
(<http://hackage.haskell.org/package/xmlhtml-0.1.5.2>), which it uses
for its internal datastructures and lower-level APIs.

Apologies for the name.

## Details

It is likely terribly inefficient; currently each transform results in
a full traversal of the document tree (although this can be somewhat
mitigated by use of the 'at' function to traverse only sub-trees.)

Selectors are currently pretty basic; a selector is just a list of
lists of predicates. All predicates in a step (inner list) must match,
and multiple steps represent nesting/descendants. For example:

    [[hasTag "section", hasId "main"], [hasTag "article", hasClass "story]]

…would match all `<article class="story">` inside `<section id="main">`. In 
future this library will support a string-based CSS-like selector syntax, so 
you could do `"section#main article.story"` for the above. Obviously this 
would be a marked improvement.

Transforms are (list-monadic) functions of `Node -> [Node]`, and can be composed
with `>=>` and `<=<`.

## Use

Look at `testDoc` and `doTest` at the bottom of the main source file. In
brief:

    let document = parseHTML "a name" htmlAsByteString
      selector = [[hasTag "body"], [hasTag "article"]]
      transFun = addClass "classname" >=> 
                 append [TextNode (T.pack "Some Content")] >=> 
                 setAttr "data-foo" "bar"
    in render $
      -- like this
      transform selector transFun document
      -- or like this
      runTransforms [(selector, transFun), …] document

## License

Copyright (c) 2011, Matthew Wilson.

Distributed under the BSD-3 License.
