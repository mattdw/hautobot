-- | Hautobot is a select-and-transform approach to HTML (and XML)
-- templating. It is a highly inferior and highly incomplete
-- implementation of the basic ideas in Christophe Grand's Enlive
-- library for Clojure (<https://github.com/cgrand/enlive>).
module Hautobot
       where

import Text.XmlHtml
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Blaze.ByteString.Builder

-- ** Some Types

-- | @Transforms@ take a @Node@, and return none, one or many @Node@s.
type Transform = Node -> [Node]

-- | A @Selector@ is a list of lists of predicates. Each inner list
-- represents a \"descendant step\". That is, each inner list must
-- match a single node, and each subsequent inner list must match a
-- node inside the previous. Examples:
-- 
-- > "a[href]"                       => [[hasTag "a", hasAttr "href"]]
-- > "#name.kls"                     => [[hasId "name", hasClass "kls"]]
-- > "section#name article.story h1" => [[hasTag "section", hasId "name"], 
-- >                                     [hasTag "article", hasClass "story"],
-- >                                     [hasTag "h1"]]
type Selector = [[Node -> Bool]]


-- ** Predicates

-- | Does the Node have the given id? (case-sensitive both on the
-- attribute name @id@, and on the value.)
hasId :: String -> Node -> Bool
hasId id = \node -> getID node == Just (T.pack id)
  where
    getID = getAttribute $ T.pack "id"

-- | Is the Node an element with the given tag name? (case-sensitive)
hasTag :: String -> Node -> Bool
hasTag name = \node -> tagName node == Just (T.pack name)

-- | Does the Node have an attribute with the given name?
hasAttr :: String -> Node -> Bool
hasAttr attr = \node -> case getAttribute (T.pack attr) node of
  Just _  -> True
  Nothing -> False

-- | Does the Node contain `word` (space-delimited) within `attr`?
hasAttrWord :: String          -- ^ attribute name
               -> String       -- ^ word to look for in attribute value
               -> Node -> Bool
hasAttrWord attr word = \node -> getAttribute (T.pack attr) node `matches` (T.pack word)
  where
    matches Nothing _ = False
    matches (Just src) chk = any (==chk) $ T.words . T.strip $ src

-- | Does the Node have the given class?
hasClass :: String -> Node -> Bool
hasClass = hasAttrWord "class"


-- ** Transformers

-- | Set an attribute (overwriting any existing value).
setAttr :: String              -- ^ attribute name
           -> String           -- ^ attribute value
           -> Transform
setAttr k v = (:[]) . setAttribute (T.pack k) (T.pack v)

-- | Rename a tag.
setTag :: String -> Transform
setTag t n = [n { elementTag = T.pack t }]

-- | Modify attribute with a function.
-- Function is @Maybe (Text currentVal) -> Maybe (Text newVal)@.
-- Returning @Nothing@ will delete the attribue.
modifyAttr :: String -> (Maybe T.Text -> Maybe T.Text) -> Transform
modifyAttr k f = \node -> let attrName = (T.pack k)
                              val = getAttribute attrName node
                              val' = f val
                              notThisAttr (k, _) = k /= attrName
                          in case val' of
                            Nothing      -> [node { elementAttrs = filter notThisAttr $ 
                                                                   elementAttrs node} ]
                            Just attrVal -> [setAttribute attrName attrVal node]

-- | Remove an attribute.
delAttr :: String -> Transform
delAttr k = modifyAttr k $ const Nothing

-- | Get an attribute value, split into words.
-- Words are just white-space delimited.
getAttrWords :: String -> Node -> [T.Text]
getAttrWords k = let attrName = (T.pack k)
             in \node -> case (getAttribute attrName node) of
               Nothing -> []
               Just val -> T.words . T.strip $ val

-- | Set an attribute from a list of words. Joins words with spaces.
setAttrWords :: String -> [T.Text] -> Transform
setAttrWords k words = \node -> [setAttribute (T.pack k) (T.unwords words) node]

-- | Prepend a word to an attribute. (Won't create duplicates.)
addAttrWord :: String             -- ^ attribute name
               -> String          -- ^ string to add (doesn't validate for wordiness)
               -> Transform
addAttrWord k v = \node ->
  let newWord = (T.pack v)
      words = getAttrWords k node
      newWords = newWord:(filter (/=newWord) words)
  in setAttrWords k newWords node

-- | Remove a word from anywhere in an attribute.
delAttrWord :: String             -- ^ attribute name
               -> String          -- ^ word to delete
               -> Transform
delAttrWord k v = \node ->
  setAttrWords k (filter (/=(T.pack v)) $ getAttrWords k node) node

-- | Add a class name to an element. (Without duplicates.)
addClass :: String -> Transform
addClass cls = addAttrWord "class" cls

-- | Remove a class name from an element.
removeClass :: String -> Transform
removeClass cls = delAttrWord "class" cls

-- | Replace the node with the provide node(s).
replaceWith :: [Node] -> Transform
replaceWith ns = const ns

-- | Remove the Node from the document.
remove :: Transform
remove = replaceWith []

-- | Replace all of the node's children with the given node(s).
content :: [Node] -> Transform
content ns = \node -> [node { elementChildren = ns }]

-- | Append node(s) after the existing children of this node.
-- (i.e. just inside the closing tag of this node.)
append :: [Node] -> Transform
append ns = \node -> [node { elementChildren = (elementChildren node) ++ ns }]

-- | Prepend node(s) before existing children.
prepend :: [Node] -> Transform
prepend ns = \node -> [node { elementChildren = ns ++ (elementChildren node) }]

-- | @at@ allows you to nest selector-transformations.  You can select
-- a node in the usual fashion, then use @at@ to select and transform
-- only the children of that node. (This may be a premature
-- optimisation. It may also be useful.)
at :: [(Selector, Transform)] -> Transform
at sts n = foldl (\ns (s, t) -> transform' s t ns) [n] sts

-- ** Internal matching

-- | Check a single selector step against a node.
checkPreds :: [Node -> Bool] -> Node -> Bool
checkPreds ps n = all id $ map (flip ($) n) ps

-- | data type to represent matches against node. PartialMatch
-- represents a so-far-successful multi-step match.
data Match = Match      -- ^ Node matches Selector.
           | PartialMatch Selector 
                        -- ^ Node matches prefix of Selector. Contains
                        -- remaining selector steps to be matched.
           | NoMatch    -- ^ Node does not match.

-- | Match a single selector step, returning the appropriate Match.
matchSelector :: Selector -> Node -> Match
matchSelector [] n = NoMatch
matchSelector [s] n = if checkPreds s n then Match else NoMatch
matchSelector (s:ss) n = if checkPreds s n then PartialMatch ss else NoMatch

-- ** High-level entry points

-- | run a list of `(Selector, Transform)` pairs across a document
runTransforms :: [(Selector, Transform)] -> Document -> Document
runTransforms ts doc = foldl (\doc (s, t) -> transform s t doc) doc ts

-- | run a single Selector and Transform across a document
transform :: Selector -> Transform -> Document -> Document
transform s t doc = modifyDocContent (transform' s t) doc

-- | As above, but operates on [Node] rather than Document.  Used
-- internally, by `at`, and potentially for fragments that don't
-- belong to a document.
transform' :: Selector -> Transform -> [Node] -> [Node]
transform' s t nodes = concat $ nmap (t' s) nodes
  where
    t' s' = \n -> case matchSelector s' n of
                    Match -> t n
                    PartialMatch remainingSel ->
                      [n { elementChildren = concat $ nmap (t' remainingSel) $
                                             childNodes n }]
                    NoMatch -> [n]

-- | A version of @map@ for the list-tree-list structure of @[Node]@.
nmap :: (Node -> [Node]) -> [Node] -> [[Node]]
nmap f [] = []
nmap f (e:es) = case e of
  (Element _ _ children) -> (f e { elementChildren = concat (nmap f children) }):nmap f es
  otherwise -> (f e):nmap f es
  
-- | Document-aware @map@, for the top-level.
docmap :: (Node -> [Node]) -> Document -> Document
docmap f doc = doc { docContent = concat $ nmap f $ docContent doc }
-- | Document-aware way to apply a function to all Nodes.
modifyDocContent :: ([Node] -> [Node]) -> Document -> Document
modifyDocContent f doc = doc { docContent = f $ docContent doc }

-- ** Examples

-- | A basic sample document
testDoc :: Document
testDoc = let htmlBS = BSC.pack "<!doctype html><html><head><title></title></head><body class='c1 c3'><p>p1</p><p>p2</p><section id='noId'><article><p>p3</p></article></section></body></html>"
              doc = parseHTML "testDoc" htmlBS
          in case doc of
            (Left anError) -> error anError
            (Right doc) ->  doc

-- | A (messy) example of some transforms
doTest :: Document
doTest = 
  transform
  [[hasTag "article"]]
  (at [([[hasTag "h1"]], setAttr "id" "title")] >=>
   append [Element (T.pack "blockquote") [] [TextNode (T.pack "a quote")]] >=>
   at [([[hasTag "blockquote"]], addClass "quote-class")])
  $ transform 
  [[hasTag "body"]]
  (addClass "test-page" >=> 
   append [TextNode (T.pack "Some Text")])
  $ transform [[hasTag "p"]]
  (addClass "old-p" >=>
   replaceWith [Element (T.pack "p") [] [TextNode (T.pack "111")],
                Element (T.pack "p") [] [TextNode (T.pack "222")]] >=>
   addClass "new-p")
  $ transform 
  [[hasTag "section"], [hasTag "p"]]
  (setTag "h1" >=> 
   append [TextNode (T.pack "here is some updated content")] >=> 
   setAttr "id" "noeyedeer") testDoc 

-- | Utility function for debugging and testing.
documentToString :: Document -> String
documentToString doc = BSC.unpack $ Blaze.ByteString.Builder.toByteString $ render doc
