module Hautobot
       where

import Text.XmlHtml
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

-- Transforms take a node, and return none, one or many Nodes.
type Transform = Node -> [Node]
-- A Selector is a list of lists of predicates.
type Selector = [[Node -> Bool]]


-- Predicates

hasId :: String -> Node -> Bool
hasId id = \node -> getID node == Just (T.pack id)
  where
    getID = getAttribute $ T.pack "id"


hasTag :: String -> Node -> Bool
hasTag name = \node -> tagName node == Just (T.pack name)


hasAttr :: String -> Node -> Bool
hasAttr attr = \node -> case getAttribute (T.pack attr) node of
  Just _  -> True
  Nothing -> False


hasAttrWord :: String -> String -> Node -> Bool
hasAttrWord attr word = \node -> getAttribute (T.pack attr) node `matches` (T.pack word)
  where
    matches Nothing _ = False
    matches (Just src) chk = any (==chk) $ T.words . T.strip $ src


hasClass :: String -> Node -> Bool
hasClass = hasAttrWord "class"


-- Transformers

setAttr :: String -> String -> Transform
setAttr k v = (:[]) . setAttribute (T.pack k) (T.pack v)


setTag :: String -> Transform
setTag t n = [n { elementTag = T.pack t }]


-- takes a function of Maybe (T.Text currentVal) -> Maybe (T.Text newVal)
-- returning `Nothing` will delete the attribue.
modifyAttr :: String -> (Maybe T.Text -> Maybe T.Text) -> Transform
modifyAttr k f = \node -> let attrName = (T.pack k)
                              val = getAttribute attrName node
                              val' = f val
                              notThisAttr (k, _) = k /= attrName
                          in case val' of
                            Nothing      -> [node { elementAttrs = filter notThisAttr $ 
                                                                   elementAttrs node} ]
                            Just attrVal -> [setAttribute attrName attrVal node]

delAttr k = modifyAttr k $ const Nothing

getAttrWords :: String -> Node -> [T.Text]
getAttrWords k = let attrName = (T.pack k)
             in \node -> case (getAttribute attrName node) of
               Nothing -> []
               Just val -> T.words . T.strip $ val

setAttrWords :: String -> [T.Text] -> Transform
setAttrWords k words = \node -> [setAttribute (T.pack k) (T.unwords words) node]


addAttrWord :: String -> String -> Transform
addAttrWord k v = \node ->
  let newWord = (T.pack v)
      words = getAttrWords k node
      newWords = newWord:(filter (/=newWord) words)
  in setAttrWords k newWords node

delAttrWord :: String -> String -> Transform
delAttrWord k v = \node ->
  setAttrWords k (filter (/=(T.pack v)) $ getAttrWords k node) node


addClass :: String -> Transform
addClass cls = addAttrWord "class" cls

removeClass :: String -> Transform
removeClass cls = delAttrWord "class" cls


replaceWith :: [Node] -> Transform
replaceWith ns = const ns

remove = replaceWith []


content :: [Node] -> Transform
content ns = \node -> [node { elementChildren = ns }]

append :: [Node] -> Transform
append ns = \node -> [node { elementChildren = (elementChildren node) ++ ns }]

at :: [(Selector, Transform)] -> Transform
at sts n = foldl (\ns (s, t) -> transform' s t ns) [n] sts

-- selector examples
-- "a[href]" => [[hasAttr "href", hasTag "a"]]
-- "#name.kls" => [[hasClass "kls", hasId "name"]]
-- "section#name article.story h1" => [[hasTag "section", hasId "name"], [hasTag "article", hasClass "story"], [hasTag "h1"]]

checkPreds :: [Node -> Bool] -> Node -> Bool
checkPreds ps n = all id $ map (flip ($) n) ps

data Match = Match                 -- node matches
           | PartialMatch Selector -- node matches prefix. contains remaining 
                                   --   selector to be matched
           | NoMatch               -- node does not match

matchSelector :: Selector -> Node -> Match
matchSelector [] n = NoMatch
matchSelector [s] n = if checkPreds s n then Match else NoMatch
matchSelector (s:ss) n = if checkPreds s n then PartialMatch ss else NoMatch


-- The high-level:
runTransforms :: [(Selector, Transform)] -> Document -> Document
runTransforms ts doc = foldl (\doc (s, t) -> transform s t doc) doc ts

transform :: Selector -> Transform -> Document -> Document
transform s t doc = modifyDocContent (transform' s t) doc

transform' :: Selector -> Transform -> [Node] -> [Node]
transform' s t nodes = concat $ nmap (t' s) nodes
  where
    t' s' = \n -> case matchSelector s' n of
                    Match -> t n
                    PartialMatch remainingSel ->
                      [n { elementChildren = concat $ nmap (t' remainingSel) $
                                             childNodes n }]
                    NoMatch -> [n]


nmap :: (Node -> [Node]) -> [Node] -> [[Node]]
nmap f [] = []
nmap f (e:es) = case e of
  (Element _ _ children) -> (f e { elementChildren = concat (nmap f children) }):nmap f es
  otherwise -> (f e):nmap f es
  

docmap f doc = doc { docContent = concat $ nmap f $ docContent doc }
modifyDocContent f doc = doc { docContent = f $ docContent doc }


testDoc = let htmlBS = BSC.pack "<!doctype html><html><head><title></title></head><body class='c1 c3'><p>p1</p><p>p2</p><section id='noId'><article><p>p3</p></article></section></body></html>"
              doc = parseHTML "testDoc" htmlBS
          in case doc of
            (Left anError) -> error anError
            (Right doc) ->  doc


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
