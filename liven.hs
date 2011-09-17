module Liven ()
       where

import Text.XmlHtml
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T


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


-- selector examples
-- "a[href]" => [hasAttr "href", hasTag "a"]
-- "#name.kls" => hasClass "kls"  hasId "name"

checkPreds :: [(Node -> Bool)] -> Node -> Bool
checkPreds ps n = all id $ map (flip ($) n) ps

-- Transforms take a node, and return none, one or many Nodes.
type Transform = Node -> [Node]
type Selector = Node -> Bool

-- The high-level:
run :: [(Selector, Transform)] -> Document -> Document
run ts doc = foldl (\doc (s, t) -> doTransform s t doc) doc ts

doTransform :: Selector -> Transform -> Document -> Document
doTransform s t doc = docmap t' doc
  where
    t' = \n -> if s n then t n else [n]


nmap :: (Node -> [Node]) -> [Node] -> [[Node]]
nmap f [] = []
nmap f (e:es) = case e of
  (Element _ _ children) -> (f e { elementChildren = concat (nmap f children) }):nmap f es
  otherwise -> (f e):nmap f es
  

docmap f doc = doc { docContent = concat $ nmap f $ docContent doc }


testDoc = let htmlBS = BSC.pack "<!doctype html><html><head><title></title></head><body class='c1 c3'><p>p1</p><p>p2</p><section id=aThingHere><p>p3</p></section></body></html>"
              doc = parseHTML "testDoc" htmlBS
          in case doc of
            (Left anError) -> error anError
            (Right doc) ->  doc

