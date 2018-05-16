{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- |
-- Module      : Network.AWS.Data.XML
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.XML where

import           Control.Monad

import           Data.Bifunctor
import           Data.Conduit
import           Data.Conduit.Lazy           (lazyConsume)
import           Data.Maybe
import           Data.Monoid                 (Monoid)
import           Data.Semigroup              (Semigroup, (<>))
import           Data.Traversable            (traverse)
import           Data.XML.Types              (Event (..))

import           GHC.Exts

import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text

import           Numeric.Natural

import           System.IO.Unsafe            (unsafePerformIO)

import           Text.XML
import           Text.XML.Unresolved         (toEvents)

import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Conduit.List           as Conduit
import qualified Text.XML.Stream.Render      as Stream

infixl 7 .@, .@?

(.@) :: FromXML a => [Node] -> Text -> Either String a
ns .@ n = findElement n ns >>= parseXML

(.@?) :: FromXML a => [Node] -> Text -> Either String (Maybe a)
ns .@? n =
    case findElement n ns of
        Left _   -> Right Nothing
        Right xs -> parseXML xs

infixr 7 @=, @@=

(@=) :: ToXML a => Name -> a -> XML
n @= x =
    case toXML x of
        XNull -> XNull
        xs    -> XOne . NodeElement $ mkElement n xs

(@@=) :: ToText a => Name -> a -> XML
n @@= x = XAttr n (toText x)

decodeXML :: FromXML a => LazyByteString -> Either String a
decodeXML lbs =
    bimap show documentRoot (parseLBS def lbs)
        >>= parseXML . childrenOf

-- The following is taken from xml-conduit.Text.XML which uses
-- unsafePerformIO anyway, with the following caveat:
--   'not generally safe, but we know that runResourceT
--    will not deallocate any of the resources being used
--    by the process.'
encodeXML :: ToElement a => a -> LazyByteString
encodeXML x = LBS.fromChunks . unsafePerformIO . lazyConsume
     $ Conduit.sourceList (toEvents doc)
    .| Conduit.map rename
    .| Stream.renderBytes def
  where
    doc = toXMLDocument $ Document
        { documentRoot     = root
        , documentEpilogue = []
        , documentPrologue =
            Prologue
                { prologueBefore  = []
                , prologueDoctype = Nothing
                , prologueAfter   = []
                }
        }

    rename = \case
        EventBeginElement n xs -> EventBeginElement (f n) (map (first f) xs)
        EventEndElement   n    -> EventEndElement   (f n)
        evt                    -> evt
      where
        f n | isNothing (nameNamespace n) = n { nameNamespace = ns }
            | otherwise                   = n

    ns   = nameNamespace (elementName root)
    root = toElement x

class FromXML a where
    parseXML :: [Node] -> Either String a

instance FromXML [Node] where
    parseXML = pure

instance FromXML a => FromXML (Maybe a) where
    parseXML [] = pure Nothing
    parseXML ns = Just <$> parseXML ns

instance FromXML Text where
    parseXML = fmap (fromMaybe mempty) . withContent "Text"

instance FromXML Char       where parseXML = parseXMLText "Char"
instance FromXML ByteString where parseXML = parseXMLText "ByteString"
instance FromXML Int        where parseXML = parseXMLText "Int"
instance FromXML Integer    where parseXML = parseXMLText "Integer"
instance FromXML Natural    where parseXML = parseXMLText "Natural"
instance FromXML Double     where parseXML = parseXMLText "Double"
instance FromXML Bool       where parseXML = parseXMLText "Bool"

class ToElement a where
    toElement :: a -> Element

instance ToElement Element where
    toElement = id

-- | Convert to an 'Element', only if the resulting element contains @> 0@ nodes.
maybeElement :: ToElement a => a -> Maybe Element
maybeElement x =
    case toElement x of
        e@(Element _ _ ns)
            | null ns   -> Nothing
            | otherwise -> Just e

-- | Provides a way to make the operators for ToXML instance
-- declaration be consistent WRT to single nodes or lists of nodes.
data XML
    = XNull
    | XAttr Name Text
    | XOne  Node
    | XMany [(Name, Text)] [Node]
      deriving (Show)

instance Semigroup XML where
    XNull <> XNull = XNull
    a     <> XNull = a
    XNull <> b     = b
    a     <> b     =
        XMany (listXMLAttributes a <> listXMLAttributes b)
              (listXMLNodes      a <> listXMLNodes      b)

instance Monoid XML where
    mempty = XNull
    mappend = (<>)

listXMLNodes :: XML -> [Node]
listXMLNodes = \case
    XNull      -> []
    XAttr {}   -> []
    XOne  n    -> [n]
    XMany _ ns -> ns

listXMLAttributes :: XML -> [(Name, Text)]
listXMLAttributes = \case
    XNull      -> []
    XAttr n t  -> [(n, t)]
    XOne  {}   -> []
    XMany as _ -> as

class ToXML a where
    toXML :: a -> XML

instance ToXML XML where
    toXML = id

instance ToXML a => ToXML (Maybe a) where
    toXML (Just x) = toXML x
    toXML Nothing  = XNull

instance ToXML Text       where toXML = toXMLText
instance ToXML ByteString where toXML = toXMLText
instance ToXML Int        where toXML = toXMLText
instance ToXML Integer    where toXML = toXMLText
instance ToXML Natural    where toXML = toXMLText
instance ToXML Double     where toXML = toXMLText
instance ToXML Bool       where toXML = toXMLText

parseXMLList :: FromXML a
             => Text
             -> [Node]
             -> Either String [a]
parseXMLList n = traverse parseXML . mapMaybe (childNodesOf n)

parseXMLText :: FromText a => String -> [Node] -> Either String a
parseXMLText n = withContent n >=>
    maybe (Left $ "empty node list, when expecting single node " ++ n)
        fromText

toXMLList :: (IsList a, ToXML (Item a)) => Name -> a -> XML
toXMLList n = XMany [] . map (NodeElement . mkElement n) . toList

toXMLText :: ToText a => a -> XML
toXMLText = XOne . NodeContent . toText

mkElement :: ToXML a => Name -> a -> Element
mkElement n (toXML -> x) =
    Element n (fromList (listXMLAttributes x)) (listXMLNodes x)

withContent :: String -> [Node] -> Either String (Maybe Text)
withContent k = \case
    []              -> Right Nothing
    [NodeContent x] -> Right (Just x)
    _               -> Left $ "encountered many nodes, when expecting text: " ++ k

-- | Find a specific named NodeElement, at the current depth in the node tree.
--
-- Fails if absent.
findElement :: Text -> [Node] -> Either String [Node]
findElement n ns =
     missingElement n ns
   . listToMaybe
   $ mapMaybe (childNodesOf n) ns

-- | Find the first specific named NodeElement, at any depth in the node tree.
--
-- Fails if absent.
firstElement :: Text -> [Node] -> Either String [Node]
firstElement n ns =
      missingElement n ns
    . listToMaybe
    $ mapMaybe go ns
  where
    go x = case x of
        NodeElement e
            | Just n == localName x -> Just (childrenOf e)
            | otherwise             -> listToMaybe (mapMaybe go (elementNodes e))
        _                           -> Nothing

childNodesOf :: Text -> Node -> Maybe [Node]
childNodesOf n x = case x of
    NodeElement e
        | Just n == localName x
              -> Just (childrenOf e)
    _         -> Nothing

childrenOf :: Element -> [Node]
childrenOf e = elementNodes e <> map node (toList (elementAttributes e))
  where
    node (k, v) = NodeElement (Element (name k) mempty [NodeContent v])

    name k = Name
        { nameLocalName = fromMaybe "" (namePrefix k) <> ":" <> nameLocalName k
        , nameNamespace = mempty
        , namePrefix    = mempty
        }

localName :: Node -> Maybe Text
localName = \case
    NodeElement e -> Just (nameLocalName (elementName e))
    _             -> Nothing

-- | An inefficient mechanism for retreiving the root
-- element name of an XML document.
rootElementName :: LazyByteString -> Maybe Text
rootElementName bs =
    either (const Nothing)
           (Just . nameLocalName . elementName . documentRoot)
           (parseLBS def bs)

missingElement :: Text -> [Node] -> Maybe a -> Either String a
missingElement n ns = maybe (Left err) Right
  where
    err = "unable to find element "
        ++ show n
        ++ " in nodes "
        ++ show (mapMaybe localName ns)
