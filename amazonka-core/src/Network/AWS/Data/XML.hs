{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Network.AWS.Data.XML
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.XML
  ( XMLNode (..),
    newXMLDocument,
    newXMLNode,
    newXMLElement,
    XMLBuilder,
    renderXML,

    -- * Serialisation
    ToXML (..),
    toXMLNode,
    toXMLAttr,
    toXMLList,

    -- * Deserialisation
  )
where

import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
-- import Data.Conduit
-- import Data.Conduit.Lazy (lazyConsume)
import qualified Data.Conduit.List as Conduit
import Data.DList (DList)
import qualified Data.DList as DList
-- import System.IO.Unsafe (unsafePerformIO)

-- import Text.XML.Unresolved (toEvents)
import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import qualified Data.XML.Types as XML.Types
import qualified Network.AWS.Data.Text as AWS.Text
import Numeric.Natural (Natural)
import qualified Text.XML as XML
import qualified Text.XML.Stream.Render as XML.Stream.Render

data XMLNode
  = XAttr XML.Name Text
  | XNode XML.Node
  deriving (Show, Eq)

newXMLDocument :: ToXML a => XML.Name -> a -> XML.Document
newXMLDocument name x =
  XML.Document
    { XML.documentPrologue =
        XML.Prologue
          { XML.prologueDoctype = Nothing,
            XML.prologueBefore = [],
            XML.prologueAfter = []
          },
      XML.documentEpilogue = [],
      XML.documentRoot = newXMLElement name (toXML x)
    }
{-# INLINEABLE newXMLDocument #-}

newXMLNode :: XML.Name -> XMLBuilder -> XMLNode
newXMLNode name =
  XNode . XML.NodeElement . newXMLElement name
{-# INLINEABLE newXMLNode #-}

newXMLElement :: XML.Name -> XMLBuilder -> XML.Element
newXMLElement name xs =
  XML.Element
    { XML.elementName = name,
      XML.elementAttributes = Map.fromList attrs,
      XML.elementNodes = nodes
    }
  where
    (attrs, nodes) =
      Either.partitionEithers $
        flip map (DList.toList xs) $ \case
          XAttr attr content ->
            Left (attr, content)
          XNode node ->
            Right node
{-# INLINEABLE newXMLElement #-}

type XMLBuilder = DList XMLNode

renderXML :: ToXML a => a -> ByteString
renderXML =
  ByteString.Lazy.toStrict
    . XML.renderLBS XML.Stream.Render.def
    . toXMLDocument
{-# INLINEABLE renderXML #-}

class ToXML a where
  toXML :: a -> XMLBuilder

  toXMLDocument :: a -> XML.Document
  toXMLDocument = newXMLDocument "Document" . toXML
  {-# INLINEABLE toXMLDocument #-}

instance ToXML XMLBuilder where
  toXML = id
  {-# INLINEABLE toXML #-}

instance ToXML Text where
  toXML = pure . XNode . XML.NodeContent
  {-# INLINEABLE toXML #-}

-- @toXMLNode "item" 1 == <item></item>
toXMLNode :: ToXML a => XML.Name -> a -> XMLBuilder
toXMLNode name =
  pure . newXMLNode name . toXML
{-# INLINEABLE toXMLNode #-}

toXMLAttr :: AWS.Text.ToText a => XML.Name -> a -> XMLBuilder
toXMLAttr name =
  pure . XAttr name . AWS.Text.toText
{-# INLINEABLE toXMLAttr #-}

-- @toXMLList "item" [1, 2, 3] == <item>1</item><item>2</item><item>3</item>@
toXMLList :: ToXML a => XML.Name -> [a] -> XMLBuilder
toXMLList name =
  foldMap (toXMLNode name)
{-# INLINEABLE toXMLList #-}

class FromXML a where
  parseXML :: [XML.Node] -> Either Text a

instance FromXML [XML.Node] where
  parseXML = Right
  {-# INLINEABLE parseXML #-}

(.@) :: FromXML a => XML.Name -> [XML.Node] -> Either Text a
name .@ nodes =
{-# INLINEABLE (.@) #-}

(.@?) :: FromXML a => XML.Name -> [XML.Node] -> Either Text (Maybe a)
name .@ nodes =
{-# INLINEABLE (.@?) #-}

-- | A combinator for chaining fmap and function application when a 'Just' value
-- is present. Eases code generation with nested/flattened values, similar to
-- aeson\'s @.:!@, but without the additional 'fmap' and 'traverse' juggling.
(<@>) ::
  (Monad f, Traversable f) =>
  -- | The optional parse result.
  f (Maybe a) ->
  -- | A continuation parser, used if the parse result is present.
  (a -> f b) ->
  f (Maybe b)
f <@> g = traverse (>>= g) (sequenceA f)
{-# INLINEABLE (<@>) #-}

-- parseXMLMap ::
--   (Eq k, Hashable k, FromXML k, FromXML v) =>
--   XML.Name ->
--   XML.Name ->
--   XML.Name ->
--   [XML.Node] ->
--   HashMap k v
-- parseXMLMap itemName

-- parseXMLList :: FromXML a => XML.Name ->
-- parseXMLNonEmpty :: FromXML a => XML.Name ->

-- infixl 7 .!@

-- (.!@) :: Functor f => f (Maybe a) -> a -> f a
-- f .!@ x = fromMaybe x <$> f

-- unlessEmpty :: Applicative f => ([a] -> f b) -> [a] -> f (Maybe b)
-- unlessEmpty f = \case
--   [] -> pure Nothing
--   xs -> Just <$> f xs

-- infixl 7 .@, .@?

-- (.@) :: FromXML a => [Node] -> Text -> Either String a
-- ns .@ n = findElement n ns >>= parseXML

-- (.@?) :: FromXML a => [Node] -> Text -> Either String (Maybe a)
-- ns .@? n =
--   case findElement n ns of
--     Left _ -> Right Nothing
--     Right xs -> parseXML xs

-- infixr 7 @=, @@=

-- (@=) :: ToXML a => Name -> a -> XML
-- n @= x =
--   case toXML x of
--     XNull -> XNull
--     xs -> XOne . NodeElement $ mkElement n xs

-- (@@=) :: ToText a => Name -> a -> XML
-- n @@= x = XAttr n (toText x)

-- decodeXML :: FromXML a => LazyByteString -> Either String a
-- decodeXML lbs =
--   bimap show documentRoot (parseLBS def lbs)
--     >>= parseXML . childrenOf

-- -- The following is taken from xml-conduit.Text.XML which uses
-- -- unsafePerformIO anyway, with the following caveat:
-- --   'not generally safe, but we know that runResourceT
-- --    will not deallocate any of the resources being used
-- --    by the process.'
-- encodeXML :: ToElement a => a -> LazyByteString
-- encodeXML x =
--   LBS.fromChunks . unsafePerformIO . lazyConsume $
--     Conduit.sourceList (toEvents doc)
--       .| Conduit.map rename
--       .| Stream.renderBytes def
--   where
--     doc =
--       toXMLDocument $
--         Document
--           { documentRoot = root,
--             documentEpilogue = [],
--             documentPrologue =
--               Prologue
--                 { prologueBefore = [],
--                   prologueDoctype = Nothing,
--                   prologueAfter = []
--                 }
--           }

--     rename = \case
--       EventBeginElement n xs -> EventBeginElement (f n) (map (first f) xs)
--       EventEndElement n -> EventEndElement (f n)
--       evt -> evt
--       where
--         f n
--           | isNothing (nameNamespace n) = n {nameNamespace = ns}
--           | otherwise = n

--     ns = nameNamespace (elementName root)
--     root = toElement x

-- class FromXML a where
--   parseXML :: [Node] -> Either String a

-- instance FromXML [Node] where
--   parseXML = pure

-- instance FromXML a => FromXML (Maybe a) where
--   parseXML [] = pure Nothing
--   parseXML ns = Just <$> parseXML ns

-- instance FromXML Text where
--   parseXML = fmap (fromMaybe mempty) . withContent "Text"

-- instance FromXML Char where
--   parseXML = parseXMLText "Char"

-- instance FromXML ByteString where
--   parseXML = parseXMLText "ByteString"

-- instance FromXML Int where
--   parseXML = parseXMLText "Int"

-- instance FromXML Integer where
--   parseXML = parseXMLText "Integer"

-- instance FromXML Natural where
--   parseXML = parseXMLText "Natural"

-- instance FromXML Double where
--   parseXML = parseXMLText "Double"

-- instance FromXML Bool where
--   parseXML = parseXMLText "Bool"

-- instance FromXML UTCTime where
--   parseXML = parseXMLText "UTCTime"

-- class ToElement a where
--   toElement :: a -> Element

-- instance ToElement Element where
--   toElement = id

-- -- | Convert to an 'Element', only if the resulting element contains @> 0@ nodes.
-- maybeElement :: ToElement a => a -> Maybe Element
-- maybeElement x =
--   case toElement x of
--     e@(Element _ _ ns)
--       | null ns -> Nothing
--       | otherwise -> Just e

-- typeXML' = DList XMLF

-- -- | Provides a way to make the operators for ToXML instance
-- -- declaration be consistent WRT to single nodes or lists of nodes.
-- data XML
--   = XNull
--   | XAttr Name Text
--   | XOne Node
--   | XMany (HashMap Name Text) (DList Node)
--   deriving (Show)

-- instance Semigroup XML where
--   XNull <> XNull = XNull
--   a <> XNull = a
--   XNull <> b = b
--   a <> b =
--     XMany
--       (listXMLAttributes a <> listXMLAttributes b)
--       (listXMLNodes a <> listXMLNodes b)

-- instance Monoid XML where
--   mempty = XNull
--   mappend = (<>)

-- listXMLNodes :: XML -> [Node]
-- listXMLNodes = \case
--   XNull -> []
--   XAttr {} -> []
--   XOne n -> [n]
--   XMany _ ns -> ns

-- listXMLAttributes :: XML -> [(Name, Text)]
-- listXMLAttributes = \case
--   XNull -> []
--   XAttr n t -> [(n, t)]
--   XOne {} -> []
--   XMany as _ -> as

-- class ToXML a where
--   toXML :: a -> XML

-- instance ToXML XML where
--   toXML = id

-- instance ToXML a => ToXML (Maybe a) where
--   toXML (Just x) = toXML x
--   toXML Nothing = XNull

-- instance ToXML Text where
--   toXML = toXMLText

-- instance ToXML ByteString where
--   toXML = toXMLText

-- instance ToXML Int where
--   toXML = toXMLText

-- instance ToXML Integer where
--   toXML = toXMLText

-- instance ToXML Natural where
--   toXML = toXMLText

-- instance ToXML Double where
--   toXML = toXMLText

-- instance ToXML Bool where
--   toXML = toXMLText

-- instance ToXML UTCTime where
--   toXML = toXMLText

-- parseXMLMap ::
--   (Eq k, Hashable k, FromText k, FromXML v) =>
--   Text ->
--   Text ->
--   Text ->
--   [Node] ->
--   Either String (HashMap k v)
-- parseXMLMap e k v =
--   fmap HashMap.fromList . traverse f . mapMaybe (childNodesOf e)
--   where
--     f ns =
--       (,)
--         <$> (ns .@ k >>= fromText)
--         <*> ns .@ v

-- parseXMLList ::
--   FromXML a =>
--   Text ->
--   [Node] ->
--   Either String [a]
-- parseXMLList n = traverse parseXML . mapMaybe (childNodesOf n)

-- parseXMLNonEmpty ::
--   FromXML a =>
--   Text ->
--   [Node] ->
--   Either String (NonEmpty a)
-- parseXMLNonEmpty name =
--   parseXMLList name
--     >=> maybe
--       (Left $ "Error parsing empty NonEmpty when expecting at least one element: " ++ show name)
--       pure
--       . NonEmpty.nonEmpty

-- parseXMLText :: FromText a => String -> [Node] -> Either String a
-- parseXMLText n =
--   withContent n
--     >=> maybe
--       (Left $ "empty node list, when expecting single node " ++ n)
--       fromText

-- toXMLList :: (IsList a, ToXML (Item a)) => Name -> a -> XML
-- toXMLList n = XMany [] . map (NodeElement . mkElement n) . toList

-- toXMLText :: ToText a => a -> XML
-- toXMLText = XOne . NodeContent . toText

-- mkElement :: ToXML a => Name -> a -> Element
-- mkElement n (toXML -> x) =
--   Element n (fromList (listXMLAttributes x)) (listXMLNodes x)

-- withContent :: String -> [Node] -> Either String (Maybe Text)
-- withContent k = \case
--   [] -> Right Nothing
--   [NodeContent x] -> Right (Just x)
--   _ -> Left $ "encountered many nodes, when expecting text: " ++ k

-- -- | Find a specific named NodeElement, at the current depth in the node tree.
-- --
-- -- Fails if absent.
-- findElement :: Text -> [Node] -> Either String [Node]
-- findElement n ns =
--   missingElement n ns
--     . listToMaybe
--     $ mapMaybe (childNodesOf n) ns

-- -- | Find the first specific named NodeElement, at any depth in the node tree.
-- --
-- -- Fails if absent.
-- firstElement :: Text -> [Node] -> Either String [Node]
-- firstElement n ns =
--   missingElement n ns
--     . listToMaybe
--     $ mapMaybe go ns
--   where
--     go x = case x of
--       NodeElement e
--         | Just n == localName x -> Just (childrenOf e)
--         | otherwise -> listToMaybe (mapMaybe go (elementNodes e))
--       _ -> Nothing

-- childNodesOf :: Text -> Node -> Maybe [Node]
-- childNodesOf n x = case x of
--   NodeElement e
--     | Just n == localName x ->
--       Just (childrenOf e)
--   _ -> Nothing

-- childrenOf :: Element -> [Node]
-- childrenOf e = elementNodes e <> map node (toList (elementAttributes e))
--   where
--     node (k, v) = NodeElement (Element (name k) mempty [NodeContent v])

--     name k =
--       Name
--         { nameLocalName = fromMaybe "" (namePrefix k) <> ":" <> nameLocalName k,
--           nameNamespace = mempty,
--           namePrefix = mempty
--         }

-- localName :: Node -> Maybe Text
-- localName = \case
--   NodeElement e -> Just (nameLocalName (elementName e))
--   _ -> Nothing

-- -- | An inefficient mechanism for retreiving the root
-- -- element name of an XML document.
-- rootElementName :: LazyByteString -> Maybe Text
-- rootElementName bs =
--   either
--     (const Nothing)
--     (Just . nameLocalName . elementName . documentRoot)
--     (parseLBS def bs)

-- missingElement :: Text -> [Node] -> Maybe a -> Either String a
-- missingElement n ns = maybe (Left err) Right
--   where
--     err =
--       "unable to find element "
--         ++ show n
--         ++ " in nodes "
--         ++ show (mapMaybe localName ns)
