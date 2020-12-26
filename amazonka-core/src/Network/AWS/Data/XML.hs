{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Network.AWS.Data.XML
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This XML parser is so relaxed it\'s horizontal.
module Network.AWS.Data.XML where

--   ( XMLNodes,
--     XMLEncoding (..),
--     XMLBuilder,
--     newXMLDocument,
--     newXMLNode,
--     newXMLElement,
--     newXMLText,

--     -- * Serialisation
--     ToXML (..),
--     renderXML,
--     toXMLNode,
--     toXMLAttribute,
--     toXMLList,

--     -- * Deserialisation
--     FromXML (..),
--     (.@),
--     (.@@),
--     (.@?),
--     (.@@?),
--     (.!@),
--     (<@>),
--     parseXMLMap,
--     parseXMLList,
--     parseXMLNonEmpty,
--   )
-- where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
-- import Data.Conduit
-- import Data.Conduit.Lazy (lazyConsume)
import qualified Data.Conduit.List as Conduit
import qualified Data.DList as DList
-- import Text.XML.Unresolved (toEvents)
import qualified Data.Either as Either
import qualified Data.Function as Function
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.XML.Types as XML.Types
import qualified Network.AWS.Data.Text as AWS.Text
import Network.AWS.Prelude
import qualified Text.XML as XML
import qualified Text.XML.Stream.Render as XML.Stream.Render

data XMLCursor
  = XMLContent Text
  | XMLElement (Map XML.Name Text) (Map XML.Name (NonEmpty XML.Element))
  deriving stock (Show)

getXMLElements :: XML.Name -> XMLCursor -> Maybe (NonEmpty XML.Element)
getXMLElements name = \case
  XMLContent {} -> Nothing
  XMLElement _as cs -> Map.lookup name cs

getXMLContents :: XMLCursor -> Maybe Text
getXMLContents = \case
  XMLContent x -> Just x
  XMLElement {} -> Nothing

newXMLCursor :: XML.Element -> XMLCursor
newXMLCursor element
  | Map.null children = XMLContent contents
  | otherwise = XMLElement (XML.elementAttributes element) children
  where
    children =
      Map.fromListWith
        (<>)
        [ (XML.elementName x, pure x)
          | XML.NodeElement x <- XML.elementNodes element
        ]

    contents =
      Text.Lazy.toStrict $
        Text.Lazy.fromChunks
          [ x
            | XML.NodeContent x <- XML.elementNodes element
          ]
{-# INLINEABLE newXMLCursor #-}

fromXMLCursor :: XML.Name -> XMLCursor -> XML.Element
fromXMLCursor name = \case
  XMLContent text ->
    XML.Element
      { XML.elementName = name,
        XML.elementAttributes = Map.empty,
        XML.elementNodes = [XML.NodeContent text]
      }
  --
  XMLElement attributes children ->
    XML.Element
      { XML.elementName = name,
        XML.elementAttributes = attributes,
        XML.elementNodes =
          DList.toList
            . foldMap (DList.fromList . map XML.NodeElement . NonEmpty.toList)
            $ (Map.elems children :: [NonEmpty XML.Element])
      }
{-# INLINEABLE fromXMLCursor #-}

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
      XML.documentRoot = fromXMLCursor name (toXML x)
    }
{-# INLINEABLE newXMLDocument #-}

class ToXML a where
  toXMLDocument :: a -> XML.Document
  toXMLDocument = newXMLDocument "Document" . toXML
  {-# INLINEABLE toXMLDocument #-}

  toXML :: a -> XMLCursor

  default toXML :: AWS.Text.ToText a => a -> XMLCursor
  toXML = XMLContent . AWS.Text.toText
  {-# INLINEABLE toXML #-}

instance ToXML XMLCursor where
  toXML = id
  {-# INLINEABLE toXML #-}

instance ToXML Char
instance ToXML Text 
instance ToXML Int
instance ToXML Integer
instance ToXML Natural
instance ToXML Scientific
instance ToXML Double
instance ToXML Bool
instance ToXML UTCTime
instance ToXML NominalDiffTime

-- renderXML :: ToXML a => a -> ByteString
-- renderXML =
--   ByteString.Lazy.toStrict
--     . XML.renderLBS XML.Stream.Render.def
--     . toXMLDocument
-- {-# INLINEABLE renderXML #-}

-- -- @toXMLNode "item" 1 == <item></item>
-- toXMLNode :: ToXML a => XML.Name -> a -> XMLBuilder
-- toXMLNode name =
--   pure . XNode . newXMLNode name . toXML
-- {-# INLINEABLE toXMLNode #-}

-- toXMLAttribute :: AWS.Text.ToText a => XML.Name -> a -> XMLBuilder
-- toXMLAttribute name =
--   pure . XAttr name . AWS.Text.toText
-- {-# INLINEABLE toXMLAttribute #-}

-- -- @toXMLList "item" [1, 2, 3] == <item>1</item><item>2</item><item>3</item>@
-- toXMLList :: ToXML a => XML.Name -> [a] -> XMLBuilder
-- toXMLList name =
--   foldMap (toXMLNode name)
-- {-# INLINEABLE toXMLList #-}

class FromXML a where
  parseXML :: XMLCursor -> Either Text a

--   -- default parseXML :: AWS.Text.FromText a => XMLNodes -> Either Text a
--   -- parseXML nodes =
--   -- {-# INLINEABLE parseXML #-}

-- instance FromXML XMLNodes where
--   parseXML = Right
--   {-# INLINEABLE parseXML #-}

-- -- instance FromXML Char
-- -- instance FromXML Text
-- -- instance FromXML Int
-- -- instance FromXML Integer
-- -- instance FromXML Natural
-- -- instance FromXML Scientific
-- -- instance FromXML Double
-- -- instance FromXML Bool
-- -- instance FromXML UTCTime
-- -- instance FromXML NominalDiffTime
-- -- unlessEmpty :: Applicative f => ([a] -> f b) -> [a] -> f (Maybe b)
-- -- unlessEmpty f = \case
-- --   [] -> pure Nothing
-- --   xs -> Just <$> f xs

-- -- infixl 7 .@, .@?

-- -- (.@) :: FromXML a => [Node] -> Text -> Either String a
-- -- ns .@ n = findElement n ns >>= parseXML

-- -- (.@?) :: FromXML a => [Node] -> Text -> Either String (Maybe a)
-- -- ns .@? n =
-- --   case findElement n ns of
-- --     Left _ -> Right Nothing
-- --     Right xs -> parseXML xs

-- -- infixr 7 @=, @@=

-- -- (@=) :: ToXML a => Name -> a -> XML
-- -- n @= x =
-- --   case toXML x of
-- --     XNull -> XNull
-- --     xs -> XOne . NodeElement $ mkElement n xs

-- -- (@@=) :: ToText a => Name -> a -> XML
-- -- n @@= x = XAttr n (toText x)

-- -- decodeXML :: FromXML a => LazyByteString -> Either String a
-- -- decodeXML lbs =
-- --   bimap show documentRoot (parseLBS def lbs)
-- --     >>= parseXML . childrenOf

-- -- -- The following is taken from xml-conduit.Text.XML which uses
-- -- -- unsafePerformIO anyway, with the following caveat:
-- -- --   'not generally safe, but we know that runResourceT
-- -- --    will not deallocate any of the resources being used
-- -- --    by the process.'
-- -- encodeXML :: ToElement a => a -> LazyByteString
-- -- encodeXML x =
-- --   LBS.fromChunks . unsafePerformIO . lazyConsume $
-- --     Conduit.sourceList (toEvents doc)
-- --       .| Conduit.map rename
-- --       .| Stream.renderBytes def
-- --   where
-- --     doc =
-- --       toXMLDocument $
-- --         Document
-- --           { documentRoot = root,
-- --             documentEpilogue = [],
-- --             documentPrologue =
-- --               Prologue
-- --                 { prologueBefore = [],
-- --                   prologueDoctype = Nothing,
-- --                   prologueAfter = []
-- --                 }
-- --           }

-- --     rename = \case
-- --       EventBeginElement n xs -> EventBeginElement (f n) (map (first f) xs)
-- --       EventEndElement n -> EventEndElement (f n)
-- --       evt -> evt
-- --       where
-- --         f n
-- --           | isNothing (nameNamespace n) = n {nameNamespace = ns}
-- --           | otherwise = n

-- --     ns = nameNamespace (elementName root)
-- --     root = toElement x

-- -- class FromXML a where
-- --   parseXML :: [Node] -> Either String a

-- -- instance FromXML [Node] where
-- --   parseXML = pure

-- -- instance FromXML a => FromXML (Maybe a) where
-- --   parseXML [] = pure Nothing
-- --   parseXML ns = Just <$> parseXML ns

-- -- instance FromXML Text where
-- --   parseXML = fmap (fromMaybe mempty) . withContent "Text"

-- -- instance FromXML Char where
-- --   parseXML = parseXMLText "Char"

-- -- instance FromXML ByteString where
-- --   parseXML = parseXMLText "ByteString"

-- -- instance FromXML Int where
-- --   parseXML = parseXMLText "Int"

-- -- instance FromXML Integer where
-- --   parseXML = parseXMLText "Integer"

-- -- instance FromXML Natural where
-- --   parseXML = parseXMLText "Natural"

-- -- instance FromXML Double where
-- --   parseXML = parseXMLText "Double"

-- -- instance FromXML Bool where
-- --   parseXML = parseXMLText "Bool"

-- -- instance FromXML UTCTime where
-- --   parseXML = parseXMLText "UTCTime"

-- -- class ToElement a where
-- --   toElement :: a -> Element

-- -- instance ToElement Element where
-- --   toElement = id

-- -- -- | Convert to an 'Element', only if the resulting element contains @> 0@ nodes.
-- -- maybeElement :: ToElement a => a -> Maybe Element
-- -- maybeElement x =
-- --   case toElement x of
-- --     e@(Element _ _ ns)
-- --       | null ns -> Nothing
-- --       | otherwise -> Just e

-- -- typeXML' = DList XMLF

-- -- -- | Provides a way to make the operators for ToXML instance
-- -- -- declaration be consistent WRT to single nodes or lists of nodes.
-- -- data XML
-- --   = XNull
-- --   | XAttr Name Text
-- --   | XOne Node
-- --   | XMany (HashMap Name Text) (DList Node)
-- --   deriving (Show)

-- -- instance Semigroup XML where
-- --   XNull <> XNull = XNull
-- --   a <> XNull = a
-- --   XNull <> b = b
-- --   a <> b =
-- --     XMany
-- --       (listXMLAttributes a <> listXMLAttributes b)
-- --       (listXMLNodes a <> listXMLNodes b)

-- -- instance Monoid XML where
-- --   mempty = XNull
-- --   mappend = (<>)

-- -- listXMLNodes :: XML -> [Node]
-- -- listXMLNodes = \case
-- --   XNull -> []
-- --   XAttr {} -> []
-- --   XOne n -> [n]
-- --   XMany _ ns -> ns

-- -- listXMLAttributes :: XML -> [(Name, Text)]
-- -- listXMLAttributes = \case
-- --   XNull -> []
-- --   XAttr n t -> [(n, t)]
-- --   XOne {} -> []
-- --   XMany as _ -> as

-- -- class ToXML a where
-- --   toXML :: a -> XML

-- -- instance ToXML XML where
-- --   toXML = id

-- -- instance ToXML a => ToXML (Maybe a) where
-- --   toXML (Just x) = toXML x
-- --   toXML Nothing = XNull

-- -- parseXMLMap ::
-- --   (Eq k, Hashable k, FromText k, FromXML v) =>
-- --   Text ->
-- --   Text ->
-- --   Text ->
-- --   [Node] ->
-- --   Either String (HashMap k v)
-- -- parseXMLMap e k v =
-- --   fmap HashMap.fromList . traverse f . mapMaybe (childNodesOf e)
-- --   where
-- --     f ns =
-- --       (,)
-- --         <$> (ns .@ k >>= fromText)
-- --         <*> ns .@ v

-- -- parseXMLList ::
-- --   FromXML a =>
-- --   Text ->
-- --   [Node] ->
-- --   Either String [a]
-- -- parseXMLList n = traverse parseXML . mapMaybe (childNodesOf n)

-- -- parseXMLNonEmpty ::
-- --   FromXML a =>
-- --   Text ->
-- --   [Node] ->
-- --   Either String (NonEmpty a)
-- -- parseXMLNonEmpty name =
-- --   parseXMLList name
-- --     >=> maybe
-- --       (Left $ "Error parsing empty NonEmpty when expecting at least one element: " ++ show name)
-- --       pure
-- --       . NonEmpty.nonEmpty

-- -- parseXMLText :: FromText a => String -> [Node] -> Either String a
-- -- parseXMLText n =
-- --   withContent n
-- --     >=> maybe
-- --       (Left $ "empty node list, when expecting single node " ++ n)
-- --       fromText

-- -- toXMLList :: (IsList a, ToXML (Item a)) => Name -> a -> XML
-- -- toXMLList n = XMany [] . map (NodeElement . mkElement n) . toList

-- -- toXMLText :: ToText a => a -> XML
-- -- toXMLText = XOne . NodeContent . toText

-- -- mkElement :: ToXML a => Name -> a -> Element
-- -- mkElement n (toXML -> x) =
-- --   Element n (fromList (listXMLAttributes x)) (listXMLNodes x)

-- -- withContent :: String -> [Node] -> Either String (Maybe Text)
-- -- withContent k = \case
-- --   [] -> Right Nothing
-- --   [NodeContent x] -> Right (Just x)
-- --   _ -> Left $ "encountered many nodes, when expecting text: " ++ k

-- -- -- | Find a specific named NodeElement, at the current depth in the node tree.
-- -- --
-- -- -- Fails if absent.
-- -- findElement :: Text -> [Node] -> Either String [Node]
-- -- findElement n ns =
-- --   missingElement n ns
-- --     . listToMaybe
-- --     $ mapMaybe (childNodesOf n) ns

-- -- -- | Find the first specific named NodeElement, at any depth in the node tree.
-- -- --
-- -- -- Fails if absent.
-- -- firstElement :: Text -> [Node] -> Either String [Node]
-- -- firstElement n ns =
-- --   missingElement n ns
-- --     . listToMaybe
-- --     $ mapMaybe go ns
-- --   where
-- --     go x = case x of
-- --       NodeElement e
-- --         | Just n == localName x -> Just (childrenOf e)
-- --         | otherwise -> listToMaybe (mapMaybe go (elementNodes e))
-- --       _ -> Nothing

(.@) :: FromXML a => XML.Name -> XMLCursor -> Either Text a
name .@ cursor =
  name .@? cursor
    >>= annotate ".@" cursor . \case
      Nothing -> Left ("no " <> XML.nameLocalName name <> " element found")
      Just ok -> Right ok
{-# INLINEABLE (.@) #-}

(.@?) :: FromXML a => XML.Name -> XMLCursor -> Either Text (Maybe a)
name .@? cursor =
  annotate ".@?" cursor $
    case getXMLElements name cursor of
      Just (x :| []) -> Just <$> parseXML (newXMLCursor x)
      Just _xs -> Left ("multiple " <> XML.nameLocalName name <> " elements found")
      Nothing -> Left "has no child elements"
{-# INLINEABLE (.@?) #-}

(.@@) :: AWS.Text.FromText a => XML.Name -> XMLCursor -> Either Text a
name .@@ cursor =
  name .@@? cursor
    >>= annotate ".@@" cursor . \case
      Nothing -> Left ("no " <> XML.nameLocalName name <> " attribute found")
      Just ok -> Right ok
{-# INLINEABLE (.@@) #-}

(.@@?) :: AWS.Text.FromText a => XML.Name -> XMLCursor -> Either Text (Maybe a)
name .@@? cursor =
  annotate ".@@?" cursor $
    traverse AWS.Text.parseText $
      case cursor of
        XMLContent {} -> Nothing
        XMLElement attributes _children -> Map.lookup name attributes
{-# INLINEABLE (.@@?) #-}

annotate :: Text -> XMLCursor -> Either Text a -> Either Text a
annotate function cursor =
  Bifunctor.first $ \text ->
    "("
      <> function
      <> ") cursor:"
      <> context cursor
      <> " "
      <> text
  where
    context = \case
      XMLContent {} -> "xml-content"
      XMLElement {} -> "xml-element"
{-# INLINEABLE annotate #-}

-- throwError
--   :: Cursor
--   -> Text
--   -> Either Text a
-- throwError cursor msg =
--   ""

-- -- A more generalised version of aeson\'s '.!='.
-- (.!@) :: Functor f => f (Maybe a) -> a -> f a
-- f .!@ x = Maybe.fromMaybe x <$> f
-- {-# INLINEABLE (.!@) #-}

-- -- | A combinator for chaining fmap and function application when using operators
-- -- inserted by the code generator.
-- (<@>) ::
--   (Monad f, Traversable f) =>
--   -- | The optional parse result.
--   f (Maybe a) ->
--   -- | A continuation parser, used if the parse result is present.
--   (a -> f b) ->
--   f (Maybe b)
-- f <@> g = traverse (>>= g) (sequenceA f)
-- {-# INLINEABLE (<@>) #-}

-- parseXMLMap ::
--   (Eq k, Hashable k, FromXML k, FromXML v) =>
--   XML.Name ->
--   XML.Name ->
--   XML.Name ->
--   XMLNodes ->
--   Either Text (HashMap k v)
-- parseXMLMap itemName keyName valName nodes =
--   pure mempty
-- {-# INLINEABLE parseXMLMap #-}
-- -- fmap fromList . traverse f . mapMaybe (childNodesOf e)
-- --   where
-- --     f ns = (,)
-- --        <$> (ns .@ k >>= fromText)
-- --        <*>  ns .@ v

-- parseXMLList ::
--   FromXML a =>
--   XML.Name ->
--   XMLNodes ->
--   Either Text [a]
-- parseXMLList name nodes = undefined
-- {-# INLINEABLE parseXMLList #-}

-- parseXMLNonEmpty ::
--   FromXML a =>
--   XML.Name ->
--   XMLNodes ->
--   Either Text (NonEmpty a)
-- parseXMLNonEmpty name nodes =
--   pure undefined
-- {-# INLINEABLE parseXMLNonEmpty #-}

withOnlyContent ::
  Text ->
  (Text -> Either Text a) ->
  [XML.Node] ->
  Either Text a
withOnlyContent ann parser nodes =
  withOnlyNode'
    ("(withOnlyContent) " <> ann)
    parser
    [x | XML.NodeContent x <- nodes]

withOnlyElement ::
  Text ->
  (XML.Element -> Either Text a) ->
  [XML.Node] ->
  Either Text a
withOnlyElement ann parser nodes =
  withOnlyNode'
    ("(withOnlyElement) " <> ann)
    parser
    [x | XML.NodeElement x <- nodes]

withOnlyNode' ::
  Text ->
  (a -> Either Text b) ->
  [a] ->
  Either Text b
withOnlyNode' ann parser = \case
  [x] -> parser x
  [] -> failure "empty node list"
  _xs -> failure "many nodes"
  where
    failure msg =
      Left $
        ann
          <> ", expected one node, got "
          <> msg

documentChildren :: XML.Document -> [XML.Node]
documentChildren = elementChildren . XML.documentRoot

nodeChildren :: XML.Node -> [XML.Node]
nodeChildren = \case
  XML.NodeElement x -> elementChildren x
  XML.NodeContent {} -> []
  XML.NodeComment {} -> []
  XML.NodeInstruction {} -> []

elementChildren :: XML.Element -> [XML.Node]
elementChildren = filterValidNodes . XML.elementNodes

filterValidNodes :: [XML.Node] -> [XML.Node]
filterValidNodes =
  List.filter $ \case
    XML.NodeElement {} -> True
    XML.NodeContent {} -> True
    XML.NodeComment {} -> False
    XML.NodeInstruction {} -> False

filterNamedElements :: XML.Name -> [XML.Node] -> [XML.Node]
filterNamedElements name =
  List.filter $ \case
    XML.NodeElement x -> matchLocalName name (XML.elementName x)
    _other -> False

matchLocalName :: XML.Name -> XML.Name -> Bool
matchLocalName = Function.on (==) XML.nameLocalName

-- -- | Find a
-- withAttributeNamed ::
--   Text ->
--   XML.Name ->
--   (XMLNodes -> Either Text a)
--   XMLNodes ->
--   Either Text a
-- withAttributeNamed ann name parser =
--   flip List.find $ \case
--     Xx ->

-- -- | Parse a singular text node.
-- withNodeContent ::
--   Text ->
--   (Text -> Either Text a) ->
--   XMLNodes ->
--   Either Text a
-- withNodeContent ann parser =
--   withNodeSingular ann $ \case
--     XML.NodeContent x ->
--       parser x
--     XML.NodeElement x ->
--       failure ("element " <> XML.nameLocalName (XML.elementName x))
--     XML.NodeInstruction{} ->
--       failure "instruction"
--     XML.NodeComment{} ->
--       failure "comment"
--   where
--    failure ctx =
--      Left $
--       "(withNodeContent) "
--         <> ann
--         <> " expect element, got "
--         <> ctx

-- -- -- | Lookup and parse a specific attribute.
-- -- withNodeAttribute ::
-- --   XML.Name ->

-- -- | Parse the children of a singular node.
-- withNodeChildren ::
--   Text ->
--   (XMLNodes -> Either Text a) ->
--   XMLNodes ->
--   Either Text a
-- withNodeChildren ann parser =
--   withNodeSingular ann $ \case
--     XML.NodeElement x ->
--       parser (elementChildren x)
--     XML.NodeContent{} ->
--       failure "node content"
--     XML.NodeInstruction{} ->
--       failure "instruction"
--     XML.NodeComment{} ->
--       failure "comment"
--   where
--    failure ctx =
--      Left $
--       "(withNodeChildren) "
--         <> ann
--         <> " expect element, got "
--         <> ctx

-- -- | Parse a single node in the current tree, ignoring instructions or comments.
-- withNodeSingular ::
--   Text ->
--   (XML.Node -> Either Text a) ->
--   XMLNodes ->
--   Either Text a
-- withNodeSingular ann parser nodes =
--    case filter isValid nodes of
--     [XNode node] -> parser node
--     [] -> failure "empty node list"
--     _xs -> failure "multiple nodes"
--   where
--     failure ctx =
--       Left $
--         "(withNodeSingular) "
--           <> ann
--           <> " expected single node, got "
--           <> ctx

--     isValid = \case
--       XAttr{} -> False
--       XNode node ->
--         case node of
--             XML.NodeElement{} -> True
--             XML.NodeContent{} -> True
--             XML.NodeInstruction{} -> False
--             XML.NodeComment{} -> False

-- withNodeContent :: Text -> XMLNodes -> Either Text (Maybe Text)
-- withNodeContent name = \case
--   [] -> Right Nothing
--   [XML.NodeContent text] -> Right (Just text)

-- documentChildren :: XML.Document -> XMLNodes
-- documentChildren = elementChildren . XML.documentRoot

-- nodeChildren :: XML.Node -> Maybe XMLNodes
-- nodeChildren = \case
--   XML.NodeElement x -> Just (elementChildren x)
--   _node -> Nothing

-- elementChildren :: XML.Element -> XMLNodes
-- elementChildren XML.Element {..} =
--   XAttr (Map.toList elementAttributes)
--     : flip Maybe.mapMaybe elementNodes
--     (\case
--       XML.NodeElement x -> XElem x
--       XML.

--       map XNode elementNodes

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
