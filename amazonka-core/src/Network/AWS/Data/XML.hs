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
--     buildXMLDocument,
--     newXMLNode,
--     buildXMLElement,
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

import qualified Data.Text as Text
import qualified Control.Monad as Monad
import Control.Monad ((>=>))
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
-- import Data.Conduit
-- import Data.Conduit.Lazy (lazyConsume)
import qualified Data.Conduit.List as Conduit
-- import qualified Data.DList as DList
-- import Text.XML.Unresolved (toEvents)
import qualified Data.Either as Either
import qualified Data.Function as Function
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.XML.Types as XML.Types
import qualified Network.AWS.Data.Text as AWS.Text
import Network.AWS.Prelude
import qualified Text.XML as XML
import qualified Text.XML.Stream.Render as XML.Stream.Render

-- getXMLElements :: XML.Name -> XMLCursor -> Maybe (NonEmpty XML.Element)
-- getXMLElements name = \case
--   XMLContent {} -> Nothing
--   XMLElement _as cs -> Map.lookup name cs

-- getXMLContents :: XMLCursor -> Maybe Text
-- getXMLContents = \case
--   XMLContent x -> Just x
--   XMLElement {} -> Nothing

-- newXMLCursor :: XML.Element -> XMLCursor
-- newXMLCursor element
--   | Map.null children = XMLContent contents
--   | otherwise = XMLElement (XML.elementAttributes element) children
--   where
--     children =
--       Map.fromListWith
--         (<>)
--         [ (XML.elementName x, pure x)
--           | XML.NodeElement x <- XML.elementNodes element
--         ]

--     contents =
--       Text.Lazy.toStrict $
--         Text.Lazy.fromChunks
--           [ x
--             | XML.NodeContent x <- XML.elementNodes element
--           ]
-- {-# INLINEABLE newXMLCursor #-}

-- -- fromXMLCursor :: XML.Name -> XMLCursor -> XML.Element
-- -- fromXMLCursor name = \case
-- --   XMLContent text ->
-- --     XML.Element
-- --       { XML.elementName = name,
-- --         XML.elementAttributes = Map.empty,
-- --         XML.elementNodes = [XML.NodeContent text]
-- --       }
-- --   --
-- --   XMLElement attributes children ->
-- --     XML.Element
-- --       { XML.elementName = name,
-- --         XML.elementAttributes = attributes,
-- --         XML.elementNodes =
-- --           DList.toList
-- --             . foldMap (DList.fromList . map XML.NodeElement . NonEmpty.toList)
-- --             $ (Map.elems children :: [NonEmpty XML.Element])
-- --       }
-- -- {-# INLINEABLE fromXMLCursor #-}

newtype XMLBuilder = XMLBuilder (XMLNodes -> XMLNodes)
  deriving (Semigroup, Monoid) via (Monoid.Endo XMLNodes)

data XMLNodes
  = XMLAttr XML.Name Text ~XMLNodes
  | XMLElem XML.Name ~XMLBuilder ~XMLNodes
  | XMLText Text ~XMLNodes
  | XMLEnd

buildXMLElement :: XML.Name -> XMLBuilder -> XML.Element
buildXMLElement root (XMLBuilder xml) =
  go (xml XMLEnd) $
    XML.Element
      { XML.elementName = root,
        XML.elementAttributes = Map.empty,
        XML.elementNodes = []
      }
  where
    go continue x =
      case continue of
        XMLAttr key val nodes ->
          go nodes $
            x
              { XML.elementAttributes =
                  Map.insert key val (XML.elementAttributes x)
              }
        --
        XMLElem name inner nodes ->
          go nodes $
            x
              { XML.elementNodes =
                  XML.NodeElement (buildXMLElement name inner) : XML.elementNodes x
              }
        --
        XMLText text nodes ->
          go nodes $
            x
              { XML.elementNodes =
                  XML.NodeContent text : XML.elementNodes x
              }
        --
        XMLEnd ->
          x
{-# INLINEABLE buildXMLElement #-}

buildXMLDocument :: XML.Name -> XMLBuilder -> XML.Document
buildXMLDocument name builder =
  XML.Document
    { XML.documentPrologue =
        XML.Prologue
          { XML.prologueDoctype = Nothing,
            XML.prologueBefore = [],
            XML.prologueAfter = []
          },
      XML.documentEpilogue = [],
      XML.documentRoot = buildXMLElement name builder
    }
{-# INLINEABLE buildXMLDocument #-}

class ToXML a where
  toXMLDocument :: a -> XML.Document
  toXMLDocument = buildXMLDocument "Document" . toXML
  {-# INLINEABLE toXMLDocument #-}

  toXML :: a -> XMLBuilder

  default toXML :: AWS.Text.ToText a => a -> XMLBuilder
  toXML = XMLBuilder . XMLText . AWS.Text.toText
  {-# INLINEABLE toXML #-}
  {-# MINIMAL toXML #-}
  
instance ToXML XMLBuilder where
  toXML = id
  {-# INLINE toXML #-}

instance ToXML Char 
instance ToXML Text
instance ToXML Bool
instance ToXML Int
instance ToXML Integer
instance ToXML Natural
instance ToXML Scientific
instance ToXML Double
instance ToXML UTCTime
instance ToXML NominalDiffTime

toXMLAttribute :: AWS.Text.ToText a => XML.Name -> a -> XMLBuilder
toXMLAttribute name = XMLBuilder . XMLAttr name . AWS.Text.toText
{-# INLINEABLE toXMLAttribute #-}

toXMLElement :: ToXML a => XML.Name -> a -> XMLBuilder
toXMLElement name = XMLBuilder . XMLElem name . toXML
{-# INLINEABLE toXMLElement #-}

-- @toXMLList "item" [1, 2, 3] == <item>1</item><item>2</item><item>3</item>@
toXMLList :: ToXML a => XML.Name -> [a] -> XMLBuilder
toXMLList name = foldMap (toXMLElement name)
{-# INLINEABLE toXMLList #-}

renderXML :: ToXML a => a -> ByteString
renderXML =
  ByteString.Lazy.toStrict
    . XML.renderLBS XML.Stream.Render.def
    . toXMLDocument
{-# INLINEABLE renderXML #-}

data XMLCursor = XMLCursor
  { cursorName :: XML.Name
  , cursorAttributes :: Map XML.Name Text
  , cursorElements :: Map XML.Name (NonEmpty XML.Element)
  , cursorContents :: Maybe Text
  } deriving stock (Show)


-- getXMLElements :: XML.Name -> XMLCursor -> Maybe (NonEmpty XML.Element)
-- getXMLElements name = \case
--   XMLContent {} -> Nothing
--   XMLElement _as cs -> Map.lookup name cs

-- getXMLContents :: XMLCursor -> Maybe Text
-- getXMLContents = \case
--   XMLContent x -> Just x
--   XMLElement {} -> Nothing

newXMLCursor :: XML.Element -> XMLCursor
newXMLCursor element =
  XMLCursor
   { cursorName = XML.elementName element,
            cursorAttributes = XML.elementAttributes element,
            cursorElements = children,
            cursorContents =  contents
          }
 where
    children =
      Map.fromListWith (<>) $
        [(XML.elementName x, pure x) | XML.NodeElement x <- XML.elementNodes element]

    contents =
        case [ x | XML.NodeContent x <- XML.elementNodes element] of
          [] -> Nothing
          xs -> Just  (Text.Lazy.toStrict (Text.Lazy.fromChunks xs))
{-# INLINEABLE newXMLCursor #-}

class FromXML a where
  parseXML :: XMLCursor -> Either Text a

instance FromXML XMLCursor where
  parseXML = Right
  {-# INLINE parseXML #-}

-- instance FromXML Char where
--   parseXML = withXMLText "Char"
--   {-# INLINEABLE parseXML #-}
  
instance FromXML Text where
  parseXML = withXMLText "Text"
  {-# INLINEABLE parseXML #-}
  
-- instance FromXML Bool where
-- instance FromXML Int where
-- instance FromXML Integer where
-- instance FromXML Natural where
-- instance FromXML Scientific where
-- instance FromXML Double where
-- instance FromXML UTCTime where
-- instance FromXML NominalDiffTime where

parseXMLMap ::
  (Eq k, Hashable k, FromXML k, FromXML v) =>
  XML.Name ->
  XML.Name ->
  XML.Name ->
  XMLCursor ->
  Either Text (HashMap k v)
parseXMLMap itemName keyName valName cursor =
  case Map.lookup itemName (cursorElements cursor) of
    Nothing -> pure HashMap.empty
    Just (x :| xs) ->
      flip traverse (x : xs) $ \item ->
        (,) <$> (item .@ keyName >>= AWS.Text.parseText)
            <*> item .@ valName
    
-- fmap fromList . traverse f . mapMaybe (childCursorOf e)
--   where
--     f ns = (,)
--        <$> (ns .@ k >>= fromText)
--        <*>  ns .@ v

{-# INLINEABLE parseXMLMap #-}

parseXMLList ::
  FromXML a =>
  XML.Name ->
  XMLCursor ->
  Either Text [a]
parseXMLList name cursor = undefined
{-# INLINEABLE parseXMLList #-}

parseXMLNonEmpty ::
  FromXML a =>
  XML.Name ->
  XMLCursor ->
  Either Text (NonEmpty a)
parseXMLNonEmpty name cursor =
  pure undefined
{-# INLINEABLE parseXMLNonEmpty #-}

withXMLText :: AWS.Text.FromText a => Text -> XMLCursor -> Either Text a
withXMLText ann cursor =
 annotate ("withXMLText :: " <> ann) cursor $
  case cursorContents cursor of
    Nothing -> Left "no text node present"
    Just text -> AWS.Text.parseText text
{-# INLINEABLE withXMLText #-}

(.@) :: FromXML a => XML.Name -> XMLCursor -> Either Text a
name .@ cursor =
  name .@? cursor
    >>= annotate ".@" cursor . \case
      Nothing -> Left ("no " <> renderName name <> " element found")
      Just ok -> Right ok
{-# INLINEABLE (.@) #-}

(.@?) :: FromXML a => XML.Name -> XMLCursor -> Either Text (Maybe a)
name .@? cursor =
  annotate ".@?" cursor $
    case Map.lookup name (cursorElements cursor) of
      Just (x :| []) -> Just <$> parseXML (newXMLCursor x)
      Just _xs -> Left ("multiple " <> renderName name <> " elements found")
      Nothing -> pure Nothing
{-# INLINEABLE (.@?) #-}

(.@@) :: AWS.Text.FromText a => XML.Name -> XMLCursor -> Either Text a
name .@@ cursor =
  name .@@? cursor
    >>= annotate ".@@" cursor . \case
      Nothing -> Left ("no " <> renderName name <> " attribute found")
      Just ok -> Right ok
{-# INLINEABLE (.@@) #-}

(.@@?) :: AWS.Text.FromText a => XML.Name -> XMLCursor -> Either Text (Maybe a)
name .@@? cursor =
  annotate ".@@?" cursor $
    traverse AWS.Text.parseText $
      Map.lookup name (cursorAttributes cursor)
{-# INLINEABLE (.@@?) #-}

-- | A generalised version of aeson\'s '.!='.
(.!@) :: Functor f => f (Maybe a) -> a -> f a
f .!@ x = Maybe.fromMaybe x <$> f
{-# INLINEABLE (.!@) #-}

-- | A combinator for chaining fmap and function application when using
-- XML operators inserted by the code generator.
(<@>) ::
  (Monad f, Traversable f) =>
  -- | The optional parse result.
  f (Maybe a) ->
  -- | A continuation parser, used only if the parse result is present.
  (a -> f b) ->
  f (Maybe b)
f <@> g = traverse (>>= g) (sequenceA f)
{-# INLINEABLE (<@>) #-}

annotate :: Text -> XMLCursor -> Either Text a -> Either Text a
annotate ann cursor =
  Bifunctor.first $ \err ->
    "("
      <> ann
      <> "):"
      <> renderName (cursorName cursor)
      <> " - "
      <> err
{-# INLINEABLE annotate #-}

renderName :: XML.Name -> Text
renderName name =
  namespace <> prefix <> XML.nameLocalName name
 where
   namespace =
     Maybe.fromMaybe "" $ do
       text <- XML.nameNamespace name
       pure (Text.cons '{' (Text.snoc text '}'))

   prefix =
     Maybe.fromMaybe "" $ do
       text <- XML.namePrefix name
       pure (Text.snoc text ':')
   
-- -- -- (.@) :: FromXML a => [Node] -> Text -> Either String a
-- -- -- ns .@ n = findElement n ns >>= parseXML

-- -- -- (.@?) :: FromXML a => [Node] -> Text -> Either String (Maybe a)
-- -- -- ns .@? n =
-- -- --   case findElement n ns of
-- -- --     Left _ -> Right Nothing
-- -- --     Right xs -> parseXML xs

-- -- -- infixr 7 @=, @@=

-- -- -- (@=) :: ToXML a => Name -> a -> XML
-- -- -- n @= x =
-- -- --   case toXML x of
-- -- --     XNull -> XNull
-- -- --     xs -> XOne . NodeElement $ mkElement n xs

-- -- -- (@@=) :: ToText a => Name -> a -> XML
-- -- -- n @@= x = XAttr n (toText x)

-- -- -- decodeXML :: FromXML a => LazyByteString -> Either String a
-- -- -- decodeXML lbs =
-- -- --   bimap show documentRoot (parseLBS def lbs)
-- -- --     >>= parseXML . childrenOf

-- -- -- -- The following is taken from xml-conduit.Text.XML which uses
-- -- -- -- unsafePerformIO anyway, with the following caveat:
-- -- -- --   'not generally safe, but we know that runResourceT
-- -- -- --    will not deallocate any of the resources being used
-- -- -- --    by the process.'
-- -- -- encodeXML :: ToElement a => a -> LazyByteString
-- -- -- encodeXML x =
-- -- --   LBS.fromChunks . unsafePerformIO . lazyConsume $
-- -- --     Conduit.sourceList (toEvents doc)
-- -- --       .| Conduit.map rename
-- -- --       .| Stream.renderBytes def
-- -- --   where
-- -- --     doc =
-- -- --       toXMLDocument $
-- -- --         Document
-- -- --           { documentRoot = root,
-- -- --             documentEpilogue = [],
-- -- --             documentPrologue =
-- -- --               Prologue
-- -- --                 { prologueBefore = [],
-- -- --                   prologueDoctype = Nothing,
-- -- --                   prologueAfter = []
-- -- --                 }
-- -- --           }

-- -- --     rename = \case
-- -- --       EventBeginElement n xs -> EventBeginElement (f n) (map (first f) xs)
-- -- --       EventEndElement n -> EventEndElement (f n)
-- -- --       evt -> evt
-- -- --       where
-- -- --         f n
-- -- --           | isNothing (nameNamespace n) = n {nameNamespace = ns}
-- -- --           | otherwise = n

-- -- --     ns = nameNamespace (elementName root)
-- -- --     root = toElement x

-- -- -- class FromXML a where
-- -- --   parseXML :: [Node] -> Either String a

-- -- -- instance FromXML [Node] where
-- -- --   parseXML = pure

-- -- -- instance FromXML a => FromXML (Maybe a) where
-- -- --   parseXML [] = pure Nothing
-- -- --   parseXML ns = Just <$> parseXML ns

-- -- -- instance FromXML Text where
-- -- --   parseXML = fmap (fromMaybe mempty) . withContent "Text"

-- -- -- instance FromXML Char where
-- -- --   parseXML = parseXMLText "Char"

-- -- -- instance FromXML ByteString where
-- -- --   parseXML = parseXMLText "ByteString"

-- -- -- instance FromXML Int where
-- -- --   parseXML = parseXMLText "Int"

-- -- -- instance FromXML Integer where
-- -- --   parseXML = parseXMLText "Integer"

-- -- -- instance FromXML Natural where
-- -- --   parseXML = parseXMLText "Natural"

-- -- -- instance FromXML Double where
-- -- --   parseXML = parseXMLText "Double"

-- -- -- instance FromXML Bool where
-- -- --   parseXML = parseXMLText "Bool"

-- -- -- instance FromXML UTCTime where
-- -- --   parseXML = parseXMLText "UTCTime"

-- -- -- class ToElement a where
-- -- --   toElement :: a -> Element

-- -- -- instance ToElement Element where
-- -- --   toElement = id

-- -- -- -- | Convert to an 'Element', only if the resulting element contains @> 0@ nodes.
-- -- -- maybeElement :: ToElement a => a -> Maybe Element
-- -- -- maybeElement x =
-- -- --   case toElement x of
-- -- --     e@(Element _ _ ns)
-- -- --       | null ns -> Nothing
-- -- --       | otherwise -> Just e

-- -- -- typeXML' = DList XMLF

-- -- -- -- | Provides a way to make the operators for ToXML instance
-- -- -- -- declaration be consistent WRT to single nodes or lists of nodes.
-- -- -- data XML
-- -- --   = XNull
-- -- --   | XAttr Name Text
-- -- --   | XOne Node
-- -- --   | XMany (HashMap Name Text) (DList Node)
-- -- --   deriving (Show)

-- -- -- instance Semigroup XML where
-- -- --   XNull <> XNull = XNull
-- -- --   a <> XNull = a
-- -- --   XNull <> b = b
-- -- --   a <> b =
-- -- --     XMany
-- -- --       (listXMLAttributes a <> listXMLAttributes b)
-- -- --       (listXMLNodes a <> listXMLNodes b)

-- -- -- instance Monoid XML where
-- -- --   mempty = XNull
-- -- --   mappend = (<>)

-- -- -- listXMLNodes :: XML -> [Node]
-- -- -- listXMLNodes = \case
-- -- --   XNull -> []
-- -- --   XAttr {} -> []
-- -- --   XOne n -> [n]
-- -- --   XMany _ ns -> ns

-- -- -- listXMLAttributes :: XML -> [(Name, Text)]
-- -- -- listXMLAttributes = \case
-- -- --   XNull -> []
-- -- --   XAttr n t -> [(n, t)]
-- -- --   XOne {} -> []
-- -- --   XMany as _ -> as

-- -- -- class ToXML a where
-- -- --   toXML :: a -> XML

-- -- -- instance ToXML XML where
-- -- --   toXML = id

-- -- -- instance ToXML a => ToXML (Maybe a) where
-- -- --   toXML (Just x) = toXML x
-- -- --   toXML Nothing = XNull

-- -- -- parseXMLMap ::
-- -- --   (Eq k, Hashable k, FromText k, FromXML v) =>
-- -- --   Text ->
-- -- --   Text ->
-- -- --   Text ->
-- -- --   [Node] ->
-- -- --   Either String (HashMap k v)
-- -- -- parseXMLMap e k v =
-- -- --   fmap HashMap.fromList . traverse f . mapMaybe (childNodesOf e)
-- -- --   where
-- -- --     f ns =
-- -- --       (,)
-- -- --         <$> (ns .@ k >>= fromText)
-- -- --         <*> ns .@ v

-- -- -- parseXMLList ::
-- -- --   FromXML a =>
-- -- --   Text ->
-- -- --   [Node] ->
-- -- --   Either String [a]
-- -- -- parseXMLList n = traverse parseXML . mapMaybe (childNodesOf n)

-- -- -- parseXMLNonEmpty ::
-- -- --   FromXML a =>
-- -- --   Text ->
-- -- --   [Node] ->
-- -- --   Either String (NonEmpty a)
-- -- -- parseXMLNonEmpty name =
-- -- --   parseXMLList name
-- -- --     >=> maybe
-- -- --       (Left $ "Error parsing empty NonEmpty when expecting at least one element: " ++ show name)
-- -- --       pure
-- -- --       . NonEmpty.nonEmpty

-- -- -- parseXMLText :: FromText a => String -> [Node] -> Either String a
-- -- -- parseXMLText n =
-- -- --   withContent n
-- -- --     >=> maybe
-- -- --       (Left $ "empty node list, when expecting single node " ++ n)
-- -- --       fromText

-- -- -- toXMLList :: (IsList a, ToXML (Item a)) => Name -> a -> XML
-- -- -- toXMLList n = XMany [] . map (NodeElement . mkElement n) . toList

-- -- -- toXMLText :: ToText a => a -> XML
-- -- -- toXMLText = XOne . NodeContent . toText

-- -- -- mkElement :: ToXML a => Name -> a -> Element
-- -- -- mkElement n (toXML -> x) =
-- -- --   Element n (fromList (listXMLAttributes x)) (listXMLNodes x)

-- -- -- withContent :: String -> [Node] -> Either String (Maybe Text)
-- -- -- withContent k = \case
-- -- --   [] -> Right Nothing
-- -- --   [NodeContent x] -> Right (Just x)
-- -- --   _ -> Left $ "encountered many nodes, when expecting text: " ++ k

-- -- -- -- | Find a specific named NodeElement, at the current depth in the node tree.
-- -- -- --
-- -- -- -- Fails if absent.
-- -- -- findElement :: Text -> [Node] -> Either String [Node]
-- -- -- findElement n ns =
-- -- --   missingElement n ns
-- -- --     . listToMaybe
-- -- --     $ mapMaybe (childNodesOf n) ns

-- -- -- -- | Find the first specific named NodeElement, at any depth in the node tree.
-- -- -- --
-- -- -- -- Fails if absent.
-- -- -- firstElement :: Text -> [Node] -> Either String [Node]
-- -- -- firstElement n ns =
-- -- --   missingElement n ns
-- -- --     . listToMaybe
-- -- --     $ mapMaybe go ns
-- -- --   where
-- -- --     go x = case x of
-- -- --       NodeElement e
-- -- --         | Just n == localName x -> Just (childrenOf e)
-- -- --         | otherwise -> listToMaybe (mapMaybe go (elementNodes e))
-- -- --       _ -> Nothing


-- -- throwError
-- --   :: Cursor
-- --   -> Text
-- --   -> Either Text a
-- -- throwError cursor msg =
-- --   ""

-- -- withOnlyContent ::
-- --   Text ->
-- --   (Text -> Either Text a) ->
-- --   [XML.Node] ->
-- --   Either Text a
-- -- withOnlyContent ann parser nodes =
-- --   withOnlyNode'
-- --     ("(withOnlyContent) " <> ann)
-- --     parser
-- --     [x | XML.NodeContent x <- nodes]

-- -- withOnlyElement ::
-- --   Text ->
-- --   (XML.Element -> Either Text a) ->
-- --   [XML.Node] ->
-- --   Either Text a
-- -- withOnlyElement ann parser nodes =
-- --   withOnlyNode'
-- --     ("(withOnlyElement) " <> ann)
-- --     parser
-- --     [x | XML.NodeElement x <- nodes]

-- -- withOnlyNode' ::
-- --   Text ->
-- --   (a -> Either Text b) ->
-- --   [a] ->
-- --   Either Text b
-- -- withOnlyNode' ann parser = \case
-- --   [x] -> parser x
-- --   [] -> failure "empty node list"
-- --   _xs -> failure "many nodes"
-- --   where
-- --     failure msg =
-- --       Left $
-- --         ann
-- --           <> ", expected one node, got "
-- --           <> msg

-- -- documentChildren :: XML.Document -> [XML.Node]
-- -- documentChildren = elementChildren . XML.documentRoot

-- -- nodeChildren :: XML.Node -> [XML.Node]
-- -- nodeChildren = \case
-- --   XML.NodeElement x -> elementChildren x
-- --   XML.NodeContent {} -> []
-- --   XML.NodeComment {} -> []
-- --   XML.NodeInstruction {} -> []

-- -- elementChildren :: XML.Element -> [XML.Node]
-- -- elementChildren = filterValidNodes . XML.elementNodes

-- -- filterValidNodes :: [XML.Node] -> [XML.Node]
-- -- filterValidNodes =
-- --   List.filter $ \case
-- --     XML.NodeElement {} -> True
-- --     XML.NodeContent {} -> True
-- --     XML.NodeComment {} -> False
-- --     XML.NodeInstruction {} -> False

-- -- filterNamedElements :: XML.Name -> [XML.Node] -> [XML.Node]
-- -- filterNamedElements name =
-- --   List.filter $ \case
-- --     XML.NodeElement x -> matchLocalName name (XML.elementName x)
-- --     _other -> False

-- -- matchLocalName :: XML.Name -> XML.Name -> Bool
-- -- matchLocalName = Function.on (==) XML.nameLocalName
