-- |
-- Module      : Network.AWS.Data.XML
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This XML parser is so relaxed it\'s horizontal.
module Network.AWS.Data.XML
  ( -- * Serialisation
    encodeXML,
    XMLBuilder (..),
    XMLNodes (..),
    buildXMLDocument,
    buildXMLElement,
    ToXML (..),
    toXMLElement,
    toXMLAttribute,
    toXMLList,

    -- * Deserialisation
    decodeXML,
    XMLCursor (..),
    newXMLCursor,
    FromXML (..),
    parseXMLMap,
    parseXMLList,
    parseXMLNonEmpty,
    withXMLText,

    -- ** Operators
    (.@),
    (.@?),
    (.@@),
    (.@@?),
    (.!@),
    (<@>),
  )
where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.XML.Types as XML.Types
import qualified Network.AWS.Data.Text as AWS.Text
import Network.AWS.Prelude
import qualified Text.XML as XML
import qualified Text.XML.Stream.Render as XML.Stream.Render

encodeXML :: ToXML a => a -> ByteString
encodeXML =
  ByteString.Lazy.toStrict
    . XML.renderLBS XML.Stream.Render.def
    . toXMLDocument
{-# INLINEABLE encodeXML #-}

decodeXML :: FromXML a => ByteStringLazy -> Either Text a
decodeXML bytes = do
  document <-
    Bifunctor.first (mappend "decodeXML: " . Text.pack . show) $
      XML.parseLBS XML.def bytes

  parseXML (newXMLCursor (XML.documentRoot document))
{-# INLINEABLE decodeXML #-}

newtype XMLBuilder = XMLBuilder (XMLNodes -> XMLNodes)
  deriving (Semigroup, Monoid) via (Monoid.Endo XMLNodes)

instance Show XMLBuilder where
  showsPrec _ (XMLBuilder builder) =
    showString "XMLBuilder "
      . showsPrec 0 (builder XMLEnd)
  {-# INLINEABLE showsPrec #-}

data XMLNodes
  = XMLAttr XML.Name Text ~XMLNodes
  | XMLElem XML.Name ~XMLBuilder ~XMLNodes
  | XMLText Text ~XMLNodes
  | XMLEnd
  deriving stock (Show)

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

class ToXML a where
  toXMLDocument :: a -> XML.Document
  toXMLDocument = buildXMLDocument "Document" . toXML
  {-# INLINEABLE toXMLDocument #-}

  toXML :: a -> XMLBuilder
  default toXML :: AWS.Text.ToText a => a -> XMLBuilder
  toXML = XMLBuilder . XMLText . AWS.Text.toText
  {-# INLINEABLE toXML #-}

instance ToXML XMLBuilder where
  toXML = id
  {-# INLINEABLE toXML #-}

instance ToXML Char

instance ToXML Text

instance ToXML Bool

instance ToXML Int

instance ToXML Integer

instance ToXML Natural

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

data XMLCursor = XMLCursor
  { cursorName :: XML.Name,
    cursorAttributes :: Map XML.Name Text,
    cursorElements :: Map XML.Name (NonEmpty XML.Element),
    cursorContents :: Maybe Text
  }
  deriving stock (Show)

newXMLCursor :: XML.Element -> XMLCursor
newXMLCursor element =
  XMLCursor
    { cursorName = XML.elementName element,
      cursorAttributes = XML.elementAttributes element,
      cursorElements = children,
      cursorContents = contents
    }
  where
    children =
      Map.fromListWith (<>) $
        [(XML.elementName x, pure x) | XML.NodeElement x <- XML.elementNodes element]

    contents =
      case [x | XML.NodeContent x <- XML.elementNodes element] of
        [] -> Nothing
        xs -> Just (Text.Lazy.toStrict (Text.Lazy.fromChunks xs))

class FromXML a where
  parseXML :: XMLCursor -> Either Text a

instance FromXML XMLCursor where
  parseXML = Right
  {-# INLINE parseXML #-}

instance FromXML Char where
  parseXML = withXMLText "Char"
  {-# INLINEABLE parseXML #-}

instance FromXML Text where
  parseXML = withXMLText "Text"
  {-# INLINEABLE parseXML #-}

instance FromXML Bool where
  parseXML = withXMLText "Bool"
  {-# INLINEABLE parseXML #-}

instance FromXML Int where
  parseXML = withXMLText "Int"
  {-# INLINEABLE parseXML #-}

instance FromXML Integer where
  parseXML = withXMLText "Integer"
  {-# INLINEABLE parseXML #-}

instance FromXML Natural where
  parseXML = withXMLText "Natural"
  {-# INLINEABLE parseXML #-}

instance FromXML Scientific where
  parseXML = withXMLText "Scientific"
  {-# INLINEABLE parseXML #-}

instance FromXML Double where
  parseXML = withXMLText "Double"
  {-# INLINEABLE parseXML #-}

instance FromXML UTCTime where
  parseXML = withXMLText "UTCTime"
  {-# INLINEABLE parseXML #-}

instance FromXML NominalDiffTime where
  parseXML = withXMLText "NominalDiffTime"
  {-# INLINEABLE parseXML #-}

withXMLText :: AWS.Text.FromText a => Text -> XMLCursor -> Either Text a
withXMLText ann cursor =
  annotate ("withXMLText." <> ann) cursor $
    case cursorContents cursor of
      Nothing -> Left "no text node present"
      Just text -> AWS.Text.parseText text
{-# INLINEABLE withXMLText #-}

parseXMLMap ::
  (Eq k, Hashable k, FromXML k, FromXML v) =>
  XML.Name ->
  XML.Name ->
  XML.Name ->
  XMLCursor ->
  Either Text (HashMap k v)
parseXMLMap itemName keyName valName cursor =
  annotate "parseXMLMap" cursor $
    case Map.lookup itemName (cursorElements cursor) of
      Nothing -> pure HashMap.empty
      Just xs -> do
        let parseItem x =
              (,) <$> x .@ keyName <*> x .@ valName

        HashMap.fromList
          <$> traverse (parseItem . newXMLCursor) (NonEmpty.toList xs)
{-# INLINEABLE parseXMLMap #-}

parseXMLList ::
  FromXML a =>
  XML.Name ->
  XMLCursor ->
  Either Text [a]
parseXMLList itemName cursor =
  annotate "parseXMLList" cursor $
    case Map.lookup itemName (cursorElements cursor) of
      Nothing -> pure []
      Just xs -> traverse (parseXML . newXMLCursor) (NonEmpty.toList xs)
{-# INLINEABLE parseXMLList #-}

parseXMLNonEmpty ::
  FromXML a =>
  XML.Name ->
  XMLCursor ->
  Either Text (NonEmpty a)
parseXMLNonEmpty itemName cursor =
  annotate "parseXMLNonEmpty" cursor $
    case Map.lookup itemName (cursorElements cursor) of
      Nothing -> Left ("missing at least one " <> renderName itemName <> " element")
      Just xs -> traverse (parseXML . newXMLCursor) xs
{-# INLINEABLE parseXMLNonEmpty #-}

(.@) :: FromXML a => XMLCursor -> XML.Name -> Either Text a
cursor .@ name =
  cursor .@? name
    >>= annotate ".@" cursor . \case
      Nothing -> Left ("no " <> renderName name <> " element found")
      Just ok -> Right ok
{-# INLINEABLE (.@) #-}

(.@?) :: FromXML a => XMLCursor -> XML.Name -> Either Text (Maybe a)
cursor .@? name =
  annotate ".@?" cursor $
    case Map.lookup name (cursorElements cursor) of
      Just (x :| []) -> Just <$> parseXML (newXMLCursor x)
      Just _xs -> Left ("multiple " <> renderName name <> " elements found")
      Nothing -> pure Nothing
{-# INLINEABLE (.@?) #-}

(.@@) :: AWS.Text.FromText a => XMLCursor -> XML.Name -> Either Text a
cursor .@@ name =
  cursor .@@? name
    >>= annotate ".@@" cursor . \case
      Nothing -> Left ("no " <> renderName name <> " attribute found")
      Just ok -> Right ok
{-# INLINEABLE (.@@) #-}

(.@@?) :: AWS.Text.FromText a => XMLCursor -> XML.Name -> Either Text (Maybe a)
cursor .@@? name =
  annotate ".@@?" cursor $
    traverse AWS.Text.parseText $
      Map.lookup name (cursorAttributes cursor)
{-# INLINEABLE (.@@?) #-}

-- | A generalised version of aeson\'s '.!='.
(.!@) :: Functor f => f (Maybe a) -> a -> f a
f .!@ x = Maybe.fromMaybe x <$> f
{-# INLINEABLE (.!@) #-}

-- | A combinator for chaining fmap and function application when using
-- nested XML operators inserted by the code generator.
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
    ann <> ":" <> renderName (cursorName cursor) <> " " <> err

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
