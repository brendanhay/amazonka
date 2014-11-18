{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Data.Internal.XML
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.XML
    (
    -- * FromXML
      FromXML   (..)
    , decodeXML
    , parseXMLText
    , (.@)
    , (.@?)

    -- * ToXML
    , ToXML     (..)
    , ToXMLRoot (..)
    , encodeXML
    , toXMLText
    , element
    , nodes
    , (=@)
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString                      as BS
import           Data.Default.Class
import           Data.Foldable                        (foldr', foldrM)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.Hashable
import           Data.List                            (find)
import           Data.List.NonEmpty                   (NonEmpty(..))
import qualified Data.List.NonEmpty                   as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import           Data.Traversable                     (traverse)
import           GHC.Generics
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Text
import           Numeric.Natural
import           Text.XML

decodeXML :: LazyByteString -> Either String [Node]
decodeXML = either failure success . parseLBS def
  where
    failure = Left  . show
    success = Right . (:[]) . NodeElement . documentRoot

encodeXML :: ToXMLRoot a => a -> LazyByteString
encodeXML x = renderLBS def doc
  where
    doc = Document
        { documentPrologue = pro
        , documentRoot     = toXMLRoot x
        , documentEpilogue = []
        }

    pro = Prologue
        { prologueBefore  = []
        , prologueDoctype = Nothing
        , prologueAfter   = []
        }

parseXMLText :: FromText a => String -> [Node] -> Either String a
parseXMLText n = withContent n fromText
{-# INLINE parseXMLText #-}

toXMLText :: ToText a => a -> [Node]
toXMLText x = [NodeContent (toText x)]
{-# INLINE toXMLText #-}

(.@) :: FromXML a => [Node] -> Text -> Either String a
ns .@ n =
    case findElem n ns of
        Nothing -> Left $ "element " ++ show n ++ " not present"
        Just xs -> parseXML xs
{-# INLINE (.@) #-}

(.@?) :: FromXML a => [Node] -> Text -> Either String (Maybe a)
ns .@? n =
    case findElem n ns of
        Nothing -> Right Nothing
        Just xs -> parseXML xs
{-# INLINE (.@?) #-}

element :: Name -> [Node] -> Element
element n ns = Element n mempty ns
{-# INLINE element #-}

nodes :: Name -> [Node] -> [Node]
nodes n ns = [NodeElement (element n ns)]
{-# INLINE nodes #-}

(=@) :: ToXML a => Name -> a -> Node
n =@ x = NodeElement (Element n mempty (toXML x))
{-# INLINE (=@) #-}

findElem :: Text -> [Node] -> Maybe [Node]
findElem n = listToMaybe . mapMaybe f
  where
    f (NodeElement e)
        | nameLocalName (elementName e) == n
            = Just (elementNodes e)
    f _     = Nothing
{-# INLINE findElem #-}

withNode :: String -> (Node -> Either String a) -> [Node] -> Either String a
withNode n f = \case
    [x] -> f x
    []  -> Left $ "empty nodes when parsing " ++ n
    _   -> Left $ "encountered many nodes, expected one, when parsing " ++ n
{-# INLINE withNode #-}

withElem :: String -> (Element -> Either String a) -> [Node] -> Either String a
withElem n f = withNode n (join . fmap f . g)
  where
    g (NodeElement x) = Right x
    g _               = Left $ "expected node element, when parsing " ++ n
{-# INLINE withElem #-}

withContent :: String -> (Text -> Either String a) -> [Node] -> Either String a
withContent n f = withNode n (join . fmap f . g)
  where
    g (NodeContent x) = Right x
    g _               = Left $ "expected node content, when parsing" ++ n
{-# INLINE withContent #-}

class FromXML a where
    parseXML :: [Node] -> Either String a

instance FromXML a => FromXML (Maybe a) where
    parseXML [] = pure Nothing
    parseXML ns = Just <$> parseXML ns
    {-# INLINE parseXML #-}

instance FromXML a => FromXML [a] where
    parseXML = traverse (parseXML . (:[]))
    {-# INLINE parseXML #-}

instance FromXML a => FromXML (NonEmpty a) where
    parseXML = parseXML >=> \case
        []   -> Left  "Empty list, expected at least 1 element."
        x:xs -> Right (x :| xs)
    {-# INLINE parseXML #-}

instance FromXML Text    where parseXML = parseXMLText "Text"
instance FromXML Int     where parseXML = parseXMLText "Int"
instance FromXML Integer where parseXML = parseXMLText "Integer"
instance FromXML Natural where parseXML = parseXMLText "Natural"
instance FromXML Double  where parseXML = parseXMLText "Double"
instance FromXML Bool    where parseXML = parseXMLText "Bool"

class ToXMLRoot a where
    toXMLRoot :: a -> Element

class ToXML a where
    toXML :: a -> [Node]

instance ToXML a => ToXML (Maybe a) where
    toXML (Just x) = toXML x
    toXML Nothing  = []
    {-# INLINE toXML #-}

instance ToXML a => ToXML [a] where
    toXML = concatMap toXML
    {-# INLINE toXML #-}

instance ToXML Text    where toXML = toXMLText
instance ToXML Int     where toXML = toXMLText
instance ToXML Integer where toXML = toXMLText
instance ToXML Natural where toXML = toXMLText
instance ToXML Double  where toXML = toXMLText
instance ToXML Bool    where toXML = toXMLText

-- infixl 6 %|, %|?

-- (%|) :: FromXML a => Cursor -> Text -> Either String a
-- (%|) xml k = Left "not implemented"

-- (%|?) :: FromXML a => Cursor -> Text -> Either String (Maybe a)
-- (%|?) xml k = Right $ hush (xml %| k)
--   where
--     hush (Left  _) = Nothing
--     hush (Right x) = Just x

-- decodeXML :: forall a. FromXML a => LazyByteString -> Either String a
-- decodeXML = either failure success . parseLBS def
--   where
--     failure = Left . show
--     success = join . fmap (fromXML o) . parseXMLRoot o

--     o = parseXMLOptions :: Tagged a XMLOptions

-- encodeXML :: forall a. ToXML a => a -> LazyByteString
-- encodeXML = renderLBS def . toXMLRoot o . toXML o
--   where
--     o = toXMLOptions :: Tagged a XMLOptions

-- data XMLOptions = XMLOptions
--     { xmlInherit   :: !Bool
--     , xmlNamespace :: Maybe Text
--     , xmlListItem  :: Maybe Text
--     , xmlMapItem   :: Text
--     , xmlCtorMod   :: String -> Text
--     , xmlFieldMod  :: String -> Text
--     }

-- instance Default XMLOptions where
--     def = XMLOptions
--         { xmlInherit   = True
--         , xmlNamespace = Nothing
--         , xmlListItem  = Just "Item"
--         , xmlMapItem   = "entry"
--         , xmlCtorMod   = Text.pack
--         , xmlFieldMod  = Text.pack
--         }

-- fromNestedRoot :: NonEmpty Text
--                -> Tagged a XMLOptions
--                -> Document
--                -> Either String [Node]
-- fromNestedRoot rs o Document{..} = foldrM unwrap initial (NonEmpty.reverse rs)
--   where
--     initial = [NodeElement documentRoot]

--     unwrap n (NodeElement Element{..}:_)
--         | elementName == name = Right elementNodes
--         | otherwise           = Left $ concat
--             [ "Unexpected root element: "
--             , show elementName
--             , ", expecting: "
--             , show name
--             ]
--       where
--         name = Name n (xmlNamespace $ untag o) Nothing
--     unwrap n (_:xs) = unwrap n xs
--     unwrap n _      = Left $ "Unexpected non-element root, expecting: " ++ show n

-- fromRoot :: Text -> Tagged a XMLOptions -> Document -> Either String [Node]
-- fromRoot = fromNestedRoot . (:| [])

-- gFromRoot :: forall a. (Generic a, GXMLRoot (Rep a))
--                 => Tagged a XMLOptions
--                 -> Document
--                 -> Either String [Node]
-- gFromRoot o = fromRoot (gRootName (untag o) $ from (undefined :: a)) o

-- class FromXML a where
--     parseXMLOptions :: Tagged a XMLOptions
--     parseXMLRoot    :: Tagged a XMLOptions -> Document -> Either String [Node]
--     parseXML        :: Tagged a XMLOptions -> [Node]   -> Either String a

--     parseXMLOptions = Tagged def

--     default parseXMLRoot :: (Generic a, GXMLRoot (Rep a))
--                         => Tagged a XMLOptions
--                         -> Document
--                         -> Either String [Node]
--     parseXMLRoot = gFromRoot

--     default parseXML :: (Generic a, GFromXML (Rep a))
--                     => Tagged a XMLOptions
--                     -> [Node]
--                     -> Either String a
--     parseXML o = fmap to . gFromXML (untag o)

-- fromNodeContent :: FromText a => [Node] -> Either String a
-- fromNodeContent [NodeContent x] = fromText x
-- fromNodeContent _               = Left "Unexpected non-textual node contents."

-- instance FromXML () where
--     parseXML _ _ = Right ()

-- instance FromXML Bool where
--     parseXML = const fromNodeContent

-- instance FromXML Text where
--     parseXMLRoot = fromRoot "Text"
--     parseXML     = const fromNodeContent

-- instance FromXML BS.ByteString where
--     parseXMLRoot = fromRoot "ByteString"
--     parseXML o   = fmap Text.encodeUtf8 . parseXML (retag o)

-- instance FromXML Int where
--     parseXMLRoot = fromRoot "Int"
--     parseXML     = const fromNodeContent

-- instance FromXML Integer where
--     parseXMLRoot = fromRoot "Integer"
--     parseXML     = const fromNodeContent

-- instance FromXML Natural where
--     parseXMLRoot = fromRoot "Natural"
--     parseXML     = const fromNodeContent

-- instance FromXML Double where
--     parseXML = const fromNodeContent

-- instance FromXML a => FromXML [a] where
--     parseXML o = sequence . f item
--       where
--         f (Just x) = map (g x)
--         f Nothing  = map (fromXML (retag o) . (:[]))

--         g n (NodeElement (Element n' _ xs))
--             | n' == Name n ns Nothing = parseXML (retag o) xs
--             | otherwise               = Left "Unrecognised list element name."
--         g _ _                         = Left "Unable to parse list element."

--         item = xmlListItem  (untag o)
--         ns   = xmlNamespace (untag o)

-- instance (Eq k, Hashable k, FromText k, FromXML v) => FromXML (HashMap k v) where
--     parseXMLRoot = fromRoot "HashMap"
--     parseXML o   = fmap Map.fromList . mapM f
--       where
--         f (NodeElement (Element n _ xs))
--             | n == Name item ns Nothing
--             , [x, y] <- xs = (,) <$> g fromNodeContent x <*> g val y
--         f _                = Left "Unable to parse hashmap pair."

--         g h (NodeElement (Element _ _ xs)) = h xs
--         g _ _ = Left "Unable to parse hashmap pair."

--         val [NodeElement (Element _ _ xs)] = parseXML parseXMLOptions xs
--         val _ = Left "Unable to parse hashmap pair value."

--         item = xmlMapItem   (untag o)
--         ns   = xmlNamespace (untag o)

-- instance FromXML a => FromXML (NonEmpty a) where
--     parseXMLRoot = fromRoot "NonEmpty"
--     parseXML o   = join
--         . fmap (note . NonEmpty.nonEmpty)
--         . parseXML (retag o)
--       where
--         note Nothing  = Left "Unexpected empty list."
--         note (Just x) = Right x

-- -- FIXME: should fail if target doesn't exist? thereby requiring the element
-- -- but it can be empty?
-- instance FromXML a => FromXML (Maybe a) where
--     parseXML o ns =
--         either (const $ Right Nothing)
--                (Right . Just)
--                (fromXML (retag o) ns :: Either String a)

-- class GFromXML f where
--     gFromXML :: XMLOptions -> [Node] -> Either String (f a)

-- instance (GFromXML f, GFromXML g) => GFromXML (f :+: g) where
--     gFromXML o ns = (L1 <$> gFromXML o ns) <|> (R1 <$> gFromXML o ns)

-- instance (GFromXML f, GFromXML g) => GFromXML (f :*: g) where
--     gFromXML o ns = (:*:) <$> gFromXML o ns <*> gFromXML o ns

-- instance GFromXML U1 where
--     gFromXML _ _ = Right U1

-- instance forall a. FromXML a => GFromXML (K1 R a) where
--     gFromXML x = fmap K1 . parseXML (Tagged o)
--       where
--         o | xmlInherit $ untag y = x
--           | otherwise            = untag y

--         y = parseXMLOptions :: Tagged a XMLOptions

-- instance GFromXML f => GFromXML (D1 c f) where
--     gFromXML o = fmap M1 . gFromXML o

-- instance GFromXML f => GFromXML (C1 c f) where
--     gFromXML o = fmap M1 . gFromXML o

-- instance (Selector c, GFromXML f) => GFromXML (S1 c f) where
--     gFromXML o ns = findNodes ns >>= fmap M1 . gFromXML o
--       where
--         findNodes [] = Left $ "Failed to find: " ++ Text.unpack sel
--         findNodes (NodeElement e : es)
--             | elementName e == name = Right $ elementNodes e
--             | otherwise    = findNodes es
--         findNodes (_ : es) = findNodes es

--         name = Name sel (xmlNamespace o) Nothing
--         sel  = xmlFieldMod o $ selName (undefined :: S1 c f p)

-- toNestedRoot :: NonEmpty Text -> Tagged a XMLOptions -> [Node] -> Document
-- toNestedRoot rs o ns = Document (Prologue [] Nothing []) root []
--   where
--     root = foldr' (\k e -> wrap k [NodeElement e])
--                   (wrap (NonEmpty.last rs) ns)
--                   (NonEmpty.init rs)

--     wrap x = Element (name x) mempty
--     name x = Name x (xmlNamespace $ untag o) Nothing

-- toRoot :: Text -> Tagged a XMLOptions -> [Node] -> Document
-- toRoot = toNestedRoot . (NonEmpty.:| [])

-- genericToRoot :: forall a. (Generic a, GXMLRoot (Rep a))
--               => Tagged a XMLOptions
--               -> [Node]
--               -> Document
-- genericToRoot o = toRoot (gRootName (untag o) $ from (undefined :: a)) o

-- class ToXML a where
--     toXMLOptions :: Tagged a XMLOptions
--     toXMLRoot    :: Tagged a XMLOptions -> [Node] -> Document
--     toXML        :: Tagged a XMLOptions -> a -> [Node]

--     toXMLOptions = Tagged def

--     default toXMLRoot :: (Generic a, GXMLRoot (Rep a))
--                       => Tagged a XMLOptions
--                       -> [Node]
--                       -> Document
--     toXMLRoot = genericToRoot

--     default toXML :: (Generic a, GToXML (Rep a))
--                   => Tagged a XMLOptions
--                   -> a
--                   -> [Node]
--     toXML o = gToXML (untag o) . from

-- instance ToXML Text where
--     toXMLRoot = toRoot "Text"
--     toXML _   = (:[]) . NodeContent

-- instance ToXML BS.ByteString where
--     toXMLRoot = toRoot "ByteString"
--     toXML o   = toXML (retag o) . Text.decodeUtf8

-- instance ToXML Int where
--     toXMLRoot = toRoot "Int"
--     toXML _   = (:[]) . NodeContent . toText

-- instance ToXML Integer where
--     toXMLRoot = toRoot "Integer"
--     toXML _   = (:[]) . NodeContent . toText

-- instance ToXML Natural where
--     toXMLRoot = toRoot "Natural"
--     toXML _   = (:[]) . NodeContent . toText

-- instance ToXML Double where
--     toXML _ = (:[]) . NodeContent . toText

-- instance ToXML a => ToXML [a] where
--     toXML o = f (xmlListItem $ untag o)
--       where
--         f (Just x) = map (g (Name x (xmlNamespace $ untag o) Nothing) . toXML o')
--         f Nothing  = concatMap (toXML o')

--         g n = NodeElement . Element n mempty

--         o' = retag o

-- instance ToXML a => ToXML (NonEmpty a) where
--     toXMLRoot = toRoot "NonEmpty"
--     toXML o   = toXML (retag o) . NonEmpty.toList

-- instance ToXML a => ToXML (Maybe a) where
--     toXML o (Just x) = toXML (retag o) x
--     toXML _ Nothing  = []

-- instance ToXML Bool where
--     toXML _ True  = [NodeContent "true"]
--     toXML _ False = [NodeContent "false"]

-- instance ToXML () where
--     toXML _ () = []

-- class GToXML f where
--     gToXML :: XMLOptions -> f a -> [Node]

-- instance (GToXML f, GToXML g) => GToXML (f :+: g) where
--     gToXML o (L1 x) = gToXML o x
--     gToXML o (R1 y) = gToXML o y

-- instance (GToXML f, GToXML g) => GToXML (f :*: g) where
--     gToXML o (x :*: y) = gToXML o x ++ gToXML o y

-- instance GToXML U1 where
--     gToXML _ _ = []

-- instance ToXML a => GToXML (K1 R a) where
--     gToXML x f = toXML o g
--       where
--         o | xmlInherit $ untag y = Tagged x
--           | otherwise            = y

--         y = toXMLOptions :: Tagged a XMLOptions
--         g = unK1 f

-- instance GToXML f => GToXML (D1 c f) where
--     gToXML o = gToXML o . unM1

-- instance GToXML f => GToXML (C1 c f) where
--     gToXML o = gToXML o . unM1

-- instance (Selector c, GToXML f) => GToXML (S1 c f) where
--     gToXML o (m1 :: (S1 c f) a) = f . gToXML o $ unM1 m1
--       where
--         f = case selName m1 of
--             "" -> id
--             n  -> (:[])
--                 . NodeElement
--                 . Element (Name (xmlFieldMod o n) (xmlNamespace o) Nothing)
--                           mempty

-- class GXMLRoot f where
--     gRootName :: XMLOptions -> f a -> Text

-- instance (GXMLRoot f, GXMLRoot g) => GXMLRoot (f :+: g) where
--     gRootName o (_ :: (f :+: g) a) = gRootName o (undefined :: f a)

-- instance GXMLRoot f => GXMLRoot (D1 c f) where
--     gRootName o = gRootName o . unM1

-- instance Constructor c => GXMLRoot (C1 c f) where
--     gRootName o _ = xmlCtorMod o $ conName (undefined :: C1 c f p)

-- instance GXMLRoot a => GXMLRoot (M1 i c a) where
--     gRootName o = gRootName o . unM1
