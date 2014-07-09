{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

-- Module      : Network.AWS.Data.XML
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.XML where

import           Control.Applicative
import           Control.Error              (note)
import           Control.Monad
import qualified Data.Attoparsec.Text       as AText
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Default
import           Data.Foldable              (foldr', foldrM)
--import           Data.HashMap.Strict        (HashMap)
import           Data.List.NonEmpty         (NonEmpty(..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Monoid
import           Data.Tagged
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time
import           GHC.Generics
import           Text.XML
import           Text.XML.Cursor
import           Control.Error

infixl 6 %|, %|?

(%|) :: FromXML a => Cursor -> Text -> Either String a
(%|) xml k = Left "not implemented"

(%|?) :: FromXML a => Cursor -> Text -> Either String (Maybe a)
(%|?) xml k = Right $ hush (xml %| k)

decodeXML :: forall a. FromXML a => ByteString -> Either String a
decodeXML = either failure success . parseLBS def
  where
    failure = Left . show
    success = join . fmap (fromXML o) . fromXMLRoot o

    o = fromXMLOptions :: Tagged a XMLOptions

encodeXML :: forall a. ToXML a => a -> ByteString
encodeXML = renderLBS def . toXMLRoot o . toXML o
  where
    o = toXMLOptions :: Tagged a XMLOptions

data XMLOptions = XMLOptions
    { xmlInherit   :: !Bool
    , xmlNamespace :: Maybe Text
    , xmlListElem  :: Maybe Text
    , xmlCtorMod   :: String -> Text
    , xmlFieldMod  :: String -> Text
    }

instance Default XMLOptions where
    def = XMLOptions
        { xmlInherit   = True
        , xmlNamespace = Nothing
        , xmlListElem  = Just "Item"
        , xmlCtorMod   = Text.pack
        , xmlFieldMod  = Text.pack
        }

fromNestedRoot :: NonEmpty Text
               -> Tagged a XMLOptions
               -> Document
               -> Either String [Node]
fromNestedRoot rs o Document{..} = foldrM unwrap initial (NonEmpty.reverse rs)
  where
    initial = [NodeElement documentRoot]

    unwrap n (NodeElement Element{..}:_)
        | elementName == name = Right elementNodes
        | otherwise           = Left $ concat
            [ "Unexpected root element: "
            , show elementName
            , ", expecting: "
            , show name
            ]
      where
        name = Name n (xmlNamespace $ untag o) Nothing
    unwrap n (_:xs) = unwrap n xs
    unwrap n _      = Left $ "Unexpected non-element root, expecting: " ++ show n

fromRoot :: Text -> Tagged a XMLOptions -> Document -> Either String [Node]
fromRoot = fromNestedRoot . (:| [])

genericFromRoot :: forall a. (Generic a, GXMLRoot (Rep a))
                => Tagged a XMLOptions
                -> Document
                -> Either String [Node]
genericFromRoot o = fromRoot (gRootName (untag o) $ from (undefined :: a)) o

class FromXML a where
    fromXMLOptions :: Tagged a XMLOptions
    fromXMLRoot    :: Tagged a XMLOptions -> Document -> Either String [Node]
    fromXML        :: Tagged a XMLOptions -> [Node]   -> Either String a

    fromXMLOptions = Tagged def

    default fromXMLRoot :: (Generic a, GXMLRoot (Rep a))
                        => Tagged a XMLOptions
                        -> Document
                        -> Either String [Node]
    fromXMLRoot = genericFromRoot

    default fromXML :: (Generic a, GFromXML (Rep a))
                    => Tagged a XMLOptions
                    -> [Node]
                    -> Either String a
    fromXML o = fmap to . gFromXML (untag o)

-- instance FromXML Text where
--     fromXMLRoot                 = fromRoot "Text"
--     fromXML _ [NodeContent txt] = Right txt
--     fromXML _ _                 = Left "Unexpected node contents."

-- instance FromXML BS.ByteString where
--     fromXMLRoot = fromRoot "ByteString"
--     fromXML o   = fmap Text.encodeUtf8 . fromXML (retag o)

-- instance FromXML Int where
--     fromXMLRoot = fromRoot "Int"
--     fromXML     = nodeParser AText.decimal

-- instance FromXML Integer where
--     fromXMLRoot = fromRoot "Integer"
--     fromXML     = nodeParser AText.decimal

-- instance FromXML Double where
--     fromXML = nodeParser AText.rational

-- instance FromXML Float where
--     fromXML = nodeParser AText.rational

-- -- FIXME: is it possible to (nicely) implement HashMaps generically?
-- instance (FromXML k, FromXML v) => FromXML (HashMap k v) where
--     fromXMLRoot = fromRoot "HashMap"
--     fromXML     = undefined

-- instance FromXML a => FromXML [a] where
--     fromXML o = sequence . f (xmlListElem $ untag o)
--       where
--         f (Just x) = map (g x)
--         f Nothing  = map (fromXML (retag o) . (:[]))

--         g n (NodeElement (Element n' _ xs))
--             | n' == Name n (xmlNamespace $ untag o) Nothing = fromXML (retag o) xs
--             | otherwise = Left "Unrecognised list element name."
--         g _ _ = Left "Unable to parse list element."

-- instance FromXML a => FromXML (NonEmpty a) where
--     fromXMLRoot = fromRoot "NonEmpty"
--     fromXML o   = join
--         . fmap (note "Unexpected empty list." . NonEmpty.nonEmpty)
--         . fromXML (retag o)

-- -- FIXME: should fail if target doesn't exist
-- instance FromXML a => FromXML (Maybe a) where
--     fromXML o ns =
--         either (const $ Right Nothing)
--                (Right . Just)
--                (fromXML (retag o) ns :: Either String a)

-- instance FromXML Bool where
--     fromXML = nodeParser (p "true" True <|> p "false" False)
--       where
--         p s b = AText.string s *> return b <* AText.endOfInput

-- instance FromXML UTCTime where
--     fromXMLRoot = fromRoot "UTCTime"
--     fromXML o   = join . fmap (parseISO8601 . Text.unpack) . fromXML (retag o)

-- instance FromXML () where
--     fromXML _ _ = Right ()

-- nodeParser :: AText.Parser a -> Tagged a XMLOptions -> [Node] -> Either String a
-- nodeParser p o = join . fmap (AText.parseOnly p) . fromXML (retag o)

class GFromXML f where
    gFromXML :: XMLOptions -> [Node] -> Either String (f a)

instance (GFromXML f, GFromXML g) => GFromXML (f :+: g) where
    gFromXML o ns = (L1 <$> gFromXML o ns) <|> (R1 <$> gFromXML o ns)

instance (GFromXML f, GFromXML g) => GFromXML (f :*: g) where
    gFromXML o ns = (:*:) <$> gFromXML o ns <*> gFromXML o ns

instance GFromXML U1 where
    gFromXML _ _ = Right U1

instance forall a. FromXML a => GFromXML (K1 R a) where
    gFromXML x = fmap K1 . fromXML (Tagged o)
      where
        o | xmlInherit $ untag y = x
          | otherwise            = untag y

        y = fromXMLOptions :: Tagged a XMLOptions

instance GFromXML f => GFromXML (D1 c f) where
    gFromXML o = fmap M1 . gFromXML o

instance GFromXML f => GFromXML (C1 c f) where
    gFromXML o = fmap M1 . gFromXML o

instance (Selector c, GFromXML f) => GFromXML (S1 c f) where
    gFromXML o ns = findNodes ns >>= fmap M1 . gFromXML o
      where
        findNodes [] = Left $ "Failed to find: " ++ Text.unpack sel
        findNodes (NodeElement e : es)
            | elementName e == name = Right $ elementNodes e
            | otherwise    = findNodes es
        findNodes (_ : es) = findNodes es

        name = Name sel (xmlNamespace o) Nothing
        sel  = xmlFieldMod o $ selName (undefined :: S1 c f p)

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

class ToXML a where
    toXMLOptions :: Tagged a XMLOptions
    toXMLRoot    :: Tagged a XMLOptions -> [Node] -> Document
    toXML        :: Tagged a XMLOptions -> a -> [Node]

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
--     toXML _   = nodeFromIntegral

-- instance ToXML Integer where
--     toXMLRoot = toRoot "Integer"
--     toXML _   = nodeFromIntegral

-- instance ToXML Double where
--     toXML _ = nodeFromFloat

-- instance ToXML Float where
--     toXML _ = nodeFromFloat

-- -- FIXME: implement this shizzle
-- instance (ToXML k, ToXML v) => ToXML (HashMap k v) where
--     toXMLRoot = undefined
--     toXML o   = undefined

-- instance ToXML a => ToXML [a] where
--     toXML o = f (xmlListElem $ untag o)
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

-- instance ToXML UTCTime where
--     toXMLRoot = toRoot "UTCTime"
--     toXML _   = (:[]) . NodeContent . formatISO8601

-- instance ToXML () where
--     toXML _ () = []

-- nodeFromIntegral :: Integral a => a -> [Node]
-- nodeFromIntegral = (:[]) . NodeContent . integralToText

-- nodeFromFloat :: RealFloat a => a -> [Node]
-- nodeFromFloat = (:[]) . NodeContent . floatToText

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

class GXMLRoot f where
    gRootName :: XMLOptions -> f a -> Text

instance (GXMLRoot f, GXMLRoot g) => GXMLRoot (f :+: g) where
    gRootName o (_ :: (f :+: g) a) = gRootName o (undefined :: f a)

instance GXMLRoot f => GXMLRoot (D1 c f) where
    gRootName o = gRootName o . unM1

instance Constructor c => GXMLRoot (C1 c f) where
    gRootName o _ = xmlCtorMod o $ conName (undefined :: C1 c f p)

instance GXMLRoot a => GXMLRoot (M1 i c a) where
    gRootName o = gRootName o . unM1
