{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Network.AWS.Data.Header
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Header where

import           Control.Error
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as BS
import qualified Data.CaseInsensitive        as CI
import           Data.Foldable               as Fold
import           Data.Function               (on)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.List                   (deleteBy)
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as Text
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text
import           Network.AWS.Data.Time
import           Network.HTTP.Types

hHost :: HeaderName
hHost = "Host"

hAMZToken :: HeaderName
hAMZToken = "X-AMZ-Security-Token"

hMetaPrefix :: HeaderName
hMetaPrefix = "X-AMZ-"

class ToHeaders a where
    toHeaders :: a -> [Header]
    toHeaders = const mempty

infixl 6 =:

(=:) :: ToHeader a => ByteString -> a -> [Header]
(=:) = toHeader

hdr :: HeaderName -> ByteString -> [Header] -> [Header]
hdr k v hs = let h = (k, v) in deleteBy ((==) `on` fst) h hs ++ [h]

hdrs :: [Header] -> [Header] -> [Header]
hdrs xs ys = Fold.foldr' (uncurry hdr) ys xs

class ToHeader a where
    toHeader :: ByteString -> a -> [Header]

instance ToHeader Text where
    toHeader k = toHeader k . Text.encodeUtf8

instance ToHeader ByteString where
    toHeader k = toHeader k . Just

instance ToByteString a => ToHeader (Maybe a) where
    toHeader k = maybe [] (\v -> [(CI.mk k, toBS v)])

instance (ToByteString k, ToByteString v) => ToHeader (HashMap k v) where
    toHeader p = map (\(k, v) -> (CI.mk (p <> toBS k), toBS v)) . Map.toList

infixl 6 ~:, ~:?

(~:) :: FromHeader a => ResponseHeaders -> HeaderName -> Either String a
(~:) hs k = note missing (find ((k ==) . fst) hs) >>= uncurry fromHeader
  where
    missing = BS.unpack $ "Unable to find header: " <> CI.original k

-- FIXME: Should ignore missing, but fail on parsing error if present
(~:?) :: FromHeader a => ResponseHeaders -> HeaderName -> Either String (Maybe a)
(~:?) hs k = Right $ hush (hs ~: k)

-- FIXME: Reuse FromText/ByteString instead of custom/disparate type class?

class FromHeader a where
    fromHeader :: HeaderName -> ByteString -> Either String a

    default fromHeader :: FromText a
                       => HeaderName
                       -> ByteString
                       -> Either String a
    fromHeader = const $ fromText . Text.decodeUtf8

instance FromHeader ByteString where
    fromHeader = const Right

instance FromHeader Text where
    fromHeader = const $ Right . Text.decodeUtf8

instance FromHeader RFC822
instance FromHeader ISO8601
instance FromHeader BasicTime
instance FromHeader AWSTime

-- (~:) :: FromHeaders a => [Header] -> HeaderName -> Either String a
-- (~:) = flip fromHeaders

-- class FromHeaders a where
--     fromHeaders :: HeaderName -> [Header] -> Either String a

--     default fromHeaders :: FromText a
--                         => HeaderName
--                         -> [Header]
--                         -> Either String a
--     fromHeaders k = fromText . Text.decodeUtf8 <=< lookupKey k

-- lookupKey :: HeaderName -> [Header] -> Either String ByteString
-- lookupKey k hs =

-- instance FromHeaders ByteString where
--     fromHeaders = lookupKey

-- instance FromHeaders Text where
--     fromHeaders k = return . Text.decodeUtf8 <=< lookupKey k

-- instance FromHeaders (HashMap Text Text) where
--     fromHeaders k = Right . Map.fromList . map decode . filter match
--       where
--         decode  = ((Text.decodeUtf8 . CI.original) *** Text.decodeUtf8)

--         match x = CI.foldedCase (fst x) `BS.isPrefixOf` CI.foldedCase k

-- instance FromText a => FromHeaders (Maybe a) where
--     fromHeaders k hs =
--         maybe (Right Nothing)
--               (fmap Just . fromText . Text.decodeUtf8)
--               (k `lookup` hs)

-- class FromHeader a where
--     fromHeader :: Header -> Either String a

--     default fromHeaders :: FromText a
--                         => HeaderName
--                         -> [Header]
--                         -> Either String a
--     fromHeaders k = fromText . Text.decodeUtf8 <=< lookupKey k
