{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Network.AWS.Data.Headers
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Headers where

import           Data.Bifunctor
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS8
import qualified Data.CaseInsensitive        as CI
import           Data.Foldable               as Fold
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.Monoid
import qualified Data.Text.Encoding          as Text
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text
import           Network.HTTP.Types

infixl 7 .#, .#?

(.#) :: FromText a => ResponseHeaders -> HeaderName -> Either String a
hs .# k = hs .#? k >>= note
  where
    note Nothing  = Left (BS8.unpack $ "Unable to find header: " <> CI.original k)
    note (Just x) = Right x

(.#?) :: FromText a => ResponseHeaders -> HeaderName -> Either String (Maybe a)
hs .#? k =
    maybe (Right Nothing)
          (fmap Just . fromText . Text.decodeUtf8)
          (k `lookup` hs)

infixr 7 =#

parseMap :: FromText a
         => ByteString
         -> ResponseHeaders
         -> Either String (HashMap Text a)
parseMap p = fmap Map.fromList . traverse g . filter f
  where
    f = BS.isPrefixOf p . CI.foldedCase . fst

    g (k, v) = (Text.decodeUtf8 . BS.drop n $ CI.original k,) <$>
        fromText (Text.decodeUtf8 v)

    n = BS.length p

(=#) :: ToHeader a => HeaderName -> a -> [Header]
(=#) = toHeader

hdr :: HeaderName -> ByteString -> [Header] -> [Header]
hdr k v hs = (k, v) : filter ((/= k) . fst) hs

hdrs :: [Header] -> [Header] -> [Header]
hdrs xs ys = Fold.foldr' (uncurry hdr) ys xs

class ToHeaders a where
    toHeaders :: a -> [Header]
    toHeaders = const mempty

class ToHeader a where
    toHeader :: HeaderName -> a -> [Header]

    default toHeader :: ToText a => HeaderName -> a -> [Header]
    toHeader k = toHeader k . toText

instance ToHeader Text where
    toHeader k v = [(k, Text.encodeUtf8 v)]

instance ToHeader ByteString where
    toHeader k v = [(k, v)]

instance ToText a => ToHeader (Maybe a) where
    toHeader k = maybe [] (toHeader k . toText)

instance (ToByteString k, ToText v) => ToHeader (HashMap k v) where
    toHeader p = map (bimap k v) . Map.toList
      where
        k = mappend p . CI.mk . toBS
        v = Text.encodeUtf8 . toText

hHost :: HeaderName
hHost = "Host"

hAMZToken :: HeaderName
hAMZToken = "X-Amz-Security-Token"

hAMZTarget :: HeaderName
hAMZTarget = "X-Amz-Target"

hAMZAlgorithm :: HeaderName
hAMZAlgorithm = "X-Amz-Algorithm"

hAMZCredential :: HeaderName
hAMZCredential = "X-Amz-Credential"

hAMZExpires :: HeaderName
hAMZExpires = "X-Amz-Expires"

hAMZSignedHeaders :: HeaderName
hAMZSignedHeaders = "X-Amz-SignedHeaders"

hAMZContentSHA256 :: HeaderName
hAMZContentSHA256 = "X-Amz-Content-SHA256"

hAMZAuth :: HeaderName
hAMZAuth = "X-Amzn-Authorization"

hAMZDate :: HeaderName
hAMZDate = "X-Amz-Date"

hMetaPrefix :: HeaderName
hMetaPrefix = "X-Amz-"
