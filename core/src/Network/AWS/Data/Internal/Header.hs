{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Data.Internal.Header
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Header where

import           Control.Applicative
import           Data.ByteString.Char8                (ByteString)
import qualified Data.ByteString.Char8                as BS
import qualified Data.CaseInsensitive                 as CI
import           Data.CaseInsensitive                 (CI)
import           Data.Foldable                        as Fold
import           Data.Function                        (on)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.List                            (deleteBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import           Data.Traversable                     (sequenceA, traverse)
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Text
import           Network.HTTP.Types

hHost :: HeaderName
hHost = "Host"

hAMZToken :: HeaderName
hAMZToken = "X-Amz-Security-Token"

hAMZTarget :: HeaderName
hAMZTarget = "X-Amz-Target"

hAMZAuth :: HeaderName
hAMZAuth = "X-Amzn-Authorization"

hAMZDate :: HeaderName
hAMZDate = "X-Amz-Date"

hMetaPrefix :: HeaderName
hMetaPrefix = "X-Amz-"

(~:) :: FromText a => ResponseHeaders -> HeaderName -> Either String a
hs ~: k = hs ~:? k >>= note
  where
    note Nothing  = Left (BS.unpack $ "Unable to find header: " <> CI.original k)
    note (Just x) = Right x

(~:?) :: FromText a => ResponseHeaders -> HeaderName -> Either String (Maybe a)
hs ~:? k =
    maybe (Right Nothing)
          (fmap Just . fromText . Text.decodeUtf8)
          (k `lookup` hs)

(~::) :: FromText v
      => ResponseHeaders
      -> CI Text
      -> Either String (HashMap (CI Text) v)
hs ~:: (CI.foldedCase -> p) = Map.fromList . catMaybes <$> traverse f hs
  where
    f (CI.map Text.decodeUtf8 -> k, Text.decodeUtf8 -> v) =
        case Text.stripPrefix p (CI.foldedCase k) of
            Nothing -> Right Nothing
            Just x  -> Just <$> g x v

    g k v = (CI.mk k,) <$> fromText v

class ToHeaders a where
    toHeaders :: a -> [Header]
    toHeaders = const mempty

(=:) :: ToHeader a => HeaderName -> a -> [Header]
(=:) = toHeader

hdr :: HeaderName -> ByteString -> [Header] -> [Header]
hdr k v hs = let h = (k, v) in deleteBy ((==) `on` fst) h hs ++ [h]

hdrs :: [Header] -> [Header] -> [Header]
hdrs xs ys = Fold.foldr' (uncurry hdr) ys xs

class ToHeader a where
    toHeader :: HeaderName -> a -> [Header]

instance ToHeader Text where
    toHeader k = toHeader k . Text.encodeUtf8

instance ToHeader ByteString where
    toHeader k = toHeader k . Just

instance ToByteString a => ToHeader (Maybe a) where
    toHeader k = maybe [] (\v -> [(k, toBS v)])

instance (ToByteString k, ToByteString v) => ToHeader (HashMap k v) where
    toHeader p = map (\(k, v) -> (p <> CI.mk (toBS k), toBS v)) . Map.toList
