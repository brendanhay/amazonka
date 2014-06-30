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

Need to sort out date serialisation
and response parsing

import           Data.ByteString.Char8       (ByteString)
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
import           Network.HTTP.Types

hHost :: HeaderName
hHost = "Host"

hAMZToken :: HeaderName
hAMZToken = "X-AMZ-Security-Token"

hMetaPrefix :: HeaderName
hMetaPrefix = "X-AMZ-"

hdr :: HeaderName -> ByteString -> [Header] -> [Header]
hdr k v hs = let h = (k, v) in deleteBy ((==) `on` fst) h hs ++ [h]

hdrs :: [Header] -> [Header] -> [Header]
hdrs xs ys = Fold.foldr' (uncurry hdr) ys xs

class ToHeaders a where
    toHeaders :: a -> [Header]
    toHeaders = const mempty

(=:) :: ToHeader a => ByteString -> a -> [Header]
(=:) = toHeader

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

-- instance ToHeader ByteString where
--     toHeader k = (CI.mk k,) . Just

-- instance ToByteString a => ToHeader a where
--     toHeader k = (CI.mk k,) . Just . ByteString.encodeUtf8 . toBS

-- instance ToHeader a => ToHeader (Maybe a) where
--     toHeader k (Just x) = toHeader k x
--     toHeader k Nothing  = (CI.mk k, Nothing)

-- (=:) :: ToHeader a => ByteString -> a -> (HeaderName, Maybe ByteString)
-- (=:) = toHeader

-- hdr :: (Applicative f, FromByteString a)
--     => HeaderName
--     -> HashMap HeaderName ByteString
--     -> f (Maybe a)
-- hdr k = pure
--     . join
--     . fmap (hush . fromBS . ByteString.decodeUtf8)
--     . Map.lookup k

-- hdrs :: Applicative f
--      => ByteString
--      -> HashMap HeaderName ByteString
--      -> f (HashMap ByteString ByteString)
-- hdrs pre hs = pure $
--     Map.fromList [f (CI.original k, v) | (k, v) <- Map.toList hs, p k]
--   where
--     f = join (***) ByteString.decodeUtf8
--     p = BS.isPrefixOf pre . CI.foldedCase
