{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Network.AWS.Data.Internal.Query
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.Query
    ( ToQuery (..)
    , renderQuery

    , Query
    , valuesOf

    , (=?)
    , pair
    , toQueryList
    ) where

import           Control.Lens
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS
import           Data.Data
import           Data.Data.Lens
import           Data.List                            (sort)
import           Data.Monoid
import           Data.String
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as Text
import           GHC.Exts
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Text
import           Network.HTTP.Types.URI               (urlEncode)
import           Numeric.Natural

data Query
    = List  [Query]
    | Pair  ByteString Query
    | Value (Maybe ByteString)
      deriving (Eq, Show, Data, Typeable)

makePrisms ''Query

instance Monoid Query where
    mempty = List []

    mappend a b = case (a, b) of
        (List l, List r) -> List (l ++ r)
        (List l, r)      -> List (r : l)
        (l,      List r) -> List (l : r)
        (l,      r)      -> List [l, r]

instance Plated Query where
    plate = uniplate

instance ToByteString Query where
    toBS = renderQuery

instance ToBuilder Query

instance ToText Query where
    toText = Text.decodeUtf8 . toBS

instance IsString Query where
    fromString = toQuery . BS.pack

valuesOf :: Traversal' Query (Maybe ByteString)
valuesOf = deep _Value

pair :: ToQuery a => ByteString -> a -> Query -> Query
pair k v = mappend (Pair k (toQuery v))

(=?) :: ToQuery a => ByteString -> a -> Query
(=?) k v = Pair k (toQuery v)

toQueryList :: (IsList a, ToQuery (Item a)) => ByteString -> a -> Query
toQueryList n = mconcat . zipWith (\i v -> n =? (toBS i, v)) idx . toList
  where
    idx = [1..] :: [Int]

renderQuery :: Query -> ByteString
renderQuery = intercalate . sort . enc Nothing
  where
    enc k (List xs)   = concatMap (enc k) xs
    enc k (Pair (urlEncode True -> k') x)
        | Just n <- k = enc (Just $ n <> "." <> k') x
        | otherwise   = enc (Just k') x
    enc k (Value (Just (urlEncode True -> v)))
        | Just n <- k = [n <> vsep <> v]
        | otherwise   = [v]
    enc k _
        | Just n <- k = [n]
        | otherwise   = []

    intercalate []     = mempty
    intercalate [x]    = x
    intercalate (x:xs) = x <> ksep <> intercalate xs

    ksep = "&"
    vsep = "="

class ToQuery a where
    toQuery :: a -> Query

    default toQuery :: ToText a => a -> Query
    toQuery = toQuery . toText

instance ToQuery Query where
    toQuery = id

instance (ToByteString k, ToQuery v) => ToQuery (k, v) where
    toQuery (k, v) = Pair (toBS k) (toQuery v)

instance ToQuery Char where
    toQuery = toQuery . BS.singleton

instance ToQuery ByteString where
    toQuery "" = Value Nothing
    toQuery bs = Value (Just bs)

instance ToQuery Text    where toQuery = toQuery . Text.encodeUtf8
instance ToQuery Int     where toQuery = toQuery . toBS
instance ToQuery Integer where toQuery = toQuery . toBS
instance ToQuery Double  where toQuery = toQuery . toBS
instance ToQuery Natural where toQuery = toQuery . toBS

instance ToQuery a => ToQuery (Maybe a) where
    toQuery (Just x) = toQuery x
    toQuery Nothing  = mempty

instance ToQuery a => ToQuery [a] where
    toQuery = List . zipWith (\n v -> toBS n =? toQuery v) idx
      where
        idx = [1..] :: [Integer]

instance ToQuery Bool where
    toQuery True  = toQuery ("true"  :: ByteString)
    toQuery False = toQuery ("false" :: ByteString)
