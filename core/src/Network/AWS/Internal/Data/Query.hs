{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- Module      : Network.AWS.Internal.Data.Query
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Data.Query
    (
    -- * Types
      QueryOptions (..)
    , Query

    -- * Lenses
    , queryField
    , keysOf
    , valuesOf

    -- * Pairs
    , pair
    , (=?)

    -- * Serialisation
    , ToQuery      (..)
    , renderQuery

    , loweredQuery
    , genericQuery
    ) where

import           Control.Applicative
import           Control.Lens                         hiding (to, from)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS
import           Data.Char
import           Data.Data
import           Data.Data.Lens
import           Data.Default
import           Data.List                            (sort)
import           Data.List.NonEmpty                   (NonEmpty(..))
import qualified Data.List.NonEmpty                   as NonEmpty
import           Data.Monoid
import           Data.String
import           Data.Text                            (Text)
import           Data.Typeable
import           GHC.Generics
import           Network.AWS.Internal.Data.ByteString
import           Network.AWS.Internal.Data.Text
import           Network.AWS.Internal.Data.Time

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

instance ToText Query where
    toText = toText . renderQuery

instance IsString Query where
    fromString = toQuery . BS.pack

keysOf :: Traversal' Query ByteString
keysOf = deep (_Pair . _1)

valuesOf :: Traversal' Query (Maybe ByteString)
valuesOf = deep _Value

pair :: ToQuery a => ByteString -> a -> Query -> Query
pair k v = mappend (Pair k (toQuery v))

(=?) :: ToQuery a => ByteString -> a -> Query
(=?) k v = Pair k (toQuery v)

renderQuery :: Query -> ByteString
renderQuery = intercalate . sort . enc Nothing
  where
    enc k (List xs)   = concatMap (enc k) xs
    enc k (Pair k' x)
        | Just n <- k = enc (Just $ n <> "." <> k') x
        | otherwise   = enc (Just k') x
    enc k (Value (Just v))
        | Just n <- k = [n <> vsep <> v]
        | otherwise   = [v]
    enc k _
        | Just n <- k = [n]
        | otherwise   = []

    intercalate []       = mempty
    intercalate (x : []) = x
    intercalate (x : xs) = x <> ksep <> intercalate xs

    ksep = "&"
    vsep = "="

newtype QueryOptions = QueryOptions
    { _queryField :: String -> ByteString
    }

queryField :: Lens' QueryOptions (String -> ByteString)
queryField f (QueryOptions g) = QueryOptions <$> f g

instance Default QueryOptions where
    def = QueryOptions (BS.dropWhile isLower . BS.pack)

loweredQuery :: (Generic a, GToQuery (Rep a))
             => a
             -> Query
loweredQuery = genericQuery (def & queryField %~ (BS.map toLower .))

genericQuery :: (Generic a, GToQuery (Rep a))
             => QueryOptions
             -> a
             -> Query
genericQuery o = gToQuery o . from

class ToQuery a where
    toQuery :: a -> Query
    toQuery = const mempty

instance ToQuery Query where
    toQuery = id

instance (ToByteString k, ToByteString v) => ToQuery (k, v) where
    toQuery (k, v) = Pair (toBS k) . Value $ Just (toBS v)

instance (ToByteString k, ToByteString v) => ToQuery (k, Maybe v) where
    toQuery (k, v) = Pair (toBS k) . Value $ toBS <$> v

instance ToQuery () where
    toQuery () = mempty

instance ToQuery Char where
    toQuery = toQuery . BS.singleton

instance ToQuery ByteString where
    toQuery "" = Value Nothing
    toQuery bs = Value (Just bs)

instance ToQuery Text      where toQuery = toQuery . toBS
instance ToQuery Int       where toQuery = toQuery . toBS
instance ToQuery Integer   where toQuery = toQuery . toBS
instance ToQuery Double    where toQuery = toQuery . toBS
instance ToQuery RFC822    where toQuery = toQuery . toBS
instance ToQuery ISO8601   where toQuery = toQuery . toBS
instance ToQuery BasicTime where toQuery = toQuery . toBS
instance ToQuery AWSTime   where toQuery = toQuery . toBS

instance ToQuery a => ToQuery [a] where
    toQuery = List . zipWith (\n v -> Pair (toBS n) (toQuery v)) idx
      where
        idx = [1..] :: [Integer]

instance ToQuery a => ToQuery (NonEmpty a) where
    toQuery = toQuery . NonEmpty.toList

instance ToQuery a => ToQuery (Maybe a) where
    toQuery (Just x) = toQuery x
    toQuery Nothing  = mempty

instance ToQuery Bool where
    toQuery True  = toQuery ("true"  :: ByteString)
    toQuery False = toQuery ("false" :: ByteString)

class GToQuery f where
    gToQuery :: QueryOptions -> f a -> Query

instance (GToQuery f, GToQuery g) => GToQuery (f :+: g) where
    gToQuery o (L1 x) = gToQuery o x
    gToQuery o (R1 y) = gToQuery o y

instance (GToQuery f, GToQuery g) => GToQuery (f :*: g) where
    gToQuery o (x :*: y) = gToQuery o x <> gToQuery o y

instance GToQuery U1 where
    gToQuery _ _ = mempty

instance ToQuery a => GToQuery (K1 R a) where
    gToQuery _ = toQuery . unK1

instance GToQuery f => GToQuery (D1 c f) where
    gToQuery o = gToQuery o . unM1

instance GToQuery f => GToQuery (C1 c f) where
    gToQuery o = gToQuery o . unM1

instance (Selector c, GToQuery f) => GToQuery (S1 c f) where
    gToQuery o = Pair name . gToQuery o . unM1
      where
        name = _queryField o $ selName (undefined :: S1 c f p)
