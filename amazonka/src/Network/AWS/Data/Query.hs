{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE RankNTypes #-} -- Required for 'deep'

-- Module      : Network.AWS.Data.Query
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Query
    (
    -- * Types
      Query

    -- * Traversals
    , keysOf
    , valuesOf

    -- * Pairs
    , pair
    , (=?)

    -- * Serialisation
    , ToQuery   (..)
    , renderQuery
    , genericToQuery
    ) where

import           Control.Applicative
import           Control.Lens                 (Traversal', _1)
import           Control.Lens.Plated
import           Control.Lens.TH
import           Control.Monad
import qualified Data.Attoparsec.ByteString   as ABS
import           Data.Bifunctor
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as Build
import qualified Data.ByteString.Lazy.Builder as Build
import           Data.Char
import           Data.Data
import           Data.Data.Lens
import           Data.Default
import           Data.Either
import           Data.Foldable                (foldl')
import qualified Data.Foldable                as Fold
import           Data.HashMap.Strict          (HashMap)
import           Data.List                    (sort, sortBy, intersperse)
import           Data.List.NonEmpty           (NonEmpty(..))
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String
import           Data.Text                    (Text)
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Time
import qualified Network.HTTP.Types.URI       as URI

data Query
    = List  [Query]
    | Pair  ByteString Query
    | Value (Maybe ByteString)
      deriving (Eq, Show, Data, Typeable)

makePrisms ''Query

keysOf :: Traversal' Query ByteString
keysOf = deep (_Pair . _1)

valuesOf :: Traversal' Query (Maybe ByteString)
valuesOf = deep _Value

pair :: ToQuery a => ByteString -> a -> Query -> Query
pair k v = mappend (Pair k (toQuery v))

(=?) :: ToQuery a => ByteString -> a -> Query
(=?) k v = Pair k (toQuery v)

-- instance Ord Query where
--     compare (List  as) (List  bs) = as `compare` bs
--     compare (Pair a _) (Pair b _) = a  `compare` b
--     compare (Value  a) (Value  b) = a  `compare` b
--     compare (List   _) _          = GT
--     compare (Pair _ _) _          = GT
--     compare (Value  _) _          = LT

instance Monoid Query where
    mempty = List []

    mappend (List l) (List r) = List (l ++ r)
    mappend (List l) r        = List (r : l)
    mappend l        (List r) = List (l : r)
    mappend l        r        = List [l, r]

instance Plated Query where
    plate = uniplate

instance IsString Query where
    fromString = toQuery . BS.pack

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

-- keysOf :: Traversal' Query ByteString
-- valuesOf :: Traversal' Query (Maybe ByteString)

data QueryOptions = QueryOptions
    { queryCtorMod  :: String -> ByteString
    , queryFieldMod :: String -> ByteString
    }

instance Default QueryOptions where
    def = QueryOptions
        { queryCtorMod  = BS.pack
        , queryFieldMod = BS.pack
        }

-- toLoweredQuery :: (Generic a, GToQuery (Rep a))
--                => a
--                -> Query
-- toLoweredQuery = genericToQuery (lowered def)

genericToQuery :: (Generic a, GToQuery (Rep a))
               => QueryOptions
               -> a
               -> Query
genericToQuery o = gToQuery o . from

class ToQuery a where
    toQuery :: a -> Query
    toQuery = const mempty

instance ToQuery Query where
    toQuery = id

--     default toQuery :: (Generic a, GToQuery (Rep a))
--                     => a
--                     -> Query
--     toQuery = genericToQuery def

-- instance ToQuery ByteString where
--     toQuery = Value

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

-- FIXME: implement this shizzle
instance (ToQuery k, ToQuery v) => ToQuery (HashMap k v) where
    toQuery = undefined

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
        name = queryFieldMod o $ selName (undefined :: S1 c f p)
