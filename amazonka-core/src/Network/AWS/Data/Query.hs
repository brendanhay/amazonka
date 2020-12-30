-- |
-- Module      : Network.AWS.Data.Query
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Query
  ( QueryBuilder (..),
    QueryPairs (..),
    buildQuery,

    -- * Serialisation
    ToQuery (..),
    toQueryPair,
    toQueryMap,
    toQueryList,
  )
where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Monoid as Monoid
import qualified Network.AWS.Data.Text as AWS.Text
import Network.AWS.Prelude
import qualified Network.HTTP.Types as HTTP.Types

newtype QueryBuilder = QueryBuilder (QueryPairs -> QueryPairs)
  deriving (Semigroup, Monoid) via (Monoid.Endo QueryPairs)

instance Show QueryBuilder where
  showsPrec _ (QueryBuilder builder) =
    showString "QueryBuilder "
      . showsPrec 0 (builder QueryEnd)
  {-# INLINEABLE showsPrec #-}

data QueryPairs
  = QueryKey ByteString ~QueryBuilder ~QueryPairs
  | QueryVal ByteString ~QueryPairs
  | QueryEnd
  deriving stock (Show)

buildQuery :: QueryBuilder -> ByteString
buildQuery (QueryBuilder query) =
  ByteString.Lazy.toStrict
    . Builder.toLazyByteString
    $ go Nothing (query QueryEnd)
  where
    go mprefix = \case
      QueryKey key (QueryBuilder nested) pairs ->
        go (Just (encodeKey mprefix key)) (nested QueryEnd)
          <> encodeAmp mprefix pairs
      --
      QueryVal val pairs ->
        encodeVal mprefix val
          <> encodeAmp mprefix pairs
      --
      QueryEnd ->
        mempty

    encodeKey mprefix key =
      case mprefix of
        Nothing -> encodeURL key
        Just prefix -> prefix <> Builder.shortByteString "." <> encodeURL key

    encodeVal mprefix val =
      case mprefix of
        Nothing -> encodeURL val
        Just prefix -> prefix <> Builder.shortByteString "=" <> encodeURL val

    encodeAmp mprefix = \case
      QueryEnd -> mempty
      continue -> Builder.shortByteString "&" <> go mprefix continue
{-# INLINEABLE buildQuery #-}

encodeURL :: ByteString -> ByteStringBuilder
encodeURL = HTTP.Types.urlEncodeBuilder True
{-# INLINE encodeURL #-}

class ToQuery a where
  toQuery :: a -> QueryBuilder

instance ToQuery QueryBuilder where
  toQuery = id
  {-# INLINE toQuery #-}

instance ToQuery Char where
  toQuery = toQuery . AWS.Text.toText
  {-# INLINEABLE toQuery #-}

instance ToQuery Text where
  toQuery = QueryBuilder . QueryVal . AWS.Text.toUTF8
  {-# INLINEABLE toQuery #-}

instance ToQuery Bool where
  toQuery = toQuery . AWS.Text.toText
  {-# INLINEABLE toQuery #-}

instance ToQuery Int where
  toQuery = toQuery . AWS.Text.toText
  {-# INLINEABLE toQuery #-}

instance ToQuery Integer where
  toQuery = toQuery . AWS.Text.toText
  {-# INLINEABLE toQuery #-}

instance ToQuery Natural where
  toQuery = toQuery . AWS.Text.toText
  {-# INLINEABLE toQuery #-}

instance ToQuery Double where
  toQuery = toQuery . AWS.Text.toText
  {-# INLINEABLE toQuery #-}

instance ToQuery UTCTime where
  toQuery = toQuery . AWS.Text.toText
  {-# INLINEABLE toQuery #-}

instance ToQuery NominalDiffTime where
  toQuery = toQuery . AWS.Text.toText
  {-# INLINEABLE toQuery #-}

toQueryPair ::
  ToQuery a =>
  ByteString ->
  a ->
  QueryBuilder
toQueryPair key =
  QueryBuilder . QueryKey key . toQuery
{-# INLINEABLE toQueryPair #-}

toQueryMap ::
  (ToQuery k, ToQuery v) =>
  ByteString ->
  ByteString ->
  ByteString ->
  HashMap k v ->
  QueryBuilder
toQueryMap itemPrefix keyPrefix valPrefix =
  foldMap (uncurry encodeItem) . zip [1 ..] . HashMap.toList
  where
    encodeItem index (key, val) =
      toQueryPair itemPrefix $
        toQueryPair (encodeIndex index) $
          toQueryPair keyPrefix key
            <> toQueryPair valPrefix val

    encodeIndex =
      ByteString.Lazy.toStrict
        . Builder.toLazyByteString
        . Builder.intDec
{-# INLINEABLE toQueryMap #-}

toQueryList ::
  ToQuery a =>
  ByteString ->
  [a] ->
  QueryBuilder
toQueryList itemPrefix =
  foldMap (uncurry encodeItem) . zip [1 ..]
  where
    encodeItem index x =
      toQueryPair itemPrefix $
        toQueryPair (encodeIndex index) x

    encodeIndex =
      ByteString.Lazy.toStrict
        . Builder.toLazyByteString
        . Builder.intDec
{-# INLINEABLE toQueryList #-}
