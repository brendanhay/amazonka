-- |
-- Module      : Network.AWS.Data.Query
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Query
  ( QueryString,
    encodeQuery,
    encodeQueryBuilder,
    QueryBuilder (..),
    QueryPairs (..),
    buildQuery,

    -- * Serialisation
    ToQuery (..),
    toQueryPair,
    toQueryMap,
    toQueryList,
  )
where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Network.AWS.Data.Text as AWS.Text
import Network.AWS.Prelude
import qualified Network.HTTP.Types as HTTP.Types

type QueryString = [(ByteString, ByteString)]

encodeQuery :: Bool -> QueryString -> ByteString
encodeQuery allowEmpty =
  ByteString.Lazy.toStrict
    . Builder.toLazyByteString
    . mconcat
    . List.intersperse (Builder.shortByteString "&")
    . map encodePair
  where
    encodePair = \case
      ("", val) ->
        encodeURL val
      (key, "")
        | not allowEmpty ->
          encodeURL key
      (key, val) ->
        encodeURL key
          <> Builder.shortByteString "="
          <> encodeURL val
{-# INLINEABLE encodeQuery #-}

encodeQueryBuilder :: Bool -> QueryBuilder -> ByteString
encodeQueryBuilder allowEmpty = encodeQuery allowEmpty . buildQuery
{-# INLINEABLE encodeQueryBuilder #-}

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

buildQuery :: QueryBuilder -> QueryString
buildQuery =
  List.sort
    . map (Bifunctor.first (ByteString.Lazy.toStrict . Builder.toLazyByteString))
    . DList.toList
    . buildList Nothing
  where
    buildList mprefix (QueryBuilder builder) =
      buildPairs mprefix (builder QueryEnd)

    buildPairs mprefix = \case
      QueryKey key builder pairs ->
        buildList (Just (buildKey mprefix key)) builder
          <> buildPairs mprefix pairs
      --
      QueryVal val pairs ->
        DList.cons (maybe (mempty, val) (,val) mprefix) $
          buildPairs mprefix pairs
      --
      QueryEnd ->
        mempty

    buildKey mprefix key =
      case mprefix of
        Nothing -> Builder.byteString key
        Just prefix ->
          prefix
            <> Builder.shortByteString "."
            <> Builder.byteString key
{-# INLINEABLE buildQuery #-}

encodeURL :: ByteString -> ByteStringBuilder
encodeURL = HTTP.Types.urlEncodeBuilder True

class ToQuery a where
  toQuery :: a -> QueryBuilder
  default toQuery :: AWS.Text.ToText a => a -> QueryBuilder
  toQuery = QueryBuilder . QueryVal . AWS.Text.toUTF8
  {-# INLINEABLE toQuery #-}

instance ToQuery QueryBuilder where
  toQuery = id
  {-# INLINEABLE toQuery #-}

instance ToQuery ByteString where
  toQuery = QueryBuilder . QueryVal
  {-# INLINEABLE toQuery #-}

instance ToQuery Char

instance ToQuery Text

instance ToQuery Bool

instance ToQuery Int

instance ToQuery Integer

instance ToQuery Natural

instance ToQuery Double

instance ToQuery UTCTime

instance ToQuery NominalDiffTime

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
