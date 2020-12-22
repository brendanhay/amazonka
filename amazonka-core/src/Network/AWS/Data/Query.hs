{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Network.AWS.Data.Query
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.Query
  ( QueryBuilder,
    buildQuery,

    -- * Serialisation
    ToQuery (..),
    toQueryMap,
    toQueryList,
  )
where

import qualified Data.Bifunctor as Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Coerce (Coercible)
import qualified Data.Coerce as Coerce
import Data.DList (DList)
import qualified Data.DList as DList
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.AWS.Data.Text as AWS.Text
import Network.HTTP.Types (HeaderName)
import qualified Network.HTTP.Types as HTTP.Types

type QueryBuilder = DList (ByteString, ByteString)

buildQuery :: QueryBuilder -> ByteString
buildQuery =
  ByteString.Lazy.toStrict
    . Builder.toLazyByteString
    . mconcat
    . List.intersperse (Builder.shortByteString "&")
    . map (\(k, v) -> encodeURL k <> Builder.shortByteString "=" <> encodeURL v)
    . DList.toList
{-# INLINEABLE buildQuery #-}

encodeURL :: ByteString -> Builder
encodeURL = HTTP.Types.urlEncodeBuilder True
{-# INLINE encodeURL #-}

class ToQuery a where
  toQuery :: a -> QueryBuilder

instance ToQuery QueryBuilder where
  toQuery = id
  {-# INLINE toQuery #-}

toQueryMap ::
  (AWS.Text.ToText k, AWS.Text.ToText v) =>
  Builder ->
  Builder ->
  Builder ->
  HashMap k v ->
  QueryBuilder
toQueryMap itemPrefix keyPrefix valPrefix =
  mconcat . zipWith encodeItem [1 ..] . HashMap.toList
  where
    encodeItem index (k, v) =
      DList.cons (encodeIndex index keyPrefix, AWS.Text.toUTF8 k) $
        DList.cons (encodeIndex index valPrefix, AWS.Text.toUTF8 v) $
          DList.empty

    encodeIndex index prefix =
      ByteString.Lazy.toStrict $
        Builder.toLazyByteString $
          itemPrefix
            <> Builder.shortByteString "."
            <> Builder.intDec index
            <> Builder.shortByteString "."
            <> prefix
{-# INLINEABLE toQueryMap #-}

toQueryList ::
  AWS.Text.ToText a =>
  Builder ->
  [a] ->
  QueryBuilder
toQueryList itemPrefix =
  DList.fromList . zipWith encodeItem [1 ..]
  where
    encodeItem index x =
      (encodeIndex index, AWS.Text.toUTF8 x)

    encodeIndex index =
      ByteString.Lazy.toStrict $
        Builder.toLazyByteString $
          itemPrefix
            <> Builder.shortByteString "."
            <> Builder.intDec index
{-# INLINEABLE toQueryList #-}

-- data QueryString
--   = QList [QueryString]
--   | QPair ByteString QueryString
--   | QValue (Maybe ByteString)
--   deriving (Eq, Show)

-- instance Semigroup QueryString where
--   a <> b = case (a, b) of
--     (QList l, QList r) -> QList (l ++ r)
--     (QList l, r) -> QList (r : l)
--     (l, QList r) -> QList (l : r)
--     (l, r) -> QList [l, r]
--   {-# INLINE (<>) #-}

-- instance Monoid QueryString where
--   mempty = QList []
--   {-# INLINE mempty #-}
--   mappend = (<>)
--   {-# INLINE mappend #-}

-- instance IsString QueryString where
--   fromString = parseQueryString . fromString
--   {-# INLINE fromString #-}

-- parseQueryString :: ByteString -> QueryString
-- parseQueryString bs
--   | BS8.null bs = mempty
--   | otherwise =
--     QList (map breakPair . filter (not . BS8.null) $ BS8.split '&' bs)
--   where
--     breakPair x =
--       case BS8.break (== '=') x of
--         ("", "") -> mempty
--         ("", v) -> stripValue v
--         (k, v) -> QPair k (stripValue v)

--     stripValue x =
--       case x of
--         "" -> QValue Nothing
--         "=" -> QValue Nothing
--         _ -> QValue (Just (fromMaybe x (stripPrefix "=" x)))

-- stripPrefix :: ByteString -> ByteString -> Maybe ByteString
-- stripPrefix = BS8.stripPrefix

-- -- FIXME: use Builder
-- instance ToByteString QueryString where
--   toBS = LBS.toStrict . Build.toLazyByteString . cat . sort . enc Nothing
--     where
--       enc :: Maybe ByteString -> QueryString -> [ByteString]
--       enc p = \case
--         QList xs -> concatMap (enc p) xs
--         QPair (urlEncode True -> k) x
--           | Just n <- p -> enc (Just (n <> kdelim <> k)) x -- <prev>.key <recur>
--           | otherwise -> enc (Just k) x -- key <recur>
--         QValue (Just (urlEncode True -> v))
--           | Just n <- p -> [n <> vsep <> v] -- key=value
--           | otherwise -> [v <> vsep] -- value= -- note: required for signing.
--         _
--           | Just n <- p -> [n <> vsep] -- key=
--           -- note: this case required for request signing
--           | otherwise -> []

--       cat :: [ByteString] -> Builder
--       cat [] = mempty
--       cat [x] = Build.byteString x
--       cat (x : xs) = Build.byteString x <> ksep <> cat xs

--       kdelim = "."
--       ksep = "&"
--       vsep = "="

-- pair :: ToQuery a => ByteString -> a -> QueryString -> QueryString
-- pair k v = mappend (QPair k (toQuery v))

-- infixr 7 =:

-- (=:) :: ToQuery a => ByteString -> a -> QueryString
-- k =: v = QPair k (toQuery v)

-- formEncode :: [(ByteString, ByteString)] ->
-- class ToQuery a where
--   toQuery :: a -> QueryString

-- instance ToQuery QueryString where
--   toQuery = id

-- instance (ToByteString k, ToQuery v) => ToQuery (k, v) where
--   toQuery (k, v) = QPair (toBS k) (toQuery v)

-- instance ToQuery Char where
--   toQuery = toQuery . BS8.singleton

-- instance ToQuery ByteString where
--   toQuery = \case
--     "" -> QValue Nothing
--     bs -> QValue (Just bs)

-- instance ToQuery Text where
--   toQuery = toQuery . Text.encodeUtf8

-- instance ToQuery Int where
--   toQuery = toQuery . toBS

-- instance ToQuery Integer where
--   toQuery = toQuery . toBS

-- instance ToQuery Double where
--   toQuery = toQuery . toBS

-- instance ToQuery Natural where
--   toQuery = toQuery . toBS

-- instance ToQuery a => ToQuery (Maybe a) where
--   toQuery = \case
--     Just x -> toQuery x
--     Nothing -> mempty

-- instance ToQuery Bool where
--   toQuery = \case
--     True -> toQuery ("true" :: ByteString)
--     False -> toQuery ("false" :: ByteString)
