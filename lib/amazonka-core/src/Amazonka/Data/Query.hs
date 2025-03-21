-- |
-- Module      : Amazonka.Data.Query
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.Query where

import Amazonka.Data.ByteString (ToByteString (..))
import Amazonka.Data.Text (ToText (..))
import Amazonka.Prelude
import Control.Category ((>>>))
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types (QueryItem)
import qualified Network.HTTP.Types.URI as URI

-- | Structured representation of a query string.
--
-- Some operations (e.g., [sqs:CreateQueue](https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_CreateQueue.html))
-- use query parameters to pass structured data like lists and maps,
-- which is why this type is more complicatated than the
-- @[(ByteString, Maybe ByteString)]@ from @http-types@ that you may
-- have expected here.
data QueryString
  = QList [QueryString]
  | QPair ByteString QueryString
  | QValue (Maybe ByteString)
  deriving stock (Eq, Show)

instance Semigroup QueryString where
  a <> b = case (a, b) of
    (QList l, QList r) -> QList (l ++ r)
    (QList l, r) -> QList (r : l)
    (l, QList r) -> QList (l : r)
    (l, r) -> QList [l, r]

instance Monoid QueryString where
  mempty = QList []
  mappend = (<>)

instance IsString QueryString where
  fromString = parseQueryString . fromString
  {-# INLINE fromString #-}

parseQueryString :: ByteString -> QueryString
parseQueryString bs
  | BS8.null bs = mempty
  | otherwise =
      QList (map breakPair . filter (not . BS8.null) $ BS8.split '&' bs)
  where
    breakPair x =
      case BS8.break (== '=') x of
        ("", "") -> mempty
        ("", v) -> stripValue v
        (k, v) -> QPair (URI.urlDecode True k) (stripValue v)

    stripValue :: ByteString -> QueryString
    stripValue x =
      case x of
        "" -> QValue Nothing
        "=" -> QValue Nothing
        _ -> QValue (Just (URI.urlDecode True $ BS8.drop 1 x))

instance ToByteString QueryString where
  toBS =
    toQueryItems Nothing
      >>> List.sortOn fst
      >>> catQueryItems
      >>> Build.toLazyByteString
      >>> LBS.toStrict
    where
      -- Carry the "key so far" as an accumulating parameter.
      toQueryItems :: Maybe ByteString -> QueryString -> [QueryItem]
      toQueryItems key = \case
        QList xs ->
          concatMap (toQueryItems key) xs
        QValue (Just (URI.urlEncode True -> v)) ->
          case key of
            Just k -> [(k, Just v)]
            Nothing -> [(v, Nothing)] -- Looks odd, required for signing.
        QValue Nothing ->
          case key of
            Just k -> [(k, Nothing)]
            Nothing -> []
        QPair (URI.urlEncode True -> k) queryString ->
          toQueryItems extendedKey queryString
          where
            extendedKey :: Maybe ByteString
            extendedKey = Just $ maybe k (<> "." <> k) key

      catQueryItems :: [QueryItem] -> ByteStringBuilder
      catQueryItems = \case
        [] -> mempty
        [item] -> encode item
        item : items -> encode item <> "&" <> catQueryItems items
        where
          encode :: QueryItem -> ByteStringBuilder
          encode (k, mv) =
            Build.byteString k <> "=" <> foldMap Build.byteString mv

pair :: (ToQuery a) => ByteString -> a -> QueryString -> QueryString
pair k v = mappend (QPair k (toQuery v))

infixr 7 =:

(=:) :: (ToQuery a) => ByteString -> a -> QueryString
k =: v = QPair k (toQuery v)

toQueryList ::
  (IsList a, ToQuery (Item a)) =>
  ByteString ->
  a ->
  QueryString
toQueryList k = QPair k . QList . zipWith f [1 ..] . toList
  where
    f :: (ToQuery a) => Int -> a -> QueryString
    f n v = toBS n =: toQuery v

toQueryMap ::
  (ToQuery k, ToQuery v) =>
  ByteString ->
  ByteString ->
  ByteString ->
  HashMap k v ->
  QueryString
toQueryMap e k v = toQueryList e . map f . HashMap.toList
  where
    f (x, y) = QList [k =: toQuery x, v =: toQuery y]

class ToQuery a where
  toQuery :: a -> QueryString
  default toQuery :: (ToText a) => a -> QueryString
  toQuery = toQuery . toText

instance ToQuery QueryString where
  toQuery = id

instance (ToByteString k, ToQuery v) => ToQuery (k, v) where
  toQuery (k, v) = QPair (toBS k) (toQuery v)

instance ToQuery Char where
  toQuery = toQuery . BS8.singleton

instance ToQuery ByteString where
  toQuery "" = QValue Nothing
  toQuery bs = QValue (Just bs)

instance ToQuery Text where toQuery = toQuery . Text.encodeUtf8

instance ToQuery Int where toQuery = toQuery . toBS

instance ToQuery Integer where toQuery = toQuery . toBS

instance ToQuery Double where toQuery = toQuery . toBS

instance ToQuery Natural where toQuery = toQuery . toBS

instance (ToQuery a) => ToQuery (Maybe a) where
  toQuery (Just x) = toQuery x
  toQuery Nothing = mempty

instance ToQuery Bool where
  toQuery True = toQuery ("true" :: ByteString)
  toQuery False = toQuery ("false" :: ByteString)
