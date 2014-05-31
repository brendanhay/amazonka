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

    -- ** Traversals
    , keysOf
    , valuesOf

    -- * Deserialisation
    , FromQuery (..)
    , decodeQuery

    -- * Serialisation
    , ToQuery   (..)
    , renderQuery
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Error                (note)
import           Control.Lens
import           Control.Lens.Plated
import           Control.Lens.TH
import           Control.Monad
import qualified Data.Attoparsec.ByteString   as ABS
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Data.Char
import           Data.Data
import           Data.Data.Lens
import           Data.Default
import           Data.Either
import           Data.Foldable                (foldl')
import qualified Data.Foldable                as Fold
import           Data.Maybe
import           Data.Typeable
import           Network.AWS.Data.ByteString
-- import           Data.HashMap.Strict        (HashMap)
import           Data.List                    (sort, sortBy, intersperse)
import           Data.List.NonEmpty           (NonEmpty(..))
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Monoid
import           Data.String
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString
import qualified Data.ByteString.Lazy         as Build
import qualified Data.ByteString.Lazy.Builder as Build
import           Data.Time
import           GHC.Generics
import qualified Network.HTTP.Types.URI       as URI
import           Data.Ord

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
    fromString [] = Value Nothing
    fromString xs = Value . Just $ BS.pack xs

-- FIXME: Neither of these is the correct type
-- And what about the breaking of query elements? not all pieces have =

decodeQuery :: ByteString -> Query
decodeQuery = Fold.foldl' (\a b -> reify b <> a) mempty . URI.parseQuery
  where
    reify (k, v)
        | BS.null k         = Value v
        | BS.any (== '.') k = fold k v
        | otherwise         = Pair k $ Value v

    fold k v =
        let ks     = BS.split '.' k
            f k' q = Pair k' q
         in foldr f (Pair (last ks) $ Value v) $ init ks

renderQuery :: ByteString -> ByteString -> Query -> ByteString
renderQuery ksep vsep = intercalate . sort . enc Nothing
  where
    enc k (List xs)   = concatMap (enc k) xs
    enc k (Pair k' x)
        | Just n <- k = enc (Just $ n <> "." <> k') x
        | otherwise   = enc (Just $ k') x
    enc k (Value (Just v))
        | Just n <- k = [n <> vsep <> v]
        | otherwise   = [v]
    enc k _
        | Just n <- k = [n]
        | otherwise   = []

    intercalate []       = mempty
    intercalate (x : []) = x
    intercalate (x : xs) = x <> ksep <> intercalate xs

-- FIXME: Can be removed when lens 4.2 is released.
deep :: forall s a. Plated s => Traversal' s a -> Traversal' s a
deep f = go
  where
    go :: Traversal' s a
    go = cloneTraversal $ failing f (plate . go)

-- keysOf :: Traversal' Query ByteString

-- valuesOf :: Traversal' Query (Maybe ByteString)

-- data QueryOptions = QueryOptions
--     { queryCtorMod  :: String -> ByteString
--     , queryFieldMod :: String -> ByteString
--     }

-- instance Default QueryOptions where
--     def = QueryOptions
--         { queryCtorMod  = ByteString.pack
--         , queryFieldMod = safeDropLower
--         }

-- newtype LoweredQueryOptions = LoweredQueryOptions { lowered :: QueryOptions }

-- instance Default LoweredQueryOptions where
--     def = LoweredQueryOptions $ def
--         { queryFieldMod = ByteString.toLower . safeDropLower
--         }

-- safeDropLower :: String -> ByteString
-- safeDropLower [] = ByteString.empty
-- safeDropLower xs
--     | all isLower xs = t
--     | otherwise      = ByteString.dropWhile isLower t
--   where
--     t = ByteString.pack xs

-- fromLoweredQuery :: (Generic a, GFromQuery (Rep a))
--                  => Query
--                  -> Either String a
-- fromLoweredQuery = genericFromQuery (lowered def)

-- genericFromQuery :: (Generic a, GFromQuery (Rep a))
--                  => QueryOptions
--                  -> Query
--                  -> Either String a
-- genericFromQuery o = fmap to . gFromQuery o

class FromQuery a where
    fromQuery :: Query -> Either String a

--     default fromQuery :: (Generic a, GFromQuery (Rep a))
--                       => Query
--                       -> Either String a
--     fromQuery = genericFromQuery def

-- instance FromQuery ByteString where
--     fromQuery = valueParser ABS.takeByteString

-- instance FromQuery Int where
--     fromQuery = valueParser ABS.decimal

-- instance FromQuery Integer where
--     fromQuery = valueParser ABS.decimal

-- instance FromQuery Double where
--     fromQuery = valueParser ABS.rational

-- instance FromQuery Float where
--     fromQuery = valueParser ABS.rational

-- instance FromQuery a => FromQuery [a] where
--     fromQuery (List qs) = concatEithers $ map fromQuery [v | Pair _ v <- sort qs]
--       where
--         concatEithers xs = case partitionEithers xs of
--             (l:_, _) -> Left l
--             ([], rs) -> Right rs
--     fromQuery _         = Left "Unexpected non-list."

-- instance FromQuery a => FromQuery (NonEmpty a) where
--     fromQuery = join
--         . fmap (note "Unexpected empty list." . NonEmpty.nonEmpty)
--         . fromQuery

-- -- FIXME: should fail if target doesn't exist
-- instance FromQuery a => FromQuery (Maybe a) where
--     fromQuery q =
--         either (const $ Right Nothing)
--                (Right . Just)
--                (fromQuery q)

-- instance FromQuery Bool where
--     fromQuery = valueParser (p "true" True <|> p "false" False)
--       where
--         p s b = ABS.string s *> return b <* ABS.endOfInput

-- instance FromQuery UTCTime where
--     fromQuery (Value v) = parseISO8601 $ ByteString.unpack v
--     fromQuery _         = Left "Unexpected non-value."

-- instance FromQuery () where
--     fromQuery _ = Right ()

-- -- FIXME: implement this shizzle
-- -- instance (FromQuery k, FromQuery v) => FromQuery (HashMap k v) where
-- --     fromQuery = undefined

-- valueParser :: ABS.Parser a -> Query -> Either String a
-- valueParser p (Value v) = ABS.parseOnly p v
-- valueParser _ _         = Left "Unexpected non-value."

-- class GFromQuery f where
--     gFromQuery :: QueryOptions -> Query -> Either String (f a)

-- instance (GFromQuery f, GFromQuery g) => GFromQuery (f :+: g) where
--     gFromQuery o q = (L1 <$> gFromQuery o q) <|> (R1 <$> gFromQuery o q)

-- instance (GFromQuery f, GFromQuery g) => GFromQuery (f :*: g) where
--     gFromQuery o q = (:*:) <$> gFromQuery o q <*> gFromQuery o q

-- instance GFromQuery U1 where
--     gFromQuery _ _ = Right U1

-- instance FromQuery a => GFromQuery (K1 R a) where
--     gFromQuery _ = fmap K1 . fromQuery

-- instance GFromQuery f => GFromQuery (D1 c f) where
--     gFromQuery o = fmap M1 . gFromQuery o

-- instance GFromQuery f => GFromQuery (C1 c f) where
--     gFromQuery o = fmap M1 . gFromQuery o

-- instance (Selector c, GFromQuery f) => GFromQuery (S1 c f) where
--     gFromQuery o =
--         either Left (fmap M1 . gFromQuery o)
--             . note ("Unable to find: " ++ ByteString.unpack name)
--             . findPair name
--       where
--         name = queryFieldMod o $ selName (undefined :: S1 c f p)

--         findPair k qry
--             | List qs <- qry            = mconcat $ map (findPair k) qs
--             | Pair k' q <- qry, k == k' = Just q
--             | otherwise                 = Nothing

-- toLoweredQuery :: (Generic a, GToQuery (Rep a))
--                => a
--                -> Query
-- toLoweredQuery = genericToQuery (lowered def)

-- genericToQuery :: (Generic a, GToQuery (Rep a))
--                => QueryOptions
--                -> a
--                -> Query
-- genericToQuery o = gToQuery o . from

class ToQuery a where
    toQuery :: a -> Query

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

-- instance ToQuery Int where
--     toQuery = valueFromIntegral

-- instance ToQuery Integer where
--     toQuery = valueFromIntegral

-- instance ToQuery Double where
--     toQuery = valueFromFloat

-- instance ToQuery Float where
--     toQuery = valueFromFloat

-- instance ToQuery a => ToQuery [a] where
--     toQuery = List . zipWith (\n v -> Pair (key n) (toQuery v)) idx
--       where
--         key = LByteString.toStrict . LByteString.toLazyByteString . LByteString.decimal
--         idx = [1..] :: [Integer]

-- instance ToQuery a => ToQuery (NonEmpty a) where
--     toQuery = toQuery . NonEmpty.toList

-- instance ToQuery a => ToQuery (Maybe a) where
--     toQuery (Just x) = toQuery x
--     toQuery Nothing  = mempty

-- instance ToQuery Bool where
--     toQuery True  = Value "true"
--     toQuery False = Value "false"

-- instance ToQuery UTCTime where
--     toQuery = Value . formatISO8601

-- instance ToQuery () where
--     toQuery () = mempty

-- -- FIXME: implement this shizzle
-- -- instance (ToQuery k, ToQuery v) => ToQuery (HashMap k v) where
-- --     toQuery = undefined

-- valueFromIntegral :: Integral a => a -> Query
-- valueFromIntegral = Value . integralToByteString

-- valueFromFloat :: RealFloat a => a -> Query
-- valueFromFloat = Value . floatToByteString

-- class GToQuery f where
--     gToQuery :: QueryOptions -> f a -> Query

-- instance (GToQuery f, GToQuery g) => GToQuery (f :+: g) where
--     gToQuery o (L1 x) = gToQuery o x
--     gToQuery o (R1 y) = gToQuery o y

-- instance (GToQuery f, GToQuery g) => GToQuery (f :*: g) where
--     gToQuery o (x :*: y) = gToQuery o x <> gToQuery o y

-- instance GToQuery U1 where
--     gToQuery _ _ = mempty

-- instance ToQuery a => GToQuery (K1 R a) where
--     gToQuery _ = toQuery . unK1

-- instance GToQuery f => GToQuery (D1 c f) where
--     gToQuery o = gToQuery o . unM1

-- instance GToQuery f => GToQuery (C1 c f) where
--     gToQuery o = gToQuery o . unM1

-- instance (Selector c, GToQuery f) => GToQuery (S1 c f) where
--     gToQuery o = Pair name . gToQuery o . unM1
--       where
--         name = queryFieldMod o $ selName (undefined :: S1 c f p)
