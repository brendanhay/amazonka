{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- Module      : Network.AWS.Data.Query
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Query where

import           Control.Applicative
import           Control.Error              (note)
import           Control.Monad
import qualified Data.Attoparsec.Text       as AText
import           Data.Char
import           Data.Default
import           Data.Either
import           Data.Foldable              (foldl')
-- import           Data.HashMap.Strict        (HashMap)
import           Data.List                  (sort)
import           Data.List.NonEmpty         (NonEmpty(..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Monoid
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as LText
import qualified Data.Text.Lazy.Builder.Int as LText
import           Data.Time
import           GHC.Generics

-- -- FIXME: Neither of these is the correct type
-- -- And what about the breaking of query elements? not all pieces have =

-- decodeQuery :: FromQuery a => [(Text, Maybe Text)] -> Either String a
-- decodeQuery = fromQuery . foldl' (\a b -> reify b <> a) mempty
--   where
--     reify (k, v)
--         | Text.null k         = Value v
--         | Text.any (== '.') k = fold k v
--         | otherwise           = Pair k $ Value v

--     fold k v =
--         let ks     = Text.split (== '.') k
--             f k' q = Pair k' q
--         in  foldr f (Pair (last ks) $ Value v) $ init ks

encodeQuery :: ToQuery a => a -> [(Text, Maybe Text)]
encodeQuery = enc "" . toQuery
  where
    enc k (List qs) = concatMap (enc k) qs
    enc k (Value v) = [(k, v)]
    enc k (Pair k' q)
        | Text.null k = enc k' q
        | otherwise   = enc (k <> "." <> k') q

-- queryFromList :: [Query] -> Query
-- queryFromList = List

data Query
    = List  [Query]
    | Value (Maybe Text)
    | Pair  Text Query
      deriving (Eq, Show)

instance Monoid Query where
    mempty                    = List []
    mappend (List l) (List r) = List (l ++ r)
    mappend (List l) r        = List (r : l)
    mappend l        (List r) = List (l : r)
    mappend l        r        = List [l, r]

-- instance Ord Query where
--     compare (List  ls)   (List  rs)   = ls `compare` rs
--     compare (Pair  k1 _) (Pair  k2 _) = k1 `compare` k2
--     compare (Value v1)   (Value v2)   = v1 `compare` v2

--     compare (List _)   (Pair _ _) = GT
--     compare (List _)   (Value _)  = GT
--     compare (Pair _ _) (Value _)  = GT

--     compare _ _ = LT

-- instance IsString Query where
--     fromString = Value . Text.pack

-- data QueryOptions = QueryOptions
--     { queryCtorMod  :: String -> Text
--     , queryFieldMod :: String -> Text
--     }

-- instance Default QueryOptions where
--     def = QueryOptions
--         { queryCtorMod  = Text.pack
--         , queryFieldMod = safeDropLower
--         }

-- newtype LoweredQueryOptions = LoweredQueryOptions { lowered :: QueryOptions }

-- instance Default LoweredQueryOptions where
--     def = LoweredQueryOptions $ def
--         { queryFieldMod = Text.toLower . safeDropLower
--         }

-- safeDropLower :: String -> Text
-- safeDropLower [] = Text.empty
-- safeDropLower xs
--     | all isLower xs = t
--     | otherwise      = Text.dropWhile isLower t
--   where
--     t = Text.pack xs

-- fromLoweredQuery :: (Generic a, GFromQuery (Rep a))
--                  => Query
--                  -> Either String a
-- fromLoweredQuery = genericFromQuery (lowered def)

-- genericFromQuery :: (Generic a, GFromQuery (Rep a))
--                  => QueryOptions
--                  -> Query
--                  -> Either String a
-- genericFromQuery o = fmap to . gFromQuery o

-- class FromQuery a where
--     fromQuery :: Query -> Either String a

--     default fromQuery :: (Generic a, GFromQuery (Rep a))
--                       => Query
--                       -> Either String a
--     fromQuery = genericFromQuery def

-- instance FromQuery Text where
--     fromQuery = valueParser AText.takeText

-- instance FromQuery Int where
--     fromQuery = valueParser AText.decimal

-- instance FromQuery Integer where
--     fromQuery = valueParser AText.decimal

-- instance FromQuery Double where
--     fromQuery = valueParser AText.rational

-- instance FromQuery Float where
--     fromQuery = valueParser AText.rational

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
--         p s b = AText.string s *> return b <* AText.endOfInput

-- instance FromQuery UTCTime where
--     fromQuery (Value v) = parseISO8601 $ Text.unpack v
--     fromQuery _         = Left "Unexpected non-value."

-- instance FromQuery () where
--     fromQuery _ = Right ()

-- -- FIXME: implement this shizzle
-- -- instance (FromQuery k, FromQuery v) => FromQuery (HashMap k v) where
-- --     fromQuery = undefined

-- valueParser :: AText.Parser a -> Query -> Either String a
-- valueParser p (Value v) = AText.parseOnly p v
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
--             . note ("Unable to find: " ++ Text.unpack name)
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

-- instance ToQuery Text where
--     toQuery = Value

instance ToQuery (Text, Text) where
    toQuery (k, v) = Pair k . Value $ Just v

instance ToQuery (Text, Maybe Text) where
    toQuery (k, mv) = Pair k (Value mv)

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
--         key = LText.toStrict . LText.toLazyText . LText.decimal
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
-- valueFromIntegral = Value . integralToText

-- valueFromFloat :: RealFloat a => a -> Query
-- valueFromFloat = Value . floatToText

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
