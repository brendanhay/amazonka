{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Network.AWS.Internal.Generic
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Generic where
    -- (
    -- -- * Types
    --   ToQuery (..)
    -- , Query   (..)

    -- -- * Query Param Wrappers
    -- , Params(..)

    -- -- * Functions
    -- , queryString

    -- -- * Defining Queries
    -- , loweredQuery
    -- , genericQuery
    -- , packQS
    -- ) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import           Data.Char                  (isLower, toLower)
import           Data.Foldable              (Foldable)
import           Data.List                  (sort)
import           Data.Monoid
import           Data.Time
import           Data.Traversable           (Traversable)
import           GHC.Generics
import           Network.AWS.Internal.Types
import           Network.HTTP.Types         (urlEncode)
import           Text.XML.Expat.Generic

instance IsXML UTCTime where
    xmlPickler = xpPrim

instance IsXML Bool where
    xmlPickler = (inp, out) `xpWrap` xpContent xpText
      where
        inp (lower -> "true")  = True
        inp (lower -> "false") = False
        inp _ = error "No parse for bool in toBool (XmlPickler)."

        out True  = "true"
        out False = "false"

        lower = BS.map toLower

--
-- Class
--

data Query
    = List [Query]
    | Pair ByteString Query
    | Value ByteString
    | Null
      deriving (Eq, Show)

instance Monoid Query where
    mempty      = Null
    mappend l r = List [l, r]

class ToQuery a where
    queryPickler :: a -> Query

    default toQuery :: (Generic a, GToQuery (Rep a)) => a -> Query
    toQuery = genericQuery defaultOptions

class FromQuery a

--
-- Functions
--

queryString :: ToQuery a => a -> [(ByteString, ByteString)]
queryString = format "" . toQuery
  where
    format :: ByteString -> Query -> [(ByteString, ByteString)]
    format _  Null        = []
    format k  (Value v)   = [(k, v)]
    format k  (List qs)   = concatMap (format k) qs
    format k1 (Pair k2 q)
        | BS.null k1 = format k2 q
        | otherwise  = format (k1 <> "." <> k2) q

fmtQueryString :: [(ByteString, ByteString)] -> ByteString
fmtQueryString = BS.intercalate "&" . map concatEq . sort
  where
    concatEq (k, v) = mconcat [k, "=", urlEncode True v]

--
-- Defining Queries
--

loweredQuery :: Options
loweredQuery = defaultOptions
    { fieldLabelModifier = map toLower . dropWhile isLower
    }

genericQuery :: (Generic a, GToQuery (Rep a)) => Options -> a -> Query
genericQuery opts = gQueryString opts . from

packQS :: IsByteString a => a -> Query
packQS = Value . toBS

--
-- Instances
--

instance ToQuery a => ToQuery (Maybe a) where
    toQuery (Just v) = toQuery v
    toQuery Nothing  = Null

instance ToQuery () where
    toQuery _ = Null

instance ToQuery ByteString where
    toQuery = Value

instance ToQuery a => ToQuery [a] where
    toQuery = List . zipWith (\n -> Pair (toBS n) . toQuery) ([1..] :: [Integer])

instance ToQuery Int where
    toQuery = packQS

instance ToQuery Integer where
    toQuery = packQS

instance ToQuery Bool where
    toQuery = packQS . map toLower . show

instance ToQuery UTCTime where
    toQuery = packQS

--
-- Wrappers
--

newtype Params b a = Params { unParams :: [a] }
    deriving (Functor, Foldable, Traversable, Show, Generic)

instance (ToQuery a, Show b) => ToQuery (Params b a) where
    toQuery (Params xs) =
        Pair (BS.pack . map toLower $ show (undefined :: b)) $ toQuery xs

instance Eq a => Eq (Params b a) where
    a == b = unParams a == unParams b

data Member

instance Show Member where
    show _ = "member"

--
-- Generics
--

class GToQuery f where
    gQueryString :: Options -> f a -> Query

instance GToQuery a => GToQuery(M1 i c a) where
    gQueryString opts = gQueryString opts . unM1

instance ToQuery a => GToQuery (K1 i a) where
    gQueryString _ = toQuery . unK1

instance GToQuery U1 where
    gQueryString _ _ = Null

instance ConsToQuery a => GToQuery (C1 c a) where
    gQueryString opts = consToQuery opts . unM1

instance ( AllNullary (f :+: g) allNullary
         , SumToQuery (f :+: g) allNullary
         ) => GToQuery (f :+: g) where
    gQueryString opts =
        (unTagged :: Tagged allNullary Query -> Query) . sumToQuery opts

class SumToQuery f allNullary where
    sumToQuery :: Options -> f a -> Tagged allNullary Query

instance GetConName f => SumToQuery f True where
    sumToQuery _ = Tagged . Value . BS.pack . getConName

class ConsToQuery f where
    consToQuery :: Options -> f a -> Query

class ConsToQuery' f isRecord where
    consToQuery' :: Options -> f a -> Tagged isRecord Query

instance (IsRecord f isRecord, ConsToQuery' f isRecord) => ConsToQuery f where
    consToQuery opts =
        (unTagged :: Tagged isRecord Query -> Query) . consToQuery' opts

instance RecordToPairs f => ConsToQuery' f True where
    consToQuery' opts = Tagged . recordToPairs opts

instance GToQuery f => ConsToQuery' f False where
    consToQuery' opts = Tagged . gQueryString opts

class RecordToPairs f where
    recordToPairs :: Options -> f a -> Query

instance (RecordToPairs a, RecordToPairs b) => RecordToPairs (a :*: b) where
    recordToPairs opts (a :*: b) = recordToPairs opts a <> recordToPairs opts b

instance (Selector s, GToQuery a) => RecordToPairs (S1 s a) where
    recordToPairs = fieldToPair

instance (Selector s, ToQuery a) => RecordToPairs (S1 s (K1 i (Maybe a))) where
    recordToPairs = fieldToPair

fieldToPair :: (Selector s, GToQuery a) => Options -> S1 s a p -> Query
fieldToPair opts m1 = Pair
    (BS.pack . fieldLabelModifier opts $ selName m1)
    (gQueryString opts $ unM1 m1)

class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x

instance (Constructor c, GToQuery a) => GetConName (C1 c a) where
    getConName = conName

class IsRecord (f :: * -> *) isRecord | f -> isRecord

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord U1 False

class AllNullary (f :: * -> *) allNullary | f -> allNullary

instance ( AllNullary a allNullaryL
         , AllNullary b allNullaryR
         , And allNullaryL allNullaryR allNullary
         ) => AllNullary (a :+: b) allNullary

instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
instance AllNullary (a :*: b) False
instance AllNullary (K1 i c) False
instance AllNullary U1 True

data True
data False

class And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And True  True  True
instance And False False False
instance And False True  False
instance And True  False False

newtype Tagged s b = Tagged { unTagged :: b }
