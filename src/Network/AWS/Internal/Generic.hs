{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternGuards       #-}

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

import           Control.Applicative
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import           Data.Char                  (isLower, toLower)
import           Data.Foldable              (Foldable)
import           Data.List                  (sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Data.Traversable           (Traversable)
import           GHC.Generics
import           Network.AWS.Internal.Types
import           Network.HTTP.Types         (urlEncode)

-- instance IsXML UTCTime where
--     xmlPickler = xpPrim

-- instance IsXML Bool where
--     xmlPickler = (inp, out) `xpWrap` xpContent xpText
--       where
--         inp (lower -> "true")  = True
--         inp (lower -> "false") = False
--         inp _ = error "No parse for bool in toBool (XmlPickler)."

--         out True  = "true"
--         out False = "false"

--         lower = BS.map toLower

--
-- Functions
--

-- queryString :: IsQuery a => a -> [(ByteString, ByteString)]
-- queryString = format "" . toQuery
--   where
--     format :: ByteString -> Query -> [(ByteString, ByteString)]
--     format _  Null        = []
--     format k  (Value v)   = [(k, v)]
--     format k  (List qs)   = concatMap (format k) qs
--     format k1 (Pair k2 q)
--         | BS.null k1 = format k2 q
--         | otherwise  = format (k1 <> "." <> k2) q

format :: [(ByteString, ByteString)] -> ByteString
format = BS.intercalate "&" . map concatEq . sort
  where
    concatEq (k, v) = mconcat [k, "=", urlEncode True v]

pickleQuery :: IsQuery a => PU a -> a -> Query
pickleQuery = pickle

unpickleQuery :: IsQuery a => PU a -> Query -> Maybe a
unpickleQuery = unpickle

--
-- Defining Queries
--

loweredQuery :: Options
loweredQuery = defaultOptions
    { fieldLabelModifier = map toLower . dropWhile isLower
    }

-- genericQueryPickler :: (Generic a, GIsQuery (Rep a)) => Options -> PU a


--
-- Instances
--

instance IsQuery a => IsQuery (Maybe a) where
    queryPickler = qpOption queryPickler

instance IsQuery () where
    queryPickler = qpLift ()

-- instance IsQuery a => IsQuery [a] where
--     toQuery = List . zipWith (\n -> Pair (toBS n) . toQuery) ([1..] :: [Integer])

instance IsQuery Int where
    queryPickler = qpPrim

instance IsQuery Integer where
    queryPickler = qpPrim

instance IsQuery Bool where
    queryPickler = qpPrim

instance IsQuery UTCTime where
    queryPickler = qpPrim

instance IsQuery ByteString where
    queryPickler = PU
        { pickle   = Value
        , unpickle = \qry -> case qry of
              (Value v) -> Just v
              _         -> Nothing
        }



-- --
-- -- Wrappers
-- --

-- newtype Params b a = Params { unParams :: [a] }
--     deriving (Functor, Foldable, Traversable, Show, Generic)

-- instance (IsQuery a, Show b) => IsQuery (Params b a) where
--     toQuery (Params xs) =
--         Pair (BS.pack . map toLower $ show (undefined :: b)) $ toQuery xs

-- instance Eq a => Eq (Params b a) where
--     a == b = unParams a == unParams b

-- data Member

-- instance Show Member where
--     show _ = "member"

--
-- Test
--

data Qux
    = Corge
    | Grault
    | Waldo
      deriving (Show, Generic)

instance IsQuery Qux

data Foo = Foo
    { fooString :: ByteString
    , fooInt    :: Int
    } deriving (Show, Generic)

instance IsQuery Foo

data Bar = Bar
    { barString :: ByteString
    , barFoo    :: Foo
    , barQux    :: Qux
    } deriving (Show, Generic)

instance IsQuery Bar

--
-- Class
--

data Query
    = List [Query]
    | Pair ByteString Query
    | Value ByteString
      deriving (Eq, Show)

instance Monoid Query where
    mempty      = List []
    mappend l r = List [l, r]

data PU a = PU
    { pickle   :: a -> Query
    , unpickle :: Query -> Maybe a
    }

class IsQuery a where
    queryPickler :: PU a

    default queryPickler :: (Generic a, GIsQuery (Rep a)) => PU a
    queryPickler = genericQueryPickler defaultOptions

--
-- Defining Picklers
--

data Options = Options
    { constructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags.
    , fieldLabelModifier     :: String -> String
      -- ^ Function applied to record field labels.
    }

defaultOptions :: Options
defaultOptions = Options id (dropWhile isLower)

genericQueryPickler opts =
    (to, from) `qpWrap` (gQueryPickler opts) (genericQueryPickler opts)

--
-- Combinators
--

qpWrap :: (a -> b, b -> a) -> PU a -> PU b
qpWrap (f, g) pua = PU
    { pickle   = pickle pua . g
    , unpickle = fmap f . unpickle pua
    }

qpElem :: ByteString -> PU a -> PU a
qpElem name pu = PU
    { pickle   = Pair name . pickle pu
    , unpickle = (unpickle pu =<<) . findPair name
    }

qpPair :: PU a -> PU b -> PU (a, b)
qpPair pua pub = PU
    { pickle   = \(a, b) -> pickle pua a <> pickle pub b
    , unpickle = \qry -> case (unpickle pua qry, unpickle pub qry) of
          (Just a, Just b) -> Just (a, b)
          _                -> Nothing
    }

qpLift :: a -> PU a
qpLift x = PU
    { pickle   = const $ List []
    , unpickle = const $ Just x
    }

qpPrim :: (Read a, Show a) => PU a
qpPrim = PU
    { pickle   = Value . BS.pack . show
    , unpickle = \qry -> case qry of
          (Value v) -> maybeRead $ BS.unpack v
          _         -> Nothing
    }

qpOption :: PU a -> PU (Maybe a)
qpOption pu = PU
    { pickle   = maybe (List []) (pickle pu)
    , unpickle = Just . unpickle pu
    }

qpSum :: PU (f r) -> PU (g r) -> PU ((f :+: g) r)
qpSum left right = (inp, out) `qpWrap` qpEither left right
  where
    inp (Left  x) = L1 x
    inp (Right x) = R1 x

    out (L1 x) = Left x
    out (R1 x) = Right x

qpEither :: PU a -> PU b -> PU (Either a b)
qpEither pua pub = PU pickleEither unpickleEither
  where
    unpickleEither qry = case unpickle pua qry of
        Just x -> Just $ Left x
        _      -> case unpickle pub qry of
            Just y -> Just $ Right y
            _      -> Nothing

    pickleEither (Left  x) = pickle pua x
    pickleEither (Right y) = pickle pub y

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

parseQuery :: ByteString -> Query
parseQuery _ = List []

findPair :: ByteString -> Query -> Maybe Query
findPair k qry
    | List qs <- qry           = listToMaybe . catMaybes $ map (findPair k) qs
    | Pair k' q <- qry, k == k' = Just q
    | otherwise               = Nothing

--
-- Generics
--

class GIsQuery f where
    gQueryPickler :: Options -> PU a -> PU (f a)

instance IsQuery a => GIsQuery (K1 i a) where
    gQueryPickler _ _ = (K1, unK1) `qpWrap` queryPickler

instance GIsQuery U1 where
    gQueryPickler _ _ = (const U1, const ()) `qpWrap` qpLift ()

instance (Datatype d, GIsQuery f) => GIsQuery (M1 D d f) where
    gQueryPickler opts = qpWrap (M1, unM1) . gQueryPickler opts

instance (GIsQuery f, GIsQuery g) => GIsQuery (f :+: g) where
    gQueryPickler opts f = gQueryPickler opts f `qpSum` gQueryPickler opts f

instance (GIsQuery f, GIsQuery g) => GIsQuery (f :*: g) where
    gQueryPickler opts f = qpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (gQueryPickler opts f `qpPair` gQueryPickler opts f)

instance (Constructor c, GIsQuery f) => GIsQuery (M1 C c f) where
    gQueryPickler opts f = qpElem
        (BS.pack . constructorTagModifier opts $ conName (undefined :: M1 C c f r))
        ((M1, unM1) `qpWrap` gQueryPickler opts f)

instance (Selector s, GIsQuery f) => GIsQuery (M1 S s f) where
    gQueryPickler opts f = qpElem
        (BS.pack . fieldLabelModifier opts $ selName (undefined :: M1 S s f r))
        ((M1, unM1) `qpWrap` gQueryPickler opts f)

-- instance GIsQuery a => GIsQuery(M1 i c a) where
--     gQueryPickler opts = gQueryPickler opts . unM1

-- instance IsQuery a => GIsQuery (K1 i a) where
--     gQueryPickler _ = Query . unK1

-- instance GIsQuery U1 where
--     gQueryPickler _ _ _ = Null

-- instance ConsIsQuery a => GIsQuery (C1 c a) where
--     gQueryPickler opts = consIsQuery opts . unM1

-- instance ( AllNullary (f :+: g) allNullary
--          , SumIsQuery (f :+: g) allNullary
--          ) => GIsQuery (f :+: g) where
--     gQueryPickler opts =
--         (unTagged :: Tagged allNullary Query -> Query) . sumIsQuery opts

-- class SumIsQuery f allNullary where
--     sumIsQuery :: Options -> f a -> Tagged allNullary Query

-- instance GetConName f => SumIsQuery f True where
--     sumIsQuery _ = Tagged . Value . BS.pack . getConName

-- class ConsIsQuery f where
--     consIsQuery :: Options -> f a -> Query

-- class ConsIsQuery' f isRecord where
--     consIsQuery' :: Options -> f a -> Tagged isRecord Query

-- instance (IsRecord f isRecord, ConsIsQuery' f isRecord) => ConsIsQuery f where
--     consIsQuery opts =
--         (unTagged :: Tagged isRecord Query -> Query) . consIsQuery' opts

-- instance RecordToPairs f => ConsIsQuery' f True where
--     consIsQuery' opts = Tagged . recordToPairs opts

-- instance GIsQuery f => ConsIsQuery' f False where
--     consIsQuery' opts = Tagged . gQueryPickler opts

-- class RecordToPairs f where
--     recordToPairs :: Options -> f a -> Query

-- instance (RecordToPairs a, RecordToPairs b) => RecordToPairs (a :*: b) where
--     recordToPairs opts (a :*: b) = recordToPairs opts a <> recordToPairs opts b

-- instance (Selector s, GIsQuery a) => RecordToPairs (S1 s a) where
--     recordToPairs = fieldToPair

-- instance (Selector s, IsQuery a) => RecordToPairs (S1 s (K1 i (Maybe a))) where
--     recordToPairs = fieldToPair

-- fieldToPair :: (Selector s, GIsQuery a) => Options -> S1 s a p -> Query
-- fieldToPair opts m1 = Pair
--     (BS.pack . fieldLabelModifier opts $ selName m1)
--     (gQueryPickler opts $ unM1 m1)

-- class GetConName f where
--     getConName :: f a -> String

-- instance (GetConName a, GetConName b) => GetConName (a :+: b) where
--     getConName (L1 x) = getConName x
--     getConName (R1 x) = getConName x

-- instance (Constructor c, GIsQuery a) => GetConName (C1 c a) where
--     getConName = conName

-- class IsRecord (f :: * -> *) isRecord | f -> isRecord

-- instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
-- instance IsRecord (M1 S NoSelector f) False
-- instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
-- instance IsRecord (K1 i c) True
-- instance IsRecord U1 False

-- class AllNullary (f :: * -> *) allNullary | f -> allNullary

-- instance ( AllNullary a allNullaryL
--          , AllNullary b allNullaryR
--          , And allNullaryL allNullaryR allNullary
--          ) => AllNullary (a :+: b) allNullary

-- instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
-- instance AllNullary (a :*: b) False
-- instance AllNullary (K1 i c) False
-- instance AllNullary U1 True

-- data True
-- data False

-- class And bool1 bool2 bool3 | bool1 bool2 -> bool3

-- instance And True  True  True
-- instance And False False False
-- instance And False True  False
-- instance And True  False False

-- newtype Tagged s b = Tagged { unTagged :: b }
