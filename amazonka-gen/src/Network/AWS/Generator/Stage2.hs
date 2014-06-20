{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Generator.Stage2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Generator.Stage2 where

import           Control.Applicative
import           Control.Arrow
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Default
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.List
import           Data.List.NonEmpty           (NonEmpty(..))
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String
import           Data.String.CaseConversion
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Util
import           GHC.Generics
import qualified Network.AWS.Generator.Stage1 as Stage1
import           Network.AWS.Generator.Stage1 hiding (Service, Operation)
import           Network.AWS.Generator.Types
import qualified Text.EDE                     as EDE

transform :: Stage1.Service -> Service
transform = trans

class Transform a where
    type T a :: *
    trans :: T a -> a

instance Transform ServiceType where
    type T ServiceType = Stage1.Service

    trans Stage1.Service{..}
        | s1SignatureVersion == S3 = RestS3
        | otherwise                = s1Type

instance Transform Signature where
    type T Signature = Stage1.Service

    trans Stage1.Service{..}
        | s1SignatureVersion == S3 = V4
        | otherwise                = s1SignatureVersion

newtype Doc = Doc { unDoc :: Text }
    deriving (Eq, Ord)

instance Transform (Maybe Doc) where
    type T (Maybe Doc) = Maybe Text

    trans = fmap Doc

instance ToJSON Doc where
    toJSON (Doc x) = toJSON x

data Service = Service
    { s2Type             :: ServiceType
    , s2Version          :: Text
    , s2Error            :: Text
    , s2Signature        :: Signature
    , s2Namespace        :: NS
    , s2VersionNamespace :: NS
    , s2TypesNamespace   :: NS
    , s2Abbrev           :: Abbrev
    , s2Documentation    :: Maybe Doc
    , s2Operations       :: [Operation]
    } deriving (Eq, Generic)

instance Ord Service where
    compare a b = f s2VersionNamespace <> f s2Version
      where
        f :: Ord a => (Service -> a) -> Ordering
        f g = compare (g a) (g b)

instance Transform Service where
    type T Service = Stage1.Service

    trans s = knot
      where
        knot = Service
            { s2Type             = trans s
            , s2Version          = s1ApiVersion s
            , s2Error            = unAbbrev (s2Abbrev knot) <> "Error"
            , s2Signature        = trans s
            , s2Namespace        = root (trans s)
            , s2VersionNamespace = trans s
            , s2TypesNamespace   = trans s <> "Types"
            , s2Abbrev           = trans s
            , s2Documentation    = trans (s1Documentation s)
            , s2Operations       = trans (knot, s1Operations s)
            }

instance ToJSON Service where
    toJSON = toField (recase Camel Under . drop 2)

current :: [Service] -> [Service]
current = mapMaybe latest . groupBy identical
  where
    identical x y = EQ == comparing s2Abbrev x y

    latest [] = Nothing
    latest xs = Just . head $ sortBy (comparing s2Version) xs

data NS = NS { unNS :: [Text] }
    deriving (Eq, Ord)

root :: NS -> NS
root = NS . reverse . drop 1 . reverse . unNS

instance IsString NS where
    fromString = NS . Text.splitOn "." . Text.pack

instance Monoid NS where
    mempty      = NS []
    mappend a b = NS $ unNS a <> unNS b

instance Transform NS where
    type T NS = Stage1.Service

    trans s = NS . abbrev $ trans s
      where
        abbrev a =
            [ "Network"
            , "AWS"
            , unAbbrev a
            , "V" <> Text.replace "-" "_" (s1ApiVersion s)
            ]

instance ToJSON NS where
    toJSON = toJSON . Text.intercalate "." . unNS

newtype Abbrev = Abbrev { unAbbrev :: Text }
    deriving (Eq, Ord)

instance Transform Abbrev where
    type T Abbrev = Stage1.Service
    trans = Abbrev
        . mconcat
        . Text.words
        . strip "AWS"
        . strip "Amazon"
        . s1ServiceAbbreviation

instance ToJSON Abbrev where
    toJSON = toJSON . unAbbrev

data Ann = Ann
   { anRequired :: !Bool
   , anMonoid   :: !Bool
   , anType     :: Text
   } deriving (Eq, Ord, Generic)

annotate :: Text -> Ann -> Ann
annotate x a
    | x `elem` xs = a { anType = x }
    | otherwise   = a
  where
    xs = ["Bucket", "Key"]

instance Transform Ann where
    type T Ann = Stage1.Shape

    trans s@SList{} = Ann (required s) True $
        "[" <> sName s <> "]"

    trans s@SMap{} = Ann (required s) True $
        "HashMap " <> sName (sKey s) <> " " <> sName (sValue s)

    trans s
        | sStreaming s = Ann (required s) True  "RequestBody"
        | otherwise    = Ann (required s) False (sName s)

required :: Shape -> Bool
required s = sPayload s || sRequired s

instance ToJSON Ann where
    toJSON (Ann True _    t) = toJSON t
    toJSON (Ann _   True  t) = toJSON t
    toJSON (Ann _   False t) = toJSON ("Maybe " <> t)

newtype Loc = Loc { unLoc :: Text }
    deriving (Eq, Generic)

instance Ord Loc where
    compare a b =
        case (unLoc a, unLoc b) of
            (x, y) | x == y    -> EQ
            ("uri",   _)       -> LT
            (_,       "uri")   -> GT
            ("query", _)       -> LT
            (_,       "query") -> GT
            _                  -> GT

instance IsString Loc where
    fromString = Loc . fromString

instance ToJSON Loc where
    toJSON = toJSON . unLoc

data Field = Field
    { f2Name          :: Text
    , f2Required      :: !Bool
    , f2Type          :: Ann
    , f2Location      :: Maybe Loc
    , f2LocationName  :: Maybe Text
    , f2Brief         :: Text
    , f2Documentation :: Maybe Doc
    , f2Payload       :: !Bool
    , f2Streaming     :: !Bool
    } deriving (Eq, Generic)

instance Ord Field where
    compare a b
        | f2Payload a = GT
        | otherwise   = comparing f2Location a b <> comparing f2Name a b

instance Transform [Field] where
    type T [Field] = (Text, Shape)

    trans (p, s@SStruct{}) = sort . map f . Map.toList $ sFields s
      where
        f (k, v) = Field
            { f2Name          = p <> k
            , f2Required      = sRequired v || sPayload v
            , f2Type          = annotate k (trans v)
            , f2Location      = Loc <$> sLocation v
            , f2LocationName  = sLocationName v
            , f2Brief         = k
            , f2Documentation = trans (sDocumentation v)
            , f2Payload       = sPayload v
            , f2Streaming     = sStreaming v
            }

    trans _ = error "Unable to transform fields from non-structure."

instance ToJSON Field where
    toJSON = toField (recase Camel Under . drop 2)

instance Transform HTTP where
    type T HTTP = (Text, HTTP)

    trans (p, h) = h
        { hUri   = map f (hUri h)
        , hQuery = Map.fromList . map g $ Map.toList (hQuery h)
        }
      where
        f (I t) = I (p <> t)
        f x     = x

        g = second (fmap (p <>))

data Request = Request
    { rq2Name     :: Text
    , rq2Http     :: HTTP
    , rq2Fields   :: [Field]
    , rq2Required :: [Field]
    , rq2Headers  :: [Field]
    , rq2Payload  :: Maybe Field
    } deriving (Eq, Generic)

instance Transform Request where
    type T Request = Stage1.Operation

    trans o = case o1Input o of
        Nothing -> Request name http mempty mempty mempty Nothing
        Just x  ->
            let fs = trans (pre, x)
                hs = filter (\f -> Just "header" == f2Location f) fs
                rs = filter f2Required fs
                p  = find f2Payload fs
             in Request name http fs rs hs p
      where
        http = fromMaybe def $ trans . (pre,) <$> o1Http o
        pre  = prefix name <> "r"
        name = o1Name o

instance ToJSON Request where
    toJSON = toField (recase Camel Under . drop 3)

data Response = Response
    { rs2Name   :: Text
    , rs2Fields :: [Field]
    } deriving (Eq, Generic)

instance Transform Response where
    type T Response = Stage1.Operation

    trans o = case o1Output o of
        Nothing -> Response (o1Name o <> "Response") mempty
        Just x  -> Response (sName x) (trans (pre, x))
      where
        pre = prefix (o1Name o) <> "rs"

instance ToJSON Response where
    toJSON = toField (recase Camel Under . drop 3)

data Operation = Operation
    { o2Service       :: Abbrev
    , o2Name          :: Text
    , o2Namespace     :: NS
    , o2Modules       :: [NS]
    , o2Documentation :: Maybe Doc
    , o2Http          :: HTTP
    , o2Request       :: Request
    , o2Response      :: Response
    } deriving (Eq, Generic)

instance Transform [Operation] where
    type T [Operation] = (Service, HashMap Text Stage1.Operation)

    trans (s, m) = map (trans . (s,)) (Map.elems m)

instance Transform Operation where
    type T Operation = (Service, Stage1.Operation)

    trans (s, o) = Operation
        { o2Service       = s2Abbrev s
        , o2Name          = o1Name o
        , o2Namespace     = s2VersionNamespace s <> NS [o1Name o]
        , o2Modules       = modules
        , o2Documentation = trans (o1Documentation o)
        , o2Http          = fromMaybe def (o1Http o)
        , o2Request       = trans o
        , o2Response      = trans o
        }
      where
        modules = sort
            [ "Network.AWS.Data"
            , "Network.AWS.Types"
            , s2TypesNamespace s
            , fromString $ "Network.AWS.Request." ++ show (s2Type s)
            ]

instance ToJSON Operation where
    toJSON = toField (recase Camel Under . drop 2)

newtype Cabal = Cabal [Service]

instance ToJSON Cabal where
    toJSON (Cabal ss) = object
        [ "modules"  .= map service (sort (current ss))
        , "versions" .= map versioned (sort ss)
        ]
      where
        service s = object
            [ "current"  .= s2Namespace s
            , "versions" .= map s2VersionNamespace (sort ss)
            ]

        versioned Service{..} = object
            [ "abbrev"  .= s2Abbrev
            , "version" .= s2Version
            , "modules" .= sort (s2TypesNamespace : map o2Namespace s2Operations)
            ]

env :: ToJSON a => a -> Object
env x = case toJSON x of
    Object o -> o
    e        -> error ("Failed to extract JSON Object from: " ++ show e)
