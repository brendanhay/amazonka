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

import           Control.Arrow
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.List
import           Data.List.NonEmpty           (NonEmpty(..))
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Maybe
import           Data.Monoid
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

newtype Doc = Doc { unDoc :: NonEmpty Text }
    deriving (Eq, Ord)

instance Transform Doc where
    type T Doc = Maybe Text

    trans (Just x) = Doc (x :| [])
    trans Nothing  = Doc ("FIXME: Pending" :| [])

instance ToJSON Doc where
    toJSON (Doc (x :| xs)) = toJSON $
        "-- | " <> f (normalise x) <> f (concatMap normalise xs)
      where
        f = Text.intercalate "\n-- "

data Service = Service
    { s2Type          :: ServiceType
    , s2Version       :: Text
    , s2Error         :: Text
    , s2Signature     :: Signature
    , s2Namespace     :: NS
    , s2Abbrev        :: Abbrev
    , s2Documentation :: Doc
    , s2Operations    :: [Operation]
    } deriving (Eq, Generic)

instance Ord Service where
    compare a b = f s2Namespace <> f s2Version
      where
        f :: Ord a => (Service -> a) -> Ordering
        f g = compare (g a) (g b)

instance Transform Service where
    type T Service = Stage1.Service

    trans s = knot
      where
        knot = Service
            { s2Type          = trans s
            , s2Version       = s1ApiVersion s
            , s2Error         = unAbbrev (s2Abbrev knot) <> "Error"
            , s2Signature     = trans s
            , s2Namespace     = trans s
            , s2Abbrev        = trans s
            , s2Documentation = trans (s1Documentation s)
            , s2Operations    = trans (knot, s1Operations s)
            }

instance ToJSON Service where
    toJSON = toField (recase Camel Under . drop 2)

data NS = NS { unNS :: [Text] }
    deriving (Eq, Ord)

root :: NS -> NS
root = NS . reverse . drop 1 . reverse . unNS

instance IsString NS where
    fromString = NS . (:[]) . Text.pack

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
            , Text.replace "-" "_" (s1ApiVersion s)
            , "Types"
            ]

instance ToJSON NS where
    toJSON = toJSON . Text.intercalate "." . unNS

newtype Abbrev = Abbrev { unAbbrev :: Text }
    deriving (Eq)

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

data Field = Field
    { f2Name          :: Text
    , f2Required      :: !Bool
    , f2Type          :: Ann
    , f2Location      :: Maybe Text
    , f2LocationName  :: Maybe Text
    , f2Documentation :: Doc
    , f2Payload       :: !Bool
    , f2Streaming     :: !Bool
    } deriving (Eq, Ord, Generic)

instance Transform [Field] where
    type T [Field] = (Text, Stage1.Shape)

    trans (p, s@SStruct{}) = sort . map f . Map.toList $ sFields s
      where
        f (k, v@SPrim{}) = Field
            { f2Name          = p <> k
            , f2Required      = sRequired v
            , f2Type          = trans v
            , f2Location      = sLocation v
            , f2LocationName  = sLocationName v
            , f2Documentation = trans (sDocumentation v)
            , f2Payload       = sPayload v
            , f2Streaming     = sStreaming v
            }

        f (k, v@SEnum{}) = Field
            { f2Name          = p <> k
            , f2Required      = sRequired v
            , f2Type          = trans v
            , f2Location      = sLocation v
            , f2LocationName  = sLocationName v
            , f2Documentation = trans (sDocumentation v)
            , f2Payload       = sPayload v
            , f2Streaming     = sStreaming v
            }

        f (k, v) = Field
            { f2Name          = p <> k
            , f2Required      = sRequired v
            , f2Type          = trans v
            , f2Location      = Nothing
            , f2LocationName  = Nothing
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
        , hQuery = Map.fromList . map (second (p <>)) $ Map.toList (hQuery h)
        }
      where
        f (I t) = I (p <> t)
        f x     = x

data Request = Request
    { rq2Name    :: Text
    , rq2Http    :: HTTP
    , rq2Fields  :: [Field]
    , rq2Headers :: [Field]
    , rq2Payload :: Maybe Field
    } deriving (Eq, Generic)

instance Transform Request where
    type T Request = Stage1.Operation

    trans o = case o1Input o of
        Nothing -> Request name http mempty mempty Nothing
        Just x  ->
            let fs = trans (pre, x)
                hs = filter (\f -> Just "header" == f2Location f) fs
                p  = find f2Payload fs
             in Request name http fs hs p
      where
        http = trans (pre , o1Http o)
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
    , o2Documentation :: Doc
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
        , o2Namespace     = root (s2Namespace s) <> NS [o1Name o]
        , o2Modules       = modules
        , o2Documentation = trans (o1Documentation o)
        , o2Http          = o1Http o
        , o2Request       = trans o
        , o2Response      = trans o
        }
      where
        modules = sort
            [ "Network.AWS.Data"
            , "Network.AWS.Types"
            , s2Namespace s
            , fromString $ "Network.AWS.Request." ++ show (s2Type s)
            ]

instance ToJSON Operation where
    toJSON = toField (recase Camel Under . drop 2)

newtype Cabal = Cabal [Service]

instance ToJSON Cabal where
    toJSON (Cabal ss) = object ["modules" .= map service (sort ss)]
      where
        service Service{..} = object
            [ "abbrev"  .= s2Abbrev
            , "version" .= s2Version
            , "modules" .= sort (s2Namespace : map o2Namespace s2Operations)
            ]

env :: ToJSON a => a -> Object
env x = case toJSON x of
    Object o -> o
    e        -> error ("Failed to extract JSON Object from: " ++ show e)
