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

import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.String.CaseConversion
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
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

data Service = Service
    { s2Type       :: Type
    , s2Version    :: Text
    , s2Namespace  :: NS
    , s2Abbrev     :: Abbrev
    , s2Operations :: [Operation]
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
            { s2Type       = trans s
            , s2Version    = s1ApiVersion s
            , s2Namespace  = trans s
            , s2Abbrev     = trans s
            , s2Operations = trans (knot, s1Operations s)
            }

instance ToJSON Service where
    toJSON = genericToJSON $ defaultOptions
        { fieldLabelModifier = recase Camel Under . drop 2
        }

instance Transform Type where
    type T Type = Stage1.Service
    trans Stage1.Service{..}
        | s1SignatureVersion == S3 = RestS3
        | otherwise                = s1Type

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

data Operation = Operation
    { o2Name      :: Text
    , o2Namespace :: NS
    } deriving (Eq)

instance Transform [Operation] where
    type T [Operation] = (Service, HashMap Text Stage1.Operation)
    trans (s, m) = map (trans . (s,)) (Map.elems m)

instance Transform Operation where
    type T Operation = (Service, Stage1.Operation)
    trans (s, o) = Operation
        { o2Name      = o1Name o
        , o2Namespace = root (s2Namespace s) <> NS [o1Name o]
        }

instance ToJSON Operation where
    toJSON _ = Null

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
env x =
    case toJSON x of
        Object o -> o
        e        -> error ("Failed to extract JSON Object from: " ++ show e)

strip :: Text -> Text -> Text
strip delim = f Text.stripSuffix . f Text.stripPrefix
  where
    f g x = fromMaybe x $ g delim x
