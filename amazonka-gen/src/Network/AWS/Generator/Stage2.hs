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

import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Lazy.Builder
import qualified Network.AWS.Generator.Stage1 as Stage1
import           Network.AWS.Generator.Stage1 hiding (Service, Operation)
import           Network.AWS.Generator.Types
import           Text.Shakespeare.Text

transform :: Stage1.Service -> Service
transform = trans

class Transform a where
    type Trans a :: *
    trans :: Trans a -> a

data Service = Service
    { s2Type       :: Type
    , s2Version    :: Text
    , s2Namespace  :: NS
    , s2Abbrev     :: Abbrev
    , s2Operations :: [Operation]
    }

instance Transform Service where
    type Trans Service = Stage1.Service
    trans s = knot
      where
        knot = Service
            { s2Type       = trans s
            , s2Version    = s1ApiVersion s
            , s2Namespace  = trans s
            , s2Abbrev     = trans s
            , s2Operations = trans (knot, s1Operations s)
            }

instance Transform Type where
    type Trans Type = Stage1.Service
    trans Stage1.Service{..}
        | s1SignatureVersion == S3 = RestS3
        | otherwise                = s1Type

data NS = NS { unNS :: [Text] }

root :: NS -> NS
root = NS . reverse . drop 1 . reverse . unNS

instance IsString NS where
    fromString = NS . (:[]) . Text.pack

instance Monoid NS where
    mempty      = NS []
    mappend a b = NS $ unNS a <> unNS b

instance Transform NS where
    type Trans NS = Stage1.Service
    trans s = NS . abbrev $ trans s
      where
        abbrev a =
            [ "Network"
            , "AWS"
            , unAbbrev a
            , Text.replace "-" "_" (s1ApiVersion s)
            , "Types"
            ]

instance ToText NS where
    toText = fromText . Text.intercalate "." . unNS

newtype Abbrev = Abbrev { unAbbrev :: Text }

instance Transform Abbrev where
    type Trans Abbrev = Stage1.Service
    trans = Abbrev
        . mconcat
        . Text.words
        . strip "AWS"
        . strip "Amazon"
        . s1ServiceAbbreviation

instance ToText Abbrev where
    toText = fromText . unAbbrev

data Operation = Operation
    { o2Name      :: Text
    , o2Namespace :: NS
    }

instance Transform [Operation] where
    type Trans [Operation] = (Service, HashMap Text Stage1.Operation)
    trans (s, m) = map (trans . (s,)) (Map.elems m)

instance Transform Operation where
    type Trans Operation = (Service, Stage1.Operation)
    trans (s, o) = Operation (o1Name o) (root (s2Namespace s) <> NS [o1Name o])

strip :: Text -> Text -> Text
strip delim = f Text.stripSuffix . f Text.stripPrefix
  where
    f g x = fromMaybe x $ g delim x
