{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Generator.ToJSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.ToJSON where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import qualified Data.Aeson           as Aeson
import           Data.Aeson           hiding (String)
import           Data.Aeson.Types     hiding (String)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Unsafe     as Text
import           GHC.Generics
import           Generator.AST
import           Text.EDE.Filters

instance ToJSON Abbrev where
    toJSON = toJSON . unAbbrev

instance ToJSON NS where
    toJSON = toJSON . Text.intercalate "." . unNS

instance ToJSON Version where
    toJSON = toJSON . unVersion

instance ToJSON Cabal where
    toJSON (Cabal ss) = object
        [ "modules"  .= map service (sort (current ss))
        , "versions" .= map versioned (sort ss)
        ]
      where
        service s = object
            [ "current"  .= rootNS (svcNamespace s)
            , "versions" .= map svcNamespace (sort ss)
            ]

        versioned Service{..} = object
            [ "name"    .= svcName
            , "version" .= svcVersion
            , "modules" .= modules
            ]
          where
            modules = typeNS svcNamespace : sort (map opNamespace svcOperations)

toField :: (Generic a, GToJSON (Rep a))
        => (String -> String)
        -> a
        -> Value
toField f = genericToJSON $ defaultOptions { fieldLabelModifier = f }

toCtor :: (Generic a, GToJSON (Rep a))
       => (String -> String)
       -> a
       -> Value
toCtor f = genericToJSON $ defaultOptions
    { constructorTagModifier = f
    , allNullaryToStringTag  = True
    }
