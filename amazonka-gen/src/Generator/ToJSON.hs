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
import qualified Data.Aeson                 as Aeson
import           Data.Aeson                 hiding (String)
import           Data.Aeson.Types           hiding (String)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Char
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Unsafe           as Text
import           GHC.Generics
import           Generator.AST
import           Network.HTTP.Types.Method
import           Text.EDE.Filters

instance ToJSON Abbrev where
    toJSON = toJSON . unAbbrev

instance ToJSON NS where
    toJSON = toJSON . Text.intercalate "." . unNS

instance ToJSON Version where
    toJSON = toJSON . unVersion

instance ToJSON Doc where
    toJSON = toJSON . unDoc

instance ToJSON Time where
    toJSON = toCtor lowered

instance ToJSON Checksum where
    toJSON = toCtor lowered

instance ToJSON ServiceType where
    toJSON = toCtor (recase Camel Under)

instance ToJSON Signature where
    toJSON = toCtor lowered

instance ToJSON JSONV where
    toJSON = toJSON . unJSONV

instance ToJSON Cabal where
    toJSON (Cabal ss) = object
        [ "modules"  .= map service (sort (current ss))
        , "versions" .= map versioned (sort ss)
        ]
      where
        service s = object
            [ "current"  .= _svcNamespace s
            , "versions" .= map _svcVersionNamespace (sort ss)
            ]

        versioned Service{..} = object
            [ "name"    .= _svcName
            , "version" .= _svcVersion
            , "modules" .= (_svcTypesNamespace : sort (map _opNamespace _svcOperations))
            ]

instance ToJSON Service where
    toJSON = toField (recase Camel Under . drop 4)

instance ToJSON Operation where
    toJSON = toField (recase Camel Under . drop 3)

instance ToJSON Request where
    toJSON Request{..} = Object (x <> y)
      where
        Object x = toJSON rqShape
        Object y = toJSON rqHttp

instance ToJSON Response where
    toJSON = toJSON . unResponse

instance ToJSON Location where
    toJSON = toCtor (lowered . drop 1)

instance ToJSON Common

instance ToJSON Shape

instance ToJSON Prim where
    toJSON = toCtor (drop 1)

instance ToJSON StdMethod where
    toJSON = toJSON . Text.decodeUtf8 . renderStdMethod

instance ToJSON HTTP where
    toJSON = toField (recase Camel Under . drop 1)

instance ToJSON PathPart where
    toJSON p = case p of
        PConst c -> f "const" c
        PVar   v -> f "var" v
      where
        f k v = object ["type" .= (k :: Text), "value" .= v]

instance ToJSON QueryPart where
    toJSON = toField (recase Camel Under . drop 2)

instance ToJSON Pagination where
    toJSON = toField (recase Camel Under . drop 2)

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
