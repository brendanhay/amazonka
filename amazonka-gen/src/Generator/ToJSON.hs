{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import           Data.Monoid
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           GHC.Generics
import           Generator.AST
import           Generator.Transform
import           Network.HTTP.Types.Method

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
    toJSON rq = Object (x <> y <> z)
      where
        Object x = toJSON (_rqHttp rq)
        Object y = toField (recase Camel Under . drop 3) rq
        Object z = object ["pad" .= pad]

        pad = Text.replicate (Text.length $ _rqName rq) " "

instance ToJSON Response where
    toJSON = toField (recase Camel Under . drop 3)

instance ToJSON Location where
    toJSON = toCtor (lowered . drop 1)

instance ToJSON Common where
    toJSON = toField (recase Camel Under . drop 4)

instance ToJSON Shape where
    toJSON s = Object (x <> y)
      where
        Object x = toField (recase Camel Under . drop 4) s
        Object y = toJSON (_shpCommon s)

instance ToJSON Prim where
    toJSON = toCtor (drop 1)

instance ToJSON Ann where
    toJSON (Ann True t) = toJSON t
    toJSON (Ann _    t) = toJSON ("Maybe " <> t)

instance ToJSON Field where
    toJSON f = Object (x <> y)
      where
        Object x = toJSON (fldCommon f)
        Object y = object
            [ "type"     .= fldType f
            , "prefixed" .= fldPrefixed f
            ]

instance ToJSON StdMethod where
    toJSON = toJSON . Text.toLower . Text.decodeUtf8 . renderStdMethod

instance ToJSON HTTP where
    toJSON = toField (recase Camel Under . drop 2)

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
    , sumEncoding            = defaultTaggedObject { tagFieldName = "type" }
    }
