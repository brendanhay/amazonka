{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

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

import           Control.Arrow              ((&&&))
import           Control.Lens               ((^.), view)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.Foldable              (foldl')
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid                hiding (Sum)
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Text.Util
import           GHC.Generics
import           Generator.AST
import           Generator.Transform
import           Network.HTTP.Types.Method
import           Text.EDE.Filters

instance ToJSON a => ToJSON (CI a) where
    toJSON = toJSON . CI.original

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
    toJSON = toCtor lowered

instance ToJSON Signature where
    toJSON = toCtor id

instance ToJSON JSONV where
    toJSON = toJSON . unJSONV

instance ToJSON Library where
    toJSON (Library l) = toJSON l

instance ToJSON Cabal where
    toJSON = toField (recase Camel Under . drop 4)

instance ToJSON TypeOverride where
    toJSON = const Null

instance ToJSON FieldOverride where
    toJSON = const Null

instance ToJSON Namespaces where
    toJSON = toField (recase Camel Under . drop 3)

instance ToJSON Service where
    toJSON s = Object (x <> y)
      where
        Object x = toField (recase Camel Under . drop 4) s
        Object y = object ["modules" .= serviceNamespaces s]

instance ToJSON Error where
    toJSON = toField (recase Camel Under . drop 3)

instance ToJSON Operation where
    toJSON = toField (recase Camel Under . drop 3)

instance ToJSON Request where
    toJSON rq@Request{..} = Object (x <> y <> z)
      where
        Object x = toJSON _rqHttp
        Object y = toField (recase Camel Under . drop 3) rq
        Object z = toJSON _rqType

instance ToJSON Style where
    toJSON = toCtor (recase Camel Under . drop 1)

instance ToJSON Response where
    toJSON rs@Response{..} = Object (x <> y)
      where
        Object x = toField (recase Camel Under . drop 3) rs
        Object y = toJSON _rsType

instance ToJSON Location where
    toJSON = toCtor (lowered . drop 1)

instance ToJSON Direction where
    toJSON d = (\(rq, rs) -> object ["request" .= rq, "response" .= rs]) $
        case d of
            DRequest  -> (True, False)
            DResponse -> (False, True)
            DBoth     -> (True, True)

instance ToJSON Common where
    toJSON = toField (recase Camel Under . drop 4)

instance ToJSON Struct
instance ToJSON List
instance ToJSON Map
instance ToJSON Sum
instance ToJSON Prim

instance ToJSON Shape where
    toJSON s = Object (x <> y <> z)
      where
        Object x =
            let f = recase Camel Under . drop 4
             in case s of
                SStruct s' -> toField f s'
                SList   s' -> toField f s'
                SMap    s' -> toField f s'
                SSum    s' -> toField f s'
                SPrim   s' -> toField f s'

        Object y = toJSON (s^.common)
        Object z = object
            [ "default" .= isDefault s
            , "monoid"  .= isMonoid s
            ]

instance ToJSON Primitive where
    toJSON = toCtor (drop 1)

instance ToJSON Ann where
    toJSON a = Object (x <> y)
      where
        Object x = object
            [ "wrapped" .= wtyp
            , "type"    .= typ
            , "strict"  .= if _anStrict a then "!" <> wtyp else typ
            ]
        Object y = toField (recase Camel Under . drop 3) a

        (wtyp, typ) = typeOf a

instance ToJSON Ctor where
    toJSON = toJSON . lowered . drop 1 . show

instance ToJSON Type' where
    toJSON t@Type{..} = Object (x <> y <> z)
      where
        Object x = object
            [ "smart_pad"  .= Text.replicate (Text.length name) " "
            , "smart_ctor" .= smartCtor name
            , "fields"     .= _typFields
            , "payload"    .= _typPayload
            , "params"     .= object params
            , "exhaustive" .= (length params == length _typFields)
            , "headers"    .= _typHeaders
            , "equality"   .= not (any (view cmnStreaming) _typFields)
            ]

        Object y = toJSON (t^.typAnn)
        Object z = toJSON (t^.typShape)

        name = t^.cmnName

        params = foldl' param [] $ zip [1..] _typFields

        param xs (i :: Int, f)
            | f^.cmnRequired = (Text.pack $ show i, toJSON f) : xs
            | otherwise        = xs

instance ToJSON Field where
    toJSON f = Object (x <> y <> z)
      where
        Object x = object
            [ "length"   .= (Text.length prefix + 1)
            , "prefixed" .= prefix
            , "lens"     .= lowerFirst (f^.cmnName)
            ]

        Object y = toJSON (f^.fldAnn)
        Object z = toJSON (_fldCommon f)

        prefix = accessor (_fldPrefixed f)

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

instance ToJSON [QueryPart] where
    toJSON = object . map (_qpKey &&& toJSON . _qpVal)

instance ToJSON Python where
    toJSON = toJSON . go
      where
        go p = case p of
            Empty                -> "(to id)"
            Keyed    k           -> lensPrefix k
            Index  x y           -> "index " <> lensPrefix x <> " " <> go y
            Apply  x (Index y z) -> "index (" <> go (Apply x (Keyed y)) <> ") " <> go z
            Apply  x y           -> lensPrefix x <> " . " <> go y
            Choice x y           -> "choice (" <> go x <> ") (" <> go y <> ")"

instance ToJSON Token where
    toJSON Token{..} = object
        [ "input"         .= _tokInput
        , "output"        .= _tokOutput
        , "output_prefix" .= pref _tokOutput
        ]
      where
        pref Index{}           = False
        pref Choice{}          = False
        pref (Apply _ Index{}) = False
        pref _                 = True

instance ToJSON Pagination where
    toJSON p = object $
        case p of
            Next rk t ->
                [ "type"       .= ("next" :: Text)
                , "result_key" .= rk
                , "token"      .= t
                ]

            More k [t] ->
                [ "type"  .= ("one" :: Text)
                , "more"  .= k
                , "token" .= t
                ]

            More k ts ->
                let f x = (Text.pack ('p' : show x),)
                    m   = Map.fromList (zipWith f [1..length ts] ts)
                    pre = "isNothing " <> Text.intercalate " && isNothing " (Map.keys m)
                 in [ "type"   .= ("many" :: Text)
                    , "more"   .= k
                    , "tokens" .= m
                    , "negate" .= pre
                    ]

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
