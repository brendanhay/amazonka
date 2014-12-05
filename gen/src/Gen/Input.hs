{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Gen.Input
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Input where

import Control.Applicative
import Control.Lens        hiding ((<.>), (??))
import Data.HashMap.Strict (HashMap)
import Data.Jason
import Data.Text           (Text)
import Gen.TH
import Gen.Types

default (Text)

data HTTP = HTTP
    { _hMethod     :: !Method
    , _hRequestUri :: URI
    } deriving (Eq, Show)

record input ''HTTP

data Ref = Ref
    { _refShape         :: !Text
    , _refDocumentation :: Maybe Text
    , _refLocation      :: Maybe Location
    , _refLocationName  :: Maybe Text
    , _refStreaming     :: Maybe Bool
    , _refException     :: Maybe Bool
    , _refFault         :: Maybe Bool
    , _refResultWrapper :: Maybe Text
    , _refWrapper       :: Maybe Bool
    , _refFlattened     :: Maybe Bool
    } deriving (Eq, Show)

record input ''Ref

data Operation = Operation
    { _oName             :: !Text
    , _oDocumentation    :: Maybe Text
    , _oDocumentationUrl :: Maybe Text
    , _oHttp             :: HTTP
    , _oInput            :: Maybe Ref
    , _oOutput           :: Maybe Ref
    , _oErrors           :: Maybe [Ref]
    } deriving (Eq, Show)

record input ''Operation

data XmlNamespace = XmlNamespace
    { _xnsPrefix :: !Text
    , _xnsUri    :: !Text
    } deriving (Eq, Show)

record input ''XmlNamespace

data SList = SList
    { _lstMember        :: Ref
    , _lstDocumentation :: Maybe Text
    , _lstMin           :: Maybe Int
    , _lstMax           :: Maybe Int
    , _lstFlattened     :: Maybe Bool
    } deriving (Eq, Show)

record input ''SList

data SStruct = SStruct
    { _scRequired      :: Maybe [Text]
    , _scDocumentation :: Maybe Text
    , _scMembers       :: OrdMap Ref
    , _scPayload       :: Maybe Text
    , _scXmlNamespace  :: Maybe XmlNamespace
    , _scException     :: Maybe Bool
    , _scFault         :: Maybe Bool
    } deriving (Eq, Show)

record input ''SStruct

data SMap = SMap
    { _mapKey           :: Ref
    , _mapValue         :: Ref
    , _mapDocumentation :: Maybe Text
    , _mapMin           :: Maybe Int
    , _mapMax           :: Maybe Int
    , _mapFlattened     :: Maybe Bool
    } deriving (Eq, Show)

record input ''SMap

data SString = SString
    { _strMin           :: Maybe Int
    , _strMax           :: Maybe Int
    , _strDocumentation :: Maybe Text
    , _strPattern       :: Maybe Text
    , _strEnum          :: Maybe [Text]
    , _strXmlAttribute  :: Maybe Bool
    , _strLocationName  :: Maybe Text
    , _strSensitive     :: Maybe Bool
    } deriving (Eq, Show)

record input ''SString

data SNum a = SNum
    { _numMin           :: Maybe a
    , _numMax           :: Maybe a
    , _numDocumentation :: Maybe Text
    , _numBox           :: Maybe Bool
    } deriving (Eq, Show)

record input ''SNum

isNatural :: (Ord a, Num a) => SNum a -> Bool
isNatural n
    | Just m <- n ^. numMin = m >= 0
    | otherwise             = False

data SBool = SBool
    { _blDocumentation :: Maybe Text
    , _blBox           :: Maybe Bool
    } deriving (Eq, Show)

record input ''SBool

data STime = STime
    { _tsTimestampFormat :: Maybe Timestamp
    , _tsDocumentation   :: Maybe Text
    } deriving (Eq, Show)

record input ''STime

data SBlob = SBlob
    { _blbSensitive     :: Maybe Bool
    , _blbDocumentation :: Maybe Text
    } deriving (Eq, Show)

record input ''SBlob

-- Need to deserialise errors
data Shape
    = List'   SList
    | Struct' SStruct
    | Map'    SMap
    | String' SString
    | Int'    (SNum Int)
    | Long'   (SNum Integer)
    | Double' (SNum Double)
    | Bool'   SBool
    | Time'   STime
    | Blob'   SBlob
      deriving (Eq, Show)

makePrisms ''Shape

instance FromJSON Shape where
    parseJSON = withObject "shape" $ \o -> do
        let f g = g <$> parseJSON (Object o)
        o .: "type" >>= \case
            "list"      -> f List'
            "structure" -> f Struct'
            "map"       -> f Map'
            "string"    -> f String'
            "integer"   -> f Int'
            "long"      -> f Long'
            "double"    -> f Double'
            "float"     -> f Double'
            "boolean"   -> f Bool'
            "timestamp" -> f Time'
            "blob"      -> f Blob'
            e           -> fail ("Unknown Shape type: " ++ show e)

data Metadata = Metadata
    { _mServiceFullName     :: !Text
    , _mServiceAbbreviation :: !Abbrev
    , _mApiVersion          :: !Text
    , _mEndpointPrefix      :: !Text
    , _mGlobalEndpoint      :: Maybe Text
    , _mSignatureVersion    :: !Signature
    , _mXmlNamespace        :: Maybe Text
    , _mTargetPrefix        :: Maybe Text
    , _mJsonVersion         :: Maybe Text
    , _mTimestampFormat     :: Maybe Timestamp
    , _mChecksumFormat      :: Maybe Checksum
    , _mProtocol            :: !Protocol
    } deriving (Eq, Show)

classy input ''Metadata

data Input = Input
    { _inpMetadata          :: Metadata
    , _inpDocumentation     :: Text
    , _inpOperations        :: HashMap Text Operation
    , _inpShapes            :: HashMap Text Shape
    , _inpPagination        :: HashMap Text (Pager ())
    , _inpWaiters           :: HashMap Text Waiter
    } deriving (Eq, Show)

record input ''Input

instance HasMetadata Input where
    metadata = inpMetadata
