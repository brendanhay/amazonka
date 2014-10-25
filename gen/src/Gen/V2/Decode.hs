{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

-- Module      : Gen.V2.Decode
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Decode where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.CaseInsensitive      (CI)
import           Data.Default
import           Data.Function
import qualified Data.HashMap.Strict       as Map
import           Data.Maybe
import           Data.Monoid               hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Manipulate
import           Gen.V2.TH

-- Keep the boto json structure completely intact.

data Signature
    = V2
    | V3
    | V4
      deriving (Eq, Show)

nullary dec ''Signature

data Protocol
    = JSON
    | RestJSON
    | RestXML
    | Query
      deriving (Eq, Show)

nullary dec ''Protocol

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Show)

nullary dec ''Timestamp

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show)

nullary dec ''Checksum

data Metadata = Metadata
    { _mServiceAbbreviation :: Text
    , _mServiceFullName     :: Text
    , _mApiVersion          :: Text
    , _mEndpointPrefix      :: Text
    , _mGlobalEndpoint      :: Text
    , _mSignatureVersion    :: !Signature
    , _mXmlNamespace        :: Text
    , _mTargetPrefix        :: Text
    , _mJsonVersion         :: Text
    , _mTimestampFormat     :: !Timestamp
    , _mChecksumFormat      :: !Checksum
    , _mProtocol            :: !Protocol
    } deriving (Eq, Show)

classy dec ''Metadata

data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
    | OPTIONS
      deriving (Eq, Show)

nullary dec ''Method

data HTTP = HTTP
    { _hMethod     :: Method
    , _hRequestUri :: Text
    } deriving (Eq, Show)

record dec ''HTTP

data Ref = Ref
    { _rShape         :: Text
    , _rDocumentation :: Maybe Text
    , _rResultWrapper :: Maybe Text
    } deriving (Eq, Show)

record dec ''Ref

data Error = Error
    { _eShape         :: Text
    , _eDocumentation :: Text
    , _eException     :: !Bool
    } deriving (Eq, Show)

record dec ''Error

data Operation = Operation
    { _oName          :: Text
    , _oDocumentation :: Text
    , _oHttp          :: HTTP
    , _oInput         :: Ref
    , _oOutput        :: Ref
    , _oErrors        :: [Error]
    } deriving (Eq, Show)

record dec ''Operation

data Shape = Shape
    deriving (Eq, Show)

record dec ''Shape

data API = API
    { _apiMetadata      :: Metadata
    , _apiDocumentation :: Text
    , _apiOperations    :: [Operation]
    , _apiShapes        :: [Shape]
    } deriving (Eq, Show)

makeLenses ''API

instance HasMetadata API where
    metadata = apiMetadata

data Paginators = Paginators
data Waiters    = Waiters
