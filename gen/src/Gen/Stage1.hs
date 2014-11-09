{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Gen.Stage1
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Stage1 where

import           Control.Applicative
import           Control.Error
import           Control.Lens        hiding ((<.>), (??))
import           Data.HashMap.Strict (HashMap)
import           Data.Jason          as J
import           Data.Jason.Types
import           Data.Text           (Text)
import qualified Data.Vector         as Vector
import           Gen.IO
import           Gen.JSON
import           Gen.Names
import           Gen.TH
import           Gen.Types
import           System.FilePath
import           System.Directory

default (Text)

data HTTP = HTTP
    { _hMethod     :: !Method
    , _hRequestUri :: URI
    } deriving (Eq, Show)

record stage1 ''HTTP

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
    } deriving (Eq, Show)

record stage1 ''Ref

data Operation = Operation
    { _oName             :: !Text
    , _oDocumentation    :: Maybe Text
    , _oDocumentationUrl :: Maybe Text
    , _oHttp             :: HTTP
    , _oInput            :: Maybe Ref
    , _oOutput           :: Maybe Ref
    , _oErrors           :: Maybe [Ref]
    } deriving (Eq, Show)

record stage1 ''Operation

data XmlNamespace = XmlNamespace
    { _xnsPrefix :: !Text
    , _xnsUri    :: !Text
    } deriving (Eq, Show)

record stage1 ''XmlNamespace

data SList = SList
    { _lstMember        :: Ref
    , _lstDocumentation :: Maybe Text
    , _lstMin           :: Maybe Int
    , _lstMax           :: Maybe Int
    , _lstFlattened     :: Maybe Bool
    , _lstLocationName  :: Maybe Text
    } deriving (Eq, Show)

record stage1 ''SList

data SStruct = SStruct
    { _scRequired      :: Maybe [Text]
    , _scDocumentation :: Maybe Text
    , _scMembers       :: OrdMap Ref
    , _scPayload       :: Maybe Text
    , _scXmlNamespace  :: Maybe XmlNamespace
    , _scException     :: Maybe Bool
    , _scFault         :: Maybe Bool
    } deriving (Eq, Show)

record stage1 ''SStruct

data SMap = SMap
    { _mapKey           :: Ref
    , _mapValue         :: Ref
    , _mapDocumentation :: Maybe Text
    , _mapMin           :: Maybe Int
    , _mapMax           :: Maybe Int
    } deriving (Eq, Show)

record stage1 ''SMap

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

record stage1 ''SString

data SNum a = SNum
    { _numMin           :: Maybe a
    , _numMax           :: Maybe a
    , _numDocumentation :: Maybe Text
    , _numBox           :: Maybe Bool
    } deriving (Eq, Show)

record stage1 ''SNum

data SBool = SBool
    { _blDocumentation :: Maybe Text
    , _blBox           :: Maybe Bool
    } deriving (Eq, Show)

record stage1 ''SBool

data STime = STime
    { _tsTimestampFormat :: Maybe Timestamp
    , _tsDocumentation   :: Maybe Text
    } deriving (Eq, Show)

record stage1 ''STime

data SBlob = SBlob
    { _blbSensitive     :: Maybe Bool
    , _blbDocumentation :: Maybe Text
    } deriving (Eq, Show)

record stage1 ''SBlob

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

data Key
    = Many [Text]
    | One  Text
      deriving (Eq, Show)

instance FromJSON Key where
    parseJSON = \case
        J.String t -> pure (One t)
        J.Array  v -> Many <$> traverse parseJSON (Vector.toList v)
        e          -> fail ("Unknown Pager Key: " ++ show e)

data Pager = Pager
    { _pgMoreResults      :: Maybe Text
    , _pgLimitKey         :: Maybe Text
    , _pgOutputToken      :: Maybe Key
    , _pgInputToken       :: Maybe Key
    , _pgResultkey        :: Maybe Key
    , _pgNonAggregatekeys :: Maybe Key
    } deriving (Eq, Show)

record stage1 ''Pager

data Waiter = Waiter
    { _wOperation    :: Maybe Text
    , _wDescription  :: Maybe Text
    , _wInterval     :: Maybe Int
    , _wMaxAttempts  :: Maybe Int
    , _wAcceptorType :: Maybe Text
    , _wSuccessType  :: Maybe Text
    , _wSuccessPath  :: Maybe Text
    , _wSuccessValue :: Maybe Value
    , _wFailureValue :: Maybe [Text]
    , _wIgnoreErrors :: Maybe [Text]
    , _wExtends      :: Maybe Text
    } deriving (Eq, Show)

record (stage1 & thField .~ keyPython) ''Waiter

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

classy stage1 ''Metadata

data Stage1 = Stage1
    { _s1Metadata          :: Metadata
    , _s1Documentation     :: Text
    , _s1Operations        :: HashMap Text Operation
    , _s1Shapes            :: HashMap Text Shape
    , _s1Pagination        :: HashMap Text Pager
    , _s1Waiters           :: HashMap Text Waiter
    } deriving (Eq, Show)

record stage1 ''Stage1

instance HasMetadata Stage1 where
    metadata = s1Metadata

model :: FilePath -> FilePath -> Script Model
model d o = do
    v  <- version
    m1 <- reqObject override
    m2 <- merge <$> sequence
        [ return m1
        , reqObject (api v)
        , optObject "waiters"    (waiters v)
        , optObject "pagination" (pagers  v)
        ]
    Model name v d m2 <$> hoistEither (parseEither parseJSON (Object m1))
  where
    version = do
        fs <- scriptIO (getDirectoryContents d)
        f  <- tryHead ("Failed to get model version from " ++ d) (filter dots fs)
        return (takeWhile (/= '.') f)

    api     = path "api.json"
    waiters = path "waiters.json"
    pagers  = path "paginators.json"

    path e v = d </> v <.> e

    override = o </> name <.> "json"

    name = takeBaseName (dropTrailingPathSeparator d)

decode :: Model -> Script Stage1
decode Model{..} = do
    say "Decode Stage1" _mPath
    hoistEither (parseEither parseJSON (Object _mModel))
