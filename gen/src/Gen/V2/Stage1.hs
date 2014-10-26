{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Module      : Gen.V2.Stage1
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Stage1 where

import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.HashMap.Strict  (HashMap)
import           Data.Jason           as Jason
import           Data.Jason.TH
import           Data.Jason.Types
import           Data.Monoid
import           Data.Text            (Text)
import           Data.Text.Manipulate
import qualified Data.Vector          as Vector
import           Gen.V2.Log
import           Gen.V2.Naming
import           Gen.V2.TH
import           Gen.V2.Types

data Metadata = Metadata
    { _mServiceFullName     :: Text
    , _mServiceAbbreviation :: Maybe Text
    , _mApiVersion          :: Text
    , _mEndpointPrefix      :: Text
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

data HTTP = HTTP
    { _hMethod     :: !Method
    , _hRequestUri :: Text
    } deriving (Eq, Show)

record stage1 ''HTTP

data Location
    = Headers
    | Header
    | URI
    | Querystring
      deriving (Eq, Show)

nullary stage1 ''Location

data Ref = Ref
    { _rShape         :: Text
    , _rDocumentation :: Maybe Text
    , _rResultWrapper :: Maybe Text
    , _rLocation      :: Maybe Location
    , _rLocationName  :: Maybe Text
    , _rStreaming     :: Maybe Bool
    } deriving (Eq, Show)

record stage1 ''Ref

data Error = Error
    { _eShape         :: Text
    , _eDocumentation :: Maybe Text
    , _eException     :: !Bool
    } deriving (Eq, Show)

record stage1 ''Error

data Operation = Operation
    { _oName             :: Text
    , _oDocumentation    :: Maybe Text
    , _oDocumentationUrl :: Maybe Text
    , _oHttp             :: HTTP
    , _oInput            :: Maybe Ref
    , _oOutput           :: Maybe Ref
    , _oErrors           :: Maybe [Error]
    } deriving (Eq, Show)

record stage1 ''Operation

data XmlNamespace = XmlNamespace
    { _xPrefix :: Text
    , _xUri    :: Text
    } deriving (Eq, Show)

record stage1 ''XmlNamespace

-- Need to deserialise errors
data Shape
    = List
      { _sMember          :: Ref
      , _sMin             :: Maybe Int
      , _sMax             :: Maybe Int
      , _sFlattened       :: Maybe Bool
      , _sLocationName    :: Maybe Text
      }

    | Structure
      { _sRequired        :: Maybe [Text]
      , _sDocumentation   :: Maybe Text -- on all?
--      , _sMembers       :: HashMap Text Ref
      , _sPayload         :: Maybe Text
      , _sXmlNamespace    :: Maybe XmlNamespace
      , _sException       :: Maybe Bool
      , _sFault           :: Maybe Bool
      }

    | Map
      { _sKey             :: Ref
      , _sValue           :: Ref
      , _sMin             :: Maybe Int
      , _sMax             :: Maybe Int
      }

    | String
      { _sMin             :: Maybe Int
      , _sMax             :: Maybe Int
      , _sPattern         :: Maybe Text
      , _sEnum            :: Maybe [Text]
      , _sXmlAttribute    :: Maybe Bool
      , _sLocationName    :: Maybe Text
      , _sSensitive       :: Maybe Bool
      }

    | Integer
      { _sMin             :: Maybe Int
      , _sMax             :: Maybe Int
      , _sBox             :: Maybe Bool
      }

    | Long
      { _sMin             :: Maybe Int
      , _sMax             :: Maybe Int
      , _sBox             :: Maybe Bool
      }

    | Double
      { _sMin             :: Maybe Int
      , _sMax             :: Maybe Int
      , _sBox             :: Maybe Bool
      }

    | Float
      { _sMin             :: Maybe Int
      , _sMax             :: Maybe Int
      , _sBox             :: Maybe Bool
      }

    | Boolean
      { _sBox             :: Maybe Bool
      }

    | Timestamp
      { _sTimestampFormat :: Maybe Timestamp
      }

    | Blob
      { _sSensitive       :: Maybe Bool
      }

    deriving (Eq, Show)

record stage1 ''Shape

data Key
    = Many [Text]
    | One  Text
      deriving (Eq, Show)

instance FromJSON Key where
    parseJSON = \case
        Jason.String t -> pure (One t)
        Jason.Array  v -> Many <$> traverse parseJSON (Vector.toList v)
        e              -> fail ("Unknown Pager Key: " ++ show e)

data Pager = Pager
    { _pMoreResults      :: Maybe Text
    , _pLimitKey         :: Maybe Text
    , _pOutputToken      :: Maybe Key
    , _pInputToken       :: Maybe Key
    , _pResultkey        :: Maybe Key
    , _pNonAggregatekeys :: Maybe Key
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

data Stage1 = Stage1
    { _s1Metadata      :: Metadata
    , _s1Documentation :: Maybe Text
    , _s1Operations    :: HashMap Text Operation
    , _s1Shapes        :: HashMap Text Shape
    , _s1Pagination    :: HashMap Text Pager
    , _a1Waiters       :: HashMap Text Waiter
    } deriving (Eq, Show)

record stage1 ''Stage1

instance HasMetadata Stage1 where
    metadata = s1Metadata

decodeS1 :: Model S1 -> Script Stage1
decodeS1 Model{..} = do
    say "Decode Model" _mPath
    hoistEither (parseEither parseJSON (Object _mModel))
