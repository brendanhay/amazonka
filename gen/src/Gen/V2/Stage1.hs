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

data HTTP = HTTP
    { _hMethod     :: !Method
    , _hRequestUri :: URI
    } deriving (Eq, Show)

record stage1 ''HTTP

data Ref = Ref
    { _refShape         :: Text
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
    { _oName             :: Text
    , _oDocumentation    :: Maybe Text
    , _oDocumentationUrl :: Maybe Text
    , _oHttp             :: HTTP
    , _oInput            :: Maybe Ref
    , _oOutput           :: Maybe Ref
    , _oErrors           :: Maybe [Ref]
    } deriving (Eq, Show)

record stage1 ''Operation

data XmlNamespace = XmlNamespace
    { _xnsPrefix :: Text
    , _xnsUri    :: Text
    } deriving (Eq, Show)

record stage1 ''XmlNamespace

-- Need to deserialise errors
data Shape
    = List
      { _shpMember          :: Ref
      , _shpDocumentation   :: Maybe Text
      , _shpMin             :: Maybe Int
      , _shpMax             :: Maybe Int
      , _shpFlattened       :: Maybe Bool
      , _shpLocationName    :: Maybe Text
      }

    | Structure
      { _shpRequired        :: Maybe [Text]
      , _shpDocumentation   :: Maybe Text
      , _shpMembers         :: OrdMap Ref
        -- ^ FIXME: Use Jason's assoc list to ensure ordering
      , _shpPayload         :: Maybe Text
      , _shpXmlNamespace    :: Maybe XmlNamespace
      , _shpException       :: Maybe Bool
      , _shpFault           :: Maybe Bool
      }

    | Map
      { _shpKey             :: Ref
      , _shpValue           :: Ref
      , _shpDocumentation   :: Maybe Text
      , _shpMin             :: Maybe Int
      , _shpMax             :: Maybe Int
      }

    | String
      { _shpMin             :: Maybe Int
      , _shpMax             :: Maybe Int
      , _shpDocumentation   :: Maybe Text
      , _shpPattern         :: Maybe Text
      , _shpEnum            :: Maybe [Text]
      , _shpXmlAttribute    :: Maybe Bool
      , _shpLocationName    :: Maybe Text
      , _shpSensitive       :: Maybe Bool
      }

    | Integer
      { _shpMin             :: Maybe Int
      , _shpMax             :: Maybe Int
      , _shpDocumentation   :: Maybe Text
      , _shpBox             :: Maybe Bool
      }

    | Long
      { _shpMin             :: Maybe Int
      , _shpMax             :: Maybe Int
      , _shpDocumentation   :: Maybe Text
      , _shpBox             :: Maybe Bool
      }

    | Double
      { _shpMin             :: Maybe Int
      , _shpMax             :: Maybe Int
      , _shpDocumentation   :: Maybe Text
      , _shpBox             :: Maybe Bool
      }

    | Float
      { _shpMin             :: Maybe Int
      , _shpMax             :: Maybe Int
      , _shpDocumentation   :: Maybe Text
      , _shpBox             :: Maybe Bool
      }

    | Boolean
      { _shpDocumentation   :: Maybe Text
      , _shpBox             :: Maybe Bool
      }

    | Timestamp
      { _shpTimestampFormat :: Maybe Timestamp
      , _shpDocumentation   :: Maybe Text
      }

    | Blob
      { _shpSensitive       :: Maybe Bool
      , _shpDocumentation   :: Maybe Text
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
