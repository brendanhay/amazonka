{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Lambda.Types
    (
    -- * Service
      Lambda
    -- ** Error
    , RESTError
    -- ** JSON
    , jsonOptions

    -- * Runtime
    , Runtime (..)

    -- * Mode
    , Mode (..)

    -- * FunctionCodeLocation
    , FunctionCodeLocation
    , functionCodeLocation
    , fclLocation
    , fclRepositoryType

    -- * FunctionConfiguration
    , FunctionConfiguration
    , functionConfiguration
    , fcCodeSize
    , fcConfigurationId
    , fcDescription
    , fcFunctionARN
    , fcFunctionName
    , fcHandler
    , fcLastModified
    , fcMemorySize
    , fcMode
    , fcRole
    , fcRuntime
    , fcTimeout

    -- * EventSourceConfiguration
    , EventSourceConfiguration
    , eventSourceConfiguration
    , escBatchSize
    , escEventSource
    , escFunctionName
    , escIsActive
    , escLastModified
    , escParameters
    , escRole
    , escStatus
    , escUUID
    ) where

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Supported version (@2014-11-11@) of the Amazon Lambda.
data Lambda deriving (Typeable)

instance AWSService Lambda where
    type Sg Lambda = V4
    type Er Lambda = RESTError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "Lambda"
        , _svcPrefix   = "lambda"
        , _svcVersion  = "2014-11-11"
        , _svcTarget   = Nothing
        }

    handle = restError alwaysFail

jsonOptions :: AesonOptions
jsonOptions = defaultOptions
    { fieldLabelModifier = dropWhile (not . isUpper)
    }

data Runtime
    = Nodejs -- ^ nodejs
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable Runtime

instance FromText Runtime where
    parser = match "nodejs" Nodejs

instance ToText Runtime where
    toText Nodejs = "nodejs"

instance FromJSON Runtime where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Runtime where
    toJSON = genericToJSON jsonOptions

data Mode
    = Event -- ^ event
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable Mode

instance FromText Mode where
    parser = match "event" Event

instance ToText Mode where
    toText Event = "event"

instance FromJSON Mode where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Mode where
    toJSON = genericToJSON jsonOptions

data FunctionCodeLocation = FunctionCodeLocation
    { _fclLocation       :: Maybe Text
    , _fclRepositoryType :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'FunctionCodeLocation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fclLocation' @::@ 'Maybe' 'Text'
--
-- * 'fclRepositoryType' @::@ 'Maybe' 'Text'
--
functionCodeLocation :: FunctionCodeLocation
functionCodeLocation = FunctionCodeLocation
    { _fclRepositoryType = Nothing
    , _fclLocation       = Nothing
    }

-- | The presigned URL you can use to download the function's .zip file that
-- you previously uploaded. The URL is valid for up to 10 minutes.
fclLocation :: Lens' FunctionCodeLocation (Maybe Text)
fclLocation = lens _fclLocation (\s a -> s { _fclLocation = a })

-- | The repository from which you can download the function.
fclRepositoryType :: Lens' FunctionCodeLocation (Maybe Text)
fclRepositoryType =
    lens _fclRepositoryType (\s a -> s { _fclRepositoryType = a })

instance FromJSON FunctionCodeLocation where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON FunctionCodeLocation where
    toJSON = genericToJSON jsonOptions

data FunctionConfiguration = FunctionConfiguration
    { _fcCodeSize        :: Maybe Integer
    , _fcConfigurationId :: Maybe Text
    , _fcDescription     :: Maybe Text
    , _fcFunctionARN     :: Maybe Text
    , _fcFunctionName    :: Maybe Text
    , _fcHandler         :: Maybe Text
    , _fcLastModified    :: Maybe RFC822
    , _fcMemorySize      :: Maybe Nat
    , _fcMode            :: Maybe Text
    , _fcRole            :: Maybe Text
    , _fcRuntime         :: Maybe Text
    , _fcTimeout         :: Maybe Nat
    } deriving (Eq, Ord, Show, Generic)

-- | 'FunctionConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcCodeSize' @::@ 'Maybe' 'Integer'
--
-- * 'fcConfigurationId' @::@ 'Maybe' 'Text'
--
-- * 'fcDescription' @::@ 'Maybe' 'Text'
--
-- * 'fcFunctionARN' @::@ 'Maybe' 'Text'
--
-- * 'fcFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'fcHandler' @::@ 'Maybe' 'Text'
--
-- * 'fcLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'fcMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'fcMode' @::@ 'Maybe' 'Text'
--
-- * 'fcRole' @::@ 'Maybe' 'Text'
--
-- * 'fcRuntime' @::@ 'Maybe' 'Text'
--
-- * 'fcTimeout' @::@ 'Maybe' 'Natural'
--
functionConfiguration :: FunctionConfiguration
functionConfiguration = FunctionConfiguration
    { _fcFunctionName    = Nothing
    , _fcFunctionARN     = Nothing
    , _fcConfigurationId = Nothing
    , _fcRuntime         = Nothing
    , _fcRole            = Nothing
    , _fcHandler         = Nothing
    , _fcMode            = Nothing
    , _fcCodeSize        = Nothing
    , _fcDescription     = Nothing
    , _fcTimeout         = Nothing
    , _fcMemorySize      = Nothing
    , _fcLastModified    = Nothing
    }

-- | The size, in bytes, of the function .zip file you uploaded.
fcCodeSize :: Lens' FunctionConfiguration (Maybe Integer)
fcCodeSize = lens _fcCodeSize (\s a -> s { _fcCodeSize = a })

-- | A Lambda-assigned unique identifier for the current function code and
-- related configuration.
fcConfigurationId :: Lens' FunctionConfiguration (Maybe Text)
fcConfigurationId =
    lens _fcConfigurationId (\s a -> s { _fcConfigurationId = a })

-- | The user-provided description.
fcDescription :: Lens' FunctionConfiguration (Maybe Text)
fcDescription = lens _fcDescription (\s a -> s { _fcDescription = a })

-- | The Amazon Resource Name (ARN) assigned to the function.
fcFunctionARN :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionARN = lens _fcFunctionARN (\s a -> s { _fcFunctionARN = a })

-- | The name of the function.
fcFunctionName :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionName = lens _fcFunctionName (\s a -> s { _fcFunctionName = a })

-- | The function Lambda calls to begin executing your function.
fcHandler :: Lens' FunctionConfiguration (Maybe Text)
fcHandler = lens _fcHandler (\s a -> s { _fcHandler = a })

-- | The timestamp of the last time you updated the function.
fcLastModified :: Lens' FunctionConfiguration (Maybe UTCTime)
fcLastModified = lens _fcLastModified (\s a -> s { _fcLastModified = a })
    . mapping _Time

-- | The memory size, in MB, you configured for the function. Must be a
-- multiple of 64 MB.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Natural)
fcMemorySize = lens _fcMemorySize (\s a -> s { _fcMemorySize = a })
    . mapping _Nat

-- | The type of the Lambda function you uploaded.
fcMode :: Lens' FunctionConfiguration (Maybe Text)
fcMode = lens _fcMode (\s a -> s { _fcMode = a })

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when
-- it executes your function to access any other Amazon Web Services (AWS)
-- resources.
fcRole :: Lens' FunctionConfiguration (Maybe Text)
fcRole = lens _fcRole (\s a -> s { _fcRole = a })

-- | The runtime environment for the Lambda function.
fcRuntime :: Lens' FunctionConfiguration (Maybe Text)
fcRuntime = lens _fcRuntime (\s a -> s { _fcRuntime = a })

-- | The function execution time at which Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
fcTimeout :: Lens' FunctionConfiguration (Maybe Natural)
fcTimeout = lens _fcTimeout (\s a -> s { _fcTimeout = a })
    . mapping _Nat

instance FromJSON FunctionConfiguration where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON FunctionConfiguration where
    toJSON = genericToJSON jsonOptions

data EventSourceConfiguration = EventSourceConfiguration
    { _escBatchSize    :: Maybe Int
    , _escEventSource  :: Maybe Text
    , _escFunctionName :: Maybe Text
    , _escIsActive     :: Maybe Bool
    , _escLastModified :: Maybe RFC822
    , _escParameters   :: Map Text Text
    , _escRole         :: Maybe Text
    , _escStatus       :: Maybe Text
    , _escUUID         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'EventSourceConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'escBatchSize' @::@ 'Maybe' 'Int'
--
-- * 'escEventSource' @::@ 'Maybe' 'Text'
--
-- * 'escFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'escIsActive' @::@ 'Maybe' 'Bool'
--
-- * 'escLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'escParameters' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'escRole' @::@ 'Maybe' 'Text'
--
-- * 'escStatus' @::@ 'Maybe' 'Text'
--
-- * 'escUUID' @::@ 'Maybe' 'Text'
--
eventSourceConfiguration :: EventSourceConfiguration
eventSourceConfiguration = EventSourceConfiguration
    { _escUUID         = Nothing
    , _escBatchSize    = Nothing
    , _escEventSource  = Nothing
    , _escFunctionName = Nothing
    , _escParameters   = mempty
    , _escRole         = Nothing
    , _escLastModified = Nothing
    , _escIsActive     = Nothing
    , _escStatus       = Nothing
    }

-- | The largest number of records that AWS Lambda will POST in the invocation
-- request to your function.
escBatchSize :: Lens' EventSourceConfiguration (Maybe Int)
escBatchSize = lens _escBatchSize (\s a -> s { _escBatchSize = a })

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
escEventSource :: Lens' EventSourceConfiguration (Maybe Text)
escEventSource = lens _escEventSource (\s a -> s { _escEventSource = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the
-- stream.
escFunctionName :: Lens' EventSourceConfiguration (Maybe Text)
escFunctionName = lens _escFunctionName (\s a -> s { _escFunctionName = a })

-- | Indicates whether the event source mapping is currently honored. Events
-- are only processes if IsActive is true.
escIsActive :: Lens' EventSourceConfiguration (Maybe Bool)
escIsActive = lens _escIsActive (\s a -> s { _escIsActive = a })

-- | The UTC time string indicating the last time the event mapping was
-- updated.
escLastModified :: Lens' EventSourceConfiguration (Maybe UTCTime)
escLastModified = lens _escLastModified (\s a -> s { _escLastModified = a })
    . mapping _Time

-- | The map (key-value pairs) defining the configuration for AWS Lambda to
-- use when reading the event source.
escParameters :: Lens' EventSourceConfiguration (HashMap Text Text)
escParameters = lens _escParameters (\s a -> s { _escParameters = a })
    . _Map

-- | The ARN of the IAM role (invocation role) that AWS Lambda can assume to
-- read from the stream and invoke the function.
escRole :: Lens' EventSourceConfiguration (Maybe Text)
escRole = lens _escRole (\s a -> s { _escRole = a })

-- | The description of the health of the event source mapping. Valid values
-- are: "PENDING", "OK", and "PROBLEM:message". Initially this staus is
-- "PENDING". When AWS Lambda begins processing events, it changes the
-- status to "OK".
escStatus :: Lens' EventSourceConfiguration (Maybe Text)
escStatus = lens _escStatus (\s a -> s { _escStatus = a })

-- | The AWS Lambda assigned opaque identifier for the mapping.
escUUID :: Lens' EventSourceConfiguration (Maybe Text)
escUUID = lens _escUUID (\s a -> s { _escUUID = a })

instance FromJSON EventSourceConfiguration where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON EventSourceConfiguration where
    toJSON = genericToJSON jsonOptions
