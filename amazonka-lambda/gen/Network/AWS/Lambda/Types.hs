{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.Lambda.Types
    (
    -- * Service
      Lambda
    -- ** Error
    , JSONError

    -- * Runtime
    , Runtime (..)

    -- * EventSourcePosition
    , EventSourcePosition (..)

    -- * InvocationType
    , InvocationType (..)

    -- * LogType
    , LogType (..)

    -- * FunctionCode
    , FunctionCode
    , functionCode
    , fcZipFile

    -- * FunctionCodeLocation
    , FunctionCodeLocation
    , functionCodeLocation
    , fclLocation
    , fclRepositoryType

    -- * FunctionConfiguration
    , FunctionConfiguration
    , functionConfiguration
    , fcCodeSize
    , fcDescription
    , fcFunctionArn
    , fcFunctionName
    , fcHandler
    , fcLastModified
    , fcMemorySize
    , fcRole
    , fcRuntime
    , fcTimeout

    -- * EventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    , eventSourceMappingConfiguration
    , esmcBatchSize
    , esmcEventSourceArn
    , esmcFunctionArn
    , esmcLastModified
    , esmcLastProcessingResult
    , esmcState
    , esmcStateTransitionReason
    , esmcUUID
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2015-03-31@ of the Amazon Lambda service.
data Lambda

instance AWSService Lambda where
    type Sg Lambda = V4
    type Er Lambda = JSONError

    service = service'
      where
        service' :: Service Lambda
        service' = Service
            { _svcAbbrev       = "Lambda"
            , _svcPrefix       = "lambda"
            , _svcVersion      = "2015-03-31"
            , _svcTargetPrefix = Nothing
            , _svcJSONVersion  = Nothing
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry Lambda
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data Runtime
    = Jvm    -- ^ jvm
    | Nodejs -- ^ nodejs
    | Python -- ^ python
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable Runtime

instance FromText Runtime where
    parser = takeLowerText >>= \case
        "jvm"    -> pure Jvm
        "nodejs" -> pure Nodejs
        "python" -> pure Python
        e        -> fail $
            "Failure parsing Runtime from " ++ show e

instance ToText Runtime where
    toText = \case
        Jvm    -> "jvm"
        Nodejs -> "nodejs"
        Python -> "python"

instance ToByteString Runtime
instance ToHeader     Runtime
instance ToQuery      Runtime

instance FromJSON Runtime where
    parseJSON = parseJSONText "Runtime"

instance ToJSON Runtime where
    toJSON = toJSONText

data EventSourcePosition
    = Latest      -- ^ LATEST
    | TrimHorizon -- ^ TRIM_HORIZON
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable EventSourcePosition

instance FromText EventSourcePosition where
    parser = takeLowerText >>= \case
        "latest"       -> pure Latest
        "trim_horizon" -> pure TrimHorizon
        e              -> fail $
            "Failure parsing EventSourcePosition from " ++ show e

instance ToText EventSourcePosition where
    toText = \case
        Latest      -> "LATEST"
        TrimHorizon -> "TRIM_HORIZON"

instance ToByteString EventSourcePosition
instance ToHeader     EventSourcePosition
instance ToQuery      EventSourcePosition

instance FromJSON EventSourcePosition where
    parseJSON = parseJSONText "EventSourcePosition"

instance ToJSON EventSourcePosition where
    toJSON = toJSONText

data InvocationType
    = DryRun          -- ^ DryRun
    | Event           -- ^ Event
    | RequestResponse -- ^ RequestResponse
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable InvocationType

instance FromText InvocationType where
    parser = takeLowerText >>= \case
        "dryrun"          -> pure DryRun
        "event"           -> pure Event
        "requestresponse" -> pure RequestResponse
        e                 -> fail $
            "Failure parsing InvocationType from " ++ show e

instance ToText InvocationType where
    toText = \case
        DryRun          -> "DryRun"
        Event           -> "Event"
        RequestResponse -> "RequestResponse"

instance ToByteString InvocationType
instance ToHeader     InvocationType
instance ToQuery      InvocationType

instance FromJSON InvocationType where
    parseJSON = parseJSONText "InvocationType"

instance ToJSON InvocationType where
    toJSON = toJSONText

data LogType
    = None  -- ^ None
    | Tail' -- ^ Tail
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable LogType

instance FromText LogType where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "tail" -> pure Tail'
        e      -> fail $
            "Failure parsing LogType from " ++ show e

instance ToText LogType where
    toText = \case
        None  -> "None"
        Tail' -> "Tail"

instance ToByteString LogType
instance ToHeader     LogType
instance ToQuery      LogType

instance FromJSON LogType where
    parseJSON = parseJSONText "LogType"

instance ToJSON LogType where
    toJSON = toJSONText

newtype FunctionCode = FunctionCode
    { _fcZipFile :: Maybe Base64
    } deriving (Eq, Read, Show)

-- | 'FunctionCode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcZipFile' @::@ 'Maybe' 'Base64'
--
functionCode :: FunctionCode
functionCode = FunctionCode
    { _fcZipFile = Nothing
    }

-- | A base64-encoded .zip file containing your packaged source code. For more
-- information about creating a .zip file, go to <http://http://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role.html Execution Permissions> in the /AWS Lambda Developer Guide/.
fcZipFile :: Lens' FunctionCode (Maybe Base64)
fcZipFile = lens _fcZipFile (\s a -> s { _fcZipFile = a })

instance FromJSON FunctionCode where
    parseJSON = withObject "FunctionCode" $ \o -> FunctionCode
        <$> o .:? "ZipFile"

instance ToJSON FunctionCode where
    toJSON FunctionCode{..} = object
        [ "ZipFile" .= _fcZipFile
        ]

data FunctionCodeLocation = FunctionCodeLocation
    { _fclLocation       :: Maybe Text
    , _fclRepositoryType :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

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

-- | The presigned URL you can use to download the function's .zip file that you
-- previously uploaded. The URL is valid for up to 10 minutes.
fclLocation :: Lens' FunctionCodeLocation (Maybe Text)
fclLocation = lens _fclLocation (\s a -> s { _fclLocation = a })

-- | The repository from which you can download the function.
fclRepositoryType :: Lens' FunctionCodeLocation (Maybe Text)
fclRepositoryType =
    lens _fclRepositoryType (\s a -> s { _fclRepositoryType = a })

instance FromJSON FunctionCodeLocation where
    parseJSON = withObject "FunctionCodeLocation" $ \o -> FunctionCodeLocation
        <$> o .:? "Location"
        <*> o .:? "RepositoryType"

instance ToJSON FunctionCodeLocation where
    toJSON FunctionCodeLocation{..} = object
        [ "RepositoryType" .= _fclRepositoryType
        , "Location"       .= _fclLocation
        ]

data FunctionConfiguration = FunctionConfiguration
    { _fcCodeSize     :: Maybe Integer
    , _fcDescription  :: Maybe Text
    , _fcFunctionArn  :: Maybe Text
    , _fcFunctionName :: Maybe Text
    , _fcHandler      :: Maybe Text
    , _fcLastModified :: Maybe Text
    , _fcMemorySize   :: Maybe Nat
    , _fcRole         :: Maybe Text
    , _fcRuntime      :: Maybe Runtime
    , _fcTimeout      :: Maybe Nat
    } deriving (Eq, Read, Show)

-- | 'FunctionConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcCodeSize' @::@ 'Maybe' 'Integer'
--
-- * 'fcDescription' @::@ 'Maybe' 'Text'
--
-- * 'fcFunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'fcFunctionName' @::@ 'Maybe' 'Text'
--
-- * 'fcHandler' @::@ 'Maybe' 'Text'
--
-- * 'fcLastModified' @::@ 'Maybe' 'Text'
--
-- * 'fcMemorySize' @::@ 'Maybe' 'Natural'
--
-- * 'fcRole' @::@ 'Maybe' 'Text'
--
-- * 'fcRuntime' @::@ 'Maybe' 'Runtime'
--
-- * 'fcTimeout' @::@ 'Maybe' 'Natural'
--
functionConfiguration :: FunctionConfiguration
functionConfiguration = FunctionConfiguration
    { _fcFunctionName = Nothing
    , _fcFunctionArn  = Nothing
    , _fcRuntime      = Nothing
    , _fcRole         = Nothing
    , _fcHandler      = Nothing
    , _fcCodeSize     = Nothing
    , _fcDescription  = Nothing
    , _fcTimeout      = Nothing
    , _fcMemorySize   = Nothing
    , _fcLastModified = Nothing
    }

-- | The size, in bytes, of the function .zip file you uploaded.
fcCodeSize :: Lens' FunctionConfiguration (Maybe Integer)
fcCodeSize = lens _fcCodeSize (\s a -> s { _fcCodeSize = a })

-- | The user-provided description.
fcDescription :: Lens' FunctionConfiguration (Maybe Text)
fcDescription = lens _fcDescription (\s a -> s { _fcDescription = a })

-- | The Amazon Resource Name (ARN) assigned to the function.
fcFunctionArn :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionArn = lens _fcFunctionArn (\s a -> s { _fcFunctionArn = a })

-- | The name of the function.
fcFunctionName :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionName = lens _fcFunctionName (\s a -> s { _fcFunctionName = a })

-- | The function Lambda calls to begin executing your function.
fcHandler :: Lens' FunctionConfiguration (Maybe Text)
fcHandler = lens _fcHandler (\s a -> s { _fcHandler = a })

-- | The timestamp of the last time you updated the function.
fcLastModified :: Lens' FunctionConfiguration (Maybe Text)
fcLastModified = lens _fcLastModified (\s a -> s { _fcLastModified = a })

-- | The memory size, in MB, you configured for the function. Must be a multiple
-- of 64 MB.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Natural)
fcMemorySize = lens _fcMemorySize (\s a -> s { _fcMemorySize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when it
-- executes your function to access any other Amazon Web Services (AWS)
-- resources.
fcRole :: Lens' FunctionConfiguration (Maybe Text)
fcRole = lens _fcRole (\s a -> s { _fcRole = a })

-- | The runtime environment for the Lambda function.
fcRuntime :: Lens' FunctionConfiguration (Maybe Runtime)
fcRuntime = lens _fcRuntime (\s a -> s { _fcRuntime = a })

-- | The function execution time at which Lambda should terminate the function.
-- Because the execution time has cost implications, we recommend you set this
-- value based on your expected execution time. The default is 3 seconds.
fcTimeout :: Lens' FunctionConfiguration (Maybe Natural)
fcTimeout = lens _fcTimeout (\s a -> s { _fcTimeout = a }) . mapping _Nat

instance FromJSON FunctionConfiguration where
    parseJSON = withObject "FunctionConfiguration" $ \o -> FunctionConfiguration
        <$> o .:? "CodeSize"
        <*> o .:? "Description"
        <*> o .:? "FunctionArn"
        <*> o .:? "FunctionName"
        <*> o .:? "Handler"
        <*> o .:? "LastModified"
        <*> o .:? "MemorySize"
        <*> o .:? "Role"
        <*> o .:? "Runtime"
        <*> o .:? "Timeout"

instance ToJSON FunctionConfiguration where
    toJSON FunctionConfiguration{..} = object
        [ "FunctionName" .= _fcFunctionName
        , "FunctionArn"  .= _fcFunctionArn
        , "Runtime"      .= _fcRuntime
        , "Role"         .= _fcRole
        , "Handler"      .= _fcHandler
        , "CodeSize"     .= _fcCodeSize
        , "Description"  .= _fcDescription
        , "Timeout"      .= _fcTimeout
        , "MemorySize"   .= _fcMemorySize
        , "LastModified" .= _fcLastModified
        ]

data EventSourceMappingConfiguration = EventSourceMappingConfiguration
    { _esmcBatchSize             :: Maybe Nat
    , _esmcEventSourceArn        :: Maybe Text
    , _esmcFunctionArn           :: Maybe Text
    , _esmcLastModified          :: Maybe POSIX
    , _esmcLastProcessingResult  :: Maybe Text
    , _esmcState                 :: Maybe Text
    , _esmcStateTransitionReason :: Maybe Text
    , _esmcUUID                  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'EventSourceMappingConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esmcBatchSize' @::@ 'Maybe' 'Natural'
--
-- * 'esmcEventSourceArn' @::@ 'Maybe' 'Text'
--
-- * 'esmcFunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'esmcLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'esmcLastProcessingResult' @::@ 'Maybe' 'Text'
--
-- * 'esmcState' @::@ 'Maybe' 'Text'
--
-- * 'esmcStateTransitionReason' @::@ 'Maybe' 'Text'
--
-- * 'esmcUUID' @::@ 'Maybe' 'Text'
--
eventSourceMappingConfiguration :: EventSourceMappingConfiguration
eventSourceMappingConfiguration = EventSourceMappingConfiguration
    { _esmcUUID                  = Nothing
    , _esmcBatchSize             = Nothing
    , _esmcEventSourceArn        = Nothing
    , _esmcFunctionArn           = Nothing
    , _esmcLastModified          = Nothing
    , _esmcLastProcessingResult  = Nothing
    , _esmcState                 = Nothing
    , _esmcStateTransitionReason = Nothing
    }

-- | The largest number of records that AWS Lambda will retrieve from your event
-- source at the time of invoking your function. Your function receives an event
-- with all the retrieved records.
esmcBatchSize :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcBatchSize = lens _esmcBatchSize (\s a -> s { _esmcBatchSize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
esmcEventSourceArn :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcEventSourceArn =
    lens _esmcEventSourceArn (\s a -> s { _esmcEventSourceArn = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the stream.
esmcFunctionArn :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcFunctionArn = lens _esmcFunctionArn (\s a -> s { _esmcFunctionArn = a })

-- | The UTC time string indicating the last time the event mapping was updated.
esmcLastModified :: Lens' EventSourceMappingConfiguration (Maybe UTCTime)
esmcLastModified = lens _esmcLastModified (\s a -> s { _esmcLastModified = a }) . mapping _Time

-- | The result of the last AWS Lambda invocation of your Lambda function.
esmcLastProcessingResult :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcLastProcessingResult =
    lens _esmcLastProcessingResult
        (\s a -> s { _esmcLastProcessingResult = a })

-- | The state of the event source mapping. It can be "Creating", "Enabled",
-- "Disabled", "Enabling", "Disabling", "Updating", or "Deleting".
esmcState :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcState = lens _esmcState (\s a -> s { _esmcState = a })

-- | The reason the event source mapping is in its current state. It is either
-- user-requested or an AWS Lambda-initiated state transition.
esmcStateTransitionReason :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcStateTransitionReason =
    lens _esmcStateTransitionReason
        (\s a -> s { _esmcStateTransitionReason = a })

-- | The AWS Lambda assigned opaque identifier for the mapping.
esmcUUID :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcUUID = lens _esmcUUID (\s a -> s { _esmcUUID = a })

instance FromJSON EventSourceMappingConfiguration where
    parseJSON = withObject "EventSourceMappingConfiguration" $ \o -> EventSourceMappingConfiguration
        <$> o .:? "BatchSize"
        <*> o .:? "EventSourceArn"
        <*> o .:? "FunctionArn"
        <*> o .:? "LastModified"
        <*> o .:? "LastProcessingResult"
        <*> o .:? "State"
        <*> o .:? "StateTransitionReason"
        <*> o .:? "UUID"

instance ToJSON EventSourceMappingConfiguration where
    toJSON EventSourceMappingConfiguration{..} = object
        [ "UUID"                  .= _esmcUUID
        , "BatchSize"             .= _esmcBatchSize
        , "EventSourceArn"        .= _esmcEventSourceArn
        , "FunctionArn"           .= _esmcFunctionArn
        , "LastModified"          .= _esmcLastModified
        , "LastProcessingResult"  .= _esmcLastProcessingResult
        , "State"                 .= _esmcState
        , "StateTransitionReason" .= _esmcStateTransitionReason
        ]
