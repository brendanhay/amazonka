{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

    -- * Errors
    , _PolicyLengthExceededException
    , _UnsupportedMediaTypeException
    , _InvalidRequestContentException
    , _InvalidParameterValueException
    , _RequestTooLargeException
    , _TooManyRequestsException
    , _ServiceException
    , _CodeStorageExceededException
    , _ResourceConflictException
    , _ResourceNotFoundException

    -- * EventSourcePosition
    , EventSourcePosition (..)

    -- * InvocationType
    , InvocationType (..)

    -- * LogType
    , LogType (..)

    -- * Runtime
    , Runtime (..)

    -- * EventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    , eventSourceMappingConfiguration
    , esmcEventSourceARN
    , esmcFunctionARN
    , esmcState
    , esmcUUID
    , esmcLastProcessingResult
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcLastModified

    -- * FunctionCode
    , FunctionCode
    , functionCode
    , fcS3ObjectVersion
    , fcS3Key
    , fcZipFile
    , fcS3Bucket

    -- * FunctionCodeLocation
    , FunctionCodeLocation
    , functionCodeLocation
    , fclLocation
    , fclRepositoryType

    -- * FunctionConfiguration
    , FunctionConfiguration
    , functionConfiguration
    , fcRuntime
    , fcMemorySize
    , fcFunctionARN
    , fcRole
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcDescription
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2015-03-31@ of the Amazon Lambda SDK.
data Lambda

instance AWSService Lambda where
    type Sg Lambda = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Lambda"
            , _svcPrefix = "lambda"
            , _svcVersion = "2015-03-31"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = 80000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Lambda function access policy is limited to 20 KB.
_PolicyLengthExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_PolicyLengthExceededException =
    _ServiceError . hasStatus 400 . hasCode "PolicyLengthExceededException"

-- | Prism for UnsupportedMediaTypeException' errors.
_UnsupportedMediaTypeException :: AWSError a => Getting (First ServiceError) a ServiceError
_UnsupportedMediaTypeException =
    _ServiceError . hasStatus 415 . hasCode "UnsupportedMediaTypeException"

-- | The request body could not be parsed as JSON.
_InvalidRequestContentException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRequestContentException =
    _ServiceError . hasStatus 400 . hasCode "InvalidRequestContentException"

-- | One of the parameters in the request is invalid. For example, if you
-- provided an IAM role for AWS Lambda to assume in the @CreateFunction@ or
-- the @UpdateFunctionConfiguration@ API, that AWS Lambda is unable to
-- assume you will get this exception.
_InvalidParameterValueException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterValueException"

-- | Prism for RequestTooLargeException' errors.
_RequestTooLargeException :: AWSError a => Getting (First ServiceError) a ServiceError
_RequestTooLargeException =
    _ServiceError . hasStatus 413 . hasCode "RequestTooLargeException"

-- | Prism for TooManyRequestsException' errors.
_TooManyRequestsException :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _ServiceError . hasStatus 429 . hasCode "TooManyRequestsException"

-- | The AWS Lambda service encountered an internal error.
_ServiceException :: AWSError a => Getting (First ServiceError) a ServiceError
_ServiceException = _ServiceError . hasStatus 500 . hasCode "ServiceException"

-- | Prism for CodeStorageExceededException' errors.
_CodeStorageExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_CodeStorageExceededException =
    _ServiceError . hasStatus 400 . hasCode "CodeStorageExceededException"

-- | The resource already exists.
_ResourceConflictException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException =
    _ServiceError . hasStatus 409 . hasCode "ResourceConflictException"

-- | The resource (for example, a Lambda function or access policy statement)
-- specified in the request does not exist.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"

data EventSourcePosition
    = TrimHorizon
    | Latest
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText EventSourcePosition where
    parser = takeLowerText >>= \case
        "LATEST" -> pure Latest
        "TRIM_HORIZON" -> pure TrimHorizon
        e -> fail ("Failure parsing EventSourcePosition from " ++ show e)

instance ToText EventSourcePosition where
    toText = \case
        Latest -> "LATEST"
        TrimHorizon -> "TRIM_HORIZON"

instance Hashable EventSourcePosition
instance ToQuery EventSourcePosition
instance ToHeader EventSourcePosition

instance ToJSON EventSourcePosition where
    toJSON = toJSONText

data InvocationType
    = Event
    | RequestResponse
    | DryRun
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText InvocationType where
    parser = takeLowerText >>= \case
        "DryRun" -> pure DryRun
        "Event" -> pure Event
        "RequestResponse" -> pure RequestResponse
        e -> fail ("Failure parsing InvocationType from " ++ show e)

instance ToText InvocationType where
    toText = \case
        DryRun -> "DryRun"
        Event -> "Event"
        RequestResponse -> "RequestResponse"

instance Hashable InvocationType
instance ToQuery InvocationType
instance ToHeader InvocationType

instance ToJSON InvocationType where
    toJSON = toJSONText

data LogType
    = None
    | Tail
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText LogType where
    parser = takeLowerText >>= \case
        "None" -> pure None
        "Tail" -> pure Tail
        e -> fail ("Failure parsing LogType from " ++ show e)

instance ToText LogType where
    toText = \case
        None -> "None"
        Tail -> "Tail"

instance Hashable LogType
instance ToQuery LogType
instance ToHeader LogType

instance ToJSON LogType where
    toJSON = toJSONText

data Runtime
    = JAVA8
    | Nodejs
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText Runtime where
    parser = takeLowerText >>= \case
        "java8" -> pure JAVA8
        "nodejs" -> pure Nodejs
        e -> fail ("Failure parsing Runtime from " ++ show e)

instance ToText Runtime where
    toText = \case
        JAVA8 -> "java8"
        Nodejs -> "nodejs"

instance Hashable Runtime
instance ToQuery Runtime
instance ToHeader Runtime

instance ToJSON Runtime where
    toJSON = toJSONText

instance FromJSON Runtime where
    parseJSON = parseJSONText "Runtime"

-- | Describes mapping between an Amazon Kinesis stream and a Lambda
-- function.
--
-- /See:/ 'eventSourceMappingConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esmcEventSourceARN'
--
-- * 'esmcFunctionARN'
--
-- * 'esmcState'
--
-- * 'esmcUUID'
--
-- * 'esmcLastProcessingResult'
--
-- * 'esmcBatchSize'
--
-- * 'esmcStateTransitionReason'
--
-- * 'esmcLastModified'
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'
    { _esmcEventSourceARN        :: Maybe Text
    , _esmcFunctionARN           :: Maybe Text
    , _esmcState                 :: Maybe Text
    , _esmcUUID                  :: Maybe Text
    , _esmcLastProcessingResult  :: Maybe Text
    , _esmcBatchSize             :: Maybe Nat
    , _esmcStateTransitionReason :: Maybe Text
    , _esmcLastModified          :: Maybe POSIX
    } deriving (Eq,Read,Show)

-- | 'EventSourceMappingConfiguration' smart constructor.
eventSourceMappingConfiguration :: EventSourceMappingConfiguration
eventSourceMappingConfiguration =
    EventSourceMappingConfiguration'
    { _esmcEventSourceARN = Nothing
    , _esmcFunctionARN = Nothing
    , _esmcState = Nothing
    , _esmcUUID = Nothing
    , _esmcLastProcessingResult = Nothing
    , _esmcBatchSize = Nothing
    , _esmcStateTransitionReason = Nothing
    , _esmcLastModified = Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
esmcEventSourceARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcEventSourceARN = lens _esmcEventSourceARN (\ s a -> s{_esmcEventSourceARN = a});

-- | The Lambda function to invoke when AWS Lambda detects an event on the
-- stream.
esmcFunctionARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcFunctionARN = lens _esmcFunctionARN (\ s a -> s{_esmcFunctionARN = a});

-- | The state of the event source mapping. It can be \"Creating\",
-- \"Enabled\", \"Disabled\", \"Enabling\", \"Disabling\", \"Updating\", or
-- \"Deleting\".
esmcState :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcState = lens _esmcState (\ s a -> s{_esmcState = a});

-- | The AWS Lambda assigned opaque identifier for the mapping.
esmcUUID :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcUUID = lens _esmcUUID (\ s a -> s{_esmcUUID = a});

-- | The result of the last AWS Lambda invocation of your Lambda function.
esmcLastProcessingResult :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcLastProcessingResult = lens _esmcLastProcessingResult (\ s a -> s{_esmcLastProcessingResult = a});

-- | The largest number of records that AWS Lambda will retrieve from your
-- event source at the time of invoking your function. Your function
-- receives an event with all the retrieved records.
esmcBatchSize :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcBatchSize = lens _esmcBatchSize (\ s a -> s{_esmcBatchSize = a}) . mapping _Nat;

-- | The reason the event source mapping is in its current state. It is
-- either user-requested or an AWS Lambda-initiated state transition.
esmcStateTransitionReason :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcStateTransitionReason = lens _esmcStateTransitionReason (\ s a -> s{_esmcStateTransitionReason = a});

-- | The UTC time string indicating the last time the event mapping was
-- updated.
esmcLastModified :: Lens' EventSourceMappingConfiguration (Maybe UTCTime)
esmcLastModified = lens _esmcLastModified (\ s a -> s{_esmcLastModified = a}) . mapping _Time;

instance FromJSON EventSourceMappingConfiguration
         where
        parseJSON
          = withObject "EventSourceMappingConfiguration"
              (\ x ->
                 EventSourceMappingConfiguration' <$>
                   (x .:? "EventSourceArn") <*> (x .:? "FunctionArn")
                     <*> (x .:? "State")
                     <*> (x .:? "UUID")
                     <*> (x .:? "LastProcessingResult")
                     <*> (x .:? "BatchSize")
                     <*> (x .:? "StateTransitionReason")
                     <*> (x .:? "LastModified"))

-- | The code for the Lambda function.
--
-- /See:/ 'functionCode' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcS3ObjectVersion'
--
-- * 'fcS3Key'
--
-- * 'fcZipFile'
--
-- * 'fcS3Bucket'
data FunctionCode = FunctionCode'
    { _fcS3ObjectVersion :: Maybe Text
    , _fcS3Key           :: Maybe Text
    , _fcZipFile         :: Maybe Base64
    , _fcS3Bucket        :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'FunctionCode' smart constructor.
functionCode :: FunctionCode
functionCode =
    FunctionCode'
    { _fcS3ObjectVersion = Nothing
    , _fcS3Key = Nothing
    , _fcZipFile = Nothing
    , _fcS3Bucket = Nothing
    }

-- | The Amazon S3 object (the deployment package) version you want to
-- upload.
fcS3ObjectVersion :: Lens' FunctionCode (Maybe Text)
fcS3ObjectVersion = lens _fcS3ObjectVersion (\ s a -> s{_fcS3ObjectVersion = a});

-- | The Amazon S3 object (the deployment package) key name you want to
-- upload.
fcS3Key :: Lens' FunctionCode (Maybe Text)
fcS3Key = lens _fcS3Key (\ s a -> s{_fcS3Key = a});

-- | A base64-encoded .zip file containing your deployment package. For more
-- information about creating a .zip file, go to
-- <http://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role.html Execution Permissions>
-- in the /AWS Lambda Developer Guide/.
fcZipFile :: Lens' FunctionCode (Maybe Base64)
fcZipFile = lens _fcZipFile (\ s a -> s{_fcZipFile = a});

-- | Amazon S3 bucket name where the .zip file containing your deployment
-- package is stored. This bucket must reside in the same AWS region where
-- you are creating the Lambda function.
fcS3Bucket :: Lens' FunctionCode (Maybe Text)
fcS3Bucket = lens _fcS3Bucket (\ s a -> s{_fcS3Bucket = a});

instance ToJSON FunctionCode where
        toJSON FunctionCode'{..}
          = object
              ["S3ObjectVersion" .= _fcS3ObjectVersion,
               "S3Key" .= _fcS3Key, "ZipFile" .= _fcZipFile,
               "S3Bucket" .= _fcS3Bucket]

-- | The object for the Lambda function location.
--
-- /See:/ 'functionCodeLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fclLocation'
--
-- * 'fclRepositoryType'
data FunctionCodeLocation = FunctionCodeLocation'
    { _fclLocation       :: Maybe Text
    , _fclRepositoryType :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'FunctionCodeLocation' smart constructor.
functionCodeLocation :: FunctionCodeLocation
functionCodeLocation =
    FunctionCodeLocation'
    { _fclLocation = Nothing
    , _fclRepositoryType = Nothing
    }

-- | The presigned URL you can use to download the function\'s .zip file that
-- you previously uploaded. The URL is valid for up to 10 minutes.
fclLocation :: Lens' FunctionCodeLocation (Maybe Text)
fclLocation = lens _fclLocation (\ s a -> s{_fclLocation = a});

-- | The repository from which you can download the function.
fclRepositoryType :: Lens' FunctionCodeLocation (Maybe Text)
fclRepositoryType = lens _fclRepositoryType (\ s a -> s{_fclRepositoryType = a});

instance FromJSON FunctionCodeLocation where
        parseJSON
          = withObject "FunctionCodeLocation"
              (\ x ->
                 FunctionCodeLocation' <$>
                   (x .:? "Location") <*> (x .:? "RepositoryType"))

-- | A complex type that describes function metadata.
--
-- /See:/ 'functionConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcRuntime'
--
-- * 'fcMemorySize'
--
-- * 'fcFunctionARN'
--
-- * 'fcRole'
--
-- * 'fcFunctionName'
--
-- * 'fcCodeSize'
--
-- * 'fcHandler'
--
-- * 'fcTimeout'
--
-- * 'fcLastModified'
--
-- * 'fcDescription'
data FunctionConfiguration = FunctionConfiguration'
    { _fcRuntime      :: Maybe Runtime
    , _fcMemorySize   :: Maybe Nat
    , _fcFunctionARN  :: Maybe Text
    , _fcRole         :: Maybe Text
    , _fcFunctionName :: Maybe Text
    , _fcCodeSize     :: Maybe Integer
    , _fcHandler      :: Maybe Text
    , _fcTimeout      :: Maybe Nat
    , _fcLastModified :: Maybe Text
    , _fcDescription  :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'FunctionConfiguration' smart constructor.
functionConfiguration :: FunctionConfiguration
functionConfiguration =
    FunctionConfiguration'
    { _fcRuntime = Nothing
    , _fcMemorySize = Nothing
    , _fcFunctionARN = Nothing
    , _fcRole = Nothing
    , _fcFunctionName = Nothing
    , _fcCodeSize = Nothing
    , _fcHandler = Nothing
    , _fcTimeout = Nothing
    , _fcLastModified = Nothing
    , _fcDescription = Nothing
    }

-- | The runtime environment for the Lambda function.
fcRuntime :: Lens' FunctionConfiguration (Maybe Runtime)
fcRuntime = lens _fcRuntime (\ s a -> s{_fcRuntime = a});

-- | The memory size, in MB, you configured for the function. Must be a
-- multiple of 64 MB.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Natural)
fcMemorySize = lens _fcMemorySize (\ s a -> s{_fcMemorySize = a}) . mapping _Nat;

-- | The Amazon Resource Name (ARN) assigned to the function.
fcFunctionARN :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionARN = lens _fcFunctionARN (\ s a -> s{_fcFunctionARN = a});

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when
-- it executes your function to access any other Amazon Web Services (AWS)
-- resources.
fcRole :: Lens' FunctionConfiguration (Maybe Text)
fcRole = lens _fcRole (\ s a -> s{_fcRole = a});

-- | The name of the function.
fcFunctionName :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionName = lens _fcFunctionName (\ s a -> s{_fcFunctionName = a});

-- | The size, in bytes, of the function .zip file you uploaded.
fcCodeSize :: Lens' FunctionConfiguration (Maybe Integer)
fcCodeSize = lens _fcCodeSize (\ s a -> s{_fcCodeSize = a});

-- | The function Lambda calls to begin executing your function.
fcHandler :: Lens' FunctionConfiguration (Maybe Text)
fcHandler = lens _fcHandler (\ s a -> s{_fcHandler = a});

-- | The function execution time at which Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
fcTimeout :: Lens' FunctionConfiguration (Maybe Natural)
fcTimeout = lens _fcTimeout (\ s a -> s{_fcTimeout = a}) . mapping _Nat;

-- | The timestamp of the last time you updated the function.
fcLastModified :: Lens' FunctionConfiguration (Maybe Text)
fcLastModified = lens _fcLastModified (\ s a -> s{_fcLastModified = a});

-- | The user-provided description.
fcDescription :: Lens' FunctionConfiguration (Maybe Text)
fcDescription = lens _fcDescription (\ s a -> s{_fcDescription = a});

instance FromJSON FunctionConfiguration where
        parseJSON
          = withObject "FunctionConfiguration"
              (\ x ->
                 FunctionConfiguration' <$>
                   (x .:? "Runtime") <*> (x .:? "MemorySize") <*>
                     (x .:? "FunctionArn")
                     <*> (x .:? "Role")
                     <*> (x .:? "FunctionName")
                     <*> (x .:? "CodeSize")
                     <*> (x .:? "Handler")
                     <*> (x .:? "Timeout")
                     <*> (x .:? "LastModified")
                     <*> (x .:? "Description"))
