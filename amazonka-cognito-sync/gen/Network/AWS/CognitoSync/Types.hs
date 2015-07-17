{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types
    (
    -- * Service
      CognitoSync

    -- * Errors
    , _InvalidParameterException
    , _NotAuthorizedException
    , _InternalErrorException
    , _InvalidConfigurationException
    , _DuplicateRequestException
    , _LambdaThrottledException
    , _AlreadyStreamedException
    , _InvalidLambdaFunctionOutputException
    , _ConcurrentModificationException
    , _TooManyRequestsException
    , _ResourceConflictException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * BulkPublishStatus
    , BulkPublishStatus (..)

    -- * Operation
    , Operation (..)

    -- * Platform
    , Platform (..)

    -- * StreamingStatus
    , StreamingStatus (..)

    -- * CognitoStreams
    , CognitoStreams
    , cognitoStreams
    , csStreamingStatus
    , csStreamName
    , csRoleARN

    -- * Dataset
    , Dataset
    , dataset
    , datLastModifiedDate
    , datNumRecords
    , datDataStorage
    , datDatasetName
    , datCreationDate
    , datLastModifiedBy
    , datIdentityId

    -- * IdentityPoolUsage
    , IdentityPoolUsage
    , identityPoolUsage
    , ipuLastModifiedDate
    , ipuIdentityPoolId
    , ipuDataStorage
    , ipuSyncSessionsCount

    -- * IdentityUsage
    , IdentityUsage
    , identityUsage
    , iuLastModifiedDate
    , iuIdentityPoolId
    , iuDatasetCount
    , iuDataStorage
    , iuIdentityId

    -- * PushSync
    , PushSync
    , pushSync
    , psApplicationARNs
    , psRoleARN

    -- * Record
    , Record
    , record
    , recSyncCount
    , recLastModifiedDate
    , recDeviceLastModifiedDate
    , recValue
    , recKey
    , recLastModifiedBy

    -- * RecordPatch
    , RecordPatch
    , recordPatch
    , rpDeviceLastModifiedDate
    , rpValue
    , rpOp
    , rpKey
    , rpSyncCount
    ) where

import           Network.AWS.CognitoSync.Types.Product
import           Network.AWS.CognitoSync.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-06-30@ of the Amazon Cognito Sync SDK.
data CognitoSync

instance AWSService CognitoSync where
    type Sg CognitoSync = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CognitoSync"
            , _svcPrefix = "cognito-sync"
            , _svcVersion = "2014-06-30"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
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

-- | Thrown when a request parameter does not comply with the associated
-- constraints.
_InvalidParameterException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameter"

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: AWSError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException =
    _ServiceError . hasStatus 403 . hasCode "NotAuthorizedError"

-- | Indicates an internal service error.
_InternalErrorException :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalErrorException =
    _ServiceError . hasStatus 500 . hasCode "InternalError"

-- | Prism for InvalidConfigurationException' errors.
_InvalidConfigurationException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationException =
    _ServiceError . hasStatus 400 . hasCode "InvalidConfiguration"

-- | An exception thrown when there is an IN_PROGRESS bulk publish operation
-- for the given identity pool.
_DuplicateRequestException :: AWSError a => Getting (First ServiceError) a ServiceError
_DuplicateRequestException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateRequest"

-- | AWS Lambda throttled your account, please contact AWS Support
_LambdaThrottledException :: AWSError a => Getting (First ServiceError) a ServiceError
_LambdaThrottledException =
    _ServiceError . hasStatus 429 . hasCode "LambdaThrottled"

-- | An exception thrown when a bulk publish operation is requested less than
-- 24 hours after a previous bulk publish operation completed successfully.
_AlreadyStreamedException :: AWSError a => Getting (First ServiceError) a ServiceError
_AlreadyStreamedException =
    _ServiceError . hasStatus 400 . hasCode "AlreadyStreamed"

-- | The AWS Lambda function returned invalid output or an exception.
_InvalidLambdaFunctionOutputException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidLambdaFunctionOutputException =
    _ServiceError . hasStatus 400 . hasCode "InvalidLambdaFunctionOutput"

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: AWSError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _ServiceError . hasStatus 400 . hasCode "ConcurrentModification"

-- | Thrown if the request is throttled.
_TooManyRequestsException :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _ServiceError . hasStatus 429 . hasCode "TooManyRequests"

-- | Thrown if an update can\'t be applied because the resource was changed
-- by another call and this would result in a conflict.
_ResourceConflictException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException =
    _ServiceError . hasStatus 409 . hasCode "ResourceConflict"

-- | Thrown if the resource doesn\'t exist.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFound"

-- | Thrown when the limit on the number of objects or operations has been
-- exceeded.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceeded"
