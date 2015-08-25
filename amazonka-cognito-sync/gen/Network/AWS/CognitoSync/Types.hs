{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types
    (
    -- * Service Configuration
      cognitoSync

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
    , dLastModifiedDate
    , dNumRecords
    , dDataStorage
    , dDatasetName
    , dCreationDate
    , dLastModifiedBy
    , dIdentityId

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
    , rSyncCount
    , rLastModifiedDate
    , rDeviceLastModifiedDate
    , rValue
    , rKey
    , rLastModifiedBy

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

-- | API version '2014-06-30' of the Amazon Cognito Sync SDK configuration.
cognitoSync :: Service
cognitoSync =
    Service
    { _svcAbbrev = "CognitoSync"
    , _svcSigner = v4
    , _svcPrefix = "cognito-sync"
    , _svcVersion = "2014-06-30"
    , _svcEndpoint = defaultEndpoint cognitoSync
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
    , _svcRetry = retry
    }
  where
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
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameter"

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException =
    _ServiceError . hasStatus 403 . hasCode "NotAuthorizedError"

-- | Indicates an internal service error.
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException =
    _ServiceError . hasStatus 500 . hasCode "InternalError"

-- | Prism for InvalidConfigurationException' errors.
_InvalidConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationException =
    _ServiceError . hasStatus 400 . hasCode "InvalidConfiguration"

-- | An exception thrown when there is an IN_PROGRESS bulk publish operation
-- for the given identity pool.
_DuplicateRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateRequestException =
    _ServiceError . hasStatus 400 . hasCode "DuplicateRequest"

-- | AWS Lambda throttled your account, please contact AWS Support
_LambdaThrottledException :: AsError a => Getting (First ServiceError) a ServiceError
_LambdaThrottledException =
    _ServiceError . hasStatus 429 . hasCode "LambdaThrottled"

-- | An exception thrown when a bulk publish operation is requested less than
-- 24 hours after a previous bulk publish operation completed successfully.
_AlreadyStreamedException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyStreamedException =
    _ServiceError . hasStatus 400 . hasCode "AlreadyStreamed"

-- | The AWS Lambda function returned invalid output or an exception.
_InvalidLambdaFunctionOutputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLambdaFunctionOutputException =
    _ServiceError . hasStatus 400 . hasCode "InvalidLambdaFunctionOutput"

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _ServiceError . hasStatus 400 . hasCode "ConcurrentModification"

-- | Thrown if the request is throttled.
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
    _ServiceError . hasStatus 429 . hasCode "TooManyRequests"

-- | Thrown if an update can\'t be applied because the resource was changed
-- by another call and this would result in a conflict.
_ResourceConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException =
    _ServiceError . hasStatus 409 . hasCode "ResourceConflict"

-- | Thrown if the resource doesn\'t exist.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFound"

-- | Thrown when the limit on the number of objects or operations has been
-- exceeded.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceeded"
