-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidParameterException,
    _NotAuthorizedException,
    _InternalErrorException,
    _InvalidConfigurationException,
    _DuplicateRequestException,
    _LambdaThrottledException,
    _AlreadyStreamedException,
    _InvalidLambdaFunctionOutputException,
    _TooManyRequestsException,
    _ConcurrentModificationException,
    _ResourceConflictException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * IdentityPoolUsage
    IdentityPoolUsage (..),
    mkIdentityPoolUsage,
    ipuDataStorage,
    ipuIdentityPoolId,
    ipuLastModifiedDate,
    ipuSyncSessionsCount,

    -- * Platform
    Platform (..),

    -- * ApplicationArn
    ApplicationArn (..),

    -- * IdentityPoolId
    IdentityPoolId (..),

    -- * Dataset
    Dataset (..),
    mkDataset,
    dCreationDate,
    dDataStorage,
    dDatasetName,
    dIdentityId,
    dLastModifiedBy,
    dLastModifiedDate,
    dNumRecords,

    -- * PushToken
    PushToken (..),

    -- * String
    String (..),

    -- * Operation
    Operation (..),

    -- * AssumeRoleArn
    AssumeRoleArn (..),

    -- * LambdaFunctionArn
    LambdaFunctionArn (..),

    -- * StreamingStatus
    StreamingStatus (..),

    -- * RecordKey
    RecordKey (..),

    -- * SyncSessionToken
    SyncSessionToken (..),

    -- * BulkPublishStatus
    BulkPublishStatus (..),

    -- * DatasetName
    DatasetName (..),

    -- * DeviceId
    DeviceId (..),

    -- * Record
    Record (..),
    mkRecord,
    rDeviceLastModifiedDate,
    rKey,
    rLastModifiedBy,
    rLastModifiedDate,
    rSyncCount,
    rValue,

    -- * CognitoStreams
    CognitoStreams (..),
    mkCognitoStreams,
    csRoleArn,
    csStreamName,
    csStreamingStatus,

    -- * IdentityId
    IdentityId (..),

    -- * IdentityUsage
    IdentityUsage (..),
    mkIdentityUsage,
    iuDataStorage,
    iuDatasetCount,
    iuIdentityId,
    iuIdentityPoolId,
    iuLastModifiedDate,

    -- * RecordPatch
    RecordPatch (..),
    mkRecordPatch,
    rpOp,
    rpKey,
    rpSyncCount,
    rpDeviceLastModifiedDate,
    rpValue,

    -- * CognitoEventType
    CognitoEventType (..),

    -- * StreamName
    StreamName (..),

    -- * ClientContext
    ClientContext (..),

    -- * PushSync
    PushSync (..),
    mkPushSync,
    psApplicationArns,
    psRoleArn,

    -- * NextToken
    NextToken (..),

    -- * LastModifiedBy
    LastModifiedBy (..),

    -- * Value
    Value (..),
  )
where

import Network.AWS.CognitoSync.Types.ApplicationArn
import Network.AWS.CognitoSync.Types.AssumeRoleArn
import Network.AWS.CognitoSync.Types.BulkPublishStatus
import Network.AWS.CognitoSync.Types.ClientContext
import Network.AWS.CognitoSync.Types.CognitoEventType
import Network.AWS.CognitoSync.Types.CognitoStreams
import Network.AWS.CognitoSync.Types.Dataset
import Network.AWS.CognitoSync.Types.DatasetName
import Network.AWS.CognitoSync.Types.DeviceId
import Network.AWS.CognitoSync.Types.IdentityId
import Network.AWS.CognitoSync.Types.IdentityPoolId
import Network.AWS.CognitoSync.Types.IdentityPoolUsage
import Network.AWS.CognitoSync.Types.IdentityUsage
import Network.AWS.CognitoSync.Types.LambdaFunctionArn
import Network.AWS.CognitoSync.Types.LastModifiedBy
import Network.AWS.CognitoSync.Types.NextToken
import Network.AWS.CognitoSync.Types.Operation
import Network.AWS.CognitoSync.Types.Platform
import Network.AWS.CognitoSync.Types.PushSync
import Network.AWS.CognitoSync.Types.PushToken
import Network.AWS.CognitoSync.Types.Record
import Network.AWS.CognitoSync.Types.RecordKey
import Network.AWS.CognitoSync.Types.RecordPatch
import Network.AWS.CognitoSync.Types.StreamName
import Network.AWS.CognitoSync.Types.StreamingStatus
import Network.AWS.CognitoSync.Types.String
import Network.AWS.CognitoSync.Types.SyncSessionToken
import Network.AWS.CognitoSync.Types.Value
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-06-30@ of the Amazon Cognito Sync SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CognitoSync",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "cognito-sync",
      Core._svcVersion = "2014-06-30",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CognitoSync",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Thrown when a request parameter does not comply with the associated constraints.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError mkServiceConfig "InvalidParameter"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError mkServiceConfig "NotAuthorizedError"
    Core.. Core.hasStatues 403
{-# DEPRECATED _NotAuthorizedException "Use generic-lens or generic-optics instead." #-}

-- | Indicates an internal service error.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError mkServiceConfig "InternalError"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalErrorException "Use generic-lens or generic-optics instead." #-}

-- | Prism for 'InvalidConfigurationException' errors.
_InvalidConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationException =
  Core._MatchServiceError mkServiceConfig "InvalidConfiguration"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidConfigurationException "Use generic-lens or generic-optics instead." #-}

-- | An exception thrown when there is an IN_PROGRESS bulk publish operation for the given identity pool.
_DuplicateRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateRequestException =
  Core._MatchServiceError mkServiceConfig "DuplicateRequest"
    Core.. Core.hasStatues 400
{-# DEPRECATED _DuplicateRequestException "Use generic-lens or generic-optics instead." #-}

-- | AWS Lambda throttled your account, please contact AWS Support
_LambdaThrottledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LambdaThrottledException =
  Core._MatchServiceError mkServiceConfig "LambdaThrottled"
    Core.. Core.hasStatues 429
{-# DEPRECATED _LambdaThrottledException "Use generic-lens or generic-optics instead." #-}

-- | An exception thrown when a bulk publish operation is requested less than 24 hours after a previous bulk publish operation completed successfully.
_AlreadyStreamedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyStreamedException =
  Core._MatchServiceError mkServiceConfig "AlreadyStreamed"
    Core.. Core.hasStatues 400
{-# DEPRECATED _AlreadyStreamedException "Use generic-lens or generic-optics instead." #-}

-- | The AWS Lambda function returned invalid output or an exception.
_InvalidLambdaFunctionOutputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLambdaFunctionOutputException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidLambdaFunctionOutput"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidLambdaFunctionOutputException "Use generic-lens or generic-optics instead." #-}

-- | Thrown if the request is throttled.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError mkServiceConfig "TooManyRequests"
    Core.. Core.hasStatues 429
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead." #-}

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError mkServiceConfig "ConcurrentModification"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | Thrown if an update can't be applied because the resource was changed by another call and this would result in a conflict.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError mkServiceConfig "ResourceConflict"
    Core.. Core.hasStatues 409
{-# DEPRECATED _ResourceConflictException "Use generic-lens or generic-optics instead." #-}

-- | Thrown if the resource doesn't exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError mkServiceConfig "ResourceNotFound"
    Core.. Core.hasStatues 404
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Thrown when the limit on the number of objects or operations has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
