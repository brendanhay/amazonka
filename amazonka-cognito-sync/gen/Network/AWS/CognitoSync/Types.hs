{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidConfigurationException,
    _ResourceConflictException,
    _InternalErrorException,
    _ConcurrentModificationException,
    _InvalidParameterException,
    _LambdaThrottledException,
    _AlreadyStreamedException,
    _InvalidLambdaFunctionOutputException,
    _LimitExceededException,
    _DuplicateRequestException,
    _ResourceNotFoundException,
    _NotAuthorizedException,
    _TooManyRequestsException,

    -- * BulkPublishStatus
    BulkPublishStatus (..),

    -- * Operation
    Operation (..),

    -- * Platform
    Platform (..),

    -- * StreamingStatus
    StreamingStatus (..),

    -- * CognitoStreams
    CognitoStreams (..),
    newCognitoStreams,
    cognitoStreams_roleArn,
    cognitoStreams_streamName,
    cognitoStreams_streamingStatus,

    -- * Dataset
    Dataset (..),
    newDataset,
    dataset_lastModifiedDate,
    dataset_numRecords,
    dataset_creationDate,
    dataset_dataStorage,
    dataset_identityId,
    dataset_lastModifiedBy,
    dataset_datasetName,

    -- * IdentityPoolUsage
    IdentityPoolUsage (..),
    newIdentityPoolUsage,
    identityPoolUsage_lastModifiedDate,
    identityPoolUsage_identityPoolId,
    identityPoolUsage_syncSessionsCount,
    identityPoolUsage_dataStorage,

    -- * IdentityUsage
    IdentityUsage (..),
    newIdentityUsage,
    identityUsage_lastModifiedDate,
    identityUsage_identityPoolId,
    identityUsage_datasetCount,
    identityUsage_dataStorage,
    identityUsage_identityId,

    -- * PushSync
    PushSync (..),
    newPushSync,
    pushSync_roleArn,
    pushSync_applicationArns,

    -- * Record
    Record (..),
    newRecord,
    record_deviceLastModifiedDate,
    record_lastModifiedDate,
    record_key,
    record_syncCount,
    record_value,
    record_lastModifiedBy,

    -- * RecordPatch
    RecordPatch (..),
    newRecordPatch,
    recordPatch_deviceLastModifiedDate,
    recordPatch_value,
    recordPatch_op,
    recordPatch_key,
    recordPatch_syncCount,
  )
where

import Network.AWS.CognitoSync.Types.BulkPublishStatus
import Network.AWS.CognitoSync.Types.CognitoStreams
import Network.AWS.CognitoSync.Types.Dataset
import Network.AWS.CognitoSync.Types.IdentityPoolUsage
import Network.AWS.CognitoSync.Types.IdentityUsage
import Network.AWS.CognitoSync.Types.Operation
import Network.AWS.CognitoSync.Types.Platform
import Network.AWS.CognitoSync.Types.PushSync
import Network.AWS.CognitoSync.Types.Record
import Network.AWS.CognitoSync.Types.RecordPatch
import Network.AWS.CognitoSync.Types.StreamingStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-06-30@ of the Amazon Cognito Sync SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CognitoSync",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cognito-sync",
      Core._serviceSigningName = "cognito-sync",
      Core._serviceVersion = "2014-06-30",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CognitoSync",
      Core._serviceRetry = retry
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | Prism for InvalidConfigurationException' errors.
_InvalidConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidConfiguration"
    Core.. Core.hasStatus 400

-- | Thrown if an update can\'t be applied because the resource was changed
-- by another call and this would result in a conflict.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflict"
    Core.. Core.hasStatus 409

-- | Indicates an internal service error.
_InternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalError"
    Core.. Core.hasStatus 500

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModification"
    Core.. Core.hasStatus 400

-- | Thrown when a request parameter does not comply with the associated
-- constraints.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameter"
    Core.. Core.hasStatus 400

-- | AWS Lambda throttled your account, please contact AWS Support
_LambdaThrottledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LambdaThrottledException =
  Core._MatchServiceError
    defaultService
    "LambdaThrottled"
    Core.. Core.hasStatus 429

-- | An exception thrown when a bulk publish operation is requested less than
-- 24 hours after a previous bulk publish operation completed successfully.
_AlreadyStreamedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyStreamedException =
  Core._MatchServiceError
    defaultService
    "AlreadyStreamed"
    Core.. Core.hasStatus 400

-- | The AWS Lambda function returned invalid output or an exception.
_InvalidLambdaFunctionOutputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLambdaFunctionOutputException =
  Core._MatchServiceError
    defaultService
    "InvalidLambdaFunctionOutput"
    Core.. Core.hasStatus 400

-- | Thrown when the limit on the number of objects or operations has been
-- exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceeded"
    Core.. Core.hasStatus 400

-- | An exception thrown when there is an IN_PROGRESS bulk publish operation
-- for the given identity pool.
_DuplicateRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateRequestException =
  Core._MatchServiceError
    defaultService
    "DuplicateRequest"
    Core.. Core.hasStatus 400

-- | Thrown if the resource doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Core.. Core.hasStatus 404

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedError"
    Core.. Core.hasStatus 403

-- | Thrown if the request is throttled.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequests"
    Core.. Core.hasStatus 429
