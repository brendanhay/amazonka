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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-06-30@ of the Amazon Cognito Sync SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "CognitoSync",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "cognito-sync",
      Prelude._svcVersion = "2014-06-30",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CognitoSync",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Prism for InvalidConfigurationException' errors.
_InvalidConfigurationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidConfigurationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidConfiguration"
    Prelude.. Prelude.hasStatus 400

-- | Thrown if an update can\'t be applied because the resource was changed
-- by another call and this would result in a conflict.
_ResourceConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceConflictException =
  Prelude._MatchServiceError
    defaultService
    "ResourceConflict"
    Prelude.. Prelude.hasStatus 409

-- | Indicates an internal service error.
_InternalErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalError"
    Prelude.. Prelude.hasStatus 500

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentModificationException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentModification"
    Prelude.. Prelude.hasStatus 400

-- | Thrown when a request parameter does not comply with the associated
-- constraints.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameter"
    Prelude.. Prelude.hasStatus 400

-- | AWS Lambda throttled your account, please contact AWS Support
_LambdaThrottledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LambdaThrottledException =
  Prelude._MatchServiceError
    defaultService
    "LambdaThrottled"
    Prelude.. Prelude.hasStatus 429

-- | An exception thrown when a bulk publish operation is requested less than
-- 24 hours after a previous bulk publish operation completed successfully.
_AlreadyStreamedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AlreadyStreamedException =
  Prelude._MatchServiceError
    defaultService
    "AlreadyStreamed"
    Prelude.. Prelude.hasStatus 400

-- | The AWS Lambda function returned invalid output or an exception.
_InvalidLambdaFunctionOutputException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLambdaFunctionOutputException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLambdaFunctionOutput"
    Prelude.. Prelude.hasStatus 400

-- | Thrown when the limit on the number of objects or operations has been
-- exceeded.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceeded"
    Prelude.. Prelude.hasStatus 400

-- | An exception thrown when there is an IN_PROGRESS bulk publish operation
-- for the given identity pool.
_DuplicateRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateRequestException =
  Prelude._MatchServiceError
    defaultService
    "DuplicateRequest"
    Prelude.. Prelude.hasStatus 400

-- | Thrown if the resource doesn\'t exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Prelude.hasStatus 404

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotAuthorizedException =
  Prelude._MatchServiceError
    defaultService
    "NotAuthorizedError"
    Prelude.. Prelude.hasStatus 403

-- | Thrown if the request is throttled.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequests"
    Prelude.. Prelude.hasStatus 429
