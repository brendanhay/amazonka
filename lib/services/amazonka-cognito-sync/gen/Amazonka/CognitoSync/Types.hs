{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoSync.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AlreadyStreamedException,
    _ConcurrentModificationException,
    _DuplicateRequestException,
    _InternalErrorException,
    _InvalidConfigurationException,
    _InvalidLambdaFunctionOutputException,
    _InvalidParameterException,
    _LambdaThrottledException,
    _LimitExceededException,
    _NotAuthorizedException,
    _ResourceConflictException,
    _ResourceNotFoundException,
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
    dataset_creationDate,
    dataset_dataStorage,
    dataset_datasetName,
    dataset_identityId,
    dataset_lastModifiedBy,
    dataset_lastModifiedDate,
    dataset_numRecords,

    -- * IdentityPoolUsage
    IdentityPoolUsage (..),
    newIdentityPoolUsage,
    identityPoolUsage_dataStorage,
    identityPoolUsage_identityPoolId,
    identityPoolUsage_lastModifiedDate,
    identityPoolUsage_syncSessionsCount,

    -- * IdentityUsage
    IdentityUsage (..),
    newIdentityUsage,
    identityUsage_dataStorage,
    identityUsage_datasetCount,
    identityUsage_identityId,
    identityUsage_identityPoolId,
    identityUsage_lastModifiedDate,

    -- * PushSync
    PushSync (..),
    newPushSync,
    pushSync_applicationArns,
    pushSync_roleArn,

    -- * Record
    Record (..),
    newRecord,
    record_deviceLastModifiedDate,
    record_key,
    record_lastModifiedBy,
    record_lastModifiedDate,
    record_syncCount,
    record_value,

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

import Amazonka.CognitoSync.Types.BulkPublishStatus
import Amazonka.CognitoSync.Types.CognitoStreams
import Amazonka.CognitoSync.Types.Dataset
import Amazonka.CognitoSync.Types.IdentityPoolUsage
import Amazonka.CognitoSync.Types.IdentityUsage
import Amazonka.CognitoSync.Types.Operation
import Amazonka.CognitoSync.Types.Platform
import Amazonka.CognitoSync.Types.PushSync
import Amazonka.CognitoSync.Types.Record
import Amazonka.CognitoSync.Types.RecordPatch
import Amazonka.CognitoSync.Types.StreamingStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-06-30@ of the Amazon Cognito Sync SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CognitoSync",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cognito-sync",
      Core.signingName = "cognito-sync",
      Core.version = "2014-06-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CognitoSync",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An exception thrown when a bulk publish operation is requested less than
-- 24 hours after a previous bulk publish operation completed successfully.
_AlreadyStreamedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AlreadyStreamedException =
  Core._MatchServiceError
    defaultService
    "AlreadyStreamedException"
    Prelude.. Core.hasStatus 400

-- | Thrown if there are parallel requests to modify a resource.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"
    Prelude.. Core.hasStatus 400

-- | An exception thrown when there is an IN_PROGRESS bulk publish operation
-- for the given identity pool.
_DuplicateRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateRequestException =
  Core._MatchServiceError
    defaultService
    "DuplicateRequestException"
    Prelude.. Core.hasStatus 400

-- | Indicates an internal service error.
_InternalErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"
    Prelude.. Core.hasStatus 500

-- | Prism for InvalidConfigurationException' errors.
_InvalidConfigurationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationException"
    Prelude.. Core.hasStatus 400

-- | The AWS Lambda function returned invalid output or an exception.
_InvalidLambdaFunctionOutputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidLambdaFunctionOutputException =
  Core._MatchServiceError
    defaultService
    "InvalidLambdaFunctionOutputException"
    Prelude.. Core.hasStatus 400

-- | Thrown when a request parameter does not comply with the associated
-- constraints.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | AWS Lambda throttled your account, please contact AWS Support
_LambdaThrottledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LambdaThrottledException =
  Core._MatchServiceError
    defaultService
    "LambdaThrottledException"
    Prelude.. Core.hasStatus 429

-- | Thrown when the limit on the number of objects or operations has been
-- exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | Thrown when a user is not authorized to access the requested resource.
_NotAuthorizedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"
    Prelude.. Core.hasStatus 403

-- | Thrown if an update can\'t be applied because the resource was changed
-- by another call and this would result in a conflict.
_ResourceConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | Thrown if the resource doesn\'t exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Thrown if the request is throttled.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
