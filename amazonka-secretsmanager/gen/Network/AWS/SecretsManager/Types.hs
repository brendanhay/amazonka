{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _MalformedPolicyDocumentException,
    _EncryptionFailure,
    _PublicPolicyException,
    _DecryptionFailure,
    _InvalidNextTokenException,
    _PreconditionNotMetException,
    _InvalidRequestException,
    _InvalidParameterException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _ResourceExistsException,
    _InternalServiceError,

    -- * FilterNameStringType
    FilterNameStringType (..),

    -- * SortOrderType
    SortOrderType (..),

    -- * StatusType
    StatusType (..),

    -- * Filter
    Filter (..),
    newFilter,
    filter_key,
    filter_values,

    -- * ReplicaRegionType
    ReplicaRegionType (..),
    newReplicaRegionType,
    replicaRegionType_kmsKeyId,
    replicaRegionType_region,

    -- * ReplicationStatusType
    ReplicationStatusType (..),
    newReplicationStatusType,
    replicationStatusType_statusMessage,
    replicationStatusType_status,
    replicationStatusType_kmsKeyId,
    replicationStatusType_lastAccessedDate,
    replicationStatusType_region,

    -- * RotationRulesType
    RotationRulesType (..),
    newRotationRulesType,
    rotationRulesType_automaticallyAfterDays,

    -- * SecretListEntry
    SecretListEntry (..),
    newSecretListEntry,
    secretListEntry_createdDate,
    secretListEntry_owningService,
    secretListEntry_secretVersionsToStages,
    secretListEntry_lastRotatedDate,
    secretListEntry_arn,
    secretListEntry_kmsKeyId,
    secretListEntry_name,
    secretListEntry_lastChangedDate,
    secretListEntry_primaryRegion,
    secretListEntry_rotationRules,
    secretListEntry_tags,
    secretListEntry_rotationEnabled,
    secretListEntry_deletedDate,
    secretListEntry_rotationLambdaARN,
    secretListEntry_description,
    secretListEntry_lastAccessedDate,

    -- * SecretVersionsListEntry
    SecretVersionsListEntry (..),
    newSecretVersionsListEntry,
    secretVersionsListEntry_createdDate,
    secretVersionsListEntry_versionStages,
    secretVersionsListEntry_versionId,
    secretVersionsListEntry_lastAccessedDate,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * ValidationErrorsEntry
    ValidationErrorsEntry (..),
    newValidationErrorsEntry,
    validationErrorsEntry_checkName,
    validationErrorsEntry_errorMessage,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecretsManager.Types.Filter
import Network.AWS.SecretsManager.Types.FilterNameStringType
import Network.AWS.SecretsManager.Types.ReplicaRegionType
import Network.AWS.SecretsManager.Types.ReplicationStatusType
import Network.AWS.SecretsManager.Types.RotationRulesType
import Network.AWS.SecretsManager.Types.SecretListEntry
import Network.AWS.SecretsManager.Types.SecretVersionsListEntry
import Network.AWS.SecretsManager.Types.SortOrderType
import Network.AWS.SecretsManager.Types.StatusType
import Network.AWS.SecretsManager.Types.Tag
import Network.AWS.SecretsManager.Types.ValidationErrorsEntry
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-10-17@ of the Amazon Secrets Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "SecretsManager",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "secretsmanager",
      Core._serviceSigningName = "secretsmanager",
      Core._serviceVersion = "2017-10-17",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "SecretsManager",
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
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | You provided a resource-based policy with syntax errors.
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyDocumentException"

-- | Secrets Manager can\'t encrypt the protected secret text using the
-- provided KMS key. Check that the customer master key (CMK) is available,
-- enabled, and not in an invalid state. For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>.
_EncryptionFailure :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionFailure =
  Core._MatchServiceError
    defaultService
    "EncryptionFailure"

-- | The BlockPublicPolicy parameter is set to true and the resource policy
-- did not prevent broad access to the secret.
_PublicPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PublicPolicyException =
  Core._MatchServiceError
    defaultService
    "PublicPolicyException"

-- | Secrets Manager can\'t decrypt the protected secret text using the
-- provided KMS key.
_DecryptionFailure :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DecryptionFailure =
  Core._MatchServiceError
    defaultService
    "DecryptionFailure"

-- | You provided an invalid @NextToken@ value.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The request failed because you did not complete all the prerequisite
-- steps.
_PreconditionNotMetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionNotMetException =
  Core._MatchServiceError
    defaultService
    "PreconditionNotMetException"

-- | You provided a parameter value that is not valid for the current state
-- of the resource.
--
-- Possible causes:
--
-- -   You tried to perform the operation on a secret that\'s currently
--     marked deleted.
--
-- -   You tried to enable rotation on a secret that doesn\'t already have
--     a Lambda function ARN configured and you didn\'t include such an ARN
--     as a parameter in this call.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | You provided an invalid value for a parameter.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The request failed because it would exceed one of the Secrets Manager
-- internal limits.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | We can\'t find the resource that you asked for.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | A resource with the ID you requested already exists.
_ResourceExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"

-- | An error occurred on the server side.
_InternalServiceError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"
