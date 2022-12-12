{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecretsManager.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DecryptionFailure,
    _EncryptionFailure,
    _InternalServiceError,
    _InvalidNextTokenException,
    _InvalidParameterException,
    _InvalidRequestException,
    _LimitExceededException,
    _MalformedPolicyDocumentException,
    _PreconditionNotMetException,
    _PublicPolicyException,
    _ResourceExistsException,
    _ResourceNotFoundException,

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
    replicationStatusType_kmsKeyId,
    replicationStatusType_lastAccessedDate,
    replicationStatusType_region,
    replicationStatusType_status,
    replicationStatusType_statusMessage,

    -- * RotationRulesType
    RotationRulesType (..),
    newRotationRulesType,
    rotationRulesType_automaticallyAfterDays,
    rotationRulesType_duration,
    rotationRulesType_scheduleExpression,

    -- * SecretListEntry
    SecretListEntry (..),
    newSecretListEntry,
    secretListEntry_arn,
    secretListEntry_createdDate,
    secretListEntry_deletedDate,
    secretListEntry_description,
    secretListEntry_kmsKeyId,
    secretListEntry_lastAccessedDate,
    secretListEntry_lastChangedDate,
    secretListEntry_lastRotatedDate,
    secretListEntry_name,
    secretListEntry_owningService,
    secretListEntry_primaryRegion,
    secretListEntry_rotationEnabled,
    secretListEntry_rotationLambdaARN,
    secretListEntry_rotationRules,
    secretListEntry_secretVersionsToStages,
    secretListEntry_tags,

    -- * SecretVersionsListEntry
    SecretVersionsListEntry (..),
    newSecretVersionsListEntry,
    secretVersionsListEntry_createdDate,
    secretVersionsListEntry_kmsKeyIds,
    secretVersionsListEntry_lastAccessedDate,
    secretVersionsListEntry_versionId,
    secretVersionsListEntry_versionStages,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecretsManager.Types.Filter
import Amazonka.SecretsManager.Types.FilterNameStringType
import Amazonka.SecretsManager.Types.ReplicaRegionType
import Amazonka.SecretsManager.Types.ReplicationStatusType
import Amazonka.SecretsManager.Types.RotationRulesType
import Amazonka.SecretsManager.Types.SecretListEntry
import Amazonka.SecretsManager.Types.SecretVersionsListEntry
import Amazonka.SecretsManager.Types.SortOrderType
import Amazonka.SecretsManager.Types.StatusType
import Amazonka.SecretsManager.Types.Tag
import Amazonka.SecretsManager.Types.ValidationErrorsEntry
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-10-17@ of the Amazon Secrets Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SecretsManager",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "secretsmanager",
      Core.signingName = "secretsmanager",
      Core.version = "2017-10-17",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SecretsManager",
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

-- | Secrets Manager can\'t decrypt the protected secret text using the
-- provided KMS key.
_DecryptionFailure :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DecryptionFailure =
  Core._MatchServiceError
    defaultService
    "DecryptionFailure"

-- | Secrets Manager can\'t encrypt the protected secret text using the
-- provided KMS key. Check that the KMS key is available, enabled, and not
-- in an invalid state. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key state: Effect on your KMS key>.
_EncryptionFailure :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionFailure =
  Core._MatchServiceError
    defaultService
    "EncryptionFailure"

-- | An error occurred on the server side.
_InternalServiceError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"

-- | The @NextToken@ value is invalid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The parameter name or value is invalid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | A parameter value is not valid for the current state of the resource.
--
-- Possible causes:
--
-- -   The secret is scheduled for deletion.
--
-- -   You tried to enable rotation on a secret that doesn\'t already have
--     a Lambda function ARN configured and you didn\'t include such an ARN
--     as a parameter in this call.
--
-- -   The secret is managed by another service, and you must use that
--     service to update it. For more information, see
--     <https://docs.aws.amazon.com/secretsmanager/latest/userguide/service-linked-secrets.html Secrets managed by other Amazon Web Services services>.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The request failed because it would exceed one of the Secrets Manager
-- quotas.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The resource policy has syntax errors.
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyDocumentException"

-- | The request failed because you did not complete all the prerequisite
-- steps.
_PreconditionNotMetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionNotMetException =
  Core._MatchServiceError
    defaultService
    "PreconditionNotMetException"

-- | The @BlockPublicPolicy@ parameter is set to true, and the resource
-- policy did not prevent broad access to the secret.
_PublicPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PublicPolicyException =
  Core._MatchServiceError
    defaultService
    "PublicPolicyException"

-- | A resource with the ID you requested already exists.
_ResourceExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"

-- | Secrets Manager can\'t find the resource that you asked for.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
