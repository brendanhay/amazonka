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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "SecretsManager",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "secretsmanager",
      Prelude._svcSigningName = "secretsmanager",
      Prelude._svcVersion = "2017-10-17",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "SecretsManager",
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

-- | You provided a resource-based policy with syntax errors.
_MalformedPolicyDocumentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MalformedPolicyDocumentException =
  Prelude._MatchServiceError
    defaultService
    "MalformedPolicyDocumentException"

-- | Secrets Manager can\'t encrypt the protected secret text using the
-- provided KMS key. Check that the customer master key (CMK) is available,
-- enabled, and not in an invalid state. For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>.
_EncryptionFailure :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EncryptionFailure =
  Prelude._MatchServiceError
    defaultService
    "EncryptionFailure"

-- | The BlockPublicPolicy parameter is set to true and the resource policy
-- did not prevent broad access to the secret.
_PublicPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PublicPolicyException =
  Prelude._MatchServiceError
    defaultService
    "PublicPolicyException"

-- | Secrets Manager can\'t decrypt the protected secret text using the
-- provided KMS key.
_DecryptionFailure :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DecryptionFailure =
  Prelude._MatchServiceError
    defaultService
    "DecryptionFailure"

-- | You provided an invalid @NextToken@ value.
_InvalidNextTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The request failed because you did not complete all the prerequisite
-- steps.
_PreconditionNotMetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PreconditionNotMetException =
  Prelude._MatchServiceError
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
_InvalidRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | You provided an invalid value for a parameter.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The request failed because it would exceed one of the Secrets Manager
-- internal limits.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | We can\'t find the resource that you asked for.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | A resource with the ID you requested already exists.
_ResourceExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceExistsException =
  Prelude._MatchServiceError
    defaultService
    "ResourceExistsException"

-- | An error occurred on the server side.
_InternalServiceError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServiceError =
  Prelude._MatchServiceError
    defaultService
    "InternalServiceError"
