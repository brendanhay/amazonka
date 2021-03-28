-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SecretsManager.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _MalformedPolicyDocumentException
    , _InvalidParameterException
    , _InvalidRequestException
    , _DecryptionFailure
    , _PublicPolicyException
    , _EncryptionFailure
    , _PreconditionNotMetException
    , _InvalidNextTokenException
    , _InternalServiceError
    , _ResourceExistsException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * NextTokenType
    , NextTokenType (..)

    -- * ExcludeCharactersType
    , ExcludeCharactersType (..)

    -- * SortOrderType
    , SortOrderType (..)

    -- * NameType
    , NameType (..)

    -- * KmsKeyIdType
    , KmsKeyIdType (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * TagKeyType
    , TagKeyType (..)

    -- * NonEmptyResourcePolicyType
    , NonEmptyResourcePolicyType (..)

    -- * SecretListEntry
    , SecretListEntry (..)
    , mkSecretListEntry
    , sleARN
    , sleCreatedDate
    , sleDeletedDate
    , sleDescription
    , sleKmsKeyId
    , sleLastAccessedDate
    , sleLastChangedDate
    , sleLastRotatedDate
    , sleName
    , sleOwningService
    , sleRotationEnabled
    , sleRotationLambdaARN
    , sleRotationRules
    , sleSecretVersionsToStages
    , sleTags

    -- * ClientRequestTokenType
    , ClientRequestTokenType (..)

    -- * SecretStringType
    , SecretStringType (..)

    -- * SecretARNType
    , SecretARNType (..)

    -- * DescriptionType
    , DescriptionType (..)

    -- * RotationLambdaARNType
    , RotationLambdaARNType (..)

    -- * SecretVersionIdType
    , SecretVersionIdType (..)

    -- * ValidationErrorsEntry
    , ValidationErrorsEntry (..)
    , mkValidationErrorsEntry
    , veeCheckName
    , veeErrorMessage

    -- * RandomPasswordType
    , RandomPasswordType (..)

    -- * FilterNameStringType
    , FilterNameStringType (..)

    -- * SecretVersionStageType
    , SecretVersionStageType (..)

    -- * Filter
    , Filter (..)
    , mkFilter
    , fKey
    , fValues

    -- * SecretIdType
    , SecretIdType (..)

    -- * ErrorMessage
    , ErrorMessage (..)

    -- * FilterValueStringType
    , FilterValueStringType (..)

    -- * RotationRulesType
    , RotationRulesType (..)
    , mkRotationRulesType
    , rrtAutomaticallyAfterDays

    -- * SecretVersionsListEntry
    , SecretVersionsListEntry (..)
    , mkSecretVersionsListEntry
    , svleCreatedDate
    , svleLastAccessedDate
    , svleVersionId
    , svleVersionStages

    -- * ARN
    , ARN (..)

    -- * Name
    , Name (..)

    -- * VersionId
    , VersionId (..)

    -- * SecretId
    , SecretId (..)

    -- * VersionStage
    , VersionStage (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * ClientRequestToken
    , ClientRequestToken (..)

    -- * SecretString
    , SecretString (..)

    -- * Description
    , Description (..)

    -- * OwningService
    , OwningService (..)

    -- * RotationLambdaARN
    , RotationLambdaARN (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.SecretsManager.Types.NextTokenType
  
import Network.AWS.SecretsManager.Types.ExcludeCharactersType
  
import Network.AWS.SecretsManager.Types.SortOrderType
  
  
import Network.AWS.SecretsManager.Types.NameType
  
  
  
import Network.AWS.SecretsManager.Types.KmsKeyIdType
  
import Network.AWS.SecretsManager.Types.Tag
  
import Network.AWS.SecretsManager.Types.TagKeyType
  
  
import Network.AWS.SecretsManager.Types.NonEmptyResourcePolicyType
  
import Network.AWS.SecretsManager.Types.SecretListEntry
  
import Network.AWS.SecretsManager.Types.ClientRequestTokenType
  
import Network.AWS.SecretsManager.Types.SecretStringType
  
import Network.AWS.SecretsManager.Types.SecretARNType
  
import Network.AWS.SecretsManager.Types.DescriptionType
  
import Network.AWS.SecretsManager.Types.RotationLambdaARNType
  
  
import Network.AWS.SecretsManager.Types.SecretVersionIdType
  
import Network.AWS.SecretsManager.Types.ValidationErrorsEntry
  
import Network.AWS.SecretsManager.Types.RandomPasswordType
  
  
import Network.AWS.SecretsManager.Types.FilterNameStringType
  
  
  
  
  
import Network.AWS.SecretsManager.Types.SecretVersionStageType
  
import Network.AWS.SecretsManager.Types.Filter
  
import Network.AWS.SecretsManager.Types.SecretIdType
  
import Network.AWS.SecretsManager.Types.ErrorMessage
  
import Network.AWS.SecretsManager.Types.FilterValueStringType
  
  
import Network.AWS.SecretsManager.Types.RotationRulesType
  
import Network.AWS.SecretsManager.Types.SecretVersionsListEntry
  
  
import Network.AWS.SecretsManager.Types.ARN
  
import Network.AWS.SecretsManager.Types.Name
  
import Network.AWS.SecretsManager.Types.VersionId
  
import Network.AWS.SecretsManager.Types.SecretId
  
import Network.AWS.SecretsManager.Types.VersionStage
  
import Network.AWS.SecretsManager.Types.Key
  
import Network.AWS.SecretsManager.Types.Value
  
import Network.AWS.SecretsManager.Types.ClientRequestToken
  
import Network.AWS.SecretsManager.Types.SecretString
  
import Network.AWS.SecretsManager.Types.Description
  
import Network.AWS.SecretsManager.Types.OwningService
  
import Network.AWS.SecretsManager.Types.RotationLambdaARN
  

-- | API version @2017-10-17@ of the Amazon Secrets Manager SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "SecretsManager",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "secretsmanager",
                 Core._svcVersion = "2017-10-17", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "SecretsManager",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The policy document that you provided isn't valid.
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyDocumentException
  = Core._MatchServiceError mkServiceConfig
      "MalformedPolicyDocumentException"
{-# INLINEABLE _MalformedPolicyDocumentException #-}
{-# DEPRECATED _MalformedPolicyDocumentException "Use generic-lens or generic-optics instead"  #-}

-- | You provided an invalid value for a parameter.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | You provided a parameter value that is not valid for the current state of the resource.
--
-- Possible causes:
--
--     * You tried to perform the operation on a secret that's currently marked deleted.
--
--
--     * You tried to enable rotation on a secret that doesn't already have a Lambda function ARN configured and you didn't include such an ARN as a parameter in this call. 
--
--
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException
  = Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# INLINEABLE _InvalidRequestException #-}
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead"  #-}

-- | Secrets Manager can't decrypt the protected secret text using the provided KMS key. 
_DecryptionFailure :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DecryptionFailure
  = Core._MatchServiceError mkServiceConfig "DecryptionFailure"
{-# INLINEABLE _DecryptionFailure #-}
{-# DEPRECATED _DecryptionFailure "Use generic-lens or generic-optics instead"  #-}

-- | The resource policy did not prevent broad access to the secret.
_PublicPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PublicPolicyException
  = Core._MatchServiceError mkServiceConfig "PublicPolicyException"
{-# INLINEABLE _PublicPolicyException #-}
{-# DEPRECATED _PublicPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | Secrets Manager can't encrypt the protected secret text using the provided KMS key. Check that the customer master key (CMK) is available, enabled, and not in an invalid state. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> .
_EncryptionFailure :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EncryptionFailure
  = Core._MatchServiceError mkServiceConfig "EncryptionFailure"
{-# INLINEABLE _EncryptionFailure #-}
{-# DEPRECATED _EncryptionFailure "Use generic-lens or generic-optics instead"  #-}

-- | The request failed because you did not complete all the prerequisite steps.
_PreconditionNotMetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PreconditionNotMetException
  = Core._MatchServiceError mkServiceConfig
      "PreconditionNotMetException"
{-# INLINEABLE _PreconditionNotMetException #-}
{-# DEPRECATED _PreconditionNotMetException "Use generic-lens or generic-optics instead"  #-}

-- | You provided an invalid @NextToken@ value.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidNextTokenException"
{-# INLINEABLE _InvalidNextTokenException #-}
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead"  #-}

-- | An error occurred on the server side.
_InternalServiceError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceError
  = Core._MatchServiceError mkServiceConfig "InternalServiceError"
{-# INLINEABLE _InternalServiceError #-}
{-# DEPRECATED _InternalServiceError "Use generic-lens or generic-optics instead"  #-}

-- | A resource with the ID you requested already exists.
_ResourceExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceExistsException
  = Core._MatchServiceError mkServiceConfig "ResourceExistsException"
{-# INLINEABLE _ResourceExistsException #-}
{-# DEPRECATED _ResourceExistsException "Use generic-lens or generic-optics instead"  #-}

-- | We can't find the resource that you asked for.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The request failed because it would exceed one of the Secrets Manager internal limits.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
