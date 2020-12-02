{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SecretsManager.Types
    (
    -- * Service Configuration
      secretsManager

    -- * Errors
    , _MalformedPolicyDocumentException
    , _InvalidParameterException
    , _InvalidRequestException
    , _DecryptionFailure
    , _EncryptionFailure
    , _InvalidNextTokenException
    , _InternalServiceError
    , _ResourceExistsException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * RotationRulesType
    , RotationRulesType
    , rotationRulesType
    , rrtAutomaticallyAfterDays

    -- * SecretListEntry
    , SecretListEntry
    , secretListEntry
    , sleLastChangedDate
    , sleARN
    , sleSecretVersionsToStages
    , sleRotationRules
    , sleDeletedDate
    , sleRotationEnabled
    , sleKMSKeyId
    , sleName
    , sleLastRotatedDate
    , sleLastAccessedDate
    , sleDescription
    , sleRotationLambdaARN
    , sleTags

    -- * SecretVersionsListEntry
    , SecretVersionsListEntry
    , secretVersionsListEntry
    , svleVersionId
    , svleVersionStages
    , svleCreatedDate
    , svleLastAccessedDate

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SecretsManager.Types.Product
import Network.AWS.SecretsManager.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2017-10-17@ of the Amazon Secrets Manager SDK configuration.
secretsManager :: Service
secretsManager =
  Service
    { _svcAbbrev = "SecretsManager"
    , _svcSigner = v4
    , _svcPrefix = "secretsmanager"
    , _svcVersion = "2017-10-17"
    , _svcEndpoint = defaultEndpoint secretsManager
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "SecretsManager"
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
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The policy document that you provided isn't valid.
--
--
_MalformedPolicyDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
  _MatchServiceError secretsManager "MalformedPolicyDocumentException"


-- | You provided an invalid value for a parameter.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError secretsManager "InvalidParameterException"


-- | You provided a parameter value that is not valid for the current state of the resource. For example, if you try to enable rotation on a secret, you must already have a Lambda function ARN configured or included as a parameter in this call.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError secretsManager "InvalidRequestException"


-- | Secrets Manager can't decrypt the protected secret text using the provided KMS key.
--
--
_DecryptionFailure :: AsError a => Getting (First ServiceError) a ServiceError
_DecryptionFailure = _MatchServiceError secretsManager "DecryptionFailure"


-- | Secrets Manager can't encrypt the protected secret text using the provided KMS key. Check that the customer master key (CMK) is available, enabled, and not in an invalid state. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> .
--
--
_EncryptionFailure :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionFailure = _MatchServiceError secretsManager "EncryptionFailure"


-- | You provided an invalid @NextToken@ value.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError secretsManager "InvalidNextTokenException"


-- | An error occurred on the server side.
--
--
_InternalServiceError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceError = _MatchServiceError secretsManager "InternalServiceError"


-- | A resource with the ID you requested already exists.
--
--
_ResourceExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceExistsException =
  _MatchServiceError secretsManager "ResourceExistsException"


-- | We can't find the resource that you asked for.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError secretsManager "ResourceNotFoundException"


-- | The request failed because it would exceed one of the Secrets Manager internal limits.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError secretsManager "LimitExceededException"

