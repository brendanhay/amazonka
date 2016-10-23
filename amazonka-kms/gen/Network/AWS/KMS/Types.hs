{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types
    (
    -- * Service Configuration
      kms

    -- * Errors
    , _InvalidMarkerException
    , _KMSInvalidStateException
    , _InvalidKeyUsageException
    , _MalformedPolicyDocumentException
    , _UnsupportedOperationException
    , _DisabledException
    , _KeyUnavailableException
    , _IncorrectKeyMaterialException
    , _KMSInternalException
    , _InvalidImportTokenException
    , _NotFoundException
    , _InvalidAliasNameException
    , _InvalidGrantIdException
    , _InvalidGrantTokenException
    , _InvalidARNException
    , _DependencyTimeoutException
    , _ExpiredImportTokenException
    , _InvalidCiphertextException
    , _AlreadyExistsException
    , _LimitExceededException

    -- * AlgorithmSpec
    , AlgorithmSpec (..)

    -- * DataKeySpec
    , DataKeySpec (..)

    -- * ExpirationModelType
    , ExpirationModelType (..)

    -- * GrantOperation
    , GrantOperation (..)

    -- * KeyState
    , KeyState (..)

    -- * KeyUsageType
    , KeyUsageType (..)

    -- * OriginType
    , OriginType (..)

    -- * WrappingKeySpec
    , WrappingKeySpec (..)

    -- * AliasListEntry
    , AliasListEntry
    , aliasListEntry
    , aleTargetKeyId
    , aleAliasName
    , aleAliasARN

    -- * GrantConstraints
    , GrantConstraints
    , grantConstraints
    , gcEncryptionContextEquals
    , gcEncryptionContextSubset

    -- * GrantListEntry
    , GrantListEntry
    , grantListEntry
    , gleKeyId
    , gleRetiringPrincipal
    , gleIssuingAccount
    , gleGrantId
    , gleConstraints
    , gleGranteePrincipal
    , gleName
    , gleCreationDate
    , gleOperations

    -- * KeyListEntry
    , KeyListEntry
    , keyListEntry
    , kleKeyId
    , kleKeyARN

    -- * KeyMetadata
    , KeyMetadata
    , keyMetadata
    , kmOrigin
    , kmExpirationModel
    , kmEnabled
    , kmValidTo
    , kmARN
    , kmKeyState
    , kmAWSAccountId
    , kmKeyUsage
    , kmCreationDate
    , kmDeletionDate
    , kmDescription
    , kmKeyId

    -- * ListGrantsResponse
    , ListGrantsResponse
    , listGrantsResponse
    , lgTruncated
    , lgGrants
    , lgNextMarker
    ) where

import           Network.AWS.KMS.Types.Product
import           Network.AWS.KMS.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2014-11-01' of the Amazon Key Management Service SDK configuration.
kms :: Service
kms =
    Service
    { _svcAbbrev = "KMS"
    , _svcSigner = v4
    , _svcPrefix = "kms"
    , _svcVersion = "2014-11-01"
    , _svcEndpoint = defaultEndpoint kms
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "KMS"
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The request was rejected because the marker that specifies where pagination should next begin is not valid.
_InvalidMarkerException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMarkerException = _ServiceError . hasCode "InvalidMarkerException"

-- | The request was rejected because the state of the specified resource is not valid for this request.
--
-- For more information about how key state affects the use of a customer master key (CMK), see <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects the Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/.
_KMSInvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSInvalidStateException = _ServiceError . hasCode "KMSInvalidStateException"

-- | The request was rejected because the specified KeySpec parameter is not valid. The currently supported value is ENCRYPT\/DECRYPT.
_InvalidKeyUsageException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidKeyUsageException = _ServiceError . hasCode "InvalidKeyUsageException"

-- | The request was rejected because the specified policy is not syntactically or semantically correct.
_MalformedPolicyDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
    _ServiceError . hasCode "MalformedPolicyDocumentException"

-- | The request was rejected because a specified parameter is not supported or a specified resource is not valid for this operation.
_UnsupportedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException =
    _ServiceError . hasCode "UnsupportedOperationException"

-- | The request was rejected because the specified key was marked as disabled.
_DisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_DisabledException = _ServiceError . hasCode "DisabledException"

-- | The request was rejected because the key was not available. The request can be retried.
_KeyUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_KeyUnavailableException = _ServiceError . hasCode "KeyUnavailableException"

-- | The request was rejected because the provided key material is invalid or is not the same key material that was previously imported into this customer master key (CMK).
_IncorrectKeyMaterialException :: AsError a => Getting (First ServiceError) a ServiceError
_IncorrectKeyMaterialException =
    _ServiceError . hasCode "IncorrectKeyMaterialException"

-- | The request was rejected because an internal exception occurred. The request can be retried.
_KMSInternalException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSInternalException = _ServiceError . hasCode "KMSInternalException"

-- | The request was rejected because the provided import token is invalid or is associated with a different customer master key (CMK).
_InvalidImportTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidImportTokenException =
    _ServiceError . hasCode "InvalidImportTokenException"

-- | The request was rejected because the specified entity or resource could not be found.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _ServiceError . hasCode "NotFoundException"

-- | The request was rejected because the specified alias name is not valid.
_InvalidAliasNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAliasNameException =
    _ServiceError . hasCode "InvalidAliasNameException"

-- | The request was rejected because the specified 'GrantId' is not valid.
_InvalidGrantIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGrantIdException = _ServiceError . hasCode "InvalidGrantIdException"

-- | The request was rejected because a grant token provided as part of the request is invalid.
_InvalidGrantTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGrantTokenException =
    _ServiceError . hasCode "InvalidGrantTokenException"

-- | The request was rejected because a specified ARN was not valid.
_InvalidARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNException = _ServiceError . hasCode "InvalidArnException"

-- | The system timed out while trying to fulfill the request. The request can be retried.
_DependencyTimeoutException :: AsError a => Getting (First ServiceError) a ServiceError
_DependencyTimeoutException =
    _ServiceError . hasCode "DependencyTimeoutException"

-- | The request was rejected because the provided import token is expired. Use < GetParametersForImport> to retrieve a new import token and public key, use the new public key to encrypt the key material, and then try the request again.
_ExpiredImportTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredImportTokenException =
    _ServiceError . hasCode "ExpiredImportTokenException"

-- | The request was rejected because the specified ciphertext has been corrupted or is otherwise invalid.
_InvalidCiphertextException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCiphertextException =
    _ServiceError . hasCode "InvalidCiphertextException"

-- | The request was rejected because it attempted to create a resource that already exists.
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException = _ServiceError . hasCode "AlreadyExistsException"

-- | The request was rejected because a limit was exceeded. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/limits.html Limits> in the /AWS Key Management Service Developer Guide/.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
