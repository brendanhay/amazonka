{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , _KMSInternalException
    , _NotFoundException
    , _InvalidAliasNameException
    , _InvalidGrantIdException
    , _InvalidGrantTokenException
    , _InvalidARNException
    , _DependencyTimeoutException
    , _InvalidCiphertextException
    , _AlreadyExistsException
    , _LimitExceededException

    -- * DataKeySpec
    , DataKeySpec (..)

    -- * GrantOperation
    , GrantOperation (..)

    -- * KeyState
    , KeyState (..)

    -- * KeyUsageType
    , KeyUsageType (..)

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
    , kmEnabled
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
    , _svcError = parseJSONError
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
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The request was rejected because the marker that specifies where
-- pagination should next begin is not valid.
_InvalidMarkerException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMarkerException =
    _ServiceError . hasStatus 400 . hasCode "InvalidMarker"

-- | The request was rejected because the state of the specified resource is
-- not valid for this request.
--
-- For more information about how key state affects the use of a customer
-- master key (CMK), go to
-- <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects the Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
_KMSInvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSInvalidStateException =
    _ServiceError . hasStatus 409 . hasCode "KMSInvalidStateException"

-- | The request was rejected because the specified KeySpec parameter is not
-- valid. The currently supported value is ENCRYPT\/DECRYPT.
_InvalidKeyUsageException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidKeyUsageException =
    _ServiceError . hasStatus 400 . hasCode "InvalidKeyUsage"

-- | The request was rejected because the specified policy is not
-- syntactically or semantically correct.
_MalformedPolicyDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
    _ServiceError . hasStatus 400 . hasCode "MalformedPolicyDocument"

-- | The request was rejected because a specified parameter is not supported.
_UnsupportedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedOperation"

-- | The request was rejected because the specified key was marked as
-- disabled.
_DisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_DisabledException = _ServiceError . hasStatus 409 . hasCode "Disabled"

-- | The request was rejected because the key was not available. The request
-- can be retried.
_KeyUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_KeyUnavailableException =
    _ServiceError . hasStatus 500 . hasCode "KeyUnavailable"

-- | The request was rejected because an internal exception occurred. The
-- request can be retried.
_KMSInternalException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSInternalException = _ServiceError . hasStatus 500 . hasCode "KMSInternal"

-- | The request was rejected because the specified entity or resource could
-- not be found.
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _ServiceError . hasStatus 404 . hasCode "NotFound"

-- | The request was rejected because the specified alias name is not valid.
_InvalidAliasNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAliasNameException =
    _ServiceError . hasStatus 400 . hasCode "InvalidAliasName"

-- | The request was rejected because the specified 'GrantId' is not valid.
_InvalidGrantIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGrantIdException =
    _ServiceError . hasStatus 400 . hasCode "InvalidGrantId"

-- | The request was rejected because a grant token provided as part of the
-- request is invalid.
_InvalidGrantTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGrantTokenException =
    _ServiceError . hasStatus 400 . hasCode "InvalidGrantToken"

-- | The request was rejected because a specified ARN was not valid.
_InvalidARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNException = _ServiceError . hasStatus 400 . hasCode "InvalidArn"

-- | The system timed out while trying to fulfill the request. The request
-- can be retried.
_DependencyTimeoutException :: AsError a => Getting (First ServiceError) a ServiceError
_DependencyTimeoutException =
    _ServiceError . hasStatus 503 . hasCode "DependencyTimeout"

-- | The request was rejected because the specified ciphertext has been
-- corrupted or is otherwise invalid.
_InvalidCiphertextException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCiphertextException =
    _ServiceError . hasStatus 400 . hasCode "InvalidCiphertext"

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
    _ServiceError . hasStatus 400 . hasCode "AlreadyExists"

-- | The request was rejected because a limit was exceeded. For more
-- information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/limits.html Limits>
-- in the /AWS Key Management Service Developer Guide/.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceeded"
