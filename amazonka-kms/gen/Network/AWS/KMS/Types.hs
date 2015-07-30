{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types
    (
    -- * Service
      KMS

    -- * Errors
    , _InvalidMarkerException
    , _InvalidKeyUsageException
    , _UnsupportedOperationException
    , _MalformedPolicyDocumentException
    , _DisabledException
    , _KeyUnavailableException
    , _KMSInternalException
    , _NotFoundException
    , _InvalidAliasNameException
    , _InvalidARNException
    , _DependencyTimeoutException
    , _InvalidGrantTokenException
    , _InvalidCiphertextException
    , _LimitExceededException
    , _AlreadyExistsException

    -- * DataKeySpec
    , DataKeySpec (..)

    -- * GrantOperation
    , GrantOperation (..)

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
    , gleRetiringPrincipal
    , gleIssuingAccount
    , gleGrantId
    , gleConstraints
    , gleGranteePrincipal
    , gleOperations

    -- * KeyListEntry
    , KeyListEntry
    , keyListEntry
    , kleKeyARN
    , kleKeyId

    -- * KeyMetadata
    , KeyMetadata
    , keyMetadata
    , kmARN
    , kmEnabled
    , kmAWSAccountId
    , kmKeyUsage
    , kmCreationDate
    , kmDescription
    , kmKeyId
    ) where

import           Network.AWS.KMS.Types.Product
import           Network.AWS.KMS.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-11-01@ of the Amazon Key Management Service SDK.
data KMS

instance AWSService KMS where
    type Sg KMS = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "KMS"
            , _svcPrefix = "kms"
            , _svcVersion = "2014-11-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The request was rejected because the marker that specifies where
-- pagination should next begin is not valid.
_InvalidMarkerException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidMarkerException =
    _ServiceError . hasStatus 400 . hasCode "InvalidMarker"

-- | The request was rejected because the specified KeySpec parameter is not
-- valid. The currently supported value is ENCRYPT\/DECRYPT.
_InvalidKeyUsageException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidKeyUsageException =
    _ServiceError . hasStatus 400 . hasCode "InvalidKeyUsage"

-- | The request was rejected because a specified parameter is not supported.
_UnsupportedOperationException :: AWSError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedOperation"

-- | The request was rejected because the specified policy is not
-- syntactically or semantically correct.
_MalformedPolicyDocumentException :: AWSError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
    _ServiceError . hasStatus 400 . hasCode "MalformedPolicyDocument"

-- | A request was rejected because the specified key was marked as disabled.
_DisabledException :: AWSError a => Getting (First ServiceError) a ServiceError
_DisabledException = _ServiceError . hasStatus 409 . hasCode "Disabled"

-- | The request was rejected because the key was disabled, not found, or
-- otherwise not available.
_KeyUnavailableException :: AWSError a => Getting (First ServiceError) a ServiceError
_KeyUnavailableException =
    _ServiceError . hasStatus 500 . hasCode "KeyUnavailable"

-- | The request was rejected because an internal exception occurred. This
-- error can be retried.
_KMSInternalException :: AWSError a => Getting (First ServiceError) a ServiceError
_KMSInternalException = _ServiceError . hasStatus 500 . hasCode "KMSInternal"

-- | The request was rejected because the specified entity or resource could
-- not be found.
_NotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _ServiceError . hasStatus 404 . hasCode "NotFound"

-- | The request was rejected because the specified alias name is not valid.
_InvalidAliasNameException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidAliasNameException =
    _ServiceError . hasStatus 400 . hasCode "InvalidAliasName"

-- | The request was rejected because a specified ARN was not valid.
_InvalidARNException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidARNException = _ServiceError . hasStatus 400 . hasCode "InvalidArn"

-- | The system timed out while trying to fulfill the request.
_DependencyTimeoutException :: AWSError a => Getting (First ServiceError) a ServiceError
_DependencyTimeoutException =
    _ServiceError . hasStatus 503 . hasCode "DependencyTimeout"

-- | A grant token provided as part of the request is invalid.
_InvalidGrantTokenException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidGrantTokenException =
    _ServiceError . hasStatus 400 . hasCode "InvalidGrantToken"

-- | The request was rejected because the specified ciphertext has been
-- corrupted or is otherwise invalid.
_InvalidCiphertextException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCiphertextException =
    _ServiceError . hasStatus 400 . hasCode "InvalidCiphertext"

-- | The request was rejected because a quota was exceeded.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 400 . hasCode "LimitExceeded"

-- | The request was rejected because it attempted to create a resource that
-- already exists.
_AlreadyExistsException :: AWSError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException =
    _ServiceError . hasStatus 400 . hasCode "AlreadyExists"
