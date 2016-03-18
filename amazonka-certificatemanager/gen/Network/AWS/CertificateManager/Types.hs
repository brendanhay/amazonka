{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types
    (
    -- * Service Configuration
      certificateManager

    -- * Errors
    , _InvalidDomainValidationOptionsException
    , _RequestInProgressException
    , _InvalidARNException
    , _ResourceNotFoundException
    , _InvalidStateException
    , _LimitExceededException
    , _ResourceInUseException

    -- * CertificateStatus
    , CertificateStatus (..)

    -- * KeyAlgorithm
    , KeyAlgorithm (..)

    -- * RevocationReason
    , RevocationReason (..)

    -- * CertificateDetail
    , CertificateDetail
    , certificateDetail
    , cdSubject
    , cdStatus
    , cdSubjectAlternativeNames
    , cdInUseBy
    , cdCreatedAt
    , cdCertificateARN
    , cdSerial
    , cdRevokedAt
    , cdNotBefore
    , cdRevocationReason
    , cdDomainName
    , cdKeyAlgorithm
    , cdIssuedAt
    , cdSignatureAlgorithm
    , cdDomainValidationOptions
    , cdIssuer
    , cdNotAfter

    -- * CertificateSummary
    , CertificateSummary
    , certificateSummary
    , csCertificateARN
    , csDomainName

    -- * DomainValidation
    , DomainValidation
    , domainValidation
    , dvValidationEmails
    , dvValidationDomain
    , dvDomainName

    -- * DomainValidationOption
    , DomainValidationOption
    , domainValidationOption
    , dvoDomainName
    , dvoValidationDomain
    ) where

import           Network.AWS.CertificateManager.Types.Product
import           Network.AWS.CertificateManager.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-12-08' of the Amazon Certificate Manager SDK configuration.
certificateManager :: Service
certificateManager =
    Service
    { _svcAbbrev = "CertificateManager"
    , _svcSigner = v4
    , _svcPrefix = "acm"
    , _svcVersion = "2015-12-08"
    , _svcEndpoint = defaultEndpoint certificateManager
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

-- | One or more values in the < DomainValidationOption> structure is
-- incorrect.
_InvalidDomainValidationOptionsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDomainValidationOptionsException =
    _ServiceError . hasCode "InvalidDomainValidationOptionsException"

-- | The certificate request is in process and the certificate in your
-- account has not yet been issued.
_RequestInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestInProgressException =
    _ServiceError . hasCode "RequestInProgressException"

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNException = _ServiceError . hasCode "InvalidArnException"

-- | The specified certificate cannot be found in the caller\'s account, or
-- the caller\'s account cannot be found.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | Processing has reached an invalid state. For example, this exception can
-- occur if the specified domain is not using email validation, or the
-- current certificate status does not permit the requested operation. See
-- the exception message returned by ACM to determine which state is not
-- valid.
_InvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStateException = _ServiceError . hasCode "InvalidStateException"

-- | An ACM limit has been exceeded. For example, you may have input more
-- domains than are allowed or you\'ve requested too many certificates for
-- your account. See the exception message returned by ACM to determine
-- which limit you have violated. For more information about ACM limits,
-- see the
-- <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits>
-- topic.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"

-- | The certificate is in use by another AWS service in the caller\'s
-- account. Remove the association and try again.
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException = _ServiceError . hasCode "ResourceInUseException"
