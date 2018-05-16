{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types
    (
    -- * Service Configuration
      certificateManagerPCA

    -- * Errors
    , _InvalidTagException
    , _MalformedCSRException
    , _RequestAlreadyProcessedException
    , _MalformedCertificateException
    , _RequestFailedException
    , _CertificateMismatchException
    , _TooManyTagsException
    , _InvalidArgsException
    , _RequestInProgressException
    , _ConcurrentModificationException
    , _InvalidNextTokenException
    , _InvalidARNException
    , _InvalidPolicyException
    , _ResourceNotFoundException
    , _InvalidStateException
    , _LimitExceededException

    -- * AuditReportResponseFormat
    , AuditReportResponseFormat (..)

    -- * AuditReportStatus
    , AuditReportStatus (..)

    -- * CertificateAuthorityStatus
    , CertificateAuthorityStatus (..)

    -- * CertificateAuthorityType
    , CertificateAuthorityType (..)

    -- * FailureReason
    , FailureReason (..)

    -- * KeyAlgorithm
    , KeyAlgorithm (..)

    -- * RevocationReason
    , RevocationReason (..)

    -- * SigningAlgorithm
    , SigningAlgorithm (..)

    -- * ValidityPeriodType
    , ValidityPeriodType (..)

    -- * ASN1Subject
    , ASN1Subject
    , asn1Subject
    , asGivenName
    , asState
    , asCommonName
    , asOrganizationalUnit
    , asCountry
    , asGenerationQualifier
    , asLocality
    , asPseudonym
    , asInitials
    , asTitle
    , asOrganization
    , asSerialNumber
    , asSurname
    , asDistinguishedNameQualifier

    -- * CertificateAuthority
    , CertificateAuthority
    , certificateAuthority
    , caStatus
    , caFailureReason
    , caCertificateAuthorityConfiguration
    , caARN
    , caCreatedAt
    , caSerial
    , caNotBefore
    , caType
    , caRevocationConfiguration
    , caLastStateChangeAt
    , caNotAfter

    -- * CertificateAuthorityConfiguration
    , CertificateAuthorityConfiguration
    , certificateAuthorityConfiguration
    , cacKeyAlgorithm
    , cacSigningAlgorithm
    , cacSubject

    -- * CrlConfiguration
    , CrlConfiguration
    , crlConfiguration
    , ccCustomCname
    , ccExpirationInDays
    , ccS3BucketName
    , ccEnabled

    -- * RevocationConfiguration
    , RevocationConfiguration
    , revocationConfiguration
    , rcCrlConfiguration

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * Validity
    , Validity
    , validity
    , vValue
    , vType
    ) where

import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.CertificateManagerPCA.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-08-22@ of the Amazon Certificate Manager Private Certificate Authority SDK configuration.
certificateManagerPCA :: Service
certificateManagerPCA =
  Service
    { _svcAbbrev = "CertificateManagerPCA"
    , _svcSigner = v4
    , _svcPrefix = "acm-pca"
    , _svcVersion = "2017-08-22"
    , _svcEndpoint = defaultEndpoint certificateManagerPCA
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CertificateManagerPCA"
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


-- | The tag associated with the CA is not valid. The invalid argument is contained in the message field.
--
--
_InvalidTagException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagException =
  _MatchServiceError certificateManagerPCA "InvalidTagException"


-- | The certificate signing request is invalid.
--
--
_MalformedCSRException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedCSRException =
  _MatchServiceError certificateManagerPCA "MalformedCSRException"


-- | Your request has already been completed.
--
--
_RequestAlreadyProcessedException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestAlreadyProcessedException =
  _MatchServiceError certificateManagerPCA "RequestAlreadyProcessedException"


-- | One or more fields in the certificate are invalid.
--
--
_MalformedCertificateException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedCertificateException =
  _MatchServiceError certificateManagerPCA "MalformedCertificateException"


-- | The request has failed for an unspecified reason.
--
--
_RequestFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestFailedException =
  _MatchServiceError certificateManagerPCA "RequestFailedException"


-- | The certificate authority certificate you are importing does not comply with conditions specified in the certificate that signed it.
--
--
_CertificateMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateMismatchException =
  _MatchServiceError certificateManagerPCA "CertificateMismatchException"


-- | You can associate up to 50 tags with a private CA. Exception information is contained in the exception message field.
--
--
_TooManyTagsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTagsException =
  _MatchServiceError certificateManagerPCA "TooManyTagsException"


-- | One or more of the specified arguments was not valid.
--
--
_InvalidArgsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgsException =
  _MatchServiceError certificateManagerPCA "InvalidArgsException"


-- | Your request is already in progress.
--
--
_RequestInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestInProgressException =
  _MatchServiceError certificateManagerPCA "RequestInProgressException"


-- | A previous update to your private CA is still ongoing.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
  _MatchServiceError certificateManagerPCA "ConcurrentModificationException"


-- | The token specified in the @NextToken@ argument is not valid. Use the token returned from your previous call to 'ListCertificateAuthorities' .
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError certificateManagerPCA "InvalidNextTokenException"


-- | The requested Amazon Resource Name (ARN) does not refer to an existing resource.
--
--
_InvalidARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNException =
  _MatchServiceError certificateManagerPCA "InvalidArnException"


-- | The S3 bucket policy is not valid. The policy must give ACM PCA rights to read from and write to the bucket and find the bucket location.
--
--
_InvalidPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPolicyException =
  _MatchServiceError certificateManagerPCA "InvalidPolicyException"


-- | A resource such as a private CA, S3 bucket, certificate, or audit report cannot be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError certificateManagerPCA "ResourceNotFoundException"


-- | The private CA is in a state during which a report cannot be generated.
--
--
_InvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStateException =
  _MatchServiceError certificateManagerPCA "InvalidStateException"


-- | An ACM PCA limit has been exceeded. See the exception message returned to determine the limit that was exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError certificateManagerPCA "LimitExceededException"

