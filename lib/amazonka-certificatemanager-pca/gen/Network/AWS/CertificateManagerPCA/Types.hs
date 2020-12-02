{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types
  ( -- * Service Configuration
    certificateManagerPCA,

    -- * Errors

    -- * ActionType
    ActionType (..),

    -- * AuditReportResponseFormat
    AuditReportResponseFormat (..),

    -- * AuditReportStatus
    AuditReportStatus (..),

    -- * CertificateAuthorityStatus
    CertificateAuthorityStatus (..),

    -- * CertificateAuthorityType
    CertificateAuthorityType (..),

    -- * FailureReason
    FailureReason (..),

    -- * KeyAlgorithm
    KeyAlgorithm (..),

    -- * ResourceOwner
    ResourceOwner (..),

    -- * RevocationReason
    RevocationReason (..),

    -- * SigningAlgorithm
    SigningAlgorithm (..),

    -- * ValidityPeriodType
    ValidityPeriodType (..),

    -- * ASN1Subject
    ASN1Subject,
    asn1Subject,
    asGivenName,
    asState,
    asCommonName,
    asOrganizationalUnit,
    asCountry,
    asGenerationQualifier,
    asLocality,
    asPseudonym,
    asInitials,
    asTitle,
    asOrganization,
    asSerialNumber,
    asSurname,
    asDistinguishedNameQualifier,

    -- * CertificateAuthority
    CertificateAuthority,
    certificateAuthority,
    caStatus,
    caFailureReason,
    caCertificateAuthorityConfiguration,
    caARN,
    caCreatedAt,
    caSerial,
    caNotBefore,
    caRestorableUntil,
    caType,
    caOwnerAccount,
    caRevocationConfiguration,
    caLastStateChangeAt,
    caNotAfter,

    -- * CertificateAuthorityConfiguration
    CertificateAuthorityConfiguration,
    certificateAuthorityConfiguration,
    cacKeyAlgorithm,
    cacSigningAlgorithm,
    cacSubject,

    -- * CrlConfiguration
    CrlConfiguration,
    crlConfiguration,
    ccCustomCname,
    ccExpirationInDays,
    ccS3BucketName,
    ccEnabled,

    -- * Permission
    Permission,
    permission,
    pSourceAccount,
    pActions,
    pCreatedAt,
    pPrincipal,
    pPolicy,
    pCertificateAuthorityARN,

    -- * RevocationConfiguration
    RevocationConfiguration,
    revocationConfiguration,
    rcCrlConfiguration,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * Validity
    Validity,
    validity,
    vValue,
    vType,
  )
where

import Network.AWS.CertificateManagerPCA.Types.ASN1Subject
import Network.AWS.CertificateManagerPCA.Types.ActionType
import Network.AWS.CertificateManagerPCA.Types.AuditReportResponseFormat
import Network.AWS.CertificateManagerPCA.Types.AuditReportStatus
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthority
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType
import Network.AWS.CertificateManagerPCA.Types.CrlConfiguration
import Network.AWS.CertificateManagerPCA.Types.FailureReason
import Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
import Network.AWS.CertificateManagerPCA.Types.Permission
import Network.AWS.CertificateManagerPCA.Types.ResourceOwner
import Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
import Network.AWS.CertificateManagerPCA.Types.RevocationReason
import Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
import Network.AWS.CertificateManagerPCA.Types.Tag
import Network.AWS.CertificateManagerPCA.Types.Validity
import Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-08-22@ of the Amazon Certificate Manager Private Certificate Authority SDK configuration.
certificateManagerPCA :: Service
certificateManagerPCA =
  Service
    { _svcAbbrev = "CertificateManagerPCA",
      _svcSigner = v4,
      _svcPrefix = "acm-pca",
      _svcVersion = "2017-08-22",
      _svcEndpoint = defaultEndpoint certificateManagerPCA,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CertificateManagerPCA",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
