-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types
  ( -- * Service configuration
    certificateManagerPCAService,

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
    ASN1Subject (..),
    mkASN1Subject,
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
    CertificateAuthority (..),
    mkCertificateAuthority,
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
    CertificateAuthorityConfiguration (..),
    mkCertificateAuthorityConfiguration,
    cacKeyAlgorithm,
    cacSigningAlgorithm,
    cacSubject,

    -- * CrlConfiguration
    CrlConfiguration (..),
    mkCrlConfiguration,
    ccCustomCname,
    ccExpirationInDays,
    ccS3BucketName,
    ccEnabled,

    -- * Permission
    Permission (..),
    mkPermission,
    pSourceAccount,
    pActions,
    pCreatedAt,
    pPrincipal,
    pPolicy,
    pCertificateAuthorityARN,

    -- * RevocationConfiguration
    RevocationConfiguration (..),
    mkRevocationConfiguration,
    rcCrlConfiguration,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Validity
    Validity (..),
    mkValidity,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-08-22@ of the Amazon Certificate Manager Private Certificate Authority SDK configuration.
certificateManagerPCAService :: Lude.Service
certificateManagerPCAService =
  Lude.Service
    { Lude._svcAbbrev = "CertificateManagerPCA",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "acm-pca",
      Lude._svcVersion = "2017-08-22",
      Lude._svcEndpoint =
        Lude.defaultEndpoint certificateManagerPCAService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CertificateManagerPCA",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
