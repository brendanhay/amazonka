-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types
  ( -- * Service configuration
    certificateManagerService,

    -- * Errors

    -- * CertificateStatus
    CertificateStatus (..),

    -- * CertificateTransparencyLoggingPreference
    CertificateTransparencyLoggingPreference (..),

    -- * CertificateType
    CertificateType (..),

    -- * DomainStatus
    DomainStatus (..),

    -- * ExtendedKeyUsageName
    ExtendedKeyUsageName (..),

    -- * FailureReason
    FailureReason (..),

    -- * KeyAlgorithm
    KeyAlgorithm (..),

    -- * KeyUsageName
    KeyUsageName (..),

    -- * RecordType
    RecordType (..),

    -- * RenewalEligibility
    RenewalEligibility (..),

    -- * RenewalStatus
    RenewalStatus (..),

    -- * RevocationReason
    RevocationReason (..),

    -- * ValidationMethod
    ValidationMethod (..),

    -- * CertificateDetail
    CertificateDetail (..),
    mkCertificateDetail,
    cdSubject,
    cdStatus,
    cdFailureReason,
    cdSubjectAlternativeNames,
    cdInUseBy,
    cdCreatedAt,
    cdCertificateARN,
    cdSerial,
    cdRenewalEligibility,
    cdExtendedKeyUsages,
    cdImportedAt,
    cdKeyUsages,
    cdRevokedAt,
    cdNotBefore,
    cdRevocationReason,
    cdDomainName,
    cdRenewalSummary,
    cdKeyAlgorithm,
    cdType,
    cdOptions,
    cdIssuedAt,
    cdSignatureAlgorithm,
    cdDomainValidationOptions,
    cdIssuer,
    cdNotAfter,
    cdCertificateAuthorityARN,

    -- * CertificateOptions
    CertificateOptions (..),
    mkCertificateOptions,
    coCertificateTransparencyLoggingPreference,

    -- * CertificateSummary
    CertificateSummary (..),
    mkCertificateSummary,
    csCertificateARN,
    csDomainName,

    -- * DomainValidation
    DomainValidation (..),
    mkDomainValidation,
    dvValidationEmails,
    dvValidationMethod,
    dvResourceRecord,
    dvValidationStatus,
    dvValidationDomain,
    dvDomainName,

    -- * DomainValidationOption
    DomainValidationOption (..),
    mkDomainValidationOption,
    dvoDomainName,
    dvoValidationDomain,

    -- * ExtendedKeyUsage
    ExtendedKeyUsage (..),
    mkExtendedKeyUsage,
    ekuOId,
    ekuName,

    -- * Filters
    Filters (..),
    mkFilters,
    fKeyTypes,
    fKeyUsage,
    fExtendedKeyUsage,

    -- * KeyUsage
    KeyUsage (..),
    mkKeyUsage,
    kuName,

    -- * RenewalSummary
    RenewalSummary (..),
    mkRenewalSummary,
    rsRenewalStatusReason,
    rsRenewalStatus,
    rsDomainValidationOptions,
    rsUpdatedAt,

    -- * ResourceRecord
    ResourceRecord (..),
    mkResourceRecord,
    rrName,
    rrType,
    rrValue,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
  )
where

import Network.AWS.CertificateManager.Types.CertificateDetail
import Network.AWS.CertificateManager.Types.CertificateOptions
import Network.AWS.CertificateManager.Types.CertificateStatus
import Network.AWS.CertificateManager.Types.CertificateSummary
import Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference
import Network.AWS.CertificateManager.Types.CertificateType
import Network.AWS.CertificateManager.Types.DomainStatus
import Network.AWS.CertificateManager.Types.DomainValidation
import Network.AWS.CertificateManager.Types.DomainValidationOption
import Network.AWS.CertificateManager.Types.ExtendedKeyUsage
import Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
import Network.AWS.CertificateManager.Types.FailureReason
import Network.AWS.CertificateManager.Types.Filters
import Network.AWS.CertificateManager.Types.KeyAlgorithm
import Network.AWS.CertificateManager.Types.KeyUsage
import Network.AWS.CertificateManager.Types.KeyUsageName
import Network.AWS.CertificateManager.Types.RecordType
import Network.AWS.CertificateManager.Types.RenewalEligibility
import Network.AWS.CertificateManager.Types.RenewalStatus
import Network.AWS.CertificateManager.Types.RenewalSummary
import Network.AWS.CertificateManager.Types.ResourceRecord
import Network.AWS.CertificateManager.Types.RevocationReason
import Network.AWS.CertificateManager.Types.Tag
import Network.AWS.CertificateManager.Types.ValidationMethod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-12-08@ of the Amazon Certificate Manager SDK configuration.
certificateManagerService :: Lude.Service
certificateManagerService =
  Lude.Service
    { Lude._svcAbbrev = "CertificateManager",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "acm",
      Lude._svcVersion = "2015-12-08",
      Lude._svcEndpoint = Lude.defaultEndpoint certificateManagerService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CertificateManager",
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
