{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Certificate Manager__
--
-- Welcome to the AWS Certificate Manager (ACM) API documentation.
-- You can use ACM to manage SSL/TLS certificates for your AWS-based websites and applications. For general information about using ACM, see the <https://docs.aws.amazon.com/acm/latest/userguide/ /AWS Certificate Manager User Guide/ > .
module Network.AWS.CertificateManager
  ( -- * Service configuration
    certificateManagerService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** CertificateValidated
    mkCertificateValidated,

    -- * Operations
    -- $operations

    -- ** ResendValidationEmail
    module Network.AWS.CertificateManager.ResendValidationEmail,

    -- ** UpdateCertificateOptions
    module Network.AWS.CertificateManager.UpdateCertificateOptions,

    -- ** ListTagsForCertificate
    module Network.AWS.CertificateManager.ListTagsForCertificate,

    -- ** GetCertificate
    module Network.AWS.CertificateManager.GetCertificate,

    -- ** AddTagsToCertificate
    module Network.AWS.CertificateManager.AddTagsToCertificate,

    -- ** RequestCertificate
    module Network.AWS.CertificateManager.RequestCertificate,

    -- ** ListCertificates (Paginated)
    module Network.AWS.CertificateManager.ListCertificates,

    -- ** DeleteCertificate
    module Network.AWS.CertificateManager.DeleteCertificate,

    -- ** RemoveTagsFromCertificate
    module Network.AWS.CertificateManager.RemoveTagsFromCertificate,

    -- ** ImportCertificate
    module Network.AWS.CertificateManager.ImportCertificate,

    -- ** DescribeCertificate
    module Network.AWS.CertificateManager.DescribeCertificate,

    -- ** RenewCertificate
    module Network.AWS.CertificateManager.RenewCertificate,

    -- ** ExportCertificate
    module Network.AWS.CertificateManager.ExportCertificate,

    -- * Types

    -- ** CertificateStatus
    CertificateStatus (..),

    -- ** CertificateTransparencyLoggingPreference
    CertificateTransparencyLoggingPreference (..),

    -- ** CertificateType
    CertificateType (..),

    -- ** DomainStatus
    DomainStatus (..),

    -- ** ExtendedKeyUsageName
    ExtendedKeyUsageName (..),

    -- ** FailureReason
    FailureReason (..),

    -- ** KeyAlgorithm
    KeyAlgorithm (..),

    -- ** KeyUsageName
    KeyUsageName (..),

    -- ** RecordType
    RecordType (..),

    -- ** RenewalEligibility
    RenewalEligibility (..),

    -- ** RenewalStatus
    RenewalStatus (..),

    -- ** RevocationReason
    RevocationReason (..),

    -- ** ValidationMethod
    ValidationMethod (..),

    -- ** CertificateDetail
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

    -- ** CertificateOptions
    CertificateOptions (..),
    mkCertificateOptions,
    coCertificateTransparencyLoggingPreference,

    -- ** CertificateSummary
    CertificateSummary (..),
    mkCertificateSummary,
    csCertificateARN,
    csDomainName,

    -- ** DomainValidation
    DomainValidation (..),
    mkDomainValidation,
    dvValidationEmails,
    dvValidationMethod,
    dvResourceRecord,
    dvValidationStatus,
    dvDomainName,
    dvValidationDomain,

    -- ** DomainValidationOption
    DomainValidationOption (..),
    mkDomainValidationOption,
    dvoDomainName,
    dvoValidationDomain,

    -- ** ExtendedKeyUsage
    ExtendedKeyUsage (..),
    mkExtendedKeyUsage,
    ekuOId,
    ekuName,

    -- ** Filters
    Filters (..),
    mkFilters,
    fKeyTypes,
    fKeyUsage,
    fExtendedKeyUsage,

    -- ** KeyUsage
    KeyUsage (..),
    mkKeyUsage,
    kuName,

    -- ** RenewalSummary
    RenewalSummary (..),
    mkRenewalSummary,
    rsRenewalStatus,
    rsUpdatedAt,
    rsDomainValidationOptions,
    rsRenewalStatusReason,

    -- ** ResourceRecord
    ResourceRecord (..),
    mkResourceRecord,
    rrValue,
    rrName,
    rrType,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.CertificateManager.AddTagsToCertificate
import Network.AWS.CertificateManager.DeleteCertificate
import Network.AWS.CertificateManager.DescribeCertificate
import Network.AWS.CertificateManager.ExportCertificate
import Network.AWS.CertificateManager.GetCertificate
import Network.AWS.CertificateManager.ImportCertificate
import Network.AWS.CertificateManager.ListCertificates
import Network.AWS.CertificateManager.ListTagsForCertificate
import Network.AWS.CertificateManager.RemoveTagsFromCertificate
import Network.AWS.CertificateManager.RenewCertificate
import Network.AWS.CertificateManager.RequestCertificate
import Network.AWS.CertificateManager.ResendValidationEmail
import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.UpdateCertificateOptions
import Network.AWS.CertificateManager.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CertificateManager'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
