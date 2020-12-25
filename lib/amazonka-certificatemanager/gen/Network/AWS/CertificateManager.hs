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
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidDomainValidationOptionsException
    _InvalidDomainValidationOptionsException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** InvalidArgsException
    _InvalidArgsException,

    -- ** RequestInProgressException
    _RequestInProgressException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

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

    -- ** DomainStatus
    DomainStatus (..),

    -- ** IdempotencyToken
    IdempotencyToken (..),

    -- ** FailureReason
    FailureReason (..),

    -- ** CertificateDetail
    CertificateDetail (..),
    mkCertificateDetail,
    cdCertificateArn,
    cdCertificateAuthorityArn,
    cdCreatedAt,
    cdDomainName,
    cdDomainValidationOptions,
    cdExtendedKeyUsages,
    cdFailureReason,
    cdImportedAt,
    cdInUseBy,
    cdIssuedAt,
    cdIssuer,
    cdKeyAlgorithm,
    cdKeyUsages,
    cdNotAfter,
    cdNotBefore,
    cdOptions,
    cdRenewalEligibility,
    cdRenewalSummary,
    cdRevocationReason,
    cdRevokedAt,
    cdSerial,
    cdSignatureAlgorithm,
    cdStatus,
    cdSubject,
    cdSubjectAlternativeNames,
    cdType,

    -- ** ValidationMethod
    ValidationMethod (..),

    -- ** ResourceRecord
    ResourceRecord (..),
    mkResourceRecord,
    rrName,
    rrType,
    rrValue,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** PrivateKey
    PrivateKey (..),

    -- ** ExtendedKeyUsageName
    ExtendedKeyUsageName (..),

    -- ** Arn
    Arn (..),

    -- ** String
    String (..),

    -- ** CertificateType
    CertificateType (..),

    -- ** RecordType
    RecordType (..),

    -- ** KeyUsageName
    KeyUsageName (..),

    -- ** DomainValidationOption
    DomainValidationOption (..),
    mkDomainValidationOption,
    dvoDomainName,
    dvoValidationDomain,

    -- ** Filters
    Filters (..),
    mkFilters,
    fExtendedKeyUsage,
    fKeyTypes,
    fKeyUsage,

    -- ** RenewalEligibility
    RenewalEligibility (..),

    -- ** CertificateSummary
    CertificateSummary (..),
    mkCertificateSummary,
    csCertificateArn,
    csDomainName,

    -- ** NextToken
    NextToken (..),

    -- ** RevocationReason
    RevocationReason (..),

    -- ** RenewalStatus
    RenewalStatus (..),

    -- ** DomainNameString
    DomainNameString (..),

    -- ** RenewalSummary
    RenewalSummary (..),
    mkRenewalSummary,
    rsRenewalStatus,
    rsDomainValidationOptions,
    rsUpdatedAt,
    rsRenewalStatusReason,

    -- ** KeyUsage
    KeyUsage (..),
    mkKeyUsage,
    kuName,

    -- ** CertificateOptions
    CertificateOptions (..),
    mkCertificateOptions,
    coCertificateTransparencyLoggingPreference,

    -- ** ExtendedKeyUsage
    ExtendedKeyUsage (..),
    mkExtendedKeyUsage,
    ekuName,
    ekuOID,

    -- ** CertificateTransparencyLoggingPreference
    CertificateTransparencyLoggingPreference (..),

    -- ** KeyAlgorithm
    KeyAlgorithm (..),

    -- ** CertificateChain
    CertificateChain (..),

    -- ** DomainValidation
    DomainValidation (..),
    mkDomainValidation,
    dvDomainName,
    dvResourceRecord,
    dvValidationDomain,
    dvValidationEmails,
    dvValidationMethod,
    dvValidationStatus,

    -- ** CertificateStatus
    CertificateStatus (..),

    -- ** CertificateArn
    CertificateArn (..),

    -- ** CertificateAuthorityArn
    CertificateAuthorityArn (..),

    -- ** DomainName
    DomainName (..),

    -- ** Issuer
    Issuer (..),

    -- ** Serial
    Serial (..),

    -- ** SignatureAlgorithm
    SignatureAlgorithm (..),

    -- ** Subject
    Subject (..),

    -- ** Name
    Name (..),

    -- ** Value
    Value (..),

    -- ** Key
    Key (..),

    -- ** ValidationDomain
    ValidationDomain (..),

    -- ** Certificate
    Certificate (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
