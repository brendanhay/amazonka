{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the /ACM Private CA API Reference/ . It provides descriptions, syntax, and usage examples for each of the actions and data types involved in creating and managing private certificate authorities (CA) for your organization.
--
-- The documentation for each action shows the Query API request parameters and the XML response. Alternatively, you can use one of the AWS SDKs to access an API that's tailored to the programming language or platform that you're using. For more information, see <https://aws.amazon.com/tools/#SDKs AWS SDKs> .
module Network.AWS.CertificateManagerPCA
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** PermissionAlreadyExistsException
    _PermissionAlreadyExistsException,

    -- ** MalformedCSRException
    _MalformedCSRException,

    -- ** RequestAlreadyProcessedException
    _RequestAlreadyProcessedException,

    -- ** MalformedCertificateException
    _MalformedCertificateException,

    -- ** RequestFailedException
    _RequestFailedException,

    -- ** CertificateMismatchException
    _CertificateMismatchException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** InvalidArgsException
    _InvalidArgsException,

    -- ** RequestInProgressException
    _RequestInProgressException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** LockoutPreventedException
    _LockoutPreventedException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- ** CertificateIssued
    mkCertificateIssued,

    -- ** AuditReportCreated
    mkAuditReportCreated,

    -- ** CertificateAuthorityCSRCreated
    mkCertificateAuthorityCSRCreated,

    -- * Operations
    -- $operations

    -- ** ImportCertificateAuthorityCertificate
    module Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate,

    -- ** CreatePermission
    module Network.AWS.CertificateManagerPCA.CreatePermission,

    -- ** DescribeCertificateAuthorityAuditReport
    module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport,

    -- ** DeletePermission
    module Network.AWS.CertificateManagerPCA.DeletePermission,

    -- ** RevokeCertificate
    module Network.AWS.CertificateManagerPCA.RevokeCertificate,

    -- ** UpdateCertificateAuthority
    module Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority,

    -- ** DeleteCertificateAuthority
    module Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority,

    -- ** GetCertificateAuthorityCsr
    module Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr,

    -- ** CreateCertificateAuthority
    module Network.AWS.CertificateManagerPCA.CreateCertificateAuthority,

    -- ** ListCertificateAuthorities (Paginated)
    module Network.AWS.CertificateManagerPCA.ListCertificateAuthorities,

    -- ** GetCertificate
    module Network.AWS.CertificateManagerPCA.GetCertificate,

    -- ** TagCertificateAuthority
    module Network.AWS.CertificateManagerPCA.TagCertificateAuthority,

    -- ** PutPolicy
    module Network.AWS.CertificateManagerPCA.PutPolicy,

    -- ** DeletePolicy
    module Network.AWS.CertificateManagerPCA.DeletePolicy,

    -- ** DescribeCertificateAuthority
    module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority,

    -- ** RestoreCertificateAuthority
    module Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority,

    -- ** IssueCertificate
    module Network.AWS.CertificateManagerPCA.IssueCertificate,

    -- ** GetCertificateAuthorityCertificate
    module Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCertificate,

    -- ** ListPermissions (Paginated)
    module Network.AWS.CertificateManagerPCA.ListPermissions,

    -- ** UntagCertificateAuthority
    module Network.AWS.CertificateManagerPCA.UntagCertificateAuthority,

    -- ** CreateCertificateAuthorityAuditReport
    module Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport,

    -- ** ListTags (Paginated)
    module Network.AWS.CertificateManagerPCA.ListTags,

    -- ** GetPolicy
    module Network.AWS.CertificateManagerPCA.GetPolicy,

    -- * Types

    -- ** SigningAlgorithm
    SigningAlgorithm (..),

    -- ** IdempotencyToken
    IdempotencyToken (..),

    -- ** FailureReason
    FailureReason (..),

    -- ** CertificateAuthorityConfiguration
    CertificateAuthorityConfiguration (..),
    mkCertificateAuthorityConfiguration,
    cacKeyAlgorithm,
    cacSigningAlgorithm,
    cacSubject,

    -- ** CertificateAuthorityType
    CertificateAuthorityType (..),

    -- ** ValidityPeriodType
    ValidityPeriodType (..),

    -- ** S3Key
    S3Key (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** CsrBody
    CsrBody (..),

    -- ** Arn
    Arn (..),

    -- ** String
    String (..),

    -- ** CrlConfiguration
    CrlConfiguration (..),
    mkCrlConfiguration,
    ccEnabled,
    ccCustomCname,
    ccExpirationInDays,
    ccS3BucketName,

    -- ** String5
    String5 (..),

    -- ** String128
    String128 (..),

    -- ** ASN1Subject
    ASN1Subject (..),
    mkASN1Subject,
    asnsCommonName,
    asnsCountry,
    asnsDistinguishedNameQualifier,
    asnsGenerationQualifier,
    asnsGivenName,
    asnsInitials,
    asnsLocality,
    asnsOrganization,
    asnsOrganizationalUnit,
    asnsPseudonym,
    asnsSerialNumber,
    asnsState,
    asnsSurname,
    asnsTitle,

    -- ** AccountId
    AccountId (..),

    -- ** CertificateAuthority
    CertificateAuthority (..),
    mkCertificateAuthority,
    caArn,
    caCertificateAuthorityConfiguration,
    caCreatedAt,
    caFailureReason,
    caLastStateChangeAt,
    caNotAfter,
    caNotBefore,
    caOwnerAccount,
    caRestorableUntil,
    caRevocationConfiguration,
    caSerial,
    caStatus,
    caType,

    -- ** NextToken
    NextToken (..),

    -- ** RevocationReason
    RevocationReason (..),

    -- ** String3To255
    String3To255 (..),

    -- ** AuditReportStatus
    AuditReportStatus (..),

    -- ** Principal
    Principal (..),

    -- ** Validity
    Validity (..),
    mkValidity,
    vValue,
    vType,

    -- ** AWSPolicy
    AWSPolicy (..),

    -- ** CertificateAuthorityStatus
    CertificateAuthorityStatus (..),

    -- ** KeyAlgorithm
    KeyAlgorithm (..),

    -- ** CertificateChain
    CertificateChain (..),

    -- ** RevocationConfiguration
    RevocationConfiguration (..),
    mkRevocationConfiguration,
    rcCrlConfiguration,

    -- ** Permission
    Permission (..),
    mkPermission,
    pActions,
    pCertificateAuthorityArn,
    pCreatedAt,
    pPolicy,
    pPrincipal,
    pSourceAccount,

    -- ** ResourceOwner
    ResourceOwner (..),

    -- ** AuditReportResponseFormat
    AuditReportResponseFormat (..),

    -- ** AuditReportId
    AuditReportId (..),

    -- ** ActionType
    ActionType (..),

    -- ** S3BucketName
    S3BucketName (..),

    -- ** CertificateAuthorityArn
    CertificateAuthorityArn (..),

    -- ** CertificateSerial
    CertificateSerial (..),

    -- ** CertificateArn
    CertificateArn (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** Policy
    Policy (..),

    -- ** CustomCname
    CustomCname (..),

    -- ** Certificate
    Certificate (..),

    -- ** CommonName
    CommonName (..),

    -- ** Country
    Country (..),

    -- ** DistinguishedNameQualifier
    DistinguishedNameQualifier (..),

    -- ** GenerationQualifier
    GenerationQualifier (..),

    -- ** GivenName
    GivenName (..),

    -- ** Organization
    Organization (..),

    -- ** OrganizationalUnit
    OrganizationalUnit (..),

    -- ** SerialNumber
    SerialNumber (..),

    -- ** Surname
    Surname (..),

    -- ** Title
    Title (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
import Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
import Network.AWS.CertificateManagerPCA.CreatePermission
import Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
import Network.AWS.CertificateManagerPCA.DeletePermission
import Network.AWS.CertificateManagerPCA.DeletePolicy
import Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
import Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
import Network.AWS.CertificateManagerPCA.GetCertificate
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCertificate
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr
import Network.AWS.CertificateManagerPCA.GetPolicy
import Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
import Network.AWS.CertificateManagerPCA.IssueCertificate
import Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
import Network.AWS.CertificateManagerPCA.ListPermissions
import Network.AWS.CertificateManagerPCA.ListTags
import Network.AWS.CertificateManagerPCA.PutPolicy
import Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
import Network.AWS.CertificateManagerPCA.RevokeCertificate
import Network.AWS.CertificateManagerPCA.TagCertificateAuthority
import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.UntagCertificateAuthority
import Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
import Network.AWS.CertificateManagerPCA.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CertificateManagerPCA'.

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
