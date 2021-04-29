{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the /ACM Private CA API Reference/. It provides descriptions,
-- syntax, and usage examples for each of the actions and data types
-- involved in creating and managing private certificate authorities (CA)
-- for your organization.
--
-- The documentation for each action shows the Query API request parameters
-- and the XML response. Alternatively, you can use one of the AWS SDKs to
-- access an API that\'s tailored to the programming language or platform
-- that you\'re using. For more information, see
-- <https://aws.amazon.com/tools/#SDKs AWS SDKs>.
--
-- Each ACM Private CA API action has a quota that determines the number of
-- times the action can be called per second. For more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaLimits.html#PcaLimits-api API Rate Quotas in ACM Private CA>
-- in the ACM Private CA user guide.
module Network.AWS.CertificateManagerPCA
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** InvalidArgsException
    _InvalidArgsException,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** MalformedCertificateException
    _MalformedCertificateException,

    -- ** RequestAlreadyProcessedException
    _RequestAlreadyProcessedException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** PermissionAlreadyExistsException
    _PermissionAlreadyExistsException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** RequestInProgressException
    _RequestInProgressException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** CertificateMismatchException
    _CertificateMismatchException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** RequestFailedException
    _RequestFailedException,

    -- ** LockoutPreventedException
    _LockoutPreventedException,

    -- ** MalformedCSRException
    _MalformedCSRException,

    -- * Waiters
    -- $waiters

    -- ** AuditReportCreated
    newAuditReportCreated,

    -- ** CertificateAuthorityCSRCreated
    newCertificateAuthorityCSRCreated,

    -- ** CertificateIssued
    newCertificateIssued,

    -- * Operations
    -- $operations

    -- ** CreatePermission
    CreatePermission (CreatePermission'),
    newCreatePermission,
    CreatePermissionResponse (CreatePermissionResponse'),
    newCreatePermissionResponse,

    -- ** RestoreCertificateAuthority
    RestoreCertificateAuthority (RestoreCertificateAuthority'),
    newRestoreCertificateAuthority,
    RestoreCertificateAuthorityResponse (RestoreCertificateAuthorityResponse'),
    newRestoreCertificateAuthorityResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DescribeCertificateAuthority
    DescribeCertificateAuthority (DescribeCertificateAuthority'),
    newDescribeCertificateAuthority,
    DescribeCertificateAuthorityResponse (DescribeCertificateAuthorityResponse'),
    newDescribeCertificateAuthorityResponse,

    -- ** TagCertificateAuthority
    TagCertificateAuthority (TagCertificateAuthority'),
    newTagCertificateAuthority,
    TagCertificateAuthorityResponse (TagCertificateAuthorityResponse'),
    newTagCertificateAuthorityResponse,

    -- ** CreateCertificateAuthorityAuditReport
    CreateCertificateAuthorityAuditReport (CreateCertificateAuthorityAuditReport'),
    newCreateCertificateAuthorityAuditReport,
    CreateCertificateAuthorityAuditReportResponse (CreateCertificateAuthorityAuditReportResponse'),
    newCreateCertificateAuthorityAuditReportResponse,

    -- ** GetCertificate
    GetCertificate (GetCertificate'),
    newGetCertificate,
    GetCertificateResponse (GetCertificateResponse'),
    newGetCertificateResponse,

    -- ** CreateCertificateAuthority
    CreateCertificateAuthority (CreateCertificateAuthority'),
    newCreateCertificateAuthority,
    CreateCertificateAuthorityResponse (CreateCertificateAuthorityResponse'),
    newCreateCertificateAuthorityResponse,

    -- ** GetCertificateAuthorityCsr
    GetCertificateAuthorityCsr (GetCertificateAuthorityCsr'),
    newGetCertificateAuthorityCsr,
    GetCertificateAuthorityCsrResponse (GetCertificateAuthorityCsrResponse'),
    newGetCertificateAuthorityCsrResponse,

    -- ** ListCertificateAuthorities (Paginated)
    ListCertificateAuthorities (ListCertificateAuthorities'),
    newListCertificateAuthorities,
    ListCertificateAuthoritiesResponse (ListCertificateAuthoritiesResponse'),
    newListCertificateAuthoritiesResponse,

    -- ** RevokeCertificate
    RevokeCertificate (RevokeCertificate'),
    newRevokeCertificate,
    RevokeCertificateResponse (RevokeCertificateResponse'),
    newRevokeCertificateResponse,

    -- ** DeletePermission
    DeletePermission (DeletePermission'),
    newDeletePermission,
    DeletePermissionResponse (DeletePermissionResponse'),
    newDeletePermissionResponse,

    -- ** ListPermissions (Paginated)
    ListPermissions (ListPermissions'),
    newListPermissions,
    ListPermissionsResponse (ListPermissionsResponse'),
    newListPermissionsResponse,

    -- ** GetCertificateAuthorityCertificate
    GetCertificateAuthorityCertificate (GetCertificateAuthorityCertificate'),
    newGetCertificateAuthorityCertificate,
    GetCertificateAuthorityCertificateResponse (GetCertificateAuthorityCertificateResponse'),
    newGetCertificateAuthorityCertificateResponse,

    -- ** IssueCertificate
    IssueCertificate (IssueCertificate'),
    newIssueCertificate,
    IssueCertificateResponse (IssueCertificateResponse'),
    newIssueCertificateResponse,

    -- ** ImportCertificateAuthorityCertificate
    ImportCertificateAuthorityCertificate (ImportCertificateAuthorityCertificate'),
    newImportCertificateAuthorityCertificate,
    ImportCertificateAuthorityCertificateResponse (ImportCertificateAuthorityCertificateResponse'),
    newImportCertificateAuthorityCertificateResponse,

    -- ** PutPolicy
    PutPolicy (PutPolicy'),
    newPutPolicy,
    PutPolicyResponse (PutPolicyResponse'),
    newPutPolicyResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** DeleteCertificateAuthority
    DeleteCertificateAuthority (DeleteCertificateAuthority'),
    newDeleteCertificateAuthority,
    DeleteCertificateAuthorityResponse (DeleteCertificateAuthorityResponse'),
    newDeleteCertificateAuthorityResponse,

    -- ** UpdateCertificateAuthority
    UpdateCertificateAuthority (UpdateCertificateAuthority'),
    newUpdateCertificateAuthority,
    UpdateCertificateAuthorityResponse (UpdateCertificateAuthorityResponse'),
    newUpdateCertificateAuthorityResponse,

    -- ** UntagCertificateAuthority
    UntagCertificateAuthority (UntagCertificateAuthority'),
    newUntagCertificateAuthority,
    UntagCertificateAuthorityResponse (UntagCertificateAuthorityResponse'),
    newUntagCertificateAuthorityResponse,

    -- ** DescribeCertificateAuthorityAuditReport
    DescribeCertificateAuthorityAuditReport (DescribeCertificateAuthorityAuditReport'),
    newDescribeCertificateAuthorityAuditReport,
    DescribeCertificateAuthorityAuditReportResponse (DescribeCertificateAuthorityAuditReportResponse'),
    newDescribeCertificateAuthorityAuditReportResponse,

    -- * Types

    -- ** AccessMethodType
    AccessMethodType (..),

    -- ** ActionType
    ActionType (..),

    -- ** AuditReportResponseFormat
    AuditReportResponseFormat (..),

    -- ** AuditReportStatus
    AuditReportStatus (..),

    -- ** CertificateAuthorityStatus
    CertificateAuthorityStatus (..),

    -- ** CertificateAuthorityType
    CertificateAuthorityType (..),

    -- ** ExtendedKeyUsageType
    ExtendedKeyUsageType (..),

    -- ** FailureReason
    FailureReason (..),

    -- ** KeyAlgorithm
    KeyAlgorithm (..),

    -- ** PolicyQualifierId
    PolicyQualifierId (..),

    -- ** ResourceOwner
    ResourceOwner (..),

    -- ** RevocationReason
    RevocationReason (..),

    -- ** SigningAlgorithm
    SigningAlgorithm (..),

    -- ** ValidityPeriodType
    ValidityPeriodType (..),

    -- ** ASN1Subject
    ASN1Subject (ASN1Subject'),
    newASN1Subject,

    -- ** AccessDescription
    AccessDescription (AccessDescription'),
    newAccessDescription,

    -- ** AccessMethod
    AccessMethod (AccessMethod'),
    newAccessMethod,

    -- ** ApiPassthrough
    ApiPassthrough (ApiPassthrough'),
    newApiPassthrough,

    -- ** CertificateAuthority
    CertificateAuthority (CertificateAuthority'),
    newCertificateAuthority,

    -- ** CertificateAuthorityConfiguration
    CertificateAuthorityConfiguration (CertificateAuthorityConfiguration'),
    newCertificateAuthorityConfiguration,

    -- ** CrlConfiguration
    CrlConfiguration (CrlConfiguration'),
    newCrlConfiguration,

    -- ** CsrExtensions
    CsrExtensions (CsrExtensions'),
    newCsrExtensions,

    -- ** EdiPartyName
    EdiPartyName (EdiPartyName'),
    newEdiPartyName,

    -- ** ExtendedKeyUsage
    ExtendedKeyUsage (ExtendedKeyUsage'),
    newExtendedKeyUsage,

    -- ** Extensions
    Extensions (Extensions'),
    newExtensions,

    -- ** GeneralName
    GeneralName (GeneralName'),
    newGeneralName,

    -- ** KeyUsage
    KeyUsage (KeyUsage'),
    newKeyUsage,

    -- ** OtherName
    OtherName (OtherName'),
    newOtherName,

    -- ** Permission
    Permission (Permission'),
    newPermission,

    -- ** PolicyInformation
    PolicyInformation (PolicyInformation'),
    newPolicyInformation,

    -- ** PolicyQualifierInfo
    PolicyQualifierInfo (PolicyQualifierInfo'),
    newPolicyQualifierInfo,

    -- ** Qualifier
    Qualifier (Qualifier'),
    newQualifier,

    -- ** RevocationConfiguration
    RevocationConfiguration (RevocationConfiguration'),
    newRevocationConfiguration,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Validity
    Validity (Validity'),
    newValidity,
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
import Network.AWS.CertificateManagerPCA.Lens
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
