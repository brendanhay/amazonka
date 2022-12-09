{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CertificateManagerPCA
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-08-22@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the /Private Certificate Authority (PCA) API Reference/. It
-- provides descriptions, syntax, and usage examples for each of the
-- actions and data types involved in creating and managing a private
-- certificate authority (CA) for your organization.
--
-- The documentation for each action shows the API request parameters and
-- the JSON response. Alternatively, you can use one of the Amazon Web
-- Services SDKs to access an API that is tailored to the programming
-- language or platform that you prefer. For more information, see
-- <https://aws.amazon.com/tools/#SDKs Amazon Web Services SDKs>.
--
-- Each ACM Private CA API operation has a quota that determines the number
-- of times the operation can be called per second. ACM Private CA
-- throttles API requests at different rates depending on the operation.
-- Throttling means that ACM Private CA rejects an otherwise valid request
-- because the request exceeds the operation\'s quota for the number of
-- requests per second. When a request is throttled, ACM Private CA returns
-- a
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/CommonErrors.html ThrottlingException>
-- error. ACM Private CA does not guarantee a minimum request rate for
-- APIs.
--
-- To see an up-to-date list of your ACM Private CA quotas, or to request a
-- quota increase, log into your Amazon Web Services account and visit the
-- <https://console.aws.amazon.com/servicequotas/ Service Quotas> console.
module Amazonka.CertificateManagerPCA
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CertificateMismatchException
    _CertificateMismatchException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidArgsException
    _InvalidArgsException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** LockoutPreventedException
    _LockoutPreventedException,

    -- ** MalformedCSRException
    _MalformedCSRException,

    -- ** MalformedCertificateException
    _MalformedCertificateException,

    -- ** PermissionAlreadyExistsException
    _PermissionAlreadyExistsException,

    -- ** RequestAlreadyProcessedException
    _RequestAlreadyProcessedException,

    -- ** RequestFailedException
    _RequestFailedException,

    -- ** RequestInProgressException
    _RequestInProgressException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TooManyTagsException
    _TooManyTagsException,

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

    -- ** CreateCertificateAuthority
    CreateCertificateAuthority (CreateCertificateAuthority'),
    newCreateCertificateAuthority,
    CreateCertificateAuthorityResponse (CreateCertificateAuthorityResponse'),
    newCreateCertificateAuthorityResponse,

    -- ** CreateCertificateAuthorityAuditReport
    CreateCertificateAuthorityAuditReport (CreateCertificateAuthorityAuditReport'),
    newCreateCertificateAuthorityAuditReport,
    CreateCertificateAuthorityAuditReportResponse (CreateCertificateAuthorityAuditReportResponse'),
    newCreateCertificateAuthorityAuditReportResponse,

    -- ** CreatePermission
    CreatePermission (CreatePermission'),
    newCreatePermission,
    CreatePermissionResponse (CreatePermissionResponse'),
    newCreatePermissionResponse,

    -- ** DeleteCertificateAuthority
    DeleteCertificateAuthority (DeleteCertificateAuthority'),
    newDeleteCertificateAuthority,
    DeleteCertificateAuthorityResponse (DeleteCertificateAuthorityResponse'),
    newDeleteCertificateAuthorityResponse,

    -- ** DeletePermission
    DeletePermission (DeletePermission'),
    newDeletePermission,
    DeletePermissionResponse (DeletePermissionResponse'),
    newDeletePermissionResponse,

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

    -- ** DescribeCertificateAuthorityAuditReport
    DescribeCertificateAuthorityAuditReport (DescribeCertificateAuthorityAuditReport'),
    newDescribeCertificateAuthorityAuditReport,
    DescribeCertificateAuthorityAuditReportResponse (DescribeCertificateAuthorityAuditReportResponse'),
    newDescribeCertificateAuthorityAuditReportResponse,

    -- ** GetCertificate
    GetCertificate (GetCertificate'),
    newGetCertificate,
    GetCertificateResponse (GetCertificateResponse'),
    newGetCertificateResponse,

    -- ** GetCertificateAuthorityCertificate
    GetCertificateAuthorityCertificate (GetCertificateAuthorityCertificate'),
    newGetCertificateAuthorityCertificate,
    GetCertificateAuthorityCertificateResponse (GetCertificateAuthorityCertificateResponse'),
    newGetCertificateAuthorityCertificateResponse,

    -- ** GetCertificateAuthorityCsr
    GetCertificateAuthorityCsr (GetCertificateAuthorityCsr'),
    newGetCertificateAuthorityCsr,
    GetCertificateAuthorityCsrResponse (GetCertificateAuthorityCsrResponse'),
    newGetCertificateAuthorityCsrResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** ImportCertificateAuthorityCertificate
    ImportCertificateAuthorityCertificate (ImportCertificateAuthorityCertificate'),
    newImportCertificateAuthorityCertificate,
    ImportCertificateAuthorityCertificateResponse (ImportCertificateAuthorityCertificateResponse'),
    newImportCertificateAuthorityCertificateResponse,

    -- ** IssueCertificate
    IssueCertificate (IssueCertificate'),
    newIssueCertificate,
    IssueCertificateResponse (IssueCertificateResponse'),
    newIssueCertificateResponse,

    -- ** ListCertificateAuthorities (Paginated)
    ListCertificateAuthorities (ListCertificateAuthorities'),
    newListCertificateAuthorities,
    ListCertificateAuthoritiesResponse (ListCertificateAuthoritiesResponse'),
    newListCertificateAuthoritiesResponse,

    -- ** ListPermissions (Paginated)
    ListPermissions (ListPermissions'),
    newListPermissions,
    ListPermissionsResponse (ListPermissionsResponse'),
    newListPermissionsResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** PutPolicy
    PutPolicy (PutPolicy'),
    newPutPolicy,
    PutPolicyResponse (PutPolicyResponse'),
    newPutPolicyResponse,

    -- ** RestoreCertificateAuthority
    RestoreCertificateAuthority (RestoreCertificateAuthority'),
    newRestoreCertificateAuthority,
    RestoreCertificateAuthorityResponse (RestoreCertificateAuthorityResponse'),
    newRestoreCertificateAuthorityResponse,

    -- ** RevokeCertificate
    RevokeCertificate (RevokeCertificate'),
    newRevokeCertificate,
    RevokeCertificateResponse (RevokeCertificateResponse'),
    newRevokeCertificateResponse,

    -- ** TagCertificateAuthority
    TagCertificateAuthority (TagCertificateAuthority'),
    newTagCertificateAuthority,
    TagCertificateAuthorityResponse (TagCertificateAuthorityResponse'),
    newTagCertificateAuthorityResponse,

    -- ** UntagCertificateAuthority
    UntagCertificateAuthority (UntagCertificateAuthority'),
    newUntagCertificateAuthority,
    UntagCertificateAuthorityResponse (UntagCertificateAuthorityResponse'),
    newUntagCertificateAuthorityResponse,

    -- ** UpdateCertificateAuthority
    UpdateCertificateAuthority (UpdateCertificateAuthority'),
    newUpdateCertificateAuthority,
    UpdateCertificateAuthorityResponse (UpdateCertificateAuthorityResponse'),
    newUpdateCertificateAuthorityResponse,

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

    -- ** CertificateAuthorityUsageMode
    CertificateAuthorityUsageMode (..),

    -- ** ExtendedKeyUsageType
    ExtendedKeyUsageType (..),

    -- ** FailureReason
    FailureReason (..),

    -- ** KeyAlgorithm
    KeyAlgorithm (..),

    -- ** KeyStorageSecurityStandard
    KeyStorageSecurityStandard (..),

    -- ** PolicyQualifierId
    PolicyQualifierId (..),

    -- ** ResourceOwner
    ResourceOwner (..),

    -- ** RevocationReason
    RevocationReason (..),

    -- ** S3ObjectAcl
    S3ObjectAcl (..),

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

    -- ** CustomAttribute
    CustomAttribute (CustomAttribute'),
    newCustomAttribute,

    -- ** CustomExtension
    CustomExtension (CustomExtension'),
    newCustomExtension,

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

    -- ** OcspConfiguration
    OcspConfiguration (OcspConfiguration'),
    newOcspConfiguration,

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

import Amazonka.CertificateManagerPCA.CreateCertificateAuthority
import Amazonka.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
import Amazonka.CertificateManagerPCA.CreatePermission
import Amazonka.CertificateManagerPCA.DeleteCertificateAuthority
import Amazonka.CertificateManagerPCA.DeletePermission
import Amazonka.CertificateManagerPCA.DeletePolicy
import Amazonka.CertificateManagerPCA.DescribeCertificateAuthority
import Amazonka.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
import Amazonka.CertificateManagerPCA.GetCertificate
import Amazonka.CertificateManagerPCA.GetCertificateAuthorityCertificate
import Amazonka.CertificateManagerPCA.GetCertificateAuthorityCsr
import Amazonka.CertificateManagerPCA.GetPolicy
import Amazonka.CertificateManagerPCA.ImportCertificateAuthorityCertificate
import Amazonka.CertificateManagerPCA.IssueCertificate
import Amazonka.CertificateManagerPCA.Lens
import Amazonka.CertificateManagerPCA.ListCertificateAuthorities
import Amazonka.CertificateManagerPCA.ListPermissions
import Amazonka.CertificateManagerPCA.ListTags
import Amazonka.CertificateManagerPCA.PutPolicy
import Amazonka.CertificateManagerPCA.RestoreCertificateAuthority
import Amazonka.CertificateManagerPCA.RevokeCertificate
import Amazonka.CertificateManagerPCA.TagCertificateAuthority
import Amazonka.CertificateManagerPCA.Types
import Amazonka.CertificateManagerPCA.UntagCertificateAuthority
import Amazonka.CertificateManagerPCA.UpdateCertificateAuthority
import Amazonka.CertificateManagerPCA.Waiters

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
