{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Certificate Manager
--
-- You can use AWS Certificate Manager (ACM) to manage SSL\/TLS
-- certificates for your AWS-based websites and applications. For more
-- information about using ACM, see the
-- <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide>.
module Network.AWS.CertificateManager
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

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** InvalidDomainValidationOptionsException
    _InvalidDomainValidationOptionsException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ValidationException
    _ValidationException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** RequestInProgressException
    _RequestInProgressException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** CertificateValidated
    newCertificateValidated,

    -- * Operations
    -- $operations

    -- ** GetAccountConfiguration
    GetAccountConfiguration (GetAccountConfiguration'),
    newGetAccountConfiguration,
    GetAccountConfigurationResponse (GetAccountConfigurationResponse'),
    newGetAccountConfigurationResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** UpdateCertificateOptions
    UpdateCertificateOptions (UpdateCertificateOptions'),
    newUpdateCertificateOptions,
    UpdateCertificateOptionsResponse (UpdateCertificateOptionsResponse'),
    newUpdateCertificateOptionsResponse,

    -- ** RemoveTagsFromCertificate
    RemoveTagsFromCertificate (RemoveTagsFromCertificate'),
    newRemoveTagsFromCertificate,
    RemoveTagsFromCertificateResponse (RemoveTagsFromCertificateResponse'),
    newRemoveTagsFromCertificateResponse,

    -- ** ExportCertificate
    ExportCertificate (ExportCertificate'),
    newExportCertificate,
    ExportCertificateResponse (ExportCertificateResponse'),
    newExportCertificateResponse,

    -- ** RenewCertificate
    RenewCertificate (RenewCertificate'),
    newRenewCertificate,
    RenewCertificateResponse (RenewCertificateResponse'),
    newRenewCertificateResponse,

    -- ** GetCertificate
    GetCertificate (GetCertificate'),
    newGetCertificate,
    GetCertificateResponse (GetCertificateResponse'),
    newGetCertificateResponse,

    -- ** DescribeCertificate
    DescribeCertificate (DescribeCertificate'),
    newDescribeCertificate,
    DescribeCertificateResponse (DescribeCertificateResponse'),
    newDescribeCertificateResponse,

    -- ** PutAccountConfiguration
    PutAccountConfiguration (PutAccountConfiguration'),
    newPutAccountConfiguration,
    PutAccountConfigurationResponse (PutAccountConfigurationResponse'),
    newPutAccountConfigurationResponse,

    -- ** ImportCertificate
    ImportCertificate (ImportCertificate'),
    newImportCertificate,
    ImportCertificateResponse (ImportCertificateResponse'),
    newImportCertificateResponse,

    -- ** ListCertificates (Paginated)
    ListCertificates (ListCertificates'),
    newListCertificates,
    ListCertificatesResponse (ListCertificatesResponse'),
    newListCertificatesResponse,

    -- ** RequestCertificate
    RequestCertificate (RequestCertificate'),
    newRequestCertificate,
    RequestCertificateResponse (RequestCertificateResponse'),
    newRequestCertificateResponse,

    -- ** ResendValidationEmail
    ResendValidationEmail (ResendValidationEmail'),
    newResendValidationEmail,
    ResendValidationEmailResponse (ResendValidationEmailResponse'),
    newResendValidationEmailResponse,

    -- ** AddTagsToCertificate
    AddTagsToCertificate (AddTagsToCertificate'),
    newAddTagsToCertificate,
    AddTagsToCertificateResponse (AddTagsToCertificateResponse'),
    newAddTagsToCertificateResponse,

    -- ** ListTagsForCertificate
    ListTagsForCertificate (ListTagsForCertificate'),
    newListTagsForCertificate,
    ListTagsForCertificateResponse (ListTagsForCertificateResponse'),
    newListTagsForCertificateResponse,

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
    CertificateDetail (CertificateDetail'),
    newCertificateDetail,

    -- ** CertificateOptions
    CertificateOptions (CertificateOptions'),
    newCertificateOptions,

    -- ** CertificateSummary
    CertificateSummary (CertificateSummary'),
    newCertificateSummary,

    -- ** DomainValidation
    DomainValidation (DomainValidation'),
    newDomainValidation,

    -- ** DomainValidationOption
    DomainValidationOption (DomainValidationOption'),
    newDomainValidationOption,

    -- ** ExpiryEventsConfiguration
    ExpiryEventsConfiguration (ExpiryEventsConfiguration'),
    newExpiryEventsConfiguration,

    -- ** ExtendedKeyUsage
    ExtendedKeyUsage (ExtendedKeyUsage'),
    newExtendedKeyUsage,

    -- ** Filters
    Filters (Filters'),
    newFilters,

    -- ** KeyUsage
    KeyUsage (KeyUsage'),
    newKeyUsage,

    -- ** RenewalSummary
    RenewalSummary (RenewalSummary'),
    newRenewalSummary,

    -- ** ResourceRecord
    ResourceRecord (ResourceRecord'),
    newResourceRecord,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.CertificateManager.AddTagsToCertificate
import Network.AWS.CertificateManager.DeleteCertificate
import Network.AWS.CertificateManager.DescribeCertificate
import Network.AWS.CertificateManager.ExportCertificate
import Network.AWS.CertificateManager.GetAccountConfiguration
import Network.AWS.CertificateManager.GetCertificate
import Network.AWS.CertificateManager.ImportCertificate
import Network.AWS.CertificateManager.Lens
import Network.AWS.CertificateManager.ListCertificates
import Network.AWS.CertificateManager.ListTagsForCertificate
import Network.AWS.CertificateManager.PutAccountConfiguration
import Network.AWS.CertificateManager.RemoveTagsFromCertificate
import Network.AWS.CertificateManager.RenewCertificate
import Network.AWS.CertificateManager.RequestCertificate
import Network.AWS.CertificateManager.ResendValidationEmail
import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.UpdateCertificateOptions
import Network.AWS.CertificateManager.Waiters

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
