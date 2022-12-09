{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CertificateManager
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-12-08@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Certificate Manager
--
-- You can use Certificate Manager (ACM) to manage SSL\/TLS certificates
-- for your Amazon Web Services-based websites and applications. For more
-- information about using ACM, see the
-- <https://docs.aws.amazon.com/acm/latest/userguide/ Certificate Manager User Guide>.
module Amazonka.CertificateManager
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InvalidArgsException
    _InvalidArgsException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** InvalidDomainValidationOptionsException
    _InvalidDomainValidationOptionsException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** RequestInProgressException
    _RequestInProgressException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** CertificateValidated
    newCertificateValidated,

    -- * Operations
    -- $operations

    -- ** AddTagsToCertificate
    AddTagsToCertificate (AddTagsToCertificate'),
    newAddTagsToCertificate,
    AddTagsToCertificateResponse (AddTagsToCertificateResponse'),
    newAddTagsToCertificateResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** DescribeCertificate
    DescribeCertificate (DescribeCertificate'),
    newDescribeCertificate,
    DescribeCertificateResponse (DescribeCertificateResponse'),
    newDescribeCertificateResponse,

    -- ** ExportCertificate
    ExportCertificate (ExportCertificate'),
    newExportCertificate,
    ExportCertificateResponse (ExportCertificateResponse'),
    newExportCertificateResponse,

    -- ** GetAccountConfiguration
    GetAccountConfiguration (GetAccountConfiguration'),
    newGetAccountConfiguration,
    GetAccountConfigurationResponse (GetAccountConfigurationResponse'),
    newGetAccountConfigurationResponse,

    -- ** GetCertificate
    GetCertificate (GetCertificate'),
    newGetCertificate,
    GetCertificateResponse (GetCertificateResponse'),
    newGetCertificateResponse,

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

    -- ** ListTagsForCertificate
    ListTagsForCertificate (ListTagsForCertificate'),
    newListTagsForCertificate,
    ListTagsForCertificateResponse (ListTagsForCertificateResponse'),
    newListTagsForCertificateResponse,

    -- ** PutAccountConfiguration
    PutAccountConfiguration (PutAccountConfiguration'),
    newPutAccountConfiguration,
    PutAccountConfigurationResponse (PutAccountConfigurationResponse'),
    newPutAccountConfigurationResponse,

    -- ** RemoveTagsFromCertificate
    RemoveTagsFromCertificate (RemoveTagsFromCertificate'),
    newRemoveTagsFromCertificate,
    RemoveTagsFromCertificateResponse (RemoveTagsFromCertificateResponse'),
    newRemoveTagsFromCertificateResponse,

    -- ** RenewCertificate
    RenewCertificate (RenewCertificate'),
    newRenewCertificate,
    RenewCertificateResponse (RenewCertificateResponse'),
    newRenewCertificateResponse,

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

    -- ** UpdateCertificateOptions
    UpdateCertificateOptions (UpdateCertificateOptions'),
    newUpdateCertificateOptions,
    UpdateCertificateOptionsResponse (UpdateCertificateOptionsResponse'),
    newUpdateCertificateOptionsResponse,

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

    -- ** SortBy
    SortBy (..),

    -- ** SortOrder
    SortOrder (..),

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

import Amazonka.CertificateManager.AddTagsToCertificate
import Amazonka.CertificateManager.DeleteCertificate
import Amazonka.CertificateManager.DescribeCertificate
import Amazonka.CertificateManager.ExportCertificate
import Amazonka.CertificateManager.GetAccountConfiguration
import Amazonka.CertificateManager.GetCertificate
import Amazonka.CertificateManager.ImportCertificate
import Amazonka.CertificateManager.Lens
import Amazonka.CertificateManager.ListCertificates
import Amazonka.CertificateManager.ListTagsForCertificate
import Amazonka.CertificateManager.PutAccountConfiguration
import Amazonka.CertificateManager.RemoveTagsFromCertificate
import Amazonka.CertificateManager.RenewCertificate
import Amazonka.CertificateManager.RequestCertificate
import Amazonka.CertificateManager.ResendValidationEmail
import Amazonka.CertificateManager.Types
import Amazonka.CertificateManager.UpdateCertificateOptions
import Amazonka.CertificateManager.Waiters

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
