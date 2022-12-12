{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.LicenseManager
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-08-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- License Manager makes it easier to manage licenses from software vendors
-- across multiple Amazon Web Services accounts and on-premises servers.
module Amazonka.LicenseManager
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AuthorizationException
    _AuthorizationException,

    -- ** ConflictException
    _ConflictException,

    -- ** EntitlementNotAllowedException
    _EntitlementNotAllowedException,

    -- ** FailedDependencyException
    _FailedDependencyException,

    -- ** FilterLimitExceededException
    _FilterLimitExceededException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidResourceStateException
    _InvalidResourceStateException,

    -- ** LicenseUsageException
    _LicenseUsageException,

    -- ** NoEntitlementsAllowedException
    _NoEntitlementsAllowedException,

    -- ** RateLimitExceededException
    _RateLimitExceededException,

    -- ** RedirectException
    _RedirectException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServerInternalException
    _ServerInternalException,

    -- ** UnsupportedDigitalSignatureMethodException
    _UnsupportedDigitalSignatureMethodException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptGrant
    AcceptGrant (AcceptGrant'),
    newAcceptGrant,
    AcceptGrantResponse (AcceptGrantResponse'),
    newAcceptGrantResponse,

    -- ** CheckInLicense
    CheckInLicense (CheckInLicense'),
    newCheckInLicense,
    CheckInLicenseResponse (CheckInLicenseResponse'),
    newCheckInLicenseResponse,

    -- ** CheckoutBorrowLicense
    CheckoutBorrowLicense (CheckoutBorrowLicense'),
    newCheckoutBorrowLicense,
    CheckoutBorrowLicenseResponse (CheckoutBorrowLicenseResponse'),
    newCheckoutBorrowLicenseResponse,

    -- ** CheckoutLicense
    CheckoutLicense (CheckoutLicense'),
    newCheckoutLicense,
    CheckoutLicenseResponse (CheckoutLicenseResponse'),
    newCheckoutLicenseResponse,

    -- ** CreateGrant
    CreateGrant (CreateGrant'),
    newCreateGrant,
    CreateGrantResponse (CreateGrantResponse'),
    newCreateGrantResponse,

    -- ** CreateGrantVersion
    CreateGrantVersion (CreateGrantVersion'),
    newCreateGrantVersion,
    CreateGrantVersionResponse (CreateGrantVersionResponse'),
    newCreateGrantVersionResponse,

    -- ** CreateLicense
    CreateLicense (CreateLicense'),
    newCreateLicense,
    CreateLicenseResponse (CreateLicenseResponse'),
    newCreateLicenseResponse,

    -- ** CreateLicenseConfiguration
    CreateLicenseConfiguration (CreateLicenseConfiguration'),
    newCreateLicenseConfiguration,
    CreateLicenseConfigurationResponse (CreateLicenseConfigurationResponse'),
    newCreateLicenseConfigurationResponse,

    -- ** CreateLicenseConversionTaskForResource
    CreateLicenseConversionTaskForResource (CreateLicenseConversionTaskForResource'),
    newCreateLicenseConversionTaskForResource,
    CreateLicenseConversionTaskForResourceResponse (CreateLicenseConversionTaskForResourceResponse'),
    newCreateLicenseConversionTaskForResourceResponse,

    -- ** CreateLicenseManagerReportGenerator
    CreateLicenseManagerReportGenerator (CreateLicenseManagerReportGenerator'),
    newCreateLicenseManagerReportGenerator,
    CreateLicenseManagerReportGeneratorResponse (CreateLicenseManagerReportGeneratorResponse'),
    newCreateLicenseManagerReportGeneratorResponse,

    -- ** CreateLicenseVersion
    CreateLicenseVersion (CreateLicenseVersion'),
    newCreateLicenseVersion,
    CreateLicenseVersionResponse (CreateLicenseVersionResponse'),
    newCreateLicenseVersionResponse,

    -- ** CreateToken
    CreateToken (CreateToken'),
    newCreateToken,
    CreateTokenResponse (CreateTokenResponse'),
    newCreateTokenResponse,

    -- ** DeleteGrant
    DeleteGrant (DeleteGrant'),
    newDeleteGrant,
    DeleteGrantResponse (DeleteGrantResponse'),
    newDeleteGrantResponse,

    -- ** DeleteLicense
    DeleteLicense (DeleteLicense'),
    newDeleteLicense,
    DeleteLicenseResponse (DeleteLicenseResponse'),
    newDeleteLicenseResponse,

    -- ** DeleteLicenseConfiguration
    DeleteLicenseConfiguration (DeleteLicenseConfiguration'),
    newDeleteLicenseConfiguration,
    DeleteLicenseConfigurationResponse (DeleteLicenseConfigurationResponse'),
    newDeleteLicenseConfigurationResponse,

    -- ** DeleteLicenseManagerReportGenerator
    DeleteLicenseManagerReportGenerator (DeleteLicenseManagerReportGenerator'),
    newDeleteLicenseManagerReportGenerator,
    DeleteLicenseManagerReportGeneratorResponse (DeleteLicenseManagerReportGeneratorResponse'),
    newDeleteLicenseManagerReportGeneratorResponse,

    -- ** DeleteToken
    DeleteToken (DeleteToken'),
    newDeleteToken,
    DeleteTokenResponse (DeleteTokenResponse'),
    newDeleteTokenResponse,

    -- ** ExtendLicenseConsumption
    ExtendLicenseConsumption (ExtendLicenseConsumption'),
    newExtendLicenseConsumption,
    ExtendLicenseConsumptionResponse (ExtendLicenseConsumptionResponse'),
    newExtendLicenseConsumptionResponse,

    -- ** GetAccessToken
    GetAccessToken (GetAccessToken'),
    newGetAccessToken,
    GetAccessTokenResponse (GetAccessTokenResponse'),
    newGetAccessTokenResponse,

    -- ** GetGrant
    GetGrant (GetGrant'),
    newGetGrant,
    GetGrantResponse (GetGrantResponse'),
    newGetGrantResponse,

    -- ** GetLicense
    GetLicense (GetLicense'),
    newGetLicense,
    GetLicenseResponse (GetLicenseResponse'),
    newGetLicenseResponse,

    -- ** GetLicenseConfiguration
    GetLicenseConfiguration (GetLicenseConfiguration'),
    newGetLicenseConfiguration,
    GetLicenseConfigurationResponse (GetLicenseConfigurationResponse'),
    newGetLicenseConfigurationResponse,

    -- ** GetLicenseConversionTask
    GetLicenseConversionTask (GetLicenseConversionTask'),
    newGetLicenseConversionTask,
    GetLicenseConversionTaskResponse (GetLicenseConversionTaskResponse'),
    newGetLicenseConversionTaskResponse,

    -- ** GetLicenseManagerReportGenerator
    GetLicenseManagerReportGenerator (GetLicenseManagerReportGenerator'),
    newGetLicenseManagerReportGenerator,
    GetLicenseManagerReportGeneratorResponse (GetLicenseManagerReportGeneratorResponse'),
    newGetLicenseManagerReportGeneratorResponse,

    -- ** GetLicenseUsage
    GetLicenseUsage (GetLicenseUsage'),
    newGetLicenseUsage,
    GetLicenseUsageResponse (GetLicenseUsageResponse'),
    newGetLicenseUsageResponse,

    -- ** GetServiceSettings
    GetServiceSettings (GetServiceSettings'),
    newGetServiceSettings,
    GetServiceSettingsResponse (GetServiceSettingsResponse'),
    newGetServiceSettingsResponse,

    -- ** ListAssociationsForLicenseConfiguration (Paginated)
    ListAssociationsForLicenseConfiguration (ListAssociationsForLicenseConfiguration'),
    newListAssociationsForLicenseConfiguration,
    ListAssociationsForLicenseConfigurationResponse (ListAssociationsForLicenseConfigurationResponse'),
    newListAssociationsForLicenseConfigurationResponse,

    -- ** ListDistributedGrants
    ListDistributedGrants (ListDistributedGrants'),
    newListDistributedGrants,
    ListDistributedGrantsResponse (ListDistributedGrantsResponse'),
    newListDistributedGrantsResponse,

    -- ** ListFailuresForLicenseConfigurationOperations
    ListFailuresForLicenseConfigurationOperations (ListFailuresForLicenseConfigurationOperations'),
    newListFailuresForLicenseConfigurationOperations,
    ListFailuresForLicenseConfigurationOperationsResponse (ListFailuresForLicenseConfigurationOperationsResponse'),
    newListFailuresForLicenseConfigurationOperationsResponse,

    -- ** ListLicenseConfigurations (Paginated)
    ListLicenseConfigurations (ListLicenseConfigurations'),
    newListLicenseConfigurations,
    ListLicenseConfigurationsResponse (ListLicenseConfigurationsResponse'),
    newListLicenseConfigurationsResponse,

    -- ** ListLicenseConversionTasks
    ListLicenseConversionTasks (ListLicenseConversionTasks'),
    newListLicenseConversionTasks,
    ListLicenseConversionTasksResponse (ListLicenseConversionTasksResponse'),
    newListLicenseConversionTasksResponse,

    -- ** ListLicenseManagerReportGenerators
    ListLicenseManagerReportGenerators (ListLicenseManagerReportGenerators'),
    newListLicenseManagerReportGenerators,
    ListLicenseManagerReportGeneratorsResponse (ListLicenseManagerReportGeneratorsResponse'),
    newListLicenseManagerReportGeneratorsResponse,

    -- ** ListLicenseSpecificationsForResource (Paginated)
    ListLicenseSpecificationsForResource (ListLicenseSpecificationsForResource'),
    newListLicenseSpecificationsForResource,
    ListLicenseSpecificationsForResourceResponse (ListLicenseSpecificationsForResourceResponse'),
    newListLicenseSpecificationsForResourceResponse,

    -- ** ListLicenseVersions
    ListLicenseVersions (ListLicenseVersions'),
    newListLicenseVersions,
    ListLicenseVersionsResponse (ListLicenseVersionsResponse'),
    newListLicenseVersionsResponse,

    -- ** ListLicenses
    ListLicenses (ListLicenses'),
    newListLicenses,
    ListLicensesResponse (ListLicensesResponse'),
    newListLicensesResponse,

    -- ** ListReceivedGrants
    ListReceivedGrants (ListReceivedGrants'),
    newListReceivedGrants,
    ListReceivedGrantsResponse (ListReceivedGrantsResponse'),
    newListReceivedGrantsResponse,

    -- ** ListReceivedGrantsForOrganization
    ListReceivedGrantsForOrganization (ListReceivedGrantsForOrganization'),
    newListReceivedGrantsForOrganization,
    ListReceivedGrantsForOrganizationResponse (ListReceivedGrantsForOrganizationResponse'),
    newListReceivedGrantsForOrganizationResponse,

    -- ** ListReceivedLicenses
    ListReceivedLicenses (ListReceivedLicenses'),
    newListReceivedLicenses,
    ListReceivedLicensesResponse (ListReceivedLicensesResponse'),
    newListReceivedLicensesResponse,

    -- ** ListReceivedLicensesForOrganization
    ListReceivedLicensesForOrganization (ListReceivedLicensesForOrganization'),
    newListReceivedLicensesForOrganization,
    ListReceivedLicensesForOrganizationResponse (ListReceivedLicensesForOrganizationResponse'),
    newListReceivedLicensesForOrganizationResponse,

    -- ** ListResourceInventory (Paginated)
    ListResourceInventory (ListResourceInventory'),
    newListResourceInventory,
    ListResourceInventoryResponse (ListResourceInventoryResponse'),
    newListResourceInventoryResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTokens
    ListTokens (ListTokens'),
    newListTokens,
    ListTokensResponse (ListTokensResponse'),
    newListTokensResponse,

    -- ** ListUsageForLicenseConfiguration (Paginated)
    ListUsageForLicenseConfiguration (ListUsageForLicenseConfiguration'),
    newListUsageForLicenseConfiguration,
    ListUsageForLicenseConfigurationResponse (ListUsageForLicenseConfigurationResponse'),
    newListUsageForLicenseConfigurationResponse,

    -- ** RejectGrant
    RejectGrant (RejectGrant'),
    newRejectGrant,
    RejectGrantResponse (RejectGrantResponse'),
    newRejectGrantResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateLicenseConfiguration
    UpdateLicenseConfiguration (UpdateLicenseConfiguration'),
    newUpdateLicenseConfiguration,
    UpdateLicenseConfigurationResponse (UpdateLicenseConfigurationResponse'),
    newUpdateLicenseConfigurationResponse,

    -- ** UpdateLicenseManagerReportGenerator
    UpdateLicenseManagerReportGenerator (UpdateLicenseManagerReportGenerator'),
    newUpdateLicenseManagerReportGenerator,
    UpdateLicenseManagerReportGeneratorResponse (UpdateLicenseManagerReportGeneratorResponse'),
    newUpdateLicenseManagerReportGeneratorResponse,

    -- ** UpdateLicenseSpecificationsForResource
    UpdateLicenseSpecificationsForResource (UpdateLicenseSpecificationsForResource'),
    newUpdateLicenseSpecificationsForResource,
    UpdateLicenseSpecificationsForResourceResponse (UpdateLicenseSpecificationsForResourceResponse'),
    newUpdateLicenseSpecificationsForResourceResponse,

    -- ** UpdateServiceSettings
    UpdateServiceSettings (UpdateServiceSettings'),
    newUpdateServiceSettings,
    UpdateServiceSettingsResponse (UpdateServiceSettingsResponse'),
    newUpdateServiceSettingsResponse,

    -- * Types

    -- ** AllowedOperation
    AllowedOperation (..),

    -- ** CheckoutType
    CheckoutType (..),

    -- ** DigitalSignatureMethod
    DigitalSignatureMethod (..),

    -- ** EntitlementDataUnit
    EntitlementDataUnit (..),

    -- ** EntitlementUnit
    EntitlementUnit (..),

    -- ** GrantStatus
    GrantStatus (..),

    -- ** InventoryFilterCondition
    InventoryFilterCondition (..),

    -- ** LicenseConfigurationStatus
    LicenseConfigurationStatus (..),

    -- ** LicenseConversionTaskStatus
    LicenseConversionTaskStatus (..),

    -- ** LicenseCountingType
    LicenseCountingType (..),

    -- ** LicenseDeletionStatus
    LicenseDeletionStatus (..),

    -- ** LicenseStatus
    LicenseStatus (..),

    -- ** ReceivedStatus
    ReceivedStatus (..),

    -- ** RenewType
    RenewType (..),

    -- ** ReportFrequencyType
    ReportFrequencyType (..),

    -- ** ReportType
    ReportType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** TokenType
    TokenType (..),

    -- ** AutomatedDiscoveryInformation
    AutomatedDiscoveryInformation (AutomatedDiscoveryInformation'),
    newAutomatedDiscoveryInformation,

    -- ** BorrowConfiguration
    BorrowConfiguration (BorrowConfiguration'),
    newBorrowConfiguration,

    -- ** ConsumedLicenseSummary
    ConsumedLicenseSummary (ConsumedLicenseSummary'),
    newConsumedLicenseSummary,

    -- ** ConsumptionConfiguration
    ConsumptionConfiguration (ConsumptionConfiguration'),
    newConsumptionConfiguration,

    -- ** DatetimeRange
    DatetimeRange (DatetimeRange'),
    newDatetimeRange,

    -- ** Entitlement
    Entitlement (Entitlement'),
    newEntitlement,

    -- ** EntitlementData
    EntitlementData (EntitlementData'),
    newEntitlementData,

    -- ** EntitlementUsage
    EntitlementUsage (EntitlementUsage'),
    newEntitlementUsage,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** Grant
    Grant (Grant'),
    newGrant,

    -- ** GrantedLicense
    GrantedLicense (GrantedLicense'),
    newGrantedLicense,

    -- ** InventoryFilter
    InventoryFilter (InventoryFilter'),
    newInventoryFilter,

    -- ** Issuer
    Issuer (Issuer'),
    newIssuer,

    -- ** IssuerDetails
    IssuerDetails (IssuerDetails'),
    newIssuerDetails,

    -- ** License
    License (License'),
    newLicense,

    -- ** LicenseConfiguration
    LicenseConfiguration (LicenseConfiguration'),
    newLicenseConfiguration,

    -- ** LicenseConfigurationAssociation
    LicenseConfigurationAssociation (LicenseConfigurationAssociation'),
    newLicenseConfigurationAssociation,

    -- ** LicenseConfigurationUsage
    LicenseConfigurationUsage (LicenseConfigurationUsage'),
    newLicenseConfigurationUsage,

    -- ** LicenseConversionContext
    LicenseConversionContext (LicenseConversionContext'),
    newLicenseConversionContext,

    -- ** LicenseConversionTask
    LicenseConversionTask (LicenseConversionTask'),
    newLicenseConversionTask,

    -- ** LicenseOperationFailure
    LicenseOperationFailure (LicenseOperationFailure'),
    newLicenseOperationFailure,

    -- ** LicenseSpecification
    LicenseSpecification (LicenseSpecification'),
    newLicenseSpecification,

    -- ** LicenseUsage
    LicenseUsage (LicenseUsage'),
    newLicenseUsage,

    -- ** ManagedResourceSummary
    ManagedResourceSummary (ManagedResourceSummary'),
    newManagedResourceSummary,

    -- ** Metadata
    Metadata (Metadata'),
    newMetadata,

    -- ** OrganizationConfiguration
    OrganizationConfiguration (OrganizationConfiguration'),
    newOrganizationConfiguration,

    -- ** ProductInformation
    ProductInformation (ProductInformation'),
    newProductInformation,

    -- ** ProductInformationFilter
    ProductInformationFilter (ProductInformationFilter'),
    newProductInformationFilter,

    -- ** ProvisionalConfiguration
    ProvisionalConfiguration (ProvisionalConfiguration'),
    newProvisionalConfiguration,

    -- ** ReceivedMetadata
    ReceivedMetadata (ReceivedMetadata'),
    newReceivedMetadata,

    -- ** ReportContext
    ReportContext (ReportContext'),
    newReportContext,

    -- ** ReportFrequency
    ReportFrequency (ReportFrequency'),
    newReportFrequency,

    -- ** ReportGenerator
    ReportGenerator (ReportGenerator'),
    newReportGenerator,

    -- ** ResourceInventory
    ResourceInventory (ResourceInventory'),
    newResourceInventory,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TokenData
    TokenData (TokenData'),
    newTokenData,
  )
where

import Amazonka.LicenseManager.AcceptGrant
import Amazonka.LicenseManager.CheckInLicense
import Amazonka.LicenseManager.CheckoutBorrowLicense
import Amazonka.LicenseManager.CheckoutLicense
import Amazonka.LicenseManager.CreateGrant
import Amazonka.LicenseManager.CreateGrantVersion
import Amazonka.LicenseManager.CreateLicense
import Amazonka.LicenseManager.CreateLicenseConfiguration
import Amazonka.LicenseManager.CreateLicenseConversionTaskForResource
import Amazonka.LicenseManager.CreateLicenseManagerReportGenerator
import Amazonka.LicenseManager.CreateLicenseVersion
import Amazonka.LicenseManager.CreateToken
import Amazonka.LicenseManager.DeleteGrant
import Amazonka.LicenseManager.DeleteLicense
import Amazonka.LicenseManager.DeleteLicenseConfiguration
import Amazonka.LicenseManager.DeleteLicenseManagerReportGenerator
import Amazonka.LicenseManager.DeleteToken
import Amazonka.LicenseManager.ExtendLicenseConsumption
import Amazonka.LicenseManager.GetAccessToken
import Amazonka.LicenseManager.GetGrant
import Amazonka.LicenseManager.GetLicense
import Amazonka.LicenseManager.GetLicenseConfiguration
import Amazonka.LicenseManager.GetLicenseConversionTask
import Amazonka.LicenseManager.GetLicenseManagerReportGenerator
import Amazonka.LicenseManager.GetLicenseUsage
import Amazonka.LicenseManager.GetServiceSettings
import Amazonka.LicenseManager.Lens
import Amazonka.LicenseManager.ListAssociationsForLicenseConfiguration
import Amazonka.LicenseManager.ListDistributedGrants
import Amazonka.LicenseManager.ListFailuresForLicenseConfigurationOperations
import Amazonka.LicenseManager.ListLicenseConfigurations
import Amazonka.LicenseManager.ListLicenseConversionTasks
import Amazonka.LicenseManager.ListLicenseManagerReportGenerators
import Amazonka.LicenseManager.ListLicenseSpecificationsForResource
import Amazonka.LicenseManager.ListLicenseVersions
import Amazonka.LicenseManager.ListLicenses
import Amazonka.LicenseManager.ListReceivedGrants
import Amazonka.LicenseManager.ListReceivedGrantsForOrganization
import Amazonka.LicenseManager.ListReceivedLicenses
import Amazonka.LicenseManager.ListReceivedLicensesForOrganization
import Amazonka.LicenseManager.ListResourceInventory
import Amazonka.LicenseManager.ListTagsForResource
import Amazonka.LicenseManager.ListTokens
import Amazonka.LicenseManager.ListUsageForLicenseConfiguration
import Amazonka.LicenseManager.RejectGrant
import Amazonka.LicenseManager.TagResource
import Amazonka.LicenseManager.Types
import Amazonka.LicenseManager.UntagResource
import Amazonka.LicenseManager.UpdateLicenseConfiguration
import Amazonka.LicenseManager.UpdateLicenseManagerReportGenerator
import Amazonka.LicenseManager.UpdateLicenseSpecificationsForResource
import Amazonka.LicenseManager.UpdateServiceSettings
import Amazonka.LicenseManager.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LicenseManager'.

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
