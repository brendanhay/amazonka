{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.LicenseManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-08-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- License Manager makes it easier to manage licenses from software vendors
-- across multiple Amazon Web Services accounts and on-premises servers.
module Network.AWS.LicenseManager
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NoEntitlementsAllowedException
    _NoEntitlementsAllowedException,

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** InvalidResourceStateException
    _InvalidResourceStateException,

    -- ** RateLimitExceededException
    _RateLimitExceededException,

    -- ** FailedDependencyException
    _FailedDependencyException,

    -- ** ConflictException
    _ConflictException,

    -- ** FilterLimitExceededException
    _FilterLimitExceededException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** AuthorizationException
    _AuthorizationException,

    -- ** RedirectException
    _RedirectException,

    -- ** ServerInternalException
    _ServerInternalException,

    -- ** EntitlementNotAllowedException
    _EntitlementNotAllowedException,

    -- ** UnsupportedDigitalSignatureMethodException
    _UnsupportedDigitalSignatureMethodException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LicenseUsageException
    _LicenseUsageException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListLicenseManagerReportGenerators
    ListLicenseManagerReportGenerators (ListLicenseManagerReportGenerators'),
    newListLicenseManagerReportGenerators,
    ListLicenseManagerReportGeneratorsResponse (ListLicenseManagerReportGeneratorsResponse'),
    newListLicenseManagerReportGeneratorsResponse,

    -- ** DeleteLicenseManagerReportGenerator
    DeleteLicenseManagerReportGenerator (DeleteLicenseManagerReportGenerator'),
    newDeleteLicenseManagerReportGenerator,
    DeleteLicenseManagerReportGeneratorResponse (DeleteLicenseManagerReportGeneratorResponse'),
    newDeleteLicenseManagerReportGeneratorResponse,

    -- ** UpdateLicenseManagerReportGenerator
    UpdateLicenseManagerReportGenerator (UpdateLicenseManagerReportGenerator'),
    newUpdateLicenseManagerReportGenerator,
    UpdateLicenseManagerReportGeneratorResponse (UpdateLicenseManagerReportGeneratorResponse'),
    newUpdateLicenseManagerReportGeneratorResponse,

    -- ** ListUsageForLicenseConfiguration (Paginated)
    ListUsageForLicenseConfiguration (ListUsageForLicenseConfiguration'),
    newListUsageForLicenseConfiguration,
    ListUsageForLicenseConfigurationResponse (ListUsageForLicenseConfigurationResponse'),
    newListUsageForLicenseConfigurationResponse,

    -- ** CreateLicenseConfiguration
    CreateLicenseConfiguration (CreateLicenseConfiguration'),
    newCreateLicenseConfiguration,
    CreateLicenseConfigurationResponse (CreateLicenseConfigurationResponse'),
    newCreateLicenseConfigurationResponse,

    -- ** CreateLicense
    CreateLicense (CreateLicense'),
    newCreateLicense,
    CreateLicenseResponse (CreateLicenseResponse'),
    newCreateLicenseResponse,

    -- ** ListLicenseConversionTasks
    ListLicenseConversionTasks (ListLicenseConversionTasks'),
    newListLicenseConversionTasks,
    ListLicenseConversionTasksResponse (ListLicenseConversionTasksResponse'),
    newListLicenseConversionTasksResponse,

    -- ** ListResourceInventory (Paginated)
    ListResourceInventory (ListResourceInventory'),
    newListResourceInventory,
    ListResourceInventoryResponse (ListResourceInventoryResponse'),
    newListResourceInventoryResponse,

    -- ** DeleteToken
    DeleteToken (DeleteToken'),
    newDeleteToken,
    DeleteTokenResponse (DeleteTokenResponse'),
    newDeleteTokenResponse,

    -- ** DeleteLicenseConfiguration
    DeleteLicenseConfiguration (DeleteLicenseConfiguration'),
    newDeleteLicenseConfiguration,
    DeleteLicenseConfigurationResponse (DeleteLicenseConfigurationResponse'),
    newDeleteLicenseConfigurationResponse,

    -- ** UpdateLicenseConfiguration
    UpdateLicenseConfiguration (UpdateLicenseConfiguration'),
    newUpdateLicenseConfiguration,
    UpdateLicenseConfigurationResponse (UpdateLicenseConfigurationResponse'),
    newUpdateLicenseConfigurationResponse,

    -- ** CheckInLicense
    CheckInLicense (CheckInLicense'),
    newCheckInLicense,
    CheckInLicenseResponse (CheckInLicenseResponse'),
    newCheckInLicenseResponse,

    -- ** ListTokens
    ListTokens (ListTokens'),
    newListTokens,
    ListTokensResponse (ListTokensResponse'),
    newListTokensResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateGrant
    CreateGrant (CreateGrant'),
    newCreateGrant,
    CreateGrantResponse (CreateGrantResponse'),
    newCreateGrantResponse,

    -- ** UpdateLicenseSpecificationsForResource
    UpdateLicenseSpecificationsForResource (UpdateLicenseSpecificationsForResource'),
    newUpdateLicenseSpecificationsForResource,
    UpdateLicenseSpecificationsForResourceResponse (UpdateLicenseSpecificationsForResourceResponse'),
    newUpdateLicenseSpecificationsForResourceResponse,

    -- ** CreateLicenseVersion
    CreateLicenseVersion (CreateLicenseVersion'),
    newCreateLicenseVersion,
    CreateLicenseVersionResponse (CreateLicenseVersionResponse'),
    newCreateLicenseVersionResponse,

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

    -- ** ListReceivedGrants
    ListReceivedGrants (ListReceivedGrants'),
    newListReceivedGrants,
    ListReceivedGrantsResponse (ListReceivedGrantsResponse'),
    newListReceivedGrantsResponse,

    -- ** GetLicenseConversionTask
    GetLicenseConversionTask (GetLicenseConversionTask'),
    newGetLicenseConversionTask,
    GetLicenseConversionTaskResponse (GetLicenseConversionTaskResponse'),
    newGetLicenseConversionTaskResponse,

    -- ** GetLicenseUsage
    GetLicenseUsage (GetLicenseUsage'),
    newGetLicenseUsage,
    GetLicenseUsageResponse (GetLicenseUsageResponse'),
    newGetLicenseUsageResponse,

    -- ** ExtendLicenseConsumption
    ExtendLicenseConsumption (ExtendLicenseConsumption'),
    newExtendLicenseConsumption,
    ExtendLicenseConsumptionResponse (ExtendLicenseConsumptionResponse'),
    newExtendLicenseConsumptionResponse,

    -- ** GetGrant
    GetGrant (GetGrant'),
    newGetGrant,
    GetGrantResponse (GetGrantResponse'),
    newGetGrantResponse,

    -- ** CheckoutLicense
    CheckoutLicense (CheckoutLicense'),
    newCheckoutLicense,
    CheckoutLicenseResponse (CheckoutLicenseResponse'),
    newCheckoutLicenseResponse,

    -- ** CreateLicenseConversionTaskForResource
    CreateLicenseConversionTaskForResource (CreateLicenseConversionTaskForResource'),
    newCreateLicenseConversionTaskForResource,
    CreateLicenseConversionTaskForResourceResponse (CreateLicenseConversionTaskForResourceResponse'),
    newCreateLicenseConversionTaskForResourceResponse,

    -- ** AcceptGrant
    AcceptGrant (AcceptGrant'),
    newAcceptGrant,
    AcceptGrantResponse (AcceptGrantResponse'),
    newAcceptGrantResponse,

    -- ** ListLicenseSpecificationsForResource (Paginated)
    ListLicenseSpecificationsForResource (ListLicenseSpecificationsForResource'),
    newListLicenseSpecificationsForResource,
    ListLicenseSpecificationsForResourceResponse (ListLicenseSpecificationsForResourceResponse'),
    newListLicenseSpecificationsForResourceResponse,

    -- ** CheckoutBorrowLicense
    CheckoutBorrowLicense (CheckoutBorrowLicense'),
    newCheckoutBorrowLicense,
    CheckoutBorrowLicenseResponse (CheckoutBorrowLicenseResponse'),
    newCheckoutBorrowLicenseResponse,

    -- ** GetServiceSettings
    GetServiceSettings (GetServiceSettings'),
    newGetServiceSettings,
    GetServiceSettingsResponse (GetServiceSettingsResponse'),
    newGetServiceSettingsResponse,

    -- ** RejectGrant
    RejectGrant (RejectGrant'),
    newRejectGrant,
    RejectGrantResponse (RejectGrantResponse'),
    newRejectGrantResponse,

    -- ** UpdateServiceSettings
    UpdateServiceSettings (UpdateServiceSettings'),
    newUpdateServiceSettings,
    UpdateServiceSettingsResponse (UpdateServiceSettingsResponse'),
    newUpdateServiceSettingsResponse,

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

    -- ** DeleteGrant
    DeleteGrant (DeleteGrant'),
    newDeleteGrant,
    DeleteGrantResponse (DeleteGrantResponse'),
    newDeleteGrantResponse,

    -- ** CreateToken
    CreateToken (CreateToken'),
    newCreateToken,
    CreateTokenResponse (CreateTokenResponse'),
    newCreateTokenResponse,

    -- ** DeleteLicense
    DeleteLicense (DeleteLicense'),
    newDeleteLicense,
    DeleteLicenseResponse (DeleteLicenseResponse'),
    newDeleteLicenseResponse,

    -- ** ListLicenses
    ListLicenses (ListLicenses'),
    newListLicenses,
    ListLicensesResponse (ListLicensesResponse'),
    newListLicensesResponse,

    -- ** ListLicenseConfigurations (Paginated)
    ListLicenseConfigurations (ListLicenseConfigurations'),
    newListLicenseConfigurations,
    ListLicenseConfigurationsResponse (ListLicenseConfigurationsResponse'),
    newListLicenseConfigurationsResponse,

    -- ** ListReceivedLicenses
    ListReceivedLicenses (ListReceivedLicenses'),
    newListReceivedLicenses,
    ListReceivedLicensesResponse (ListReceivedLicensesResponse'),
    newListReceivedLicensesResponse,

    -- ** CreateGrantVersion
    CreateGrantVersion (CreateGrantVersion'),
    newCreateGrantVersion,
    CreateGrantVersionResponse (CreateGrantVersionResponse'),
    newCreateGrantVersionResponse,

    -- ** ListAssociationsForLicenseConfiguration (Paginated)
    ListAssociationsForLicenseConfiguration (ListAssociationsForLicenseConfiguration'),
    newListAssociationsForLicenseConfiguration,
    ListAssociationsForLicenseConfigurationResponse (ListAssociationsForLicenseConfigurationResponse'),
    newListAssociationsForLicenseConfigurationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListLicenseVersions
    ListLicenseVersions (ListLicenseVersions'),
    newListLicenseVersions,
    ListLicenseVersionsResponse (ListLicenseVersionsResponse'),
    newListLicenseVersionsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetLicenseManagerReportGenerator
    GetLicenseManagerReportGenerator (GetLicenseManagerReportGenerator'),
    newGetLicenseManagerReportGenerator,
    GetLicenseManagerReportGeneratorResponse (GetLicenseManagerReportGeneratorResponse'),
    newGetLicenseManagerReportGeneratorResponse,

    -- ** GetAccessToken
    GetAccessToken (GetAccessToken'),
    newGetAccessToken,
    GetAccessTokenResponse (GetAccessTokenResponse'),
    newGetAccessTokenResponse,

    -- ** CreateLicenseManagerReportGenerator
    CreateLicenseManagerReportGenerator (CreateLicenseManagerReportGenerator'),
    newCreateLicenseManagerReportGenerator,
    CreateLicenseManagerReportGeneratorResponse (CreateLicenseManagerReportGeneratorResponse'),
    newCreateLicenseManagerReportGeneratorResponse,

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

import Network.AWS.LicenseManager.AcceptGrant
import Network.AWS.LicenseManager.CheckInLicense
import Network.AWS.LicenseManager.CheckoutBorrowLicense
import Network.AWS.LicenseManager.CheckoutLicense
import Network.AWS.LicenseManager.CreateGrant
import Network.AWS.LicenseManager.CreateGrantVersion
import Network.AWS.LicenseManager.CreateLicense
import Network.AWS.LicenseManager.CreateLicenseConfiguration
import Network.AWS.LicenseManager.CreateLicenseConversionTaskForResource
import Network.AWS.LicenseManager.CreateLicenseManagerReportGenerator
import Network.AWS.LicenseManager.CreateLicenseVersion
import Network.AWS.LicenseManager.CreateToken
import Network.AWS.LicenseManager.DeleteGrant
import Network.AWS.LicenseManager.DeleteLicense
import Network.AWS.LicenseManager.DeleteLicenseConfiguration
import Network.AWS.LicenseManager.DeleteLicenseManagerReportGenerator
import Network.AWS.LicenseManager.DeleteToken
import Network.AWS.LicenseManager.ExtendLicenseConsumption
import Network.AWS.LicenseManager.GetAccessToken
import Network.AWS.LicenseManager.GetGrant
import Network.AWS.LicenseManager.GetLicense
import Network.AWS.LicenseManager.GetLicenseConfiguration
import Network.AWS.LicenseManager.GetLicenseConversionTask
import Network.AWS.LicenseManager.GetLicenseManagerReportGenerator
import Network.AWS.LicenseManager.GetLicenseUsage
import Network.AWS.LicenseManager.GetServiceSettings
import Network.AWS.LicenseManager.Lens
import Network.AWS.LicenseManager.ListAssociationsForLicenseConfiguration
import Network.AWS.LicenseManager.ListDistributedGrants
import Network.AWS.LicenseManager.ListFailuresForLicenseConfigurationOperations
import Network.AWS.LicenseManager.ListLicenseConfigurations
import Network.AWS.LicenseManager.ListLicenseConversionTasks
import Network.AWS.LicenseManager.ListLicenseManagerReportGenerators
import Network.AWS.LicenseManager.ListLicenseSpecificationsForResource
import Network.AWS.LicenseManager.ListLicenseVersions
import Network.AWS.LicenseManager.ListLicenses
import Network.AWS.LicenseManager.ListReceivedGrants
import Network.AWS.LicenseManager.ListReceivedLicenses
import Network.AWS.LicenseManager.ListResourceInventory
import Network.AWS.LicenseManager.ListTagsForResource
import Network.AWS.LicenseManager.ListTokens
import Network.AWS.LicenseManager.ListUsageForLicenseConfiguration
import Network.AWS.LicenseManager.RejectGrant
import Network.AWS.LicenseManager.TagResource
import Network.AWS.LicenseManager.Types
import Network.AWS.LicenseManager.UntagResource
import Network.AWS.LicenseManager.UpdateLicenseConfiguration
import Network.AWS.LicenseManager.UpdateLicenseManagerReportGenerator
import Network.AWS.LicenseManager.UpdateLicenseSpecificationsForResource
import Network.AWS.LicenseManager.UpdateServiceSettings
import Network.AWS.LicenseManager.Waiters

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
