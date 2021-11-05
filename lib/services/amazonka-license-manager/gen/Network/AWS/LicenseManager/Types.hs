{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LicenseManager.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LicenseManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NoEntitlementsAllowedException,
    _ValidationException,
    _AccessDeniedException,
    _ResourceLimitExceededException,
    _InvalidResourceStateException,
    _RateLimitExceededException,
    _FailedDependencyException,
    _ConflictException,
    _FilterLimitExceededException,
    _InvalidParameterValueException,
    _AuthorizationException,
    _RedirectException,
    _ServerInternalException,
    _EntitlementNotAllowedException,
    _UnsupportedDigitalSignatureMethodException,
    _ResourceNotFoundException,
    _LicenseUsageException,

    -- * AllowedOperation
    AllowedOperation (..),

    -- * CheckoutType
    CheckoutType (..),

    -- * DigitalSignatureMethod
    DigitalSignatureMethod (..),

    -- * EntitlementDataUnit
    EntitlementDataUnit (..),

    -- * EntitlementUnit
    EntitlementUnit (..),

    -- * GrantStatus
    GrantStatus (..),

    -- * InventoryFilterCondition
    InventoryFilterCondition (..),

    -- * LicenseConfigurationStatus
    LicenseConfigurationStatus (..),

    -- * LicenseConversionTaskStatus
    LicenseConversionTaskStatus (..),

    -- * LicenseCountingType
    LicenseCountingType (..),

    -- * LicenseDeletionStatus
    LicenseDeletionStatus (..),

    -- * LicenseStatus
    LicenseStatus (..),

    -- * ReceivedStatus
    ReceivedStatus (..),

    -- * RenewType
    RenewType (..),

    -- * ReportFrequencyType
    ReportFrequencyType (..),

    -- * ReportType
    ReportType (..),

    -- * ResourceType
    ResourceType (..),

    -- * TokenType
    TokenType (..),

    -- * AutomatedDiscoveryInformation
    AutomatedDiscoveryInformation (..),
    newAutomatedDiscoveryInformation,
    automatedDiscoveryInformation_lastRunTime,

    -- * BorrowConfiguration
    BorrowConfiguration (..),
    newBorrowConfiguration,
    borrowConfiguration_allowEarlyCheckIn,
    borrowConfiguration_maxTimeToLiveInMinutes,

    -- * ConsumedLicenseSummary
    ConsumedLicenseSummary (..),
    newConsumedLicenseSummary,
    consumedLicenseSummary_resourceType,
    consumedLicenseSummary_consumedLicenses,

    -- * ConsumptionConfiguration
    ConsumptionConfiguration (..),
    newConsumptionConfiguration,
    consumptionConfiguration_borrowConfiguration,
    consumptionConfiguration_provisionalConfiguration,
    consumptionConfiguration_renewType,

    -- * DatetimeRange
    DatetimeRange (..),
    newDatetimeRange,
    datetimeRange_end,
    datetimeRange_begin,

    -- * Entitlement
    Entitlement (..),
    newEntitlement,
    entitlement_maxCount,
    entitlement_value,
    entitlement_overage,
    entitlement_allowCheckIn,
    entitlement_name,
    entitlement_unit,

    -- * EntitlementData
    EntitlementData (..),
    newEntitlementData,
    entitlementData_value,
    entitlementData_name,
    entitlementData_unit,

    -- * EntitlementUsage
    EntitlementUsage (..),
    newEntitlementUsage,
    entitlementUsage_maxCount,
    entitlementUsage_name,
    entitlementUsage_consumedValue,
    entitlementUsage_unit,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * Grant
    Grant (..),
    newGrant,
    grant_statusReason,
    grant_grantArn,
    grant_grantName,
    grant_parentArn,
    grant_licenseArn,
    grant_granteePrincipalArn,
    grant_homeRegion,
    grant_grantStatus,
    grant_version,
    grant_grantedOperations,

    -- * GrantedLicense
    GrantedLicense (..),
    newGrantedLicense,
    grantedLicense_status,
    grantedLicense_productName,
    grantedLicense_licenseName,
    grantedLicense_productSKU,
    grantedLicense_homeRegion,
    grantedLicense_receivedMetadata,
    grantedLicense_version,
    grantedLicense_entitlements,
    grantedLicense_validity,
    grantedLicense_consumptionConfiguration,
    grantedLicense_beneficiary,
    grantedLicense_issuer,
    grantedLicense_licenseArn,
    grantedLicense_licenseMetadata,
    grantedLicense_createTime,

    -- * InventoryFilter
    InventoryFilter (..),
    newInventoryFilter,
    inventoryFilter_value,
    inventoryFilter_name,
    inventoryFilter_condition,

    -- * Issuer
    Issuer (..),
    newIssuer,
    issuer_signKey,
    issuer_name,

    -- * IssuerDetails
    IssuerDetails (..),
    newIssuerDetails,
    issuerDetails_keyFingerprint,
    issuerDetails_signKey,
    issuerDetails_name,

    -- * License
    License (..),
    newLicense,
    license_status,
    license_productName,
    license_licenseName,
    license_productSKU,
    license_homeRegion,
    license_version,
    license_entitlements,
    license_validity,
    license_consumptionConfiguration,
    license_beneficiary,
    license_issuer,
    license_licenseArn,
    license_licenseMetadata,
    license_createTime,

    -- * LicenseConfiguration
    LicenseConfiguration (..),
    newLicenseConfiguration,
    licenseConfiguration_status,
    licenseConfiguration_ownerAccountId,
    licenseConfiguration_consumedLicenseSummaryList,
    licenseConfiguration_licenseCount,
    licenseConfiguration_managedResourceSummaryList,
    licenseConfiguration_name,
    licenseConfiguration_licenseCountHardLimit,
    licenseConfiguration_disassociateWhenNotFound,
    licenseConfiguration_productInformationList,
    licenseConfiguration_licenseCountingType,
    licenseConfiguration_automatedDiscoveryInformation,
    licenseConfiguration_consumedLicenses,
    licenseConfiguration_licenseRules,
    licenseConfiguration_licenseConfigurationId,
    licenseConfiguration_description,
    licenseConfiguration_licenseConfigurationArn,

    -- * LicenseConfigurationAssociation
    LicenseConfigurationAssociation (..),
    newLicenseConfigurationAssociation,
    licenseConfigurationAssociation_resourceType,
    licenseConfigurationAssociation_amiAssociationScope,
    licenseConfigurationAssociation_associationTime,
    licenseConfigurationAssociation_resourceArn,
    licenseConfigurationAssociation_resourceOwnerId,

    -- * LicenseConfigurationUsage
    LicenseConfigurationUsage (..),
    newLicenseConfigurationUsage,
    licenseConfigurationUsage_resourceType,
    licenseConfigurationUsage_resourceStatus,
    licenseConfigurationUsage_associationTime,
    licenseConfigurationUsage_resourceArn,
    licenseConfigurationUsage_consumedLicenses,
    licenseConfigurationUsage_resourceOwnerId,

    -- * LicenseConversionContext
    LicenseConversionContext (..),
    newLicenseConversionContext,
    licenseConversionContext_usageOperation,

    -- * LicenseConversionTask
    LicenseConversionTask (..),
    newLicenseConversionTask,
    licenseConversionTask_status,
    licenseConversionTask_startTime,
    licenseConversionTask_destinationLicenseContext,
    licenseConversionTask_licenseConversionTaskId,
    licenseConversionTask_resourceArn,
    licenseConversionTask_statusMessage,
    licenseConversionTask_endTime,
    licenseConversionTask_licenseConversionTime,
    licenseConversionTask_sourceLicenseContext,

    -- * LicenseOperationFailure
    LicenseOperationFailure (..),
    newLicenseOperationFailure,
    licenseOperationFailure_resourceType,
    licenseOperationFailure_operationRequestedBy,
    licenseOperationFailure_resourceArn,
    licenseOperationFailure_metadataList,
    licenseOperationFailure_operationName,
    licenseOperationFailure_failureTime,
    licenseOperationFailure_errorMessage,
    licenseOperationFailure_resourceOwnerId,

    -- * LicenseSpecification
    LicenseSpecification (..),
    newLicenseSpecification,
    licenseSpecification_amiAssociationScope,
    licenseSpecification_licenseConfigurationArn,

    -- * LicenseUsage
    LicenseUsage (..),
    newLicenseUsage,
    licenseUsage_entitlementUsages,

    -- * ManagedResourceSummary
    ManagedResourceSummary (..),
    newManagedResourceSummary,
    managedResourceSummary_associationCount,
    managedResourceSummary_resourceType,

    -- * Metadata
    Metadata (..),
    newMetadata,
    metadata_value,
    metadata_name,

    -- * OrganizationConfiguration
    OrganizationConfiguration (..),
    newOrganizationConfiguration,
    organizationConfiguration_enableIntegration,

    -- * ProductInformation
    ProductInformation (..),
    newProductInformation,
    productInformation_resourceType,
    productInformation_productInformationFilterList,

    -- * ProductInformationFilter
    ProductInformationFilter (..),
    newProductInformationFilter,
    productInformationFilter_productInformationFilterValue,
    productInformationFilter_productInformationFilterName,
    productInformationFilter_productInformationFilterComparator,

    -- * ProvisionalConfiguration
    ProvisionalConfiguration (..),
    newProvisionalConfiguration,
    provisionalConfiguration_maxTimeToLiveInMinutes,

    -- * ReceivedMetadata
    ReceivedMetadata (..),
    newReceivedMetadata,
    receivedMetadata_receivedStatus,
    receivedMetadata_allowedOperations,
    receivedMetadata_receivedStatusReason,

    -- * ReportContext
    ReportContext (..),
    newReportContext,
    reportContext_licenseConfigurationArns,

    -- * ReportFrequency
    ReportFrequency (..),
    newReportFrequency,
    reportFrequency_period,
    reportFrequency_value,

    -- * ReportGenerator
    ReportGenerator (..),
    newReportGenerator,
    reportGenerator_lastReportGenerationTime,
    reportGenerator_lastRunFailureReason,
    reportGenerator_lastRunStatus,
    reportGenerator_reportGeneratorName,
    reportGenerator_reportFrequency,
    reportGenerator_s3Location,
    reportGenerator_licenseManagerReportGeneratorArn,
    reportGenerator_reportCreatorAccount,
    reportGenerator_description,
    reportGenerator_reportType,
    reportGenerator_createTime,
    reportGenerator_tags,
    reportGenerator_reportContext,

    -- * ResourceInventory
    ResourceInventory (..),
    newResourceInventory,
    resourceInventory_platform,
    resourceInventory_resourceId,
    resourceInventory_resourceType,
    resourceInventory_platformVersion,
    resourceInventory_resourceArn,
    resourceInventory_resourceOwningAccountId,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucket,
    s3Location_keyPrefix,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TokenData
    TokenData (..),
    newTokenData,
    tokenData_status,
    tokenData_tokenId,
    tokenData_tokenProperties,
    tokenData_roleArns,
    tokenData_tokenType,
    tokenData_expirationTime,
    tokenData_licenseArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LicenseManager.Types.AllowedOperation
import Network.AWS.LicenseManager.Types.AutomatedDiscoveryInformation
import Network.AWS.LicenseManager.Types.BorrowConfiguration
import Network.AWS.LicenseManager.Types.CheckoutType
import Network.AWS.LicenseManager.Types.ConsumedLicenseSummary
import Network.AWS.LicenseManager.Types.ConsumptionConfiguration
import Network.AWS.LicenseManager.Types.DatetimeRange
import Network.AWS.LicenseManager.Types.DigitalSignatureMethod
import Network.AWS.LicenseManager.Types.Entitlement
import Network.AWS.LicenseManager.Types.EntitlementData
import Network.AWS.LicenseManager.Types.EntitlementDataUnit
import Network.AWS.LicenseManager.Types.EntitlementUnit
import Network.AWS.LicenseManager.Types.EntitlementUsage
import Network.AWS.LicenseManager.Types.Filter
import Network.AWS.LicenseManager.Types.Grant
import Network.AWS.LicenseManager.Types.GrantStatus
import Network.AWS.LicenseManager.Types.GrantedLicense
import Network.AWS.LicenseManager.Types.InventoryFilter
import Network.AWS.LicenseManager.Types.InventoryFilterCondition
import Network.AWS.LicenseManager.Types.Issuer
import Network.AWS.LicenseManager.Types.IssuerDetails
import Network.AWS.LicenseManager.Types.License
import Network.AWS.LicenseManager.Types.LicenseConfiguration
import Network.AWS.LicenseManager.Types.LicenseConfigurationAssociation
import Network.AWS.LicenseManager.Types.LicenseConfigurationStatus
import Network.AWS.LicenseManager.Types.LicenseConfigurationUsage
import Network.AWS.LicenseManager.Types.LicenseConversionContext
import Network.AWS.LicenseManager.Types.LicenseConversionTask
import Network.AWS.LicenseManager.Types.LicenseConversionTaskStatus
import Network.AWS.LicenseManager.Types.LicenseCountingType
import Network.AWS.LicenseManager.Types.LicenseDeletionStatus
import Network.AWS.LicenseManager.Types.LicenseOperationFailure
import Network.AWS.LicenseManager.Types.LicenseSpecification
import Network.AWS.LicenseManager.Types.LicenseStatus
import Network.AWS.LicenseManager.Types.LicenseUsage
import Network.AWS.LicenseManager.Types.ManagedResourceSummary
import Network.AWS.LicenseManager.Types.Metadata
import Network.AWS.LicenseManager.Types.OrganizationConfiguration
import Network.AWS.LicenseManager.Types.ProductInformation
import Network.AWS.LicenseManager.Types.ProductInformationFilter
import Network.AWS.LicenseManager.Types.ProvisionalConfiguration
import Network.AWS.LicenseManager.Types.ReceivedMetadata
import Network.AWS.LicenseManager.Types.ReceivedStatus
import Network.AWS.LicenseManager.Types.RenewType
import Network.AWS.LicenseManager.Types.ReportContext
import Network.AWS.LicenseManager.Types.ReportFrequency
import Network.AWS.LicenseManager.Types.ReportFrequencyType
import Network.AWS.LicenseManager.Types.ReportGenerator
import Network.AWS.LicenseManager.Types.ReportType
import Network.AWS.LicenseManager.Types.ResourceInventory
import Network.AWS.LicenseManager.Types.ResourceType
import Network.AWS.LicenseManager.Types.S3Location
import Network.AWS.LicenseManager.Types.Tag
import Network.AWS.LicenseManager.Types.TokenData
import Network.AWS.LicenseManager.Types.TokenType
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-08-01@ of the Amazon License Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "LicenseManager",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "license-manager",
      Core._serviceSigningName = "license-manager",
      Core._serviceVersion = "2018-08-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "LicenseManager",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | There are no entitlements found for this license, or the entitlement
-- maximum count is reached.
_NoEntitlementsAllowedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoEntitlementsAllowedException =
  Core._MatchServiceError
    defaultService
    "NoEntitlementsAllowedException"

-- | The provided input is not valid. Try your request again.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | Access to resource denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Your resource limits have been exceeded.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | License Manager cannot allocate a license to a resource because of its
-- state.
--
-- For example, you cannot allocate a license to an instance in the process
-- of shutting down.
_InvalidResourceStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateException"

-- | Too many requests have been submitted. Try again after a brief wait.
_RateLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RateLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RateLimitExceededException"

-- | A dependency required to run the API is missing.
_FailedDependencyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FailedDependencyException =
  Core._MatchServiceError
    defaultService
    "FailedDependencyException"

-- | There was a conflict processing the request. Try your request again.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The request uses too many filters or too many filter values.
_FilterLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FilterLimitExceededException =
  Core._MatchServiceError
    defaultService
    "FilterLimitExceededException"

-- | One or more parameter values are not valid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | The Amazon Web Services user account does not have permission to perform
-- the action. Check the IAM policy associated with this account.
_AuthorizationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationException =
  Core._MatchServiceError
    defaultService
    "AuthorizationException"

-- | This is not the correct Region for the resource. Try again.
_RedirectException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RedirectException =
  Core._MatchServiceError
    defaultService
    "RedirectException"

-- | The server experienced an internal error. Try again.
_ServerInternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerInternalException =
  Core._MatchServiceError
    defaultService
    "ServerInternalException"

-- | The entitlement is not allowed.
_EntitlementNotAllowedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntitlementNotAllowedException =
  Core._MatchServiceError
    defaultService
    "EntitlementNotAllowedException"

-- | The digital signature method is unsupported. Try your request again.
_UnsupportedDigitalSignatureMethodException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedDigitalSignatureMethodException =
  Core._MatchServiceError
    defaultService
    "UnsupportedDigitalSignatureMethodException"

-- | The resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | You do not have enough licenses available to support a new resource
-- launch.
_LicenseUsageException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LicenseUsageException =
  Core._MatchServiceError
    defaultService
    "LicenseUsageException"
