{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManager.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _AuthorizationException,
    _ConflictException,
    _EntitlementNotAllowedException,
    _FailedDependencyException,
    _FilterLimitExceededException,
    _InvalidParameterValueException,
    _InvalidResourceStateException,
    _LicenseUsageException,
    _NoEntitlementsAllowedException,
    _RateLimitExceededException,
    _RedirectException,
    _ResourceLimitExceededException,
    _ResourceNotFoundException,
    _ServerInternalException,
    _UnsupportedDigitalSignatureMethodException,
    _ValidationException,

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
    consumedLicenseSummary_consumedLicenses,
    consumedLicenseSummary_resourceType,

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
    entitlement_allowCheckIn,
    entitlement_maxCount,
    entitlement_overage,
    entitlement_value,
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
    filter_name,
    filter_values,

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
    grantedLicense_beneficiary,
    grantedLicense_consumptionConfiguration,
    grantedLicense_createTime,
    grantedLicense_entitlements,
    grantedLicense_homeRegion,
    grantedLicense_issuer,
    grantedLicense_licenseArn,
    grantedLicense_licenseMetadata,
    grantedLicense_licenseName,
    grantedLicense_productName,
    grantedLicense_productSKU,
    grantedLicense_receivedMetadata,
    grantedLicense_status,
    grantedLicense_validity,
    grantedLicense_version,

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
    issuerDetails_name,
    issuerDetails_signKey,

    -- * License
    License (..),
    newLicense,
    license_beneficiary,
    license_consumptionConfiguration,
    license_createTime,
    license_entitlements,
    license_homeRegion,
    license_issuer,
    license_licenseArn,
    license_licenseMetadata,
    license_licenseName,
    license_productName,
    license_productSKU,
    license_status,
    license_validity,
    license_version,

    -- * LicenseConfiguration
    LicenseConfiguration (..),
    newLicenseConfiguration,
    licenseConfiguration_automatedDiscoveryInformation,
    licenseConfiguration_consumedLicenseSummaryList,
    licenseConfiguration_consumedLicenses,
    licenseConfiguration_description,
    licenseConfiguration_disassociateWhenNotFound,
    licenseConfiguration_licenseConfigurationArn,
    licenseConfiguration_licenseConfigurationId,
    licenseConfiguration_licenseCount,
    licenseConfiguration_licenseCountHardLimit,
    licenseConfiguration_licenseCountingType,
    licenseConfiguration_licenseRules,
    licenseConfiguration_managedResourceSummaryList,
    licenseConfiguration_name,
    licenseConfiguration_ownerAccountId,
    licenseConfiguration_productInformationList,
    licenseConfiguration_status,

    -- * LicenseConfigurationAssociation
    LicenseConfigurationAssociation (..),
    newLicenseConfigurationAssociation,
    licenseConfigurationAssociation_amiAssociationScope,
    licenseConfigurationAssociation_associationTime,
    licenseConfigurationAssociation_resourceArn,
    licenseConfigurationAssociation_resourceOwnerId,
    licenseConfigurationAssociation_resourceType,

    -- * LicenseConfigurationUsage
    LicenseConfigurationUsage (..),
    newLicenseConfigurationUsage,
    licenseConfigurationUsage_associationTime,
    licenseConfigurationUsage_consumedLicenses,
    licenseConfigurationUsage_resourceArn,
    licenseConfigurationUsage_resourceOwnerId,
    licenseConfigurationUsage_resourceStatus,
    licenseConfigurationUsage_resourceType,

    -- * LicenseConversionContext
    LicenseConversionContext (..),
    newLicenseConversionContext,
    licenseConversionContext_usageOperation,

    -- * LicenseConversionTask
    LicenseConversionTask (..),
    newLicenseConversionTask,
    licenseConversionTask_destinationLicenseContext,
    licenseConversionTask_endTime,
    licenseConversionTask_licenseConversionTaskId,
    licenseConversionTask_licenseConversionTime,
    licenseConversionTask_resourceArn,
    licenseConversionTask_sourceLicenseContext,
    licenseConversionTask_startTime,
    licenseConversionTask_status,
    licenseConversionTask_statusMessage,

    -- * LicenseOperationFailure
    LicenseOperationFailure (..),
    newLicenseOperationFailure,
    licenseOperationFailure_errorMessage,
    licenseOperationFailure_failureTime,
    licenseOperationFailure_metadataList,
    licenseOperationFailure_operationName,
    licenseOperationFailure_operationRequestedBy,
    licenseOperationFailure_resourceArn,
    licenseOperationFailure_resourceOwnerId,
    licenseOperationFailure_resourceType,

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
    metadata_name,
    metadata_value,

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
    receivedMetadata_allowedOperations,
    receivedMetadata_receivedStatus,
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
    reportGenerator_createTime,
    reportGenerator_description,
    reportGenerator_lastReportGenerationTime,
    reportGenerator_lastRunFailureReason,
    reportGenerator_lastRunStatus,
    reportGenerator_licenseManagerReportGeneratorArn,
    reportGenerator_reportContext,
    reportGenerator_reportCreatorAccount,
    reportGenerator_reportFrequency,
    reportGenerator_reportGeneratorName,
    reportGenerator_reportType,
    reportGenerator_s3Location,
    reportGenerator_tags,

    -- * ResourceInventory
    ResourceInventory (..),
    newResourceInventory,
    resourceInventory_platform,
    resourceInventory_platformVersion,
    resourceInventory_resourceArn,
    resourceInventory_resourceId,
    resourceInventory_resourceOwningAccountId,
    resourceInventory_resourceType,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucket,
    s3Location_keyPrefix,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TokenData
    TokenData (..),
    newTokenData,
    tokenData_expirationTime,
    tokenData_licenseArn,
    tokenData_roleArns,
    tokenData_status,
    tokenData_tokenId,
    tokenData_tokenProperties,
    tokenData_tokenType,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types.AllowedOperation
import Amazonka.LicenseManager.Types.AutomatedDiscoveryInformation
import Amazonka.LicenseManager.Types.BorrowConfiguration
import Amazonka.LicenseManager.Types.CheckoutType
import Amazonka.LicenseManager.Types.ConsumedLicenseSummary
import Amazonka.LicenseManager.Types.ConsumptionConfiguration
import Amazonka.LicenseManager.Types.DatetimeRange
import Amazonka.LicenseManager.Types.DigitalSignatureMethod
import Amazonka.LicenseManager.Types.Entitlement
import Amazonka.LicenseManager.Types.EntitlementData
import Amazonka.LicenseManager.Types.EntitlementDataUnit
import Amazonka.LicenseManager.Types.EntitlementUnit
import Amazonka.LicenseManager.Types.EntitlementUsage
import Amazonka.LicenseManager.Types.Filter
import Amazonka.LicenseManager.Types.Grant
import Amazonka.LicenseManager.Types.GrantStatus
import Amazonka.LicenseManager.Types.GrantedLicense
import Amazonka.LicenseManager.Types.InventoryFilter
import Amazonka.LicenseManager.Types.InventoryFilterCondition
import Amazonka.LicenseManager.Types.Issuer
import Amazonka.LicenseManager.Types.IssuerDetails
import Amazonka.LicenseManager.Types.License
import Amazonka.LicenseManager.Types.LicenseConfiguration
import Amazonka.LicenseManager.Types.LicenseConfigurationAssociation
import Amazonka.LicenseManager.Types.LicenseConfigurationStatus
import Amazonka.LicenseManager.Types.LicenseConfigurationUsage
import Amazonka.LicenseManager.Types.LicenseConversionContext
import Amazonka.LicenseManager.Types.LicenseConversionTask
import Amazonka.LicenseManager.Types.LicenseConversionTaskStatus
import Amazonka.LicenseManager.Types.LicenseCountingType
import Amazonka.LicenseManager.Types.LicenseDeletionStatus
import Amazonka.LicenseManager.Types.LicenseOperationFailure
import Amazonka.LicenseManager.Types.LicenseSpecification
import Amazonka.LicenseManager.Types.LicenseStatus
import Amazonka.LicenseManager.Types.LicenseUsage
import Amazonka.LicenseManager.Types.ManagedResourceSummary
import Amazonka.LicenseManager.Types.Metadata
import Amazonka.LicenseManager.Types.OrganizationConfiguration
import Amazonka.LicenseManager.Types.ProductInformation
import Amazonka.LicenseManager.Types.ProductInformationFilter
import Amazonka.LicenseManager.Types.ProvisionalConfiguration
import Amazonka.LicenseManager.Types.ReceivedMetadata
import Amazonka.LicenseManager.Types.ReceivedStatus
import Amazonka.LicenseManager.Types.RenewType
import Amazonka.LicenseManager.Types.ReportContext
import Amazonka.LicenseManager.Types.ReportFrequency
import Amazonka.LicenseManager.Types.ReportFrequencyType
import Amazonka.LicenseManager.Types.ReportGenerator
import Amazonka.LicenseManager.Types.ReportType
import Amazonka.LicenseManager.Types.ResourceInventory
import Amazonka.LicenseManager.Types.ResourceType
import Amazonka.LicenseManager.Types.S3Location
import Amazonka.LicenseManager.Types.Tag
import Amazonka.LicenseManager.Types.TokenData
import Amazonka.LicenseManager.Types.TokenType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-08-01@ of the Amazon License Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "LicenseManager",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "license-manager",
      Core.signingName = "license-manager",
      Core.version = "2018-08-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "LicenseManager",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Access to resource denied.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The Amazon Web Services user account does not have permission to perform
-- the action. Check the IAM policy associated with this account.
_AuthorizationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AuthorizationException =
  Core._MatchServiceError
    defaultService
    "AuthorizationException"

-- | There was a conflict processing the request. Try your request again.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The entitlement is not allowed.
_EntitlementNotAllowedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_EntitlementNotAllowedException =
  Core._MatchServiceError
    defaultService
    "EntitlementNotAllowedException"

-- | A dependency required to run the API is missing.
_FailedDependencyException :: Core.AsError a => Lens.Fold a Core.ServiceError
_FailedDependencyException =
  Core._MatchServiceError
    defaultService
    "FailedDependencyException"

-- | The request uses too many filters or too many filter values.
_FilterLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_FilterLimitExceededException =
  Core._MatchServiceError
    defaultService
    "FilterLimitExceededException"

-- | One or more parameter values are not valid.
_InvalidParameterValueException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | License Manager cannot allocate a license to a resource because of its
-- state.
--
-- For example, you cannot allocate a license to an instance in the process
-- of shutting down.
_InvalidResourceStateException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidResourceStateException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateException"

-- | You do not have enough licenses available to support a new resource
-- launch.
_LicenseUsageException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LicenseUsageException =
  Core._MatchServiceError
    defaultService
    "LicenseUsageException"

-- | There are no entitlements found for this license, or the entitlement
-- maximum count is reached.
_NoEntitlementsAllowedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_NoEntitlementsAllowedException =
  Core._MatchServiceError
    defaultService
    "NoEntitlementsAllowedException"

-- | Too many requests have been submitted. Try again after a brief wait.
_RateLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_RateLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RateLimitExceededException"

-- | This is not the correct Region for the resource. Try again.
_RedirectException :: Core.AsError a => Lens.Fold a Core.ServiceError
_RedirectException =
  Core._MatchServiceError
    defaultService
    "RedirectException"

-- | Your resource limits have been exceeded.
_ResourceLimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | The resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The server experienced an internal error. Try again.
_ServerInternalException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServerInternalException =
  Core._MatchServiceError
    defaultService
    "ServerInternalException"

-- | The digital signature method is unsupported. Try your request again.
_UnsupportedDigitalSignatureMethodException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UnsupportedDigitalSignatureMethodException =
  Core._MatchServiceError
    defaultService
    "UnsupportedDigitalSignatureMethodException"

-- | The provided input is not valid. Try your request again.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
