{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManager.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Lens
  ( -- * Operations

    -- ** ListLicenseManagerReportGenerators
    listLicenseManagerReportGenerators_filters,
    listLicenseManagerReportGenerators_nextToken,
    listLicenseManagerReportGenerators_maxResults,
    listLicenseManagerReportGeneratorsResponse_nextToken,
    listLicenseManagerReportGeneratorsResponse_reportGenerators,
    listLicenseManagerReportGeneratorsResponse_httpStatus,

    -- ** DeleteLicenseManagerReportGenerator
    deleteLicenseManagerReportGenerator_licenseManagerReportGeneratorArn,
    deleteLicenseManagerReportGeneratorResponse_httpStatus,

    -- ** UpdateLicenseManagerReportGenerator
    updateLicenseManagerReportGenerator_description,
    updateLicenseManagerReportGenerator_licenseManagerReportGeneratorArn,
    updateLicenseManagerReportGenerator_reportGeneratorName,
    updateLicenseManagerReportGenerator_type,
    updateLicenseManagerReportGenerator_reportContext,
    updateLicenseManagerReportGenerator_reportFrequency,
    updateLicenseManagerReportGenerator_clientToken,
    updateLicenseManagerReportGeneratorResponse_httpStatus,

    -- ** ListUsageForLicenseConfiguration
    listUsageForLicenseConfiguration_filters,
    listUsageForLicenseConfiguration_nextToken,
    listUsageForLicenseConfiguration_maxResults,
    listUsageForLicenseConfiguration_licenseConfigurationArn,
    listUsageForLicenseConfigurationResponse_nextToken,
    listUsageForLicenseConfigurationResponse_licenseConfigurationUsageList,
    listUsageForLicenseConfigurationResponse_httpStatus,

    -- ** CreateLicenseConfiguration
    createLicenseConfiguration_licenseCount,
    createLicenseConfiguration_licenseCountHardLimit,
    createLicenseConfiguration_disassociateWhenNotFound,
    createLicenseConfiguration_productInformationList,
    createLicenseConfiguration_licenseRules,
    createLicenseConfiguration_description,
    createLicenseConfiguration_tags,
    createLicenseConfiguration_name,
    createLicenseConfiguration_licenseCountingType,
    createLicenseConfigurationResponse_licenseConfigurationArn,
    createLicenseConfigurationResponse_httpStatus,

    -- ** CreateLicense
    createLicense_licenseMetadata,
    createLicense_licenseName,
    createLicense_productName,
    createLicense_productSKU,
    createLicense_issuer,
    createLicense_homeRegion,
    createLicense_validity,
    createLicense_entitlements,
    createLicense_beneficiary,
    createLicense_consumptionConfiguration,
    createLicense_clientToken,
    createLicenseResponse_status,
    createLicenseResponse_version,
    createLicenseResponse_licenseArn,
    createLicenseResponse_httpStatus,

    -- ** ListLicenseConversionTasks
    listLicenseConversionTasks_filters,
    listLicenseConversionTasks_nextToken,
    listLicenseConversionTasks_maxResults,
    listLicenseConversionTasksResponse_licenseConversionTasks,
    listLicenseConversionTasksResponse_nextToken,
    listLicenseConversionTasksResponse_httpStatus,

    -- ** ListResourceInventory
    listResourceInventory_filters,
    listResourceInventory_nextToken,
    listResourceInventory_maxResults,
    listResourceInventoryResponse_resourceInventoryList,
    listResourceInventoryResponse_nextToken,
    listResourceInventoryResponse_httpStatus,

    -- ** DeleteToken
    deleteToken_tokenId,
    deleteTokenResponse_httpStatus,

    -- ** DeleteLicenseConfiguration
    deleteLicenseConfiguration_licenseConfigurationArn,
    deleteLicenseConfigurationResponse_httpStatus,

    -- ** UpdateLicenseConfiguration
    updateLicenseConfiguration_licenseCount,
    updateLicenseConfiguration_name,
    updateLicenseConfiguration_licenseConfigurationStatus,
    updateLicenseConfiguration_licenseCountHardLimit,
    updateLicenseConfiguration_disassociateWhenNotFound,
    updateLicenseConfiguration_productInformationList,
    updateLicenseConfiguration_licenseRules,
    updateLicenseConfiguration_description,
    updateLicenseConfiguration_licenseConfigurationArn,
    updateLicenseConfigurationResponse_httpStatus,

    -- ** CheckInLicense
    checkInLicense_beneficiary,
    checkInLicense_licenseConsumptionToken,
    checkInLicenseResponse_httpStatus,

    -- ** ListTokens
    listTokens_tokenIds,
    listTokens_filters,
    listTokens_nextToken,
    listTokens_maxResults,
    listTokensResponse_tokens,
    listTokensResponse_nextToken,
    listTokensResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateGrant
    createGrant_clientToken,
    createGrant_grantName,
    createGrant_licenseArn,
    createGrant_principals,
    createGrant_homeRegion,
    createGrant_allowedOperations,
    createGrantResponse_status,
    createGrantResponse_version,
    createGrantResponse_grantArn,
    createGrantResponse_httpStatus,

    -- ** UpdateLicenseSpecificationsForResource
    updateLicenseSpecificationsForResource_addLicenseSpecifications,
    updateLicenseSpecificationsForResource_removeLicenseSpecifications,
    updateLicenseSpecificationsForResource_resourceArn,
    updateLicenseSpecificationsForResourceResponse_httpStatus,

    -- ** CreateLicenseVersion
    createLicenseVersion_sourceVersion,
    createLicenseVersion_licenseMetadata,
    createLicenseVersion_licenseArn,
    createLicenseVersion_licenseName,
    createLicenseVersion_productName,
    createLicenseVersion_issuer,
    createLicenseVersion_homeRegion,
    createLicenseVersion_validity,
    createLicenseVersion_entitlements,
    createLicenseVersion_consumptionConfiguration,
    createLicenseVersion_status,
    createLicenseVersion_clientToken,
    createLicenseVersionResponse_status,
    createLicenseVersionResponse_version,
    createLicenseVersionResponse_licenseArn,
    createLicenseVersionResponse_httpStatus,

    -- ** GetLicense
    getLicense_version,
    getLicense_licenseArn,
    getLicenseResponse_license,
    getLicenseResponse_httpStatus,

    -- ** GetLicenseConfiguration
    getLicenseConfiguration_licenseConfigurationArn,
    getLicenseConfigurationResponse_status,
    getLicenseConfigurationResponse_ownerAccountId,
    getLicenseConfigurationResponse_consumedLicenseSummaryList,
    getLicenseConfigurationResponse_licenseCount,
    getLicenseConfigurationResponse_managedResourceSummaryList,
    getLicenseConfigurationResponse_name,
    getLicenseConfigurationResponse_licenseCountHardLimit,
    getLicenseConfigurationResponse_disassociateWhenNotFound,
    getLicenseConfigurationResponse_productInformationList,
    getLicenseConfigurationResponse_licenseCountingType,
    getLicenseConfigurationResponse_automatedDiscoveryInformation,
    getLicenseConfigurationResponse_consumedLicenses,
    getLicenseConfigurationResponse_licenseRules,
    getLicenseConfigurationResponse_licenseConfigurationId,
    getLicenseConfigurationResponse_description,
    getLicenseConfigurationResponse_licenseConfigurationArn,
    getLicenseConfigurationResponse_tags,
    getLicenseConfigurationResponse_httpStatus,

    -- ** ListReceivedGrants
    listReceivedGrants_filters,
    listReceivedGrants_nextToken,
    listReceivedGrants_grantArns,
    listReceivedGrants_maxResults,
    listReceivedGrantsResponse_grants,
    listReceivedGrantsResponse_nextToken,
    listReceivedGrantsResponse_httpStatus,

    -- ** GetLicenseConversionTask
    getLicenseConversionTask_licenseConversionTaskId,
    getLicenseConversionTaskResponse_status,
    getLicenseConversionTaskResponse_startTime,
    getLicenseConversionTaskResponse_destinationLicenseContext,
    getLicenseConversionTaskResponse_licenseConversionTaskId,
    getLicenseConversionTaskResponse_resourceArn,
    getLicenseConversionTaskResponse_statusMessage,
    getLicenseConversionTaskResponse_endTime,
    getLicenseConversionTaskResponse_licenseConversionTime,
    getLicenseConversionTaskResponse_sourceLicenseContext,
    getLicenseConversionTaskResponse_httpStatus,

    -- ** GetLicenseUsage
    getLicenseUsage_licenseArn,
    getLicenseUsageResponse_licenseUsage,
    getLicenseUsageResponse_httpStatus,

    -- ** ExtendLicenseConsumption
    extendLicenseConsumption_dryRun,
    extendLicenseConsumption_licenseConsumptionToken,
    extendLicenseConsumptionResponse_expiration,
    extendLicenseConsumptionResponse_licenseConsumptionToken,
    extendLicenseConsumptionResponse_httpStatus,

    -- ** GetGrant
    getGrant_version,
    getGrant_grantArn,
    getGrantResponse_grant,
    getGrantResponse_httpStatus,

    -- ** CheckoutLicense
    checkoutLicense_nodeId,
    checkoutLicense_beneficiary,
    checkoutLicense_productSKU,
    checkoutLicense_checkoutType,
    checkoutLicense_keyFingerprint,
    checkoutLicense_entitlements,
    checkoutLicense_clientToken,
    checkoutLicenseResponse_entitlementsAllowed,
    checkoutLicenseResponse_checkoutType,
    checkoutLicenseResponse_expiration,
    checkoutLicenseResponse_signedToken,
    checkoutLicenseResponse_licenseConsumptionToken,
    checkoutLicenseResponse_nodeId,
    checkoutLicenseResponse_issuedAt,
    checkoutLicenseResponse_licenseArn,
    checkoutLicenseResponse_httpStatus,

    -- ** CreateLicenseConversionTaskForResource
    createLicenseConversionTaskForResource_resourceArn,
    createLicenseConversionTaskForResource_sourceLicenseContext,
    createLicenseConversionTaskForResource_destinationLicenseContext,
    createLicenseConversionTaskForResourceResponse_licenseConversionTaskId,
    createLicenseConversionTaskForResourceResponse_httpStatus,

    -- ** AcceptGrant
    acceptGrant_grantArn,
    acceptGrantResponse_status,
    acceptGrantResponse_version,
    acceptGrantResponse_grantArn,
    acceptGrantResponse_httpStatus,

    -- ** ListLicenseSpecificationsForResource
    listLicenseSpecificationsForResource_nextToken,
    listLicenseSpecificationsForResource_maxResults,
    listLicenseSpecificationsForResource_resourceArn,
    listLicenseSpecificationsForResourceResponse_licenseSpecifications,
    listLicenseSpecificationsForResourceResponse_nextToken,
    listLicenseSpecificationsForResourceResponse_httpStatus,

    -- ** CheckoutBorrowLicense
    checkoutBorrowLicense_checkoutMetadata,
    checkoutBorrowLicense_nodeId,
    checkoutBorrowLicense_licenseArn,
    checkoutBorrowLicense_entitlements,
    checkoutBorrowLicense_digitalSignatureMethod,
    checkoutBorrowLicense_clientToken,
    checkoutBorrowLicenseResponse_entitlementsAllowed,
    checkoutBorrowLicenseResponse_checkoutMetadata,
    checkoutBorrowLicenseResponse_expiration,
    checkoutBorrowLicenseResponse_signedToken,
    checkoutBorrowLicenseResponse_licenseConsumptionToken,
    checkoutBorrowLicenseResponse_nodeId,
    checkoutBorrowLicenseResponse_issuedAt,
    checkoutBorrowLicenseResponse_licenseArn,
    checkoutBorrowLicenseResponse_httpStatus,

    -- ** GetServiceSettings
    getServiceSettingsResponse_enableCrossAccountsDiscovery,
    getServiceSettingsResponse_snsTopicArn,
    getServiceSettingsResponse_licenseManagerResourceShareArn,
    getServiceSettingsResponse_s3BucketArn,
    getServiceSettingsResponse_organizationConfiguration,
    getServiceSettingsResponse_httpStatus,

    -- ** RejectGrant
    rejectGrant_grantArn,
    rejectGrantResponse_status,
    rejectGrantResponse_version,
    rejectGrantResponse_grantArn,
    rejectGrantResponse_httpStatus,

    -- ** UpdateServiceSettings
    updateServiceSettings_enableCrossAccountsDiscovery,
    updateServiceSettings_snsTopicArn,
    updateServiceSettings_s3BucketArn,
    updateServiceSettings_organizationConfiguration,
    updateServiceSettingsResponse_httpStatus,

    -- ** ListDistributedGrants
    listDistributedGrants_filters,
    listDistributedGrants_nextToken,
    listDistributedGrants_grantArns,
    listDistributedGrants_maxResults,
    listDistributedGrantsResponse_grants,
    listDistributedGrantsResponse_nextToken,
    listDistributedGrantsResponse_httpStatus,

    -- ** ListFailuresForLicenseConfigurationOperations
    listFailuresForLicenseConfigurationOperations_nextToken,
    listFailuresForLicenseConfigurationOperations_maxResults,
    listFailuresForLicenseConfigurationOperations_licenseConfigurationArn,
    listFailuresForLicenseConfigurationOperationsResponse_nextToken,
    listFailuresForLicenseConfigurationOperationsResponse_licenseOperationFailureList,
    listFailuresForLicenseConfigurationOperationsResponse_httpStatus,

    -- ** DeleteGrant
    deleteGrant_statusReason,
    deleteGrant_grantArn,
    deleteGrant_version,
    deleteGrantResponse_status,
    deleteGrantResponse_version,
    deleteGrantResponse_grantArn,
    deleteGrantResponse_httpStatus,

    -- ** CreateToken
    createToken_tokenProperties,
    createToken_roleArns,
    createToken_expirationInDays,
    createToken_licenseArn,
    createToken_clientToken,
    createTokenResponse_token,
    createTokenResponse_tokenId,
    createTokenResponse_tokenType,
    createTokenResponse_httpStatus,

    -- ** DeleteLicense
    deleteLicense_licenseArn,
    deleteLicense_sourceVersion,
    deleteLicenseResponse_status,
    deleteLicenseResponse_deletionDate,
    deleteLicenseResponse_httpStatus,

    -- ** ListLicenses
    listLicenses_filters,
    listLicenses_nextToken,
    listLicenses_licenseArns,
    listLicenses_maxResults,
    listLicensesResponse_nextToken,
    listLicensesResponse_licenses,
    listLicensesResponse_httpStatus,

    -- ** ListLicenseConfigurations
    listLicenseConfigurations_filters,
    listLicenseConfigurations_nextToken,
    listLicenseConfigurations_licenseConfigurationArns,
    listLicenseConfigurations_maxResults,
    listLicenseConfigurationsResponse_nextToken,
    listLicenseConfigurationsResponse_licenseConfigurations,
    listLicenseConfigurationsResponse_httpStatus,

    -- ** ListReceivedLicenses
    listReceivedLicenses_filters,
    listReceivedLicenses_nextToken,
    listReceivedLicenses_licenseArns,
    listReceivedLicenses_maxResults,
    listReceivedLicensesResponse_nextToken,
    listReceivedLicensesResponse_licenses,
    listReceivedLicensesResponse_httpStatus,

    -- ** CreateGrantVersion
    createGrantVersion_status,
    createGrantVersion_allowedOperations,
    createGrantVersion_grantName,
    createGrantVersion_sourceVersion,
    createGrantVersion_statusReason,
    createGrantVersion_clientToken,
    createGrantVersion_grantArn,
    createGrantVersionResponse_status,
    createGrantVersionResponse_version,
    createGrantVersionResponse_grantArn,
    createGrantVersionResponse_httpStatus,

    -- ** ListAssociationsForLicenseConfiguration
    listAssociationsForLicenseConfiguration_nextToken,
    listAssociationsForLicenseConfiguration_maxResults,
    listAssociationsForLicenseConfiguration_licenseConfigurationArn,
    listAssociationsForLicenseConfigurationResponse_licenseConfigurationAssociations,
    listAssociationsForLicenseConfigurationResponse_nextToken,
    listAssociationsForLicenseConfigurationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListLicenseVersions
    listLicenseVersions_nextToken,
    listLicenseVersions_maxResults,
    listLicenseVersions_licenseArn,
    listLicenseVersionsResponse_nextToken,
    listLicenseVersionsResponse_licenses,
    listLicenseVersionsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetLicenseManagerReportGenerator
    getLicenseManagerReportGenerator_licenseManagerReportGeneratorArn,
    getLicenseManagerReportGeneratorResponse_reportGenerator,
    getLicenseManagerReportGeneratorResponse_httpStatus,

    -- ** GetAccessToken
    getAccessToken_tokenProperties,
    getAccessToken_token,
    getAccessTokenResponse_accessToken,
    getAccessTokenResponse_httpStatus,

    -- ** CreateLicenseManagerReportGenerator
    createLicenseManagerReportGenerator_description,
    createLicenseManagerReportGenerator_tags,
    createLicenseManagerReportGenerator_reportGeneratorName,
    createLicenseManagerReportGenerator_type,
    createLicenseManagerReportGenerator_reportContext,
    createLicenseManagerReportGenerator_reportFrequency,
    createLicenseManagerReportGenerator_clientToken,
    createLicenseManagerReportGeneratorResponse_licenseManagerReportGeneratorArn,
    createLicenseManagerReportGeneratorResponse_httpStatus,

    -- * Types

    -- ** AutomatedDiscoveryInformation
    automatedDiscoveryInformation_lastRunTime,

    -- ** BorrowConfiguration
    borrowConfiguration_allowEarlyCheckIn,
    borrowConfiguration_maxTimeToLiveInMinutes,

    -- ** ConsumedLicenseSummary
    consumedLicenseSummary_resourceType,
    consumedLicenseSummary_consumedLicenses,

    -- ** ConsumptionConfiguration
    consumptionConfiguration_borrowConfiguration,
    consumptionConfiguration_provisionalConfiguration,
    consumptionConfiguration_renewType,

    -- ** DatetimeRange
    datetimeRange_end,
    datetimeRange_begin,

    -- ** Entitlement
    entitlement_maxCount,
    entitlement_value,
    entitlement_overage,
    entitlement_allowCheckIn,
    entitlement_name,
    entitlement_unit,

    -- ** EntitlementData
    entitlementData_value,
    entitlementData_name,
    entitlementData_unit,

    -- ** EntitlementUsage
    entitlementUsage_maxCount,
    entitlementUsage_name,
    entitlementUsage_consumedValue,
    entitlementUsage_unit,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** Grant
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

    -- ** GrantedLicense
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

    -- ** InventoryFilter
    inventoryFilter_value,
    inventoryFilter_name,
    inventoryFilter_condition,

    -- ** Issuer
    issuer_signKey,
    issuer_name,

    -- ** IssuerDetails
    issuerDetails_keyFingerprint,
    issuerDetails_signKey,
    issuerDetails_name,

    -- ** License
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

    -- ** LicenseConfiguration
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

    -- ** LicenseConfigurationAssociation
    licenseConfigurationAssociation_resourceType,
    licenseConfigurationAssociation_amiAssociationScope,
    licenseConfigurationAssociation_associationTime,
    licenseConfigurationAssociation_resourceArn,
    licenseConfigurationAssociation_resourceOwnerId,

    -- ** LicenseConfigurationUsage
    licenseConfigurationUsage_resourceType,
    licenseConfigurationUsage_resourceStatus,
    licenseConfigurationUsage_associationTime,
    licenseConfigurationUsage_resourceArn,
    licenseConfigurationUsage_consumedLicenses,
    licenseConfigurationUsage_resourceOwnerId,

    -- ** LicenseConversionContext
    licenseConversionContext_usageOperation,

    -- ** LicenseConversionTask
    licenseConversionTask_status,
    licenseConversionTask_startTime,
    licenseConversionTask_destinationLicenseContext,
    licenseConversionTask_licenseConversionTaskId,
    licenseConversionTask_resourceArn,
    licenseConversionTask_statusMessage,
    licenseConversionTask_endTime,
    licenseConversionTask_licenseConversionTime,
    licenseConversionTask_sourceLicenseContext,

    -- ** LicenseOperationFailure
    licenseOperationFailure_resourceType,
    licenseOperationFailure_operationRequestedBy,
    licenseOperationFailure_resourceArn,
    licenseOperationFailure_metadataList,
    licenseOperationFailure_operationName,
    licenseOperationFailure_failureTime,
    licenseOperationFailure_errorMessage,
    licenseOperationFailure_resourceOwnerId,

    -- ** LicenseSpecification
    licenseSpecification_amiAssociationScope,
    licenseSpecification_licenseConfigurationArn,

    -- ** LicenseUsage
    licenseUsage_entitlementUsages,

    -- ** ManagedResourceSummary
    managedResourceSummary_associationCount,
    managedResourceSummary_resourceType,

    -- ** Metadata
    metadata_value,
    metadata_name,

    -- ** OrganizationConfiguration
    organizationConfiguration_enableIntegration,

    -- ** ProductInformation
    productInformation_resourceType,
    productInformation_productInformationFilterList,

    -- ** ProductInformationFilter
    productInformationFilter_productInformationFilterValue,
    productInformationFilter_productInformationFilterName,
    productInformationFilter_productInformationFilterComparator,

    -- ** ProvisionalConfiguration
    provisionalConfiguration_maxTimeToLiveInMinutes,

    -- ** ReceivedMetadata
    receivedMetadata_receivedStatus,
    receivedMetadata_allowedOperations,
    receivedMetadata_receivedStatusReason,

    -- ** ReportContext
    reportContext_licenseConfigurationArns,

    -- ** ReportFrequency
    reportFrequency_period,
    reportFrequency_value,

    -- ** ReportGenerator
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

    -- ** ResourceInventory
    resourceInventory_platform,
    resourceInventory_resourceId,
    resourceInventory_resourceType,
    resourceInventory_platformVersion,
    resourceInventory_resourceArn,
    resourceInventory_resourceOwningAccountId,

    -- ** S3Location
    s3Location_bucket,
    s3Location_keyPrefix,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TokenData
    tokenData_status,
    tokenData_tokenId,
    tokenData_tokenProperties,
    tokenData_roleArns,
    tokenData_tokenType,
    tokenData_expirationTime,
    tokenData_licenseArn,
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
import Amazonka.LicenseManager.ListReceivedLicenses
import Amazonka.LicenseManager.ListResourceInventory
import Amazonka.LicenseManager.ListTagsForResource
import Amazonka.LicenseManager.ListTokens
import Amazonka.LicenseManager.ListUsageForLicenseConfiguration
import Amazonka.LicenseManager.RejectGrant
import Amazonka.LicenseManager.TagResource
import Amazonka.LicenseManager.Types.AutomatedDiscoveryInformation
import Amazonka.LicenseManager.Types.BorrowConfiguration
import Amazonka.LicenseManager.Types.ConsumedLicenseSummary
import Amazonka.LicenseManager.Types.ConsumptionConfiguration
import Amazonka.LicenseManager.Types.DatetimeRange
import Amazonka.LicenseManager.Types.Entitlement
import Amazonka.LicenseManager.Types.EntitlementData
import Amazonka.LicenseManager.Types.EntitlementUsage
import Amazonka.LicenseManager.Types.Filter
import Amazonka.LicenseManager.Types.Grant
import Amazonka.LicenseManager.Types.GrantedLicense
import Amazonka.LicenseManager.Types.InventoryFilter
import Amazonka.LicenseManager.Types.Issuer
import Amazonka.LicenseManager.Types.IssuerDetails
import Amazonka.LicenseManager.Types.License
import Amazonka.LicenseManager.Types.LicenseConfiguration
import Amazonka.LicenseManager.Types.LicenseConfigurationAssociation
import Amazonka.LicenseManager.Types.LicenseConfigurationUsage
import Amazonka.LicenseManager.Types.LicenseConversionContext
import Amazonka.LicenseManager.Types.LicenseConversionTask
import Amazonka.LicenseManager.Types.LicenseOperationFailure
import Amazonka.LicenseManager.Types.LicenseSpecification
import Amazonka.LicenseManager.Types.LicenseUsage
import Amazonka.LicenseManager.Types.ManagedResourceSummary
import Amazonka.LicenseManager.Types.Metadata
import Amazonka.LicenseManager.Types.OrganizationConfiguration
import Amazonka.LicenseManager.Types.ProductInformation
import Amazonka.LicenseManager.Types.ProductInformationFilter
import Amazonka.LicenseManager.Types.ProvisionalConfiguration
import Amazonka.LicenseManager.Types.ReceivedMetadata
import Amazonka.LicenseManager.Types.ReportContext
import Amazonka.LicenseManager.Types.ReportFrequency
import Amazonka.LicenseManager.Types.ReportGenerator
import Amazonka.LicenseManager.Types.ResourceInventory
import Amazonka.LicenseManager.Types.S3Location
import Amazonka.LicenseManager.Types.Tag
import Amazonka.LicenseManager.Types.TokenData
import Amazonka.LicenseManager.UntagResource
import Amazonka.LicenseManager.UpdateLicenseConfiguration
import Amazonka.LicenseManager.UpdateLicenseManagerReportGenerator
import Amazonka.LicenseManager.UpdateLicenseSpecificationsForResource
import Amazonka.LicenseManager.UpdateServiceSettings
