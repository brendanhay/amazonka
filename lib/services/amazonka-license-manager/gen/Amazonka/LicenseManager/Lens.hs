{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManager.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Lens
  ( -- * Operations

    -- ** AcceptGrant
    acceptGrant_grantArn,
    acceptGrantResponse_grantArn,
    acceptGrantResponse_status,
    acceptGrantResponse_version,
    acceptGrantResponse_httpStatus,

    -- ** CheckInLicense
    checkInLicense_beneficiary,
    checkInLicense_licenseConsumptionToken,
    checkInLicenseResponse_httpStatus,

    -- ** CheckoutBorrowLicense
    checkoutBorrowLicense_checkoutMetadata,
    checkoutBorrowLicense_nodeId,
    checkoutBorrowLicense_licenseArn,
    checkoutBorrowLicense_entitlements,
    checkoutBorrowLicense_digitalSignatureMethod,
    checkoutBorrowLicense_clientToken,
    checkoutBorrowLicenseResponse_checkoutMetadata,
    checkoutBorrowLicenseResponse_entitlementsAllowed,
    checkoutBorrowLicenseResponse_expiration,
    checkoutBorrowLicenseResponse_issuedAt,
    checkoutBorrowLicenseResponse_licenseArn,
    checkoutBorrowLicenseResponse_licenseConsumptionToken,
    checkoutBorrowLicenseResponse_nodeId,
    checkoutBorrowLicenseResponse_signedToken,
    checkoutBorrowLicenseResponse_httpStatus,

    -- ** CheckoutLicense
    checkoutLicense_beneficiary,
    checkoutLicense_nodeId,
    checkoutLicense_productSKU,
    checkoutLicense_checkoutType,
    checkoutLicense_keyFingerprint,
    checkoutLicense_entitlements,
    checkoutLicense_clientToken,
    checkoutLicenseResponse_checkoutType,
    checkoutLicenseResponse_entitlementsAllowed,
    checkoutLicenseResponse_expiration,
    checkoutLicenseResponse_issuedAt,
    checkoutLicenseResponse_licenseArn,
    checkoutLicenseResponse_licenseConsumptionToken,
    checkoutLicenseResponse_nodeId,
    checkoutLicenseResponse_signedToken,
    checkoutLicenseResponse_httpStatus,

    -- ** CreateGrant
    createGrant_clientToken,
    createGrant_grantName,
    createGrant_licenseArn,
    createGrant_principals,
    createGrant_homeRegion,
    createGrant_allowedOperations,
    createGrantResponse_grantArn,
    createGrantResponse_status,
    createGrantResponse_version,
    createGrantResponse_httpStatus,

    -- ** CreateGrantVersion
    createGrantVersion_allowedOperations,
    createGrantVersion_grantName,
    createGrantVersion_sourceVersion,
    createGrantVersion_status,
    createGrantVersion_statusReason,
    createGrantVersion_clientToken,
    createGrantVersion_grantArn,
    createGrantVersionResponse_grantArn,
    createGrantVersionResponse_status,
    createGrantVersionResponse_version,
    createGrantVersionResponse_httpStatus,

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
    createLicenseResponse_licenseArn,
    createLicenseResponse_status,
    createLicenseResponse_version,
    createLicenseResponse_httpStatus,

    -- ** CreateLicenseConfiguration
    createLicenseConfiguration_description,
    createLicenseConfiguration_disassociateWhenNotFound,
    createLicenseConfiguration_licenseCount,
    createLicenseConfiguration_licenseCountHardLimit,
    createLicenseConfiguration_licenseRules,
    createLicenseConfiguration_productInformationList,
    createLicenseConfiguration_tags,
    createLicenseConfiguration_name,
    createLicenseConfiguration_licenseCountingType,
    createLicenseConfigurationResponse_licenseConfigurationArn,
    createLicenseConfigurationResponse_httpStatus,

    -- ** CreateLicenseConversionTaskForResource
    createLicenseConversionTaskForResource_resourceArn,
    createLicenseConversionTaskForResource_sourceLicenseContext,
    createLicenseConversionTaskForResource_destinationLicenseContext,
    createLicenseConversionTaskForResourceResponse_licenseConversionTaskId,
    createLicenseConversionTaskForResourceResponse_httpStatus,

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

    -- ** CreateLicenseVersion
    createLicenseVersion_licenseMetadata,
    createLicenseVersion_sourceVersion,
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
    createLicenseVersionResponse_licenseArn,
    createLicenseVersionResponse_status,
    createLicenseVersionResponse_version,
    createLicenseVersionResponse_httpStatus,

    -- ** CreateToken
    createToken_expirationInDays,
    createToken_roleArns,
    createToken_tokenProperties,
    createToken_licenseArn,
    createToken_clientToken,
    createTokenResponse_token,
    createTokenResponse_tokenId,
    createTokenResponse_tokenType,
    createTokenResponse_httpStatus,

    -- ** DeleteGrant
    deleteGrant_statusReason,
    deleteGrant_grantArn,
    deleteGrant_version,
    deleteGrantResponse_grantArn,
    deleteGrantResponse_status,
    deleteGrantResponse_version,
    deleteGrantResponse_httpStatus,

    -- ** DeleteLicense
    deleteLicense_licenseArn,
    deleteLicense_sourceVersion,
    deleteLicenseResponse_deletionDate,
    deleteLicenseResponse_status,
    deleteLicenseResponse_httpStatus,

    -- ** DeleteLicenseConfiguration
    deleteLicenseConfiguration_licenseConfigurationArn,
    deleteLicenseConfigurationResponse_httpStatus,

    -- ** DeleteLicenseManagerReportGenerator
    deleteLicenseManagerReportGenerator_licenseManagerReportGeneratorArn,
    deleteLicenseManagerReportGeneratorResponse_httpStatus,

    -- ** DeleteToken
    deleteToken_tokenId,
    deleteTokenResponse_httpStatus,

    -- ** ExtendLicenseConsumption
    extendLicenseConsumption_dryRun,
    extendLicenseConsumption_licenseConsumptionToken,
    extendLicenseConsumptionResponse_expiration,
    extendLicenseConsumptionResponse_licenseConsumptionToken,
    extendLicenseConsumptionResponse_httpStatus,

    -- ** GetAccessToken
    getAccessToken_tokenProperties,
    getAccessToken_token,
    getAccessTokenResponse_accessToken,
    getAccessTokenResponse_httpStatus,

    -- ** GetGrant
    getGrant_version,
    getGrant_grantArn,
    getGrantResponse_grant,
    getGrantResponse_httpStatus,

    -- ** GetLicense
    getLicense_version,
    getLicense_licenseArn,
    getLicenseResponse_license,
    getLicenseResponse_httpStatus,

    -- ** GetLicenseConfiguration
    getLicenseConfiguration_licenseConfigurationArn,
    getLicenseConfigurationResponse_automatedDiscoveryInformation,
    getLicenseConfigurationResponse_consumedLicenseSummaryList,
    getLicenseConfigurationResponse_consumedLicenses,
    getLicenseConfigurationResponse_description,
    getLicenseConfigurationResponse_disassociateWhenNotFound,
    getLicenseConfigurationResponse_licenseConfigurationArn,
    getLicenseConfigurationResponse_licenseConfigurationId,
    getLicenseConfigurationResponse_licenseCount,
    getLicenseConfigurationResponse_licenseCountHardLimit,
    getLicenseConfigurationResponse_licenseCountingType,
    getLicenseConfigurationResponse_licenseRules,
    getLicenseConfigurationResponse_managedResourceSummaryList,
    getLicenseConfigurationResponse_name,
    getLicenseConfigurationResponse_ownerAccountId,
    getLicenseConfigurationResponse_productInformationList,
    getLicenseConfigurationResponse_status,
    getLicenseConfigurationResponse_tags,
    getLicenseConfigurationResponse_httpStatus,

    -- ** GetLicenseConversionTask
    getLicenseConversionTask_licenseConversionTaskId,
    getLicenseConversionTaskResponse_destinationLicenseContext,
    getLicenseConversionTaskResponse_endTime,
    getLicenseConversionTaskResponse_licenseConversionTaskId,
    getLicenseConversionTaskResponse_licenseConversionTime,
    getLicenseConversionTaskResponse_resourceArn,
    getLicenseConversionTaskResponse_sourceLicenseContext,
    getLicenseConversionTaskResponse_startTime,
    getLicenseConversionTaskResponse_status,
    getLicenseConversionTaskResponse_statusMessage,
    getLicenseConversionTaskResponse_httpStatus,

    -- ** GetLicenseManagerReportGenerator
    getLicenseManagerReportGenerator_licenseManagerReportGeneratorArn,
    getLicenseManagerReportGeneratorResponse_reportGenerator,
    getLicenseManagerReportGeneratorResponse_httpStatus,

    -- ** GetLicenseUsage
    getLicenseUsage_licenseArn,
    getLicenseUsageResponse_licenseUsage,
    getLicenseUsageResponse_httpStatus,

    -- ** GetServiceSettings
    getServiceSettingsResponse_enableCrossAccountsDiscovery,
    getServiceSettingsResponse_licenseManagerResourceShareArn,
    getServiceSettingsResponse_organizationConfiguration,
    getServiceSettingsResponse_s3BucketArn,
    getServiceSettingsResponse_snsTopicArn,
    getServiceSettingsResponse_httpStatus,

    -- ** ListAssociationsForLicenseConfiguration
    listAssociationsForLicenseConfiguration_maxResults,
    listAssociationsForLicenseConfiguration_nextToken,
    listAssociationsForLicenseConfiguration_licenseConfigurationArn,
    listAssociationsForLicenseConfigurationResponse_licenseConfigurationAssociations,
    listAssociationsForLicenseConfigurationResponse_nextToken,
    listAssociationsForLicenseConfigurationResponse_httpStatus,

    -- ** ListDistributedGrants
    listDistributedGrants_filters,
    listDistributedGrants_grantArns,
    listDistributedGrants_maxResults,
    listDistributedGrants_nextToken,
    listDistributedGrantsResponse_grants,
    listDistributedGrantsResponse_nextToken,
    listDistributedGrantsResponse_httpStatus,

    -- ** ListFailuresForLicenseConfigurationOperations
    listFailuresForLicenseConfigurationOperations_maxResults,
    listFailuresForLicenseConfigurationOperations_nextToken,
    listFailuresForLicenseConfigurationOperations_licenseConfigurationArn,
    listFailuresForLicenseConfigurationOperationsResponse_licenseOperationFailureList,
    listFailuresForLicenseConfigurationOperationsResponse_nextToken,
    listFailuresForLicenseConfigurationOperationsResponse_httpStatus,

    -- ** ListLicenseConfigurations
    listLicenseConfigurations_filters,
    listLicenseConfigurations_licenseConfigurationArns,
    listLicenseConfigurations_maxResults,
    listLicenseConfigurations_nextToken,
    listLicenseConfigurationsResponse_licenseConfigurations,
    listLicenseConfigurationsResponse_nextToken,
    listLicenseConfigurationsResponse_httpStatus,

    -- ** ListLicenseConversionTasks
    listLicenseConversionTasks_filters,
    listLicenseConversionTasks_maxResults,
    listLicenseConversionTasks_nextToken,
    listLicenseConversionTasksResponse_licenseConversionTasks,
    listLicenseConversionTasksResponse_nextToken,
    listLicenseConversionTasksResponse_httpStatus,

    -- ** ListLicenseManagerReportGenerators
    listLicenseManagerReportGenerators_filters,
    listLicenseManagerReportGenerators_maxResults,
    listLicenseManagerReportGenerators_nextToken,
    listLicenseManagerReportGeneratorsResponse_nextToken,
    listLicenseManagerReportGeneratorsResponse_reportGenerators,
    listLicenseManagerReportGeneratorsResponse_httpStatus,

    -- ** ListLicenseSpecificationsForResource
    listLicenseSpecificationsForResource_maxResults,
    listLicenseSpecificationsForResource_nextToken,
    listLicenseSpecificationsForResource_resourceArn,
    listLicenseSpecificationsForResourceResponse_licenseSpecifications,
    listLicenseSpecificationsForResourceResponse_nextToken,
    listLicenseSpecificationsForResourceResponse_httpStatus,

    -- ** ListLicenseVersions
    listLicenseVersions_maxResults,
    listLicenseVersions_nextToken,
    listLicenseVersions_licenseArn,
    listLicenseVersionsResponse_licenses,
    listLicenseVersionsResponse_nextToken,
    listLicenseVersionsResponse_httpStatus,

    -- ** ListLicenses
    listLicenses_filters,
    listLicenses_licenseArns,
    listLicenses_maxResults,
    listLicenses_nextToken,
    listLicensesResponse_licenses,
    listLicensesResponse_nextToken,
    listLicensesResponse_httpStatus,

    -- ** ListReceivedGrants
    listReceivedGrants_filters,
    listReceivedGrants_grantArns,
    listReceivedGrants_maxResults,
    listReceivedGrants_nextToken,
    listReceivedGrantsResponse_grants,
    listReceivedGrantsResponse_nextToken,
    listReceivedGrantsResponse_httpStatus,

    -- ** ListReceivedGrantsForOrganization
    listReceivedGrantsForOrganization_filters,
    listReceivedGrantsForOrganization_maxResults,
    listReceivedGrantsForOrganization_nextToken,
    listReceivedGrantsForOrganization_licenseArn,
    listReceivedGrantsForOrganizationResponse_grants,
    listReceivedGrantsForOrganizationResponse_nextToken,
    listReceivedGrantsForOrganizationResponse_httpStatus,

    -- ** ListReceivedLicenses
    listReceivedLicenses_filters,
    listReceivedLicenses_licenseArns,
    listReceivedLicenses_maxResults,
    listReceivedLicenses_nextToken,
    listReceivedLicensesResponse_licenses,
    listReceivedLicensesResponse_nextToken,
    listReceivedLicensesResponse_httpStatus,

    -- ** ListReceivedLicensesForOrganization
    listReceivedLicensesForOrganization_filters,
    listReceivedLicensesForOrganization_maxResults,
    listReceivedLicensesForOrganization_nextToken,
    listReceivedLicensesForOrganizationResponse_licenses,
    listReceivedLicensesForOrganizationResponse_nextToken,
    listReceivedLicensesForOrganizationResponse_httpStatus,

    -- ** ListResourceInventory
    listResourceInventory_filters,
    listResourceInventory_maxResults,
    listResourceInventory_nextToken,
    listResourceInventoryResponse_nextToken,
    listResourceInventoryResponse_resourceInventoryList,
    listResourceInventoryResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTokens
    listTokens_filters,
    listTokens_maxResults,
    listTokens_nextToken,
    listTokens_tokenIds,
    listTokensResponse_nextToken,
    listTokensResponse_tokens,
    listTokensResponse_httpStatus,

    -- ** ListUsageForLicenseConfiguration
    listUsageForLicenseConfiguration_filters,
    listUsageForLicenseConfiguration_maxResults,
    listUsageForLicenseConfiguration_nextToken,
    listUsageForLicenseConfiguration_licenseConfigurationArn,
    listUsageForLicenseConfigurationResponse_licenseConfigurationUsageList,
    listUsageForLicenseConfigurationResponse_nextToken,
    listUsageForLicenseConfigurationResponse_httpStatus,

    -- ** RejectGrant
    rejectGrant_grantArn,
    rejectGrantResponse_grantArn,
    rejectGrantResponse_status,
    rejectGrantResponse_version,
    rejectGrantResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateLicenseConfiguration
    updateLicenseConfiguration_description,
    updateLicenseConfiguration_disassociateWhenNotFound,
    updateLicenseConfiguration_licenseConfigurationStatus,
    updateLicenseConfiguration_licenseCount,
    updateLicenseConfiguration_licenseCountHardLimit,
    updateLicenseConfiguration_licenseRules,
    updateLicenseConfiguration_name,
    updateLicenseConfiguration_productInformationList,
    updateLicenseConfiguration_licenseConfigurationArn,
    updateLicenseConfigurationResponse_httpStatus,

    -- ** UpdateLicenseManagerReportGenerator
    updateLicenseManagerReportGenerator_description,
    updateLicenseManagerReportGenerator_licenseManagerReportGeneratorArn,
    updateLicenseManagerReportGenerator_reportGeneratorName,
    updateLicenseManagerReportGenerator_type,
    updateLicenseManagerReportGenerator_reportContext,
    updateLicenseManagerReportGenerator_reportFrequency,
    updateLicenseManagerReportGenerator_clientToken,
    updateLicenseManagerReportGeneratorResponse_httpStatus,

    -- ** UpdateLicenseSpecificationsForResource
    updateLicenseSpecificationsForResource_addLicenseSpecifications,
    updateLicenseSpecificationsForResource_removeLicenseSpecifications,
    updateLicenseSpecificationsForResource_resourceArn,
    updateLicenseSpecificationsForResourceResponse_httpStatus,

    -- ** UpdateServiceSettings
    updateServiceSettings_enableCrossAccountsDiscovery,
    updateServiceSettings_organizationConfiguration,
    updateServiceSettings_s3BucketArn,
    updateServiceSettings_snsTopicArn,
    updateServiceSettingsResponse_httpStatus,

    -- * Types

    -- ** AutomatedDiscoveryInformation
    automatedDiscoveryInformation_lastRunTime,

    -- ** BorrowConfiguration
    borrowConfiguration_allowEarlyCheckIn,
    borrowConfiguration_maxTimeToLiveInMinutes,

    -- ** ConsumedLicenseSummary
    consumedLicenseSummary_consumedLicenses,
    consumedLicenseSummary_resourceType,

    -- ** ConsumptionConfiguration
    consumptionConfiguration_borrowConfiguration,
    consumptionConfiguration_provisionalConfiguration,
    consumptionConfiguration_renewType,

    -- ** DatetimeRange
    datetimeRange_end,
    datetimeRange_begin,

    -- ** Entitlement
    entitlement_allowCheckIn,
    entitlement_maxCount,
    entitlement_overage,
    entitlement_value,
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
    filter_name,
    filter_values,

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

    -- ** InventoryFilter
    inventoryFilter_value,
    inventoryFilter_name,
    inventoryFilter_condition,

    -- ** Issuer
    issuer_signKey,
    issuer_name,

    -- ** IssuerDetails
    issuerDetails_keyFingerprint,
    issuerDetails_name,
    issuerDetails_signKey,

    -- ** License
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

    -- ** LicenseConfiguration
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

    -- ** LicenseConfigurationAssociation
    licenseConfigurationAssociation_amiAssociationScope,
    licenseConfigurationAssociation_associationTime,
    licenseConfigurationAssociation_resourceArn,
    licenseConfigurationAssociation_resourceOwnerId,
    licenseConfigurationAssociation_resourceType,

    -- ** LicenseConfigurationUsage
    licenseConfigurationUsage_associationTime,
    licenseConfigurationUsage_consumedLicenses,
    licenseConfigurationUsage_resourceArn,
    licenseConfigurationUsage_resourceOwnerId,
    licenseConfigurationUsage_resourceStatus,
    licenseConfigurationUsage_resourceType,

    -- ** LicenseConversionContext
    licenseConversionContext_usageOperation,

    -- ** LicenseConversionTask
    licenseConversionTask_destinationLicenseContext,
    licenseConversionTask_endTime,
    licenseConversionTask_licenseConversionTaskId,
    licenseConversionTask_licenseConversionTime,
    licenseConversionTask_resourceArn,
    licenseConversionTask_sourceLicenseContext,
    licenseConversionTask_startTime,
    licenseConversionTask_status,
    licenseConversionTask_statusMessage,

    -- ** LicenseOperationFailure
    licenseOperationFailure_errorMessage,
    licenseOperationFailure_failureTime,
    licenseOperationFailure_metadataList,
    licenseOperationFailure_operationName,
    licenseOperationFailure_operationRequestedBy,
    licenseOperationFailure_resourceArn,
    licenseOperationFailure_resourceOwnerId,
    licenseOperationFailure_resourceType,

    -- ** LicenseSpecification
    licenseSpecification_amiAssociationScope,
    licenseSpecification_licenseConfigurationArn,

    -- ** LicenseUsage
    licenseUsage_entitlementUsages,

    -- ** ManagedResourceSummary
    managedResourceSummary_associationCount,
    managedResourceSummary_resourceType,

    -- ** Metadata
    metadata_name,
    metadata_value,

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
    receivedMetadata_allowedOperations,
    receivedMetadata_receivedStatus,
    receivedMetadata_receivedStatusReason,

    -- ** ReportContext
    reportContext_licenseConfigurationArns,

    -- ** ReportFrequency
    reportFrequency_period,
    reportFrequency_value,

    -- ** ReportGenerator
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

    -- ** ResourceInventory
    resourceInventory_platform,
    resourceInventory_platformVersion,
    resourceInventory_resourceArn,
    resourceInventory_resourceId,
    resourceInventory_resourceOwningAccountId,
    resourceInventory_resourceType,

    -- ** S3Location
    s3Location_bucket,
    s3Location_keyPrefix,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TokenData
    tokenData_expirationTime,
    tokenData_licenseArn,
    tokenData_roleArns,
    tokenData_status,
    tokenData_tokenId,
    tokenData_tokenProperties,
    tokenData_tokenType,
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
import Amazonka.LicenseManager.ListReceivedGrantsForOrganization
import Amazonka.LicenseManager.ListReceivedLicenses
import Amazonka.LicenseManager.ListReceivedLicensesForOrganization
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
