{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManager.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Lens
  ( -- * Operations

    -- ** AcceptGrant
    acceptGrant_grantArn,
    acceptGrantResponse_status,
    acceptGrantResponse_grantArn,
    acceptGrantResponse_version,
    acceptGrantResponse_httpStatus,

    -- ** CheckInLicense
    checkInLicense_beneficiary,
    checkInLicense_licenseConsumptionToken,
    checkInLicenseResponse_httpStatus,

    -- ** CheckoutBorrowLicense
    checkoutBorrowLicense_nodeId,
    checkoutBorrowLicense_checkoutMetadata,
    checkoutBorrowLicense_licenseArn,
    checkoutBorrowLicense_entitlements,
    checkoutBorrowLicense_digitalSignatureMethod,
    checkoutBorrowLicense_clientToken,
    checkoutBorrowLicenseResponse_nodeId,
    checkoutBorrowLicenseResponse_licenseArn,
    checkoutBorrowLicenseResponse_expiration,
    checkoutBorrowLicenseResponse_licenseConsumptionToken,
    checkoutBorrowLicenseResponse_signedToken,
    checkoutBorrowLicenseResponse_checkoutMetadata,
    checkoutBorrowLicenseResponse_entitlementsAllowed,
    checkoutBorrowLicenseResponse_issuedAt,
    checkoutBorrowLicenseResponse_httpStatus,

    -- ** CheckoutLicense
    checkoutLicense_nodeId,
    checkoutLicense_beneficiary,
    checkoutLicense_productSKU,
    checkoutLicense_checkoutType,
    checkoutLicense_keyFingerprint,
    checkoutLicense_entitlements,
    checkoutLicense_clientToken,
    checkoutLicenseResponse_nodeId,
    checkoutLicenseResponse_licenseArn,
    checkoutLicenseResponse_expiration,
    checkoutLicenseResponse_licenseConsumptionToken,
    checkoutLicenseResponse_checkoutType,
    checkoutLicenseResponse_signedToken,
    checkoutLicenseResponse_entitlementsAllowed,
    checkoutLicenseResponse_issuedAt,
    checkoutLicenseResponse_httpStatus,

    -- ** CreateGrant
    createGrant_clientToken,
    createGrant_grantName,
    createGrant_licenseArn,
    createGrant_principals,
    createGrant_homeRegion,
    createGrant_allowedOperations,
    createGrantResponse_status,
    createGrantResponse_grantArn,
    createGrantResponse_version,
    createGrantResponse_httpStatus,

    -- ** CreateGrantVersion
    createGrantVersion_sourceVersion,
    createGrantVersion_statusReason,
    createGrantVersion_allowedOperations,
    createGrantVersion_status,
    createGrantVersion_grantName,
    createGrantVersion_clientToken,
    createGrantVersion_grantArn,
    createGrantVersionResponse_status,
    createGrantVersionResponse_grantArn,
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
    createLicenseConfiguration_tags,
    createLicenseConfiguration_productInformationList,
    createLicenseConfiguration_licenseRules,
    createLicenseConfiguration_description,
    createLicenseConfiguration_licenseCount,
    createLicenseConfiguration_licenseCountHardLimit,
    createLicenseConfiguration_disassociateWhenNotFound,
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
    createLicenseManagerReportGenerator_tags,
    createLicenseManagerReportGenerator_description,
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
    createToken_roleArns,
    createToken_expirationInDays,
    createToken_tokenProperties,
    createToken_licenseArn,
    createToken_clientToken,
    createTokenResponse_tokenId,
    createTokenResponse_tokenType,
    createTokenResponse_token,
    createTokenResponse_httpStatus,

    -- ** DeleteGrant
    deleteGrant_statusReason,
    deleteGrant_grantArn,
    deleteGrant_version,
    deleteGrantResponse_status,
    deleteGrantResponse_grantArn,
    deleteGrantResponse_version,
    deleteGrantResponse_httpStatus,

    -- ** DeleteLicense
    deleteLicense_licenseArn,
    deleteLicense_sourceVersion,
    deleteLicenseResponse_status,
    deleteLicenseResponse_deletionDate,
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
    getLicenseConfigurationResponse_tags,
    getLicenseConfigurationResponse_name,
    getLicenseConfigurationResponse_productInformationList,
    getLicenseConfigurationResponse_licenseCountingType,
    getLicenseConfigurationResponse_licenseRules,
    getLicenseConfigurationResponse_consumedLicenses,
    getLicenseConfigurationResponse_licenseConfigurationArn,
    getLicenseConfigurationResponse_status,
    getLicenseConfigurationResponse_description,
    getLicenseConfigurationResponse_licenseConfigurationId,
    getLicenseConfigurationResponse_managedResourceSummaryList,
    getLicenseConfigurationResponse_ownerAccountId,
    getLicenseConfigurationResponse_licenseCount,
    getLicenseConfigurationResponse_licenseCountHardLimit,
    getLicenseConfigurationResponse_disassociateWhenNotFound,
    getLicenseConfigurationResponse_automatedDiscoveryInformation,
    getLicenseConfigurationResponse_consumedLicenseSummaryList,
    getLicenseConfigurationResponse_httpStatus,

    -- ** GetLicenseConversionTask
    getLicenseConversionTask_licenseConversionTaskId,
    getLicenseConversionTaskResponse_sourceLicenseContext,
    getLicenseConversionTaskResponse_licenseConversionTime,
    getLicenseConversionTaskResponse_status,
    getLicenseConversionTaskResponse_endTime,
    getLicenseConversionTaskResponse_destinationLicenseContext,
    getLicenseConversionTaskResponse_resourceArn,
    getLicenseConversionTaskResponse_licenseConversionTaskId,
    getLicenseConversionTaskResponse_statusMessage,
    getLicenseConversionTaskResponse_startTime,
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
    getServiceSettingsResponse_organizationConfiguration,
    getServiceSettingsResponse_licenseManagerResourceShareArn,
    getServiceSettingsResponse_snsTopicArn,
    getServiceSettingsResponse_enableCrossAccountsDiscovery,
    getServiceSettingsResponse_s3BucketArn,
    getServiceSettingsResponse_httpStatus,

    -- ** ListAssociationsForLicenseConfiguration
    listAssociationsForLicenseConfiguration_nextToken,
    listAssociationsForLicenseConfiguration_maxResults,
    listAssociationsForLicenseConfiguration_licenseConfigurationArn,
    listAssociationsForLicenseConfigurationResponse_nextToken,
    listAssociationsForLicenseConfigurationResponse_licenseConfigurationAssociations,
    listAssociationsForLicenseConfigurationResponse_httpStatus,

    -- ** ListDistributedGrants
    listDistributedGrants_nextToken,
    listDistributedGrants_filters,
    listDistributedGrants_grantArns,
    listDistributedGrants_maxResults,
    listDistributedGrantsResponse_nextToken,
    listDistributedGrantsResponse_grants,
    listDistributedGrantsResponse_httpStatus,

    -- ** ListFailuresForLicenseConfigurationOperations
    listFailuresForLicenseConfigurationOperations_nextToken,
    listFailuresForLicenseConfigurationOperations_maxResults,
    listFailuresForLicenseConfigurationOperations_licenseConfigurationArn,
    listFailuresForLicenseConfigurationOperationsResponse_nextToken,
    listFailuresForLicenseConfigurationOperationsResponse_licenseOperationFailureList,
    listFailuresForLicenseConfigurationOperationsResponse_httpStatus,

    -- ** ListLicenseConfigurations
    listLicenseConfigurations_nextToken,
    listLicenseConfigurations_licenseConfigurationArns,
    listLicenseConfigurations_filters,
    listLicenseConfigurations_maxResults,
    listLicenseConfigurationsResponse_licenseConfigurations,
    listLicenseConfigurationsResponse_nextToken,
    listLicenseConfigurationsResponse_httpStatus,

    -- ** ListLicenseConversionTasks
    listLicenseConversionTasks_nextToken,
    listLicenseConversionTasks_filters,
    listLicenseConversionTasks_maxResults,
    listLicenseConversionTasksResponse_nextToken,
    listLicenseConversionTasksResponse_licenseConversionTasks,
    listLicenseConversionTasksResponse_httpStatus,

    -- ** ListLicenseManagerReportGenerators
    listLicenseManagerReportGenerators_nextToken,
    listLicenseManagerReportGenerators_filters,
    listLicenseManagerReportGenerators_maxResults,
    listLicenseManagerReportGeneratorsResponse_nextToken,
    listLicenseManagerReportGeneratorsResponse_reportGenerators,
    listLicenseManagerReportGeneratorsResponse_httpStatus,

    -- ** ListLicenseSpecificationsForResource
    listLicenseSpecificationsForResource_nextToken,
    listLicenseSpecificationsForResource_maxResults,
    listLicenseSpecificationsForResource_resourceArn,
    listLicenseSpecificationsForResourceResponse_nextToken,
    listLicenseSpecificationsForResourceResponse_licenseSpecifications,
    listLicenseSpecificationsForResourceResponse_httpStatus,

    -- ** ListLicenseVersions
    listLicenseVersions_nextToken,
    listLicenseVersions_maxResults,
    listLicenseVersions_licenseArn,
    listLicenseVersionsResponse_nextToken,
    listLicenseVersionsResponse_licenses,
    listLicenseVersionsResponse_httpStatus,

    -- ** ListLicenses
    listLicenses_nextToken,
    listLicenses_filters,
    listLicenses_licenseArns,
    listLicenses_maxResults,
    listLicensesResponse_nextToken,
    listLicensesResponse_licenses,
    listLicensesResponse_httpStatus,

    -- ** ListReceivedGrants
    listReceivedGrants_nextToken,
    listReceivedGrants_filters,
    listReceivedGrants_grantArns,
    listReceivedGrants_maxResults,
    listReceivedGrantsResponse_nextToken,
    listReceivedGrantsResponse_grants,
    listReceivedGrantsResponse_httpStatus,

    -- ** ListReceivedGrantsForOrganization
    listReceivedGrantsForOrganization_nextToken,
    listReceivedGrantsForOrganization_filters,
    listReceivedGrantsForOrganization_maxResults,
    listReceivedGrantsForOrganization_licenseArn,
    listReceivedGrantsForOrganizationResponse_nextToken,
    listReceivedGrantsForOrganizationResponse_grants,
    listReceivedGrantsForOrganizationResponse_httpStatus,

    -- ** ListReceivedLicenses
    listReceivedLicenses_nextToken,
    listReceivedLicenses_filters,
    listReceivedLicenses_licenseArns,
    listReceivedLicenses_maxResults,
    listReceivedLicensesResponse_nextToken,
    listReceivedLicensesResponse_licenses,
    listReceivedLicensesResponse_httpStatus,

    -- ** ListReceivedLicensesForOrganization
    listReceivedLicensesForOrganization_nextToken,
    listReceivedLicensesForOrganization_filters,
    listReceivedLicensesForOrganization_maxResults,
    listReceivedLicensesForOrganizationResponse_nextToken,
    listReceivedLicensesForOrganizationResponse_licenses,
    listReceivedLicensesForOrganizationResponse_httpStatus,

    -- ** ListResourceInventory
    listResourceInventory_nextToken,
    listResourceInventory_filters,
    listResourceInventory_maxResults,
    listResourceInventoryResponse_nextToken,
    listResourceInventoryResponse_resourceInventoryList,
    listResourceInventoryResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTokens
    listTokens_nextToken,
    listTokens_filters,
    listTokens_tokenIds,
    listTokens_maxResults,
    listTokensResponse_nextToken,
    listTokensResponse_tokens,
    listTokensResponse_httpStatus,

    -- ** ListUsageForLicenseConfiguration
    listUsageForLicenseConfiguration_nextToken,
    listUsageForLicenseConfiguration_filters,
    listUsageForLicenseConfiguration_maxResults,
    listUsageForLicenseConfiguration_licenseConfigurationArn,
    listUsageForLicenseConfigurationResponse_nextToken,
    listUsageForLicenseConfigurationResponse_licenseConfigurationUsageList,
    listUsageForLicenseConfigurationResponse_httpStatus,

    -- ** RejectGrant
    rejectGrant_grantArn,
    rejectGrantResponse_status,
    rejectGrantResponse_grantArn,
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
    updateLicenseConfiguration_name,
    updateLicenseConfiguration_productInformationList,
    updateLicenseConfiguration_licenseRules,
    updateLicenseConfiguration_description,
    updateLicenseConfiguration_licenseCount,
    updateLicenseConfiguration_licenseCountHardLimit,
    updateLicenseConfiguration_licenseConfigurationStatus,
    updateLicenseConfiguration_disassociateWhenNotFound,
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
    updateServiceSettings_organizationConfiguration,
    updateServiceSettings_snsTopicArn,
    updateServiceSettings_enableCrossAccountsDiscovery,
    updateServiceSettings_s3BucketArn,
    updateServiceSettingsResponse_httpStatus,

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
    consumptionConfiguration_renewType,
    consumptionConfiguration_borrowConfiguration,
    consumptionConfiguration_provisionalConfiguration,

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
    grantedLicense_productName,
    grantedLicense_issuer,
    grantedLicense_licenseName,
    grantedLicense_licenseMetadata,
    grantedLicense_licenseArn,
    grantedLicense_consumptionConfiguration,
    grantedLicense_entitlements,
    grantedLicense_status,
    grantedLicense_receivedMetadata,
    grantedLicense_homeRegion,
    grantedLicense_createTime,
    grantedLicense_productSKU,
    grantedLicense_beneficiary,
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
    issuerDetails_name,
    issuerDetails_keyFingerprint,
    issuerDetails_signKey,

    -- ** License
    license_productName,
    license_issuer,
    license_licenseName,
    license_licenseMetadata,
    license_licenseArn,
    license_consumptionConfiguration,
    license_entitlements,
    license_status,
    license_homeRegion,
    license_createTime,
    license_productSKU,
    license_beneficiary,
    license_validity,
    license_version,

    -- ** LicenseConfiguration
    licenseConfiguration_name,
    licenseConfiguration_productInformationList,
    licenseConfiguration_licenseCountingType,
    licenseConfiguration_licenseRules,
    licenseConfiguration_consumedLicenses,
    licenseConfiguration_licenseConfigurationArn,
    licenseConfiguration_status,
    licenseConfiguration_description,
    licenseConfiguration_licenseConfigurationId,
    licenseConfiguration_managedResourceSummaryList,
    licenseConfiguration_ownerAccountId,
    licenseConfiguration_licenseCount,
    licenseConfiguration_licenseCountHardLimit,
    licenseConfiguration_disassociateWhenNotFound,
    licenseConfiguration_automatedDiscoveryInformation,
    licenseConfiguration_consumedLicenseSummaryList,

    -- ** LicenseConfigurationAssociation
    licenseConfigurationAssociation_resourceType,
    licenseConfigurationAssociation_resourceOwnerId,
    licenseConfigurationAssociation_associationTime,
    licenseConfigurationAssociation_resourceArn,
    licenseConfigurationAssociation_amiAssociationScope,

    -- ** LicenseConfigurationUsage
    licenseConfigurationUsage_resourceType,
    licenseConfigurationUsage_resourceOwnerId,
    licenseConfigurationUsage_consumedLicenses,
    licenseConfigurationUsage_associationTime,
    licenseConfigurationUsage_resourceArn,
    licenseConfigurationUsage_resourceStatus,

    -- ** LicenseConversionContext
    licenseConversionContext_usageOperation,

    -- ** LicenseConversionTask
    licenseConversionTask_sourceLicenseContext,
    licenseConversionTask_licenseConversionTime,
    licenseConversionTask_status,
    licenseConversionTask_endTime,
    licenseConversionTask_destinationLicenseContext,
    licenseConversionTask_resourceArn,
    licenseConversionTask_licenseConversionTaskId,
    licenseConversionTask_statusMessage,
    licenseConversionTask_startTime,

    -- ** LicenseOperationFailure
    licenseOperationFailure_resourceType,
    licenseOperationFailure_resourceOwnerId,
    licenseOperationFailure_errorMessage,
    licenseOperationFailure_failureTime,
    licenseOperationFailure_operationRequestedBy,
    licenseOperationFailure_resourceArn,
    licenseOperationFailure_metadataList,
    licenseOperationFailure_operationName,

    -- ** LicenseSpecification
    licenseSpecification_amiAssociationScope,
    licenseSpecification_licenseConfigurationArn,

    -- ** LicenseUsage
    licenseUsage_entitlementUsages,

    -- ** ManagedResourceSummary
    managedResourceSummary_resourceType,
    managedResourceSummary_associationCount,

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
    reportGenerator_tags,
    reportGenerator_reportType,
    reportGenerator_reportFrequency,
    reportGenerator_licenseManagerReportGeneratorArn,
    reportGenerator_lastReportGenerationTime,
    reportGenerator_description,
    reportGenerator_reportGeneratorName,
    reportGenerator_s3Location,
    reportGenerator_createTime,
    reportGenerator_lastRunFailureReason,
    reportGenerator_lastRunStatus,
    reportGenerator_reportContext,
    reportGenerator_reportCreatorAccount,

    -- ** ResourceInventory
    resourceInventory_resourceId,
    resourceInventory_resourceOwningAccountId,
    resourceInventory_resourceType,
    resourceInventory_platform,
    resourceInventory_platformVersion,
    resourceInventory_resourceArn,

    -- ** S3Location
    s3Location_bucket,
    s3Location_keyPrefix,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TokenData
    tokenData_licenseArn,
    tokenData_expirationTime,
    tokenData_roleArns,
    tokenData_status,
    tokenData_tokenId,
    tokenData_tokenType,
    tokenData_tokenProperties,
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
