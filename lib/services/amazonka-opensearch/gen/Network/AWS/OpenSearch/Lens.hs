{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearch.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Lens
  ( -- * Operations

    -- ** RejectInboundConnection
    rejectInboundConnection_connectionId,
    rejectInboundConnectionResponse_connection,
    rejectInboundConnectionResponse_httpStatus,

    -- ** DescribeOutboundConnections
    describeOutboundConnections_filters,
    describeOutboundConnections_nextToken,
    describeOutboundConnections_maxResults,
    describeOutboundConnectionsResponse_connections,
    describeOutboundConnectionsResponse_nextToken,
    describeOutboundConnectionsResponse_httpStatus,

    -- ** RemoveTags
    removeTags_arn,
    removeTags_tagKeys,

    -- ** DescribeInstanceTypeLimits
    describeInstanceTypeLimits_domainName,
    describeInstanceTypeLimits_instanceType,
    describeInstanceTypeLimits_engineVersion,
    describeInstanceTypeLimitsResponse_limitsByRole,
    describeInstanceTypeLimitsResponse_httpStatus,

    -- ** DescribeInboundConnections
    describeInboundConnections_filters,
    describeInboundConnections_nextToken,
    describeInboundConnections_maxResults,
    describeInboundConnectionsResponse_connections,
    describeInboundConnectionsResponse_nextToken,
    describeInboundConnectionsResponse_httpStatus,

    -- ** CancelServiceSoftwareUpdate
    cancelServiceSoftwareUpdate_domainName,
    cancelServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    cancelServiceSoftwareUpdateResponse_httpStatus,

    -- ** ListDomainsForPackage
    listDomainsForPackage_nextToken,
    listDomainsForPackage_maxResults,
    listDomainsForPackage_packageID,
    listDomainsForPackageResponse_domainPackageDetailsList,
    listDomainsForPackageResponse_nextToken,
    listDomainsForPackageResponse_httpStatus,

    -- ** ListPackagesForDomain
    listPackagesForDomain_nextToken,
    listPackagesForDomain_maxResults,
    listPackagesForDomain_domainName,
    listPackagesForDomainResponse_domainPackageDetailsList,
    listPackagesForDomainResponse_nextToken,
    listPackagesForDomainResponse_httpStatus,

    -- ** UpgradeDomain
    upgradeDomain_performCheckOnly,
    upgradeDomain_advancedOptions,
    upgradeDomain_domainName,
    upgradeDomain_targetVersion,
    upgradeDomainResponse_domainName,
    upgradeDomainResponse_upgradeId,
    upgradeDomainResponse_performCheckOnly,
    upgradeDomainResponse_targetVersion,
    upgradeDomainResponse_advancedOptions,
    upgradeDomainResponse_httpStatus,

    -- ** DescribeDomainAutoTunes
    describeDomainAutoTunes_nextToken,
    describeDomainAutoTunes_maxResults,
    describeDomainAutoTunes_domainName,
    describeDomainAutoTunesResponse_nextToken,
    describeDomainAutoTunesResponse_autoTunes,
    describeDomainAutoTunesResponse_httpStatus,

    -- ** DescribeReservedInstances
    describeReservedInstances_nextToken,
    describeReservedInstances_reservedInstanceId,
    describeReservedInstances_maxResults,
    describeReservedInstancesResponse_nextToken,
    describeReservedInstancesResponse_reservedInstances,
    describeReservedInstancesResponse_httpStatus,

    -- ** StartServiceSoftwareUpdate
    startServiceSoftwareUpdate_domainName,
    startServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    startServiceSoftwareUpdateResponse_httpStatus,

    -- ** DeleteOutboundConnection
    deleteOutboundConnection_connectionId,
    deleteOutboundConnectionResponse_connection,
    deleteOutboundConnectionResponse_httpStatus,

    -- ** ListVersions
    listVersions_nextToken,
    listVersions_maxResults,
    listVersionsResponse_versions,
    listVersionsResponse_nextToken,
    listVersionsResponse_httpStatus,

    -- ** DescribeReservedInstanceOfferings
    describeReservedInstanceOfferings_nextToken,
    describeReservedInstanceOfferings_reservedInstanceOfferingId,
    describeReservedInstanceOfferings_maxResults,
    describeReservedInstanceOfferingsResponse_nextToken,
    describeReservedInstanceOfferingsResponse_reservedInstanceOfferings,
    describeReservedInstanceOfferingsResponse_httpStatus,

    -- ** ListDomainNames
    listDomainNames_engineType,
    listDomainNamesResponse_domainNames,
    listDomainNamesResponse_httpStatus,

    -- ** PurchaseReservedInstanceOffering
    purchaseReservedInstanceOffering_instanceCount,
    purchaseReservedInstanceOffering_reservedInstanceOfferingId,
    purchaseReservedInstanceOffering_reservationName,
    purchaseReservedInstanceOfferingResponse_reservationName,
    purchaseReservedInstanceOfferingResponse_reservedInstanceId,
    purchaseReservedInstanceOfferingResponse_httpStatus,

    -- ** DescribeDomains
    describeDomains_domainNames,
    describeDomainsResponse_httpStatus,
    describeDomainsResponse_domainStatusList,

    -- ** AssociatePackage
    associatePackage_packageID,
    associatePackage_domainName,
    associatePackageResponse_domainPackageDetails,
    associatePackageResponse_httpStatus,

    -- ** ListInstanceTypeDetails
    listInstanceTypeDetails_nextToken,
    listInstanceTypeDetails_domainName,
    listInstanceTypeDetails_maxResults,
    listInstanceTypeDetails_engineVersion,
    listInstanceTypeDetailsResponse_instanceTypeDetails,
    listInstanceTypeDetailsResponse_nextToken,
    listInstanceTypeDetailsResponse_httpStatus,

    -- ** GetPackageVersionHistory
    getPackageVersionHistory_nextToken,
    getPackageVersionHistory_maxResults,
    getPackageVersionHistory_packageID,
    getPackageVersionHistoryResponse_packageID,
    getPackageVersionHistoryResponse_packageVersionHistoryList,
    getPackageVersionHistoryResponse_nextToken,
    getPackageVersionHistoryResponse_httpStatus,

    -- ** GetUpgradeHistory
    getUpgradeHistory_nextToken,
    getUpgradeHistory_maxResults,
    getUpgradeHistory_domainName,
    getUpgradeHistoryResponse_nextToken,
    getUpgradeHistoryResponse_upgradeHistories,
    getUpgradeHistoryResponse_httpStatus,

    -- ** DescribePackages
    describePackages_filters,
    describePackages_nextToken,
    describePackages_maxResults,
    describePackagesResponse_packageDetailsList,
    describePackagesResponse_nextToken,
    describePackagesResponse_httpStatus,

    -- ** CreateDomain
    createDomain_eBSOptions,
    createDomain_engineVersion,
    createDomain_nodeToNodeEncryptionOptions,
    createDomain_accessPolicies,
    createDomain_autoTuneOptions,
    createDomain_logPublishingOptions,
    createDomain_clusterConfig,
    createDomain_advancedSecurityOptions,
    createDomain_tagList,
    createDomain_snapshotOptions,
    createDomain_cognitoOptions,
    createDomain_encryptionAtRestOptions,
    createDomain_vPCOptions,
    createDomain_domainEndpointOptions,
    createDomain_advancedOptions,
    createDomain_domainName,
    createDomainResponse_domainStatus,
    createDomainResponse_httpStatus,

    -- ** DescribeDomainConfig
    describeDomainConfig_domainName,
    describeDomainConfigResponse_httpStatus,
    describeDomainConfigResponse_domainConfig,

    -- ** GetUpgradeStatus
    getUpgradeStatus_domainName,
    getUpgradeStatusResponse_stepStatus,
    getUpgradeStatusResponse_upgradeName,
    getUpgradeStatusResponse_upgradeStep,
    getUpgradeStatusResponse_httpStatus,

    -- ** DeleteInboundConnection
    deleteInboundConnection_connectionId,
    deleteInboundConnectionResponse_connection,
    deleteInboundConnectionResponse_httpStatus,

    -- ** DissociatePackage
    dissociatePackage_packageID,
    dissociatePackage_domainName,
    dissociatePackageResponse_domainPackageDetails,
    dissociatePackageResponse_httpStatus,

    -- ** DescribeDomain
    describeDomain_domainName,
    describeDomainResponse_httpStatus,
    describeDomainResponse_domainStatus,

    -- ** AddTags
    addTags_arn,
    addTags_tagList,

    -- ** AcceptInboundConnection
    acceptInboundConnection_connectionId,
    acceptInboundConnectionResponse_connection,
    acceptInboundConnectionResponse_httpStatus,

    -- ** UpdateDomainConfig
    updateDomainConfig_eBSOptions,
    updateDomainConfig_nodeToNodeEncryptionOptions,
    updateDomainConfig_accessPolicies,
    updateDomainConfig_autoTuneOptions,
    updateDomainConfig_logPublishingOptions,
    updateDomainConfig_clusterConfig,
    updateDomainConfig_advancedSecurityOptions,
    updateDomainConfig_snapshotOptions,
    updateDomainConfig_cognitoOptions,
    updateDomainConfig_encryptionAtRestOptions,
    updateDomainConfig_vPCOptions,
    updateDomainConfig_domainEndpointOptions,
    updateDomainConfig_advancedOptions,
    updateDomainConfig_domainName,
    updateDomainConfigResponse_httpStatus,
    updateDomainConfigResponse_domainConfig,

    -- ** ListTags
    listTags_arn,
    listTagsResponse_tagList,
    listTagsResponse_httpStatus,

    -- ** DeletePackage
    deletePackage_packageID,
    deletePackageResponse_packageDetails,
    deletePackageResponse_httpStatus,

    -- ** UpdatePackage
    updatePackage_packageDescription,
    updatePackage_commitMessage,
    updatePackage_packageID,
    updatePackage_packageSource,
    updatePackageResponse_packageDetails,
    updatePackageResponse_httpStatus,

    -- ** CreateOutboundConnection
    createOutboundConnection_localDomainInfo,
    createOutboundConnection_remoteDomainInfo,
    createOutboundConnection_connectionAlias,
    createOutboundConnectionResponse_remoteDomainInfo,
    createOutboundConnectionResponse_connectionAlias,
    createOutboundConnectionResponse_localDomainInfo,
    createOutboundConnectionResponse_connectionId,
    createOutboundConnectionResponse_connectionStatus,
    createOutboundConnectionResponse_httpStatus,

    -- ** CreatePackage
    createPackage_packageDescription,
    createPackage_packageName,
    createPackage_packageType,
    createPackage_packageSource,
    createPackageResponse_packageDetails,
    createPackageResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_domainStatus,
    deleteDomainResponse_httpStatus,

    -- ** GetCompatibleVersions
    getCompatibleVersions_domainName,
    getCompatibleVersionsResponse_compatibleVersions,
    getCompatibleVersionsResponse_httpStatus,

    -- * Types

    -- ** AWSDomainInformation
    aWSDomainInformation_ownerId,
    aWSDomainInformation_region,
    aWSDomainInformation_domainName,

    -- ** AccessPoliciesStatus
    accessPoliciesStatus_options,
    accessPoliciesStatus_status,

    -- ** AdditionalLimit
    additionalLimit_limitName,
    additionalLimit_limitValues,

    -- ** AdvancedOptionsStatus
    advancedOptionsStatus_options,
    advancedOptionsStatus_status,

    -- ** AdvancedSecurityOptions
    advancedSecurityOptions_enabled,
    advancedSecurityOptions_internalUserDatabaseEnabled,
    advancedSecurityOptions_sAMLOptions,

    -- ** AdvancedSecurityOptionsInput
    advancedSecurityOptionsInput_enabled,
    advancedSecurityOptionsInput_internalUserDatabaseEnabled,
    advancedSecurityOptionsInput_masterUserOptions,
    advancedSecurityOptionsInput_sAMLOptions,

    -- ** AdvancedSecurityOptionsStatus
    advancedSecurityOptionsStatus_options,
    advancedSecurityOptionsStatus_status,

    -- ** AutoTune
    autoTune_autoTuneDetails,
    autoTune_autoTuneType,

    -- ** AutoTuneDetails
    autoTuneDetails_scheduledAutoTuneDetails,

    -- ** AutoTuneMaintenanceSchedule
    autoTuneMaintenanceSchedule_startAt,
    autoTuneMaintenanceSchedule_cronExpressionForRecurrence,
    autoTuneMaintenanceSchedule_duration,

    -- ** AutoTuneOptions
    autoTuneOptions_desiredState,
    autoTuneOptions_rollbackOnDisable,
    autoTuneOptions_maintenanceSchedules,

    -- ** AutoTuneOptionsInput
    autoTuneOptionsInput_desiredState,
    autoTuneOptionsInput_maintenanceSchedules,

    -- ** AutoTuneOptionsOutput
    autoTuneOptionsOutput_state,
    autoTuneOptionsOutput_errorMessage,

    -- ** AutoTuneOptionsStatus
    autoTuneOptionsStatus_status,
    autoTuneOptionsStatus_options,

    -- ** AutoTuneStatus
    autoTuneStatus_pendingDeletion,
    autoTuneStatus_errorMessage,
    autoTuneStatus_updateVersion,
    autoTuneStatus_creationDate,
    autoTuneStatus_updateDate,
    autoTuneStatus_state,

    -- ** ClusterConfig
    clusterConfig_dedicatedMasterCount,
    clusterConfig_dedicatedMasterType,
    clusterConfig_dedicatedMasterEnabled,
    clusterConfig_instanceCount,
    clusterConfig_coldStorageOptions,
    clusterConfig_zoneAwarenessEnabled,
    clusterConfig_instanceType,
    clusterConfig_warmEnabled,
    clusterConfig_zoneAwarenessConfig,
    clusterConfig_warmCount,
    clusterConfig_warmType,

    -- ** ClusterConfigStatus
    clusterConfigStatus_options,
    clusterConfigStatus_status,

    -- ** CognitoOptions
    cognitoOptions_identityPoolId,
    cognitoOptions_enabled,
    cognitoOptions_userPoolId,
    cognitoOptions_roleArn,

    -- ** CognitoOptionsStatus
    cognitoOptionsStatus_options,
    cognitoOptionsStatus_status,

    -- ** ColdStorageOptions
    coldStorageOptions_enabled,

    -- ** CompatibleVersionsMap
    compatibleVersionsMap_sourceVersion,
    compatibleVersionsMap_targetVersions,

    -- ** DescribePackagesFilter
    describePackagesFilter_value,
    describePackagesFilter_name,

    -- ** DomainConfig
    domainConfig_eBSOptions,
    domainConfig_engineVersion,
    domainConfig_nodeToNodeEncryptionOptions,
    domainConfig_accessPolicies,
    domainConfig_autoTuneOptions,
    domainConfig_logPublishingOptions,
    domainConfig_clusterConfig,
    domainConfig_advancedSecurityOptions,
    domainConfig_snapshotOptions,
    domainConfig_cognitoOptions,
    domainConfig_encryptionAtRestOptions,
    domainConfig_vPCOptions,
    domainConfig_domainEndpointOptions,
    domainConfig_advancedOptions,

    -- ** DomainEndpointOptions
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_tLSSecurityPolicy,
    domainEndpointOptions_customEndpointEnabled,
    domainEndpointOptions_customEndpoint,
    domainEndpointOptions_customEndpointCertificateArn,

    -- ** DomainEndpointOptionsStatus
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- ** DomainInfo
    domainInfo_domainName,
    domainInfo_engineType,

    -- ** DomainInformationContainer
    domainInformationContainer_aWSDomainInformation,

    -- ** DomainPackageDetails
    domainPackageDetails_lastUpdated,
    domainPackageDetails_packageID,
    domainPackageDetails_packageType,
    domainPackageDetails_packageName,
    domainPackageDetails_packageVersion,
    domainPackageDetails_domainPackageStatus,
    domainPackageDetails_domainName,
    domainPackageDetails_errorDetails,
    domainPackageDetails_referencePath,

    -- ** DomainStatus
    domainStatus_eBSOptions,
    domainStatus_engineVersion,
    domainStatus_nodeToNodeEncryptionOptions,
    domainStatus_accessPolicies,
    domainStatus_serviceSoftwareOptions,
    domainStatus_autoTuneOptions,
    domainStatus_logPublishingOptions,
    domainStatus_advancedSecurityOptions,
    domainStatus_created,
    domainStatus_snapshotOptions,
    domainStatus_cognitoOptions,
    domainStatus_encryptionAtRestOptions,
    domainStatus_deleted,
    domainStatus_vPCOptions,
    domainStatus_endpoints,
    domainStatus_domainEndpointOptions,
    domainStatus_processing,
    domainStatus_endpoint,
    domainStatus_upgradeProcessing,
    domainStatus_advancedOptions,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_arn,
    domainStatus_clusterConfig,

    -- ** Duration
    duration_value,
    duration_unit,

    -- ** EBSOptions
    eBSOptions_volumeSize,
    eBSOptions_iops,
    eBSOptions_volumeType,
    eBSOptions_eBSEnabled,

    -- ** EBSOptionsStatus
    eBSOptionsStatus_options,
    eBSOptionsStatus_status,

    -- ** EncryptionAtRestOptions
    encryptionAtRestOptions_enabled,
    encryptionAtRestOptions_kmsKeyId,

    -- ** EncryptionAtRestOptionsStatus
    encryptionAtRestOptionsStatus_options,
    encryptionAtRestOptionsStatus_status,

    -- ** ErrorDetails
    errorDetails_errorType,
    errorDetails_errorMessage,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** InboundConnection
    inboundConnection_remoteDomainInfo,
    inboundConnection_localDomainInfo,
    inboundConnection_connectionId,
    inboundConnection_connectionStatus,

    -- ** InboundConnectionStatus
    inboundConnectionStatus_message,
    inboundConnectionStatus_statusCode,

    -- ** InstanceCountLimits
    instanceCountLimits_maximumInstanceCount,
    instanceCountLimits_minimumInstanceCount,

    -- ** InstanceLimits
    instanceLimits_instanceCountLimits,

    -- ** InstanceTypeDetails
    instanceTypeDetails_encryptionEnabled,
    instanceTypeDetails_cognitoEnabled,
    instanceTypeDetails_instanceRole,
    instanceTypeDetails_instanceType,
    instanceTypeDetails_warmEnabled,
    instanceTypeDetails_advancedSecurityEnabled,
    instanceTypeDetails_appLogsEnabled,

    -- ** Limits
    limits_instanceLimits,
    limits_additionalLimits,
    limits_storageTypes,

    -- ** LogPublishingOption
    logPublishingOption_enabled,
    logPublishingOption_cloudWatchLogsLogGroupArn,

    -- ** LogPublishingOptionsStatus
    logPublishingOptionsStatus_status,
    logPublishingOptionsStatus_options,

    -- ** MasterUserOptions
    masterUserOptions_masterUserPassword,
    masterUserOptions_masterUserName,
    masterUserOptions_masterUserARN,

    -- ** NodeToNodeEncryptionOptions
    nodeToNodeEncryptionOptions_enabled,

    -- ** NodeToNodeEncryptionOptionsStatus
    nodeToNodeEncryptionOptionsStatus_options,
    nodeToNodeEncryptionOptionsStatus_status,

    -- ** OptionStatus
    optionStatus_pendingDeletion,
    optionStatus_updateVersion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- ** OutboundConnection
    outboundConnection_remoteDomainInfo,
    outboundConnection_connectionAlias,
    outboundConnection_localDomainInfo,
    outboundConnection_connectionId,
    outboundConnection_connectionStatus,

    -- ** OutboundConnectionStatus
    outboundConnectionStatus_message,
    outboundConnectionStatus_statusCode,

    -- ** PackageDetails
    packageDetails_packageID,
    packageDetails_packageType,
    packageDetails_lastUpdatedAt,
    packageDetails_createdAt,
    packageDetails_packageName,
    packageDetails_packageStatus,
    packageDetails_packageDescription,
    packageDetails_errorDetails,
    packageDetails_availablePackageVersion,

    -- ** PackageSource
    packageSource_s3Key,
    packageSource_s3BucketName,

    -- ** PackageVersionHistory
    packageVersionHistory_createdAt,
    packageVersionHistory_packageVersion,
    packageVersionHistory_commitMessage,

    -- ** RecurringCharge
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- ** ReservedInstance
    reservedInstance_state,
    reservedInstance_currencyCode,
    reservedInstance_instanceCount,
    reservedInstance_startTime,
    reservedInstance_instanceType,
    reservedInstance_reservationName,
    reservedInstance_billingSubscriptionId,
    reservedInstance_recurringCharges,
    reservedInstance_usagePrice,
    reservedInstance_reservedInstanceId,
    reservedInstance_reservedInstanceOfferingId,
    reservedInstance_fixedPrice,
    reservedInstance_duration,
    reservedInstance_paymentOption,

    -- ** ReservedInstanceOffering
    reservedInstanceOffering_currencyCode,
    reservedInstanceOffering_instanceType,
    reservedInstanceOffering_recurringCharges,
    reservedInstanceOffering_usagePrice,
    reservedInstanceOffering_reservedInstanceOfferingId,
    reservedInstanceOffering_fixedPrice,
    reservedInstanceOffering_duration,
    reservedInstanceOffering_paymentOption,

    -- ** SAMLIdp
    sAMLIdp_metadataContent,
    sAMLIdp_entityId,

    -- ** SAMLOptionsInput
    sAMLOptionsInput_masterUserName,
    sAMLOptionsInput_enabled,
    sAMLOptionsInput_idp,
    sAMLOptionsInput_rolesKey,
    sAMLOptionsInput_masterBackendRole,
    sAMLOptionsInput_sessionTimeoutMinutes,
    sAMLOptionsInput_subjectKey,

    -- ** SAMLOptionsOutput
    sAMLOptionsOutput_enabled,
    sAMLOptionsOutput_idp,
    sAMLOptionsOutput_rolesKey,
    sAMLOptionsOutput_sessionTimeoutMinutes,
    sAMLOptionsOutput_subjectKey,

    -- ** ScheduledAutoTuneDetails
    scheduledAutoTuneDetails_severity,
    scheduledAutoTuneDetails_action,
    scheduledAutoTuneDetails_date,
    scheduledAutoTuneDetails_actionType,

    -- ** ServiceSoftwareOptions
    serviceSoftwareOptions_automatedUpdateDate,
    serviceSoftwareOptions_currentVersion,
    serviceSoftwareOptions_optionalDeployment,
    serviceSoftwareOptions_updateStatus,
    serviceSoftwareOptions_cancellable,
    serviceSoftwareOptions_updateAvailable,
    serviceSoftwareOptions_description,
    serviceSoftwareOptions_newVersion,

    -- ** SnapshotOptions
    snapshotOptions_automatedSnapshotStartHour,

    -- ** SnapshotOptionsStatus
    snapshotOptionsStatus_options,
    snapshotOptionsStatus_status,

    -- ** StorageType
    storageType_storageTypeLimits,
    storageType_storageSubTypeName,
    storageType_storageTypeName,

    -- ** StorageTypeLimit
    storageTypeLimit_limitName,
    storageTypeLimit_limitValues,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UpgradeHistory
    upgradeHistory_upgradeStatus,
    upgradeHistory_stepsList,
    upgradeHistory_upgradeName,
    upgradeHistory_startTimestamp,

    -- ** UpgradeStepItem
    upgradeStepItem_upgradeStepStatus,
    upgradeStepItem_progressPercent,
    upgradeStepItem_issues,
    upgradeStepItem_upgradeStep,

    -- ** VPCDerivedInfo
    vPCDerivedInfo_securityGroupIds,
    vPCDerivedInfo_subnetIds,
    vPCDerivedInfo_vPCId,
    vPCDerivedInfo_availabilityZones,

    -- ** VPCDerivedInfoStatus
    vPCDerivedInfoStatus_options,
    vPCDerivedInfoStatus_status,

    -- ** VPCOptions
    vPCOptions_securityGroupIds,
    vPCOptions_subnetIds,

    -- ** VersionStatus
    versionStatus_options,
    versionStatus_status,

    -- ** ZoneAwarenessConfig
    zoneAwarenessConfig_availabilityZoneCount,
  )
where

import Amazonka.OpenSearch.AcceptInboundConnection
import Amazonka.OpenSearch.AddTags
import Amazonka.OpenSearch.AssociatePackage
import Amazonka.OpenSearch.CancelServiceSoftwareUpdate
import Amazonka.OpenSearch.CreateDomain
import Amazonka.OpenSearch.CreateOutboundConnection
import Amazonka.OpenSearch.CreatePackage
import Amazonka.OpenSearch.DeleteDomain
import Amazonka.OpenSearch.DeleteInboundConnection
import Amazonka.OpenSearch.DeleteOutboundConnection
import Amazonka.OpenSearch.DeletePackage
import Amazonka.OpenSearch.DescribeDomain
import Amazonka.OpenSearch.DescribeDomainAutoTunes
import Amazonka.OpenSearch.DescribeDomainConfig
import Amazonka.OpenSearch.DescribeDomains
import Amazonka.OpenSearch.DescribeInboundConnections
import Amazonka.OpenSearch.DescribeInstanceTypeLimits
import Amazonka.OpenSearch.DescribeOutboundConnections
import Amazonka.OpenSearch.DescribePackages
import Amazonka.OpenSearch.DescribeReservedInstanceOfferings
import Amazonka.OpenSearch.DescribeReservedInstances
import Amazonka.OpenSearch.DissociatePackage
import Amazonka.OpenSearch.GetCompatibleVersions
import Amazonka.OpenSearch.GetPackageVersionHistory
import Amazonka.OpenSearch.GetUpgradeHistory
import Amazonka.OpenSearch.GetUpgradeStatus
import Amazonka.OpenSearch.ListDomainNames
import Amazonka.OpenSearch.ListDomainsForPackage
import Amazonka.OpenSearch.ListInstanceTypeDetails
import Amazonka.OpenSearch.ListPackagesForDomain
import Amazonka.OpenSearch.ListTags
import Amazonka.OpenSearch.ListVersions
import Amazonka.OpenSearch.PurchaseReservedInstanceOffering
import Amazonka.OpenSearch.RejectInboundConnection
import Amazonka.OpenSearch.RemoveTags
import Amazonka.OpenSearch.StartServiceSoftwareUpdate
import Amazonka.OpenSearch.Types.AWSDomainInformation
import Amazonka.OpenSearch.Types.AccessPoliciesStatus
import Amazonka.OpenSearch.Types.AdditionalLimit
import Amazonka.OpenSearch.Types.AdvancedOptionsStatus
import Amazonka.OpenSearch.Types.AdvancedSecurityOptions
import Amazonka.OpenSearch.Types.AdvancedSecurityOptionsInput
import Amazonka.OpenSearch.Types.AdvancedSecurityOptionsStatus
import Amazonka.OpenSearch.Types.AutoTune
import Amazonka.OpenSearch.Types.AutoTuneDetails
import Amazonka.OpenSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.OpenSearch.Types.AutoTuneOptions
import Amazonka.OpenSearch.Types.AutoTuneOptionsInput
import Amazonka.OpenSearch.Types.AutoTuneOptionsOutput
import Amazonka.OpenSearch.Types.AutoTuneOptionsStatus
import Amazonka.OpenSearch.Types.AutoTuneStatus
import Amazonka.OpenSearch.Types.ClusterConfig
import Amazonka.OpenSearch.Types.ClusterConfigStatus
import Amazonka.OpenSearch.Types.CognitoOptions
import Amazonka.OpenSearch.Types.CognitoOptionsStatus
import Amazonka.OpenSearch.Types.ColdStorageOptions
import Amazonka.OpenSearch.Types.CompatibleVersionsMap
import Amazonka.OpenSearch.Types.DescribePackagesFilter
import Amazonka.OpenSearch.Types.DomainConfig
import Amazonka.OpenSearch.Types.DomainEndpointOptions
import Amazonka.OpenSearch.Types.DomainEndpointOptionsStatus
import Amazonka.OpenSearch.Types.DomainInfo
import Amazonka.OpenSearch.Types.DomainInformationContainer
import Amazonka.OpenSearch.Types.DomainPackageDetails
import Amazonka.OpenSearch.Types.DomainStatus
import Amazonka.OpenSearch.Types.Duration
import Amazonka.OpenSearch.Types.EBSOptions
import Amazonka.OpenSearch.Types.EBSOptionsStatus
import Amazonka.OpenSearch.Types.EncryptionAtRestOptions
import Amazonka.OpenSearch.Types.EncryptionAtRestOptionsStatus
import Amazonka.OpenSearch.Types.ErrorDetails
import Amazonka.OpenSearch.Types.Filter
import Amazonka.OpenSearch.Types.InboundConnection
import Amazonka.OpenSearch.Types.InboundConnectionStatus
import Amazonka.OpenSearch.Types.InstanceCountLimits
import Amazonka.OpenSearch.Types.InstanceLimits
import Amazonka.OpenSearch.Types.InstanceTypeDetails
import Amazonka.OpenSearch.Types.Limits
import Amazonka.OpenSearch.Types.LogPublishingOption
import Amazonka.OpenSearch.Types.LogPublishingOptionsStatus
import Amazonka.OpenSearch.Types.MasterUserOptions
import Amazonka.OpenSearch.Types.NodeToNodeEncryptionOptions
import Amazonka.OpenSearch.Types.NodeToNodeEncryptionOptionsStatus
import Amazonka.OpenSearch.Types.OptionStatus
import Amazonka.OpenSearch.Types.OutboundConnection
import Amazonka.OpenSearch.Types.OutboundConnectionStatus
import Amazonka.OpenSearch.Types.PackageDetails
import Amazonka.OpenSearch.Types.PackageSource
import Amazonka.OpenSearch.Types.PackageVersionHistory
import Amazonka.OpenSearch.Types.RecurringCharge
import Amazonka.OpenSearch.Types.ReservedInstance
import Amazonka.OpenSearch.Types.ReservedInstanceOffering
import Amazonka.OpenSearch.Types.SAMLIdp
import Amazonka.OpenSearch.Types.SAMLOptionsInput
import Amazonka.OpenSearch.Types.SAMLOptionsOutput
import Amazonka.OpenSearch.Types.ScheduledAutoTuneDetails
import Amazonka.OpenSearch.Types.ServiceSoftwareOptions
import Amazonka.OpenSearch.Types.SnapshotOptions
import Amazonka.OpenSearch.Types.SnapshotOptionsStatus
import Amazonka.OpenSearch.Types.StorageType
import Amazonka.OpenSearch.Types.StorageTypeLimit
import Amazonka.OpenSearch.Types.Tag
import Amazonka.OpenSearch.Types.UpgradeHistory
import Amazonka.OpenSearch.Types.UpgradeStepItem
import Amazonka.OpenSearch.Types.VPCDerivedInfo
import Amazonka.OpenSearch.Types.VPCDerivedInfoStatus
import Amazonka.OpenSearch.Types.VPCOptions
import Amazonka.OpenSearch.Types.VersionStatus
import Amazonka.OpenSearch.Types.ZoneAwarenessConfig
import Amazonka.OpenSearch.UpdateDomainConfig
import Amazonka.OpenSearch.UpdatePackage
import Amazonka.OpenSearch.UpgradeDomain
