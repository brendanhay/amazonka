{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticSearch.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Lens
  ( -- * Operations

    -- ** CreateOutboundCrossClusterSearchConnection
    createOutboundCrossClusterSearchConnection_sourceDomainInfo,
    createOutboundCrossClusterSearchConnection_destinationDomainInfo,
    createOutboundCrossClusterSearchConnection_connectionAlias,
    createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_connectionAlias,
    createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId,
    createOutboundCrossClusterSearchConnectionResponse_connectionStatus,
    createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** DescribeInboundCrossClusterSearchConnections
    describeInboundCrossClusterSearchConnections_filters,
    describeInboundCrossClusterSearchConnections_nextToken,
    describeInboundCrossClusterSearchConnections_maxResults,
    describeInboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections,
    describeInboundCrossClusterSearchConnectionsResponse_nextToken,
    describeInboundCrossClusterSearchConnectionsResponse_httpStatus,

    -- ** CreateElasticsearchDomain
    createElasticsearchDomain_eBSOptions,
    createElasticsearchDomain_nodeToNodeEncryptionOptions,
    createElasticsearchDomain_accessPolicies,
    createElasticsearchDomain_autoTuneOptions,
    createElasticsearchDomain_logPublishingOptions,
    createElasticsearchDomain_advancedSecurityOptions,
    createElasticsearchDomain_tagList,
    createElasticsearchDomain_elasticsearchClusterConfig,
    createElasticsearchDomain_snapshotOptions,
    createElasticsearchDomain_cognitoOptions,
    createElasticsearchDomain_encryptionAtRestOptions,
    createElasticsearchDomain_vPCOptions,
    createElasticsearchDomain_domainEndpointOptions,
    createElasticsearchDomain_advancedOptions,
    createElasticsearchDomain_elasticsearchVersion,
    createElasticsearchDomain_domainName,
    createElasticsearchDomainResponse_domainStatus,
    createElasticsearchDomainResponse_httpStatus,

    -- ** RemoveTags
    removeTags_arn,
    removeTags_tagKeys,

    -- ** GetCompatibleElasticsearchVersions
    getCompatibleElasticsearchVersions_domainName,
    getCompatibleElasticsearchVersionsResponse_compatibleElasticsearchVersions,
    getCompatibleElasticsearchVersionsResponse_httpStatus,

    -- ** DescribeElasticsearchDomains
    describeElasticsearchDomains_domainNames,
    describeElasticsearchDomainsResponse_httpStatus,
    describeElasticsearchDomainsResponse_domainStatusList,

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

    -- ** StartElasticsearchServiceSoftwareUpdate
    startElasticsearchServiceSoftwareUpdate_domainName,
    startElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    startElasticsearchServiceSoftwareUpdateResponse_httpStatus,

    -- ** DescribeDomainAutoTunes
    describeDomainAutoTunes_nextToken,
    describeDomainAutoTunes_maxResults,
    describeDomainAutoTunes_domainName,
    describeDomainAutoTunesResponse_nextToken,
    describeDomainAutoTunesResponse_autoTunes,
    describeDomainAutoTunesResponse_httpStatus,

    -- ** ListElasticsearchInstanceTypes
    listElasticsearchInstanceTypes_nextToken,
    listElasticsearchInstanceTypes_domainName,
    listElasticsearchInstanceTypes_maxResults,
    listElasticsearchInstanceTypes_elasticsearchVersion,
    listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes,
    listElasticsearchInstanceTypesResponse_nextToken,
    listElasticsearchInstanceTypesResponse_httpStatus,

    -- ** DeleteElasticsearchServiceRole

    -- ** DescribeElasticsearchDomain
    describeElasticsearchDomain_domainName,
    describeElasticsearchDomainResponse_httpStatus,
    describeElasticsearchDomainResponse_domainStatus,

    -- ** ListDomainNames
    listDomainNames_engineType,
    listDomainNamesResponse_domainNames,
    listDomainNamesResponse_httpStatus,

    -- ** AssociatePackage
    associatePackage_packageID,
    associatePackage_domainName,
    associatePackageResponse_domainPackageDetails,
    associatePackageResponse_httpStatus,

    -- ** DeleteOutboundCrossClusterSearchConnection
    deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteOutboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** DescribeElasticsearchInstanceTypeLimits
    describeElasticsearchInstanceTypeLimits_domainName,
    describeElasticsearchInstanceTypeLimits_instanceType,
    describeElasticsearchInstanceTypeLimits_elasticsearchVersion,
    describeElasticsearchInstanceTypeLimitsResponse_limitsByRole,
    describeElasticsearchInstanceTypeLimitsResponse_httpStatus,

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

    -- ** DescribeElasticsearchDomainConfig
    describeElasticsearchDomainConfig_domainName,
    describeElasticsearchDomainConfigResponse_httpStatus,
    describeElasticsearchDomainConfigResponse_domainConfig,

    -- ** GetUpgradeStatus
    getUpgradeStatus_domainName,
    getUpgradeStatusResponse_stepStatus,
    getUpgradeStatusResponse_upgradeName,
    getUpgradeStatusResponse_upgradeStep,
    getUpgradeStatusResponse_httpStatus,

    -- ** DeleteElasticsearchDomain
    deleteElasticsearchDomain_domainName,
    deleteElasticsearchDomainResponse_domainStatus,
    deleteElasticsearchDomainResponse_httpStatus,

    -- ** DissociatePackage
    dissociatePackage_packageID,
    dissociatePackage_domainName,
    dissociatePackageResponse_domainPackageDetails,
    dissociatePackageResponse_httpStatus,

    -- ** PurchaseReservedElasticsearchInstanceOffering
    purchaseReservedElasticsearchInstanceOffering_instanceCount,
    purchaseReservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId,
    purchaseReservedElasticsearchInstanceOffering_reservationName,
    purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId,
    purchaseReservedElasticsearchInstanceOfferingResponse_reservationName,
    purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus,

    -- ** DescribeReservedElasticsearchInstances
    describeReservedElasticsearchInstances_reservedElasticsearchInstanceId,
    describeReservedElasticsearchInstances_nextToken,
    describeReservedElasticsearchInstances_maxResults,
    describeReservedElasticsearchInstancesResponse_reservedElasticsearchInstances,
    describeReservedElasticsearchInstancesResponse_nextToken,
    describeReservedElasticsearchInstancesResponse_httpStatus,

    -- ** UpdateElasticsearchDomainConfig
    updateElasticsearchDomainConfig_eBSOptions,
    updateElasticsearchDomainConfig_nodeToNodeEncryptionOptions,
    updateElasticsearchDomainConfig_accessPolicies,
    updateElasticsearchDomainConfig_autoTuneOptions,
    updateElasticsearchDomainConfig_logPublishingOptions,
    updateElasticsearchDomainConfig_advancedSecurityOptions,
    updateElasticsearchDomainConfig_elasticsearchClusterConfig,
    updateElasticsearchDomainConfig_snapshotOptions,
    updateElasticsearchDomainConfig_cognitoOptions,
    updateElasticsearchDomainConfig_encryptionAtRestOptions,
    updateElasticsearchDomainConfig_vPCOptions,
    updateElasticsearchDomainConfig_domainEndpointOptions,
    updateElasticsearchDomainConfig_advancedOptions,
    updateElasticsearchDomainConfig_domainName,
    updateElasticsearchDomainConfigResponse_httpStatus,
    updateElasticsearchDomainConfigResponse_domainConfig,

    -- ** ListElasticsearchVersions
    listElasticsearchVersions_nextToken,
    listElasticsearchVersions_maxResults,
    listElasticsearchVersionsResponse_nextToken,
    listElasticsearchVersionsResponse_elasticsearchVersions,
    listElasticsearchVersionsResponse_httpStatus,

    -- ** AddTags
    addTags_arn,
    addTags_tagList,

    -- ** DeleteInboundCrossClusterSearchConnection
    deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteInboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** DescribeReservedElasticsearchInstanceOfferings
    describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId,
    describeReservedElasticsearchInstanceOfferings_nextToken,
    describeReservedElasticsearchInstanceOfferings_maxResults,
    describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings,
    describeReservedElasticsearchInstanceOfferingsResponse_nextToken,
    describeReservedElasticsearchInstanceOfferingsResponse_httpStatus,

    -- ** UpgradeElasticsearchDomain
    upgradeElasticsearchDomain_performCheckOnly,
    upgradeElasticsearchDomain_domainName,
    upgradeElasticsearchDomain_targetVersion,
    upgradeElasticsearchDomainResponse_domainName,
    upgradeElasticsearchDomainResponse_performCheckOnly,
    upgradeElasticsearchDomainResponse_targetVersion,
    upgradeElasticsearchDomainResponse_httpStatus,

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

    -- ** CancelElasticsearchServiceSoftwareUpdate
    cancelElasticsearchServiceSoftwareUpdate_domainName,
    cancelElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    cancelElasticsearchServiceSoftwareUpdateResponse_httpStatus,

    -- ** CreatePackage
    createPackage_packageDescription,
    createPackage_packageName,
    createPackage_packageType,
    createPackage_packageSource,
    createPackageResponse_packageDetails,
    createPackageResponse_httpStatus,

    -- ** RejectInboundCrossClusterSearchConnection
    rejectInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    rejectInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    rejectInboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** DescribeOutboundCrossClusterSearchConnections
    describeOutboundCrossClusterSearchConnections_filters,
    describeOutboundCrossClusterSearchConnections_nextToken,
    describeOutboundCrossClusterSearchConnections_maxResults,
    describeOutboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections,
    describeOutboundCrossClusterSearchConnectionsResponse_nextToken,
    describeOutboundCrossClusterSearchConnectionsResponse_httpStatus,

    -- ** AcceptInboundCrossClusterSearchConnection
    acceptInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    acceptInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    acceptInboundCrossClusterSearchConnectionResponse_httpStatus,

    -- * Types

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

    -- ** DomainInformation
    domainInformation_ownerId,
    domainInformation_region,
    domainInformation_domainName,

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

    -- ** ElasticsearchClusterConfig
    elasticsearchClusterConfig_dedicatedMasterCount,
    elasticsearchClusterConfig_dedicatedMasterType,
    elasticsearchClusterConfig_dedicatedMasterEnabled,
    elasticsearchClusterConfig_instanceCount,
    elasticsearchClusterConfig_coldStorageOptions,
    elasticsearchClusterConfig_zoneAwarenessEnabled,
    elasticsearchClusterConfig_instanceType,
    elasticsearchClusterConfig_warmEnabled,
    elasticsearchClusterConfig_zoneAwarenessConfig,
    elasticsearchClusterConfig_warmCount,
    elasticsearchClusterConfig_warmType,

    -- ** ElasticsearchClusterConfigStatus
    elasticsearchClusterConfigStatus_options,
    elasticsearchClusterConfigStatus_status,

    -- ** ElasticsearchDomainConfig
    elasticsearchDomainConfig_eBSOptions,
    elasticsearchDomainConfig_nodeToNodeEncryptionOptions,
    elasticsearchDomainConfig_accessPolicies,
    elasticsearchDomainConfig_autoTuneOptions,
    elasticsearchDomainConfig_logPublishingOptions,
    elasticsearchDomainConfig_advancedSecurityOptions,
    elasticsearchDomainConfig_elasticsearchClusterConfig,
    elasticsearchDomainConfig_snapshotOptions,
    elasticsearchDomainConfig_cognitoOptions,
    elasticsearchDomainConfig_encryptionAtRestOptions,
    elasticsearchDomainConfig_vPCOptions,
    elasticsearchDomainConfig_domainEndpointOptions,
    elasticsearchDomainConfig_advancedOptions,
    elasticsearchDomainConfig_elasticsearchVersion,

    -- ** ElasticsearchDomainStatus
    elasticsearchDomainStatus_eBSOptions,
    elasticsearchDomainStatus_nodeToNodeEncryptionOptions,
    elasticsearchDomainStatus_accessPolicies,
    elasticsearchDomainStatus_serviceSoftwareOptions,
    elasticsearchDomainStatus_autoTuneOptions,
    elasticsearchDomainStatus_logPublishingOptions,
    elasticsearchDomainStatus_advancedSecurityOptions,
    elasticsearchDomainStatus_created,
    elasticsearchDomainStatus_snapshotOptions,
    elasticsearchDomainStatus_cognitoOptions,
    elasticsearchDomainStatus_encryptionAtRestOptions,
    elasticsearchDomainStatus_deleted,
    elasticsearchDomainStatus_vPCOptions,
    elasticsearchDomainStatus_endpoints,
    elasticsearchDomainStatus_domainEndpointOptions,
    elasticsearchDomainStatus_processing,
    elasticsearchDomainStatus_endpoint,
    elasticsearchDomainStatus_upgradeProcessing,
    elasticsearchDomainStatus_advancedOptions,
    elasticsearchDomainStatus_elasticsearchVersion,
    elasticsearchDomainStatus_domainId,
    elasticsearchDomainStatus_domainName,
    elasticsearchDomainStatus_arn,
    elasticsearchDomainStatus_elasticsearchClusterConfig,

    -- ** ElasticsearchVersionStatus
    elasticsearchVersionStatus_options,
    elasticsearchVersionStatus_status,

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

    -- ** InboundCrossClusterSearchConnection
    inboundCrossClusterSearchConnection_destinationDomainInfo,
    inboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    inboundCrossClusterSearchConnection_connectionStatus,
    inboundCrossClusterSearchConnection_sourceDomainInfo,

    -- ** InboundCrossClusterSearchConnectionStatus
    inboundCrossClusterSearchConnectionStatus_message,
    inboundCrossClusterSearchConnectionStatus_statusCode,

    -- ** InstanceCountLimits
    instanceCountLimits_maximumInstanceCount,
    instanceCountLimits_minimumInstanceCount,

    -- ** InstanceLimits
    instanceLimits_instanceCountLimits,

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

    -- ** OutboundCrossClusterSearchConnection
    outboundCrossClusterSearchConnection_destinationDomainInfo,
    outboundCrossClusterSearchConnection_connectionAlias,
    outboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    outboundCrossClusterSearchConnection_connectionStatus,
    outboundCrossClusterSearchConnection_sourceDomainInfo,

    -- ** OutboundCrossClusterSearchConnectionStatus
    outboundCrossClusterSearchConnectionStatus_message,
    outboundCrossClusterSearchConnectionStatus_statusCode,

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

    -- ** ReservedElasticsearchInstance
    reservedElasticsearchInstance_state,
    reservedElasticsearchInstance_currencyCode,
    reservedElasticsearchInstance_startTime,
    reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstance_reservedElasticsearchInstanceId,
    reservedElasticsearchInstance_elasticsearchInstanceCount,
    reservedElasticsearchInstance_reservationName,
    reservedElasticsearchInstance_elasticsearchInstanceType,
    reservedElasticsearchInstance_recurringCharges,
    reservedElasticsearchInstance_usagePrice,
    reservedElasticsearchInstance_fixedPrice,
    reservedElasticsearchInstance_duration,
    reservedElasticsearchInstance_paymentOption,

    -- ** ReservedElasticsearchInstanceOffering
    reservedElasticsearchInstanceOffering_currencyCode,
    reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstanceOffering_elasticsearchInstanceType,
    reservedElasticsearchInstanceOffering_recurringCharges,
    reservedElasticsearchInstanceOffering_usagePrice,
    reservedElasticsearchInstanceOffering_fixedPrice,
    reservedElasticsearchInstanceOffering_duration,
    reservedElasticsearchInstanceOffering_paymentOption,

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

    -- ** ZoneAwarenessConfig
    zoneAwarenessConfig_availabilityZoneCount,
  )
where

import Amazonka.ElasticSearch.AcceptInboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.AddTags
import Amazonka.ElasticSearch.AssociatePackage
import Amazonka.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
import Amazonka.ElasticSearch.CreateElasticsearchDomain
import Amazonka.ElasticSearch.CreateOutboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.CreatePackage
import Amazonka.ElasticSearch.DeleteElasticsearchDomain
import Amazonka.ElasticSearch.DeleteElasticsearchServiceRole
import Amazonka.ElasticSearch.DeleteInboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.DeletePackage
import Amazonka.ElasticSearch.DescribeDomainAutoTunes
import Amazonka.ElasticSearch.DescribeElasticsearchDomain
import Amazonka.ElasticSearch.DescribeElasticsearchDomainConfig
import Amazonka.ElasticSearch.DescribeElasticsearchDomains
import Amazonka.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
import Amazonka.ElasticSearch.DescribeInboundCrossClusterSearchConnections
import Amazonka.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
import Amazonka.ElasticSearch.DescribePackages
import Amazonka.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
import Amazonka.ElasticSearch.DescribeReservedElasticsearchInstances
import Amazonka.ElasticSearch.DissociatePackage
import Amazonka.ElasticSearch.GetCompatibleElasticsearchVersions
import Amazonka.ElasticSearch.GetPackageVersionHistory
import Amazonka.ElasticSearch.GetUpgradeHistory
import Amazonka.ElasticSearch.GetUpgradeStatus
import Amazonka.ElasticSearch.ListDomainNames
import Amazonka.ElasticSearch.ListDomainsForPackage
import Amazonka.ElasticSearch.ListElasticsearchInstanceTypes
import Amazonka.ElasticSearch.ListElasticsearchVersions
import Amazonka.ElasticSearch.ListPackagesForDomain
import Amazonka.ElasticSearch.ListTags
import Amazonka.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
import Amazonka.ElasticSearch.RejectInboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.RemoveTags
import Amazonka.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
import Amazonka.ElasticSearch.Types.AccessPoliciesStatus
import Amazonka.ElasticSearch.Types.AdditionalLimit
import Amazonka.ElasticSearch.Types.AdvancedOptionsStatus
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptions
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsInput
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Amazonka.ElasticSearch.Types.AutoTune
import Amazonka.ElasticSearch.Types.AutoTuneDetails
import Amazonka.ElasticSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.ElasticSearch.Types.AutoTuneOptions
import Amazonka.ElasticSearch.Types.AutoTuneOptionsInput
import Amazonka.ElasticSearch.Types.AutoTuneOptionsOutput
import Amazonka.ElasticSearch.Types.AutoTuneOptionsStatus
import Amazonka.ElasticSearch.Types.AutoTuneStatus
import Amazonka.ElasticSearch.Types.CognitoOptions
import Amazonka.ElasticSearch.Types.CognitoOptionsStatus
import Amazonka.ElasticSearch.Types.ColdStorageOptions
import Amazonka.ElasticSearch.Types.CompatibleVersionsMap
import Amazonka.ElasticSearch.Types.DescribePackagesFilter
import Amazonka.ElasticSearch.Types.DomainEndpointOptions
import Amazonka.ElasticSearch.Types.DomainEndpointOptionsStatus
import Amazonka.ElasticSearch.Types.DomainInfo
import Amazonka.ElasticSearch.Types.DomainInformation
import Amazonka.ElasticSearch.Types.DomainPackageDetails
import Amazonka.ElasticSearch.Types.Duration
import Amazonka.ElasticSearch.Types.EBSOptions
import Amazonka.ElasticSearch.Types.EBSOptionsStatus
import Amazonka.ElasticSearch.Types.ElasticsearchClusterConfig
import Amazonka.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Amazonka.ElasticSearch.Types.ElasticsearchDomainConfig
import Amazonka.ElasticSearch.Types.ElasticsearchDomainStatus
import Amazonka.ElasticSearch.Types.ElasticsearchVersionStatus
import Amazonka.ElasticSearch.Types.EncryptionAtRestOptions
import Amazonka.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Amazonka.ElasticSearch.Types.ErrorDetails
import Amazonka.ElasticSearch.Types.Filter
import Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
import Amazonka.ElasticSearch.Types.InstanceCountLimits
import Amazonka.ElasticSearch.Types.InstanceLimits
import Amazonka.ElasticSearch.Types.Limits
import Amazonka.ElasticSearch.Types.LogPublishingOption
import Amazonka.ElasticSearch.Types.LogPublishingOptionsStatus
import Amazonka.ElasticSearch.Types.MasterUserOptions
import Amazonka.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Amazonka.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Amazonka.ElasticSearch.Types.OptionStatus
import Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
import Amazonka.ElasticSearch.Types.PackageDetails
import Amazonka.ElasticSearch.Types.PackageSource
import Amazonka.ElasticSearch.Types.PackageVersionHistory
import Amazonka.ElasticSearch.Types.RecurringCharge
import Amazonka.ElasticSearch.Types.ReservedElasticsearchInstance
import Amazonka.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
import Amazonka.ElasticSearch.Types.SAMLIdp
import Amazonka.ElasticSearch.Types.SAMLOptionsInput
import Amazonka.ElasticSearch.Types.SAMLOptionsOutput
import Amazonka.ElasticSearch.Types.ScheduledAutoTuneDetails
import Amazonka.ElasticSearch.Types.ServiceSoftwareOptions
import Amazonka.ElasticSearch.Types.SnapshotOptions
import Amazonka.ElasticSearch.Types.SnapshotOptionsStatus
import Amazonka.ElasticSearch.Types.StorageType
import Amazonka.ElasticSearch.Types.StorageTypeLimit
import Amazonka.ElasticSearch.Types.Tag
import Amazonka.ElasticSearch.Types.UpgradeHistory
import Amazonka.ElasticSearch.Types.UpgradeStepItem
import Amazonka.ElasticSearch.Types.VPCDerivedInfo
import Amazonka.ElasticSearch.Types.VPCDerivedInfoStatus
import Amazonka.ElasticSearch.Types.VPCOptions
import Amazonka.ElasticSearch.Types.ZoneAwarenessConfig
import Amazonka.ElasticSearch.UpdateElasticsearchDomainConfig
import Amazonka.ElasticSearch.UpdatePackage
import Amazonka.ElasticSearch.UpgradeElasticsearchDomain
