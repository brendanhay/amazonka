{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Lens
  ( -- * Operations

    -- ** DescribeInboundCrossClusterSearchConnections
    describeInboundCrossClusterSearchConnections_nextToken,
    describeInboundCrossClusterSearchConnections_maxResults,
    describeInboundCrossClusterSearchConnections_filters,
    describeInboundCrossClusterSearchConnectionsResponse_nextToken,
    describeInboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections,
    describeInboundCrossClusterSearchConnectionsResponse_httpStatus,

    -- ** RemoveTags
    removeTags_arn,
    removeTags_tagKeys,

    -- ** CreateOutboundCrossClusterSearchConnection
    createOutboundCrossClusterSearchConnection_sourceDomainInfo,
    createOutboundCrossClusterSearchConnection_destinationDomainInfo,
    createOutboundCrossClusterSearchConnection_connectionAlias,
    createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId,
    createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_connectionAlias,
    createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_connectionStatus,
    createOutboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** GetUpgradeHistory
    getUpgradeHistory_nextToken,
    getUpgradeHistory_maxResults,
    getUpgradeHistory_domainName,
    getUpgradeHistoryResponse_nextToken,
    getUpgradeHistoryResponse_upgradeHistories,
    getUpgradeHistoryResponse_httpStatus,

    -- ** DescribeElasticsearchDomainConfig
    describeElasticsearchDomainConfig_domainName,
    describeElasticsearchDomainConfigResponse_httpStatus,
    describeElasticsearchDomainConfigResponse_domainConfig,

    -- ** AcceptInboundCrossClusterSearchConnection
    acceptInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    acceptInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    acceptInboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** DeleteOutboundCrossClusterSearchConnection
    deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteOutboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** ListDomainNames
    listDomainNamesResponse_domainNames,
    listDomainNamesResponse_httpStatus,

    -- ** CancelElasticsearchServiceSoftwareUpdate
    cancelElasticsearchServiceSoftwareUpdate_domainName,
    cancelElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    cancelElasticsearchServiceSoftwareUpdateResponse_httpStatus,

    -- ** DescribeElasticsearchDomain
    describeElasticsearchDomain_domainName,
    describeElasticsearchDomainResponse_httpStatus,
    describeElasticsearchDomainResponse_domainStatus,

    -- ** DeleteElasticsearchServiceRole

    -- ** ListElasticsearchInstanceTypes
    listElasticsearchInstanceTypes_nextToken,
    listElasticsearchInstanceTypes_maxResults,
    listElasticsearchInstanceTypes_domainName,
    listElasticsearchInstanceTypes_elasticsearchVersion,
    listElasticsearchInstanceTypesResponse_nextToken,
    listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes,
    listElasticsearchInstanceTypesResponse_httpStatus,

    -- ** UpdatePackage
    updatePackage_commitMessage,
    updatePackage_packageDescription,
    updatePackage_packageID,
    updatePackage_packageSource,
    updatePackageResponse_packageDetails,
    updatePackageResponse_httpStatus,

    -- ** DeletePackage
    deletePackage_packageID,
    deletePackageResponse_packageDetails,
    deletePackageResponse_httpStatus,

    -- ** AddTags
    addTags_arn,
    addTags_tagList,

    -- ** DeleteInboundCrossClusterSearchConnection
    deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteInboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** UpdateElasticsearchDomainConfig
    updateElasticsearchDomainConfig_eBSOptions,
    updateElasticsearchDomainConfig_snapshotOptions,
    updateElasticsearchDomainConfig_elasticsearchClusterConfig,
    updateElasticsearchDomainConfig_domainEndpointOptions,
    updateElasticsearchDomainConfig_vPCOptions,
    updateElasticsearchDomainConfig_autoTuneOptions,
    updateElasticsearchDomainConfig_accessPolicies,
    updateElasticsearchDomainConfig_encryptionAtRestOptions,
    updateElasticsearchDomainConfig_cognitoOptions,
    updateElasticsearchDomainConfig_nodeToNodeEncryptionOptions,
    updateElasticsearchDomainConfig_advancedOptions,
    updateElasticsearchDomainConfig_advancedSecurityOptions,
    updateElasticsearchDomainConfig_logPublishingOptions,
    updateElasticsearchDomainConfig_domainName,
    updateElasticsearchDomainConfigResponse_httpStatus,
    updateElasticsearchDomainConfigResponse_domainConfig,

    -- ** ListElasticsearchVersions
    listElasticsearchVersions_nextToken,
    listElasticsearchVersions_maxResults,
    listElasticsearchVersionsResponse_nextToken,
    listElasticsearchVersionsResponse_elasticsearchVersions,
    listElasticsearchVersionsResponse_httpStatus,

    -- ** DeleteElasticsearchDomain
    deleteElasticsearchDomain_domainName,
    deleteElasticsearchDomainResponse_domainStatus,
    deleteElasticsearchDomainResponse_httpStatus,

    -- ** GetCompatibleElasticsearchVersions
    getCompatibleElasticsearchVersions_domainName,
    getCompatibleElasticsearchVersionsResponse_compatibleElasticsearchVersions,
    getCompatibleElasticsearchVersionsResponse_httpStatus,

    -- ** DissociatePackage
    dissociatePackage_packageID,
    dissociatePackage_domainName,
    dissociatePackageResponse_domainPackageDetails,
    dissociatePackageResponse_httpStatus,

    -- ** CreateElasticsearchDomain
    createElasticsearchDomain_eBSOptions,
    createElasticsearchDomain_snapshotOptions,
    createElasticsearchDomain_elasticsearchClusterConfig,
    createElasticsearchDomain_domainEndpointOptions,
    createElasticsearchDomain_vPCOptions,
    createElasticsearchDomain_autoTuneOptions,
    createElasticsearchDomain_accessPolicies,
    createElasticsearchDomain_encryptionAtRestOptions,
    createElasticsearchDomain_cognitoOptions,
    createElasticsearchDomain_nodeToNodeEncryptionOptions,
    createElasticsearchDomain_elasticsearchVersion,
    createElasticsearchDomain_advancedOptions,
    createElasticsearchDomain_tagList,
    createElasticsearchDomain_advancedSecurityOptions,
    createElasticsearchDomain_logPublishingOptions,
    createElasticsearchDomain_domainName,
    createElasticsearchDomainResponse_domainStatus,
    createElasticsearchDomainResponse_httpStatus,

    -- ** DescribePackages
    describePackages_nextToken,
    describePackages_maxResults,
    describePackages_filters,
    describePackagesResponse_nextToken,
    describePackagesResponse_packageDetailsList,
    describePackagesResponse_httpStatus,

    -- ** GetPackageVersionHistory
    getPackageVersionHistory_nextToken,
    getPackageVersionHistory_maxResults,
    getPackageVersionHistory_packageID,
    getPackageVersionHistoryResponse_nextToken,
    getPackageVersionHistoryResponse_packageID,
    getPackageVersionHistoryResponse_packageVersionHistoryList,
    getPackageVersionHistoryResponse_httpStatus,

    -- ** DescribeElasticsearchInstanceTypeLimits
    describeElasticsearchInstanceTypeLimits_domainName,
    describeElasticsearchInstanceTypeLimits_instanceType,
    describeElasticsearchInstanceTypeLimits_elasticsearchVersion,
    describeElasticsearchInstanceTypeLimitsResponse_limitsByRole,
    describeElasticsearchInstanceTypeLimitsResponse_httpStatus,

    -- ** DescribeOutboundCrossClusterSearchConnections
    describeOutboundCrossClusterSearchConnections_nextToken,
    describeOutboundCrossClusterSearchConnections_maxResults,
    describeOutboundCrossClusterSearchConnections_filters,
    describeOutboundCrossClusterSearchConnectionsResponse_nextToken,
    describeOutboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections,
    describeOutboundCrossClusterSearchConnectionsResponse_httpStatus,

    -- ** AssociatePackage
    associatePackage_packageID,
    associatePackage_domainName,
    associatePackageResponse_domainPackageDetails,
    associatePackageResponse_httpStatus,

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

    -- ** DescribeDomainAutoTunes
    describeDomainAutoTunes_nextToken,
    describeDomainAutoTunes_maxResults,
    describeDomainAutoTunes_domainName,
    describeDomainAutoTunesResponse_nextToken,
    describeDomainAutoTunesResponse_autoTunes,
    describeDomainAutoTunesResponse_httpStatus,

    -- ** ListTags
    listTags_arn,
    listTagsResponse_tagList,
    listTagsResponse_httpStatus,

    -- ** UpgradeElasticsearchDomain
    upgradeElasticsearchDomain_performCheckOnly,
    upgradeElasticsearchDomain_domainName,
    upgradeElasticsearchDomain_targetVersion,
    upgradeElasticsearchDomainResponse_targetVersion,
    upgradeElasticsearchDomainResponse_domainName,
    upgradeElasticsearchDomainResponse_performCheckOnly,
    upgradeElasticsearchDomainResponse_httpStatus,

    -- ** ListPackagesForDomain
    listPackagesForDomain_nextToken,
    listPackagesForDomain_maxResults,
    listPackagesForDomain_domainName,
    listPackagesForDomainResponse_nextToken,
    listPackagesForDomainResponse_domainPackageDetailsList,
    listPackagesForDomainResponse_httpStatus,

    -- ** DescribeReservedElasticsearchInstances
    describeReservedElasticsearchInstances_nextToken,
    describeReservedElasticsearchInstances_maxResults,
    describeReservedElasticsearchInstances_reservedElasticsearchInstanceId,
    describeReservedElasticsearchInstancesResponse_nextToken,
    describeReservedElasticsearchInstancesResponse_reservedElasticsearchInstances,
    describeReservedElasticsearchInstancesResponse_httpStatus,

    -- ** DescribeReservedElasticsearchInstanceOfferings
    describeReservedElasticsearchInstanceOfferings_nextToken,
    describeReservedElasticsearchInstanceOfferings_maxResults,
    describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId,
    describeReservedElasticsearchInstanceOfferingsResponse_nextToken,
    describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings,
    describeReservedElasticsearchInstanceOfferingsResponse_httpStatus,

    -- ** StartElasticsearchServiceSoftwareUpdate
    startElasticsearchServiceSoftwareUpdate_domainName,
    startElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    startElasticsearchServiceSoftwareUpdateResponse_httpStatus,

    -- ** ListDomainsForPackage
    listDomainsForPackage_nextToken,
    listDomainsForPackage_maxResults,
    listDomainsForPackage_packageID,
    listDomainsForPackageResponse_nextToken,
    listDomainsForPackageResponse_domainPackageDetailsList,
    listDomainsForPackageResponse_httpStatus,

    -- ** DescribeElasticsearchDomains
    describeElasticsearchDomains_domainNames,
    describeElasticsearchDomainsResponse_httpStatus,
    describeElasticsearchDomainsResponse_domainStatusList,

    -- ** PurchaseReservedElasticsearchInstanceOffering
    purchaseReservedElasticsearchInstanceOffering_instanceCount,
    purchaseReservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId,
    purchaseReservedElasticsearchInstanceOffering_reservationName,
    purchaseReservedElasticsearchInstanceOfferingResponse_reservationName,
    purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId,
    purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus,

    -- ** GetUpgradeStatus
    getUpgradeStatus_domainName,
    getUpgradeStatusResponse_upgradeName,
    getUpgradeStatusResponse_upgradeStep,
    getUpgradeStatusResponse_stepStatus,
    getUpgradeStatusResponse_httpStatus,

    -- * Types

    -- ** AccessPoliciesStatus
    accessPoliciesStatus_options,
    accessPoliciesStatus_status,

    -- ** AdditionalLimit
    additionalLimit_limitValues,
    additionalLimit_limitName,

    -- ** AdvancedOptionsStatus
    advancedOptionsStatus_options,
    advancedOptionsStatus_status,

    -- ** AdvancedSecurityOptions
    advancedSecurityOptions_internalUserDatabaseEnabled,
    advancedSecurityOptions_sAMLOptions,
    advancedSecurityOptions_enabled,

    -- ** AdvancedSecurityOptionsInput
    advancedSecurityOptionsInput_internalUserDatabaseEnabled,
    advancedSecurityOptionsInput_sAMLOptions,
    advancedSecurityOptionsInput_enabled,
    advancedSecurityOptionsInput_masterUserOptions,

    -- ** AdvancedSecurityOptionsStatus
    advancedSecurityOptionsStatus_options,
    advancedSecurityOptionsStatus_status,

    -- ** AutoTune
    autoTune_autoTuneType,
    autoTune_autoTuneDetails,

    -- ** AutoTuneDetails
    autoTuneDetails_scheduledAutoTuneDetails,

    -- ** AutoTuneMaintenanceSchedule
    autoTuneMaintenanceSchedule_duration,
    autoTuneMaintenanceSchedule_startAt,
    autoTuneMaintenanceSchedule_cronExpressionForRecurrence,

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
    autoTuneStatus_updateVersion,
    autoTuneStatus_errorMessage,
    autoTuneStatus_pendingDeletion,
    autoTuneStatus_creationDate,
    autoTuneStatus_updateDate,
    autoTuneStatus_state,

    -- ** CognitoOptions
    cognitoOptions_identityPoolId,
    cognitoOptions_roleArn,
    cognitoOptions_userPoolId,
    cognitoOptions_enabled,

    -- ** CognitoOptionsStatus
    cognitoOptionsStatus_options,
    cognitoOptionsStatus_status,

    -- ** CompatibleVersionsMap
    compatibleVersionsMap_sourceVersion,
    compatibleVersionsMap_targetVersions,

    -- ** DescribePackagesFilter
    describePackagesFilter_name,
    describePackagesFilter_value,

    -- ** DomainEndpointOptions
    domainEndpointOptions_customEndpointCertificateArn,
    domainEndpointOptions_customEndpoint,
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_tLSSecurityPolicy,
    domainEndpointOptions_customEndpointEnabled,

    -- ** DomainEndpointOptionsStatus
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- ** DomainInfo
    domainInfo_domainName,

    -- ** DomainInformation
    domainInformation_ownerId,
    domainInformation_region,
    domainInformation_domainName,

    -- ** DomainPackageDetails
    domainPackageDetails_domainPackageStatus,
    domainPackageDetails_packageVersion,
    domainPackageDetails_packageName,
    domainPackageDetails_lastUpdated,
    domainPackageDetails_packageID,
    domainPackageDetails_domainName,
    domainPackageDetails_referencePath,
    domainPackageDetails_packageType,
    domainPackageDetails_errorDetails,

    -- ** Duration
    duration_unit,
    duration_value,

    -- ** EBSOptions
    eBSOptions_eBSEnabled,
    eBSOptions_volumeType,
    eBSOptions_volumeSize,
    eBSOptions_iops,

    -- ** EBSOptionsStatus
    eBSOptionsStatus_options,
    eBSOptionsStatus_status,

    -- ** ElasticsearchClusterConfig
    elasticsearchClusterConfig_zoneAwarenessConfig,
    elasticsearchClusterConfig_dedicatedMasterCount,
    elasticsearchClusterConfig_warmEnabled,
    elasticsearchClusterConfig_instanceType,
    elasticsearchClusterConfig_zoneAwarenessEnabled,
    elasticsearchClusterConfig_dedicatedMasterEnabled,
    elasticsearchClusterConfig_warmCount,
    elasticsearchClusterConfig_dedicatedMasterType,
    elasticsearchClusterConfig_warmType,
    elasticsearchClusterConfig_instanceCount,

    -- ** ElasticsearchClusterConfigStatus
    elasticsearchClusterConfigStatus_options,
    elasticsearchClusterConfigStatus_status,

    -- ** ElasticsearchDomainConfig
    elasticsearchDomainConfig_eBSOptions,
    elasticsearchDomainConfig_snapshotOptions,
    elasticsearchDomainConfig_elasticsearchClusterConfig,
    elasticsearchDomainConfig_domainEndpointOptions,
    elasticsearchDomainConfig_vPCOptions,
    elasticsearchDomainConfig_autoTuneOptions,
    elasticsearchDomainConfig_accessPolicies,
    elasticsearchDomainConfig_encryptionAtRestOptions,
    elasticsearchDomainConfig_cognitoOptions,
    elasticsearchDomainConfig_nodeToNodeEncryptionOptions,
    elasticsearchDomainConfig_elasticsearchVersion,
    elasticsearchDomainConfig_advancedOptions,
    elasticsearchDomainConfig_advancedSecurityOptions,
    elasticsearchDomainConfig_logPublishingOptions,

    -- ** ElasticsearchDomainStatus
    elasticsearchDomainStatus_eBSOptions,
    elasticsearchDomainStatus_snapshotOptions,
    elasticsearchDomainStatus_domainEndpointOptions,
    elasticsearchDomainStatus_upgradeProcessing,
    elasticsearchDomainStatus_endpoints,
    elasticsearchDomainStatus_vPCOptions,
    elasticsearchDomainStatus_autoTuneOptions,
    elasticsearchDomainStatus_accessPolicies,
    elasticsearchDomainStatus_encryptionAtRestOptions,
    elasticsearchDomainStatus_serviceSoftwareOptions,
    elasticsearchDomainStatus_cognitoOptions,
    elasticsearchDomainStatus_nodeToNodeEncryptionOptions,
    elasticsearchDomainStatus_elasticsearchVersion,
    elasticsearchDomainStatus_advancedOptions,
    elasticsearchDomainStatus_processing,
    elasticsearchDomainStatus_endpoint,
    elasticsearchDomainStatus_created,
    elasticsearchDomainStatus_advancedSecurityOptions,
    elasticsearchDomainStatus_logPublishingOptions,
    elasticsearchDomainStatus_deleted,
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
    inboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    inboundCrossClusterSearchConnection_sourceDomainInfo,
    inboundCrossClusterSearchConnection_destinationDomainInfo,
    inboundCrossClusterSearchConnection_connectionStatus,

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
    optionStatus_updateVersion,
    optionStatus_pendingDeletion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- ** OutboundCrossClusterSearchConnection
    outboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    outboundCrossClusterSearchConnection_sourceDomainInfo,
    outboundCrossClusterSearchConnection_connectionAlias,
    outboundCrossClusterSearchConnection_destinationDomainInfo,
    outboundCrossClusterSearchConnection_connectionStatus,

    -- ** OutboundCrossClusterSearchConnectionStatus
    outboundCrossClusterSearchConnectionStatus_message,
    outboundCrossClusterSearchConnectionStatus_statusCode,

    -- ** PackageDetails
    packageDetails_availablePackageVersion,
    packageDetails_packageStatus,
    packageDetails_packageName,
    packageDetails_createdAt,
    packageDetails_packageID,
    packageDetails_packageDescription,
    packageDetails_lastUpdatedAt,
    packageDetails_packageType,
    packageDetails_errorDetails,

    -- ** PackageSource
    packageSource_s3Key,
    packageSource_s3BucketName,

    -- ** PackageVersionHistory
    packageVersionHistory_packageVersion,
    packageVersionHistory_createdAt,
    packageVersionHistory_commitMessage,

    -- ** RecurringCharge
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- ** ReservedElasticsearchInstance
    reservedElasticsearchInstance_reservationName,
    reservedElasticsearchInstance_paymentOption,
    reservedElasticsearchInstance_elasticsearchInstanceCount,
    reservedElasticsearchInstance_duration,
    reservedElasticsearchInstance_startTime,
    reservedElasticsearchInstance_currencyCode,
    reservedElasticsearchInstance_elasticsearchInstanceType,
    reservedElasticsearchInstance_state,
    reservedElasticsearchInstance_fixedPrice,
    reservedElasticsearchInstance_reservedElasticsearchInstanceId,
    reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstance_usagePrice,
    reservedElasticsearchInstance_recurringCharges,

    -- ** ReservedElasticsearchInstanceOffering
    reservedElasticsearchInstanceOffering_paymentOption,
    reservedElasticsearchInstanceOffering_duration,
    reservedElasticsearchInstanceOffering_currencyCode,
    reservedElasticsearchInstanceOffering_elasticsearchInstanceType,
    reservedElasticsearchInstanceOffering_fixedPrice,
    reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstanceOffering_usagePrice,
    reservedElasticsearchInstanceOffering_recurringCharges,

    -- ** SAMLIdp
    sAMLIdp_metadataContent,
    sAMLIdp_entityId,

    -- ** SAMLOptionsInput
    sAMLOptionsInput_masterBackendRole,
    sAMLOptionsInput_rolesKey,
    sAMLOptionsInput_sessionTimeoutMinutes,
    sAMLOptionsInput_idp,
    sAMLOptionsInput_enabled,
    sAMLOptionsInput_masterUserName,
    sAMLOptionsInput_subjectKey,

    -- ** SAMLOptionsOutput
    sAMLOptionsOutput_rolesKey,
    sAMLOptionsOutput_sessionTimeoutMinutes,
    sAMLOptionsOutput_idp,
    sAMLOptionsOutput_enabled,
    sAMLOptionsOutput_subjectKey,

    -- ** ScheduledAutoTuneDetails
    scheduledAutoTuneDetails_actionType,
    scheduledAutoTuneDetails_severity,
    scheduledAutoTuneDetails_date,
    scheduledAutoTuneDetails_action,

    -- ** ServiceSoftwareOptions
    serviceSoftwareOptions_newVersion,
    serviceSoftwareOptions_currentVersion,
    serviceSoftwareOptions_updateAvailable,
    serviceSoftwareOptions_cancellable,
    serviceSoftwareOptions_updateStatus,
    serviceSoftwareOptions_optionalDeployment,
    serviceSoftwareOptions_description,
    serviceSoftwareOptions_automatedUpdateDate,

    -- ** SnapshotOptions
    snapshotOptions_automatedSnapshotStartHour,

    -- ** SnapshotOptionsStatus
    snapshotOptionsStatus_options,
    snapshotOptionsStatus_status,

    -- ** StorageType
    storageType_storageTypeLimits,
    storageType_storageTypeName,
    storageType_storageSubTypeName,

    -- ** StorageTypeLimit
    storageTypeLimit_limitValues,
    storageTypeLimit_limitName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UpgradeHistory
    upgradeHistory_upgradeName,
    upgradeHistory_startTimestamp,
    upgradeHistory_upgradeStatus,
    upgradeHistory_stepsList,

    -- ** UpgradeStepItem
    upgradeStepItem_upgradeStepStatus,
    upgradeStepItem_progressPercent,
    upgradeStepItem_upgradeStep,
    upgradeStepItem_issues,

    -- ** VPCDerivedInfo
    vPCDerivedInfo_securityGroupIds,
    vPCDerivedInfo_availabilityZones,
    vPCDerivedInfo_subnetIds,
    vPCDerivedInfo_vPCId,

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

import Network.AWS.ElasticSearch.AcceptInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.AddTags
import Network.AWS.ElasticSearch.AssociatePackage
import Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
import Network.AWS.ElasticSearch.CreateElasticsearchDomain
import Network.AWS.ElasticSearch.CreateOutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.CreatePackage
import Network.AWS.ElasticSearch.DeleteElasticsearchDomain
import Network.AWS.ElasticSearch.DeleteElasticsearchServiceRole
import Network.AWS.ElasticSearch.DeleteInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.DeletePackage
import Network.AWS.ElasticSearch.DescribeDomainAutoTunes
import Network.AWS.ElasticSearch.DescribeElasticsearchDomain
import Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
import Network.AWS.ElasticSearch.DescribeElasticsearchDomains
import Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
import Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections
import Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
import Network.AWS.ElasticSearch.DescribePackages
import Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
import Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
import Network.AWS.ElasticSearch.DissociatePackage
import Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
import Network.AWS.ElasticSearch.GetPackageVersionHistory
import Network.AWS.ElasticSearch.GetUpgradeHistory
import Network.AWS.ElasticSearch.GetUpgradeStatus
import Network.AWS.ElasticSearch.ListDomainNames
import Network.AWS.ElasticSearch.ListDomainsForPackage
import Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
import Network.AWS.ElasticSearch.ListElasticsearchVersions
import Network.AWS.ElasticSearch.ListPackagesForDomain
import Network.AWS.ElasticSearch.ListTags
import Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
import Network.AWS.ElasticSearch.RejectInboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.RemoveTags
import Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
import Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
import Network.AWS.ElasticSearch.Types.AdditionalLimit
import Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Network.AWS.ElasticSearch.Types.AutoTune
import Network.AWS.ElasticSearch.Types.AutoTuneDetails
import Network.AWS.ElasticSearch.Types.AutoTuneMaintenanceSchedule
import Network.AWS.ElasticSearch.Types.AutoTuneOptions
import Network.AWS.ElasticSearch.Types.AutoTuneOptionsInput
import Network.AWS.ElasticSearch.Types.AutoTuneOptionsOutput
import Network.AWS.ElasticSearch.Types.AutoTuneOptionsStatus
import Network.AWS.ElasticSearch.Types.AutoTuneStatus
import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
import Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
import Network.AWS.ElasticSearch.Types.DescribePackagesFilter
import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
import Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
import Network.AWS.ElasticSearch.Types.DomainInfo
import Network.AWS.ElasticSearch.Types.DomainInformation
import Network.AWS.ElasticSearch.Types.DomainPackageDetails
import Network.AWS.ElasticSearch.Types.Duration
import Network.AWS.ElasticSearch.Types.EBSOptions
import Network.AWS.ElasticSearch.Types.EBSOptionsStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.Filter
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
import Network.AWS.ElasticSearch.Types.InstanceCountLimits
import Network.AWS.ElasticSearch.Types.InstanceLimits
import Network.AWS.ElasticSearch.Types.Limits
import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
import Network.AWS.ElasticSearch.Types.MasterUserOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
import Network.AWS.ElasticSearch.Types.PackageDetails
import Network.AWS.ElasticSearch.Types.PackageSource
import Network.AWS.ElasticSearch.Types.PackageVersionHistory
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
import Network.AWS.ElasticSearch.Types.SAMLIdp
import Network.AWS.ElasticSearch.Types.SAMLOptionsInput
import Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
import Network.AWS.ElasticSearch.Types.ScheduledAutoTuneDetails
import Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
import Network.AWS.ElasticSearch.Types.StorageType
import Network.AWS.ElasticSearch.Types.StorageTypeLimit
import Network.AWS.ElasticSearch.Types.Tag
import Network.AWS.ElasticSearch.Types.UpgradeHistory
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
import Network.AWS.ElasticSearch.Types.VPCOptions
import Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
import Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
import Network.AWS.ElasticSearch.UpdatePackage
import Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
