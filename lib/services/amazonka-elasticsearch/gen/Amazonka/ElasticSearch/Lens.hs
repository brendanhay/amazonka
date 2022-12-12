{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticSearch.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Lens
  ( -- * Operations

    -- ** AcceptInboundCrossClusterSearchConnection
    acceptInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    acceptInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    acceptInboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** AddTags
    addTags_arn,
    addTags_tagList,

    -- ** AssociatePackage
    associatePackage_packageID,
    associatePackage_domainName,
    associatePackageResponse_domainPackageDetails,
    associatePackageResponse_httpStatus,

    -- ** AuthorizeVpcEndpointAccess
    authorizeVpcEndpointAccess_domainName,
    authorizeVpcEndpointAccess_account,
    authorizeVpcEndpointAccessResponse_httpStatus,
    authorizeVpcEndpointAccessResponse_authorizedPrincipal,

    -- ** CancelElasticsearchServiceSoftwareUpdate
    cancelElasticsearchServiceSoftwareUpdate_domainName,
    cancelElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    cancelElasticsearchServiceSoftwareUpdateResponse_httpStatus,

    -- ** CreateElasticsearchDomain
    createElasticsearchDomain_accessPolicies,
    createElasticsearchDomain_advancedOptions,
    createElasticsearchDomain_advancedSecurityOptions,
    createElasticsearchDomain_autoTuneOptions,
    createElasticsearchDomain_cognitoOptions,
    createElasticsearchDomain_domainEndpointOptions,
    createElasticsearchDomain_eBSOptions,
    createElasticsearchDomain_elasticsearchClusterConfig,
    createElasticsearchDomain_elasticsearchVersion,
    createElasticsearchDomain_encryptionAtRestOptions,
    createElasticsearchDomain_logPublishingOptions,
    createElasticsearchDomain_nodeToNodeEncryptionOptions,
    createElasticsearchDomain_snapshotOptions,
    createElasticsearchDomain_tagList,
    createElasticsearchDomain_vPCOptions,
    createElasticsearchDomain_domainName,
    createElasticsearchDomainResponse_domainStatus,
    createElasticsearchDomainResponse_httpStatus,

    -- ** CreateOutboundCrossClusterSearchConnection
    createOutboundCrossClusterSearchConnection_sourceDomainInfo,
    createOutboundCrossClusterSearchConnection_destinationDomainInfo,
    createOutboundCrossClusterSearchConnection_connectionAlias,
    createOutboundCrossClusterSearchConnectionResponse_connectionAlias,
    createOutboundCrossClusterSearchConnectionResponse_connectionStatus,
    createOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnectionId,
    createOutboundCrossClusterSearchConnectionResponse_destinationDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_sourceDomainInfo,
    createOutboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** CreatePackage
    createPackage_packageDescription,
    createPackage_packageName,
    createPackage_packageType,
    createPackage_packageSource,
    createPackageResponse_packageDetails,
    createPackageResponse_httpStatus,

    -- ** CreateVpcEndpoint
    createVpcEndpoint_clientToken,
    createVpcEndpoint_domainArn,
    createVpcEndpoint_vpcOptions,
    createVpcEndpointResponse_httpStatus,
    createVpcEndpointResponse_vpcEndpoint,

    -- ** DeleteElasticsearchDomain
    deleteElasticsearchDomain_domainName,
    deleteElasticsearchDomainResponse_domainStatus,
    deleteElasticsearchDomainResponse_httpStatus,

    -- ** DeleteElasticsearchServiceRole

    -- ** DeleteInboundCrossClusterSearchConnection
    deleteInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    deleteInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteInboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** DeleteOutboundCrossClusterSearchConnection
    deleteOutboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    deleteOutboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    deleteOutboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** DeletePackage
    deletePackage_packageID,
    deletePackageResponse_packageDetails,
    deletePackageResponse_httpStatus,

    -- ** DeleteVpcEndpoint
    deleteVpcEndpoint_vpcEndpointId,
    deleteVpcEndpointResponse_httpStatus,
    deleteVpcEndpointResponse_vpcEndpointSummary,

    -- ** DescribeDomainAutoTunes
    describeDomainAutoTunes_maxResults,
    describeDomainAutoTunes_nextToken,
    describeDomainAutoTunes_domainName,
    describeDomainAutoTunesResponse_autoTunes,
    describeDomainAutoTunesResponse_nextToken,
    describeDomainAutoTunesResponse_httpStatus,

    -- ** DescribeDomainChangeProgress
    describeDomainChangeProgress_changeId,
    describeDomainChangeProgress_domainName,
    describeDomainChangeProgressResponse_changeProgressStatus,
    describeDomainChangeProgressResponse_httpStatus,

    -- ** DescribeElasticsearchDomain
    describeElasticsearchDomain_domainName,
    describeElasticsearchDomainResponse_httpStatus,
    describeElasticsearchDomainResponse_domainStatus,

    -- ** DescribeElasticsearchDomainConfig
    describeElasticsearchDomainConfig_domainName,
    describeElasticsearchDomainConfigResponse_httpStatus,
    describeElasticsearchDomainConfigResponse_domainConfig,

    -- ** DescribeElasticsearchDomains
    describeElasticsearchDomains_domainNames,
    describeElasticsearchDomainsResponse_httpStatus,
    describeElasticsearchDomainsResponse_domainStatusList,

    -- ** DescribeElasticsearchInstanceTypeLimits
    describeElasticsearchInstanceTypeLimits_domainName,
    describeElasticsearchInstanceTypeLimits_instanceType,
    describeElasticsearchInstanceTypeLimits_elasticsearchVersion,
    describeElasticsearchInstanceTypeLimitsResponse_limitsByRole,
    describeElasticsearchInstanceTypeLimitsResponse_httpStatus,

    -- ** DescribeInboundCrossClusterSearchConnections
    describeInboundCrossClusterSearchConnections_filters,
    describeInboundCrossClusterSearchConnections_maxResults,
    describeInboundCrossClusterSearchConnections_nextToken,
    describeInboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections,
    describeInboundCrossClusterSearchConnectionsResponse_nextToken,
    describeInboundCrossClusterSearchConnectionsResponse_httpStatus,

    -- ** DescribeOutboundCrossClusterSearchConnections
    describeOutboundCrossClusterSearchConnections_filters,
    describeOutboundCrossClusterSearchConnections_maxResults,
    describeOutboundCrossClusterSearchConnections_nextToken,
    describeOutboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections,
    describeOutboundCrossClusterSearchConnectionsResponse_nextToken,
    describeOutboundCrossClusterSearchConnectionsResponse_httpStatus,

    -- ** DescribePackages
    describePackages_filters,
    describePackages_maxResults,
    describePackages_nextToken,
    describePackagesResponse_nextToken,
    describePackagesResponse_packageDetailsList,
    describePackagesResponse_httpStatus,

    -- ** DescribeReservedElasticsearchInstanceOfferings
    describeReservedElasticsearchInstanceOfferings_maxResults,
    describeReservedElasticsearchInstanceOfferings_nextToken,
    describeReservedElasticsearchInstanceOfferings_reservedElasticsearchInstanceOfferingId,
    describeReservedElasticsearchInstanceOfferingsResponse_nextToken,
    describeReservedElasticsearchInstanceOfferingsResponse_reservedElasticsearchInstanceOfferings,
    describeReservedElasticsearchInstanceOfferingsResponse_httpStatus,

    -- ** DescribeReservedElasticsearchInstances
    describeReservedElasticsearchInstances_maxResults,
    describeReservedElasticsearchInstances_nextToken,
    describeReservedElasticsearchInstances_reservedElasticsearchInstanceId,
    describeReservedElasticsearchInstancesResponse_nextToken,
    describeReservedElasticsearchInstancesResponse_reservedElasticsearchInstances,
    describeReservedElasticsearchInstancesResponse_httpStatus,

    -- ** DescribeVpcEndpoints
    describeVpcEndpoints_vpcEndpointIds,
    describeVpcEndpointsResponse_httpStatus,
    describeVpcEndpointsResponse_vpcEndpoints,
    describeVpcEndpointsResponse_vpcEndpointErrors,

    -- ** DissociatePackage
    dissociatePackage_packageID,
    dissociatePackage_domainName,
    dissociatePackageResponse_domainPackageDetails,
    dissociatePackageResponse_httpStatus,

    -- ** GetCompatibleElasticsearchVersions
    getCompatibleElasticsearchVersions_domainName,
    getCompatibleElasticsearchVersionsResponse_compatibleElasticsearchVersions,
    getCompatibleElasticsearchVersionsResponse_httpStatus,

    -- ** GetPackageVersionHistory
    getPackageVersionHistory_maxResults,
    getPackageVersionHistory_nextToken,
    getPackageVersionHistory_packageID,
    getPackageVersionHistoryResponse_nextToken,
    getPackageVersionHistoryResponse_packageID,
    getPackageVersionHistoryResponse_packageVersionHistoryList,
    getPackageVersionHistoryResponse_httpStatus,

    -- ** GetUpgradeHistory
    getUpgradeHistory_maxResults,
    getUpgradeHistory_nextToken,
    getUpgradeHistory_domainName,
    getUpgradeHistoryResponse_nextToken,
    getUpgradeHistoryResponse_upgradeHistories,
    getUpgradeHistoryResponse_httpStatus,

    -- ** GetUpgradeStatus
    getUpgradeStatus_domainName,
    getUpgradeStatusResponse_stepStatus,
    getUpgradeStatusResponse_upgradeName,
    getUpgradeStatusResponse_upgradeStep,
    getUpgradeStatusResponse_httpStatus,

    -- ** ListDomainNames
    listDomainNames_engineType,
    listDomainNamesResponse_domainNames,
    listDomainNamesResponse_httpStatus,

    -- ** ListDomainsForPackage
    listDomainsForPackage_maxResults,
    listDomainsForPackage_nextToken,
    listDomainsForPackage_packageID,
    listDomainsForPackageResponse_domainPackageDetailsList,
    listDomainsForPackageResponse_nextToken,
    listDomainsForPackageResponse_httpStatus,

    -- ** ListElasticsearchInstanceTypes
    listElasticsearchInstanceTypes_domainName,
    listElasticsearchInstanceTypes_maxResults,
    listElasticsearchInstanceTypes_nextToken,
    listElasticsearchInstanceTypes_elasticsearchVersion,
    listElasticsearchInstanceTypesResponse_elasticsearchInstanceTypes,
    listElasticsearchInstanceTypesResponse_nextToken,
    listElasticsearchInstanceTypesResponse_httpStatus,

    -- ** ListElasticsearchVersions
    listElasticsearchVersions_maxResults,
    listElasticsearchVersions_nextToken,
    listElasticsearchVersionsResponse_elasticsearchVersions,
    listElasticsearchVersionsResponse_nextToken,
    listElasticsearchVersionsResponse_httpStatus,

    -- ** ListPackagesForDomain
    listPackagesForDomain_maxResults,
    listPackagesForDomain_nextToken,
    listPackagesForDomain_domainName,
    listPackagesForDomainResponse_domainPackageDetailsList,
    listPackagesForDomainResponse_nextToken,
    listPackagesForDomainResponse_httpStatus,

    -- ** ListTags
    listTags_arn,
    listTagsResponse_tagList,
    listTagsResponse_httpStatus,

    -- ** ListVpcEndpointAccess
    listVpcEndpointAccess_nextToken,
    listVpcEndpointAccess_domainName,
    listVpcEndpointAccessResponse_httpStatus,
    listVpcEndpointAccessResponse_authorizedPrincipalList,
    listVpcEndpointAccessResponse_nextToken,

    -- ** ListVpcEndpoints
    listVpcEndpoints_nextToken,
    listVpcEndpointsResponse_httpStatus,
    listVpcEndpointsResponse_vpcEndpointSummaryList,
    listVpcEndpointsResponse_nextToken,

    -- ** ListVpcEndpointsForDomain
    listVpcEndpointsForDomain_nextToken,
    listVpcEndpointsForDomain_domainName,
    listVpcEndpointsForDomainResponse_httpStatus,
    listVpcEndpointsForDomainResponse_vpcEndpointSummaryList,
    listVpcEndpointsForDomainResponse_nextToken,

    -- ** PurchaseReservedElasticsearchInstanceOffering
    purchaseReservedElasticsearchInstanceOffering_instanceCount,
    purchaseReservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId,
    purchaseReservedElasticsearchInstanceOffering_reservationName,
    purchaseReservedElasticsearchInstanceOfferingResponse_reservationName,
    purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId,
    purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus,

    -- ** RejectInboundCrossClusterSearchConnection
    rejectInboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    rejectInboundCrossClusterSearchConnectionResponse_crossClusterSearchConnection,
    rejectInboundCrossClusterSearchConnectionResponse_httpStatus,

    -- ** RemoveTags
    removeTags_arn,
    removeTags_tagKeys,

    -- ** RevokeVpcEndpointAccess
    revokeVpcEndpointAccess_domainName,
    revokeVpcEndpointAccess_account,
    revokeVpcEndpointAccessResponse_httpStatus,

    -- ** StartElasticsearchServiceSoftwareUpdate
    startElasticsearchServiceSoftwareUpdate_domainName,
    startElasticsearchServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    startElasticsearchServiceSoftwareUpdateResponse_httpStatus,

    -- ** UpdateElasticsearchDomainConfig
    updateElasticsearchDomainConfig_accessPolicies,
    updateElasticsearchDomainConfig_advancedOptions,
    updateElasticsearchDomainConfig_advancedSecurityOptions,
    updateElasticsearchDomainConfig_autoTuneOptions,
    updateElasticsearchDomainConfig_cognitoOptions,
    updateElasticsearchDomainConfig_domainEndpointOptions,
    updateElasticsearchDomainConfig_dryRun,
    updateElasticsearchDomainConfig_eBSOptions,
    updateElasticsearchDomainConfig_elasticsearchClusterConfig,
    updateElasticsearchDomainConfig_encryptionAtRestOptions,
    updateElasticsearchDomainConfig_logPublishingOptions,
    updateElasticsearchDomainConfig_nodeToNodeEncryptionOptions,
    updateElasticsearchDomainConfig_snapshotOptions,
    updateElasticsearchDomainConfig_vPCOptions,
    updateElasticsearchDomainConfig_domainName,
    updateElasticsearchDomainConfigResponse_dryRunResults,
    updateElasticsearchDomainConfigResponse_httpStatus,
    updateElasticsearchDomainConfigResponse_domainConfig,

    -- ** UpdatePackage
    updatePackage_commitMessage,
    updatePackage_packageDescription,
    updatePackage_packageID,
    updatePackage_packageSource,
    updatePackageResponse_packageDetails,
    updatePackageResponse_httpStatus,

    -- ** UpdateVpcEndpoint
    updateVpcEndpoint_vpcEndpointId,
    updateVpcEndpoint_vpcOptions,
    updateVpcEndpointResponse_httpStatus,
    updateVpcEndpointResponse_vpcEndpoint,

    -- ** UpgradeElasticsearchDomain
    upgradeElasticsearchDomain_performCheckOnly,
    upgradeElasticsearchDomain_domainName,
    upgradeElasticsearchDomain_targetVersion,
    upgradeElasticsearchDomainResponse_changeProgressDetails,
    upgradeElasticsearchDomainResponse_domainName,
    upgradeElasticsearchDomainResponse_performCheckOnly,
    upgradeElasticsearchDomainResponse_targetVersion,
    upgradeElasticsearchDomainResponse_httpStatus,

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
    advancedSecurityOptions_anonymousAuthDisableDate,
    advancedSecurityOptions_anonymousAuthEnabled,
    advancedSecurityOptions_enabled,
    advancedSecurityOptions_internalUserDatabaseEnabled,
    advancedSecurityOptions_sAMLOptions,

    -- ** AdvancedSecurityOptionsInput
    advancedSecurityOptionsInput_anonymousAuthEnabled,
    advancedSecurityOptionsInput_enabled,
    advancedSecurityOptionsInput_internalUserDatabaseEnabled,
    advancedSecurityOptionsInput_masterUserOptions,
    advancedSecurityOptionsInput_sAMLOptions,

    -- ** AdvancedSecurityOptionsStatus
    advancedSecurityOptionsStatus_options,
    advancedSecurityOptionsStatus_status,

    -- ** AuthorizedPrincipal
    authorizedPrincipal_principal,
    authorizedPrincipal_principalType,

    -- ** AutoTune
    autoTune_autoTuneDetails,
    autoTune_autoTuneType,

    -- ** AutoTuneDetails
    autoTuneDetails_scheduledAutoTuneDetails,

    -- ** AutoTuneMaintenanceSchedule
    autoTuneMaintenanceSchedule_cronExpressionForRecurrence,
    autoTuneMaintenanceSchedule_duration,
    autoTuneMaintenanceSchedule_startAt,

    -- ** AutoTuneOptions
    autoTuneOptions_desiredState,
    autoTuneOptions_maintenanceSchedules,
    autoTuneOptions_rollbackOnDisable,

    -- ** AutoTuneOptionsInput
    autoTuneOptionsInput_desiredState,
    autoTuneOptionsInput_maintenanceSchedules,

    -- ** AutoTuneOptionsOutput
    autoTuneOptionsOutput_errorMessage,
    autoTuneOptionsOutput_state,

    -- ** AutoTuneOptionsStatus
    autoTuneOptionsStatus_options,
    autoTuneOptionsStatus_status,

    -- ** AutoTuneStatus
    autoTuneStatus_errorMessage,
    autoTuneStatus_pendingDeletion,
    autoTuneStatus_updateVersion,
    autoTuneStatus_creationDate,
    autoTuneStatus_updateDate,
    autoTuneStatus_state,

    -- ** ChangeProgressDetails
    changeProgressDetails_changeId,
    changeProgressDetails_message,

    -- ** ChangeProgressStage
    changeProgressStage_description,
    changeProgressStage_lastUpdated,
    changeProgressStage_name,
    changeProgressStage_status,

    -- ** ChangeProgressStatusDetails
    changeProgressStatusDetails_changeId,
    changeProgressStatusDetails_changeProgressStages,
    changeProgressStatusDetails_completedProperties,
    changeProgressStatusDetails_pendingProperties,
    changeProgressStatusDetails_startTime,
    changeProgressStatusDetails_status,
    changeProgressStatusDetails_totalNumberOfStages,

    -- ** CognitoOptions
    cognitoOptions_enabled,
    cognitoOptions_identityPoolId,
    cognitoOptions_roleArn,
    cognitoOptions_userPoolId,

    -- ** CognitoOptionsStatus
    cognitoOptionsStatus_options,
    cognitoOptionsStatus_status,

    -- ** ColdStorageOptions
    coldStorageOptions_enabled,

    -- ** CompatibleVersionsMap
    compatibleVersionsMap_sourceVersion,
    compatibleVersionsMap_targetVersions,

    -- ** DescribePackagesFilter
    describePackagesFilter_name,
    describePackagesFilter_value,

    -- ** DomainEndpointOptions
    domainEndpointOptions_customEndpoint,
    domainEndpointOptions_customEndpointCertificateArn,
    domainEndpointOptions_customEndpointEnabled,
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_tLSSecurityPolicy,

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
    domainPackageDetails_domainName,
    domainPackageDetails_domainPackageStatus,
    domainPackageDetails_errorDetails,
    domainPackageDetails_lastUpdated,
    domainPackageDetails_packageID,
    domainPackageDetails_packageName,
    domainPackageDetails_packageType,
    domainPackageDetails_packageVersion,
    domainPackageDetails_referencePath,

    -- ** DryRunResults
    dryRunResults_deploymentType,
    dryRunResults_message,

    -- ** Duration
    duration_unit,
    duration_value,

    -- ** EBSOptions
    eBSOptions_eBSEnabled,
    eBSOptions_iops,
    eBSOptions_throughput,
    eBSOptions_volumeSize,
    eBSOptions_volumeType,

    -- ** EBSOptionsStatus
    eBSOptionsStatus_options,
    eBSOptionsStatus_status,

    -- ** ElasticsearchClusterConfig
    elasticsearchClusterConfig_coldStorageOptions,
    elasticsearchClusterConfig_dedicatedMasterCount,
    elasticsearchClusterConfig_dedicatedMasterEnabled,
    elasticsearchClusterConfig_dedicatedMasterType,
    elasticsearchClusterConfig_instanceCount,
    elasticsearchClusterConfig_instanceType,
    elasticsearchClusterConfig_warmCount,
    elasticsearchClusterConfig_warmEnabled,
    elasticsearchClusterConfig_warmType,
    elasticsearchClusterConfig_zoneAwarenessConfig,
    elasticsearchClusterConfig_zoneAwarenessEnabled,

    -- ** ElasticsearchClusterConfigStatus
    elasticsearchClusterConfigStatus_options,
    elasticsearchClusterConfigStatus_status,

    -- ** ElasticsearchDomainConfig
    elasticsearchDomainConfig_accessPolicies,
    elasticsearchDomainConfig_advancedOptions,
    elasticsearchDomainConfig_advancedSecurityOptions,
    elasticsearchDomainConfig_autoTuneOptions,
    elasticsearchDomainConfig_changeProgressDetails,
    elasticsearchDomainConfig_cognitoOptions,
    elasticsearchDomainConfig_domainEndpointOptions,
    elasticsearchDomainConfig_eBSOptions,
    elasticsearchDomainConfig_elasticsearchClusterConfig,
    elasticsearchDomainConfig_elasticsearchVersion,
    elasticsearchDomainConfig_encryptionAtRestOptions,
    elasticsearchDomainConfig_logPublishingOptions,
    elasticsearchDomainConfig_nodeToNodeEncryptionOptions,
    elasticsearchDomainConfig_snapshotOptions,
    elasticsearchDomainConfig_vPCOptions,

    -- ** ElasticsearchDomainStatus
    elasticsearchDomainStatus_accessPolicies,
    elasticsearchDomainStatus_advancedOptions,
    elasticsearchDomainStatus_advancedSecurityOptions,
    elasticsearchDomainStatus_autoTuneOptions,
    elasticsearchDomainStatus_changeProgressDetails,
    elasticsearchDomainStatus_cognitoOptions,
    elasticsearchDomainStatus_created,
    elasticsearchDomainStatus_deleted,
    elasticsearchDomainStatus_domainEndpointOptions,
    elasticsearchDomainStatus_eBSOptions,
    elasticsearchDomainStatus_elasticsearchVersion,
    elasticsearchDomainStatus_encryptionAtRestOptions,
    elasticsearchDomainStatus_endpoint,
    elasticsearchDomainStatus_endpoints,
    elasticsearchDomainStatus_logPublishingOptions,
    elasticsearchDomainStatus_nodeToNodeEncryptionOptions,
    elasticsearchDomainStatus_processing,
    elasticsearchDomainStatus_serviceSoftwareOptions,
    elasticsearchDomainStatus_snapshotOptions,
    elasticsearchDomainStatus_upgradeProcessing,
    elasticsearchDomainStatus_vPCOptions,
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
    errorDetails_errorMessage,
    errorDetails_errorType,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** InboundCrossClusterSearchConnection
    inboundCrossClusterSearchConnection_connectionStatus,
    inboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    inboundCrossClusterSearchConnection_destinationDomainInfo,
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
    limits_additionalLimits,
    limits_instanceLimits,
    limits_storageTypes,

    -- ** LogPublishingOption
    logPublishingOption_cloudWatchLogsLogGroupArn,
    logPublishingOption_enabled,

    -- ** LogPublishingOptionsStatus
    logPublishingOptionsStatus_options,
    logPublishingOptionsStatus_status,

    -- ** MasterUserOptions
    masterUserOptions_masterUserARN,
    masterUserOptions_masterUserName,
    masterUserOptions_masterUserPassword,

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
    outboundCrossClusterSearchConnection_connectionAlias,
    outboundCrossClusterSearchConnection_connectionStatus,
    outboundCrossClusterSearchConnection_crossClusterSearchConnectionId,
    outboundCrossClusterSearchConnection_destinationDomainInfo,
    outboundCrossClusterSearchConnection_sourceDomainInfo,

    -- ** OutboundCrossClusterSearchConnectionStatus
    outboundCrossClusterSearchConnectionStatus_message,
    outboundCrossClusterSearchConnectionStatus_statusCode,

    -- ** PackageDetails
    packageDetails_availablePackageVersion,
    packageDetails_createdAt,
    packageDetails_errorDetails,
    packageDetails_lastUpdatedAt,
    packageDetails_packageDescription,
    packageDetails_packageID,
    packageDetails_packageName,
    packageDetails_packageStatus,
    packageDetails_packageType,

    -- ** PackageSource
    packageSource_s3BucketName,
    packageSource_s3Key,

    -- ** PackageVersionHistory
    packageVersionHistory_commitMessage,
    packageVersionHistory_createdAt,
    packageVersionHistory_packageVersion,

    -- ** RecurringCharge
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- ** ReservedElasticsearchInstance
    reservedElasticsearchInstance_currencyCode,
    reservedElasticsearchInstance_duration,
    reservedElasticsearchInstance_elasticsearchInstanceCount,
    reservedElasticsearchInstance_elasticsearchInstanceType,
    reservedElasticsearchInstance_fixedPrice,
    reservedElasticsearchInstance_paymentOption,
    reservedElasticsearchInstance_recurringCharges,
    reservedElasticsearchInstance_reservationName,
    reservedElasticsearchInstance_reservedElasticsearchInstanceId,
    reservedElasticsearchInstance_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstance_startTime,
    reservedElasticsearchInstance_state,
    reservedElasticsearchInstance_usagePrice,

    -- ** ReservedElasticsearchInstanceOffering
    reservedElasticsearchInstanceOffering_currencyCode,
    reservedElasticsearchInstanceOffering_duration,
    reservedElasticsearchInstanceOffering_elasticsearchInstanceType,
    reservedElasticsearchInstanceOffering_fixedPrice,
    reservedElasticsearchInstanceOffering_paymentOption,
    reservedElasticsearchInstanceOffering_recurringCharges,
    reservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId,
    reservedElasticsearchInstanceOffering_usagePrice,

    -- ** SAMLIdp
    sAMLIdp_metadataContent,
    sAMLIdp_entityId,

    -- ** SAMLOptionsInput
    sAMLOptionsInput_enabled,
    sAMLOptionsInput_idp,
    sAMLOptionsInput_masterBackendRole,
    sAMLOptionsInput_masterUserName,
    sAMLOptionsInput_rolesKey,
    sAMLOptionsInput_sessionTimeoutMinutes,
    sAMLOptionsInput_subjectKey,

    -- ** SAMLOptionsOutput
    sAMLOptionsOutput_enabled,
    sAMLOptionsOutput_idp,
    sAMLOptionsOutput_rolesKey,
    sAMLOptionsOutput_sessionTimeoutMinutes,
    sAMLOptionsOutput_subjectKey,

    -- ** ScheduledAutoTuneDetails
    scheduledAutoTuneDetails_action,
    scheduledAutoTuneDetails_actionType,
    scheduledAutoTuneDetails_date,
    scheduledAutoTuneDetails_severity,

    -- ** ServiceSoftwareOptions
    serviceSoftwareOptions_automatedUpdateDate,
    serviceSoftwareOptions_cancellable,
    serviceSoftwareOptions_currentVersion,
    serviceSoftwareOptions_description,
    serviceSoftwareOptions_newVersion,
    serviceSoftwareOptions_optionalDeployment,
    serviceSoftwareOptions_updateAvailable,
    serviceSoftwareOptions_updateStatus,

    -- ** SnapshotOptions
    snapshotOptions_automatedSnapshotStartHour,

    -- ** SnapshotOptionsStatus
    snapshotOptionsStatus_options,
    snapshotOptionsStatus_status,

    -- ** StorageType
    storageType_storageSubTypeName,
    storageType_storageTypeLimits,
    storageType_storageTypeName,

    -- ** StorageTypeLimit
    storageTypeLimit_limitName,
    storageTypeLimit_limitValues,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UpgradeHistory
    upgradeHistory_startTimestamp,
    upgradeHistory_stepsList,
    upgradeHistory_upgradeName,
    upgradeHistory_upgradeStatus,

    -- ** UpgradeStepItem
    upgradeStepItem_issues,
    upgradeStepItem_progressPercent,
    upgradeStepItem_upgradeStep,
    upgradeStepItem_upgradeStepStatus,

    -- ** VPCDerivedInfo
    vPCDerivedInfo_availabilityZones,
    vPCDerivedInfo_securityGroupIds,
    vPCDerivedInfo_subnetIds,
    vPCDerivedInfo_vPCId,

    -- ** VPCDerivedInfoStatus
    vPCDerivedInfoStatus_options,
    vPCDerivedInfoStatus_status,

    -- ** VPCOptions
    vPCOptions_securityGroupIds,
    vPCOptions_subnetIds,

    -- ** VpcEndpoint
    vpcEndpoint_domainArn,
    vpcEndpoint_endpoint,
    vpcEndpoint_status,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcEndpointOwner,
    vpcEndpoint_vpcOptions,

    -- ** VpcEndpointError
    vpcEndpointError_errorCode,
    vpcEndpointError_errorMessage,
    vpcEndpointError_vpcEndpointId,

    -- ** VpcEndpointSummary
    vpcEndpointSummary_domainArn,
    vpcEndpointSummary_status,
    vpcEndpointSummary_vpcEndpointId,
    vpcEndpointSummary_vpcEndpointOwner,

    -- ** ZoneAwarenessConfig
    zoneAwarenessConfig_availabilityZoneCount,
  )
where

import Amazonka.ElasticSearch.AcceptInboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.AddTags
import Amazonka.ElasticSearch.AssociatePackage
import Amazonka.ElasticSearch.AuthorizeVpcEndpointAccess
import Amazonka.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
import Amazonka.ElasticSearch.CreateElasticsearchDomain
import Amazonka.ElasticSearch.CreateOutboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.CreatePackage
import Amazonka.ElasticSearch.CreateVpcEndpoint
import Amazonka.ElasticSearch.DeleteElasticsearchDomain
import Amazonka.ElasticSearch.DeleteElasticsearchServiceRole
import Amazonka.ElasticSearch.DeleteInboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.DeleteOutboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.DeletePackage
import Amazonka.ElasticSearch.DeleteVpcEndpoint
import Amazonka.ElasticSearch.DescribeDomainAutoTunes
import Amazonka.ElasticSearch.DescribeDomainChangeProgress
import Amazonka.ElasticSearch.DescribeElasticsearchDomain
import Amazonka.ElasticSearch.DescribeElasticsearchDomainConfig
import Amazonka.ElasticSearch.DescribeElasticsearchDomains
import Amazonka.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
import Amazonka.ElasticSearch.DescribeInboundCrossClusterSearchConnections
import Amazonka.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
import Amazonka.ElasticSearch.DescribePackages
import Amazonka.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
import Amazonka.ElasticSearch.DescribeReservedElasticsearchInstances
import Amazonka.ElasticSearch.DescribeVpcEndpoints
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
import Amazonka.ElasticSearch.ListVpcEndpointAccess
import Amazonka.ElasticSearch.ListVpcEndpoints
import Amazonka.ElasticSearch.ListVpcEndpointsForDomain
import Amazonka.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
import Amazonka.ElasticSearch.RejectInboundCrossClusterSearchConnection
import Amazonka.ElasticSearch.RemoveTags
import Amazonka.ElasticSearch.RevokeVpcEndpointAccess
import Amazonka.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
import Amazonka.ElasticSearch.Types.AccessPoliciesStatus
import Amazonka.ElasticSearch.Types.AdditionalLimit
import Amazonka.ElasticSearch.Types.AdvancedOptionsStatus
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptions
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsInput
import Amazonka.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Amazonka.ElasticSearch.Types.AuthorizedPrincipal
import Amazonka.ElasticSearch.Types.AutoTune
import Amazonka.ElasticSearch.Types.AutoTuneDetails
import Amazonka.ElasticSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.ElasticSearch.Types.AutoTuneOptions
import Amazonka.ElasticSearch.Types.AutoTuneOptionsInput
import Amazonka.ElasticSearch.Types.AutoTuneOptionsOutput
import Amazonka.ElasticSearch.Types.AutoTuneOptionsStatus
import Amazonka.ElasticSearch.Types.AutoTuneStatus
import Amazonka.ElasticSearch.Types.ChangeProgressDetails
import Amazonka.ElasticSearch.Types.ChangeProgressStage
import Amazonka.ElasticSearch.Types.ChangeProgressStatusDetails
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
import Amazonka.ElasticSearch.Types.DryRunResults
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
import Amazonka.ElasticSearch.Types.VpcEndpoint
import Amazonka.ElasticSearch.Types.VpcEndpointError
import Amazonka.ElasticSearch.Types.VpcEndpointSummary
import Amazonka.ElasticSearch.Types.ZoneAwarenessConfig
import Amazonka.ElasticSearch.UpdateElasticsearchDomainConfig
import Amazonka.ElasticSearch.UpdatePackage
import Amazonka.ElasticSearch.UpdateVpcEndpoint
import Amazonka.ElasticSearch.UpgradeElasticsearchDomain
