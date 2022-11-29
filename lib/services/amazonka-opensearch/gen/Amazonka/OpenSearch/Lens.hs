{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearch.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Lens
  ( -- * Operations

    -- ** AcceptInboundConnection
    acceptInboundConnection_connectionId,
    acceptInboundConnectionResponse_connection,
    acceptInboundConnectionResponse_httpStatus,

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

    -- ** CancelServiceSoftwareUpdate
    cancelServiceSoftwareUpdate_domainName,
    cancelServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    cancelServiceSoftwareUpdateResponse_httpStatus,

    -- ** CreateDomain
    createDomain_nodeToNodeEncryptionOptions,
    createDomain_clusterConfig,
    createDomain_advancedOptions,
    createDomain_tagList,
    createDomain_advancedSecurityOptions,
    createDomain_cognitoOptions,
    createDomain_encryptionAtRestOptions,
    createDomain_eBSOptions,
    createDomain_accessPolicies,
    createDomain_vPCOptions,
    createDomain_autoTuneOptions,
    createDomain_domainEndpointOptions,
    createDomain_snapshotOptions,
    createDomain_logPublishingOptions,
    createDomain_engineVersion,
    createDomain_domainName,
    createDomainResponse_domainStatus,
    createDomainResponse_httpStatus,

    -- ** CreateOutboundConnection
    createOutboundConnection_localDomainInfo,
    createOutboundConnection_remoteDomainInfo,
    createOutboundConnection_connectionAlias,
    createOutboundConnectionResponse_connectionAlias,
    createOutboundConnectionResponse_remoteDomainInfo,
    createOutboundConnectionResponse_connectionId,
    createOutboundConnectionResponse_localDomainInfo,
    createOutboundConnectionResponse_connectionStatus,
    createOutboundConnectionResponse_httpStatus,

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

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_domainStatus,
    deleteDomainResponse_httpStatus,

    -- ** DeleteInboundConnection
    deleteInboundConnection_connectionId,
    deleteInboundConnectionResponse_connection,
    deleteInboundConnectionResponse_httpStatus,

    -- ** DeleteOutboundConnection
    deleteOutboundConnection_connectionId,
    deleteOutboundConnectionResponse_connection,
    deleteOutboundConnectionResponse_httpStatus,

    -- ** DeletePackage
    deletePackage_packageID,
    deletePackageResponse_packageDetails,
    deletePackageResponse_httpStatus,

    -- ** DeleteVpcEndpoint
    deleteVpcEndpoint_vpcEndpointId,
    deleteVpcEndpointResponse_httpStatus,
    deleteVpcEndpointResponse_vpcEndpointSummary,

    -- ** DescribeDomain
    describeDomain_domainName,
    describeDomainResponse_httpStatus,
    describeDomainResponse_domainStatus,

    -- ** DescribeDomainAutoTunes
    describeDomainAutoTunes_nextToken,
    describeDomainAutoTunes_maxResults,
    describeDomainAutoTunes_domainName,
    describeDomainAutoTunesResponse_nextToken,
    describeDomainAutoTunesResponse_autoTunes,
    describeDomainAutoTunesResponse_httpStatus,

    -- ** DescribeDomainChangeProgress
    describeDomainChangeProgress_changeId,
    describeDomainChangeProgress_domainName,
    describeDomainChangeProgressResponse_changeProgressStatus,
    describeDomainChangeProgressResponse_httpStatus,

    -- ** DescribeDomainConfig
    describeDomainConfig_domainName,
    describeDomainConfigResponse_httpStatus,
    describeDomainConfigResponse_domainConfig,

    -- ** DescribeDomains
    describeDomains_domainNames,
    describeDomainsResponse_httpStatus,
    describeDomainsResponse_domainStatusList,

    -- ** DescribeInboundConnections
    describeInboundConnections_nextToken,
    describeInboundConnections_filters,
    describeInboundConnections_maxResults,
    describeInboundConnectionsResponse_nextToken,
    describeInboundConnectionsResponse_connections,
    describeInboundConnectionsResponse_httpStatus,

    -- ** DescribeInstanceTypeLimits
    describeInstanceTypeLimits_domainName,
    describeInstanceTypeLimits_instanceType,
    describeInstanceTypeLimits_engineVersion,
    describeInstanceTypeLimitsResponse_limitsByRole,
    describeInstanceTypeLimitsResponse_httpStatus,

    -- ** DescribeOutboundConnections
    describeOutboundConnections_nextToken,
    describeOutboundConnections_filters,
    describeOutboundConnections_maxResults,
    describeOutboundConnectionsResponse_nextToken,
    describeOutboundConnectionsResponse_connections,
    describeOutboundConnectionsResponse_httpStatus,

    -- ** DescribePackages
    describePackages_nextToken,
    describePackages_filters,
    describePackages_maxResults,
    describePackagesResponse_nextToken,
    describePackagesResponse_packageDetailsList,
    describePackagesResponse_httpStatus,

    -- ** DescribeReservedInstanceOfferings
    describeReservedInstanceOfferings_nextToken,
    describeReservedInstanceOfferings_maxResults,
    describeReservedInstanceOfferings_reservedInstanceOfferingId,
    describeReservedInstanceOfferingsResponse_nextToken,
    describeReservedInstanceOfferingsResponse_reservedInstanceOfferings,
    describeReservedInstanceOfferingsResponse_httpStatus,

    -- ** DescribeReservedInstances
    describeReservedInstances_nextToken,
    describeReservedInstances_reservedInstanceId,
    describeReservedInstances_maxResults,
    describeReservedInstancesResponse_nextToken,
    describeReservedInstancesResponse_reservedInstances,
    describeReservedInstancesResponse_httpStatus,

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

    -- ** GetCompatibleVersions
    getCompatibleVersions_domainName,
    getCompatibleVersionsResponse_compatibleVersions,
    getCompatibleVersionsResponse_httpStatus,

    -- ** GetPackageVersionHistory
    getPackageVersionHistory_nextToken,
    getPackageVersionHistory_maxResults,
    getPackageVersionHistory_packageID,
    getPackageVersionHistoryResponse_nextToken,
    getPackageVersionHistoryResponse_packageID,
    getPackageVersionHistoryResponse_packageVersionHistoryList,
    getPackageVersionHistoryResponse_httpStatus,

    -- ** GetUpgradeHistory
    getUpgradeHistory_nextToken,
    getUpgradeHistory_maxResults,
    getUpgradeHistory_domainName,
    getUpgradeHistoryResponse_nextToken,
    getUpgradeHistoryResponse_upgradeHistories,
    getUpgradeHistoryResponse_httpStatus,

    -- ** GetUpgradeStatus
    getUpgradeStatus_domainName,
    getUpgradeStatusResponse_upgradeStep,
    getUpgradeStatusResponse_upgradeName,
    getUpgradeStatusResponse_stepStatus,
    getUpgradeStatusResponse_httpStatus,

    -- ** ListDomainNames
    listDomainNames_engineType,
    listDomainNamesResponse_domainNames,
    listDomainNamesResponse_httpStatus,

    -- ** ListDomainsForPackage
    listDomainsForPackage_nextToken,
    listDomainsForPackage_maxResults,
    listDomainsForPackage_packageID,
    listDomainsForPackageResponse_nextToken,
    listDomainsForPackageResponse_domainPackageDetailsList,
    listDomainsForPackageResponse_httpStatus,

    -- ** ListInstanceTypeDetails
    listInstanceTypeDetails_nextToken,
    listInstanceTypeDetails_domainName,
    listInstanceTypeDetails_maxResults,
    listInstanceTypeDetails_engineVersion,
    listInstanceTypeDetailsResponse_nextToken,
    listInstanceTypeDetailsResponse_instanceTypeDetails,
    listInstanceTypeDetailsResponse_httpStatus,

    -- ** ListPackagesForDomain
    listPackagesForDomain_nextToken,
    listPackagesForDomain_maxResults,
    listPackagesForDomain_domainName,
    listPackagesForDomainResponse_nextToken,
    listPackagesForDomainResponse_domainPackageDetailsList,
    listPackagesForDomainResponse_httpStatus,

    -- ** ListTags
    listTags_arn,
    listTagsResponse_tagList,
    listTagsResponse_httpStatus,

    -- ** ListVersions
    listVersions_nextToken,
    listVersions_maxResults,
    listVersionsResponse_nextToken,
    listVersionsResponse_versions,
    listVersionsResponse_httpStatus,

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

    -- ** PurchaseReservedInstanceOffering
    purchaseReservedInstanceOffering_instanceCount,
    purchaseReservedInstanceOffering_reservedInstanceOfferingId,
    purchaseReservedInstanceOffering_reservationName,
    purchaseReservedInstanceOfferingResponse_reservedInstanceId,
    purchaseReservedInstanceOfferingResponse_reservationName,
    purchaseReservedInstanceOfferingResponse_httpStatus,

    -- ** RejectInboundConnection
    rejectInboundConnection_connectionId,
    rejectInboundConnectionResponse_connection,
    rejectInboundConnectionResponse_httpStatus,

    -- ** RemoveTags
    removeTags_arn,
    removeTags_tagKeys,

    -- ** RevokeVpcEndpointAccess
    revokeVpcEndpointAccess_domainName,
    revokeVpcEndpointAccess_account,
    revokeVpcEndpointAccessResponse_httpStatus,

    -- ** StartServiceSoftwareUpdate
    startServiceSoftwareUpdate_domainName,
    startServiceSoftwareUpdateResponse_serviceSoftwareOptions,
    startServiceSoftwareUpdateResponse_httpStatus,

    -- ** UpdateDomainConfig
    updateDomainConfig_nodeToNodeEncryptionOptions,
    updateDomainConfig_clusterConfig,
    updateDomainConfig_advancedOptions,
    updateDomainConfig_advancedSecurityOptions,
    updateDomainConfig_cognitoOptions,
    updateDomainConfig_encryptionAtRestOptions,
    updateDomainConfig_dryRun,
    updateDomainConfig_eBSOptions,
    updateDomainConfig_accessPolicies,
    updateDomainConfig_vPCOptions,
    updateDomainConfig_autoTuneOptions,
    updateDomainConfig_domainEndpointOptions,
    updateDomainConfig_snapshotOptions,
    updateDomainConfig_logPublishingOptions,
    updateDomainConfig_domainName,
    updateDomainConfigResponse_dryRunResults,
    updateDomainConfigResponse_httpStatus,
    updateDomainConfigResponse_domainConfig,

    -- ** UpdatePackage
    updatePackage_packageDescription,
    updatePackage_commitMessage,
    updatePackage_packageID,
    updatePackage_packageSource,
    updatePackageResponse_packageDetails,
    updatePackageResponse_httpStatus,

    -- ** UpdateVpcEndpoint
    updateVpcEndpoint_vpcEndpointId,
    updateVpcEndpoint_vpcOptions,
    updateVpcEndpointResponse_httpStatus,
    updateVpcEndpointResponse_vpcEndpoint,

    -- ** UpgradeDomain
    upgradeDomain_advancedOptions,
    upgradeDomain_performCheckOnly,
    upgradeDomain_domainName,
    upgradeDomain_targetVersion,
    upgradeDomainResponse_advancedOptions,
    upgradeDomainResponse_changeProgressDetails,
    upgradeDomainResponse_domainName,
    upgradeDomainResponse_targetVersion,
    upgradeDomainResponse_performCheckOnly,
    upgradeDomainResponse_upgradeId,
    upgradeDomainResponse_httpStatus,

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
    advancedSecurityOptions_internalUserDatabaseEnabled,
    advancedSecurityOptions_sAMLOptions,
    advancedSecurityOptions_anonymousAuthEnabled,
    advancedSecurityOptions_enabled,
    advancedSecurityOptions_anonymousAuthDisableDate,

    -- ** AdvancedSecurityOptionsInput
    advancedSecurityOptionsInput_internalUserDatabaseEnabled,
    advancedSecurityOptionsInput_sAMLOptions,
    advancedSecurityOptionsInput_anonymousAuthEnabled,
    advancedSecurityOptionsInput_enabled,
    advancedSecurityOptionsInput_masterUserOptions,

    -- ** AdvancedSecurityOptionsStatus
    advancedSecurityOptionsStatus_options,
    advancedSecurityOptionsStatus_status,

    -- ** AuthorizedPrincipal
    authorizedPrincipal_principal,
    authorizedPrincipal_principalType,

    -- ** AutoTune
    autoTune_autoTuneType,
    autoTune_autoTuneDetails,

    -- ** AutoTuneDetails
    autoTuneDetails_scheduledAutoTuneDetails,

    -- ** AutoTuneMaintenanceSchedule
    autoTuneMaintenanceSchedule_duration,
    autoTuneMaintenanceSchedule_cronExpressionForRecurrence,
    autoTuneMaintenanceSchedule_startAt,

    -- ** AutoTuneOptions
    autoTuneOptions_maintenanceSchedules,
    autoTuneOptions_desiredState,
    autoTuneOptions_rollbackOnDisable,

    -- ** AutoTuneOptionsInput
    autoTuneOptionsInput_maintenanceSchedules,
    autoTuneOptionsInput_desiredState,

    -- ** AutoTuneOptionsOutput
    autoTuneOptionsOutput_errorMessage,
    autoTuneOptionsOutput_state,

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

    -- ** ChangeProgressDetails
    changeProgressDetails_message,
    changeProgressDetails_changeId,

    -- ** ChangeProgressStage
    changeProgressStage_name,
    changeProgressStage_status,
    changeProgressStage_description,
    changeProgressStage_lastUpdated,

    -- ** ChangeProgressStatusDetails
    changeProgressStatusDetails_totalNumberOfStages,
    changeProgressStatusDetails_changeId,
    changeProgressStatusDetails_pendingProperties,
    changeProgressStatusDetails_status,
    changeProgressStatusDetails_changeProgressStages,
    changeProgressStatusDetails_completedProperties,
    changeProgressStatusDetails_startTime,

    -- ** ClusterConfig
    clusterConfig_warmCount,
    clusterConfig_coldStorageOptions,
    clusterConfig_dedicatedMasterType,
    clusterConfig_zoneAwarenessEnabled,
    clusterConfig_dedicatedMasterEnabled,
    clusterConfig_warmType,
    clusterConfig_instanceType,
    clusterConfig_zoneAwarenessConfig,
    clusterConfig_instanceCount,
    clusterConfig_warmEnabled,
    clusterConfig_dedicatedMasterCount,

    -- ** ClusterConfigStatus
    clusterConfigStatus_options,
    clusterConfigStatus_status,

    -- ** CognitoOptions
    cognitoOptions_roleArn,
    cognitoOptions_enabled,
    cognitoOptions_identityPoolId,
    cognitoOptions_userPoolId,

    -- ** CognitoOptionsStatus
    cognitoOptionsStatus_options,
    cognitoOptionsStatus_status,

    -- ** ColdStorageOptions
    coldStorageOptions_enabled,

    -- ** CompatibleVersionsMap
    compatibleVersionsMap_targetVersions,
    compatibleVersionsMap_sourceVersion,

    -- ** DescribePackagesFilter
    describePackagesFilter_name,
    describePackagesFilter_value,

    -- ** DomainConfig
    domainConfig_nodeToNodeEncryptionOptions,
    domainConfig_clusterConfig,
    domainConfig_advancedOptions,
    domainConfig_changeProgressDetails,
    domainConfig_advancedSecurityOptions,
    domainConfig_cognitoOptions,
    domainConfig_encryptionAtRestOptions,
    domainConfig_eBSOptions,
    domainConfig_accessPolicies,
    domainConfig_vPCOptions,
    domainConfig_autoTuneOptions,
    domainConfig_domainEndpointOptions,
    domainConfig_snapshotOptions,
    domainConfig_logPublishingOptions,
    domainConfig_engineVersion,

    -- ** DomainEndpointOptions
    domainEndpointOptions_customEndpointCertificateArn,
    domainEndpointOptions_tLSSecurityPolicy,
    domainEndpointOptions_customEndpointEnabled,
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_customEndpoint,

    -- ** DomainEndpointOptionsStatus
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- ** DomainInfo
    domainInfo_engineType,
    domainInfo_domainName,

    -- ** DomainInformationContainer
    domainInformationContainer_aWSDomainInformation,

    -- ** DomainPackageDetails
    domainPackageDetails_referencePath,
    domainPackageDetails_packageName,
    domainPackageDetails_domainName,
    domainPackageDetails_errorDetails,
    domainPackageDetails_domainPackageStatus,
    domainPackageDetails_packageID,
    domainPackageDetails_lastUpdated,
    domainPackageDetails_packageVersion,
    domainPackageDetails_packageType,

    -- ** DomainStatus
    domainStatus_nodeToNodeEncryptionOptions,
    domainStatus_advancedOptions,
    domainStatus_changeProgressDetails,
    domainStatus_deleted,
    domainStatus_created,
    domainStatus_advancedSecurityOptions,
    domainStatus_upgradeProcessing,
    domainStatus_processing,
    domainStatus_cognitoOptions,
    domainStatus_encryptionAtRestOptions,
    domainStatus_endpoints,
    domainStatus_eBSOptions,
    domainStatus_accessPolicies,
    domainStatus_vPCOptions,
    domainStatus_autoTuneOptions,
    domainStatus_domainEndpointOptions,
    domainStatus_endpoint,
    domainStatus_serviceSoftwareOptions,
    domainStatus_snapshotOptions,
    domainStatus_logPublishingOptions,
    domainStatus_engineVersion,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_arn,
    domainStatus_clusterConfig,

    -- ** DryRunResults
    dryRunResults_message,
    dryRunResults_deploymentType,

    -- ** Duration
    duration_unit,
    duration_value,

    -- ** EBSOptions
    eBSOptions_volumeType,
    eBSOptions_volumeSize,
    eBSOptions_throughput,
    eBSOptions_eBSEnabled,
    eBSOptions_iops,

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
    errorDetails_errorMessage,
    errorDetails_errorType,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** InboundConnection
    inboundConnection_remoteDomainInfo,
    inboundConnection_connectionId,
    inboundConnection_localDomainInfo,
    inboundConnection_connectionStatus,

    -- ** InboundConnectionStatus
    inboundConnectionStatus_message,
    inboundConnectionStatus_statusCode,

    -- ** InstanceCountLimits
    instanceCountLimits_minimumInstanceCount,
    instanceCountLimits_maximumInstanceCount,

    -- ** InstanceLimits
    instanceLimits_instanceCountLimits,

    -- ** InstanceTypeDetails
    instanceTypeDetails_advancedSecurityEnabled,
    instanceTypeDetails_encryptionEnabled,
    instanceTypeDetails_instanceType,
    instanceTypeDetails_cognitoEnabled,
    instanceTypeDetails_instanceRole,
    instanceTypeDetails_appLogsEnabled,
    instanceTypeDetails_warmEnabled,

    -- ** Limits
    limits_instanceLimits,
    limits_storageTypes,
    limits_additionalLimits,

    -- ** LogPublishingOption
    logPublishingOption_enabled,
    logPublishingOption_cloudWatchLogsLogGroupArn,

    -- ** LogPublishingOptionsStatus
    logPublishingOptionsStatus_status,
    logPublishingOptionsStatus_options,

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

    -- ** OutboundConnection
    outboundConnection_connectionAlias,
    outboundConnection_remoteDomainInfo,
    outboundConnection_connectionId,
    outboundConnection_localDomainInfo,
    outboundConnection_connectionStatus,

    -- ** OutboundConnectionStatus
    outboundConnectionStatus_message,
    outboundConnectionStatus_statusCode,

    -- ** PackageDetails
    packageDetails_packageDescription,
    packageDetails_packageName,
    packageDetails_lastUpdatedAt,
    packageDetails_errorDetails,
    packageDetails_packageID,
    packageDetails_packageType,
    packageDetails_availablePackageVersion,
    packageDetails_packageStatus,
    packageDetails_createdAt,

    -- ** PackageSource
    packageSource_s3BucketName,
    packageSource_s3Key,

    -- ** PackageVersionHistory
    packageVersionHistory_packageVersion,
    packageVersionHistory_commitMessage,
    packageVersionHistory_createdAt,

    -- ** RecurringCharge
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- ** ReservedInstance
    reservedInstance_billingSubscriptionId,
    reservedInstance_reservedInstanceId,
    reservedInstance_recurringCharges,
    reservedInstance_state,
    reservedInstance_reservationName,
    reservedInstance_instanceType,
    reservedInstance_duration,
    reservedInstance_currencyCode,
    reservedInstance_instanceCount,
    reservedInstance_fixedPrice,
    reservedInstance_reservedInstanceOfferingId,
    reservedInstance_startTime,
    reservedInstance_paymentOption,
    reservedInstance_usagePrice,

    -- ** ReservedInstanceOffering
    reservedInstanceOffering_recurringCharges,
    reservedInstanceOffering_instanceType,
    reservedInstanceOffering_duration,
    reservedInstanceOffering_currencyCode,
    reservedInstanceOffering_fixedPrice,
    reservedInstanceOffering_reservedInstanceOfferingId,
    reservedInstanceOffering_paymentOption,
    reservedInstanceOffering_usagePrice,

    -- ** SAMLIdp
    sAMLIdp_metadataContent,
    sAMLIdp_entityId,

    -- ** SAMLOptionsInput
    sAMLOptionsInput_masterUserName,
    sAMLOptionsInput_enabled,
    sAMLOptionsInput_sessionTimeoutMinutes,
    sAMLOptionsInput_rolesKey,
    sAMLOptionsInput_masterBackendRole,
    sAMLOptionsInput_idp,
    sAMLOptionsInput_subjectKey,

    -- ** SAMLOptionsOutput
    sAMLOptionsOutput_enabled,
    sAMLOptionsOutput_sessionTimeoutMinutes,
    sAMLOptionsOutput_rolesKey,
    sAMLOptionsOutput_idp,
    sAMLOptionsOutput_subjectKey,

    -- ** ScheduledAutoTuneDetails
    scheduledAutoTuneDetails_severity,
    scheduledAutoTuneDetails_actionType,
    scheduledAutoTuneDetails_date,
    scheduledAutoTuneDetails_action,

    -- ** ServiceSoftwareOptions
    serviceSoftwareOptions_optionalDeployment,
    serviceSoftwareOptions_newVersion,
    serviceSoftwareOptions_updateAvailable,
    serviceSoftwareOptions_cancellable,
    serviceSoftwareOptions_updateStatus,
    serviceSoftwareOptions_automatedUpdateDate,
    serviceSoftwareOptions_description,
    serviceSoftwareOptions_currentVersion,

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
    upgradeStepItem_upgradeStep,
    upgradeStepItem_issues,
    upgradeStepItem_progressPercent,
    upgradeStepItem_upgradeStepStatus,

    -- ** VPCDerivedInfo
    vPCDerivedInfo_securityGroupIds,
    vPCDerivedInfo_availabilityZones,
    vPCDerivedInfo_vPCId,
    vPCDerivedInfo_subnetIds,

    -- ** VPCDerivedInfoStatus
    vPCDerivedInfoStatus_options,
    vPCDerivedInfoStatus_status,

    -- ** VPCOptions
    vPCOptions_securityGroupIds,
    vPCOptions_subnetIds,

    -- ** VersionStatus
    versionStatus_options,
    versionStatus_status,

    -- ** VpcEndpoint
    vpcEndpoint_vpcEndpointOwner,
    vpcEndpoint_domainArn,
    vpcEndpoint_status,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcOptions,
    vpcEndpoint_endpoint,

    -- ** VpcEndpointError
    vpcEndpointError_errorMessage,
    vpcEndpointError_vpcEndpointId,
    vpcEndpointError_errorCode,

    -- ** VpcEndpointSummary
    vpcEndpointSummary_vpcEndpointOwner,
    vpcEndpointSummary_domainArn,
    vpcEndpointSummary_status,
    vpcEndpointSummary_vpcEndpointId,

    -- ** ZoneAwarenessConfig
    zoneAwarenessConfig_availabilityZoneCount,
  )
where

import Amazonka.OpenSearch.AcceptInboundConnection
import Amazonka.OpenSearch.AddTags
import Amazonka.OpenSearch.AssociatePackage
import Amazonka.OpenSearch.AuthorizeVpcEndpointAccess
import Amazonka.OpenSearch.CancelServiceSoftwareUpdate
import Amazonka.OpenSearch.CreateDomain
import Amazonka.OpenSearch.CreateOutboundConnection
import Amazonka.OpenSearch.CreatePackage
import Amazonka.OpenSearch.CreateVpcEndpoint
import Amazonka.OpenSearch.DeleteDomain
import Amazonka.OpenSearch.DeleteInboundConnection
import Amazonka.OpenSearch.DeleteOutboundConnection
import Amazonka.OpenSearch.DeletePackage
import Amazonka.OpenSearch.DeleteVpcEndpoint
import Amazonka.OpenSearch.DescribeDomain
import Amazonka.OpenSearch.DescribeDomainAutoTunes
import Amazonka.OpenSearch.DescribeDomainChangeProgress
import Amazonka.OpenSearch.DescribeDomainConfig
import Amazonka.OpenSearch.DescribeDomains
import Amazonka.OpenSearch.DescribeInboundConnections
import Amazonka.OpenSearch.DescribeInstanceTypeLimits
import Amazonka.OpenSearch.DescribeOutboundConnections
import Amazonka.OpenSearch.DescribePackages
import Amazonka.OpenSearch.DescribeReservedInstanceOfferings
import Amazonka.OpenSearch.DescribeReservedInstances
import Amazonka.OpenSearch.DescribeVpcEndpoints
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
import Amazonka.OpenSearch.ListVpcEndpointAccess
import Amazonka.OpenSearch.ListVpcEndpoints
import Amazonka.OpenSearch.ListVpcEndpointsForDomain
import Amazonka.OpenSearch.PurchaseReservedInstanceOffering
import Amazonka.OpenSearch.RejectInboundConnection
import Amazonka.OpenSearch.RemoveTags
import Amazonka.OpenSearch.RevokeVpcEndpointAccess
import Amazonka.OpenSearch.StartServiceSoftwareUpdate
import Amazonka.OpenSearch.Types.AWSDomainInformation
import Amazonka.OpenSearch.Types.AccessPoliciesStatus
import Amazonka.OpenSearch.Types.AdditionalLimit
import Amazonka.OpenSearch.Types.AdvancedOptionsStatus
import Amazonka.OpenSearch.Types.AdvancedSecurityOptions
import Amazonka.OpenSearch.Types.AdvancedSecurityOptionsInput
import Amazonka.OpenSearch.Types.AdvancedSecurityOptionsStatus
import Amazonka.OpenSearch.Types.AuthorizedPrincipal
import Amazonka.OpenSearch.Types.AutoTune
import Amazonka.OpenSearch.Types.AutoTuneDetails
import Amazonka.OpenSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.OpenSearch.Types.AutoTuneOptions
import Amazonka.OpenSearch.Types.AutoTuneOptionsInput
import Amazonka.OpenSearch.Types.AutoTuneOptionsOutput
import Amazonka.OpenSearch.Types.AutoTuneOptionsStatus
import Amazonka.OpenSearch.Types.AutoTuneStatus
import Amazonka.OpenSearch.Types.ChangeProgressDetails
import Amazonka.OpenSearch.Types.ChangeProgressStage
import Amazonka.OpenSearch.Types.ChangeProgressStatusDetails
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
import Amazonka.OpenSearch.Types.DryRunResults
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
import Amazonka.OpenSearch.Types.VpcEndpoint
import Amazonka.OpenSearch.Types.VpcEndpointError
import Amazonka.OpenSearch.Types.VpcEndpointSummary
import Amazonka.OpenSearch.Types.ZoneAwarenessConfig
import Amazonka.OpenSearch.UpdateDomainConfig
import Amazonka.OpenSearch.UpdatePackage
import Amazonka.OpenSearch.UpdateVpcEndpoint
import Amazonka.OpenSearch.UpgradeDomain
