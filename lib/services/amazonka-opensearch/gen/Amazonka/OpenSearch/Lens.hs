{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearch.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createDomain_accessPolicies,
    createDomain_advancedOptions,
    createDomain_advancedSecurityOptions,
    createDomain_autoTuneOptions,
    createDomain_clusterConfig,
    createDomain_cognitoOptions,
    createDomain_domainEndpointOptions,
    createDomain_eBSOptions,
    createDomain_encryptionAtRestOptions,
    createDomain_engineVersion,
    createDomain_logPublishingOptions,
    createDomain_nodeToNodeEncryptionOptions,
    createDomain_snapshotOptions,
    createDomain_tagList,
    createDomain_vPCOptions,
    createDomain_domainName,
    createDomainResponse_domainStatus,
    createDomainResponse_httpStatus,

    -- ** CreateOutboundConnection
    createOutboundConnection_localDomainInfo,
    createOutboundConnection_remoteDomainInfo,
    createOutboundConnection_connectionAlias,
    createOutboundConnectionResponse_connectionAlias,
    createOutboundConnectionResponse_connectionId,
    createOutboundConnectionResponse_connectionStatus,
    createOutboundConnectionResponse_localDomainInfo,
    createOutboundConnectionResponse_remoteDomainInfo,
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

    -- ** DescribeDomainConfig
    describeDomainConfig_domainName,
    describeDomainConfigResponse_httpStatus,
    describeDomainConfigResponse_domainConfig,

    -- ** DescribeDomains
    describeDomains_domainNames,
    describeDomainsResponse_httpStatus,
    describeDomainsResponse_domainStatusList,

    -- ** DescribeInboundConnections
    describeInboundConnections_filters,
    describeInboundConnections_maxResults,
    describeInboundConnections_nextToken,
    describeInboundConnectionsResponse_connections,
    describeInboundConnectionsResponse_nextToken,
    describeInboundConnectionsResponse_httpStatus,

    -- ** DescribeInstanceTypeLimits
    describeInstanceTypeLimits_domainName,
    describeInstanceTypeLimits_instanceType,
    describeInstanceTypeLimits_engineVersion,
    describeInstanceTypeLimitsResponse_limitsByRole,
    describeInstanceTypeLimitsResponse_httpStatus,

    -- ** DescribeOutboundConnections
    describeOutboundConnections_filters,
    describeOutboundConnections_maxResults,
    describeOutboundConnections_nextToken,
    describeOutboundConnectionsResponse_connections,
    describeOutboundConnectionsResponse_nextToken,
    describeOutboundConnectionsResponse_httpStatus,

    -- ** DescribePackages
    describePackages_filters,
    describePackages_maxResults,
    describePackages_nextToken,
    describePackagesResponse_nextToken,
    describePackagesResponse_packageDetailsList,
    describePackagesResponse_httpStatus,

    -- ** DescribeReservedInstanceOfferings
    describeReservedInstanceOfferings_maxResults,
    describeReservedInstanceOfferings_nextToken,
    describeReservedInstanceOfferings_reservedInstanceOfferingId,
    describeReservedInstanceOfferingsResponse_nextToken,
    describeReservedInstanceOfferingsResponse_reservedInstanceOfferings,
    describeReservedInstanceOfferingsResponse_httpStatus,

    -- ** DescribeReservedInstances
    describeReservedInstances_maxResults,
    describeReservedInstances_nextToken,
    describeReservedInstances_reservedInstanceId,
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

    -- ** ListInstanceTypeDetails
    listInstanceTypeDetails_domainName,
    listInstanceTypeDetails_maxResults,
    listInstanceTypeDetails_nextToken,
    listInstanceTypeDetails_engineVersion,
    listInstanceTypeDetailsResponse_instanceTypeDetails,
    listInstanceTypeDetailsResponse_nextToken,
    listInstanceTypeDetailsResponse_httpStatus,

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

    -- ** ListVersions
    listVersions_maxResults,
    listVersions_nextToken,
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
    purchaseReservedInstanceOfferingResponse_reservationName,
    purchaseReservedInstanceOfferingResponse_reservedInstanceId,
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
    updateDomainConfig_accessPolicies,
    updateDomainConfig_advancedOptions,
    updateDomainConfig_advancedSecurityOptions,
    updateDomainConfig_autoTuneOptions,
    updateDomainConfig_clusterConfig,
    updateDomainConfig_cognitoOptions,
    updateDomainConfig_domainEndpointOptions,
    updateDomainConfig_dryRun,
    updateDomainConfig_eBSOptions,
    updateDomainConfig_encryptionAtRestOptions,
    updateDomainConfig_logPublishingOptions,
    updateDomainConfig_nodeToNodeEncryptionOptions,
    updateDomainConfig_snapshotOptions,
    updateDomainConfig_vPCOptions,
    updateDomainConfig_domainName,
    updateDomainConfigResponse_dryRunResults,
    updateDomainConfigResponse_httpStatus,
    updateDomainConfigResponse_domainConfig,

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

    -- ** UpgradeDomain
    upgradeDomain_advancedOptions,
    upgradeDomain_performCheckOnly,
    upgradeDomain_domainName,
    upgradeDomain_targetVersion,
    upgradeDomainResponse_advancedOptions,
    upgradeDomainResponse_changeProgressDetails,
    upgradeDomainResponse_domainName,
    upgradeDomainResponse_performCheckOnly,
    upgradeDomainResponse_targetVersion,
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

    -- ** ClusterConfig
    clusterConfig_coldStorageOptions,
    clusterConfig_dedicatedMasterCount,
    clusterConfig_dedicatedMasterEnabled,
    clusterConfig_dedicatedMasterType,
    clusterConfig_instanceCount,
    clusterConfig_instanceType,
    clusterConfig_warmCount,
    clusterConfig_warmEnabled,
    clusterConfig_warmType,
    clusterConfig_zoneAwarenessConfig,
    clusterConfig_zoneAwarenessEnabled,

    -- ** ClusterConfigStatus
    clusterConfigStatus_options,
    clusterConfigStatus_status,

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

    -- ** DomainConfig
    domainConfig_accessPolicies,
    domainConfig_advancedOptions,
    domainConfig_advancedSecurityOptions,
    domainConfig_autoTuneOptions,
    domainConfig_changeProgressDetails,
    domainConfig_clusterConfig,
    domainConfig_cognitoOptions,
    domainConfig_domainEndpointOptions,
    domainConfig_eBSOptions,
    domainConfig_encryptionAtRestOptions,
    domainConfig_engineVersion,
    domainConfig_logPublishingOptions,
    domainConfig_nodeToNodeEncryptionOptions,
    domainConfig_snapshotOptions,
    domainConfig_vPCOptions,

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

    -- ** DomainInformationContainer
    domainInformationContainer_aWSDomainInformation,

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

    -- ** DomainStatus
    domainStatus_accessPolicies,
    domainStatus_advancedOptions,
    domainStatus_advancedSecurityOptions,
    domainStatus_autoTuneOptions,
    domainStatus_changeProgressDetails,
    domainStatus_cognitoOptions,
    domainStatus_created,
    domainStatus_deleted,
    domainStatus_domainEndpointOptions,
    domainStatus_eBSOptions,
    domainStatus_encryptionAtRestOptions,
    domainStatus_endpoint,
    domainStatus_endpoints,
    domainStatus_engineVersion,
    domainStatus_logPublishingOptions,
    domainStatus_nodeToNodeEncryptionOptions,
    domainStatus_processing,
    domainStatus_serviceSoftwareOptions,
    domainStatus_snapshotOptions,
    domainStatus_upgradeProcessing,
    domainStatus_vPCOptions,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_arn,
    domainStatus_clusterConfig,

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
    inboundConnection_connectionId,
    inboundConnection_connectionStatus,
    inboundConnection_localDomainInfo,
    inboundConnection_remoteDomainInfo,

    -- ** InboundConnectionStatus
    inboundConnectionStatus_message,
    inboundConnectionStatus_statusCode,

    -- ** InstanceCountLimits
    instanceCountLimits_maximumInstanceCount,
    instanceCountLimits_minimumInstanceCount,

    -- ** InstanceLimits
    instanceLimits_instanceCountLimits,

    -- ** InstanceTypeDetails
    instanceTypeDetails_advancedSecurityEnabled,
    instanceTypeDetails_appLogsEnabled,
    instanceTypeDetails_cognitoEnabled,
    instanceTypeDetails_encryptionEnabled,
    instanceTypeDetails_instanceRole,
    instanceTypeDetails_instanceType,
    instanceTypeDetails_warmEnabled,

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

    -- ** OutboundConnection
    outboundConnection_connectionAlias,
    outboundConnection_connectionId,
    outboundConnection_connectionStatus,
    outboundConnection_localDomainInfo,
    outboundConnection_remoteDomainInfo,

    -- ** OutboundConnectionStatus
    outboundConnectionStatus_message,
    outboundConnectionStatus_statusCode,

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

    -- ** ReservedInstance
    reservedInstance_billingSubscriptionId,
    reservedInstance_currencyCode,
    reservedInstance_duration,
    reservedInstance_fixedPrice,
    reservedInstance_instanceCount,
    reservedInstance_instanceType,
    reservedInstance_paymentOption,
    reservedInstance_recurringCharges,
    reservedInstance_reservationName,
    reservedInstance_reservedInstanceId,
    reservedInstance_reservedInstanceOfferingId,
    reservedInstance_startTime,
    reservedInstance_state,
    reservedInstance_usagePrice,

    -- ** ReservedInstanceOffering
    reservedInstanceOffering_currencyCode,
    reservedInstanceOffering_duration,
    reservedInstanceOffering_fixedPrice,
    reservedInstanceOffering_instanceType,
    reservedInstanceOffering_paymentOption,
    reservedInstanceOffering_recurringCharges,
    reservedInstanceOffering_reservedInstanceOfferingId,
    reservedInstanceOffering_usagePrice,

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

    -- ** VersionStatus
    versionStatus_options,
    versionStatus_status,

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
