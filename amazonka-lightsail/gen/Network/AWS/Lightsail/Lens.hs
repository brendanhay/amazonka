{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Lens
  ( -- * Operations

    -- ** GetContainerServices
    getContainerServices_serviceName,
    getContainerServicesResponse_containerServices,
    getContainerServicesResponse_httpStatus,

    -- ** GetStaticIp
    getStaticIp_staticIpName,
    getStaticIpResponse_staticIp,
    getStaticIpResponse_httpStatus,

    -- ** GetRelationalDatabaseSnapshots
    getRelationalDatabaseSnapshots_pageToken,
    getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots,
    getRelationalDatabaseSnapshotsResponse_nextPageToken,
    getRelationalDatabaseSnapshotsResponse_httpStatus,

    -- ** GetDistributions
    getDistributions_pageToken,
    getDistributions_distributionName,
    getDistributionsResponse_distributions,
    getDistributionsResponse_nextPageToken,
    getDistributionsResponse_httpStatus,

    -- ** GetDiskSnapshot
    getDiskSnapshot_diskSnapshotName,
    getDiskSnapshotResponse_diskSnapshot,
    getDiskSnapshotResponse_httpStatus,

    -- ** CreateContainerServiceDeployment
    createContainerServiceDeployment_publicEndpoint,
    createContainerServiceDeployment_containers,
    createContainerServiceDeployment_serviceName,
    createContainerServiceDeploymentResponse_containerService,
    createContainerServiceDeploymentResponse_httpStatus,

    -- ** PeerVpc
    peerVpcResponse_operation,
    peerVpcResponse_httpStatus,

    -- ** UpdateLoadBalancerAttribute
    updateLoadBalancerAttribute_loadBalancerName,
    updateLoadBalancerAttribute_attributeName,
    updateLoadBalancerAttribute_attributeValue,
    updateLoadBalancerAttributeResponse_operations,
    updateLoadBalancerAttributeResponse_httpStatus,

    -- ** UpdateDistributionBundle
    updateDistributionBundle_bundleId,
    updateDistributionBundle_distributionName,
    updateDistributionBundleResponse_operation,
    updateDistributionBundleResponse_httpStatus,

    -- ** AllocateStaticIp
    allocateStaticIp_staticIpName,
    allocateStaticIpResponse_operations,
    allocateStaticIpResponse_httpStatus,

    -- ** CloseInstancePublicPorts
    closeInstancePublicPorts_portInfo,
    closeInstancePublicPorts_instanceName,
    closeInstancePublicPortsResponse_operation,
    closeInstancePublicPortsResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_certificateName,
    deleteCertificateResponse_operations,
    deleteCertificateResponse_httpStatus,

    -- ** DisableAddOn
    disableAddOn_addOnType,
    disableAddOn_resourceName,
    disableAddOnResponse_operations,
    disableAddOnResponse_httpStatus,

    -- ** GetCloudFormationStackRecords
    getCloudFormationStackRecords_pageToken,
    getCloudFormationStackRecordsResponse_cloudFormationStackRecords,
    getCloudFormationStackRecordsResponse_nextPageToken,
    getCloudFormationStackRecordsResponse_httpStatus,

    -- ** IsVpcPeered
    isVpcPeeredResponse_isPeered,
    isVpcPeeredResponse_httpStatus,

    -- ** GetRelationalDatabaseSnapshot
    getRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,
    getRelationalDatabaseSnapshotResponse_relationalDatabaseSnapshot,
    getRelationalDatabaseSnapshotResponse_httpStatus,

    -- ** GetRelationalDatabaseBlueprints
    getRelationalDatabaseBlueprints_pageToken,
    getRelationalDatabaseBlueprintsResponse_blueprints,
    getRelationalDatabaseBlueprintsResponse_nextPageToken,
    getRelationalDatabaseBlueprintsResponse_httpStatus,

    -- ** DeleteInstanceSnapshot
    deleteInstanceSnapshot_instanceSnapshotName,
    deleteInstanceSnapshotResponse_operations,
    deleteInstanceSnapshotResponse_httpStatus,

    -- ** UnpeerVpc
    unpeerVpcResponse_operation,
    unpeerVpcResponse_httpStatus,

    -- ** GetContainerAPIMetadata
    getContainerAPIMetadataResponse_metadata,
    getContainerAPIMetadataResponse_httpStatus,

    -- ** GetInstances
    getInstances_pageToken,
    getInstancesResponse_instances,
    getInstancesResponse_nextPageToken,
    getInstancesResponse_httpStatus,

    -- ** CreateInstancesFromSnapshot
    createInstancesFromSnapshot_ipAddressType,
    createInstancesFromSnapshot_restoreDate,
    createInstancesFromSnapshot_userData,
    createInstancesFromSnapshot_addOns,
    createInstancesFromSnapshot_attachedDiskMapping,
    createInstancesFromSnapshot_instanceSnapshotName,
    createInstancesFromSnapshot_keyPairName,
    createInstancesFromSnapshot_useLatestRestorableAutoSnapshot,
    createInstancesFromSnapshot_tags,
    createInstancesFromSnapshot_sourceInstanceName,
    createInstancesFromSnapshot_instanceNames,
    createInstancesFromSnapshot_availabilityZone,
    createInstancesFromSnapshot_bundleId,
    createInstancesFromSnapshotResponse_operations,
    createInstancesFromSnapshotResponse_httpStatus,

    -- ** StartInstance
    startInstance_instanceName,
    startInstanceResponse_operations,
    startInstanceResponse_httpStatus,

    -- ** CreateKeyPair
    createKeyPair_tags,
    createKeyPair_keyPairName,
    createKeyPairResponse_privateKeyBase64,
    createKeyPairResponse_operation,
    createKeyPairResponse_keyPair,
    createKeyPairResponse_publicKeyBase64,
    createKeyPairResponse_httpStatus,

    -- ** GetInstanceAccessDetails
    getInstanceAccessDetails_protocol,
    getInstanceAccessDetails_instanceName,
    getInstanceAccessDetailsResponse_accessDetails,
    getInstanceAccessDetailsResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_restoreDate,
    copySnapshot_sourceSnapshotName,
    copySnapshot_useLatestRestorableAutoSnapshot,
    copySnapshot_sourceResourceName,
    copySnapshot_targetSnapshotName,
    copySnapshot_sourceRegion,
    copySnapshotResponse_operations,
    copySnapshotResponse_httpStatus,

    -- ** CreateInstanceSnapshot
    createInstanceSnapshot_tags,
    createInstanceSnapshot_instanceSnapshotName,
    createInstanceSnapshot_instanceName,
    createInstanceSnapshotResponse_operations,
    createInstanceSnapshotResponse_httpStatus,

    -- ** StopInstance
    stopInstance_force,
    stopInstance_instanceName,
    stopInstanceResponse_operations,
    stopInstanceResponse_httpStatus,

    -- ** RegisterContainerImage
    registerContainerImage_serviceName,
    registerContainerImage_label,
    registerContainerImage_digest,
    registerContainerImageResponse_containerImage,
    registerContainerImageResponse_httpStatus,

    -- ** DetachCertificateFromDistribution
    detachCertificateFromDistribution_distributionName,
    detachCertificateFromDistributionResponse_operation,
    detachCertificateFromDistributionResponse_httpStatus,

    -- ** CreateLoadBalancer
    createLoadBalancer_ipAddressType,
    createLoadBalancer_certificateAlternativeNames,
    createLoadBalancer_healthCheckPath,
    createLoadBalancer_tags,
    createLoadBalancer_certificateDomainName,
    createLoadBalancer_certificateName,
    createLoadBalancer_loadBalancerName,
    createLoadBalancer_instancePort,
    createLoadBalancerResponse_operations,
    createLoadBalancerResponse_httpStatus,

    -- ** GetContainerServiceDeployments
    getContainerServiceDeployments_serviceName,
    getContainerServiceDeploymentsResponse_deployments,
    getContainerServiceDeploymentsResponse_httpStatus,

    -- ** DeleteContactMethod
    deleteContactMethod_protocol,
    deleteContactMethodResponse_operations,
    deleteContactMethodResponse_httpStatus,

    -- ** GetDomain
    getDomain_domainName,
    getDomainResponse_domain,
    getDomainResponse_httpStatus,

    -- ** DetachStaticIp
    detachStaticIp_staticIpName,
    detachStaticIpResponse_operations,
    detachStaticIpResponse_httpStatus,

    -- ** AttachDisk
    attachDisk_diskName,
    attachDisk_instanceName,
    attachDisk_diskPath,
    attachDiskResponse_operations,
    attachDiskResponse_httpStatus,

    -- ** GetDisk
    getDisk_diskName,
    getDiskResponse_disk,
    getDiskResponse_httpStatus,

    -- ** GetRelationalDatabaseLogEvents
    getRelationalDatabaseLogEvents_startFromHead,
    getRelationalDatabaseLogEvents_pageToken,
    getRelationalDatabaseLogEvents_startTime,
    getRelationalDatabaseLogEvents_endTime,
    getRelationalDatabaseLogEvents_relationalDatabaseName,
    getRelationalDatabaseLogEvents_logStreamName,
    getRelationalDatabaseLogEventsResponse_nextBackwardToken,
    getRelationalDatabaseLogEventsResponse_nextForwardToken,
    getRelationalDatabaseLogEventsResponse_resourceLogEvents,
    getRelationalDatabaseLogEventsResponse_httpStatus,

    -- ** GetRelationalDatabases
    getRelationalDatabases_pageToken,
    getRelationalDatabasesResponse_nextPageToken,
    getRelationalDatabasesResponse_relationalDatabases,
    getRelationalDatabasesResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_resourceName,
    untagResource_tagKeys,
    untagResourceResponse_operations,
    untagResourceResponse_httpStatus,

    -- ** CreateDiskFromSnapshot
    createDiskFromSnapshot_sourceDiskName,
    createDiskFromSnapshot_restoreDate,
    createDiskFromSnapshot_addOns,
    createDiskFromSnapshot_useLatestRestorableAutoSnapshot,
    createDiskFromSnapshot_tags,
    createDiskFromSnapshot_diskSnapshotName,
    createDiskFromSnapshot_diskName,
    createDiskFromSnapshot_availabilityZone,
    createDiskFromSnapshot_sizeInGb,
    createDiskFromSnapshotResponse_operations,
    createDiskFromSnapshotResponse_httpStatus,

    -- ** GetInstanceMetricData
    getInstanceMetricData_instanceName,
    getInstanceMetricData_metricName,
    getInstanceMetricData_period,
    getInstanceMetricData_startTime,
    getInstanceMetricData_endTime,
    getInstanceMetricData_unit,
    getInstanceMetricData_statistics,
    getInstanceMetricDataResponse_metricName,
    getInstanceMetricDataResponse_metricData,
    getInstanceMetricDataResponse_httpStatus,

    -- ** DeleteLoadBalancer
    deleteLoadBalancer_loadBalancerName,
    deleteLoadBalancerResponse_operations,
    deleteLoadBalancerResponse_httpStatus,

    -- ** ExportSnapshot
    exportSnapshot_sourceSnapshotName,
    exportSnapshotResponse_operations,
    exportSnapshotResponse_httpStatus,

    -- ** CreateRelationalDatabaseFromSnapshot
    createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId,
    createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName,
    createRelationalDatabaseFromSnapshot_restoreTime,
    createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName,
    createRelationalDatabaseFromSnapshot_publiclyAccessible,
    createRelationalDatabaseFromSnapshot_availabilityZone,
    createRelationalDatabaseFromSnapshot_tags,
    createRelationalDatabaseFromSnapshot_useLatestRestorableTime,
    createRelationalDatabaseFromSnapshot_relationalDatabaseName,
    createRelationalDatabaseFromSnapshotResponse_operations,
    createRelationalDatabaseFromSnapshotResponse_httpStatus,

    -- ** GetOperations
    getOperations_pageToken,
    getOperationsResponse_operations,
    getOperationsResponse_nextPageToken,
    getOperationsResponse_httpStatus,

    -- ** GetExportSnapshotRecords
    getExportSnapshotRecords_pageToken,
    getExportSnapshotRecordsResponse_exportSnapshotRecords,
    getExportSnapshotRecordsResponse_nextPageToken,
    getExportSnapshotRecordsResponse_httpStatus,

    -- ** GetLoadBalancerMetricData
    getLoadBalancerMetricData_loadBalancerName,
    getLoadBalancerMetricData_metricName,
    getLoadBalancerMetricData_period,
    getLoadBalancerMetricData_startTime,
    getLoadBalancerMetricData_endTime,
    getLoadBalancerMetricData_unit,
    getLoadBalancerMetricData_statistics,
    getLoadBalancerMetricDataResponse_metricName,
    getLoadBalancerMetricDataResponse_metricData,
    getLoadBalancerMetricDataResponse_httpStatus,

    -- ** GetInstanceSnapshots
    getInstanceSnapshots_pageToken,
    getInstanceSnapshotsResponse_instanceSnapshots,
    getInstanceSnapshotsResponse_nextPageToken,
    getInstanceSnapshotsResponse_httpStatus,

    -- ** DeleteInstance
    deleteInstance_forceDeleteAddOns,
    deleteInstance_instanceName,
    deleteInstanceResponse_operations,
    deleteInstanceResponse_httpStatus,

    -- ** CreateLoadBalancerTlsCertificate
    createLoadBalancerTlsCertificate_certificateAlternativeNames,
    createLoadBalancerTlsCertificate_tags,
    createLoadBalancerTlsCertificate_loadBalancerName,
    createLoadBalancerTlsCertificate_certificateName,
    createLoadBalancerTlsCertificate_certificateDomainName,
    createLoadBalancerTlsCertificateResponse_operations,
    createLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** RebootInstance
    rebootInstance_instanceName,
    rebootInstanceResponse_operations,
    rebootInstanceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_resourceName,
    tagResource_tags,
    tagResourceResponse_operations,
    tagResourceResponse_httpStatus,

    -- ** GetRelationalDatabase
    getRelationalDatabase_relationalDatabaseName,
    getRelationalDatabaseResponse_relationalDatabase,
    getRelationalDatabaseResponse_httpStatus,

    -- ** GetKeyPairs
    getKeyPairs_pageToken,
    getKeyPairsResponse_keyPairs,
    getKeyPairsResponse_nextPageToken,
    getKeyPairsResponse_httpStatus,

    -- ** AttachInstancesToLoadBalancer
    attachInstancesToLoadBalancer_loadBalancerName,
    attachInstancesToLoadBalancer_instanceNames,
    attachInstancesToLoadBalancerResponse_operations,
    attachInstancesToLoadBalancerResponse_httpStatus,

    -- ** GetRegions
    getRegions_includeRelationalDatabaseAvailabilityZones,
    getRegions_includeAvailabilityZones,
    getRegionsResponse_regions,
    getRegionsResponse_httpStatus,

    -- ** TestAlarm
    testAlarm_alarmName,
    testAlarm_state,
    testAlarmResponse_operations,
    testAlarmResponse_httpStatus,

    -- ** CreateDiskSnapshot
    createDiskSnapshot_instanceName,
    createDiskSnapshot_tags,
    createDiskSnapshot_diskName,
    createDiskSnapshot_diskSnapshotName,
    createDiskSnapshotResponse_operations,
    createDiskSnapshotResponse_httpStatus,

    -- ** SetIpAddressType
    setIpAddressType_resourceType,
    setIpAddressType_resourceName,
    setIpAddressType_ipAddressType,
    setIpAddressTypeResponse_operations,
    setIpAddressTypeResponse_httpStatus,

    -- ** DeleteAlarm
    deleteAlarm_alarmName,
    deleteAlarmResponse_operations,
    deleteAlarmResponse_httpStatus,

    -- ** SendContactMethodVerification
    sendContactMethodVerification_protocol,
    sendContactMethodVerificationResponse_operations,
    sendContactMethodVerificationResponse_httpStatus,

    -- ** GetRelationalDatabaseMasterUserPassword
    getRelationalDatabaseMasterUserPassword_passwordVersion,
    getRelationalDatabaseMasterUserPassword_relationalDatabaseName,
    getRelationalDatabaseMasterUserPasswordResponse_createdAt,
    getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword,
    getRelationalDatabaseMasterUserPasswordResponse_httpStatus,

    -- ** GetBlueprints
    getBlueprints_pageToken,
    getBlueprints_includeInactive,
    getBlueprintsResponse_blueprints,
    getBlueprintsResponse_nextPageToken,
    getBlueprintsResponse_httpStatus,

    -- ** DetachDisk
    detachDisk_diskName,
    detachDiskResponse_operations,
    detachDiskResponse_httpStatus,

    -- ** GetInstancePortStates
    getInstancePortStates_instanceName,
    getInstancePortStatesResponse_portStates,
    getInstancePortStatesResponse_httpStatus,

    -- ** AttachStaticIp
    attachStaticIp_staticIpName,
    attachStaticIp_instanceName,
    attachStaticIpResponse_operations,
    attachStaticIpResponse_httpStatus,

    -- ** DownloadDefaultKeyPair
    downloadDefaultKeyPairResponse_privateKeyBase64,
    downloadDefaultKeyPairResponse_publicKeyBase64,
    downloadDefaultKeyPairResponse_httpStatus,

    -- ** GetLoadBalancers
    getLoadBalancers_pageToken,
    getLoadBalancersResponse_nextPageToken,
    getLoadBalancersResponse_loadBalancers,
    getLoadBalancersResponse_httpStatus,

    -- ** UpdateRelationalDatabase
    updateRelationalDatabase_preferredBackupWindow,
    updateRelationalDatabase_caCertificateIdentifier,
    updateRelationalDatabase_disableBackupRetention,
    updateRelationalDatabase_masterUserPassword,
    updateRelationalDatabase_publiclyAccessible,
    updateRelationalDatabase_preferredMaintenanceWindow,
    updateRelationalDatabase_enableBackupRetention,
    updateRelationalDatabase_rotateMasterUserPassword,
    updateRelationalDatabase_applyImmediately,
    updateRelationalDatabase_relationalDatabaseName,
    updateRelationalDatabaseResponse_operations,
    updateRelationalDatabaseResponse_httpStatus,

    -- ** GetRelationalDatabaseBundles
    getRelationalDatabaseBundles_pageToken,
    getRelationalDatabaseBundlesResponse_nextPageToken,
    getRelationalDatabaseBundlesResponse_bundles,
    getRelationalDatabaseBundlesResponse_httpStatus,

    -- ** AttachLoadBalancerTlsCertificate
    attachLoadBalancerTlsCertificate_loadBalancerName,
    attachLoadBalancerTlsCertificate_certificateName,
    attachLoadBalancerTlsCertificateResponse_operations,
    attachLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** AttachCertificateToDistribution
    attachCertificateToDistribution_distributionName,
    attachCertificateToDistribution_certificateName,
    attachCertificateToDistributionResponse_operation,
    attachCertificateToDistributionResponse_httpStatus,

    -- ** DeleteRelationalDatabase
    deleteRelationalDatabase_finalRelationalDatabaseSnapshotName,
    deleteRelationalDatabase_skipFinalSnapshot,
    deleteRelationalDatabase_relationalDatabaseName,
    deleteRelationalDatabaseResponse_operations,
    deleteRelationalDatabaseResponse_httpStatus,

    -- ** GetInstance
    getInstance_instanceName,
    getInstanceResponse_instance,
    getInstanceResponse_httpStatus,

    -- ** RebootRelationalDatabase
    rebootRelationalDatabase_relationalDatabaseName,
    rebootRelationalDatabaseResponse_operations,
    rebootRelationalDatabaseResponse_httpStatus,

    -- ** GetRelationalDatabaseEvents
    getRelationalDatabaseEvents_durationInMinutes,
    getRelationalDatabaseEvents_pageToken,
    getRelationalDatabaseEvents_relationalDatabaseName,
    getRelationalDatabaseEventsResponse_nextPageToken,
    getRelationalDatabaseEventsResponse_relationalDatabaseEvents,
    getRelationalDatabaseEventsResponse_httpStatus,

    -- ** CreateDomain
    createDomain_tags,
    createDomain_domainName,
    createDomainResponse_operation,
    createDomainResponse_httpStatus,

    -- ** GetStaticIps
    getStaticIps_pageToken,
    getStaticIpsResponse_nextPageToken,
    getStaticIpsResponse_staticIps,
    getStaticIpsResponse_httpStatus,

    -- ** DeleteDisk
    deleteDisk_forceDeleteAddOns,
    deleteDisk_diskName,
    deleteDiskResponse_operations,
    deleteDiskResponse_httpStatus,

    -- ** GetRelationalDatabaseMetricData
    getRelationalDatabaseMetricData_relationalDatabaseName,
    getRelationalDatabaseMetricData_metricName,
    getRelationalDatabaseMetricData_period,
    getRelationalDatabaseMetricData_startTime,
    getRelationalDatabaseMetricData_endTime,
    getRelationalDatabaseMetricData_unit,
    getRelationalDatabaseMetricData_statistics,
    getRelationalDatabaseMetricDataResponse_metricName,
    getRelationalDatabaseMetricDataResponse_metricData,
    getRelationalDatabaseMetricDataResponse_httpStatus,

    -- ** GetDiskSnapshots
    getDiskSnapshots_pageToken,
    getDiskSnapshotsResponse_nextPageToken,
    getDiskSnapshotsResponse_diskSnapshots,
    getDiskSnapshotsResponse_httpStatus,

    -- ** DeleteKeyPair
    deleteKeyPair_keyPairName,
    deleteKeyPairResponse_operation,
    deleteKeyPairResponse_httpStatus,

    -- ** GetLoadBalancer
    getLoadBalancer_loadBalancerName,
    getLoadBalancerResponse_loadBalancer,
    getLoadBalancerResponse_httpStatus,

    -- ** GetBundles
    getBundles_pageToken,
    getBundles_includeInactive,
    getBundlesResponse_nextPageToken,
    getBundlesResponse_bundles,
    getBundlesResponse_httpStatus,

    -- ** CreateCertificate
    createCertificate_subjectAlternativeNames,
    createCertificate_tags,
    createCertificate_certificateName,
    createCertificate_domainName,
    createCertificateResponse_operations,
    createCertificateResponse_certificate,
    createCertificateResponse_httpStatus,

    -- ** DetachInstancesFromLoadBalancer
    detachInstancesFromLoadBalancer_loadBalancerName,
    detachInstancesFromLoadBalancer_instanceNames,
    detachInstancesFromLoadBalancerResponse_operations,
    detachInstancesFromLoadBalancerResponse_httpStatus,

    -- ** GetLoadBalancerTlsCertificates
    getLoadBalancerTlsCertificates_loadBalancerName,
    getLoadBalancerTlsCertificatesResponse_tlsCertificates,
    getLoadBalancerTlsCertificatesResponse_httpStatus,

    -- ** DeleteContainerImage
    deleteContainerImage_serviceName,
    deleteContainerImage_image,
    deleteContainerImageResponse_httpStatus,

    -- ** GetOperationsForResource
    getOperationsForResource_pageToken,
    getOperationsForResource_resourceName,
    getOperationsForResourceResponse_operations,
    getOperationsForResourceResponse_nextPageCount,
    getOperationsForResourceResponse_nextPageToken,
    getOperationsForResourceResponse_httpStatus,

    -- ** CreateDisk
    createDisk_addOns,
    createDisk_tags,
    createDisk_diskName,
    createDisk_availabilityZone,
    createDisk_sizeInGb,
    createDiskResponse_operations,
    createDiskResponse_httpStatus,

    -- ** EnableAddOn
    enableAddOn_resourceName,
    enableAddOn_addOnRequest,
    enableAddOnResponse_operations,
    enableAddOnResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_operation,
    deleteDomainResponse_httpStatus,

    -- ** StartRelationalDatabase
    startRelationalDatabase_relationalDatabaseName,
    startRelationalDatabaseResponse_operations,
    startRelationalDatabaseResponse_httpStatus,

    -- ** CreateRelationalDatabaseSnapshot
    createRelationalDatabaseSnapshot_tags,
    createRelationalDatabaseSnapshot_relationalDatabaseName,
    createRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,
    createRelationalDatabaseSnapshotResponse_operations,
    createRelationalDatabaseSnapshotResponse_httpStatus,

    -- ** GetAlarms
    getAlarms_pageToken,
    getAlarms_alarmName,
    getAlarms_monitoredResourceName,
    getAlarmsResponse_nextPageToken,
    getAlarmsResponse_alarms,
    getAlarmsResponse_httpStatus,

    -- ** CreateDistribution
    createDistribution_ipAddressType,
    createDistribution_cacheBehaviorSettings,
    createDistribution_tags,
    createDistribution_cacheBehaviors,
    createDistribution_distributionName,
    createDistribution_origin,
    createDistribution_defaultCacheBehavior,
    createDistribution_bundleId,
    createDistributionResponse_operation,
    createDistributionResponse_distribution,
    createDistributionResponse_httpStatus,

    -- ** CreateInstances
    createInstances_ipAddressType,
    createInstances_userData,
    createInstances_addOns,
    createInstances_keyPairName,
    createInstances_tags,
    createInstances_customImageName,
    createInstances_instanceNames,
    createInstances_availabilityZone,
    createInstances_blueprintId,
    createInstances_bundleId,
    createInstancesResponse_operations,
    createInstancesResponse_httpStatus,

    -- ** CreateContainerService
    createContainerService_deployment,
    createContainerService_tags,
    createContainerService_publicDomainNames,
    createContainerService_serviceName,
    createContainerService_power,
    createContainerService_scale,
    createContainerServiceResponse_containerService,
    createContainerServiceResponse_httpStatus,

    -- ** GetDistributionLatestCacheReset
    getDistributionLatestCacheReset_distributionName,
    getDistributionLatestCacheResetResponse_status,
    getDistributionLatestCacheResetResponse_createTime,
    getDistributionLatestCacheResetResponse_httpStatus,

    -- ** StopRelationalDatabase
    stopRelationalDatabase_relationalDatabaseSnapshotName,
    stopRelationalDatabase_relationalDatabaseName,
    stopRelationalDatabaseResponse_operations,
    stopRelationalDatabaseResponse_httpStatus,

    -- ** DeleteKnownHostKeys
    deleteKnownHostKeys_instanceName,
    deleteKnownHostKeysResponse_operations,
    deleteKnownHostKeysResponse_httpStatus,

    -- ** OpenInstancePublicPorts
    openInstancePublicPorts_portInfo,
    openInstancePublicPorts_instanceName,
    openInstancePublicPortsResponse_operation,
    openInstancePublicPortsResponse_httpStatus,

    -- ** GetActiveNames
    getActiveNames_pageToken,
    getActiveNamesResponse_nextPageToken,
    getActiveNamesResponse_activeNames,
    getActiveNamesResponse_httpStatus,

    -- ** GetAutoSnapshots
    getAutoSnapshots_resourceName,
    getAutoSnapshotsResponse_resourceType,
    getAutoSnapshotsResponse_autoSnapshots,
    getAutoSnapshotsResponse_resourceName,
    getAutoSnapshotsResponse_httpStatus,

    -- ** GetRelationalDatabaseLogStreams
    getRelationalDatabaseLogStreams_relationalDatabaseName,
    getRelationalDatabaseLogStreamsResponse_logStreams,
    getRelationalDatabaseLogStreamsResponse_httpStatus,

    -- ** GetDistributionBundles
    getDistributionBundlesResponse_bundles,
    getDistributionBundlesResponse_httpStatus,

    -- ** GetOperation
    getOperation_operationId,
    getOperationResponse_operation,
    getOperationResponse_httpStatus,

    -- ** DeleteRelationalDatabaseSnapshot
    deleteRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,
    deleteRelationalDatabaseSnapshotResponse_operations,
    deleteRelationalDatabaseSnapshotResponse_httpStatus,

    -- ** GetInstanceSnapshot
    getInstanceSnapshot_instanceSnapshotName,
    getInstanceSnapshotResponse_instanceSnapshot,
    getInstanceSnapshotResponse_httpStatus,

    -- ** DeleteContainerService
    deleteContainerService_serviceName,
    deleteContainerServiceResponse_httpStatus,

    -- ** UpdateDistribution
    updateDistribution_isEnabled,
    updateDistribution_origin,
    updateDistribution_cacheBehaviorSettings,
    updateDistribution_cacheBehaviors,
    updateDistribution_defaultCacheBehavior,
    updateDistribution_distributionName,
    updateDistributionResponse_operation,
    updateDistributionResponse_httpStatus,

    -- ** PutInstancePublicPorts
    putInstancePublicPorts_portInfos,
    putInstancePublicPorts_instanceName,
    putInstancePublicPortsResponse_operation,
    putInstancePublicPortsResponse_httpStatus,

    -- ** ResetDistributionCache
    resetDistributionCache_distributionName,
    resetDistributionCacheResponse_status,
    resetDistributionCacheResponse_operation,
    resetDistributionCacheResponse_createTime,
    resetDistributionCacheResponse_httpStatus,

    -- ** CreateContactMethod
    createContactMethod_protocol,
    createContactMethod_contactEndpoint,
    createContactMethodResponse_operations,
    createContactMethodResponse_httpStatus,

    -- ** DeleteDistribution
    deleteDistribution_distributionName,
    deleteDistributionResponse_operation,
    deleteDistributionResponse_httpStatus,

    -- ** UpdateContainerService
    updateContainerService_power,
    updateContainerService_scale,
    updateContainerService_publicDomainNames,
    updateContainerService_isDisabled,
    updateContainerService_serviceName,
    updateContainerServiceResponse_containerService,
    updateContainerServiceResponse_httpStatus,

    -- ** GetKeyPair
    getKeyPair_keyPairName,
    getKeyPairResponse_keyPair,
    getKeyPairResponse_httpStatus,

    -- ** CreateCloudFormationStack
    createCloudFormationStack_instances,
    createCloudFormationStackResponse_operations,
    createCloudFormationStackResponse_httpStatus,

    -- ** CreateDomainEntry
    createDomainEntry_domainName,
    createDomainEntry_domainEntry,
    createDomainEntryResponse_operation,
    createDomainEntryResponse_httpStatus,

    -- ** GetInstanceState
    getInstanceState_instanceName,
    getInstanceStateResponse_state,
    getInstanceStateResponse_httpStatus,

    -- ** GetDistributionMetricData
    getDistributionMetricData_distributionName,
    getDistributionMetricData_metricName,
    getDistributionMetricData_startTime,
    getDistributionMetricData_endTime,
    getDistributionMetricData_period,
    getDistributionMetricData_unit,
    getDistributionMetricData_statistics,
    getDistributionMetricDataResponse_metricName,
    getDistributionMetricDataResponse_metricData,
    getDistributionMetricDataResponse_httpStatus,

    -- ** GetDisks
    getDisks_pageToken,
    getDisksResponse_nextPageToken,
    getDisksResponse_disks,
    getDisksResponse_httpStatus,

    -- ** GetContainerServiceMetricData
    getContainerServiceMetricData_serviceName,
    getContainerServiceMetricData_metricName,
    getContainerServiceMetricData_startTime,
    getContainerServiceMetricData_endTime,
    getContainerServiceMetricData_period,
    getContainerServiceMetricData_statistics,
    getContainerServiceMetricDataResponse_metricName,
    getContainerServiceMetricDataResponse_metricData,
    getContainerServiceMetricDataResponse_httpStatus,

    -- ** CreateContainerServiceRegistryLogin
    createContainerServiceRegistryLoginResponse_registryLogin,
    createContainerServiceRegistryLoginResponse_httpStatus,

    -- ** ImportKeyPair
    importKeyPair_keyPairName,
    importKeyPair_publicKeyBase64,
    importKeyPairResponse_operation,
    importKeyPairResponse_httpStatus,

    -- ** GetContainerServicePowers
    getContainerServicePowersResponse_powers,
    getContainerServicePowersResponse_httpStatus,

    -- ** DeleteDiskSnapshot
    deleteDiskSnapshot_diskSnapshotName,
    deleteDiskSnapshotResponse_operations,
    deleteDiskSnapshotResponse_httpStatus,

    -- ** GetCertificates
    getCertificates_includeCertificateDetails,
    getCertificates_certificateStatuses,
    getCertificates_certificateName,
    getCertificatesResponse_certificates,
    getCertificatesResponse_httpStatus,

    -- ** ReleaseStaticIp
    releaseStaticIp_staticIpName,
    releaseStaticIpResponse_operations,
    releaseStaticIpResponse_httpStatus,

    -- ** UpdateRelationalDatabaseParameters
    updateRelationalDatabaseParameters_relationalDatabaseName,
    updateRelationalDatabaseParameters_parameters,
    updateRelationalDatabaseParametersResponse_operations,
    updateRelationalDatabaseParametersResponse_httpStatus,

    -- ** DeleteLoadBalancerTlsCertificate
    deleteLoadBalancerTlsCertificate_force,
    deleteLoadBalancerTlsCertificate_loadBalancerName,
    deleteLoadBalancerTlsCertificate_certificateName,
    deleteLoadBalancerTlsCertificateResponse_operations,
    deleteLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** UpdateDomainEntry
    updateDomainEntry_domainName,
    updateDomainEntry_domainEntry,
    updateDomainEntryResponse_operations,
    updateDomainEntryResponse_httpStatus,

    -- ** GetContainerLog
    getContainerLog_pageToken,
    getContainerLog_filterPattern,
    getContainerLog_startTime,
    getContainerLog_endTime,
    getContainerLog_serviceName,
    getContainerLog_containerName,
    getContainerLogResponse_logEvents,
    getContainerLogResponse_nextPageToken,
    getContainerLogResponse_httpStatus,

    -- ** DeleteDomainEntry
    deleteDomainEntry_domainName,
    deleteDomainEntry_domainEntry,
    deleteDomainEntryResponse_operation,
    deleteDomainEntryResponse_httpStatus,

    -- ** GetContainerImages
    getContainerImages_serviceName,
    getContainerImagesResponse_containerImages,
    getContainerImagesResponse_httpStatus,

    -- ** GetDomains
    getDomains_pageToken,
    getDomainsResponse_domains,
    getDomainsResponse_nextPageToken,
    getDomainsResponse_httpStatus,

    -- ** PutAlarm
    putAlarm_datapointsToAlarm,
    putAlarm_notificationTriggers,
    putAlarm_notificationEnabled,
    putAlarm_treatMissingData,
    putAlarm_contactProtocols,
    putAlarm_alarmName,
    putAlarm_metricName,
    putAlarm_monitoredResourceName,
    putAlarm_comparisonOperator,
    putAlarm_threshold,
    putAlarm_evaluationPeriods,
    putAlarmResponse_operations,
    putAlarmResponse_httpStatus,

    -- ** DeleteAutoSnapshot
    deleteAutoSnapshot_resourceName,
    deleteAutoSnapshot_date,
    deleteAutoSnapshotResponse_operations,
    deleteAutoSnapshotResponse_httpStatus,

    -- ** GetContactMethods
    getContactMethods_protocols,
    getContactMethodsResponse_contactMethods,
    getContactMethodsResponse_httpStatus,

    -- ** GetRelationalDatabaseParameters
    getRelationalDatabaseParameters_pageToken,
    getRelationalDatabaseParameters_relationalDatabaseName,
    getRelationalDatabaseParametersResponse_nextPageToken,
    getRelationalDatabaseParametersResponse_parameters,
    getRelationalDatabaseParametersResponse_httpStatus,

    -- ** CreateRelationalDatabase
    createRelationalDatabase_preferredBackupWindow,
    createRelationalDatabase_masterUserPassword,
    createRelationalDatabase_publiclyAccessible,
    createRelationalDatabase_availabilityZone,
    createRelationalDatabase_preferredMaintenanceWindow,
    createRelationalDatabase_tags,
    createRelationalDatabase_relationalDatabaseName,
    createRelationalDatabase_relationalDatabaseBlueprintId,
    createRelationalDatabase_relationalDatabaseBundleId,
    createRelationalDatabase_masterDatabaseName,
    createRelationalDatabase_masterUsername,
    createRelationalDatabaseResponse_operations,
    createRelationalDatabaseResponse_httpStatus,

    -- * Types

    -- ** AddOn
    addOn_snapshotTimeOfDay,
    addOn_status,
    addOn_name,
    addOn_nextSnapshotTimeOfDay,

    -- ** AddOnRequest
    addOnRequest_autoSnapshotAddOnRequest,
    addOnRequest_addOnType,

    -- ** Alarm
    alarm_datapointsToAlarm,
    alarm_threshold,
    alarm_comparisonOperator,
    alarm_monitoredResourceInfo,
    alarm_notificationTriggers,
    alarm_unit,
    alarm_metricName,
    alarm_notificationEnabled,
    alarm_createdAt,
    alarm_arn,
    alarm_treatMissingData,
    alarm_resourceType,
    alarm_supportCode,
    alarm_state,
    alarm_name,
    alarm_statistic,
    alarm_evaluationPeriods,
    alarm_period,
    alarm_location,
    alarm_contactProtocols,

    -- ** AttachedDisk
    attachedDisk_sizeInGb,
    attachedDisk_path,

    -- ** AutoSnapshotAddOnRequest
    autoSnapshotAddOnRequest_snapshotTimeOfDay,

    -- ** AutoSnapshotDetails
    autoSnapshotDetails_status,
    autoSnapshotDetails_createdAt,
    autoSnapshotDetails_date,
    autoSnapshotDetails_fromAttachedDisks,

    -- ** AvailabilityZone
    availabilityZone_zoneName,
    availabilityZone_state,

    -- ** Blueprint
    blueprint_platform,
    blueprint_isActive,
    blueprint_licenseUrl,
    blueprint_productUrl,
    blueprint_version,
    blueprint_blueprintId,
    blueprint_versionCode,
    blueprint_name,
    blueprint_group,
    blueprint_description,
    blueprint_type,
    blueprint_minPower,

    -- ** Bundle
    bundle_power,
    bundle_instanceType,
    bundle_ramSizeInGb,
    bundle_bundleId,
    bundle_isActive,
    bundle_name,
    bundle_transferPerMonthInGb,
    bundle_cpuCount,
    bundle_price,
    bundle_supportedPlatforms,
    bundle_diskSizeInGb,

    -- ** CacheBehavior
    cacheBehavior_behavior,

    -- ** CacheBehaviorPerPath
    cacheBehaviorPerPath_behavior,
    cacheBehaviorPerPath_path,

    -- ** CacheSettings
    cacheSettings_maximumTTL,
    cacheSettings_forwardedHeaders,
    cacheSettings_defaultTTL,
    cacheSettings_cachedHTTPMethods,
    cacheSettings_allowedHTTPMethods,
    cacheSettings_forwardedQueryStrings,
    cacheSettings_forwardedCookies,
    cacheSettings_minimumTTL,

    -- ** Certificate
    certificate_status,
    certificate_notBefore,
    certificate_inUseResourceCount,
    certificate_createdAt,
    certificate_arn,
    certificate_eligibleToRenew,
    certificate_supportCode,
    certificate_subjectAlternativeNames,
    certificate_name,
    certificate_domainName,
    certificate_revocationReason,
    certificate_revokedAt,
    certificate_issuerCA,
    certificate_notAfter,
    certificate_tags,
    certificate_serialNumber,
    certificate_issuedAt,
    certificate_keyAlgorithm,
    certificate_domainValidationRecords,
    certificate_requestFailureReason,
    certificate_renewalSummary,

    -- ** CertificateSummary
    certificateSummary_certificateArn,
    certificateSummary_domainName,
    certificateSummary_certificateDetail,
    certificateSummary_tags,
    certificateSummary_certificateName,

    -- ** CloudFormationStackRecord
    cloudFormationStackRecord_createdAt,
    cloudFormationStackRecord_arn,
    cloudFormationStackRecord_resourceType,
    cloudFormationStackRecord_state,
    cloudFormationStackRecord_name,
    cloudFormationStackRecord_sourceInfo,
    cloudFormationStackRecord_location,
    cloudFormationStackRecord_destinationInfo,

    -- ** CloudFormationStackRecordSourceInfo
    cloudFormationStackRecordSourceInfo_arn,
    cloudFormationStackRecordSourceInfo_resourceType,
    cloudFormationStackRecordSourceInfo_name,

    -- ** ContactMethod
    contactMethod_status,
    contactMethod_contactEndpoint,
    contactMethod_createdAt,
    contactMethod_arn,
    contactMethod_resourceType,
    contactMethod_supportCode,
    contactMethod_name,
    contactMethod_protocol,
    contactMethod_location,

    -- ** Container
    container_environment,
    container_ports,
    container_image,
    container_command,

    -- ** ContainerImage
    containerImage_createdAt,
    containerImage_image,
    containerImage_digest,

    -- ** ContainerService
    containerService_power,
    containerService_currentDeployment,
    containerService_createdAt,
    containerService_arn,
    containerService_containerServiceName,
    containerService_privateDomainName,
    containerService_resourceType,
    containerService_state,
    containerService_principalArn,
    containerService_tags,
    containerService_nextDeployment,
    containerService_url,
    containerService_scale,
    containerService_publicDomainNames,
    containerService_location,
    containerService_powerId,
    containerService_isDisabled,

    -- ** ContainerServiceDeployment
    containerServiceDeployment_createdAt,
    containerServiceDeployment_version,
    containerServiceDeployment_publicEndpoint,
    containerServiceDeployment_state,
    containerServiceDeployment_containers,

    -- ** ContainerServiceDeploymentRequest
    containerServiceDeploymentRequest_publicEndpoint,
    containerServiceDeploymentRequest_containers,

    -- ** ContainerServiceEndpoint
    containerServiceEndpoint_containerPort,
    containerServiceEndpoint_containerName,
    containerServiceEndpoint_healthCheck,

    -- ** ContainerServiceHealthCheckConfig
    containerServiceHealthCheckConfig_intervalSeconds,
    containerServiceHealthCheckConfig_healthyThreshold,
    containerServiceHealthCheckConfig_unhealthyThreshold,
    containerServiceHealthCheckConfig_timeoutSeconds,
    containerServiceHealthCheckConfig_path,
    containerServiceHealthCheckConfig_successCodes,

    -- ** ContainerServiceLogEvent
    containerServiceLogEvent_message,
    containerServiceLogEvent_createdAt,

    -- ** ContainerServicePower
    containerServicePower_ramSizeInGb,
    containerServicePower_isActive,
    containerServicePower_name,
    containerServicePower_cpuCount,
    containerServicePower_price,
    containerServicePower_powerId,

    -- ** ContainerServiceRegistryLogin
    containerServiceRegistryLogin_expiresAt,
    containerServiceRegistryLogin_registry,
    containerServiceRegistryLogin_password,
    containerServiceRegistryLogin_username,

    -- ** CookieObject
    cookieObject_cookiesAllowList,
    cookieObject_option,

    -- ** DestinationInfo
    destinationInfo_id,
    destinationInfo_service,

    -- ** Disk
    disk_gbInUse,
    disk_attachmentState,
    disk_addOns,
    disk_isAttached,
    disk_createdAt,
    disk_arn,
    disk_resourceType,
    disk_supportCode,
    disk_sizeInGb,
    disk_state,
    disk_name,
    disk_attachedTo,
    disk_tags,
    disk_iops,
    disk_location,
    disk_path,
    disk_isSystemDisk,

    -- ** DiskInfo
    diskInfo_sizeInGb,
    diskInfo_name,
    diskInfo_path,
    diskInfo_isSystemDisk,

    -- ** DiskMap
    diskMap_originalDiskPath,
    diskMap_newDiskName,

    -- ** DiskSnapshot
    diskSnapshot_isFromAutoSnapshot,
    diskSnapshot_fromDiskName,
    diskSnapshot_createdAt,
    diskSnapshot_arn,
    diskSnapshot_fromInstanceArn,
    diskSnapshot_resourceType,
    diskSnapshot_supportCode,
    diskSnapshot_sizeInGb,
    diskSnapshot_state,
    diskSnapshot_name,
    diskSnapshot_tags,
    diskSnapshot_fromDiskArn,
    diskSnapshot_fromInstanceName,
    diskSnapshot_location,
    diskSnapshot_progress,

    -- ** DiskSnapshotInfo
    diskSnapshotInfo_sizeInGb,

    -- ** DistributionBundle
    distributionBundle_bundleId,
    distributionBundle_isActive,
    distributionBundle_name,
    distributionBundle_transferPerMonthInGb,
    distributionBundle_price,

    -- ** Domain
    domain_createdAt,
    domain_arn,
    domain_resourceType,
    domain_supportCode,
    domain_name,
    domain_tags,
    domain_location,
    domain_domainEntries,

    -- ** DomainEntry
    domainEntry_options,
    domainEntry_id,
    domainEntry_name,
    domainEntry_isAlias,
    domainEntry_target,
    domainEntry_type,

    -- ** DomainValidationRecord
    domainValidationRecord_resourceRecord,
    domainValidationRecord_domainName,

    -- ** EndpointRequest
    endpointRequest_healthCheck,
    endpointRequest_containerName,
    endpointRequest_containerPort,

    -- ** ExportSnapshotRecord
    exportSnapshotRecord_createdAt,
    exportSnapshotRecord_arn,
    exportSnapshotRecord_resourceType,
    exportSnapshotRecord_state,
    exportSnapshotRecord_name,
    exportSnapshotRecord_sourceInfo,
    exportSnapshotRecord_location,
    exportSnapshotRecord_destinationInfo,

    -- ** ExportSnapshotRecordSourceInfo
    exportSnapshotRecordSourceInfo_diskSnapshotInfo,
    exportSnapshotRecordSourceInfo_createdAt,
    exportSnapshotRecordSourceInfo_arn,
    exportSnapshotRecordSourceInfo_resourceType,
    exportSnapshotRecordSourceInfo_name,
    exportSnapshotRecordSourceInfo_fromResourceArn,
    exportSnapshotRecordSourceInfo_instanceSnapshotInfo,
    exportSnapshotRecordSourceInfo_fromResourceName,

    -- ** HeaderObject
    headerObject_headersAllowList,
    headerObject_option,

    -- ** HostKeyAttributes
    hostKeyAttributes_algorithm,
    hostKeyAttributes_publicKey,
    hostKeyAttributes_fingerprintSHA256,
    hostKeyAttributes_notValidBefore,
    hostKeyAttributes_notValidAfter,
    hostKeyAttributes_fingerprintSHA1,
    hostKeyAttributes_witnessedAt,

    -- ** InputOrigin
    inputOrigin_regionName,
    inputOrigin_protocolPolicy,
    inputOrigin_name,

    -- ** Instance
    instance_ipAddressType,
    instance_ipv6Addresses,
    instance_bundleId,
    instance_hardware,
    instance_addOns,
    instance_blueprintName,
    instance_sshKeyName,
    instance_createdAt,
    instance_arn,
    instance_blueprintId,
    instance_resourceType,
    instance_supportCode,
    instance_state,
    instance_name,
    instance_tags,
    instance_networking,
    instance_username,
    instance_publicIpAddress,
    instance_isStaticIp,
    instance_location,
    instance_privateIpAddress,

    -- ** InstanceAccessDetails
    instanceAccessDetails_hostKeys,
    instanceAccessDetails_instanceName,
    instanceAccessDetails_expiresAt,
    instanceAccessDetails_privateKey,
    instanceAccessDetails_certKey,
    instanceAccessDetails_passwordData,
    instanceAccessDetails_ipAddress,
    instanceAccessDetails_password,
    instanceAccessDetails_username,
    instanceAccessDetails_protocol,

    -- ** InstanceEntry
    instanceEntry_userData,
    instanceEntry_sourceName,
    instanceEntry_instanceType,
    instanceEntry_portInfoSource,
    instanceEntry_availabilityZone,

    -- ** InstanceHardware
    instanceHardware_ramSizeInGb,
    instanceHardware_disks,
    instanceHardware_cpuCount,

    -- ** InstanceHealthSummary
    instanceHealthSummary_instanceName,
    instanceHealthSummary_instanceHealthReason,
    instanceHealthSummary_instanceHealth,

    -- ** InstanceNetworking
    instanceNetworking_monthlyTransfer,
    instanceNetworking_ports,

    -- ** InstancePortInfo
    instancePortInfo_fromPort,
    instancePortInfo_cidrListAliases,
    instancePortInfo_ipv6Cidrs,
    instancePortInfo_commonName,
    instancePortInfo_cidrs,
    instancePortInfo_accessType,
    instancePortInfo_accessDirection,
    instancePortInfo_protocol,
    instancePortInfo_toPort,
    instancePortInfo_accessFrom,

    -- ** InstancePortState
    instancePortState_fromPort,
    instancePortState_cidrListAliases,
    instancePortState_ipv6Cidrs,
    instancePortState_state,
    instancePortState_cidrs,
    instancePortState_protocol,
    instancePortState_toPort,

    -- ** InstanceSnapshot
    instanceSnapshot_isFromAutoSnapshot,
    instanceSnapshot_createdAt,
    instanceSnapshot_arn,
    instanceSnapshot_fromInstanceArn,
    instanceSnapshot_resourceType,
    instanceSnapshot_supportCode,
    instanceSnapshot_sizeInGb,
    instanceSnapshot_fromBundleId,
    instanceSnapshot_state,
    instanceSnapshot_name,
    instanceSnapshot_fromBlueprintId,
    instanceSnapshot_tags,
    instanceSnapshot_fromInstanceName,
    instanceSnapshot_location,
    instanceSnapshot_progress,
    instanceSnapshot_fromAttachedDisks,

    -- ** InstanceSnapshotInfo
    instanceSnapshotInfo_fromDiskInfo,
    instanceSnapshotInfo_fromBundleId,
    instanceSnapshotInfo_fromBlueprintId,

    -- ** InstanceState
    instanceState_code,
    instanceState_name,

    -- ** KeyPair
    keyPair_createdAt,
    keyPair_arn,
    keyPair_resourceType,
    keyPair_supportCode,
    keyPair_name,
    keyPair_tags,
    keyPair_fingerprint,
    keyPair_location,

    -- ** LightsailDistribution
    lightsailDistribution_isEnabled,
    lightsailDistribution_ipAddressType,
    lightsailDistribution_origin,
    lightsailDistribution_status,
    lightsailDistribution_originPublicDNS,
    lightsailDistribution_bundleId,
    lightsailDistribution_alternativeDomainNames,
    lightsailDistribution_createdAt,
    lightsailDistribution_cacheBehaviorSettings,
    lightsailDistribution_arn,
    lightsailDistribution_resourceType,
    lightsailDistribution_supportCode,
    lightsailDistribution_name,
    lightsailDistribution_domainName,
    lightsailDistribution_ableToUpdateBundle,
    lightsailDistribution_tags,
    lightsailDistribution_cacheBehaviors,
    lightsailDistribution_defaultCacheBehavior,
    lightsailDistribution_location,
    lightsailDistribution_certificateName,

    -- ** LoadBalancer
    loadBalancer_ipAddressType,
    loadBalancer_tlsCertificateSummaries,
    loadBalancer_instanceHealthSummary,
    loadBalancer_publicPorts,
    loadBalancer_configurationOptions,
    loadBalancer_instancePort,
    loadBalancer_createdAt,
    loadBalancer_arn,
    loadBalancer_resourceType,
    loadBalancer_supportCode,
    loadBalancer_state,
    loadBalancer_name,
    loadBalancer_healthCheckPath,
    loadBalancer_tags,
    loadBalancer_dnsName,
    loadBalancer_protocol,
    loadBalancer_location,

    -- ** LoadBalancerTlsCertificate
    loadBalancerTlsCertificate_status,
    loadBalancerTlsCertificate_notBefore,
    loadBalancerTlsCertificate_serial,
    loadBalancerTlsCertificate_isAttached,
    loadBalancerTlsCertificate_createdAt,
    loadBalancerTlsCertificate_arn,
    loadBalancerTlsCertificate_resourceType,
    loadBalancerTlsCertificate_supportCode,
    loadBalancerTlsCertificate_subjectAlternativeNames,
    loadBalancerTlsCertificate_name,
    loadBalancerTlsCertificate_domainName,
    loadBalancerTlsCertificate_subject,
    loadBalancerTlsCertificate_failureReason,
    loadBalancerTlsCertificate_revocationReason,
    loadBalancerTlsCertificate_revokedAt,
    loadBalancerTlsCertificate_notAfter,
    loadBalancerTlsCertificate_tags,
    loadBalancerTlsCertificate_signatureAlgorithm,
    loadBalancerTlsCertificate_issuer,
    loadBalancerTlsCertificate_issuedAt,
    loadBalancerTlsCertificate_keyAlgorithm,
    loadBalancerTlsCertificate_domainValidationRecords,
    loadBalancerTlsCertificate_location,
    loadBalancerTlsCertificate_loadBalancerName,
    loadBalancerTlsCertificate_renewalSummary,

    -- ** LoadBalancerTlsCertificateDomainValidationOption
    loadBalancerTlsCertificateDomainValidationOption_domainName,
    loadBalancerTlsCertificateDomainValidationOption_validationStatus,

    -- ** LoadBalancerTlsCertificateDomainValidationRecord
    loadBalancerTlsCertificateDomainValidationRecord_name,
    loadBalancerTlsCertificateDomainValidationRecord_domainName,
    loadBalancerTlsCertificateDomainValidationRecord_validationStatus,
    loadBalancerTlsCertificateDomainValidationRecord_value,
    loadBalancerTlsCertificateDomainValidationRecord_type,

    -- ** LoadBalancerTlsCertificateRenewalSummary
    loadBalancerTlsCertificateRenewalSummary_domainValidationOptions,
    loadBalancerTlsCertificateRenewalSummary_renewalStatus,

    -- ** LoadBalancerTlsCertificateSummary
    loadBalancerTlsCertificateSummary_isAttached,
    loadBalancerTlsCertificateSummary_name,

    -- ** LogEvent
    logEvent_message,
    logEvent_createdAt,

    -- ** MetricDatapoint
    metricDatapoint_minimum,
    metricDatapoint_unit,
    metricDatapoint_sum,
    metricDatapoint_sampleCount,
    metricDatapoint_timestamp,
    metricDatapoint_average,
    metricDatapoint_maximum,

    -- ** MonitoredResourceInfo
    monitoredResourceInfo_arn,
    monitoredResourceInfo_resourceType,
    monitoredResourceInfo_name,

    -- ** MonthlyTransfer
    monthlyTransfer_gbPerMonthAllocated,

    -- ** Operation
    operation_operationDetails,
    operation_status,
    operation_isTerminal,
    operation_createdAt,
    operation_id,
    operation_resourceType,
    operation_statusChangedAt,
    operation_location,
    operation_resourceName,
    operation_operationType,
    operation_errorCode,
    operation_errorDetails,

    -- ** Origin
    origin_regionName,
    origin_protocolPolicy,
    origin_resourceType,
    origin_name,

    -- ** PasswordData
    passwordData_keyPairName,
    passwordData_ciphertext,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_description,

    -- ** PendingModifiedRelationalDatabaseValues
    pendingModifiedRelationalDatabaseValues_masterUserPassword,
    pendingModifiedRelationalDatabaseValues_backupRetentionEnabled,
    pendingModifiedRelationalDatabaseValues_engineVersion,

    -- ** PortInfo
    portInfo_fromPort,
    portInfo_cidrListAliases,
    portInfo_ipv6Cidrs,
    portInfo_cidrs,
    portInfo_protocol,
    portInfo_toPort,

    -- ** QueryStringObject
    queryStringObject_queryStringsAllowList,
    queryStringObject_option,

    -- ** RegionInfo
    regionInfo_availabilityZones,
    regionInfo_continentCode,
    regionInfo_relationalDatabaseAvailabilityZones,
    regionInfo_name,
    regionInfo_description,
    regionInfo_displayName,

    -- ** RelationalDatabase
    relationalDatabase_relationalDatabaseBundleId,
    relationalDatabase_masterEndpoint,
    relationalDatabase_pendingMaintenanceActions,
    relationalDatabase_preferredBackupWindow,
    relationalDatabase_caCertificateIdentifier,
    relationalDatabase_relationalDatabaseBlueprintId,
    relationalDatabase_latestRestorableTime,
    relationalDatabase_masterDatabaseName,
    relationalDatabase_hardware,
    relationalDatabase_createdAt,
    relationalDatabase_arn,
    relationalDatabase_masterUsername,
    relationalDatabase_resourceType,
    relationalDatabase_publiclyAccessible,
    relationalDatabase_supportCode,
    relationalDatabase_state,
    relationalDatabase_name,
    relationalDatabase_backupRetentionEnabled,
    relationalDatabase_engineVersion,
    relationalDatabase_preferredMaintenanceWindow,
    relationalDatabase_tags,
    relationalDatabase_parameterApplyStatus,
    relationalDatabase_pendingModifiedValues,
    relationalDatabase_engine,
    relationalDatabase_secondaryAvailabilityZone,
    relationalDatabase_location,

    -- ** RelationalDatabaseBlueprint
    relationalDatabaseBlueprint_engineDescription,
    relationalDatabaseBlueprint_blueprintId,
    relationalDatabaseBlueprint_engineVersionDescription,
    relationalDatabaseBlueprint_isEngineDefault,
    relationalDatabaseBlueprint_engineVersion,
    relationalDatabaseBlueprint_engine,

    -- ** RelationalDatabaseBundle
    relationalDatabaseBundle_ramSizeInGb,
    relationalDatabaseBundle_bundleId,
    relationalDatabaseBundle_isActive,
    relationalDatabaseBundle_name,
    relationalDatabaseBundle_isEncrypted,
    relationalDatabaseBundle_transferPerMonthInGb,
    relationalDatabaseBundle_cpuCount,
    relationalDatabaseBundle_price,
    relationalDatabaseBundle_diskSizeInGb,

    -- ** RelationalDatabaseEndpoint
    relationalDatabaseEndpoint_address,
    relationalDatabaseEndpoint_port,

    -- ** RelationalDatabaseEvent
    relationalDatabaseEvent_message,
    relationalDatabaseEvent_createdAt,
    relationalDatabaseEvent_eventCategories,
    relationalDatabaseEvent_resource,

    -- ** RelationalDatabaseHardware
    relationalDatabaseHardware_ramSizeInGb,
    relationalDatabaseHardware_cpuCount,
    relationalDatabaseHardware_diskSizeInGb,

    -- ** RelationalDatabaseParameter
    relationalDatabaseParameter_allowedValues,
    relationalDatabaseParameter_parameterValue,
    relationalDatabaseParameter_applyType,
    relationalDatabaseParameter_parameterName,
    relationalDatabaseParameter_description,
    relationalDatabaseParameter_applyMethod,
    relationalDatabaseParameter_isModifiable,
    relationalDatabaseParameter_dataType,

    -- ** RelationalDatabaseSnapshot
    relationalDatabaseSnapshot_fromRelationalDatabaseName,
    relationalDatabaseSnapshot_createdAt,
    relationalDatabaseSnapshot_arn,
    relationalDatabaseSnapshot_resourceType,
    relationalDatabaseSnapshot_supportCode,
    relationalDatabaseSnapshot_sizeInGb,
    relationalDatabaseSnapshot_state,
    relationalDatabaseSnapshot_name,
    relationalDatabaseSnapshot_engineVersion,
    relationalDatabaseSnapshot_fromRelationalDatabaseBundleId,
    relationalDatabaseSnapshot_tags,
    relationalDatabaseSnapshot_engine,
    relationalDatabaseSnapshot_fromRelationalDatabaseArn,
    relationalDatabaseSnapshot_location,
    relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId,

    -- ** RenewalSummary
    renewalSummary_updatedAt,
    renewalSummary_renewalStatus,
    renewalSummary_renewalStatusReason,
    renewalSummary_domainValidationRecords,

    -- ** ResourceLocation
    resourceLocation_regionName,
    resourceLocation_availabilityZone,

    -- ** ResourceRecord
    resourceRecord_name,
    resourceRecord_value,
    resourceRecord_type,

    -- ** StaticIp
    staticIp_isAttached,
    staticIp_createdAt,
    staticIp_arn,
    staticIp_resourceType,
    staticIp_supportCode,
    staticIp_name,
    staticIp_ipAddress,
    staticIp_attachedTo,
    staticIp_location,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.Lightsail.AllocateStaticIp
import Network.AWS.Lightsail.AttachCertificateToDistribution
import Network.AWS.Lightsail.AttachDisk
import Network.AWS.Lightsail.AttachInstancesToLoadBalancer
import Network.AWS.Lightsail.AttachLoadBalancerTlsCertificate
import Network.AWS.Lightsail.AttachStaticIp
import Network.AWS.Lightsail.CloseInstancePublicPorts
import Network.AWS.Lightsail.CopySnapshot
import Network.AWS.Lightsail.CreateCertificate
import Network.AWS.Lightsail.CreateCloudFormationStack
import Network.AWS.Lightsail.CreateContactMethod
import Network.AWS.Lightsail.CreateContainerService
import Network.AWS.Lightsail.CreateContainerServiceDeployment
import Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
import Network.AWS.Lightsail.CreateDisk
import Network.AWS.Lightsail.CreateDiskFromSnapshot
import Network.AWS.Lightsail.CreateDiskSnapshot
import Network.AWS.Lightsail.CreateDistribution
import Network.AWS.Lightsail.CreateDomain
import Network.AWS.Lightsail.CreateDomainEntry
import Network.AWS.Lightsail.CreateInstanceSnapshot
import Network.AWS.Lightsail.CreateInstances
import Network.AWS.Lightsail.CreateInstancesFromSnapshot
import Network.AWS.Lightsail.CreateKeyPair
import Network.AWS.Lightsail.CreateLoadBalancer
import Network.AWS.Lightsail.CreateLoadBalancerTlsCertificate
import Network.AWS.Lightsail.CreateRelationalDatabase
import Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot
import Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot
import Network.AWS.Lightsail.DeleteAlarm
import Network.AWS.Lightsail.DeleteAutoSnapshot
import Network.AWS.Lightsail.DeleteCertificate
import Network.AWS.Lightsail.DeleteContactMethod
import Network.AWS.Lightsail.DeleteContainerImage
import Network.AWS.Lightsail.DeleteContainerService
import Network.AWS.Lightsail.DeleteDisk
import Network.AWS.Lightsail.DeleteDiskSnapshot
import Network.AWS.Lightsail.DeleteDistribution
import Network.AWS.Lightsail.DeleteDomain
import Network.AWS.Lightsail.DeleteDomainEntry
import Network.AWS.Lightsail.DeleteInstance
import Network.AWS.Lightsail.DeleteInstanceSnapshot
import Network.AWS.Lightsail.DeleteKeyPair
import Network.AWS.Lightsail.DeleteKnownHostKeys
import Network.AWS.Lightsail.DeleteLoadBalancer
import Network.AWS.Lightsail.DeleteLoadBalancerTlsCertificate
import Network.AWS.Lightsail.DeleteRelationalDatabase
import Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
import Network.AWS.Lightsail.DetachCertificateFromDistribution
import Network.AWS.Lightsail.DetachDisk
import Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
import Network.AWS.Lightsail.DetachStaticIp
import Network.AWS.Lightsail.DisableAddOn
import Network.AWS.Lightsail.DownloadDefaultKeyPair
import Network.AWS.Lightsail.EnableAddOn
import Network.AWS.Lightsail.ExportSnapshot
import Network.AWS.Lightsail.GetActiveNames
import Network.AWS.Lightsail.GetAlarms
import Network.AWS.Lightsail.GetAutoSnapshots
import Network.AWS.Lightsail.GetBlueprints
import Network.AWS.Lightsail.GetBundles
import Network.AWS.Lightsail.GetCertificates
import Network.AWS.Lightsail.GetCloudFormationStackRecords
import Network.AWS.Lightsail.GetContactMethods
import Network.AWS.Lightsail.GetContainerAPIMetadata
import Network.AWS.Lightsail.GetContainerImages
import Network.AWS.Lightsail.GetContainerLog
import Network.AWS.Lightsail.GetContainerServiceDeployments
import Network.AWS.Lightsail.GetContainerServiceMetricData
import Network.AWS.Lightsail.GetContainerServicePowers
import Network.AWS.Lightsail.GetContainerServices
import Network.AWS.Lightsail.GetDisk
import Network.AWS.Lightsail.GetDiskSnapshot
import Network.AWS.Lightsail.GetDiskSnapshots
import Network.AWS.Lightsail.GetDisks
import Network.AWS.Lightsail.GetDistributionBundles
import Network.AWS.Lightsail.GetDistributionLatestCacheReset
import Network.AWS.Lightsail.GetDistributionMetricData
import Network.AWS.Lightsail.GetDistributions
import Network.AWS.Lightsail.GetDomain
import Network.AWS.Lightsail.GetDomains
import Network.AWS.Lightsail.GetExportSnapshotRecords
import Network.AWS.Lightsail.GetInstance
import Network.AWS.Lightsail.GetInstanceAccessDetails
import Network.AWS.Lightsail.GetInstanceMetricData
import Network.AWS.Lightsail.GetInstancePortStates
import Network.AWS.Lightsail.GetInstanceSnapshot
import Network.AWS.Lightsail.GetInstanceSnapshots
import Network.AWS.Lightsail.GetInstanceState
import Network.AWS.Lightsail.GetInstances
import Network.AWS.Lightsail.GetKeyPair
import Network.AWS.Lightsail.GetKeyPairs
import Network.AWS.Lightsail.GetLoadBalancer
import Network.AWS.Lightsail.GetLoadBalancerMetricData
import Network.AWS.Lightsail.GetLoadBalancerTlsCertificates
import Network.AWS.Lightsail.GetLoadBalancers
import Network.AWS.Lightsail.GetOperation
import Network.AWS.Lightsail.GetOperations
import Network.AWS.Lightsail.GetOperationsForResource
import Network.AWS.Lightsail.GetRegions
import Network.AWS.Lightsail.GetRelationalDatabase
import Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
import Network.AWS.Lightsail.GetRelationalDatabaseBundles
import Network.AWS.Lightsail.GetRelationalDatabaseEvents
import Network.AWS.Lightsail.GetRelationalDatabaseLogEvents
import Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
import Network.AWS.Lightsail.GetRelationalDatabaseMasterUserPassword
import Network.AWS.Lightsail.GetRelationalDatabaseMetricData
import Network.AWS.Lightsail.GetRelationalDatabaseParameters
import Network.AWS.Lightsail.GetRelationalDatabaseSnapshot
import Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
import Network.AWS.Lightsail.GetRelationalDatabases
import Network.AWS.Lightsail.GetStaticIp
import Network.AWS.Lightsail.GetStaticIps
import Network.AWS.Lightsail.ImportKeyPair
import Network.AWS.Lightsail.IsVpcPeered
import Network.AWS.Lightsail.OpenInstancePublicPorts
import Network.AWS.Lightsail.PeerVpc
import Network.AWS.Lightsail.PutAlarm
import Network.AWS.Lightsail.PutInstancePublicPorts
import Network.AWS.Lightsail.RebootInstance
import Network.AWS.Lightsail.RebootRelationalDatabase
import Network.AWS.Lightsail.RegisterContainerImage
import Network.AWS.Lightsail.ReleaseStaticIp
import Network.AWS.Lightsail.ResetDistributionCache
import Network.AWS.Lightsail.SendContactMethodVerification
import Network.AWS.Lightsail.SetIpAddressType
import Network.AWS.Lightsail.StartInstance
import Network.AWS.Lightsail.StartRelationalDatabase
import Network.AWS.Lightsail.StopInstance
import Network.AWS.Lightsail.StopRelationalDatabase
import Network.AWS.Lightsail.TagResource
import Network.AWS.Lightsail.TestAlarm
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.AddOnRequest
import Network.AWS.Lightsail.Types.Alarm
import Network.AWS.Lightsail.Types.AttachedDisk
import Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
import Network.AWS.Lightsail.Types.AutoSnapshotDetails
import Network.AWS.Lightsail.Types.AvailabilityZone
import Network.AWS.Lightsail.Types.Blueprint
import Network.AWS.Lightsail.Types.Bundle
import Network.AWS.Lightsail.Types.CacheBehavior
import Network.AWS.Lightsail.Types.CacheBehaviorPerPath
import Network.AWS.Lightsail.Types.CacheSettings
import Network.AWS.Lightsail.Types.Certificate
import Network.AWS.Lightsail.Types.CertificateSummary
import Network.AWS.Lightsail.Types.CloudFormationStackRecord
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Network.AWS.Lightsail.Types.ContactMethod
import Network.AWS.Lightsail.Types.Container
import Network.AWS.Lightsail.Types.ContainerImage
import Network.AWS.Lightsail.Types.ContainerService
import Network.AWS.Lightsail.Types.ContainerServiceDeployment
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
import Network.AWS.Lightsail.Types.ContainerServiceEndpoint
import Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
import Network.AWS.Lightsail.Types.ContainerServiceLogEvent
import Network.AWS.Lightsail.Types.ContainerServicePower
import Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin
import Network.AWS.Lightsail.Types.CookieObject
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.Disk
import Network.AWS.Lightsail.Types.DiskInfo
import Network.AWS.Lightsail.Types.DiskMap
import Network.AWS.Lightsail.Types.DiskSnapshot
import Network.AWS.Lightsail.Types.DiskSnapshotInfo
import Network.AWS.Lightsail.Types.DistributionBundle
import Network.AWS.Lightsail.Types.Domain
import Network.AWS.Lightsail.Types.DomainEntry
import Network.AWS.Lightsail.Types.DomainValidationRecord
import Network.AWS.Lightsail.Types.EndpointRequest
import Network.AWS.Lightsail.Types.ExportSnapshotRecord
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Network.AWS.Lightsail.Types.HeaderObject
import Network.AWS.Lightsail.Types.HostKeyAttributes
import Network.AWS.Lightsail.Types.InputOrigin
import Network.AWS.Lightsail.Types.Instance
import Network.AWS.Lightsail.Types.InstanceAccessDetails
import Network.AWS.Lightsail.Types.InstanceEntry
import Network.AWS.Lightsail.Types.InstanceHardware
import Network.AWS.Lightsail.Types.InstanceHealthSummary
import Network.AWS.Lightsail.Types.InstanceNetworking
import Network.AWS.Lightsail.Types.InstancePortInfo
import Network.AWS.Lightsail.Types.InstancePortState
import Network.AWS.Lightsail.Types.InstanceSnapshot
import Network.AWS.Lightsail.Types.InstanceSnapshotInfo
import Network.AWS.Lightsail.Types.InstanceState
import Network.AWS.Lightsail.Types.KeyPair
import Network.AWS.Lightsail.Types.LightsailDistribution
import Network.AWS.Lightsail.Types.LoadBalancer
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificate
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateSummary
import Network.AWS.Lightsail.Types.LogEvent
import Network.AWS.Lightsail.Types.MetricDatapoint
import Network.AWS.Lightsail.Types.MonitoredResourceInfo
import Network.AWS.Lightsail.Types.MonthlyTransfer
import Network.AWS.Lightsail.Types.Operation
import Network.AWS.Lightsail.Types.Origin
import Network.AWS.Lightsail.Types.PasswordData
import Network.AWS.Lightsail.Types.PendingMaintenanceAction
import Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Network.AWS.Lightsail.Types.PortInfo
import Network.AWS.Lightsail.Types.QueryStringObject
import Network.AWS.Lightsail.Types.RegionInfo
import Network.AWS.Lightsail.Types.RelationalDatabase
import Network.AWS.Lightsail.Types.RelationalDatabaseBlueprint
import Network.AWS.Lightsail.Types.RelationalDatabaseBundle
import Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
import Network.AWS.Lightsail.Types.RelationalDatabaseEvent
import Network.AWS.Lightsail.Types.RelationalDatabaseHardware
import Network.AWS.Lightsail.Types.RelationalDatabaseParameter
import Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
import Network.AWS.Lightsail.Types.RenewalSummary
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceRecord
import Network.AWS.Lightsail.Types.StaticIp
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Lightsail.UnpeerVpc
import Network.AWS.Lightsail.UntagResource
import Network.AWS.Lightsail.UpdateContainerService
import Network.AWS.Lightsail.UpdateDistribution
import Network.AWS.Lightsail.UpdateDistributionBundle
import Network.AWS.Lightsail.UpdateDomainEntry
import Network.AWS.Lightsail.UpdateLoadBalancerAttribute
import Network.AWS.Lightsail.UpdateRelationalDatabase
import Network.AWS.Lightsail.UpdateRelationalDatabaseParameters
