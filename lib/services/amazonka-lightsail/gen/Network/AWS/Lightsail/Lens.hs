{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Lens
  ( -- * Operations

    -- ** CloseInstancePublicPorts
    closeInstancePublicPorts_portInfo,
    closeInstancePublicPorts_instanceName,
    closeInstancePublicPortsResponse_operation,
    closeInstancePublicPortsResponse_httpStatus,

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

    -- ** DeleteBucketAccessKey
    deleteBucketAccessKey_bucketName,
    deleteBucketAccessKey_accessKeyId,
    deleteBucketAccessKeyResponse_operations,
    deleteBucketAccessKeyResponse_httpStatus,

    -- ** AllocateStaticIp
    allocateStaticIp_staticIpName,
    allocateStaticIpResponse_operations,
    allocateStaticIpResponse_httpStatus,

    -- ** DeleteKeyPair
    deleteKeyPair_keyPairName,
    deleteKeyPairResponse_operation,
    deleteKeyPairResponse_httpStatus,

    -- ** DeleteInstanceSnapshot
    deleteInstanceSnapshot_instanceSnapshotName,
    deleteInstanceSnapshotResponse_operations,
    deleteInstanceSnapshotResponse_httpStatus,

    -- ** GetInstances
    getInstances_pageToken,
    getInstancesResponse_nextPageToken,
    getInstancesResponse_instances,
    getInstancesResponse_httpStatus,

    -- ** GetLoadBalancer
    getLoadBalancer_loadBalancerName,
    getLoadBalancerResponse_loadBalancer,
    getLoadBalancerResponse_httpStatus,

    -- ** DisableAddOn
    disableAddOn_addOnType,
    disableAddOn_resourceName,
    disableAddOnResponse_operations,
    disableAddOnResponse_httpStatus,

    -- ** GetDistributions
    getDistributions_distributionName,
    getDistributions_pageToken,
    getDistributionsResponse_nextPageToken,
    getDistributionsResponse_distributions,
    getDistributionsResponse_httpStatus,

    -- ** CreateContainerServiceDeployment
    createContainerServiceDeployment_publicEndpoint,
    createContainerServiceDeployment_containers,
    createContainerServiceDeployment_serviceName,
    createContainerServiceDeploymentResponse_containerService,
    createContainerServiceDeploymentResponse_httpStatus,

    -- ** GetInstance
    getInstance_instanceName,
    getInstanceResponse_instance,
    getInstanceResponse_httpStatus,

    -- ** DeleteBucket
    deleteBucket_forceDelete,
    deleteBucket_bucketName,
    deleteBucketResponse_operations,
    deleteBucketResponse_httpStatus,

    -- ** UpdateBucket
    updateBucket_readonlyAccessAccounts,
    updateBucket_accessRules,
    updateBucket_versioning,
    updateBucket_bucketName,
    updateBucketResponse_bucket,
    updateBucketResponse_operations,
    updateBucketResponse_httpStatus,

    -- ** GetRelationalDatabaseEvents
    getRelationalDatabaseEvents_durationInMinutes,
    getRelationalDatabaseEvents_pageToken,
    getRelationalDatabaseEvents_relationalDatabaseName,
    getRelationalDatabaseEventsResponse_nextPageToken,
    getRelationalDatabaseEventsResponse_relationalDatabaseEvents,
    getRelationalDatabaseEventsResponse_httpStatus,

    -- ** AttachCertificateToDistribution
    attachCertificateToDistribution_distributionName,
    attachCertificateToDistribution_certificateName,
    attachCertificateToDistributionResponse_operation,
    attachCertificateToDistributionResponse_httpStatus,

    -- ** GetContainerServices
    getContainerServices_serviceName,
    getContainerServicesResponse_containerServices,
    getContainerServicesResponse_httpStatus,

    -- ** UpdateDistributionBundle
    updateDistributionBundle_bundleId,
    updateDistributionBundle_distributionName,
    updateDistributionBundleResponse_operation,
    updateDistributionBundleResponse_httpStatus,

    -- ** GetRelationalDatabaseSnapshots
    getRelationalDatabaseSnapshots_pageToken,
    getRelationalDatabaseSnapshotsResponse_nextPageToken,
    getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots,
    getRelationalDatabaseSnapshotsResponse_httpStatus,

    -- ** GetBucketBundles
    getBucketBundles_includeInactive,
    getBucketBundlesResponse_bundles,
    getBucketBundlesResponse_httpStatus,

    -- ** CreateBucket
    createBucket_enableObjectVersioning,
    createBucket_tags,
    createBucket_bucketName,
    createBucket_bundleId,
    createBucketResponse_bucket,
    createBucketResponse_operations,
    createBucketResponse_httpStatus,

    -- ** AttachStaticIp
    attachStaticIp_staticIpName,
    attachStaticIp_instanceName,
    attachStaticIpResponse_operations,
    attachStaticIpResponse_httpStatus,

    -- ** GetRelationalDatabaseParameters
    getRelationalDatabaseParameters_pageToken,
    getRelationalDatabaseParameters_relationalDatabaseName,
    getRelationalDatabaseParametersResponse_nextPageToken,
    getRelationalDatabaseParametersResponse_parameters,
    getRelationalDatabaseParametersResponse_httpStatus,

    -- ** DetachDisk
    detachDisk_diskName,
    detachDiskResponse_operations,
    detachDiskResponse_httpStatus,

    -- ** GetContactMethods
    getContactMethods_protocols,
    getContactMethodsResponse_contactMethods,
    getContactMethodsResponse_httpStatus,

    -- ** DownloadDefaultKeyPair
    downloadDefaultKeyPairResponse_publicKeyBase64,
    downloadDefaultKeyPairResponse_privateKeyBase64,
    downloadDefaultKeyPairResponse_httpStatus,

    -- ** DeleteLoadBalancerTlsCertificate
    deleteLoadBalancerTlsCertificate_force,
    deleteLoadBalancerTlsCertificate_loadBalancerName,
    deleteLoadBalancerTlsCertificate_certificateName,
    deleteLoadBalancerTlsCertificateResponse_operations,
    deleteLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** TestAlarm
    testAlarm_alarmName,
    testAlarm_state,
    testAlarmResponse_operations,
    testAlarmResponse_httpStatus,

    -- ** GetDomains
    getDomains_pageToken,
    getDomainsResponse_nextPageToken,
    getDomainsResponse_domains,
    getDomainsResponse_httpStatus,

    -- ** GetContainerImages
    getContainerImages_serviceName,
    getContainerImagesResponse_containerImages,
    getContainerImagesResponse_httpStatus,

    -- ** UpdateRelationalDatabaseParameters
    updateRelationalDatabaseParameters_relationalDatabaseName,
    updateRelationalDatabaseParameters_parameters,
    updateRelationalDatabaseParametersResponse_operations,
    updateRelationalDatabaseParametersResponse_httpStatus,

    -- ** CreateLoadBalancerTlsCertificate
    createLoadBalancerTlsCertificate_certificateAlternativeNames,
    createLoadBalancerTlsCertificate_tags,
    createLoadBalancerTlsCertificate_loadBalancerName,
    createLoadBalancerTlsCertificate_certificateName,
    createLoadBalancerTlsCertificate_certificateDomainName,
    createLoadBalancerTlsCertificateResponse_operations,
    createLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** CreateDomainEntry
    createDomainEntry_domainName,
    createDomainEntry_domainEntry,
    createDomainEntryResponse_operation,
    createDomainEntryResponse_httpStatus,

    -- ** GetContainerServicePowers
    getContainerServicePowersResponse_powers,
    getContainerServicePowersResponse_httpStatus,

    -- ** ImportKeyPair
    importKeyPair_keyPairName,
    importKeyPair_publicKeyBase64,
    importKeyPairResponse_operation,
    importKeyPairResponse_httpStatus,

    -- ** GetInstanceSnapshots
    getInstanceSnapshots_pageToken,
    getInstanceSnapshotsResponse_nextPageToken,
    getInstanceSnapshotsResponse_instanceSnapshots,
    getInstanceSnapshotsResponse_httpStatus,

    -- ** ExportSnapshot
    exportSnapshot_sourceSnapshotName,
    exportSnapshotResponse_operations,
    exportSnapshotResponse_httpStatus,

    -- ** CreateRelationalDatabaseFromSnapshot
    createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName,
    createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId,
    createRelationalDatabaseFromSnapshot_publiclyAccessible,
    createRelationalDatabaseFromSnapshot_useLatestRestorableTime,
    createRelationalDatabaseFromSnapshot_restoreTime,
    createRelationalDatabaseFromSnapshot_availabilityZone,
    createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName,
    createRelationalDatabaseFromSnapshot_tags,
    createRelationalDatabaseFromSnapshot_relationalDatabaseName,
    createRelationalDatabaseFromSnapshotResponse_operations,
    createRelationalDatabaseFromSnapshotResponse_httpStatus,

    -- ** CreateCloudFormationStack
    createCloudFormationStack_instances,
    createCloudFormationStackResponse_operations,
    createCloudFormationStackResponse_httpStatus,

    -- ** GetExportSnapshotRecords
    getExportSnapshotRecords_pageToken,
    getExportSnapshotRecordsResponse_nextPageToken,
    getExportSnapshotRecordsResponse_exportSnapshotRecords,
    getExportSnapshotRecordsResponse_httpStatus,

    -- ** ReleaseStaticIp
    releaseStaticIp_staticIpName,
    releaseStaticIpResponse_operations,
    releaseStaticIpResponse_httpStatus,

    -- ** DeleteInstance
    deleteInstance_forceDeleteAddOns,
    deleteInstance_instanceName,
    deleteInstanceResponse_operations,
    deleteInstanceResponse_httpStatus,

    -- ** CreateContainerServiceRegistryLogin
    createContainerServiceRegistryLoginResponse_registryLogin,
    createContainerServiceRegistryLoginResponse_httpStatus,

    -- ** GetCertificates
    getCertificates_certificateStatuses,
    getCertificates_certificateName,
    getCertificates_includeCertificateDetails,
    getCertificatesResponse_certificates,
    getCertificatesResponse_httpStatus,

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

    -- ** RebootInstance
    rebootInstance_instanceName,
    rebootInstanceResponse_operations,
    rebootInstanceResponse_httpStatus,

    -- ** DeleteLoadBalancer
    deleteLoadBalancer_loadBalancerName,
    deleteLoadBalancerResponse_operations,
    deleteLoadBalancerResponse_httpStatus,

    -- ** CreateDiskFromSnapshot
    createDiskFromSnapshot_useLatestRestorableAutoSnapshot,
    createDiskFromSnapshot_sourceDiskName,
    createDiskFromSnapshot_addOns,
    createDiskFromSnapshot_diskSnapshotName,
    createDiskFromSnapshot_restoreDate,
    createDiskFromSnapshot_tags,
    createDiskFromSnapshot_diskName,
    createDiskFromSnapshot_availabilityZone,
    createDiskFromSnapshot_sizeInGb,
    createDiskFromSnapshotResponse_operations,
    createDiskFromSnapshotResponse_httpStatus,

    -- ** GetRelationalDatabases
    getRelationalDatabases_pageToken,
    getRelationalDatabasesResponse_nextPageToken,
    getRelationalDatabasesResponse_relationalDatabases,
    getRelationalDatabasesResponse_httpStatus,

    -- ** GetInstanceSnapshot
    getInstanceSnapshot_instanceSnapshotName,
    getInstanceSnapshotResponse_instanceSnapshot,
    getInstanceSnapshotResponse_httpStatus,

    -- ** GetRelationalDatabaseLogEvents
    getRelationalDatabaseLogEvents_startTime,
    getRelationalDatabaseLogEvents_startFromHead,
    getRelationalDatabaseLogEvents_endTime,
    getRelationalDatabaseLogEvents_pageToken,
    getRelationalDatabaseLogEvents_relationalDatabaseName,
    getRelationalDatabaseLogEvents_logStreamName,
    getRelationalDatabaseLogEventsResponse_nextBackwardToken,
    getRelationalDatabaseLogEventsResponse_resourceLogEvents,
    getRelationalDatabaseLogEventsResponse_nextForwardToken,
    getRelationalDatabaseLogEventsResponse_httpStatus,

    -- ** CreateContactMethod
    createContactMethod_protocol,
    createContactMethod_contactEndpoint,
    createContactMethodResponse_operations,
    createContactMethodResponse_httpStatus,

    -- ** GetRelationalDatabaseLogStreams
    getRelationalDatabaseLogStreams_relationalDatabaseName,
    getRelationalDatabaseLogStreamsResponse_logStreams,
    getRelationalDatabaseLogStreamsResponse_httpStatus,

    -- ** GetDomain
    getDomain_domainName,
    getDomainResponse_domain,
    getDomainResponse_httpStatus,

    -- ** GetAutoSnapshots
    getAutoSnapshots_resourceName,
    getAutoSnapshotsResponse_resourceType,
    getAutoSnapshotsResponse_resourceName,
    getAutoSnapshotsResponse_autoSnapshots,
    getAutoSnapshotsResponse_httpStatus,

    -- ** GetActiveNames
    getActiveNames_pageToken,
    getActiveNamesResponse_nextPageToken,
    getActiveNamesResponse_activeNames,
    getActiveNamesResponse_httpStatus,

    -- ** DeleteContactMethod
    deleteContactMethod_protocol,
    deleteContactMethodResponse_operations,
    deleteContactMethodResponse_httpStatus,

    -- ** CreateDistribution
    createDistribution_cacheBehaviorSettings,
    createDistribution_ipAddressType,
    createDistribution_cacheBehaviors,
    createDistribution_tags,
    createDistribution_distributionName,
    createDistribution_origin,
    createDistribution_defaultCacheBehavior,
    createDistribution_bundleId,
    createDistributionResponse_distribution,
    createDistributionResponse_operation,
    createDistributionResponse_httpStatus,

    -- ** StopRelationalDatabase
    stopRelationalDatabase_relationalDatabaseSnapshotName,
    stopRelationalDatabase_relationalDatabaseName,
    stopRelationalDatabaseResponse_operations,
    stopRelationalDatabaseResponse_httpStatus,

    -- ** CreateRelationalDatabaseSnapshot
    createRelationalDatabaseSnapshot_tags,
    createRelationalDatabaseSnapshot_relationalDatabaseName,
    createRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,
    createRelationalDatabaseSnapshotResponse_operations,
    createRelationalDatabaseSnapshotResponse_httpStatus,

    -- ** DetachCertificateFromDistribution
    detachCertificateFromDistribution_distributionName,
    detachCertificateFromDistributionResponse_operation,
    detachCertificateFromDistributionResponse_httpStatus,

    -- ** CreateContainerService
    createContainerService_publicDomainNames,
    createContainerService_tags,
    createContainerService_deployment,
    createContainerService_serviceName,
    createContainerService_power,
    createContainerService_scale,
    createContainerServiceResponse_containerService,
    createContainerServiceResponse_httpStatus,

    -- ** GetInstanceAccessDetails
    getInstanceAccessDetails_protocol,
    getInstanceAccessDetails_instanceName,
    getInstanceAccessDetailsResponse_accessDetails,
    getInstanceAccessDetailsResponse_httpStatus,

    -- ** EnableAddOn
    enableAddOn_resourceName,
    enableAddOn_addOnRequest,
    enableAddOnResponse_operations,
    enableAddOnResponse_httpStatus,

    -- ** StopInstance
    stopInstance_force,
    stopInstance_instanceName,
    stopInstanceResponse_operations,
    stopInstanceResponse_httpStatus,

    -- ** DetachInstancesFromLoadBalancer
    detachInstancesFromLoadBalancer_loadBalancerName,
    detachInstancesFromLoadBalancer_instanceNames,
    detachInstancesFromLoadBalancerResponse_operations,
    detachInstancesFromLoadBalancerResponse_httpStatus,

    -- ** RegisterContainerImage
    registerContainerImage_serviceName,
    registerContainerImage_label,
    registerContainerImage_digest,
    registerContainerImageResponse_containerImage,
    registerContainerImageResponse_httpStatus,

    -- ** CreateCertificate
    createCertificate_subjectAlternativeNames,
    createCertificate_tags,
    createCertificate_certificateName,
    createCertificate_domainName,
    createCertificateResponse_certificate,
    createCertificateResponse_operations,
    createCertificateResponse_httpStatus,

    -- ** CreateInstanceSnapshot
    createInstanceSnapshot_tags,
    createInstanceSnapshot_instanceSnapshotName,
    createInstanceSnapshot_instanceName,
    createInstanceSnapshotResponse_operations,
    createInstanceSnapshotResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_useLatestRestorableAutoSnapshot,
    copySnapshot_restoreDate,
    copySnapshot_sourceResourceName,
    copySnapshot_sourceSnapshotName,
    copySnapshot_targetSnapshotName,
    copySnapshot_sourceRegion,
    copySnapshotResponse_operations,
    copySnapshotResponse_httpStatus,

    -- ** GetRelationalDatabaseSnapshot
    getRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,
    getRelationalDatabaseSnapshotResponse_relationalDatabaseSnapshot,
    getRelationalDatabaseSnapshotResponse_httpStatus,

    -- ** IsVpcPeered
    isVpcPeeredResponse_isPeered,
    isVpcPeeredResponse_httpStatus,

    -- ** GetStaticIps
    getStaticIps_pageToken,
    getStaticIpsResponse_nextPageToken,
    getStaticIpsResponse_staticIps,
    getStaticIpsResponse_httpStatus,

    -- ** UnpeerVpc
    unpeerVpcResponse_operation,
    unpeerVpcResponse_httpStatus,

    -- ** DeleteDisk
    deleteDisk_forceDeleteAddOns,
    deleteDisk_diskName,
    deleteDiskResponse_operations,
    deleteDiskResponse_httpStatus,

    -- ** CreateInstancesFromSnapshot
    createInstancesFromSnapshot_useLatestRestorableAutoSnapshot,
    createInstancesFromSnapshot_instanceSnapshotName,
    createInstancesFromSnapshot_addOns,
    createInstancesFromSnapshot_userData,
    createInstancesFromSnapshot_restoreDate,
    createInstancesFromSnapshot_ipAddressType,
    createInstancesFromSnapshot_keyPairName,
    createInstancesFromSnapshot_sourceInstanceName,
    createInstancesFromSnapshot_attachedDiskMapping,
    createInstancesFromSnapshot_tags,
    createInstancesFromSnapshot_instanceNames,
    createInstancesFromSnapshot_availabilityZone,
    createInstancesFromSnapshot_bundleId,
    createInstancesFromSnapshotResponse_operations,
    createInstancesFromSnapshotResponse_httpStatus,

    -- ** GetCloudFormationStackRecords
    getCloudFormationStackRecords_pageToken,
    getCloudFormationStackRecordsResponse_nextPageToken,
    getCloudFormationStackRecordsResponse_cloudFormationStackRecords,
    getCloudFormationStackRecordsResponse_httpStatus,

    -- ** CreateDomain
    createDomain_tags,
    createDomain_domainName,
    createDomainResponse_operation,
    createDomainResponse_httpStatus,

    -- ** GetRelationalDatabaseBlueprints
    getRelationalDatabaseBlueprints_pageToken,
    getRelationalDatabaseBlueprintsResponse_blueprints,
    getRelationalDatabaseBlueprintsResponse_nextPageToken,
    getRelationalDatabaseBlueprintsResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_certificateName,
    deleteCertificateResponse_operations,
    deleteCertificateResponse_httpStatus,

    -- ** GetDiskSnapshots
    getDiskSnapshots_pageToken,
    getDiskSnapshotsResponse_nextPageToken,
    getDiskSnapshotsResponse_diskSnapshots,
    getDiskSnapshotsResponse_httpStatus,

    -- ** GetContainerAPIMetadata
    getContainerAPIMetadataResponse_metadata,
    getContainerAPIMetadataResponse_httpStatus,

    -- ** GetBucketMetricData
    getBucketMetricData_bucketName,
    getBucketMetricData_metricName,
    getBucketMetricData_startTime,
    getBucketMetricData_endTime,
    getBucketMetricData_period,
    getBucketMetricData_statistics,
    getBucketMetricData_unit,
    getBucketMetricDataResponse_metricName,
    getBucketMetricDataResponse_metricData,
    getBucketMetricDataResponse_httpStatus,

    -- ** PeerVpc
    peerVpcResponse_operation,
    peerVpcResponse_httpStatus,

    -- ** GetRelationalDatabaseBundles
    getRelationalDatabaseBundles_pageToken,
    getRelationalDatabaseBundlesResponse_nextPageToken,
    getRelationalDatabaseBundlesResponse_bundles,
    getRelationalDatabaseBundlesResponse_httpStatus,

    -- ** GetLoadBalancers
    getLoadBalancers_pageToken,
    getLoadBalancersResponse_nextPageToken,
    getLoadBalancersResponse_loadBalancers,
    getLoadBalancersResponse_httpStatus,

    -- ** RebootRelationalDatabase
    rebootRelationalDatabase_relationalDatabaseName,
    rebootRelationalDatabaseResponse_operations,
    rebootRelationalDatabaseResponse_httpStatus,

    -- ** AttachLoadBalancerTlsCertificate
    attachLoadBalancerTlsCertificate_loadBalancerName,
    attachLoadBalancerTlsCertificate_certificateName,
    attachLoadBalancerTlsCertificateResponse_operations,
    attachLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** UpdateLoadBalancerAttribute
    updateLoadBalancerAttribute_loadBalancerName,
    updateLoadBalancerAttribute_attributeName,
    updateLoadBalancerAttribute_attributeValue,
    updateLoadBalancerAttributeResponse_operations,
    updateLoadBalancerAttributeResponse_httpStatus,

    -- ** DeleteRelationalDatabase
    deleteRelationalDatabase_skipFinalSnapshot,
    deleteRelationalDatabase_finalRelationalDatabaseSnapshotName,
    deleteRelationalDatabase_relationalDatabaseName,
    deleteRelationalDatabaseResponse_operations,
    deleteRelationalDatabaseResponse_httpStatus,

    -- ** GetDiskSnapshot
    getDiskSnapshot_diskSnapshotName,
    getDiskSnapshotResponse_diskSnapshot,
    getDiskSnapshotResponse_httpStatus,

    -- ** UpdateRelationalDatabase
    updateRelationalDatabase_masterUserPassword,
    updateRelationalDatabase_publiclyAccessible,
    updateRelationalDatabase_enableBackupRetention,
    updateRelationalDatabase_preferredMaintenanceWindow,
    updateRelationalDatabase_caCertificateIdentifier,
    updateRelationalDatabase_preferredBackupWindow,
    updateRelationalDatabase_applyImmediately,
    updateRelationalDatabase_rotateMasterUserPassword,
    updateRelationalDatabase_disableBackupRetention,
    updateRelationalDatabase_relationalDatabaseName,
    updateRelationalDatabaseResponse_operations,
    updateRelationalDatabaseResponse_httpStatus,

    -- ** GetStaticIp
    getStaticIp_staticIpName,
    getStaticIpResponse_staticIp,
    getStaticIpResponse_httpStatus,

    -- ** GetRelationalDatabaseMasterUserPassword
    getRelationalDatabaseMasterUserPassword_passwordVersion,
    getRelationalDatabaseMasterUserPassword_relationalDatabaseName,
    getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword,
    getRelationalDatabaseMasterUserPasswordResponse_createdAt,
    getRelationalDatabaseMasterUserPasswordResponse_httpStatus,

    -- ** GetBlueprints
    getBlueprints_includeInactive,
    getBlueprints_pageToken,
    getBlueprintsResponse_blueprints,
    getBlueprintsResponse_nextPageToken,
    getBlueprintsResponse_httpStatus,

    -- ** PutAlarm
    putAlarm_treatMissingData,
    putAlarm_contactProtocols,
    putAlarm_datapointsToAlarm,
    putAlarm_notificationEnabled,
    putAlarm_notificationTriggers,
    putAlarm_alarmName,
    putAlarm_metricName,
    putAlarm_monitoredResourceName,
    putAlarm_comparisonOperator,
    putAlarm_threshold,
    putAlarm_evaluationPeriods,
    putAlarmResponse_operations,
    putAlarmResponse_httpStatus,

    -- ** DeleteAlarm
    deleteAlarm_alarmName,
    deleteAlarmResponse_operations,
    deleteAlarmResponse_httpStatus,

    -- ** GetInstancePortStates
    getInstancePortStates_instanceName,
    getInstancePortStatesResponse_portStates,
    getInstancePortStatesResponse_httpStatus,

    -- ** DeleteAutoSnapshot
    deleteAutoSnapshot_resourceName,
    deleteAutoSnapshot_date,
    deleteAutoSnapshotResponse_operations,
    deleteAutoSnapshotResponse_httpStatus,

    -- ** CreateRelationalDatabase
    createRelationalDatabase_masterUserPassword,
    createRelationalDatabase_publiclyAccessible,
    createRelationalDatabase_preferredMaintenanceWindow,
    createRelationalDatabase_preferredBackupWindow,
    createRelationalDatabase_availabilityZone,
    createRelationalDatabase_tags,
    createRelationalDatabase_relationalDatabaseName,
    createRelationalDatabase_relationalDatabaseBlueprintId,
    createRelationalDatabase_relationalDatabaseBundleId,
    createRelationalDatabase_masterDatabaseName,
    createRelationalDatabase_masterUsername,
    createRelationalDatabaseResponse_operations,
    createRelationalDatabaseResponse_httpStatus,

    -- ** SendContactMethodVerification
    sendContactMethodVerification_protocol,
    sendContactMethodVerificationResponse_operations,
    sendContactMethodVerificationResponse_httpStatus,

    -- ** GetContainerLog
    getContainerLog_startTime,
    getContainerLog_endTime,
    getContainerLog_pageToken,
    getContainerLog_filterPattern,
    getContainerLog_serviceName,
    getContainerLog_containerName,
    getContainerLogResponse_nextPageToken,
    getContainerLogResponse_logEvents,
    getContainerLogResponse_httpStatus,

    -- ** CreateDiskSnapshot
    createDiskSnapshot_diskName,
    createDiskSnapshot_instanceName,
    createDiskSnapshot_tags,
    createDiskSnapshot_diskSnapshotName,
    createDiskSnapshotResponse_operations,
    createDiskSnapshotResponse_httpStatus,

    -- ** DeleteDomainEntry
    deleteDomainEntry_domainName,
    deleteDomainEntry_domainEntry,
    deleteDomainEntryResponse_operation,
    deleteDomainEntryResponse_httpStatus,

    -- ** UpdateDomainEntry
    updateDomainEntry_domainName,
    updateDomainEntry_domainEntry,
    updateDomainEntryResponse_operations,
    updateDomainEntryResponse_httpStatus,

    -- ** GetRegions
    getRegions_includeRelationalDatabaseAvailabilityZones,
    getRegions_includeAvailabilityZones,
    getRegionsResponse_regions,
    getRegionsResponse_httpStatus,

    -- ** SetIpAddressType
    setIpAddressType_resourceType,
    setIpAddressType_resourceName,
    setIpAddressType_ipAddressType,
    setIpAddressTypeResponse_operations,
    setIpAddressTypeResponse_httpStatus,

    -- ** DeleteDiskSnapshot
    deleteDiskSnapshot_diskSnapshotName,
    deleteDiskSnapshotResponse_operations,
    deleteDiskSnapshotResponse_httpStatus,

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

    -- ** GetInstanceState
    getInstanceState_instanceName,
    getInstanceStateResponse_state,
    getInstanceStateResponse_httpStatus,

    -- ** GetKeyPairs
    getKeyPairs_pageToken,
    getKeyPairsResponse_nextPageToken,
    getKeyPairsResponse_keyPairs,
    getKeyPairsResponse_httpStatus,

    -- ** GetOperations
    getOperations_pageToken,
    getOperationsResponse_nextPageToken,
    getOperationsResponse_operations,
    getOperationsResponse_httpStatus,

    -- ** GetBucketAccessKeys
    getBucketAccessKeys_bucketName,
    getBucketAccessKeysResponse_accessKeys,
    getBucketAccessKeysResponse_httpStatus,

    -- ** GetDisks
    getDisks_pageToken,
    getDisksResponse_nextPageToken,
    getDisksResponse_disks,
    getDisksResponse_httpStatus,

    -- ** GetRelationalDatabase
    getRelationalDatabase_relationalDatabaseName,
    getRelationalDatabaseResponse_relationalDatabase,
    getRelationalDatabaseResponse_httpStatus,

    -- ** AttachInstancesToLoadBalancer
    attachInstancesToLoadBalancer_loadBalancerName,
    attachInstancesToLoadBalancer_instanceNames,
    attachInstancesToLoadBalancerResponse_operations,
    attachInstancesToLoadBalancerResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_resourceName,
    tagResource_tags,
    tagResourceResponse_operations,
    tagResourceResponse_httpStatus,

    -- ** GetOperation
    getOperation_operationId,
    getOperationResponse_operation,
    getOperationResponse_httpStatus,

    -- ** ResetDistributionCache
    resetDistributionCache_distributionName,
    resetDistributionCacheResponse_status,
    resetDistributionCacheResponse_operation,
    resetDistributionCacheResponse_createTime,
    resetDistributionCacheResponse_httpStatus,

    -- ** UpdateBucketBundle
    updateBucketBundle_bucketName,
    updateBucketBundle_bundleId,
    updateBucketBundleResponse_operations,
    updateBucketBundleResponse_httpStatus,

    -- ** UpdateDistribution
    updateDistribution_origin,
    updateDistribution_cacheBehaviorSettings,
    updateDistribution_isEnabled,
    updateDistribution_defaultCacheBehavior,
    updateDistribution_cacheBehaviors,
    updateDistribution_distributionName,
    updateDistributionResponse_operation,
    updateDistributionResponse_httpStatus,

    -- ** GetBuckets
    getBuckets_bucketName,
    getBuckets_includeConnectedResources,
    getBuckets_pageToken,
    getBucketsResponse_nextPageToken,
    getBucketsResponse_buckets,
    getBucketsResponse_httpStatus,

    -- ** DeleteDistribution
    deleteDistribution_distributionName,
    deleteDistributionResponse_operation,
    deleteDistributionResponse_httpStatus,

    -- ** UpdateContainerService
    updateContainerService_scale,
    updateContainerService_power,
    updateContainerService_isDisabled,
    updateContainerService_publicDomainNames,
    updateContainerService_serviceName,
    updateContainerServiceResponse_containerService,
    updateContainerServiceResponse_httpStatus,

    -- ** DeleteRelationalDatabaseSnapshot
    deleteRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,
    deleteRelationalDatabaseSnapshotResponse_operations,
    deleteRelationalDatabaseSnapshotResponse_httpStatus,

    -- ** DeleteContainerService
    deleteContainerService_serviceName,
    deleteContainerServiceResponse_httpStatus,

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

    -- ** GetKeyPair
    getKeyPair_keyPairName,
    getKeyPairResponse_keyPair,
    getKeyPairResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_resourceName,
    untagResource_tagKeys,
    untagResourceResponse_operations,
    untagResourceResponse_httpStatus,

    -- ** PutInstancePublicPorts
    putInstancePublicPorts_portInfos,
    putInstancePublicPorts_instanceName,
    putInstancePublicPortsResponse_operation,
    putInstancePublicPortsResponse_httpStatus,

    -- ** GetDistributionBundles
    getDistributionBundlesResponse_bundles,
    getDistributionBundlesResponse_httpStatus,

    -- ** GetDisk
    getDisk_diskName,
    getDiskResponse_disk,
    getDiskResponse_httpStatus,

    -- ** GetDistributionLatestCacheReset
    getDistributionLatestCacheReset_distributionName,
    getDistributionLatestCacheResetResponse_status,
    getDistributionLatestCacheResetResponse_createTime,
    getDistributionLatestCacheResetResponse_httpStatus,

    -- ** CreateLoadBalancer
    createLoadBalancer_healthCheckPath,
    createLoadBalancer_certificateName,
    createLoadBalancer_certificateDomainName,
    createLoadBalancer_certificateAlternativeNames,
    createLoadBalancer_ipAddressType,
    createLoadBalancer_tags,
    createLoadBalancer_loadBalancerName,
    createLoadBalancer_instancePort,
    createLoadBalancerResponse_operations,
    createLoadBalancerResponse_httpStatus,

    -- ** GetContainerServiceDeployments
    getContainerServiceDeployments_serviceName,
    getContainerServiceDeploymentsResponse_deployments,
    getContainerServiceDeploymentsResponse_httpStatus,

    -- ** DeleteKnownHostKeys
    deleteKnownHostKeys_instanceName,
    deleteKnownHostKeysResponse_operations,
    deleteKnownHostKeysResponse_httpStatus,

    -- ** AttachDisk
    attachDisk_diskName,
    attachDisk_instanceName,
    attachDisk_diskPath,
    attachDiskResponse_operations,
    attachDiskResponse_httpStatus,

    -- ** DetachStaticIp
    detachStaticIp_staticIpName,
    detachStaticIpResponse_operations,
    detachStaticIpResponse_httpStatus,

    -- ** CreateInstances
    createInstances_customImageName,
    createInstances_addOns,
    createInstances_userData,
    createInstances_ipAddressType,
    createInstances_keyPairName,
    createInstances_tags,
    createInstances_instanceNames,
    createInstances_availabilityZone,
    createInstances_blueprintId,
    createInstances_bundleId,
    createInstancesResponse_operations,
    createInstancesResponse_httpStatus,

    -- ** GetAlarms
    getAlarms_alarmName,
    getAlarms_monitoredResourceName,
    getAlarms_pageToken,
    getAlarmsResponse_nextPageToken,
    getAlarmsResponse_alarms,
    getAlarmsResponse_httpStatus,

    -- ** OpenInstancePublicPorts
    openInstancePublicPorts_portInfo,
    openInstancePublicPorts_instanceName,
    openInstancePublicPortsResponse_operation,
    openInstancePublicPortsResponse_httpStatus,

    -- ** StartRelationalDatabase
    startRelationalDatabase_relationalDatabaseName,
    startRelationalDatabaseResponse_operations,
    startRelationalDatabaseResponse_httpStatus,

    -- ** DeleteContainerImage
    deleteContainerImage_serviceName,
    deleteContainerImage_image,
    deleteContainerImageResponse_httpStatus,

    -- ** GetBundles
    getBundles_includeInactive,
    getBundles_pageToken,
    getBundlesResponse_nextPageToken,
    getBundlesResponse_bundles,
    getBundlesResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_operation,
    deleteDomainResponse_httpStatus,

    -- ** GetLoadBalancerTlsCertificates
    getLoadBalancerTlsCertificates_loadBalancerName,
    getLoadBalancerTlsCertificatesResponse_tlsCertificates,
    getLoadBalancerTlsCertificatesResponse_httpStatus,

    -- ** SetResourceAccessForBucket
    setResourceAccessForBucket_resourceName,
    setResourceAccessForBucket_bucketName,
    setResourceAccessForBucket_access,
    setResourceAccessForBucketResponse_operations,
    setResourceAccessForBucketResponse_httpStatus,

    -- ** CreateDisk
    createDisk_addOns,
    createDisk_tags,
    createDisk_diskName,
    createDisk_availabilityZone,
    createDisk_sizeInGb,
    createDiskResponse_operations,
    createDiskResponse_httpStatus,

    -- ** CreateBucketAccessKey
    createBucketAccessKey_bucketName,
    createBucketAccessKeyResponse_accessKey,
    createBucketAccessKeyResponse_operations,
    createBucketAccessKeyResponse_httpStatus,

    -- ** GetOperationsForResource
    getOperationsForResource_pageToken,
    getOperationsForResource_resourceName,
    getOperationsForResourceResponse_nextPageCount,
    getOperationsForResourceResponse_nextPageToken,
    getOperationsForResourceResponse_operations,
    getOperationsForResourceResponse_httpStatus,

    -- ** CreateKeyPair
    createKeyPair_tags,
    createKeyPair_keyPairName,
    createKeyPairResponse_keyPair,
    createKeyPairResponse_operation,
    createKeyPairResponse_publicKeyBase64,
    createKeyPairResponse_privateKeyBase64,
    createKeyPairResponse_httpStatus,

    -- ** StartInstance
    startInstance_instanceName,
    startInstanceResponse_operations,
    startInstanceResponse_httpStatus,

    -- * Types

    -- ** AccessKey
    accessKey_status,
    accessKey_createdAt,
    accessKey_secretAccessKey,
    accessKey_lastUsed,
    accessKey_accessKeyId,

    -- ** AccessKeyLastUsed
    accessKeyLastUsed_lastUsedDate,
    accessKeyLastUsed_serviceName,
    accessKeyLastUsed_region,

    -- ** AccessRules
    accessRules_getObject,
    accessRules_allowPublicOverrides,

    -- ** AddOn
    addOn_status,
    addOn_nextSnapshotTimeOfDay,
    addOn_snapshotTimeOfDay,
    addOn_name,

    -- ** AddOnRequest
    addOnRequest_autoSnapshotAddOnRequest,
    addOnRequest_addOnType,

    -- ** Alarm
    alarm_state,
    alarm_treatMissingData,
    alarm_resourceType,
    alarm_arn,
    alarm_createdAt,
    alarm_location,
    alarm_contactProtocols,
    alarm_period,
    alarm_evaluationPeriods,
    alarm_metricName,
    alarm_comparisonOperator,
    alarm_name,
    alarm_threshold,
    alarm_datapointsToAlarm,
    alarm_supportCode,
    alarm_notificationEnabled,
    alarm_notificationTriggers,
    alarm_statistic,
    alarm_unit,
    alarm_monitoredResourceInfo,

    -- ** AttachedDisk
    attachedDisk_path,
    attachedDisk_sizeInGb,

    -- ** AutoSnapshotAddOnRequest
    autoSnapshotAddOnRequest_snapshotTimeOfDay,

    -- ** AutoSnapshotDetails
    autoSnapshotDetails_status,
    autoSnapshotDetails_fromAttachedDisks,
    autoSnapshotDetails_createdAt,
    autoSnapshotDetails_date,

    -- ** AvailabilityZone
    availabilityZone_state,
    availabilityZone_zoneName,

    -- ** Blueprint
    blueprint_versionCode,
    blueprint_platform,
    blueprint_group,
    blueprint_minPower,
    blueprint_productUrl,
    blueprint_licenseUrl,
    blueprint_name,
    blueprint_version,
    blueprint_blueprintId,
    blueprint_type,
    blueprint_isActive,
    blueprint_description,

    -- ** Bucket
    bucket_state,
    bucket_objectVersioning,
    bucket_resourceType,
    bucket_arn,
    bucket_createdAt,
    bucket_location,
    bucket_readonlyAccessAccounts,
    bucket_url,
    bucket_resourcesReceivingAccess,
    bucket_bundleId,
    bucket_accessRules,
    bucket_ableToUpdateBundle,
    bucket_name,
    bucket_supportCode,
    bucket_tags,

    -- ** BucketBundle
    bucketBundle_storagePerMonthInGb,
    bucketBundle_transferPerMonthInGb,
    bucketBundle_bundleId,
    bucketBundle_name,
    bucketBundle_price,
    bucketBundle_isActive,

    -- ** BucketState
    bucketState_code,
    bucketState_message,

    -- ** Bundle
    bundle_cpuCount,
    bundle_transferPerMonthInGb,
    bundle_bundleId,
    bundle_instanceType,
    bundle_name,
    bundle_power,
    bundle_diskSizeInGb,
    bundle_supportedPlatforms,
    bundle_price,
    bundle_isActive,
    bundle_ramSizeInGb,

    -- ** CacheBehavior
    cacheBehavior_behavior,

    -- ** CacheBehaviorPerPath
    cacheBehaviorPerPath_path,
    cacheBehaviorPerPath_behavior,

    -- ** CacheSettings
    cacheSettings_maximumTTL,
    cacheSettings_cachedHTTPMethods,
    cacheSettings_forwardedCookies,
    cacheSettings_allowedHTTPMethods,
    cacheSettings_defaultTTL,
    cacheSettings_minimumTTL,
    cacheSettings_forwardedHeaders,
    cacheSettings_forwardedQueryStrings,

    -- ** Certificate
    certificate_status,
    certificate_subjectAlternativeNames,
    certificate_arn,
    certificate_createdAt,
    certificate_eligibleToRenew,
    certificate_requestFailureReason,
    certificate_revokedAt,
    certificate_notBefore,
    certificate_revocationReason,
    certificate_domainName,
    certificate_name,
    certificate_renewalSummary,
    certificate_supportCode,
    certificate_domainValidationRecords,
    certificate_inUseResourceCount,
    certificate_issuedAt,
    certificate_keyAlgorithm,
    certificate_serialNumber,
    certificate_issuerCA,
    certificate_tags,
    certificate_notAfter,

    -- ** CertificateSummary
    certificateSummary_certificateDetail,
    certificateSummary_certificateName,
    certificateSummary_certificateArn,
    certificateSummary_domainName,
    certificateSummary_tags,

    -- ** CloudFormationStackRecord
    cloudFormationStackRecord_state,
    cloudFormationStackRecord_destinationInfo,
    cloudFormationStackRecord_resourceType,
    cloudFormationStackRecord_arn,
    cloudFormationStackRecord_createdAt,
    cloudFormationStackRecord_location,
    cloudFormationStackRecord_name,
    cloudFormationStackRecord_sourceInfo,

    -- ** CloudFormationStackRecordSourceInfo
    cloudFormationStackRecordSourceInfo_resourceType,
    cloudFormationStackRecordSourceInfo_arn,
    cloudFormationStackRecordSourceInfo_name,

    -- ** ContactMethod
    contactMethod_status,
    contactMethod_resourceType,
    contactMethod_arn,
    contactMethod_createdAt,
    contactMethod_location,
    contactMethod_protocol,
    contactMethod_name,
    contactMethod_supportCode,
    contactMethod_contactEndpoint,

    -- ** Container
    container_image,
    container_command,
    container_environment,
    container_ports,

    -- ** ContainerImage
    containerImage_image,
    containerImage_createdAt,
    containerImage_digest,

    -- ** ContainerService
    containerService_state,
    containerService_powerId,
    containerService_resourceType,
    containerService_arn,
    containerService_createdAt,
    containerService_location,
    containerService_scale,
    containerService_url,
    containerService_stateDetail,
    containerService_nextDeployment,
    containerService_principalArn,
    containerService_power,
    containerService_privateDomainName,
    containerService_isDisabled,
    containerService_publicDomainNames,
    containerService_containerServiceName,
    containerService_currentDeployment,
    containerService_tags,

    -- ** ContainerServiceDeployment
    containerServiceDeployment_state,
    containerServiceDeployment_publicEndpoint,
    containerServiceDeployment_createdAt,
    containerServiceDeployment_containers,
    containerServiceDeployment_version,

    -- ** ContainerServiceDeploymentRequest
    containerServiceDeploymentRequest_publicEndpoint,
    containerServiceDeploymentRequest_containers,

    -- ** ContainerServiceEndpoint
    containerServiceEndpoint_healthCheck,
    containerServiceEndpoint_containerName,
    containerServiceEndpoint_containerPort,

    -- ** ContainerServiceHealthCheckConfig
    containerServiceHealthCheckConfig_healthyThreshold,
    containerServiceHealthCheckConfig_path,
    containerServiceHealthCheckConfig_successCodes,
    containerServiceHealthCheckConfig_intervalSeconds,
    containerServiceHealthCheckConfig_timeoutSeconds,
    containerServiceHealthCheckConfig_unhealthyThreshold,

    -- ** ContainerServiceLogEvent
    containerServiceLogEvent_createdAt,
    containerServiceLogEvent_message,

    -- ** ContainerServicePower
    containerServicePower_powerId,
    containerServicePower_cpuCount,
    containerServicePower_name,
    containerServicePower_price,
    containerServicePower_isActive,
    containerServicePower_ramSizeInGb,

    -- ** ContainerServiceRegistryLogin
    containerServiceRegistryLogin_expiresAt,
    containerServiceRegistryLogin_username,
    containerServiceRegistryLogin_password,
    containerServiceRegistryLogin_registry,

    -- ** ContainerServiceStateDetail
    containerServiceStateDetail_code,
    containerServiceStateDetail_message,

    -- ** CookieObject
    cookieObject_cookiesAllowList,
    cookieObject_option,

    -- ** DestinationInfo
    destinationInfo_service,
    destinationInfo_id,

    -- ** Disk
    disk_state,
    disk_resourceType,
    disk_arn,
    disk_path,
    disk_createdAt,
    disk_location,
    disk_iops,
    disk_isAttached,
    disk_addOns,
    disk_attachmentState,
    disk_name,
    disk_sizeInGb,
    disk_supportCode,
    disk_isSystemDisk,
    disk_attachedTo,
    disk_gbInUse,
    disk_tags,

    -- ** DiskInfo
    diskInfo_path,
    diskInfo_name,
    diskInfo_sizeInGb,
    diskInfo_isSystemDisk,

    -- ** DiskMap
    diskMap_newDiskName,
    diskMap_originalDiskPath,

    -- ** DiskSnapshot
    diskSnapshot_fromDiskName,
    diskSnapshot_isFromAutoSnapshot,
    diskSnapshot_state,
    diskSnapshot_resourceType,
    diskSnapshot_arn,
    diskSnapshot_createdAt,
    diskSnapshot_location,
    diskSnapshot_progress,
    diskSnapshot_name,
    diskSnapshot_sizeInGb,
    diskSnapshot_supportCode,
    diskSnapshot_fromInstanceArn,
    diskSnapshot_fromInstanceName,
    diskSnapshot_fromDiskArn,
    diskSnapshot_tags,

    -- ** DiskSnapshotInfo
    diskSnapshotInfo_sizeInGb,

    -- ** DistributionBundle
    distributionBundle_transferPerMonthInGb,
    distributionBundle_bundleId,
    distributionBundle_name,
    distributionBundle_price,
    distributionBundle_isActive,

    -- ** Domain
    domain_resourceType,
    domain_domainEntries,
    domain_arn,
    domain_createdAt,
    domain_location,
    domain_name,
    domain_supportCode,
    domain_tags,

    -- ** DomainEntry
    domainEntry_isAlias,
    domainEntry_name,
    domainEntry_id,
    domainEntry_options,
    domainEntry_type,
    domainEntry_target,

    -- ** DomainValidationRecord
    domainValidationRecord_resourceRecord,
    domainValidationRecord_domainName,

    -- ** EndpointRequest
    endpointRequest_healthCheck,
    endpointRequest_containerName,
    endpointRequest_containerPort,

    -- ** ExportSnapshotRecord
    exportSnapshotRecord_state,
    exportSnapshotRecord_destinationInfo,
    exportSnapshotRecord_resourceType,
    exportSnapshotRecord_arn,
    exportSnapshotRecord_createdAt,
    exportSnapshotRecord_location,
    exportSnapshotRecord_name,
    exportSnapshotRecord_sourceInfo,

    -- ** ExportSnapshotRecordSourceInfo
    exportSnapshotRecordSourceInfo_diskSnapshotInfo,
    exportSnapshotRecordSourceInfo_resourceType,
    exportSnapshotRecordSourceInfo_arn,
    exportSnapshotRecordSourceInfo_createdAt,
    exportSnapshotRecordSourceInfo_fromResourceArn,
    exportSnapshotRecordSourceInfo_name,
    exportSnapshotRecordSourceInfo_instanceSnapshotInfo,
    exportSnapshotRecordSourceInfo_fromResourceName,

    -- ** HeaderObject
    headerObject_headersAllowList,
    headerObject_option,

    -- ** HostKeyAttributes
    hostKeyAttributes_notValidAfter,
    hostKeyAttributes_notValidBefore,
    hostKeyAttributes_fingerprintSHA1,
    hostKeyAttributes_publicKey,
    hostKeyAttributes_algorithm,
    hostKeyAttributes_witnessedAt,
    hostKeyAttributes_fingerprintSHA256,

    -- ** InputOrigin
    inputOrigin_regionName,
    inputOrigin_name,
    inputOrigin_protocolPolicy,

    -- ** Instance
    instance_state,
    instance_resourceType,
    instance_arn,
    instance_createdAt,
    instance_location,
    instance_sshKeyName,
    instance_addOns,
    instance_username,
    instance_networking,
    instance_bundleId,
    instance_name,
    instance_ipAddressType,
    instance_supportCode,
    instance_blueprintId,
    instance_privateIpAddress,
    instance_blueprintName,
    instance_isStaticIp,
    instance_publicIpAddress,
    instance_hardware,
    instance_ipv6Addresses,
    instance_tags,

    -- ** InstanceAccessDetails
    instanceAccessDetails_hostKeys,
    instanceAccessDetails_certKey,
    instanceAccessDetails_ipAddress,
    instanceAccessDetails_privateKey,
    instanceAccessDetails_expiresAt,
    instanceAccessDetails_username,
    instanceAccessDetails_protocol,
    instanceAccessDetails_passwordData,
    instanceAccessDetails_password,
    instanceAccessDetails_instanceName,

    -- ** InstanceEntry
    instanceEntry_userData,
    instanceEntry_sourceName,
    instanceEntry_instanceType,
    instanceEntry_portInfoSource,
    instanceEntry_availabilityZone,

    -- ** InstanceHardware
    instanceHardware_cpuCount,
    instanceHardware_disks,
    instanceHardware_ramSizeInGb,

    -- ** InstanceHealthSummary
    instanceHealthSummary_instanceHealth,
    instanceHealthSummary_instanceName,
    instanceHealthSummary_instanceHealthReason,

    -- ** InstanceNetworking
    instanceNetworking_monthlyTransfer,
    instanceNetworking_ports,

    -- ** InstancePortInfo
    instancePortInfo_fromPort,
    instancePortInfo_cidrs,
    instancePortInfo_commonName,
    instancePortInfo_protocol,
    instancePortInfo_cidrListAliases,
    instancePortInfo_ipv6Cidrs,
    instancePortInfo_accessDirection,
    instancePortInfo_accessType,
    instancePortInfo_toPort,
    instancePortInfo_accessFrom,

    -- ** InstancePortState
    instancePortState_fromPort,
    instancePortState_cidrs,
    instancePortState_state,
    instancePortState_protocol,
    instancePortState_cidrListAliases,
    instancePortState_ipv6Cidrs,
    instancePortState_toPort,

    -- ** InstanceSnapshot
    instanceSnapshot_fromBlueprintId,
    instanceSnapshot_isFromAutoSnapshot,
    instanceSnapshot_state,
    instanceSnapshot_resourceType,
    instanceSnapshot_fromAttachedDisks,
    instanceSnapshot_arn,
    instanceSnapshot_createdAt,
    instanceSnapshot_location,
    instanceSnapshot_progress,
    instanceSnapshot_name,
    instanceSnapshot_fromBundleId,
    instanceSnapshot_sizeInGb,
    instanceSnapshot_supportCode,
    instanceSnapshot_fromInstanceArn,
    instanceSnapshot_fromInstanceName,
    instanceSnapshot_tags,

    -- ** InstanceSnapshotInfo
    instanceSnapshotInfo_fromBlueprintId,
    instanceSnapshotInfo_fromBundleId,
    instanceSnapshotInfo_fromDiskInfo,

    -- ** InstanceState
    instanceState_name,
    instanceState_code,

    -- ** KeyPair
    keyPair_resourceType,
    keyPair_arn,
    keyPair_createdAt,
    keyPair_location,
    keyPair_fingerprint,
    keyPair_name,
    keyPair_supportCode,
    keyPair_tags,

    -- ** LightsailDistribution
    lightsailDistribution_status,
    lightsailDistribution_origin,
    lightsailDistribution_certificateName,
    lightsailDistribution_resourceType,
    lightsailDistribution_arn,
    lightsailDistribution_createdAt,
    lightsailDistribution_location,
    lightsailDistribution_cacheBehaviorSettings,
    lightsailDistribution_alternativeDomainNames,
    lightsailDistribution_bundleId,
    lightsailDistribution_ableToUpdateBundle,
    lightsailDistribution_originPublicDNS,
    lightsailDistribution_domainName,
    lightsailDistribution_name,
    lightsailDistribution_ipAddressType,
    lightsailDistribution_isEnabled,
    lightsailDistribution_supportCode,
    lightsailDistribution_defaultCacheBehavior,
    lightsailDistribution_cacheBehaviors,
    lightsailDistribution_tags,

    -- ** LoadBalancer
    loadBalancer_healthCheckPath,
    loadBalancer_state,
    loadBalancer_resourceType,
    loadBalancer_arn,
    loadBalancer_createdAt,
    loadBalancer_location,
    loadBalancer_instancePort,
    loadBalancer_configurationOptions,
    loadBalancer_protocol,
    loadBalancer_tlsCertificateSummaries,
    loadBalancer_name,
    loadBalancer_ipAddressType,
    loadBalancer_supportCode,
    loadBalancer_publicPorts,
    loadBalancer_dnsName,
    loadBalancer_instanceHealthSummary,
    loadBalancer_tags,

    -- ** LoadBalancerTlsCertificate
    loadBalancerTlsCertificate_failureReason,
    loadBalancerTlsCertificate_subject,
    loadBalancerTlsCertificate_status,
    loadBalancerTlsCertificate_subjectAlternativeNames,
    loadBalancerTlsCertificate_resourceType,
    loadBalancerTlsCertificate_arn,
    loadBalancerTlsCertificate_createdAt,
    loadBalancerTlsCertificate_location,
    loadBalancerTlsCertificate_loadBalancerName,
    loadBalancerTlsCertificate_serial,
    loadBalancerTlsCertificate_isAttached,
    loadBalancerTlsCertificate_revokedAt,
    loadBalancerTlsCertificate_notBefore,
    loadBalancerTlsCertificate_revocationReason,
    loadBalancerTlsCertificate_domainName,
    loadBalancerTlsCertificate_name,
    loadBalancerTlsCertificate_renewalSummary,
    loadBalancerTlsCertificate_supportCode,
    loadBalancerTlsCertificate_domainValidationRecords,
    loadBalancerTlsCertificate_issuedAt,
    loadBalancerTlsCertificate_keyAlgorithm,
    loadBalancerTlsCertificate_signatureAlgorithm,
    loadBalancerTlsCertificate_issuer,
    loadBalancerTlsCertificate_tags,
    loadBalancerTlsCertificate_notAfter,

    -- ** LoadBalancerTlsCertificateDomainValidationOption
    loadBalancerTlsCertificateDomainValidationOption_domainName,
    loadBalancerTlsCertificateDomainValidationOption_validationStatus,

    -- ** LoadBalancerTlsCertificateDomainValidationRecord
    loadBalancerTlsCertificateDomainValidationRecord_value,
    loadBalancerTlsCertificateDomainValidationRecord_domainName,
    loadBalancerTlsCertificateDomainValidationRecord_name,
    loadBalancerTlsCertificateDomainValidationRecord_validationStatus,
    loadBalancerTlsCertificateDomainValidationRecord_type,

    -- ** LoadBalancerTlsCertificateRenewalSummary
    loadBalancerTlsCertificateRenewalSummary_renewalStatus,
    loadBalancerTlsCertificateRenewalSummary_domainValidationOptions,

    -- ** LoadBalancerTlsCertificateSummary
    loadBalancerTlsCertificateSummary_isAttached,
    loadBalancerTlsCertificateSummary_name,

    -- ** LogEvent
    logEvent_createdAt,
    logEvent_message,

    -- ** MetricDatapoint
    metricDatapoint_sampleCount,
    metricDatapoint_maximum,
    metricDatapoint_average,
    metricDatapoint_minimum,
    metricDatapoint_sum,
    metricDatapoint_timestamp,
    metricDatapoint_unit,

    -- ** MonitoredResourceInfo
    monitoredResourceInfo_resourceType,
    monitoredResourceInfo_arn,
    monitoredResourceInfo_name,

    -- ** MonthlyTransfer
    monthlyTransfer_gbPerMonthAllocated,

    -- ** Operation
    operation_status,
    operation_operationDetails,
    operation_resourceType,
    operation_createdAt,
    operation_resourceName,
    operation_location,
    operation_statusChangedAt,
    operation_errorDetails,
    operation_errorCode,
    operation_id,
    operation_operationType,
    operation_isTerminal,

    -- ** Origin
    origin_regionName,
    origin_resourceType,
    origin_name,
    origin_protocolPolicy,

    -- ** PasswordData
    passwordData_keyPairName,
    passwordData_ciphertext,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_currentApplyDate,

    -- ** PendingModifiedRelationalDatabaseValues
    pendingModifiedRelationalDatabaseValues_engineVersion,
    pendingModifiedRelationalDatabaseValues_masterUserPassword,
    pendingModifiedRelationalDatabaseValues_backupRetentionEnabled,

    -- ** PortInfo
    portInfo_fromPort,
    portInfo_cidrs,
    portInfo_protocol,
    portInfo_cidrListAliases,
    portInfo_ipv6Cidrs,
    portInfo_toPort,

    -- ** QueryStringObject
    queryStringObject_queryStringsAllowList,
    queryStringObject_option,

    -- ** RegionInfo
    regionInfo_availabilityZones,
    regionInfo_name,
    regionInfo_relationalDatabaseAvailabilityZones,
    regionInfo_displayName,
    regionInfo_continentCode,
    regionInfo_description,

    -- ** RelationalDatabase
    relationalDatabase_engineVersion,
    relationalDatabase_relationalDatabaseBundleId,
    relationalDatabase_masterEndpoint,
    relationalDatabase_state,
    relationalDatabase_resourceType,
    relationalDatabase_publiclyAccessible,
    relationalDatabase_masterUsername,
    relationalDatabase_arn,
    relationalDatabase_createdAt,
    relationalDatabase_location,
    relationalDatabase_engine,
    relationalDatabase_latestRestorableTime,
    relationalDatabase_preferredMaintenanceWindow,
    relationalDatabase_relationalDatabaseBlueprintId,
    relationalDatabase_caCertificateIdentifier,
    relationalDatabase_name,
    relationalDatabase_backupRetentionEnabled,
    relationalDatabase_preferredBackupWindow,
    relationalDatabase_pendingMaintenanceActions,
    relationalDatabase_supportCode,
    relationalDatabase_secondaryAvailabilityZone,
    relationalDatabase_pendingModifiedValues,
    relationalDatabase_masterDatabaseName,
    relationalDatabase_hardware,
    relationalDatabase_parameterApplyStatus,
    relationalDatabase_tags,

    -- ** RelationalDatabaseBlueprint
    relationalDatabaseBlueprint_engineVersion,
    relationalDatabaseBlueprint_isEngineDefault,
    relationalDatabaseBlueprint_engineVersionDescription,
    relationalDatabaseBlueprint_engine,
    relationalDatabaseBlueprint_blueprintId,
    relationalDatabaseBlueprint_engineDescription,

    -- ** RelationalDatabaseBundle
    relationalDatabaseBundle_isEncrypted,
    relationalDatabaseBundle_cpuCount,
    relationalDatabaseBundle_transferPerMonthInGb,
    relationalDatabaseBundle_bundleId,
    relationalDatabaseBundle_name,
    relationalDatabaseBundle_diskSizeInGb,
    relationalDatabaseBundle_price,
    relationalDatabaseBundle_isActive,
    relationalDatabaseBundle_ramSizeInGb,

    -- ** RelationalDatabaseEndpoint
    relationalDatabaseEndpoint_address,
    relationalDatabaseEndpoint_port,

    -- ** RelationalDatabaseEvent
    relationalDatabaseEvent_createdAt,
    relationalDatabaseEvent_eventCategories,
    relationalDatabaseEvent_resource,
    relationalDatabaseEvent_message,

    -- ** RelationalDatabaseHardware
    relationalDatabaseHardware_cpuCount,
    relationalDatabaseHardware_diskSizeInGb,
    relationalDatabaseHardware_ramSizeInGb,

    -- ** RelationalDatabaseParameter
    relationalDatabaseParameter_applyType,
    relationalDatabaseParameter_parameterValue,
    relationalDatabaseParameter_applyMethod,
    relationalDatabaseParameter_dataType,
    relationalDatabaseParameter_isModifiable,
    relationalDatabaseParameter_allowedValues,
    relationalDatabaseParameter_parameterName,
    relationalDatabaseParameter_description,

    -- ** RelationalDatabaseSnapshot
    relationalDatabaseSnapshot_engineVersion,
    relationalDatabaseSnapshot_state,
    relationalDatabaseSnapshot_fromRelationalDatabaseName,
    relationalDatabaseSnapshot_resourceType,
    relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId,
    relationalDatabaseSnapshot_arn,
    relationalDatabaseSnapshot_createdAt,
    relationalDatabaseSnapshot_location,
    relationalDatabaseSnapshot_engine,
    relationalDatabaseSnapshot_name,
    relationalDatabaseSnapshot_sizeInGb,
    relationalDatabaseSnapshot_supportCode,
    relationalDatabaseSnapshot_fromRelationalDatabaseArn,
    relationalDatabaseSnapshot_fromRelationalDatabaseBundleId,
    relationalDatabaseSnapshot_tags,

    -- ** RenewalSummary
    renewalSummary_renewalStatus,
    renewalSummary_domainValidationRecords,
    renewalSummary_updatedAt,
    renewalSummary_renewalStatusReason,

    -- ** ResourceLocation
    resourceLocation_regionName,
    resourceLocation_availabilityZone,

    -- ** ResourceReceivingAccess
    resourceReceivingAccess_resourceType,
    resourceReceivingAccess_name,

    -- ** ResourceRecord
    resourceRecord_value,
    resourceRecord_name,
    resourceRecord_type,

    -- ** StaticIp
    staticIp_ipAddress,
    staticIp_resourceType,
    staticIp_arn,
    staticIp_createdAt,
    staticIp_location,
    staticIp_isAttached,
    staticIp_name,
    staticIp_supportCode,
    staticIp_attachedTo,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Amazonka.Lightsail.AllocateStaticIp
import Amazonka.Lightsail.AttachCertificateToDistribution
import Amazonka.Lightsail.AttachDisk
import Amazonka.Lightsail.AttachInstancesToLoadBalancer
import Amazonka.Lightsail.AttachLoadBalancerTlsCertificate
import Amazonka.Lightsail.AttachStaticIp
import Amazonka.Lightsail.CloseInstancePublicPorts
import Amazonka.Lightsail.CopySnapshot
import Amazonka.Lightsail.CreateBucket
import Amazonka.Lightsail.CreateBucketAccessKey
import Amazonka.Lightsail.CreateCertificate
import Amazonka.Lightsail.CreateCloudFormationStack
import Amazonka.Lightsail.CreateContactMethod
import Amazonka.Lightsail.CreateContainerService
import Amazonka.Lightsail.CreateContainerServiceDeployment
import Amazonka.Lightsail.CreateContainerServiceRegistryLogin
import Amazonka.Lightsail.CreateDisk
import Amazonka.Lightsail.CreateDiskFromSnapshot
import Amazonka.Lightsail.CreateDiskSnapshot
import Amazonka.Lightsail.CreateDistribution
import Amazonka.Lightsail.CreateDomain
import Amazonka.Lightsail.CreateDomainEntry
import Amazonka.Lightsail.CreateInstanceSnapshot
import Amazonka.Lightsail.CreateInstances
import Amazonka.Lightsail.CreateInstancesFromSnapshot
import Amazonka.Lightsail.CreateKeyPair
import Amazonka.Lightsail.CreateLoadBalancer
import Amazonka.Lightsail.CreateLoadBalancerTlsCertificate
import Amazonka.Lightsail.CreateRelationalDatabase
import Amazonka.Lightsail.CreateRelationalDatabaseFromSnapshot
import Amazonka.Lightsail.CreateRelationalDatabaseSnapshot
import Amazonka.Lightsail.DeleteAlarm
import Amazonka.Lightsail.DeleteAutoSnapshot
import Amazonka.Lightsail.DeleteBucket
import Amazonka.Lightsail.DeleteBucketAccessKey
import Amazonka.Lightsail.DeleteCertificate
import Amazonka.Lightsail.DeleteContactMethod
import Amazonka.Lightsail.DeleteContainerImage
import Amazonka.Lightsail.DeleteContainerService
import Amazonka.Lightsail.DeleteDisk
import Amazonka.Lightsail.DeleteDiskSnapshot
import Amazonka.Lightsail.DeleteDistribution
import Amazonka.Lightsail.DeleteDomain
import Amazonka.Lightsail.DeleteDomainEntry
import Amazonka.Lightsail.DeleteInstance
import Amazonka.Lightsail.DeleteInstanceSnapshot
import Amazonka.Lightsail.DeleteKeyPair
import Amazonka.Lightsail.DeleteKnownHostKeys
import Amazonka.Lightsail.DeleteLoadBalancer
import Amazonka.Lightsail.DeleteLoadBalancerTlsCertificate
import Amazonka.Lightsail.DeleteRelationalDatabase
import Amazonka.Lightsail.DeleteRelationalDatabaseSnapshot
import Amazonka.Lightsail.DetachCertificateFromDistribution
import Amazonka.Lightsail.DetachDisk
import Amazonka.Lightsail.DetachInstancesFromLoadBalancer
import Amazonka.Lightsail.DetachStaticIp
import Amazonka.Lightsail.DisableAddOn
import Amazonka.Lightsail.DownloadDefaultKeyPair
import Amazonka.Lightsail.EnableAddOn
import Amazonka.Lightsail.ExportSnapshot
import Amazonka.Lightsail.GetActiveNames
import Amazonka.Lightsail.GetAlarms
import Amazonka.Lightsail.GetAutoSnapshots
import Amazonka.Lightsail.GetBlueprints
import Amazonka.Lightsail.GetBucketAccessKeys
import Amazonka.Lightsail.GetBucketBundles
import Amazonka.Lightsail.GetBucketMetricData
import Amazonka.Lightsail.GetBuckets
import Amazonka.Lightsail.GetBundles
import Amazonka.Lightsail.GetCertificates
import Amazonka.Lightsail.GetCloudFormationStackRecords
import Amazonka.Lightsail.GetContactMethods
import Amazonka.Lightsail.GetContainerAPIMetadata
import Amazonka.Lightsail.GetContainerImages
import Amazonka.Lightsail.GetContainerLog
import Amazonka.Lightsail.GetContainerServiceDeployments
import Amazonka.Lightsail.GetContainerServiceMetricData
import Amazonka.Lightsail.GetContainerServicePowers
import Amazonka.Lightsail.GetContainerServices
import Amazonka.Lightsail.GetDisk
import Amazonka.Lightsail.GetDiskSnapshot
import Amazonka.Lightsail.GetDiskSnapshots
import Amazonka.Lightsail.GetDisks
import Amazonka.Lightsail.GetDistributionBundles
import Amazonka.Lightsail.GetDistributionLatestCacheReset
import Amazonka.Lightsail.GetDistributionMetricData
import Amazonka.Lightsail.GetDistributions
import Amazonka.Lightsail.GetDomain
import Amazonka.Lightsail.GetDomains
import Amazonka.Lightsail.GetExportSnapshotRecords
import Amazonka.Lightsail.GetInstance
import Amazonka.Lightsail.GetInstanceAccessDetails
import Amazonka.Lightsail.GetInstanceMetricData
import Amazonka.Lightsail.GetInstancePortStates
import Amazonka.Lightsail.GetInstanceSnapshot
import Amazonka.Lightsail.GetInstanceSnapshots
import Amazonka.Lightsail.GetInstanceState
import Amazonka.Lightsail.GetInstances
import Amazonka.Lightsail.GetKeyPair
import Amazonka.Lightsail.GetKeyPairs
import Amazonka.Lightsail.GetLoadBalancer
import Amazonka.Lightsail.GetLoadBalancerMetricData
import Amazonka.Lightsail.GetLoadBalancerTlsCertificates
import Amazonka.Lightsail.GetLoadBalancers
import Amazonka.Lightsail.GetOperation
import Amazonka.Lightsail.GetOperations
import Amazonka.Lightsail.GetOperationsForResource
import Amazonka.Lightsail.GetRegions
import Amazonka.Lightsail.GetRelationalDatabase
import Amazonka.Lightsail.GetRelationalDatabaseBlueprints
import Amazonka.Lightsail.GetRelationalDatabaseBundles
import Amazonka.Lightsail.GetRelationalDatabaseEvents
import Amazonka.Lightsail.GetRelationalDatabaseLogEvents
import Amazonka.Lightsail.GetRelationalDatabaseLogStreams
import Amazonka.Lightsail.GetRelationalDatabaseMasterUserPassword
import Amazonka.Lightsail.GetRelationalDatabaseMetricData
import Amazonka.Lightsail.GetRelationalDatabaseParameters
import Amazonka.Lightsail.GetRelationalDatabaseSnapshot
import Amazonka.Lightsail.GetRelationalDatabaseSnapshots
import Amazonka.Lightsail.GetRelationalDatabases
import Amazonka.Lightsail.GetStaticIp
import Amazonka.Lightsail.GetStaticIps
import Amazonka.Lightsail.ImportKeyPair
import Amazonka.Lightsail.IsVpcPeered
import Amazonka.Lightsail.OpenInstancePublicPorts
import Amazonka.Lightsail.PeerVpc
import Amazonka.Lightsail.PutAlarm
import Amazonka.Lightsail.PutInstancePublicPorts
import Amazonka.Lightsail.RebootInstance
import Amazonka.Lightsail.RebootRelationalDatabase
import Amazonka.Lightsail.RegisterContainerImage
import Amazonka.Lightsail.ReleaseStaticIp
import Amazonka.Lightsail.ResetDistributionCache
import Amazonka.Lightsail.SendContactMethodVerification
import Amazonka.Lightsail.SetIpAddressType
import Amazonka.Lightsail.SetResourceAccessForBucket
import Amazonka.Lightsail.StartInstance
import Amazonka.Lightsail.StartRelationalDatabase
import Amazonka.Lightsail.StopInstance
import Amazonka.Lightsail.StopRelationalDatabase
import Amazonka.Lightsail.TagResource
import Amazonka.Lightsail.TestAlarm
import Amazonka.Lightsail.Types.AccessKey
import Amazonka.Lightsail.Types.AccessKeyLastUsed
import Amazonka.Lightsail.Types.AccessRules
import Amazonka.Lightsail.Types.AddOn
import Amazonka.Lightsail.Types.AddOnRequest
import Amazonka.Lightsail.Types.Alarm
import Amazonka.Lightsail.Types.AttachedDisk
import Amazonka.Lightsail.Types.AutoSnapshotAddOnRequest
import Amazonka.Lightsail.Types.AutoSnapshotDetails
import Amazonka.Lightsail.Types.AvailabilityZone
import Amazonka.Lightsail.Types.Blueprint
import Amazonka.Lightsail.Types.Bucket
import Amazonka.Lightsail.Types.BucketBundle
import Amazonka.Lightsail.Types.BucketState
import Amazonka.Lightsail.Types.Bundle
import Amazonka.Lightsail.Types.CacheBehavior
import Amazonka.Lightsail.Types.CacheBehaviorPerPath
import Amazonka.Lightsail.Types.CacheSettings
import Amazonka.Lightsail.Types.Certificate
import Amazonka.Lightsail.Types.CertificateSummary
import Amazonka.Lightsail.Types.CloudFormationStackRecord
import Amazonka.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Amazonka.Lightsail.Types.ContactMethod
import Amazonka.Lightsail.Types.Container
import Amazonka.Lightsail.Types.ContainerImage
import Amazonka.Lightsail.Types.ContainerService
import Amazonka.Lightsail.Types.ContainerServiceDeployment
import Amazonka.Lightsail.Types.ContainerServiceDeploymentRequest
import Amazonka.Lightsail.Types.ContainerServiceEndpoint
import Amazonka.Lightsail.Types.ContainerServiceHealthCheckConfig
import Amazonka.Lightsail.Types.ContainerServiceLogEvent
import Amazonka.Lightsail.Types.ContainerServicePower
import Amazonka.Lightsail.Types.ContainerServiceRegistryLogin
import Amazonka.Lightsail.Types.ContainerServiceStateDetail
import Amazonka.Lightsail.Types.CookieObject
import Amazonka.Lightsail.Types.DestinationInfo
import Amazonka.Lightsail.Types.Disk
import Amazonka.Lightsail.Types.DiskInfo
import Amazonka.Lightsail.Types.DiskMap
import Amazonka.Lightsail.Types.DiskSnapshot
import Amazonka.Lightsail.Types.DiskSnapshotInfo
import Amazonka.Lightsail.Types.DistributionBundle
import Amazonka.Lightsail.Types.Domain
import Amazonka.Lightsail.Types.DomainEntry
import Amazonka.Lightsail.Types.DomainValidationRecord
import Amazonka.Lightsail.Types.EndpointRequest
import Amazonka.Lightsail.Types.ExportSnapshotRecord
import Amazonka.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Amazonka.Lightsail.Types.HeaderObject
import Amazonka.Lightsail.Types.HostKeyAttributes
import Amazonka.Lightsail.Types.InputOrigin
import Amazonka.Lightsail.Types.Instance
import Amazonka.Lightsail.Types.InstanceAccessDetails
import Amazonka.Lightsail.Types.InstanceEntry
import Amazonka.Lightsail.Types.InstanceHardware
import Amazonka.Lightsail.Types.InstanceHealthSummary
import Amazonka.Lightsail.Types.InstanceNetworking
import Amazonka.Lightsail.Types.InstancePortInfo
import Amazonka.Lightsail.Types.InstancePortState
import Amazonka.Lightsail.Types.InstanceSnapshot
import Amazonka.Lightsail.Types.InstanceSnapshotInfo
import Amazonka.Lightsail.Types.InstanceState
import Amazonka.Lightsail.Types.KeyPair
import Amazonka.Lightsail.Types.LightsailDistribution
import Amazonka.Lightsail.Types.LoadBalancer
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificate
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateSummary
import Amazonka.Lightsail.Types.LogEvent
import Amazonka.Lightsail.Types.MetricDatapoint
import Amazonka.Lightsail.Types.MonitoredResourceInfo
import Amazonka.Lightsail.Types.MonthlyTransfer
import Amazonka.Lightsail.Types.Operation
import Amazonka.Lightsail.Types.Origin
import Amazonka.Lightsail.Types.PasswordData
import Amazonka.Lightsail.Types.PendingMaintenanceAction
import Amazonka.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Amazonka.Lightsail.Types.PortInfo
import Amazonka.Lightsail.Types.QueryStringObject
import Amazonka.Lightsail.Types.RegionInfo
import Amazonka.Lightsail.Types.RelationalDatabase
import Amazonka.Lightsail.Types.RelationalDatabaseBlueprint
import Amazonka.Lightsail.Types.RelationalDatabaseBundle
import Amazonka.Lightsail.Types.RelationalDatabaseEndpoint
import Amazonka.Lightsail.Types.RelationalDatabaseEvent
import Amazonka.Lightsail.Types.RelationalDatabaseHardware
import Amazonka.Lightsail.Types.RelationalDatabaseParameter
import Amazonka.Lightsail.Types.RelationalDatabaseSnapshot
import Amazonka.Lightsail.Types.RenewalSummary
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceReceivingAccess
import Amazonka.Lightsail.Types.ResourceRecord
import Amazonka.Lightsail.Types.StaticIp
import Amazonka.Lightsail.Types.Tag
import Amazonka.Lightsail.UnpeerVpc
import Amazonka.Lightsail.UntagResource
import Amazonka.Lightsail.UpdateBucket
import Amazonka.Lightsail.UpdateBucketBundle
import Amazonka.Lightsail.UpdateContainerService
import Amazonka.Lightsail.UpdateDistribution
import Amazonka.Lightsail.UpdateDistributionBundle
import Amazonka.Lightsail.UpdateDomainEntry
import Amazonka.Lightsail.UpdateLoadBalancerAttribute
import Amazonka.Lightsail.UpdateRelationalDatabase
import Amazonka.Lightsail.UpdateRelationalDatabaseParameters
