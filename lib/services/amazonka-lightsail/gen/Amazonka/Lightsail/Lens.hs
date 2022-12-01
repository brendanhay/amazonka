{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Lens
  ( -- * Operations

    -- ** AllocateStaticIp
    allocateStaticIp_staticIpName,
    allocateStaticIpResponse_operations,
    allocateStaticIpResponse_httpStatus,

    -- ** AttachCertificateToDistribution
    attachCertificateToDistribution_distributionName,
    attachCertificateToDistribution_certificateName,
    attachCertificateToDistributionResponse_operation,
    attachCertificateToDistributionResponse_httpStatus,

    -- ** AttachDisk
    attachDisk_diskName,
    attachDisk_instanceName,
    attachDisk_diskPath,
    attachDiskResponse_operations,
    attachDiskResponse_httpStatus,

    -- ** AttachInstancesToLoadBalancer
    attachInstancesToLoadBalancer_loadBalancerName,
    attachInstancesToLoadBalancer_instanceNames,
    attachInstancesToLoadBalancerResponse_operations,
    attachInstancesToLoadBalancerResponse_httpStatus,

    -- ** AttachLoadBalancerTlsCertificate
    attachLoadBalancerTlsCertificate_loadBalancerName,
    attachLoadBalancerTlsCertificate_certificateName,
    attachLoadBalancerTlsCertificateResponse_operations,
    attachLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** AttachStaticIp
    attachStaticIp_staticIpName,
    attachStaticIp_instanceName,
    attachStaticIpResponse_operations,
    attachStaticIpResponse_httpStatus,

    -- ** CloseInstancePublicPorts
    closeInstancePublicPorts_portInfo,
    closeInstancePublicPorts_instanceName,
    closeInstancePublicPortsResponse_operation,
    closeInstancePublicPortsResponse_httpStatus,

    -- ** CopySnapshot
    copySnapshot_useLatestRestorableAutoSnapshot,
    copySnapshot_sourceResourceName,
    copySnapshot_restoreDate,
    copySnapshot_sourceSnapshotName,
    copySnapshot_targetSnapshotName,
    copySnapshot_sourceRegion,
    copySnapshotResponse_operations,
    copySnapshotResponse_httpStatus,

    -- ** CreateBucket
    createBucket_tags,
    createBucket_enableObjectVersioning,
    createBucket_bucketName,
    createBucket_bundleId,
    createBucketResponse_operations,
    createBucketResponse_bucket,
    createBucketResponse_httpStatus,

    -- ** CreateBucketAccessKey
    createBucketAccessKey_bucketName,
    createBucketAccessKeyResponse_operations,
    createBucketAccessKeyResponse_accessKey,
    createBucketAccessKeyResponse_httpStatus,

    -- ** CreateCertificate
    createCertificate_tags,
    createCertificate_subjectAlternativeNames,
    createCertificate_certificateName,
    createCertificate_domainName,
    createCertificateResponse_operations,
    createCertificateResponse_certificate,
    createCertificateResponse_httpStatus,

    -- ** CreateCloudFormationStack
    createCloudFormationStack_instances,
    createCloudFormationStackResponse_operations,
    createCloudFormationStackResponse_httpStatus,

    -- ** CreateContactMethod
    createContactMethod_protocol,
    createContactMethod_contactEndpoint,
    createContactMethodResponse_operations,
    createContactMethodResponse_httpStatus,

    -- ** CreateContainerService
    createContainerService_tags,
    createContainerService_deployment,
    createContainerService_publicDomainNames,
    createContainerService_privateRegistryAccess,
    createContainerService_serviceName,
    createContainerService_power,
    createContainerService_scale,
    createContainerServiceResponse_containerService,
    createContainerServiceResponse_httpStatus,

    -- ** CreateContainerServiceDeployment
    createContainerServiceDeployment_containers,
    createContainerServiceDeployment_publicEndpoint,
    createContainerServiceDeployment_serviceName,
    createContainerServiceDeploymentResponse_containerService,
    createContainerServiceDeploymentResponse_httpStatus,

    -- ** CreateContainerServiceRegistryLogin
    createContainerServiceRegistryLoginResponse_registryLogin,
    createContainerServiceRegistryLoginResponse_httpStatus,

    -- ** CreateDisk
    createDisk_tags,
    createDisk_addOns,
    createDisk_diskName,
    createDisk_availabilityZone,
    createDisk_sizeInGb,
    createDiskResponse_operations,
    createDiskResponse_httpStatus,

    -- ** CreateDiskFromSnapshot
    createDiskFromSnapshot_tags,
    createDiskFromSnapshot_useLatestRestorableAutoSnapshot,
    createDiskFromSnapshot_sourceDiskName,
    createDiskFromSnapshot_diskSnapshotName,
    createDiskFromSnapshot_restoreDate,
    createDiskFromSnapshot_addOns,
    createDiskFromSnapshot_diskName,
    createDiskFromSnapshot_availabilityZone,
    createDiskFromSnapshot_sizeInGb,
    createDiskFromSnapshotResponse_operations,
    createDiskFromSnapshotResponse_httpStatus,

    -- ** CreateDiskSnapshot
    createDiskSnapshot_tags,
    createDiskSnapshot_instanceName,
    createDiskSnapshot_diskName,
    createDiskSnapshot_diskSnapshotName,
    createDiskSnapshotResponse_operations,
    createDiskSnapshotResponse_httpStatus,

    -- ** CreateDistribution
    createDistribution_tags,
    createDistribution_cacheBehaviorSettings,
    createDistribution_cacheBehaviors,
    createDistribution_ipAddressType,
    createDistribution_distributionName,
    createDistribution_origin,
    createDistribution_defaultCacheBehavior,
    createDistribution_bundleId,
    createDistributionResponse_distribution,
    createDistributionResponse_operation,
    createDistributionResponse_httpStatus,

    -- ** CreateDomain
    createDomain_tags,
    createDomain_domainName,
    createDomainResponse_operation,
    createDomainResponse_httpStatus,

    -- ** CreateDomainEntry
    createDomainEntry_domainName,
    createDomainEntry_domainEntry,
    createDomainEntryResponse_operation,
    createDomainEntryResponse_httpStatus,

    -- ** CreateInstanceSnapshot
    createInstanceSnapshot_tags,
    createInstanceSnapshot_instanceSnapshotName,
    createInstanceSnapshot_instanceName,
    createInstanceSnapshotResponse_operations,
    createInstanceSnapshotResponse_httpStatus,

    -- ** CreateInstances
    createInstances_tags,
    createInstances_userData,
    createInstances_customImageName,
    createInstances_addOns,
    createInstances_keyPairName,
    createInstances_ipAddressType,
    createInstances_instanceNames,
    createInstances_availabilityZone,
    createInstances_blueprintId,
    createInstances_bundleId,
    createInstancesResponse_operations,
    createInstancesResponse_httpStatus,

    -- ** CreateInstancesFromSnapshot
    createInstancesFromSnapshot_tags,
    createInstancesFromSnapshot_userData,
    createInstancesFromSnapshot_useLatestRestorableAutoSnapshot,
    createInstancesFromSnapshot_attachedDiskMapping,
    createInstancesFromSnapshot_instanceSnapshotName,
    createInstancesFromSnapshot_sourceInstanceName,
    createInstancesFromSnapshot_restoreDate,
    createInstancesFromSnapshot_addOns,
    createInstancesFromSnapshot_keyPairName,
    createInstancesFromSnapshot_ipAddressType,
    createInstancesFromSnapshot_instanceNames,
    createInstancesFromSnapshot_availabilityZone,
    createInstancesFromSnapshot_bundleId,
    createInstancesFromSnapshotResponse_operations,
    createInstancesFromSnapshotResponse_httpStatus,

    -- ** CreateKeyPair
    createKeyPair_tags,
    createKeyPair_keyPairName,
    createKeyPairResponse_publicKeyBase64,
    createKeyPairResponse_keyPair,
    createKeyPairResponse_privateKeyBase64,
    createKeyPairResponse_operation,
    createKeyPairResponse_httpStatus,

    -- ** CreateLoadBalancer
    createLoadBalancer_tags,
    createLoadBalancer_healthCheckPath,
    createLoadBalancer_certificateAlternativeNames,
    createLoadBalancer_certificateName,
    createLoadBalancer_ipAddressType,
    createLoadBalancer_certificateDomainName,
    createLoadBalancer_tlsPolicyName,
    createLoadBalancer_loadBalancerName,
    createLoadBalancer_instancePort,
    createLoadBalancerResponse_operations,
    createLoadBalancerResponse_httpStatus,

    -- ** CreateLoadBalancerTlsCertificate
    createLoadBalancerTlsCertificate_tags,
    createLoadBalancerTlsCertificate_certificateAlternativeNames,
    createLoadBalancerTlsCertificate_loadBalancerName,
    createLoadBalancerTlsCertificate_certificateName,
    createLoadBalancerTlsCertificate_certificateDomainName,
    createLoadBalancerTlsCertificateResponse_operations,
    createLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** CreateRelationalDatabase
    createRelationalDatabase_tags,
    createRelationalDatabase_preferredBackupWindow,
    createRelationalDatabase_availabilityZone,
    createRelationalDatabase_publiclyAccessible,
    createRelationalDatabase_masterUserPassword,
    createRelationalDatabase_preferredMaintenanceWindow,
    createRelationalDatabase_relationalDatabaseName,
    createRelationalDatabase_relationalDatabaseBlueprintId,
    createRelationalDatabase_relationalDatabaseBundleId,
    createRelationalDatabase_masterDatabaseName,
    createRelationalDatabase_masterUsername,
    createRelationalDatabaseResponse_operations,
    createRelationalDatabaseResponse_httpStatus,

    -- ** CreateRelationalDatabaseFromSnapshot
    createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId,
    createRelationalDatabaseFromSnapshot_tags,
    createRelationalDatabaseFromSnapshot_restoreTime,
    createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName,
    createRelationalDatabaseFromSnapshot_availabilityZone,
    createRelationalDatabaseFromSnapshot_publiclyAccessible,
    createRelationalDatabaseFromSnapshot_useLatestRestorableTime,
    createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName,
    createRelationalDatabaseFromSnapshot_relationalDatabaseName,
    createRelationalDatabaseFromSnapshotResponse_operations,
    createRelationalDatabaseFromSnapshotResponse_httpStatus,

    -- ** CreateRelationalDatabaseSnapshot
    createRelationalDatabaseSnapshot_tags,
    createRelationalDatabaseSnapshot_relationalDatabaseName,
    createRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,
    createRelationalDatabaseSnapshotResponse_operations,
    createRelationalDatabaseSnapshotResponse_httpStatus,

    -- ** DeleteAlarm
    deleteAlarm_alarmName,
    deleteAlarmResponse_operations,
    deleteAlarmResponse_httpStatus,

    -- ** DeleteAutoSnapshot
    deleteAutoSnapshot_resourceName,
    deleteAutoSnapshot_date,
    deleteAutoSnapshotResponse_operations,
    deleteAutoSnapshotResponse_httpStatus,

    -- ** DeleteBucket
    deleteBucket_forceDelete,
    deleteBucket_bucketName,
    deleteBucketResponse_operations,
    deleteBucketResponse_httpStatus,

    -- ** DeleteBucketAccessKey
    deleteBucketAccessKey_bucketName,
    deleteBucketAccessKey_accessKeyId,
    deleteBucketAccessKeyResponse_operations,
    deleteBucketAccessKeyResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_certificateName,
    deleteCertificateResponse_operations,
    deleteCertificateResponse_httpStatus,

    -- ** DeleteContactMethod
    deleteContactMethod_protocol,
    deleteContactMethodResponse_operations,
    deleteContactMethodResponse_httpStatus,

    -- ** DeleteContainerImage
    deleteContainerImage_serviceName,
    deleteContainerImage_image,
    deleteContainerImageResponse_httpStatus,

    -- ** DeleteContainerService
    deleteContainerService_serviceName,
    deleteContainerServiceResponse_httpStatus,

    -- ** DeleteDisk
    deleteDisk_forceDeleteAddOns,
    deleteDisk_diskName,
    deleteDiskResponse_operations,
    deleteDiskResponse_httpStatus,

    -- ** DeleteDiskSnapshot
    deleteDiskSnapshot_diskSnapshotName,
    deleteDiskSnapshotResponse_operations,
    deleteDiskSnapshotResponse_httpStatus,

    -- ** DeleteDistribution
    deleteDistribution_distributionName,
    deleteDistributionResponse_operation,
    deleteDistributionResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainName,
    deleteDomainResponse_operation,
    deleteDomainResponse_httpStatus,

    -- ** DeleteDomainEntry
    deleteDomainEntry_domainName,
    deleteDomainEntry_domainEntry,
    deleteDomainEntryResponse_operation,
    deleteDomainEntryResponse_httpStatus,

    -- ** DeleteInstance
    deleteInstance_forceDeleteAddOns,
    deleteInstance_instanceName,
    deleteInstanceResponse_operations,
    deleteInstanceResponse_httpStatus,

    -- ** DeleteInstanceSnapshot
    deleteInstanceSnapshot_instanceSnapshotName,
    deleteInstanceSnapshotResponse_operations,
    deleteInstanceSnapshotResponse_httpStatus,

    -- ** DeleteKeyPair
    deleteKeyPair_expectedFingerprint,
    deleteKeyPair_keyPairName,
    deleteKeyPairResponse_operation,
    deleteKeyPairResponse_httpStatus,

    -- ** DeleteKnownHostKeys
    deleteKnownHostKeys_instanceName,
    deleteKnownHostKeysResponse_operations,
    deleteKnownHostKeysResponse_httpStatus,

    -- ** DeleteLoadBalancer
    deleteLoadBalancer_loadBalancerName,
    deleteLoadBalancerResponse_operations,
    deleteLoadBalancerResponse_httpStatus,

    -- ** DeleteLoadBalancerTlsCertificate
    deleteLoadBalancerTlsCertificate_force,
    deleteLoadBalancerTlsCertificate_loadBalancerName,
    deleteLoadBalancerTlsCertificate_certificateName,
    deleteLoadBalancerTlsCertificateResponse_operations,
    deleteLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** DeleteRelationalDatabase
    deleteRelationalDatabase_finalRelationalDatabaseSnapshotName,
    deleteRelationalDatabase_skipFinalSnapshot,
    deleteRelationalDatabase_relationalDatabaseName,
    deleteRelationalDatabaseResponse_operations,
    deleteRelationalDatabaseResponse_httpStatus,

    -- ** DeleteRelationalDatabaseSnapshot
    deleteRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,
    deleteRelationalDatabaseSnapshotResponse_operations,
    deleteRelationalDatabaseSnapshotResponse_httpStatus,

    -- ** DetachCertificateFromDistribution
    detachCertificateFromDistribution_distributionName,
    detachCertificateFromDistributionResponse_operation,
    detachCertificateFromDistributionResponse_httpStatus,

    -- ** DetachDisk
    detachDisk_diskName,
    detachDiskResponse_operations,
    detachDiskResponse_httpStatus,

    -- ** DetachInstancesFromLoadBalancer
    detachInstancesFromLoadBalancer_loadBalancerName,
    detachInstancesFromLoadBalancer_instanceNames,
    detachInstancesFromLoadBalancerResponse_operations,
    detachInstancesFromLoadBalancerResponse_httpStatus,

    -- ** DetachStaticIp
    detachStaticIp_staticIpName,
    detachStaticIpResponse_operations,
    detachStaticIpResponse_httpStatus,

    -- ** DisableAddOn
    disableAddOn_addOnType,
    disableAddOn_resourceName,
    disableAddOnResponse_operations,
    disableAddOnResponse_httpStatus,

    -- ** DownloadDefaultKeyPair
    downloadDefaultKeyPairResponse_publicKeyBase64,
    downloadDefaultKeyPairResponse_privateKeyBase64,
    downloadDefaultKeyPairResponse_createdAt,
    downloadDefaultKeyPairResponse_httpStatus,

    -- ** EnableAddOn
    enableAddOn_resourceName,
    enableAddOn_addOnRequest,
    enableAddOnResponse_operations,
    enableAddOnResponse_httpStatus,

    -- ** ExportSnapshot
    exportSnapshot_sourceSnapshotName,
    exportSnapshotResponse_operations,
    exportSnapshotResponse_httpStatus,

    -- ** GetActiveNames
    getActiveNames_pageToken,
    getActiveNamesResponse_nextPageToken,
    getActiveNamesResponse_activeNames,
    getActiveNamesResponse_httpStatus,

    -- ** GetAlarms
    getAlarms_pageToken,
    getAlarms_alarmName,
    getAlarms_monitoredResourceName,
    getAlarmsResponse_alarms,
    getAlarmsResponse_nextPageToken,
    getAlarmsResponse_httpStatus,

    -- ** GetAutoSnapshots
    getAutoSnapshots_resourceName,
    getAutoSnapshotsResponse_resourceType,
    getAutoSnapshotsResponse_resourceName,
    getAutoSnapshotsResponse_autoSnapshots,
    getAutoSnapshotsResponse_httpStatus,

    -- ** GetBlueprints
    getBlueprints_includeInactive,
    getBlueprints_pageToken,
    getBlueprintsResponse_nextPageToken,
    getBlueprintsResponse_blueprints,
    getBlueprintsResponse_httpStatus,

    -- ** GetBucketAccessKeys
    getBucketAccessKeys_bucketName,
    getBucketAccessKeysResponse_accessKeys,
    getBucketAccessKeysResponse_httpStatus,

    -- ** GetBucketBundles
    getBucketBundles_includeInactive,
    getBucketBundlesResponse_bundles,
    getBucketBundlesResponse_httpStatus,

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

    -- ** GetBuckets
    getBuckets_includeConnectedResources,
    getBuckets_pageToken,
    getBuckets_bucketName,
    getBucketsResponse_nextPageToken,
    getBucketsResponse_accountLevelBpaSync,
    getBucketsResponse_buckets,
    getBucketsResponse_httpStatus,

    -- ** GetBundles
    getBundles_includeInactive,
    getBundles_pageToken,
    getBundlesResponse_nextPageToken,
    getBundlesResponse_bundles,
    getBundlesResponse_httpStatus,

    -- ** GetCertificates
    getCertificates_includeCertificateDetails,
    getCertificates_certificateName,
    getCertificates_certificateStatuses,
    getCertificatesResponse_certificates,
    getCertificatesResponse_httpStatus,

    -- ** GetCloudFormationStackRecords
    getCloudFormationStackRecords_pageToken,
    getCloudFormationStackRecordsResponse_nextPageToken,
    getCloudFormationStackRecordsResponse_cloudFormationStackRecords,
    getCloudFormationStackRecordsResponse_httpStatus,

    -- ** GetContactMethods
    getContactMethods_protocols,
    getContactMethodsResponse_contactMethods,
    getContactMethodsResponse_httpStatus,

    -- ** GetContainerAPIMetadata
    getContainerAPIMetadataResponse_metadata,
    getContainerAPIMetadataResponse_httpStatus,

    -- ** GetContainerImages
    getContainerImages_serviceName,
    getContainerImagesResponse_containerImages,
    getContainerImagesResponse_httpStatus,

    -- ** GetContainerLog
    getContainerLog_pageToken,
    getContainerLog_endTime,
    getContainerLog_filterPattern,
    getContainerLog_startTime,
    getContainerLog_serviceName,
    getContainerLog_containerName,
    getContainerLogResponse_logEvents,
    getContainerLogResponse_nextPageToken,
    getContainerLogResponse_httpStatus,

    -- ** GetContainerServiceDeployments
    getContainerServiceDeployments_serviceName,
    getContainerServiceDeploymentsResponse_deployments,
    getContainerServiceDeploymentsResponse_httpStatus,

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

    -- ** GetContainerServicePowers
    getContainerServicePowersResponse_powers,
    getContainerServicePowersResponse_httpStatus,

    -- ** GetContainerServices
    getContainerServices_serviceName,
    getContainerServicesResponse_containerServices,
    getContainerServicesResponse_httpStatus,

    -- ** GetDisk
    getDisk_diskName,
    getDiskResponse_disk,
    getDiskResponse_httpStatus,

    -- ** GetDiskSnapshot
    getDiskSnapshot_diskSnapshotName,
    getDiskSnapshotResponse_diskSnapshot,
    getDiskSnapshotResponse_httpStatus,

    -- ** GetDiskSnapshots
    getDiskSnapshots_pageToken,
    getDiskSnapshotsResponse_nextPageToken,
    getDiskSnapshotsResponse_diskSnapshots,
    getDiskSnapshotsResponse_httpStatus,

    -- ** GetDisks
    getDisks_pageToken,
    getDisksResponse_nextPageToken,
    getDisksResponse_disks,
    getDisksResponse_httpStatus,

    -- ** GetDistributionBundles
    getDistributionBundlesResponse_bundles,
    getDistributionBundlesResponse_httpStatus,

    -- ** GetDistributionLatestCacheReset
    getDistributionLatestCacheReset_distributionName,
    getDistributionLatestCacheResetResponse_status,
    getDistributionLatestCacheResetResponse_createTime,
    getDistributionLatestCacheResetResponse_httpStatus,

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

    -- ** GetDistributions
    getDistributions_pageToken,
    getDistributions_distributionName,
    getDistributionsResponse_nextPageToken,
    getDistributionsResponse_distributions,
    getDistributionsResponse_httpStatus,

    -- ** GetDomain
    getDomain_domainName,
    getDomainResponse_domain,
    getDomainResponse_httpStatus,

    -- ** GetDomains
    getDomains_pageToken,
    getDomainsResponse_domains,
    getDomainsResponse_nextPageToken,
    getDomainsResponse_httpStatus,

    -- ** GetExportSnapshotRecords
    getExportSnapshotRecords_pageToken,
    getExportSnapshotRecordsResponse_exportSnapshotRecords,
    getExportSnapshotRecordsResponse_nextPageToken,
    getExportSnapshotRecordsResponse_httpStatus,

    -- ** GetInstance
    getInstance_instanceName,
    getInstanceResponse_instance,
    getInstanceResponse_httpStatus,

    -- ** GetInstanceAccessDetails
    getInstanceAccessDetails_protocol,
    getInstanceAccessDetails_instanceName,
    getInstanceAccessDetailsResponse_accessDetails,
    getInstanceAccessDetailsResponse_httpStatus,

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

    -- ** GetInstancePortStates
    getInstancePortStates_instanceName,
    getInstancePortStatesResponse_portStates,
    getInstancePortStatesResponse_httpStatus,

    -- ** GetInstanceSnapshot
    getInstanceSnapshot_instanceSnapshotName,
    getInstanceSnapshotResponse_instanceSnapshot,
    getInstanceSnapshotResponse_httpStatus,

    -- ** GetInstanceSnapshots
    getInstanceSnapshots_pageToken,
    getInstanceSnapshotsResponse_nextPageToken,
    getInstanceSnapshotsResponse_instanceSnapshots,
    getInstanceSnapshotsResponse_httpStatus,

    -- ** GetInstanceState
    getInstanceState_instanceName,
    getInstanceStateResponse_state,
    getInstanceStateResponse_httpStatus,

    -- ** GetInstances
    getInstances_pageToken,
    getInstancesResponse_instances,
    getInstancesResponse_nextPageToken,
    getInstancesResponse_httpStatus,

    -- ** GetKeyPair
    getKeyPair_keyPairName,
    getKeyPairResponse_keyPair,
    getKeyPairResponse_httpStatus,

    -- ** GetKeyPairs
    getKeyPairs_includeDefaultKeyPair,
    getKeyPairs_pageToken,
    getKeyPairsResponse_keyPairs,
    getKeyPairsResponse_nextPageToken,
    getKeyPairsResponse_httpStatus,

    -- ** GetLoadBalancer
    getLoadBalancer_loadBalancerName,
    getLoadBalancerResponse_loadBalancer,
    getLoadBalancerResponse_httpStatus,

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

    -- ** GetLoadBalancerTlsCertificates
    getLoadBalancerTlsCertificates_loadBalancerName,
    getLoadBalancerTlsCertificatesResponse_tlsCertificates,
    getLoadBalancerTlsCertificatesResponse_httpStatus,

    -- ** GetLoadBalancerTlsPolicies
    getLoadBalancerTlsPolicies_pageToken,
    getLoadBalancerTlsPoliciesResponse_tlsPolicies,
    getLoadBalancerTlsPoliciesResponse_nextPageToken,
    getLoadBalancerTlsPoliciesResponse_httpStatus,

    -- ** GetLoadBalancers
    getLoadBalancers_pageToken,
    getLoadBalancersResponse_nextPageToken,
    getLoadBalancersResponse_loadBalancers,
    getLoadBalancersResponse_httpStatus,

    -- ** GetOperation
    getOperation_operationId,
    getOperationResponse_operation,
    getOperationResponse_httpStatus,

    -- ** GetOperations
    getOperations_pageToken,
    getOperationsResponse_operations,
    getOperationsResponse_nextPageToken,
    getOperationsResponse_httpStatus,

    -- ** GetOperationsForResource
    getOperationsForResource_pageToken,
    getOperationsForResource_resourceName,
    getOperationsForResourceResponse_operations,
    getOperationsForResourceResponse_nextPageToken,
    getOperationsForResourceResponse_nextPageCount,
    getOperationsForResourceResponse_httpStatus,

    -- ** GetRegions
    getRegions_includeRelationalDatabaseAvailabilityZones,
    getRegions_includeAvailabilityZones,
    getRegionsResponse_regions,
    getRegionsResponse_httpStatus,

    -- ** GetRelationalDatabase
    getRelationalDatabase_relationalDatabaseName,
    getRelationalDatabaseResponse_relationalDatabase,
    getRelationalDatabaseResponse_httpStatus,

    -- ** GetRelationalDatabaseBlueprints
    getRelationalDatabaseBlueprints_pageToken,
    getRelationalDatabaseBlueprintsResponse_nextPageToken,
    getRelationalDatabaseBlueprintsResponse_blueprints,
    getRelationalDatabaseBlueprintsResponse_httpStatus,

    -- ** GetRelationalDatabaseBundles
    getRelationalDatabaseBundles_includeInactive,
    getRelationalDatabaseBundles_pageToken,
    getRelationalDatabaseBundlesResponse_nextPageToken,
    getRelationalDatabaseBundlesResponse_bundles,
    getRelationalDatabaseBundlesResponse_httpStatus,

    -- ** GetRelationalDatabaseEvents
    getRelationalDatabaseEvents_pageToken,
    getRelationalDatabaseEvents_durationInMinutes,
    getRelationalDatabaseEvents_relationalDatabaseName,
    getRelationalDatabaseEventsResponse_nextPageToken,
    getRelationalDatabaseEventsResponse_relationalDatabaseEvents,
    getRelationalDatabaseEventsResponse_httpStatus,

    -- ** GetRelationalDatabaseLogEvents
    getRelationalDatabaseLogEvents_startFromHead,
    getRelationalDatabaseLogEvents_pageToken,
    getRelationalDatabaseLogEvents_endTime,
    getRelationalDatabaseLogEvents_startTime,
    getRelationalDatabaseLogEvents_relationalDatabaseName,
    getRelationalDatabaseLogEvents_logStreamName,
    getRelationalDatabaseLogEventsResponse_nextForwardToken,
    getRelationalDatabaseLogEventsResponse_resourceLogEvents,
    getRelationalDatabaseLogEventsResponse_nextBackwardToken,
    getRelationalDatabaseLogEventsResponse_httpStatus,

    -- ** GetRelationalDatabaseLogStreams
    getRelationalDatabaseLogStreams_relationalDatabaseName,
    getRelationalDatabaseLogStreamsResponse_logStreams,
    getRelationalDatabaseLogStreamsResponse_httpStatus,

    -- ** GetRelationalDatabaseMasterUserPassword
    getRelationalDatabaseMasterUserPassword_passwordVersion,
    getRelationalDatabaseMasterUserPassword_relationalDatabaseName,
    getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword,
    getRelationalDatabaseMasterUserPasswordResponse_createdAt,
    getRelationalDatabaseMasterUserPasswordResponse_httpStatus,

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

    -- ** GetRelationalDatabaseParameters
    getRelationalDatabaseParameters_pageToken,
    getRelationalDatabaseParameters_relationalDatabaseName,
    getRelationalDatabaseParametersResponse_nextPageToken,
    getRelationalDatabaseParametersResponse_parameters,
    getRelationalDatabaseParametersResponse_httpStatus,

    -- ** GetRelationalDatabaseSnapshot
    getRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,
    getRelationalDatabaseSnapshotResponse_relationalDatabaseSnapshot,
    getRelationalDatabaseSnapshotResponse_httpStatus,

    -- ** GetRelationalDatabaseSnapshots
    getRelationalDatabaseSnapshots_pageToken,
    getRelationalDatabaseSnapshotsResponse_nextPageToken,
    getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots,
    getRelationalDatabaseSnapshotsResponse_httpStatus,

    -- ** GetRelationalDatabases
    getRelationalDatabases_pageToken,
    getRelationalDatabasesResponse_nextPageToken,
    getRelationalDatabasesResponse_relationalDatabases,
    getRelationalDatabasesResponse_httpStatus,

    -- ** GetStaticIp
    getStaticIp_staticIpName,
    getStaticIpResponse_staticIp,
    getStaticIpResponse_httpStatus,

    -- ** GetStaticIps
    getStaticIps_pageToken,
    getStaticIpsResponse_nextPageToken,
    getStaticIpsResponse_staticIps,
    getStaticIpsResponse_httpStatus,

    -- ** ImportKeyPair
    importKeyPair_keyPairName,
    importKeyPair_publicKeyBase64,
    importKeyPairResponse_operation,
    importKeyPairResponse_httpStatus,

    -- ** IsVpcPeered
    isVpcPeeredResponse_isPeered,
    isVpcPeeredResponse_httpStatus,

    -- ** OpenInstancePublicPorts
    openInstancePublicPorts_portInfo,
    openInstancePublicPorts_instanceName,
    openInstancePublicPortsResponse_operation,
    openInstancePublicPortsResponse_httpStatus,

    -- ** PeerVpc
    peerVpcResponse_operation,
    peerVpcResponse_httpStatus,

    -- ** PutAlarm
    putAlarm_treatMissingData,
    putAlarm_datapointsToAlarm,
    putAlarm_notificationEnabled,
    putAlarm_contactProtocols,
    putAlarm_notificationTriggers,
    putAlarm_alarmName,
    putAlarm_metricName,
    putAlarm_monitoredResourceName,
    putAlarm_comparisonOperator,
    putAlarm_threshold,
    putAlarm_evaluationPeriods,
    putAlarmResponse_operations,
    putAlarmResponse_httpStatus,

    -- ** PutInstancePublicPorts
    putInstancePublicPorts_portInfos,
    putInstancePublicPorts_instanceName,
    putInstancePublicPortsResponse_operation,
    putInstancePublicPortsResponse_httpStatus,

    -- ** RebootInstance
    rebootInstance_instanceName,
    rebootInstanceResponse_operations,
    rebootInstanceResponse_httpStatus,

    -- ** RebootRelationalDatabase
    rebootRelationalDatabase_relationalDatabaseName,
    rebootRelationalDatabaseResponse_operations,
    rebootRelationalDatabaseResponse_httpStatus,

    -- ** RegisterContainerImage
    registerContainerImage_serviceName,
    registerContainerImage_label,
    registerContainerImage_digest,
    registerContainerImageResponse_containerImage,
    registerContainerImageResponse_httpStatus,

    -- ** ReleaseStaticIp
    releaseStaticIp_staticIpName,
    releaseStaticIpResponse_operations,
    releaseStaticIpResponse_httpStatus,

    -- ** ResetDistributionCache
    resetDistributionCache_distributionName,
    resetDistributionCacheResponse_status,
    resetDistributionCacheResponse_createTime,
    resetDistributionCacheResponse_operation,
    resetDistributionCacheResponse_httpStatus,

    -- ** SendContactMethodVerification
    sendContactMethodVerification_protocol,
    sendContactMethodVerificationResponse_operations,
    sendContactMethodVerificationResponse_httpStatus,

    -- ** SetIpAddressType
    setIpAddressType_resourceType,
    setIpAddressType_resourceName,
    setIpAddressType_ipAddressType,
    setIpAddressTypeResponse_operations,
    setIpAddressTypeResponse_httpStatus,

    -- ** SetResourceAccessForBucket
    setResourceAccessForBucket_resourceName,
    setResourceAccessForBucket_bucketName,
    setResourceAccessForBucket_access,
    setResourceAccessForBucketResponse_operations,
    setResourceAccessForBucketResponse_httpStatus,

    -- ** StartInstance
    startInstance_instanceName,
    startInstanceResponse_operations,
    startInstanceResponse_httpStatus,

    -- ** StartRelationalDatabase
    startRelationalDatabase_relationalDatabaseName,
    startRelationalDatabaseResponse_operations,
    startRelationalDatabaseResponse_httpStatus,

    -- ** StopInstance
    stopInstance_force,
    stopInstance_instanceName,
    stopInstanceResponse_operations,
    stopInstanceResponse_httpStatus,

    -- ** StopRelationalDatabase
    stopRelationalDatabase_relationalDatabaseSnapshotName,
    stopRelationalDatabase_relationalDatabaseName,
    stopRelationalDatabaseResponse_operations,
    stopRelationalDatabaseResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_resourceName,
    tagResource_tags,
    tagResourceResponse_operations,
    tagResourceResponse_httpStatus,

    -- ** TestAlarm
    testAlarm_alarmName,
    testAlarm_state,
    testAlarmResponse_operations,
    testAlarmResponse_httpStatus,

    -- ** UnpeerVpc
    unpeerVpcResponse_operation,
    unpeerVpcResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_resourceName,
    untagResource_tagKeys,
    untagResourceResponse_operations,
    untagResourceResponse_httpStatus,

    -- ** UpdateBucket
    updateBucket_readonlyAccessAccounts,
    updateBucket_versioning,
    updateBucket_accessRules,
    updateBucket_accessLogConfig,
    updateBucket_bucketName,
    updateBucketResponse_operations,
    updateBucketResponse_bucket,
    updateBucketResponse_httpStatus,

    -- ** UpdateBucketBundle
    updateBucketBundle_bucketName,
    updateBucketBundle_bundleId,
    updateBucketBundleResponse_operations,
    updateBucketBundleResponse_httpStatus,

    -- ** UpdateContainerService
    updateContainerService_power,
    updateContainerService_scale,
    updateContainerService_publicDomainNames,
    updateContainerService_privateRegistryAccess,
    updateContainerService_isDisabled,
    updateContainerService_serviceName,
    updateContainerServiceResponse_containerService,
    updateContainerServiceResponse_httpStatus,

    -- ** UpdateDistribution
    updateDistribution_isEnabled,
    updateDistribution_cacheBehaviorSettings,
    updateDistribution_cacheBehaviors,
    updateDistribution_origin,
    updateDistribution_defaultCacheBehavior,
    updateDistribution_distributionName,
    updateDistributionResponse_operation,
    updateDistributionResponse_httpStatus,

    -- ** UpdateDistributionBundle
    updateDistributionBundle_distributionName,
    updateDistributionBundle_bundleId,
    updateDistributionBundleResponse_operation,
    updateDistributionBundleResponse_httpStatus,

    -- ** UpdateDomainEntry
    updateDomainEntry_domainName,
    updateDomainEntry_domainEntry,
    updateDomainEntryResponse_operations,
    updateDomainEntryResponse_httpStatus,

    -- ** UpdateInstanceMetadataOptions
    updateInstanceMetadataOptions_httpPutResponseHopLimit,
    updateInstanceMetadataOptions_httpTokens,
    updateInstanceMetadataOptions_httpEndpoint,
    updateInstanceMetadataOptions_httpProtocolIpv6,
    updateInstanceMetadataOptions_instanceName,
    updateInstanceMetadataOptionsResponse_operation,
    updateInstanceMetadataOptionsResponse_httpStatus,

    -- ** UpdateLoadBalancerAttribute
    updateLoadBalancerAttribute_loadBalancerName,
    updateLoadBalancerAttribute_attributeName,
    updateLoadBalancerAttribute_attributeValue,
    updateLoadBalancerAttributeResponse_operations,
    updateLoadBalancerAttributeResponse_httpStatus,

    -- ** UpdateRelationalDatabase
    updateRelationalDatabase_preferredBackupWindow,
    updateRelationalDatabase_applyImmediately,
    updateRelationalDatabase_publiclyAccessible,
    updateRelationalDatabase_masterUserPassword,
    updateRelationalDatabase_enableBackupRetention,
    updateRelationalDatabase_caCertificateIdentifier,
    updateRelationalDatabase_preferredMaintenanceWindow,
    updateRelationalDatabase_rotateMasterUserPassword,
    updateRelationalDatabase_disableBackupRetention,
    updateRelationalDatabase_relationalDatabaseName,
    updateRelationalDatabaseResponse_operations,
    updateRelationalDatabaseResponse_httpStatus,

    -- ** UpdateRelationalDatabaseParameters
    updateRelationalDatabaseParameters_relationalDatabaseName,
    updateRelationalDatabaseParameters_parameters,
    updateRelationalDatabaseParametersResponse_operations,
    updateRelationalDatabaseParametersResponse_httpStatus,

    -- * Types

    -- ** AccessKey
    accessKey_status,
    accessKey_secretAccessKey,
    accessKey_lastUsed,
    accessKey_createdAt,
    accessKey_accessKeyId,

    -- ** AccessKeyLastUsed
    accessKeyLastUsed_lastUsedDate,
    accessKeyLastUsed_region,
    accessKeyLastUsed_serviceName,

    -- ** AccessRules
    accessRules_allowPublicOverrides,
    accessRules_getObject,

    -- ** AccountLevelBpaSync
    accountLevelBpaSync_message,
    accountLevelBpaSync_status,
    accountLevelBpaSync_lastSyncedAt,
    accountLevelBpaSync_bpaImpactsLightsail,

    -- ** AddOn
    addOn_name,
    addOn_status,
    addOn_snapshotTimeOfDay,
    addOn_nextSnapshotTimeOfDay,

    -- ** AddOnRequest
    addOnRequest_autoSnapshotAddOnRequest,
    addOnRequest_addOnType,

    -- ** Alarm
    alarm_resourceType,
    alarm_name,
    alarm_period,
    alarm_arn,
    alarm_state,
    alarm_treatMissingData,
    alarm_evaluationPeriods,
    alarm_datapointsToAlarm,
    alarm_location,
    alarm_notificationEnabled,
    alarm_metricName,
    alarm_monitoredResourceInfo,
    alarm_threshold,
    alarm_contactProtocols,
    alarm_comparisonOperator,
    alarm_supportCode,
    alarm_statistic,
    alarm_unit,
    alarm_notificationTriggers,
    alarm_createdAt,

    -- ** AttachedDisk
    attachedDisk_sizeInGb,
    attachedDisk_path,

    -- ** AutoSnapshotAddOnRequest
    autoSnapshotAddOnRequest_snapshotTimeOfDay,

    -- ** AutoSnapshotDetails
    autoSnapshotDetails_fromAttachedDisks,
    autoSnapshotDetails_date,
    autoSnapshotDetails_status,
    autoSnapshotDetails_createdAt,

    -- ** AvailabilityZone
    availabilityZone_zoneName,
    availabilityZone_state,

    -- ** Blueprint
    blueprint_minPower,
    blueprint_isActive,
    blueprint_versionCode,
    blueprint_name,
    blueprint_type,
    blueprint_licenseUrl,
    blueprint_blueprintId,
    blueprint_description,
    blueprint_platform,
    blueprint_productUrl,
    blueprint_group,
    blueprint_version,

    -- ** Bucket
    bucket_tags,
    bucket_resourceType,
    bucket_resourcesReceivingAccess,
    bucket_name,
    bucket_readonlyAccessAccounts,
    bucket_arn,
    bucket_state,
    bucket_url,
    bucket_location,
    bucket_ableToUpdateBundle,
    bucket_bundleId,
    bucket_objectVersioning,
    bucket_accessRules,
    bucket_supportCode,
    bucket_createdAt,
    bucket_accessLogConfig,

    -- ** BucketAccessLogConfig
    bucketAccessLogConfig_destination,
    bucketAccessLogConfig_prefix,
    bucketAccessLogConfig_enabled,

    -- ** BucketBundle
    bucketBundle_isActive,
    bucketBundle_name,
    bucketBundle_transferPerMonthInGb,
    bucketBundle_storagePerMonthInGb,
    bucketBundle_price,
    bucketBundle_bundleId,

    -- ** BucketState
    bucketState_message,
    bucketState_code,

    -- ** Bundle
    bundle_cpuCount,
    bundle_isActive,
    bundle_name,
    bundle_power,
    bundle_transferPerMonthInGb,
    bundle_diskSizeInGb,
    bundle_instanceType,
    bundle_price,
    bundle_bundleId,
    bundle_supportedPlatforms,
    bundle_ramSizeInGb,

    -- ** CacheBehavior
    cacheBehavior_behavior,

    -- ** CacheBehaviorPerPath
    cacheBehaviorPerPath_path,
    cacheBehaviorPerPath_behavior,

    -- ** CacheSettings
    cacheSettings_allowedHTTPMethods,
    cacheSettings_defaultTTL,
    cacheSettings_forwardedCookies,
    cacheSettings_maximumTTL,
    cacheSettings_cachedHTTPMethods,
    cacheSettings_forwardedQueryStrings,
    cacheSettings_forwardedHeaders,
    cacheSettings_minimumTTL,

    -- ** Certificate
    certificate_tags,
    certificate_name,
    certificate_domainName,
    certificate_arn,
    certificate_renewalSummary,
    certificate_keyAlgorithm,
    certificate_requestFailureReason,
    certificate_status,
    certificate_inUseResourceCount,
    certificate_notBefore,
    certificate_revocationReason,
    certificate_serialNumber,
    certificate_domainValidationRecords,
    certificate_eligibleToRenew,
    certificate_revokedAt,
    certificate_supportCode,
    certificate_issuerCA,
    certificate_notAfter,
    certificate_subjectAlternativeNames,
    certificate_createdAt,
    certificate_issuedAt,

    -- ** CertificateSummary
    certificateSummary_tags,
    certificateSummary_domainName,
    certificateSummary_certificateName,
    certificateSummary_certificateArn,
    certificateSummary_certificateDetail,

    -- ** CloudFormationStackRecord
    cloudFormationStackRecord_resourceType,
    cloudFormationStackRecord_name,
    cloudFormationStackRecord_arn,
    cloudFormationStackRecord_state,
    cloudFormationStackRecord_sourceInfo,
    cloudFormationStackRecord_location,
    cloudFormationStackRecord_createdAt,
    cloudFormationStackRecord_destinationInfo,

    -- ** CloudFormationStackRecordSourceInfo
    cloudFormationStackRecordSourceInfo_resourceType,
    cloudFormationStackRecordSourceInfo_name,
    cloudFormationStackRecordSourceInfo_arn,

    -- ** ContactMethod
    contactMethod_resourceType,
    contactMethod_name,
    contactMethod_contactEndpoint,
    contactMethod_arn,
    contactMethod_status,
    contactMethod_location,
    contactMethod_protocol,
    contactMethod_supportCode,
    contactMethod_createdAt,

    -- ** Container
    container_environment,
    container_ports,
    container_command,
    container_image,

    -- ** ContainerImage
    containerImage_digest,
    containerImage_image,
    containerImage_createdAt,

    -- ** ContainerService
    containerService_tags,
    containerService_resourceType,
    containerService_power,
    containerService_stateDetail,
    containerService_currentDeployment,
    containerService_privateDomainName,
    containerService_principalArn,
    containerService_arn,
    containerService_state,
    containerService_url,
    containerService_location,
    containerService_containerServiceName,
    containerService_powerId,
    containerService_scale,
    containerService_createdAt,
    containerService_publicDomainNames,
    containerService_privateRegistryAccess,
    containerService_isDisabled,
    containerService_nextDeployment,

    -- ** ContainerServiceDeployment
    containerServiceDeployment_containers,
    containerServiceDeployment_state,
    containerServiceDeployment_publicEndpoint,
    containerServiceDeployment_createdAt,
    containerServiceDeployment_version,

    -- ** ContainerServiceDeploymentRequest
    containerServiceDeploymentRequest_containers,
    containerServiceDeploymentRequest_publicEndpoint,

    -- ** ContainerServiceECRImagePullerRole
    containerServiceECRImagePullerRole_isActive,
    containerServiceECRImagePullerRole_principalArn,

    -- ** ContainerServiceECRImagePullerRoleRequest
    containerServiceECRImagePullerRoleRequest_isActive,

    -- ** ContainerServiceEndpoint
    containerServiceEndpoint_containerPort,
    containerServiceEndpoint_healthCheck,
    containerServiceEndpoint_containerName,

    -- ** ContainerServiceHealthCheckConfig
    containerServiceHealthCheckConfig_timeoutSeconds,
    containerServiceHealthCheckConfig_intervalSeconds,
    containerServiceHealthCheckConfig_path,
    containerServiceHealthCheckConfig_healthyThreshold,
    containerServiceHealthCheckConfig_unhealthyThreshold,
    containerServiceHealthCheckConfig_successCodes,

    -- ** ContainerServiceLogEvent
    containerServiceLogEvent_message,
    containerServiceLogEvent_createdAt,

    -- ** ContainerServicePower
    containerServicePower_cpuCount,
    containerServicePower_isActive,
    containerServicePower_name,
    containerServicePower_price,
    containerServicePower_powerId,
    containerServicePower_ramSizeInGb,

    -- ** ContainerServiceRegistryLogin
    containerServiceRegistryLogin_password,
    containerServiceRegistryLogin_registry,
    containerServiceRegistryLogin_username,
    containerServiceRegistryLogin_expiresAt,

    -- ** ContainerServiceStateDetail
    containerServiceStateDetail_message,
    containerServiceStateDetail_code,

    -- ** CookieObject
    cookieObject_cookiesAllowList,
    cookieObject_option,

    -- ** DestinationInfo
    destinationInfo_id,
    destinationInfo_service,

    -- ** Disk
    disk_tags,
    disk_resourceType,
    disk_name,
    disk_gbInUse,
    disk_sizeInGb,
    disk_attachedTo,
    disk_arn,
    disk_state,
    disk_path,
    disk_location,
    disk_isAttached,
    disk_addOns,
    disk_isSystemDisk,
    disk_attachmentState,
    disk_supportCode,
    disk_iops,
    disk_createdAt,

    -- ** DiskInfo
    diskInfo_name,
    diskInfo_sizeInGb,
    diskInfo_path,
    diskInfo_isSystemDisk,

    -- ** DiskMap
    diskMap_newDiskName,
    diskMap_originalDiskPath,

    -- ** DiskSnapshot
    diskSnapshot_tags,
    diskSnapshot_progress,
    diskSnapshot_resourceType,
    diskSnapshot_name,
    diskSnapshot_fromInstanceArn,
    diskSnapshot_sizeInGb,
    diskSnapshot_fromDiskName,
    diskSnapshot_arn,
    diskSnapshot_state,
    diskSnapshot_fromDiskArn,
    diskSnapshot_location,
    diskSnapshot_fromInstanceName,
    diskSnapshot_isFromAutoSnapshot,
    diskSnapshot_supportCode,
    diskSnapshot_createdAt,

    -- ** DiskSnapshotInfo
    diskSnapshotInfo_sizeInGb,

    -- ** DistributionBundle
    distributionBundle_isActive,
    distributionBundle_name,
    distributionBundle_transferPerMonthInGb,
    distributionBundle_price,
    distributionBundle_bundleId,

    -- ** DnsRecordCreationState
    dnsRecordCreationState_message,
    dnsRecordCreationState_code,

    -- ** Domain
    domain_tags,
    domain_resourceType,
    domain_name,
    domain_registeredDomainDelegationInfo,
    domain_arn,
    domain_location,
    domain_domainEntries,
    domain_supportCode,
    domain_createdAt,

    -- ** DomainEntry
    domainEntry_name,
    domainEntry_type,
    domainEntry_isAlias,
    domainEntry_target,
    domainEntry_id,
    domainEntry_options,

    -- ** DomainValidationRecord
    domainValidationRecord_dnsRecordCreationState,
    domainValidationRecord_domainName,
    domainValidationRecord_validationStatus,
    domainValidationRecord_resourceRecord,

    -- ** EndpointRequest
    endpointRequest_healthCheck,
    endpointRequest_containerName,
    endpointRequest_containerPort,

    -- ** ExportSnapshotRecord
    exportSnapshotRecord_resourceType,
    exportSnapshotRecord_name,
    exportSnapshotRecord_arn,
    exportSnapshotRecord_state,
    exportSnapshotRecord_sourceInfo,
    exportSnapshotRecord_location,
    exportSnapshotRecord_createdAt,
    exportSnapshotRecord_destinationInfo,

    -- ** ExportSnapshotRecordSourceInfo
    exportSnapshotRecordSourceInfo_resourceType,
    exportSnapshotRecordSourceInfo_name,
    exportSnapshotRecordSourceInfo_fromResourceName,
    exportSnapshotRecordSourceInfo_fromResourceArn,
    exportSnapshotRecordSourceInfo_arn,
    exportSnapshotRecordSourceInfo_diskSnapshotInfo,
    exportSnapshotRecordSourceInfo_createdAt,
    exportSnapshotRecordSourceInfo_instanceSnapshotInfo,

    -- ** HeaderObject
    headerObject_option,
    headerObject_headersAllowList,

    -- ** HostKeyAttributes
    hostKeyAttributes_notValidAfter,
    hostKeyAttributes_publicKey,
    hostKeyAttributes_witnessedAt,
    hostKeyAttributes_fingerprintSHA1,
    hostKeyAttributes_notValidBefore,
    hostKeyAttributes_algorithm,
    hostKeyAttributes_fingerprintSHA256,

    -- ** InputOrigin
    inputOrigin_name,
    inputOrigin_protocolPolicy,
    inputOrigin_regionName,

    -- ** Instance
    instance_tags,
    instance_resourceType,
    instance_name,
    instance_username,
    instance_blueprintId,
    instance_arn,
    instance_state,
    instance_isStaticIp,
    instance_location,
    instance_hardware,
    instance_publicIpAddress,
    instance_bundleId,
    instance_privateIpAddress,
    instance_sshKeyName,
    instance_addOns,
    instance_supportCode,
    instance_blueprintName,
    instance_ipAddressType,
    instance_networking,
    instance_createdAt,
    instance_ipv6Addresses,
    instance_metadataOptions,

    -- ** InstanceAccessDetails
    instanceAccessDetails_hostKeys,
    instanceAccessDetails_instanceName,
    instanceAccessDetails_password,
    instanceAccessDetails_privateKey,
    instanceAccessDetails_username,
    instanceAccessDetails_passwordData,
    instanceAccessDetails_certKey,
    instanceAccessDetails_expiresAt,
    instanceAccessDetails_protocol,
    instanceAccessDetails_ipAddress,

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
    instanceHealthSummary_instanceHealthReason,
    instanceHealthSummary_instanceHealth,
    instanceHealthSummary_instanceName,

    -- ** InstanceMetadataOptions
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_state,
    instanceMetadataOptions_httpTokens,
    instanceMetadataOptions_httpEndpoint,
    instanceMetadataOptions_httpProtocolIpv6,

    -- ** InstanceNetworking
    instanceNetworking_ports,
    instanceNetworking_monthlyTransfer,

    -- ** InstancePortInfo
    instancePortInfo_accessDirection,
    instancePortInfo_toPort,
    instancePortInfo_ipv6Cidrs,
    instancePortInfo_cidrListAliases,
    instancePortInfo_cidrs,
    instancePortInfo_accessFrom,
    instancePortInfo_commonName,
    instancePortInfo_protocol,
    instancePortInfo_accessType,
    instancePortInfo_fromPort,

    -- ** InstancePortState
    instancePortState_toPort,
    instancePortState_ipv6Cidrs,
    instancePortState_cidrListAliases,
    instancePortState_cidrs,
    instancePortState_state,
    instancePortState_protocol,
    instancePortState_fromPort,

    -- ** InstanceSnapshot
    instanceSnapshot_tags,
    instanceSnapshot_progress,
    instanceSnapshot_fromAttachedDisks,
    instanceSnapshot_resourceType,
    instanceSnapshot_name,
    instanceSnapshot_fromInstanceArn,
    instanceSnapshot_sizeInGb,
    instanceSnapshot_arn,
    instanceSnapshot_state,
    instanceSnapshot_location,
    instanceSnapshot_fromBlueprintId,
    instanceSnapshot_fromInstanceName,
    instanceSnapshot_isFromAutoSnapshot,
    instanceSnapshot_fromBundleId,
    instanceSnapshot_supportCode,
    instanceSnapshot_createdAt,

    -- ** InstanceSnapshotInfo
    instanceSnapshotInfo_fromDiskInfo,
    instanceSnapshotInfo_fromBlueprintId,
    instanceSnapshotInfo_fromBundleId,

    -- ** InstanceState
    instanceState_name,
    instanceState_code,

    -- ** KeyPair
    keyPair_tags,
    keyPair_resourceType,
    keyPair_name,
    keyPair_arn,
    keyPair_location,
    keyPair_fingerprint,
    keyPair_supportCode,
    keyPair_createdAt,

    -- ** LightsailDistribution
    lightsailDistribution_tags,
    lightsailDistribution_resourceType,
    lightsailDistribution_name,
    lightsailDistribution_isEnabled,
    lightsailDistribution_domainName,
    lightsailDistribution_cacheBehaviorSettings,
    lightsailDistribution_arn,
    lightsailDistribution_originPublicDNS,
    lightsailDistribution_status,
    lightsailDistribution_certificateName,
    lightsailDistribution_location,
    lightsailDistribution_ableToUpdateBundle,
    lightsailDistribution_bundleId,
    lightsailDistribution_alternativeDomainNames,
    lightsailDistribution_cacheBehaviors,
    lightsailDistribution_supportCode,
    lightsailDistribution_ipAddressType,
    lightsailDistribution_origin,
    lightsailDistribution_createdAt,
    lightsailDistribution_defaultCacheBehavior,

    -- ** LoadBalancer
    loadBalancer_tags,
    loadBalancer_resourceType,
    loadBalancer_name,
    loadBalancer_httpsRedirectionEnabled,
    loadBalancer_healthCheckPath,
    loadBalancer_instanceHealthSummary,
    loadBalancer_configurationOptions,
    loadBalancer_arn,
    loadBalancer_state,
    loadBalancer_publicPorts,
    loadBalancer_location,
    loadBalancer_instancePort,
    loadBalancer_tlsCertificateSummaries,
    loadBalancer_protocol,
    loadBalancer_dnsName,
    loadBalancer_supportCode,
    loadBalancer_ipAddressType,
    loadBalancer_createdAt,
    loadBalancer_tlsPolicyName,

    -- ** LoadBalancerTlsCertificate
    loadBalancerTlsCertificate_tags,
    loadBalancerTlsCertificate_issuer,
    loadBalancerTlsCertificate_resourceType,
    loadBalancerTlsCertificate_name,
    loadBalancerTlsCertificate_domainName,
    loadBalancerTlsCertificate_loadBalancerName,
    loadBalancerTlsCertificate_serial,
    loadBalancerTlsCertificate_arn,
    loadBalancerTlsCertificate_renewalSummary,
    loadBalancerTlsCertificate_keyAlgorithm,
    loadBalancerTlsCertificate_status,
    loadBalancerTlsCertificate_location,
    loadBalancerTlsCertificate_notBefore,
    loadBalancerTlsCertificate_revocationReason,
    loadBalancerTlsCertificate_signatureAlgorithm,
    loadBalancerTlsCertificate_isAttached,
    loadBalancerTlsCertificate_domainValidationRecords,
    loadBalancerTlsCertificate_revokedAt,
    loadBalancerTlsCertificate_supportCode,
    loadBalancerTlsCertificate_notAfter,
    loadBalancerTlsCertificate_subject,
    loadBalancerTlsCertificate_subjectAlternativeNames,
    loadBalancerTlsCertificate_createdAt,
    loadBalancerTlsCertificate_failureReason,
    loadBalancerTlsCertificate_issuedAt,

    -- ** LoadBalancerTlsCertificateDnsRecordCreationState
    loadBalancerTlsCertificateDnsRecordCreationState_message,
    loadBalancerTlsCertificateDnsRecordCreationState_code,

    -- ** LoadBalancerTlsCertificateDomainValidationOption
    loadBalancerTlsCertificateDomainValidationOption_domainName,
    loadBalancerTlsCertificateDomainValidationOption_validationStatus,

    -- ** LoadBalancerTlsCertificateDomainValidationRecord
    loadBalancerTlsCertificateDomainValidationRecord_name,
    loadBalancerTlsCertificateDomainValidationRecord_type,
    loadBalancerTlsCertificateDomainValidationRecord_dnsRecordCreationState,
    loadBalancerTlsCertificateDomainValidationRecord_domainName,
    loadBalancerTlsCertificateDomainValidationRecord_validationStatus,
    loadBalancerTlsCertificateDomainValidationRecord_value,

    -- ** LoadBalancerTlsCertificateRenewalSummary
    loadBalancerTlsCertificateRenewalSummary_domainValidationOptions,
    loadBalancerTlsCertificateRenewalSummary_renewalStatus,

    -- ** LoadBalancerTlsCertificateSummary
    loadBalancerTlsCertificateSummary_name,
    loadBalancerTlsCertificateSummary_isAttached,

    -- ** LoadBalancerTlsPolicy
    loadBalancerTlsPolicy_name,
    loadBalancerTlsPolicy_protocols,
    loadBalancerTlsPolicy_description,
    loadBalancerTlsPolicy_isDefault,
    loadBalancerTlsPolicy_ciphers,

    -- ** LogEvent
    logEvent_message,
    logEvent_createdAt,

    -- ** MetricDatapoint
    metricDatapoint_minimum,
    metricDatapoint_average,
    metricDatapoint_timestamp,
    metricDatapoint_sampleCount,
    metricDatapoint_sum,
    metricDatapoint_maximum,
    metricDatapoint_unit,

    -- ** MonitoredResourceInfo
    monitoredResourceInfo_resourceType,
    monitoredResourceInfo_name,
    monitoredResourceInfo_arn,

    -- ** MonthlyTransfer
    monthlyTransfer_gbPerMonthAllocated,

    -- ** NameServersUpdateState
    nameServersUpdateState_message,
    nameServersUpdateState_code,

    -- ** Operation
    operation_resourceType,
    operation_statusChangedAt,
    operation_isTerminal,
    operation_resourceName,
    operation_operationType,
    operation_errorDetails,
    operation_status,
    operation_id,
    operation_location,
    operation_errorCode,
    operation_operationDetails,
    operation_createdAt,

    -- ** Origin
    origin_resourceType,
    origin_name,
    origin_protocolPolicy,
    origin_regionName,

    -- ** PasswordData
    passwordData_ciphertext,
    passwordData_keyPairName,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,

    -- ** PendingModifiedRelationalDatabaseValues
    pendingModifiedRelationalDatabaseValues_backupRetentionEnabled,
    pendingModifiedRelationalDatabaseValues_masterUserPassword,
    pendingModifiedRelationalDatabaseValues_engineVersion,

    -- ** PortInfo
    portInfo_toPort,
    portInfo_ipv6Cidrs,
    portInfo_cidrListAliases,
    portInfo_cidrs,
    portInfo_protocol,
    portInfo_fromPort,

    -- ** PrivateRegistryAccess
    privateRegistryAccess_ecrImagePullerRole,

    -- ** PrivateRegistryAccessRequest
    privateRegistryAccessRequest_ecrImagePullerRole,

    -- ** QueryStringObject
    queryStringObject_queryStringsAllowList,
    queryStringObject_option,

    -- ** R53HostedZoneDeletionState
    r53HostedZoneDeletionState_message,
    r53HostedZoneDeletionState_code,

    -- ** RegionInfo
    regionInfo_relationalDatabaseAvailabilityZones,
    regionInfo_name,
    regionInfo_availabilityZones,
    regionInfo_displayName,
    regionInfo_description,
    regionInfo_continentCode,

    -- ** RegisteredDomainDelegationInfo
    registeredDomainDelegationInfo_r53HostedZoneDeletionState,
    registeredDomainDelegationInfo_nameServersUpdateState,

    -- ** RelationalDatabase
    relationalDatabase_relationalDatabaseBundleId,
    relationalDatabase_tags,
    relationalDatabase_resourceType,
    relationalDatabase_name,
    relationalDatabase_masterUsername,
    relationalDatabase_preferredBackupWindow,
    relationalDatabase_backupRetentionEnabled,
    relationalDatabase_secondaryAvailabilityZone,
    relationalDatabase_relationalDatabaseBlueprintId,
    relationalDatabase_latestRestorableTime,
    relationalDatabase_masterDatabaseName,
    relationalDatabase_arn,
    relationalDatabase_state,
    relationalDatabase_publiclyAccessible,
    relationalDatabase_parameterApplyStatus,
    relationalDatabase_location,
    relationalDatabase_hardware,
    relationalDatabase_masterEndpoint,
    relationalDatabase_caCertificateIdentifier,
    relationalDatabase_pendingMaintenanceActions,
    relationalDatabase_engine,
    relationalDatabase_pendingModifiedValues,
    relationalDatabase_preferredMaintenanceWindow,
    relationalDatabase_supportCode,
    relationalDatabase_createdAt,
    relationalDatabase_engineVersion,

    -- ** RelationalDatabaseBlueprint
    relationalDatabaseBlueprint_isEngineDefault,
    relationalDatabaseBlueprint_blueprintId,
    relationalDatabaseBlueprint_engineVersionDescription,
    relationalDatabaseBlueprint_engineDescription,
    relationalDatabaseBlueprint_engine,
    relationalDatabaseBlueprint_engineVersion,

    -- ** RelationalDatabaseBundle
    relationalDatabaseBundle_cpuCount,
    relationalDatabaseBundle_isActive,
    relationalDatabaseBundle_name,
    relationalDatabaseBundle_transferPerMonthInGb,
    relationalDatabaseBundle_diskSizeInGb,
    relationalDatabaseBundle_isEncrypted,
    relationalDatabaseBundle_price,
    relationalDatabaseBundle_bundleId,
    relationalDatabaseBundle_ramSizeInGb,

    -- ** RelationalDatabaseEndpoint
    relationalDatabaseEndpoint_port,
    relationalDatabaseEndpoint_address,

    -- ** RelationalDatabaseEvent
    relationalDatabaseEvent_message,
    relationalDatabaseEvent_eventCategories,
    relationalDatabaseEvent_createdAt,
    relationalDatabaseEvent_resource,

    -- ** RelationalDatabaseHardware
    relationalDatabaseHardware_cpuCount,
    relationalDatabaseHardware_diskSizeInGb,
    relationalDatabaseHardware_ramSizeInGb,

    -- ** RelationalDatabaseParameter
    relationalDatabaseParameter_parameterValue,
    relationalDatabaseParameter_applyMethod,
    relationalDatabaseParameter_applyType,
    relationalDatabaseParameter_isModifiable,
    relationalDatabaseParameter_description,
    relationalDatabaseParameter_parameterName,
    relationalDatabaseParameter_allowedValues,
    relationalDatabaseParameter_dataType,

    -- ** RelationalDatabaseSnapshot
    relationalDatabaseSnapshot_tags,
    relationalDatabaseSnapshot_resourceType,
    relationalDatabaseSnapshot_name,
    relationalDatabaseSnapshot_fromRelationalDatabaseBundleId,
    relationalDatabaseSnapshot_sizeInGb,
    relationalDatabaseSnapshot_arn,
    relationalDatabaseSnapshot_state,
    relationalDatabaseSnapshot_fromRelationalDatabaseName,
    relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId,
    relationalDatabaseSnapshot_location,
    relationalDatabaseSnapshot_fromRelationalDatabaseArn,
    relationalDatabaseSnapshot_engine,
    relationalDatabaseSnapshot_supportCode,
    relationalDatabaseSnapshot_createdAt,
    relationalDatabaseSnapshot_engineVersion,

    -- ** RenewalSummary
    renewalSummary_renewalStatusReason,
    renewalSummary_renewalStatus,
    renewalSummary_domainValidationRecords,
    renewalSummary_updatedAt,

    -- ** ResourceLocation
    resourceLocation_availabilityZone,
    resourceLocation_regionName,

    -- ** ResourceReceivingAccess
    resourceReceivingAccess_resourceType,
    resourceReceivingAccess_name,

    -- ** ResourceRecord
    resourceRecord_name,
    resourceRecord_type,
    resourceRecord_value,

    -- ** StaticIp
    staticIp_resourceType,
    staticIp_name,
    staticIp_attachedTo,
    staticIp_arn,
    staticIp_location,
    staticIp_isAttached,
    staticIp_supportCode,
    staticIp_createdAt,
    staticIp_ipAddress,

    -- ** Tag
    tag_key,
    tag_value,
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
import Amazonka.Lightsail.GetLoadBalancerTlsPolicies
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
import Amazonka.Lightsail.Types.AccountLevelBpaSync
import Amazonka.Lightsail.Types.AddOn
import Amazonka.Lightsail.Types.AddOnRequest
import Amazonka.Lightsail.Types.Alarm
import Amazonka.Lightsail.Types.AttachedDisk
import Amazonka.Lightsail.Types.AutoSnapshotAddOnRequest
import Amazonka.Lightsail.Types.AutoSnapshotDetails
import Amazonka.Lightsail.Types.AvailabilityZone
import Amazonka.Lightsail.Types.Blueprint
import Amazonka.Lightsail.Types.Bucket
import Amazonka.Lightsail.Types.BucketAccessLogConfig
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
import Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRole
import Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRoleRequest
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
import Amazonka.Lightsail.Types.DnsRecordCreationState
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
import Amazonka.Lightsail.Types.InstanceMetadataOptions
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
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDnsRecordCreationState
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateSummary
import Amazonka.Lightsail.Types.LoadBalancerTlsPolicy
import Amazonka.Lightsail.Types.LogEvent
import Amazonka.Lightsail.Types.MetricDatapoint
import Amazonka.Lightsail.Types.MonitoredResourceInfo
import Amazonka.Lightsail.Types.MonthlyTransfer
import Amazonka.Lightsail.Types.NameServersUpdateState
import Amazonka.Lightsail.Types.Operation
import Amazonka.Lightsail.Types.Origin
import Amazonka.Lightsail.Types.PasswordData
import Amazonka.Lightsail.Types.PendingMaintenanceAction
import Amazonka.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Amazonka.Lightsail.Types.PortInfo
import Amazonka.Lightsail.Types.PrivateRegistryAccess
import Amazonka.Lightsail.Types.PrivateRegistryAccessRequest
import Amazonka.Lightsail.Types.QueryStringObject
import Amazonka.Lightsail.Types.R53HostedZoneDeletionState
import Amazonka.Lightsail.Types.RegionInfo
import Amazonka.Lightsail.Types.RegisteredDomainDelegationInfo
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
import Amazonka.Lightsail.UpdateInstanceMetadataOptions
import Amazonka.Lightsail.UpdateLoadBalancerAttribute
import Amazonka.Lightsail.UpdateRelationalDatabase
import Amazonka.Lightsail.UpdateRelationalDatabaseParameters
