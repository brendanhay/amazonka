{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    attachDisk_autoMounting,
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
    copySnapshot_restoreDate,
    copySnapshot_sourceResourceName,
    copySnapshot_sourceSnapshotName,
    copySnapshot_useLatestRestorableAutoSnapshot,
    copySnapshot_targetSnapshotName,
    copySnapshot_sourceRegion,
    copySnapshotResponse_operations,
    copySnapshotResponse_httpStatus,

    -- ** CreateBucket
    createBucket_enableObjectVersioning,
    createBucket_tags,
    createBucket_bucketName,
    createBucket_bundleId,
    createBucketResponse_bucket,
    createBucketResponse_operations,
    createBucketResponse_httpStatus,

    -- ** CreateBucketAccessKey
    createBucketAccessKey_bucketName,
    createBucketAccessKeyResponse_accessKey,
    createBucketAccessKeyResponse_operations,
    createBucketAccessKeyResponse_httpStatus,

    -- ** CreateCertificate
    createCertificate_subjectAlternativeNames,
    createCertificate_tags,
    createCertificate_certificateName,
    createCertificate_domainName,
    createCertificateResponse_certificate,
    createCertificateResponse_operations,
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
    createContainerService_deployment,
    createContainerService_privateRegistryAccess,
    createContainerService_publicDomainNames,
    createContainerService_tags,
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
    createDisk_addOns,
    createDisk_tags,
    createDisk_diskName,
    createDisk_availabilityZone,
    createDisk_sizeInGb,
    createDiskResponse_operations,
    createDiskResponse_httpStatus,

    -- ** CreateDiskFromSnapshot
    createDiskFromSnapshot_addOns,
    createDiskFromSnapshot_diskSnapshotName,
    createDiskFromSnapshot_restoreDate,
    createDiskFromSnapshot_sourceDiskName,
    createDiskFromSnapshot_tags,
    createDiskFromSnapshot_useLatestRestorableAutoSnapshot,
    createDiskFromSnapshot_diskName,
    createDiskFromSnapshot_availabilityZone,
    createDiskFromSnapshot_sizeInGb,
    createDiskFromSnapshotResponse_operations,
    createDiskFromSnapshotResponse_httpStatus,

    -- ** CreateDiskSnapshot
    createDiskSnapshot_diskName,
    createDiskSnapshot_instanceName,
    createDiskSnapshot_tags,
    createDiskSnapshot_diskSnapshotName,
    createDiskSnapshotResponse_operations,
    createDiskSnapshotResponse_httpStatus,

    -- ** CreateDistribution
    createDistribution_cacheBehaviorSettings,
    createDistribution_cacheBehaviors,
    createDistribution_ipAddressType,
    createDistribution_tags,
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

    -- ** CreateGUISessionAccessDetails
    createGUISessionAccessDetails_resourceName,
    createGUISessionAccessDetailsResponse_failureReason,
    createGUISessionAccessDetailsResponse_percentageComplete,
    createGUISessionAccessDetailsResponse_resourceName,
    createGUISessionAccessDetailsResponse_sessions,
    createGUISessionAccessDetailsResponse_status,
    createGUISessionAccessDetailsResponse_httpStatus,

    -- ** CreateInstanceSnapshot
    createInstanceSnapshot_tags,
    createInstanceSnapshot_instanceSnapshotName,
    createInstanceSnapshot_instanceName,
    createInstanceSnapshotResponse_operations,
    createInstanceSnapshotResponse_httpStatus,

    -- ** CreateInstances
    createInstances_addOns,
    createInstances_customImageName,
    createInstances_ipAddressType,
    createInstances_keyPairName,
    createInstances_tags,
    createInstances_userData,
    createInstances_instanceNames,
    createInstances_availabilityZone,
    createInstances_blueprintId,
    createInstances_bundleId,
    createInstancesResponse_operations,
    createInstancesResponse_httpStatus,

    -- ** CreateInstancesFromSnapshot
    createInstancesFromSnapshot_addOns,
    createInstancesFromSnapshot_attachedDiskMapping,
    createInstancesFromSnapshot_instanceSnapshotName,
    createInstancesFromSnapshot_ipAddressType,
    createInstancesFromSnapshot_keyPairName,
    createInstancesFromSnapshot_restoreDate,
    createInstancesFromSnapshot_sourceInstanceName,
    createInstancesFromSnapshot_tags,
    createInstancesFromSnapshot_useLatestRestorableAutoSnapshot,
    createInstancesFromSnapshot_userData,
    createInstancesFromSnapshot_instanceNames,
    createInstancesFromSnapshot_availabilityZone,
    createInstancesFromSnapshot_bundleId,
    createInstancesFromSnapshotResponse_operations,
    createInstancesFromSnapshotResponse_httpStatus,

    -- ** CreateKeyPair
    createKeyPair_tags,
    createKeyPair_keyPairName,
    createKeyPairResponse_keyPair,
    createKeyPairResponse_operation,
    createKeyPairResponse_privateKeyBase64,
    createKeyPairResponse_publicKeyBase64,
    createKeyPairResponse_httpStatus,

    -- ** CreateLoadBalancer
    createLoadBalancer_certificateAlternativeNames,
    createLoadBalancer_certificateDomainName,
    createLoadBalancer_certificateName,
    createLoadBalancer_healthCheckPath,
    createLoadBalancer_ipAddressType,
    createLoadBalancer_tags,
    createLoadBalancer_tlsPolicyName,
    createLoadBalancer_loadBalancerName,
    createLoadBalancer_instancePort,
    createLoadBalancerResponse_operations,
    createLoadBalancerResponse_httpStatus,

    -- ** CreateLoadBalancerTlsCertificate
    createLoadBalancerTlsCertificate_certificateAlternativeNames,
    createLoadBalancerTlsCertificate_tags,
    createLoadBalancerTlsCertificate_loadBalancerName,
    createLoadBalancerTlsCertificate_certificateName,
    createLoadBalancerTlsCertificate_certificateDomainName,
    createLoadBalancerTlsCertificateResponse_operations,
    createLoadBalancerTlsCertificateResponse_httpStatus,

    -- ** CreateRelationalDatabase
    createRelationalDatabase_availabilityZone,
    createRelationalDatabase_masterUserPassword,
    createRelationalDatabase_preferredBackupWindow,
    createRelationalDatabase_preferredMaintenanceWindow,
    createRelationalDatabase_publiclyAccessible,
    createRelationalDatabase_tags,
    createRelationalDatabase_relationalDatabaseName,
    createRelationalDatabase_relationalDatabaseBlueprintId,
    createRelationalDatabase_relationalDatabaseBundleId,
    createRelationalDatabase_masterDatabaseName,
    createRelationalDatabase_masterUsername,
    createRelationalDatabaseResponse_operations,
    createRelationalDatabaseResponse_httpStatus,

    -- ** CreateRelationalDatabaseFromSnapshot
    createRelationalDatabaseFromSnapshot_availabilityZone,
    createRelationalDatabaseFromSnapshot_publiclyAccessible,
    createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId,
    createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName,
    createRelationalDatabaseFromSnapshot_restoreTime,
    createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName,
    createRelationalDatabaseFromSnapshot_tags,
    createRelationalDatabaseFromSnapshot_useLatestRestorableTime,
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
    downloadDefaultKeyPairResponse_createdAt,
    downloadDefaultKeyPairResponse_privateKeyBase64,
    downloadDefaultKeyPairResponse_publicKeyBase64,
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
    getActiveNamesResponse_activeNames,
    getActiveNamesResponse_nextPageToken,
    getActiveNamesResponse_httpStatus,

    -- ** GetAlarms
    getAlarms_alarmName,
    getAlarms_monitoredResourceName,
    getAlarms_pageToken,
    getAlarmsResponse_alarms,
    getAlarmsResponse_nextPageToken,
    getAlarmsResponse_httpStatus,

    -- ** GetAutoSnapshots
    getAutoSnapshots_resourceName,
    getAutoSnapshotsResponse_autoSnapshots,
    getAutoSnapshotsResponse_resourceName,
    getAutoSnapshotsResponse_resourceType,
    getAutoSnapshotsResponse_httpStatus,

    -- ** GetBlueprints
    getBlueprints_appCategory,
    getBlueprints_includeInactive,
    getBlueprints_pageToken,
    getBlueprintsResponse_blueprints,
    getBlueprintsResponse_nextPageToken,
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
    getBucketMetricDataResponse_metricData,
    getBucketMetricDataResponse_metricName,
    getBucketMetricDataResponse_httpStatus,

    -- ** GetBuckets
    getBuckets_bucketName,
    getBuckets_includeConnectedResources,
    getBuckets_pageToken,
    getBucketsResponse_accountLevelBpaSync,
    getBucketsResponse_buckets,
    getBucketsResponse_nextPageToken,
    getBucketsResponse_httpStatus,

    -- ** GetBundles
    getBundles_appCategory,
    getBundles_includeInactive,
    getBundles_pageToken,
    getBundlesResponse_bundles,
    getBundlesResponse_nextPageToken,
    getBundlesResponse_httpStatus,

    -- ** GetCertificates
    getCertificates_certificateName,
    getCertificates_certificateStatuses,
    getCertificates_includeCertificateDetails,
    getCertificates_pageToken,
    getCertificatesResponse_certificates,
    getCertificatesResponse_nextPageToken,
    getCertificatesResponse_httpStatus,

    -- ** GetCloudFormationStackRecords
    getCloudFormationStackRecords_pageToken,
    getCloudFormationStackRecordsResponse_cloudFormationStackRecords,
    getCloudFormationStackRecordsResponse_nextPageToken,
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
    getContainerLog_endTime,
    getContainerLog_filterPattern,
    getContainerLog_pageToken,
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
    getContainerServiceMetricDataResponse_metricData,
    getContainerServiceMetricDataResponse_metricName,
    getContainerServiceMetricDataResponse_httpStatus,

    -- ** GetContainerServicePowers
    getContainerServicePowersResponse_powers,
    getContainerServicePowersResponse_httpStatus,

    -- ** GetContainerServices
    getContainerServices_serviceName,
    getContainerServicesResponse_containerServices,
    getContainerServicesResponse_httpStatus,

    -- ** GetCostEstimate
    getCostEstimate_resourceName,
    getCostEstimate_startTime,
    getCostEstimate_endTime,
    getCostEstimateResponse_resourcesBudgetEstimate,
    getCostEstimateResponse_httpStatus,

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
    getDiskSnapshotsResponse_diskSnapshots,
    getDiskSnapshotsResponse_nextPageToken,
    getDiskSnapshotsResponse_httpStatus,

    -- ** GetDisks
    getDisks_pageToken,
    getDisksResponse_disks,
    getDisksResponse_nextPageToken,
    getDisksResponse_httpStatus,

    -- ** GetDistributionBundles
    getDistributionBundlesResponse_bundles,
    getDistributionBundlesResponse_httpStatus,

    -- ** GetDistributionLatestCacheReset
    getDistributionLatestCacheReset_distributionName,
    getDistributionLatestCacheResetResponse_createTime,
    getDistributionLatestCacheResetResponse_status,
    getDistributionLatestCacheResetResponse_httpStatus,

    -- ** GetDistributionMetricData
    getDistributionMetricData_distributionName,
    getDistributionMetricData_metricName,
    getDistributionMetricData_startTime,
    getDistributionMetricData_endTime,
    getDistributionMetricData_period,
    getDistributionMetricData_unit,
    getDistributionMetricData_statistics,
    getDistributionMetricDataResponse_metricData,
    getDistributionMetricDataResponse_metricName,
    getDistributionMetricDataResponse_httpStatus,

    -- ** GetDistributions
    getDistributions_distributionName,
    getDistributions_pageToken,
    getDistributionsResponse_distributions,
    getDistributionsResponse_nextPageToken,
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
    getInstanceMetricDataResponse_metricData,
    getInstanceMetricDataResponse_metricName,
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
    getInstanceSnapshotsResponse_instanceSnapshots,
    getInstanceSnapshotsResponse_nextPageToken,
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
    getLoadBalancerMetricDataResponse_metricData,
    getLoadBalancerMetricDataResponse_metricName,
    getLoadBalancerMetricDataResponse_httpStatus,

    -- ** GetLoadBalancerTlsCertificates
    getLoadBalancerTlsCertificates_loadBalancerName,
    getLoadBalancerTlsCertificatesResponse_tlsCertificates,
    getLoadBalancerTlsCertificatesResponse_httpStatus,

    -- ** GetLoadBalancerTlsPolicies
    getLoadBalancerTlsPolicies_pageToken,
    getLoadBalancerTlsPoliciesResponse_nextPageToken,
    getLoadBalancerTlsPoliciesResponse_tlsPolicies,
    getLoadBalancerTlsPoliciesResponse_httpStatus,

    -- ** GetLoadBalancers
    getLoadBalancers_pageToken,
    getLoadBalancersResponse_loadBalancers,
    getLoadBalancersResponse_nextPageToken,
    getLoadBalancersResponse_httpStatus,

    -- ** GetOperation
    getOperation_operationId,
    getOperationResponse_operation,
    getOperationResponse_httpStatus,

    -- ** GetOperations
    getOperations_pageToken,
    getOperationsResponse_nextPageToken,
    getOperationsResponse_operations,
    getOperationsResponse_httpStatus,

    -- ** GetOperationsForResource
    getOperationsForResource_pageToken,
    getOperationsForResource_resourceName,
    getOperationsForResourceResponse_nextPageCount,
    getOperationsForResourceResponse_nextPageToken,
    getOperationsForResourceResponse_operations,
    getOperationsForResourceResponse_httpStatus,

    -- ** GetRegions
    getRegions_includeAvailabilityZones,
    getRegions_includeRelationalDatabaseAvailabilityZones,
    getRegionsResponse_regions,
    getRegionsResponse_httpStatus,

    -- ** GetRelationalDatabase
    getRelationalDatabase_relationalDatabaseName,
    getRelationalDatabaseResponse_relationalDatabase,
    getRelationalDatabaseResponse_httpStatus,

    -- ** GetRelationalDatabaseBlueprints
    getRelationalDatabaseBlueprints_pageToken,
    getRelationalDatabaseBlueprintsResponse_blueprints,
    getRelationalDatabaseBlueprintsResponse_nextPageToken,
    getRelationalDatabaseBlueprintsResponse_httpStatus,

    -- ** GetRelationalDatabaseBundles
    getRelationalDatabaseBundles_includeInactive,
    getRelationalDatabaseBundles_pageToken,
    getRelationalDatabaseBundlesResponse_bundles,
    getRelationalDatabaseBundlesResponse_nextPageToken,
    getRelationalDatabaseBundlesResponse_httpStatus,

    -- ** GetRelationalDatabaseEvents
    getRelationalDatabaseEvents_durationInMinutes,
    getRelationalDatabaseEvents_pageToken,
    getRelationalDatabaseEvents_relationalDatabaseName,
    getRelationalDatabaseEventsResponse_nextPageToken,
    getRelationalDatabaseEventsResponse_relationalDatabaseEvents,
    getRelationalDatabaseEventsResponse_httpStatus,

    -- ** GetRelationalDatabaseLogEvents
    getRelationalDatabaseLogEvents_endTime,
    getRelationalDatabaseLogEvents_pageToken,
    getRelationalDatabaseLogEvents_startFromHead,
    getRelationalDatabaseLogEvents_startTime,
    getRelationalDatabaseLogEvents_relationalDatabaseName,
    getRelationalDatabaseLogEvents_logStreamName,
    getRelationalDatabaseLogEventsResponse_nextBackwardToken,
    getRelationalDatabaseLogEventsResponse_nextForwardToken,
    getRelationalDatabaseLogEventsResponse_resourceLogEvents,
    getRelationalDatabaseLogEventsResponse_httpStatus,

    -- ** GetRelationalDatabaseLogStreams
    getRelationalDatabaseLogStreams_relationalDatabaseName,
    getRelationalDatabaseLogStreamsResponse_logStreams,
    getRelationalDatabaseLogStreamsResponse_httpStatus,

    -- ** GetRelationalDatabaseMasterUserPassword
    getRelationalDatabaseMasterUserPassword_passwordVersion,
    getRelationalDatabaseMasterUserPassword_relationalDatabaseName,
    getRelationalDatabaseMasterUserPasswordResponse_createdAt,
    getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword,
    getRelationalDatabaseMasterUserPasswordResponse_httpStatus,

    -- ** GetRelationalDatabaseMetricData
    getRelationalDatabaseMetricData_relationalDatabaseName,
    getRelationalDatabaseMetricData_metricName,
    getRelationalDatabaseMetricData_period,
    getRelationalDatabaseMetricData_startTime,
    getRelationalDatabaseMetricData_endTime,
    getRelationalDatabaseMetricData_unit,
    getRelationalDatabaseMetricData_statistics,
    getRelationalDatabaseMetricDataResponse_metricData,
    getRelationalDatabaseMetricDataResponse_metricName,
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
    putAlarm_contactProtocols,
    putAlarm_datapointsToAlarm,
    putAlarm_notificationEnabled,
    putAlarm_notificationTriggers,
    putAlarm_treatMissingData,
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
    resetDistributionCacheResponse_createTime,
    resetDistributionCacheResponse_operation,
    resetDistributionCacheResponse_status,
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

    -- ** StartGUISession
    startGUISession_resourceName,
    startGUISessionResponse_operations,
    startGUISessionResponse_httpStatus,

    -- ** StartInstance
    startInstance_instanceName,
    startInstanceResponse_operations,
    startInstanceResponse_httpStatus,

    -- ** StartRelationalDatabase
    startRelationalDatabase_relationalDatabaseName,
    startRelationalDatabaseResponse_operations,
    startRelationalDatabaseResponse_httpStatus,

    -- ** StopGUISession
    stopGUISession_resourceName,
    stopGUISessionResponse_operations,
    stopGUISessionResponse_httpStatus,

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
    updateBucket_accessLogConfig,
    updateBucket_accessRules,
    updateBucket_readonlyAccessAccounts,
    updateBucket_versioning,
    updateBucket_bucketName,
    updateBucketResponse_bucket,
    updateBucketResponse_operations,
    updateBucketResponse_httpStatus,

    -- ** UpdateBucketBundle
    updateBucketBundle_bucketName,
    updateBucketBundle_bundleId,
    updateBucketBundleResponse_operations,
    updateBucketBundleResponse_httpStatus,

    -- ** UpdateContainerService
    updateContainerService_isDisabled,
    updateContainerService_power,
    updateContainerService_privateRegistryAccess,
    updateContainerService_publicDomainNames,
    updateContainerService_scale,
    updateContainerService_serviceName,
    updateContainerServiceResponse_containerService,
    updateContainerServiceResponse_httpStatus,

    -- ** UpdateDistribution
    updateDistribution_cacheBehaviorSettings,
    updateDistribution_cacheBehaviors,
    updateDistribution_defaultCacheBehavior,
    updateDistribution_isEnabled,
    updateDistribution_origin,
    updateDistribution_distributionName,
    updateDistributionResponse_operation,
    updateDistributionResponse_httpStatus,

    -- ** UpdateDistributionBundle
    updateDistributionBundle_bundleId,
    updateDistributionBundle_distributionName,
    updateDistributionBundleResponse_operation,
    updateDistributionBundleResponse_httpStatus,

    -- ** UpdateDomainEntry
    updateDomainEntry_domainName,
    updateDomainEntry_domainEntry,
    updateDomainEntryResponse_operations,
    updateDomainEntryResponse_httpStatus,

    -- ** UpdateInstanceMetadataOptions
    updateInstanceMetadataOptions_httpEndpoint,
    updateInstanceMetadataOptions_httpProtocolIpv6,
    updateInstanceMetadataOptions_httpPutResponseHopLimit,
    updateInstanceMetadataOptions_httpTokens,
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
    updateRelationalDatabase_applyImmediately,
    updateRelationalDatabase_caCertificateIdentifier,
    updateRelationalDatabase_disableBackupRetention,
    updateRelationalDatabase_enableBackupRetention,
    updateRelationalDatabase_masterUserPassword,
    updateRelationalDatabase_preferredBackupWindow,
    updateRelationalDatabase_preferredMaintenanceWindow,
    updateRelationalDatabase_publiclyAccessible,
    updateRelationalDatabase_rotateMasterUserPassword,
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
    accessKey_accessKeyId,
    accessKey_createdAt,
    accessKey_lastUsed,
    accessKey_secretAccessKey,
    accessKey_status,

    -- ** AccessKeyLastUsed
    accessKeyLastUsed_lastUsedDate,
    accessKeyLastUsed_region,
    accessKeyLastUsed_serviceName,

    -- ** AccessRules
    accessRules_allowPublicOverrides,
    accessRules_getObject,

    -- ** AccountLevelBpaSync
    accountLevelBpaSync_bpaImpactsLightsail,
    accountLevelBpaSync_lastSyncedAt,
    accountLevelBpaSync_message,
    accountLevelBpaSync_status,

    -- ** AddOn
    addOn_duration,
    addOn_name,
    addOn_nextSnapshotTimeOfDay,
    addOn_snapshotTimeOfDay,
    addOn_status,
    addOn_threshold,

    -- ** AddOnRequest
    addOnRequest_autoSnapshotAddOnRequest,
    addOnRequest_stopInstanceOnIdleRequest,
    addOnRequest_addOnType,

    -- ** Alarm
    alarm_arn,
    alarm_comparisonOperator,
    alarm_contactProtocols,
    alarm_createdAt,
    alarm_datapointsToAlarm,
    alarm_evaluationPeriods,
    alarm_location,
    alarm_metricName,
    alarm_monitoredResourceInfo,
    alarm_name,
    alarm_notificationEnabled,
    alarm_notificationTriggers,
    alarm_period,
    alarm_resourceType,
    alarm_state,
    alarm_statistic,
    alarm_supportCode,
    alarm_threshold,
    alarm_treatMissingData,
    alarm_unit,

    -- ** AttachedDisk
    attachedDisk_path,
    attachedDisk_sizeInGb,

    -- ** AutoSnapshotAddOnRequest
    autoSnapshotAddOnRequest_snapshotTimeOfDay,

    -- ** AutoSnapshotDetails
    autoSnapshotDetails_createdAt,
    autoSnapshotDetails_date,
    autoSnapshotDetails_fromAttachedDisks,
    autoSnapshotDetails_status,

    -- ** AvailabilityZone
    availabilityZone_state,
    availabilityZone_zoneName,

    -- ** Blueprint
    blueprint_appCategory,
    blueprint_blueprintId,
    blueprint_description,
    blueprint_group,
    blueprint_isActive,
    blueprint_licenseUrl,
    blueprint_minPower,
    blueprint_name,
    blueprint_platform,
    blueprint_productUrl,
    blueprint_type,
    blueprint_version,
    blueprint_versionCode,

    -- ** Bucket
    bucket_ableToUpdateBundle,
    bucket_accessLogConfig,
    bucket_accessRules,
    bucket_arn,
    bucket_bundleId,
    bucket_createdAt,
    bucket_location,
    bucket_name,
    bucket_objectVersioning,
    bucket_readonlyAccessAccounts,
    bucket_resourceType,
    bucket_resourcesReceivingAccess,
    bucket_state,
    bucket_supportCode,
    bucket_tags,
    bucket_url,

    -- ** BucketAccessLogConfig
    bucketAccessLogConfig_destination,
    bucketAccessLogConfig_prefix,
    bucketAccessLogConfig_enabled,

    -- ** BucketBundle
    bucketBundle_bundleId,
    bucketBundle_isActive,
    bucketBundle_name,
    bucketBundle_price,
    bucketBundle_storagePerMonthInGb,
    bucketBundle_transferPerMonthInGb,

    -- ** BucketState
    bucketState_code,
    bucketState_message,

    -- ** Bundle
    bundle_bundleId,
    bundle_cpuCount,
    bundle_diskSizeInGb,
    bundle_instanceType,
    bundle_isActive,
    bundle_name,
    bundle_power,
    bundle_price,
    bundle_ramSizeInGb,
    bundle_supportedAppCategories,
    bundle_supportedPlatforms,
    bundle_transferPerMonthInGb,

    -- ** CacheBehavior
    cacheBehavior_behavior,

    -- ** CacheBehaviorPerPath
    cacheBehaviorPerPath_behavior,
    cacheBehaviorPerPath_path,

    -- ** CacheSettings
    cacheSettings_allowedHTTPMethods,
    cacheSettings_cachedHTTPMethods,
    cacheSettings_defaultTTL,
    cacheSettings_forwardedCookies,
    cacheSettings_forwardedHeaders,
    cacheSettings_forwardedQueryStrings,
    cacheSettings_maximumTTL,
    cacheSettings_minimumTTL,

    -- ** Certificate
    certificate_arn,
    certificate_createdAt,
    certificate_domainName,
    certificate_domainValidationRecords,
    certificate_eligibleToRenew,
    certificate_inUseResourceCount,
    certificate_issuedAt,
    certificate_issuerCA,
    certificate_keyAlgorithm,
    certificate_name,
    certificate_notAfter,
    certificate_notBefore,
    certificate_renewalSummary,
    certificate_requestFailureReason,
    certificate_revocationReason,
    certificate_revokedAt,
    certificate_serialNumber,
    certificate_status,
    certificate_subjectAlternativeNames,
    certificate_supportCode,
    certificate_tags,

    -- ** CertificateSummary
    certificateSummary_certificateArn,
    certificateSummary_certificateDetail,
    certificateSummary_certificateName,
    certificateSummary_domainName,
    certificateSummary_tags,

    -- ** CloudFormationStackRecord
    cloudFormationStackRecord_arn,
    cloudFormationStackRecord_createdAt,
    cloudFormationStackRecord_destinationInfo,
    cloudFormationStackRecord_location,
    cloudFormationStackRecord_name,
    cloudFormationStackRecord_resourceType,
    cloudFormationStackRecord_sourceInfo,
    cloudFormationStackRecord_state,

    -- ** CloudFormationStackRecordSourceInfo
    cloudFormationStackRecordSourceInfo_arn,
    cloudFormationStackRecordSourceInfo_name,
    cloudFormationStackRecordSourceInfo_resourceType,

    -- ** ContactMethod
    contactMethod_arn,
    contactMethod_contactEndpoint,
    contactMethod_createdAt,
    contactMethod_location,
    contactMethod_name,
    contactMethod_protocol,
    contactMethod_resourceType,
    contactMethod_status,
    contactMethod_supportCode,

    -- ** Container
    container_command,
    container_environment,
    container_image,
    container_ports,

    -- ** ContainerImage
    containerImage_createdAt,
    containerImage_digest,
    containerImage_image,

    -- ** ContainerService
    containerService_arn,
    containerService_containerServiceName,
    containerService_createdAt,
    containerService_currentDeployment,
    containerService_isDisabled,
    containerService_location,
    containerService_nextDeployment,
    containerService_power,
    containerService_powerId,
    containerService_principalArn,
    containerService_privateDomainName,
    containerService_privateRegistryAccess,
    containerService_publicDomainNames,
    containerService_resourceType,
    containerService_scale,
    containerService_state,
    containerService_stateDetail,
    containerService_tags,
    containerService_url,

    -- ** ContainerServiceDeployment
    containerServiceDeployment_containers,
    containerServiceDeployment_createdAt,
    containerServiceDeployment_publicEndpoint,
    containerServiceDeployment_state,
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
    containerServiceEndpoint_containerName,
    containerServiceEndpoint_containerPort,
    containerServiceEndpoint_healthCheck,

    -- ** ContainerServiceHealthCheckConfig
    containerServiceHealthCheckConfig_healthyThreshold,
    containerServiceHealthCheckConfig_intervalSeconds,
    containerServiceHealthCheckConfig_path,
    containerServiceHealthCheckConfig_successCodes,
    containerServiceHealthCheckConfig_timeoutSeconds,
    containerServiceHealthCheckConfig_unhealthyThreshold,

    -- ** ContainerServiceLogEvent
    containerServiceLogEvent_createdAt,
    containerServiceLogEvent_message,

    -- ** ContainerServicePower
    containerServicePower_cpuCount,
    containerServicePower_isActive,
    containerServicePower_name,
    containerServicePower_powerId,
    containerServicePower_price,
    containerServicePower_ramSizeInGb,

    -- ** ContainerServiceRegistryLogin
    containerServiceRegistryLogin_expiresAt,
    containerServiceRegistryLogin_password,
    containerServiceRegistryLogin_registry,
    containerServiceRegistryLogin_username,

    -- ** ContainerServiceStateDetail
    containerServiceStateDetail_code,
    containerServiceStateDetail_message,

    -- ** CookieObject
    cookieObject_cookiesAllowList,
    cookieObject_option,

    -- ** CostEstimate
    costEstimate_resultsByTime,
    costEstimate_usageType,

    -- ** DestinationInfo
    destinationInfo_id,
    destinationInfo_service,

    -- ** Disk
    disk_addOns,
    disk_arn,
    disk_attachedTo,
    disk_attachmentState,
    disk_autoMountStatus,
    disk_createdAt,
    disk_gbInUse,
    disk_iops,
    disk_isAttached,
    disk_isSystemDisk,
    disk_location,
    disk_name,
    disk_path,
    disk_resourceType,
    disk_sizeInGb,
    disk_state,
    disk_supportCode,
    disk_tags,

    -- ** DiskInfo
    diskInfo_isSystemDisk,
    diskInfo_name,
    diskInfo_path,
    diskInfo_sizeInGb,

    -- ** DiskMap
    diskMap_newDiskName,
    diskMap_originalDiskPath,

    -- ** DiskSnapshot
    diskSnapshot_arn,
    diskSnapshot_createdAt,
    diskSnapshot_fromDiskArn,
    diskSnapshot_fromDiskName,
    diskSnapshot_fromInstanceArn,
    diskSnapshot_fromInstanceName,
    diskSnapshot_isFromAutoSnapshot,
    diskSnapshot_location,
    diskSnapshot_name,
    diskSnapshot_progress,
    diskSnapshot_resourceType,
    diskSnapshot_sizeInGb,
    diskSnapshot_state,
    diskSnapshot_supportCode,
    diskSnapshot_tags,

    -- ** DiskSnapshotInfo
    diskSnapshotInfo_sizeInGb,

    -- ** DistributionBundle
    distributionBundle_bundleId,
    distributionBundle_isActive,
    distributionBundle_name,
    distributionBundle_price,
    distributionBundle_transferPerMonthInGb,

    -- ** DnsRecordCreationState
    dnsRecordCreationState_code,
    dnsRecordCreationState_message,

    -- ** Domain
    domain_arn,
    domain_createdAt,
    domain_domainEntries,
    domain_location,
    domain_name,
    domain_registeredDomainDelegationInfo,
    domain_resourceType,
    domain_supportCode,
    domain_tags,

    -- ** DomainEntry
    domainEntry_id,
    domainEntry_isAlias,
    domainEntry_name,
    domainEntry_options,
    domainEntry_target,
    domainEntry_type,

    -- ** DomainValidationRecord
    domainValidationRecord_dnsRecordCreationState,
    domainValidationRecord_domainName,
    domainValidationRecord_resourceRecord,
    domainValidationRecord_validationStatus,

    -- ** EndpointRequest
    endpointRequest_healthCheck,
    endpointRequest_containerName,
    endpointRequest_containerPort,

    -- ** EstimateByTime
    estimateByTime_currency,
    estimateByTime_pricingUnit,
    estimateByTime_timePeriod,
    estimateByTime_unit,
    estimateByTime_usageCost,

    -- ** ExportSnapshotRecord
    exportSnapshotRecord_arn,
    exportSnapshotRecord_createdAt,
    exportSnapshotRecord_destinationInfo,
    exportSnapshotRecord_location,
    exportSnapshotRecord_name,
    exportSnapshotRecord_resourceType,
    exportSnapshotRecord_sourceInfo,
    exportSnapshotRecord_state,

    -- ** ExportSnapshotRecordSourceInfo
    exportSnapshotRecordSourceInfo_arn,
    exportSnapshotRecordSourceInfo_createdAt,
    exportSnapshotRecordSourceInfo_diskSnapshotInfo,
    exportSnapshotRecordSourceInfo_fromResourceArn,
    exportSnapshotRecordSourceInfo_fromResourceName,
    exportSnapshotRecordSourceInfo_instanceSnapshotInfo,
    exportSnapshotRecordSourceInfo_name,
    exportSnapshotRecordSourceInfo_resourceType,

    -- ** HeaderObject
    headerObject_headersAllowList,
    headerObject_option,

    -- ** HostKeyAttributes
    hostKeyAttributes_algorithm,
    hostKeyAttributes_fingerprintSHA1,
    hostKeyAttributes_fingerprintSHA256,
    hostKeyAttributes_notValidAfter,
    hostKeyAttributes_notValidBefore,
    hostKeyAttributes_publicKey,
    hostKeyAttributes_witnessedAt,

    -- ** InputOrigin
    inputOrigin_name,
    inputOrigin_protocolPolicy,
    inputOrigin_regionName,

    -- ** Instance
    instance_addOns,
    instance_arn,
    instance_blueprintId,
    instance_blueprintName,
    instance_bundleId,
    instance_createdAt,
    instance_hardware,
    instance_ipAddressType,
    instance_ipv6Addresses,
    instance_isStaticIp,
    instance_location,
    instance_metadataOptions,
    instance_name,
    instance_networking,
    instance_privateIpAddress,
    instance_publicIpAddress,
    instance_resourceType,
    instance_sshKeyName,
    instance_state,
    instance_supportCode,
    instance_tags,
    instance_username,

    -- ** InstanceAccessDetails
    instanceAccessDetails_certKey,
    instanceAccessDetails_expiresAt,
    instanceAccessDetails_hostKeys,
    instanceAccessDetails_instanceName,
    instanceAccessDetails_ipAddress,
    instanceAccessDetails_password,
    instanceAccessDetails_passwordData,
    instanceAccessDetails_privateKey,
    instanceAccessDetails_protocol,
    instanceAccessDetails_username,

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
    instanceHealthSummary_instanceHealthReason,
    instanceHealthSummary_instanceName,

    -- ** InstanceMetadataOptions
    instanceMetadataOptions_httpEndpoint,
    instanceMetadataOptions_httpProtocolIpv6,
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_httpTokens,
    instanceMetadataOptions_state,

    -- ** InstanceNetworking
    instanceNetworking_monthlyTransfer,
    instanceNetworking_ports,

    -- ** InstancePortInfo
    instancePortInfo_accessDirection,
    instancePortInfo_accessFrom,
    instancePortInfo_accessType,
    instancePortInfo_cidrListAliases,
    instancePortInfo_cidrs,
    instancePortInfo_commonName,
    instancePortInfo_fromPort,
    instancePortInfo_ipv6Cidrs,
    instancePortInfo_protocol,
    instancePortInfo_toPort,

    -- ** InstancePortState
    instancePortState_cidrListAliases,
    instancePortState_cidrs,
    instancePortState_fromPort,
    instancePortState_ipv6Cidrs,
    instancePortState_protocol,
    instancePortState_state,
    instancePortState_toPort,

    -- ** InstanceSnapshot
    instanceSnapshot_arn,
    instanceSnapshot_createdAt,
    instanceSnapshot_fromAttachedDisks,
    instanceSnapshot_fromBlueprintId,
    instanceSnapshot_fromBundleId,
    instanceSnapshot_fromInstanceArn,
    instanceSnapshot_fromInstanceName,
    instanceSnapshot_isFromAutoSnapshot,
    instanceSnapshot_location,
    instanceSnapshot_name,
    instanceSnapshot_progress,
    instanceSnapshot_resourceType,
    instanceSnapshot_sizeInGb,
    instanceSnapshot_state,
    instanceSnapshot_supportCode,
    instanceSnapshot_tags,

    -- ** InstanceSnapshotInfo
    instanceSnapshotInfo_fromBlueprintId,
    instanceSnapshotInfo_fromBundleId,
    instanceSnapshotInfo_fromDiskInfo,

    -- ** InstanceState
    instanceState_code,
    instanceState_name,

    -- ** KeyPair
    keyPair_arn,
    keyPair_createdAt,
    keyPair_fingerprint,
    keyPair_location,
    keyPair_name,
    keyPair_resourceType,
    keyPair_supportCode,
    keyPair_tags,

    -- ** LightsailDistribution
    lightsailDistribution_ableToUpdateBundle,
    lightsailDistribution_alternativeDomainNames,
    lightsailDistribution_arn,
    lightsailDistribution_bundleId,
    lightsailDistribution_cacheBehaviorSettings,
    lightsailDistribution_cacheBehaviors,
    lightsailDistribution_certificateName,
    lightsailDistribution_createdAt,
    lightsailDistribution_defaultCacheBehavior,
    lightsailDistribution_domainName,
    lightsailDistribution_ipAddressType,
    lightsailDistribution_isEnabled,
    lightsailDistribution_location,
    lightsailDistribution_name,
    lightsailDistribution_origin,
    lightsailDistribution_originPublicDNS,
    lightsailDistribution_resourceType,
    lightsailDistribution_status,
    lightsailDistribution_supportCode,
    lightsailDistribution_tags,

    -- ** LoadBalancer
    loadBalancer_arn,
    loadBalancer_configurationOptions,
    loadBalancer_createdAt,
    loadBalancer_dnsName,
    loadBalancer_healthCheckPath,
    loadBalancer_httpsRedirectionEnabled,
    loadBalancer_instanceHealthSummary,
    loadBalancer_instancePort,
    loadBalancer_ipAddressType,
    loadBalancer_location,
    loadBalancer_name,
    loadBalancer_protocol,
    loadBalancer_publicPorts,
    loadBalancer_resourceType,
    loadBalancer_state,
    loadBalancer_supportCode,
    loadBalancer_tags,
    loadBalancer_tlsCertificateSummaries,
    loadBalancer_tlsPolicyName,

    -- ** LoadBalancerTlsCertificate
    loadBalancerTlsCertificate_arn,
    loadBalancerTlsCertificate_createdAt,
    loadBalancerTlsCertificate_domainName,
    loadBalancerTlsCertificate_domainValidationRecords,
    loadBalancerTlsCertificate_failureReason,
    loadBalancerTlsCertificate_isAttached,
    loadBalancerTlsCertificate_issuedAt,
    loadBalancerTlsCertificate_issuer,
    loadBalancerTlsCertificate_keyAlgorithm,
    loadBalancerTlsCertificate_loadBalancerName,
    loadBalancerTlsCertificate_location,
    loadBalancerTlsCertificate_name,
    loadBalancerTlsCertificate_notAfter,
    loadBalancerTlsCertificate_notBefore,
    loadBalancerTlsCertificate_renewalSummary,
    loadBalancerTlsCertificate_resourceType,
    loadBalancerTlsCertificate_revocationReason,
    loadBalancerTlsCertificate_revokedAt,
    loadBalancerTlsCertificate_serial,
    loadBalancerTlsCertificate_signatureAlgorithm,
    loadBalancerTlsCertificate_status,
    loadBalancerTlsCertificate_subject,
    loadBalancerTlsCertificate_subjectAlternativeNames,
    loadBalancerTlsCertificate_supportCode,
    loadBalancerTlsCertificate_tags,

    -- ** LoadBalancerTlsCertificateDnsRecordCreationState
    loadBalancerTlsCertificateDnsRecordCreationState_code,
    loadBalancerTlsCertificateDnsRecordCreationState_message,

    -- ** LoadBalancerTlsCertificateDomainValidationOption
    loadBalancerTlsCertificateDomainValidationOption_domainName,
    loadBalancerTlsCertificateDomainValidationOption_validationStatus,

    -- ** LoadBalancerTlsCertificateDomainValidationRecord
    loadBalancerTlsCertificateDomainValidationRecord_dnsRecordCreationState,
    loadBalancerTlsCertificateDomainValidationRecord_domainName,
    loadBalancerTlsCertificateDomainValidationRecord_name,
    loadBalancerTlsCertificateDomainValidationRecord_type,
    loadBalancerTlsCertificateDomainValidationRecord_validationStatus,
    loadBalancerTlsCertificateDomainValidationRecord_value,

    -- ** LoadBalancerTlsCertificateRenewalSummary
    loadBalancerTlsCertificateRenewalSummary_domainValidationOptions,
    loadBalancerTlsCertificateRenewalSummary_renewalStatus,

    -- ** LoadBalancerTlsCertificateSummary
    loadBalancerTlsCertificateSummary_isAttached,
    loadBalancerTlsCertificateSummary_name,

    -- ** LoadBalancerTlsPolicy
    loadBalancerTlsPolicy_ciphers,
    loadBalancerTlsPolicy_description,
    loadBalancerTlsPolicy_isDefault,
    loadBalancerTlsPolicy_name,
    loadBalancerTlsPolicy_protocols,

    -- ** LogEvent
    logEvent_createdAt,
    logEvent_message,

    -- ** MetricDatapoint
    metricDatapoint_average,
    metricDatapoint_maximum,
    metricDatapoint_minimum,
    metricDatapoint_sampleCount,
    metricDatapoint_sum,
    metricDatapoint_timestamp,
    metricDatapoint_unit,

    -- ** MonitoredResourceInfo
    monitoredResourceInfo_arn,
    monitoredResourceInfo_name,
    monitoredResourceInfo_resourceType,

    -- ** MonthlyTransfer
    monthlyTransfer_gbPerMonthAllocated,

    -- ** NameServersUpdateState
    nameServersUpdateState_code,
    nameServersUpdateState_message,

    -- ** Operation
    operation_createdAt,
    operation_errorCode,
    operation_errorDetails,
    operation_id,
    operation_isTerminal,
    operation_location,
    operation_operationDetails,
    operation_operationType,
    operation_resourceName,
    operation_resourceType,
    operation_status,
    operation_statusChangedAt,

    -- ** Origin
    origin_name,
    origin_protocolPolicy,
    origin_regionName,
    origin_resourceType,

    -- ** PasswordData
    passwordData_ciphertext,
    passwordData_keyPairName,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_description,

    -- ** PendingModifiedRelationalDatabaseValues
    pendingModifiedRelationalDatabaseValues_backupRetentionEnabled,
    pendingModifiedRelationalDatabaseValues_engineVersion,
    pendingModifiedRelationalDatabaseValues_masterUserPassword,

    -- ** PortInfo
    portInfo_cidrListAliases,
    portInfo_cidrs,
    portInfo_fromPort,
    portInfo_ipv6Cidrs,
    portInfo_protocol,
    portInfo_toPort,

    -- ** PrivateRegistryAccess
    privateRegistryAccess_ecrImagePullerRole,

    -- ** PrivateRegistryAccessRequest
    privateRegistryAccessRequest_ecrImagePullerRole,

    -- ** QueryStringObject
    queryStringObject_option,
    queryStringObject_queryStringsAllowList,

    -- ** R53HostedZoneDeletionState
    r53HostedZoneDeletionState_code,
    r53HostedZoneDeletionState_message,

    -- ** RegionInfo
    regionInfo_availabilityZones,
    regionInfo_continentCode,
    regionInfo_description,
    regionInfo_displayName,
    regionInfo_name,
    regionInfo_relationalDatabaseAvailabilityZones,

    -- ** RegisteredDomainDelegationInfo
    registeredDomainDelegationInfo_nameServersUpdateState,
    registeredDomainDelegationInfo_r53HostedZoneDeletionState,

    -- ** RelationalDatabase
    relationalDatabase_arn,
    relationalDatabase_backupRetentionEnabled,
    relationalDatabase_caCertificateIdentifier,
    relationalDatabase_createdAt,
    relationalDatabase_engine,
    relationalDatabase_engineVersion,
    relationalDatabase_hardware,
    relationalDatabase_latestRestorableTime,
    relationalDatabase_location,
    relationalDatabase_masterDatabaseName,
    relationalDatabase_masterEndpoint,
    relationalDatabase_masterUsername,
    relationalDatabase_name,
    relationalDatabase_parameterApplyStatus,
    relationalDatabase_pendingMaintenanceActions,
    relationalDatabase_pendingModifiedValues,
    relationalDatabase_preferredBackupWindow,
    relationalDatabase_preferredMaintenanceWindow,
    relationalDatabase_publiclyAccessible,
    relationalDatabase_relationalDatabaseBlueprintId,
    relationalDatabase_relationalDatabaseBundleId,
    relationalDatabase_resourceType,
    relationalDatabase_secondaryAvailabilityZone,
    relationalDatabase_state,
    relationalDatabase_supportCode,
    relationalDatabase_tags,

    -- ** RelationalDatabaseBlueprint
    relationalDatabaseBlueprint_blueprintId,
    relationalDatabaseBlueprint_engine,
    relationalDatabaseBlueprint_engineDescription,
    relationalDatabaseBlueprint_engineVersion,
    relationalDatabaseBlueprint_engineVersionDescription,
    relationalDatabaseBlueprint_isEngineDefault,

    -- ** RelationalDatabaseBundle
    relationalDatabaseBundle_bundleId,
    relationalDatabaseBundle_cpuCount,
    relationalDatabaseBundle_diskSizeInGb,
    relationalDatabaseBundle_isActive,
    relationalDatabaseBundle_isEncrypted,
    relationalDatabaseBundle_name,
    relationalDatabaseBundle_price,
    relationalDatabaseBundle_ramSizeInGb,
    relationalDatabaseBundle_transferPerMonthInGb,

    -- ** RelationalDatabaseEndpoint
    relationalDatabaseEndpoint_address,
    relationalDatabaseEndpoint_port,

    -- ** RelationalDatabaseEvent
    relationalDatabaseEvent_createdAt,
    relationalDatabaseEvent_eventCategories,
    relationalDatabaseEvent_message,
    relationalDatabaseEvent_resource,

    -- ** RelationalDatabaseHardware
    relationalDatabaseHardware_cpuCount,
    relationalDatabaseHardware_diskSizeInGb,
    relationalDatabaseHardware_ramSizeInGb,

    -- ** RelationalDatabaseParameter
    relationalDatabaseParameter_allowedValues,
    relationalDatabaseParameter_applyMethod,
    relationalDatabaseParameter_applyType,
    relationalDatabaseParameter_dataType,
    relationalDatabaseParameter_description,
    relationalDatabaseParameter_isModifiable,
    relationalDatabaseParameter_parameterName,
    relationalDatabaseParameter_parameterValue,

    -- ** RelationalDatabaseSnapshot
    relationalDatabaseSnapshot_arn,
    relationalDatabaseSnapshot_createdAt,
    relationalDatabaseSnapshot_engine,
    relationalDatabaseSnapshot_engineVersion,
    relationalDatabaseSnapshot_fromRelationalDatabaseArn,
    relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId,
    relationalDatabaseSnapshot_fromRelationalDatabaseBundleId,
    relationalDatabaseSnapshot_fromRelationalDatabaseName,
    relationalDatabaseSnapshot_location,
    relationalDatabaseSnapshot_name,
    relationalDatabaseSnapshot_resourceType,
    relationalDatabaseSnapshot_sizeInGb,
    relationalDatabaseSnapshot_state,
    relationalDatabaseSnapshot_supportCode,
    relationalDatabaseSnapshot_tags,

    -- ** RenewalSummary
    renewalSummary_domainValidationRecords,
    renewalSummary_renewalStatus,
    renewalSummary_renewalStatusReason,
    renewalSummary_updatedAt,

    -- ** ResourceBudgetEstimate
    resourceBudgetEstimate_costEstimates,
    resourceBudgetEstimate_endTime,
    resourceBudgetEstimate_resourceName,
    resourceBudgetEstimate_resourceType,
    resourceBudgetEstimate_startTime,

    -- ** ResourceLocation
    resourceLocation_availabilityZone,
    resourceLocation_regionName,

    -- ** ResourceReceivingAccess
    resourceReceivingAccess_name,
    resourceReceivingAccess_resourceType,

    -- ** ResourceRecord
    resourceRecord_name,
    resourceRecord_type,
    resourceRecord_value,

    -- ** Session
    session_isPrimary,
    session_name,
    session_url,

    -- ** StaticIp
    staticIp_arn,
    staticIp_attachedTo,
    staticIp_createdAt,
    staticIp_ipAddress,
    staticIp_isAttached,
    staticIp_location,
    staticIp_name,
    staticIp_resourceType,
    staticIp_supportCode,

    -- ** StopInstanceOnIdleRequest
    stopInstanceOnIdleRequest_duration,
    stopInstanceOnIdleRequest_threshold,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TimePeriod
    timePeriod_end,
    timePeriod_start,
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
import Amazonka.Lightsail.CreateGUISessionAccessDetails
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
import Amazonka.Lightsail.GetCostEstimate
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
import Amazonka.Lightsail.StartGUISession
import Amazonka.Lightsail.StartInstance
import Amazonka.Lightsail.StartRelationalDatabase
import Amazonka.Lightsail.StopGUISession
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
import Amazonka.Lightsail.Types.CostEstimate
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
import Amazonka.Lightsail.Types.EstimateByTime
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
import Amazonka.Lightsail.Types.ResourceBudgetEstimate
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceReceivingAccess
import Amazonka.Lightsail.Types.ResourceRecord
import Amazonka.Lightsail.Types.Session
import Amazonka.Lightsail.Types.StaticIp
import Amazonka.Lightsail.Types.StopInstanceOnIdleRequest
import Amazonka.Lightsail.Types.Tag
import Amazonka.Lightsail.Types.TimePeriod
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
