{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataSync.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Lens
  ( -- * Operations

    -- ** AddStorageSystem
    addStorageSystem_cloudWatchLogGroupArn,
    addStorageSystem_name,
    addStorageSystem_tags,
    addStorageSystem_serverConfiguration,
    addStorageSystem_systemType,
    addStorageSystem_agentArns,
    addStorageSystem_clientToken,
    addStorageSystem_credentials,
    addStorageSystemResponse_httpStatus,
    addStorageSystemResponse_storageSystemArn,

    -- ** CancelTaskExecution
    cancelTaskExecution_taskExecutionArn,
    cancelTaskExecutionResponse_httpStatus,

    -- ** CreateAgent
    createAgent_agentName,
    createAgent_securityGroupArns,
    createAgent_subnetArns,
    createAgent_tags,
    createAgent_vpcEndpointId,
    createAgent_activationKey,
    createAgentResponse_agentArn,
    createAgentResponse_httpStatus,

    -- ** CreateLocationEfs
    createLocationEfs_accessPointArn,
    createLocationEfs_fileSystemAccessRoleArn,
    createLocationEfs_inTransitEncryption,
    createLocationEfs_subdirectory,
    createLocationEfs_tags,
    createLocationEfs_efsFilesystemArn,
    createLocationEfs_ec2Config,
    createLocationEfsResponse_locationArn,
    createLocationEfsResponse_httpStatus,

    -- ** CreateLocationFsxLustre
    createLocationFsxLustre_subdirectory,
    createLocationFsxLustre_tags,
    createLocationFsxLustre_fsxFilesystemArn,
    createLocationFsxLustre_securityGroupArns,
    createLocationFsxLustreResponse_locationArn,
    createLocationFsxLustreResponse_httpStatus,

    -- ** CreateLocationFsxOntap
    createLocationFsxOntap_subdirectory,
    createLocationFsxOntap_tags,
    createLocationFsxOntap_protocol,
    createLocationFsxOntap_securityGroupArns,
    createLocationFsxOntap_storageVirtualMachineArn,
    createLocationFsxOntapResponse_locationArn,
    createLocationFsxOntapResponse_httpStatus,

    -- ** CreateLocationFsxOpenZfs
    createLocationFsxOpenZfs_subdirectory,
    createLocationFsxOpenZfs_tags,
    createLocationFsxOpenZfs_fsxFilesystemArn,
    createLocationFsxOpenZfs_protocol,
    createLocationFsxOpenZfs_securityGroupArns,
    createLocationFsxOpenZfsResponse_locationArn,
    createLocationFsxOpenZfsResponse_httpStatus,

    -- ** CreateLocationFsxWindows
    createLocationFsxWindows_domain,
    createLocationFsxWindows_subdirectory,
    createLocationFsxWindows_tags,
    createLocationFsxWindows_fsxFilesystemArn,
    createLocationFsxWindows_securityGroupArns,
    createLocationFsxWindows_user,
    createLocationFsxWindows_password,
    createLocationFsxWindowsResponse_locationArn,
    createLocationFsxWindowsResponse_httpStatus,

    -- ** CreateLocationHdfs
    createLocationHdfs_blockSize,
    createLocationHdfs_kerberosKeytab,
    createLocationHdfs_kerberosKrb5Conf,
    createLocationHdfs_kerberosPrincipal,
    createLocationHdfs_kmsKeyProviderUri,
    createLocationHdfs_qopConfiguration,
    createLocationHdfs_replicationFactor,
    createLocationHdfs_simpleUser,
    createLocationHdfs_subdirectory,
    createLocationHdfs_tags,
    createLocationHdfs_nameNodes,
    createLocationHdfs_authenticationType,
    createLocationHdfs_agentArns,
    createLocationHdfsResponse_locationArn,
    createLocationHdfsResponse_httpStatus,

    -- ** CreateLocationNfs
    createLocationNfs_mountOptions,
    createLocationNfs_tags,
    createLocationNfs_subdirectory,
    createLocationNfs_serverHostname,
    createLocationNfs_onPremConfig,
    createLocationNfsResponse_locationArn,
    createLocationNfsResponse_httpStatus,

    -- ** CreateLocationObjectStorage
    createLocationObjectStorage_accessKey,
    createLocationObjectStorage_secretKey,
    createLocationObjectStorage_serverCertificate,
    createLocationObjectStorage_serverPort,
    createLocationObjectStorage_serverProtocol,
    createLocationObjectStorage_subdirectory,
    createLocationObjectStorage_tags,
    createLocationObjectStorage_serverHostname,
    createLocationObjectStorage_bucketName,
    createLocationObjectStorage_agentArns,
    createLocationObjectStorageResponse_locationArn,
    createLocationObjectStorageResponse_httpStatus,

    -- ** CreateLocationS3
    createLocationS3_agentArns,
    createLocationS3_s3StorageClass,
    createLocationS3_subdirectory,
    createLocationS3_tags,
    createLocationS3_s3BucketArn,
    createLocationS3_s3Config,
    createLocationS3Response_locationArn,
    createLocationS3Response_httpStatus,

    -- ** CreateLocationSmb
    createLocationSmb_domain,
    createLocationSmb_mountOptions,
    createLocationSmb_tags,
    createLocationSmb_subdirectory,
    createLocationSmb_serverHostname,
    createLocationSmb_user,
    createLocationSmb_password,
    createLocationSmb_agentArns,
    createLocationSmbResponse_locationArn,
    createLocationSmbResponse_httpStatus,

    -- ** CreateTask
    createTask_cloudWatchLogGroupArn,
    createTask_excludes,
    createTask_includes,
    createTask_name,
    createTask_options,
    createTask_schedule,
    createTask_tags,
    createTask_sourceLocationArn,
    createTask_destinationLocationArn,
    createTaskResponse_taskArn,
    createTaskResponse_httpStatus,

    -- ** DeleteAgent
    deleteAgent_agentArn,
    deleteAgentResponse_httpStatus,

    -- ** DeleteLocation
    deleteLocation_locationArn,
    deleteLocationResponse_httpStatus,

    -- ** DeleteTask
    deleteTask_taskArn,
    deleteTaskResponse_httpStatus,

    -- ** DescribeAgent
    describeAgent_agentArn,
    describeAgentResponse_agentArn,
    describeAgentResponse_creationTime,
    describeAgentResponse_endpointType,
    describeAgentResponse_lastConnectionTime,
    describeAgentResponse_name,
    describeAgentResponse_privateLinkConfig,
    describeAgentResponse_status,
    describeAgentResponse_httpStatus,

    -- ** DescribeDiscoveryJob
    describeDiscoveryJob_discoveryJobArn,
    describeDiscoveryJobResponse_collectionDurationMinutes,
    describeDiscoveryJobResponse_discoveryJobArn,
    describeDiscoveryJobResponse_jobEndTime,
    describeDiscoveryJobResponse_jobStartTime,
    describeDiscoveryJobResponse_status,
    describeDiscoveryJobResponse_storageSystemArn,
    describeDiscoveryJobResponse_httpStatus,

    -- ** DescribeLocationEfs
    describeLocationEfs_locationArn,
    describeLocationEfsResponse_accessPointArn,
    describeLocationEfsResponse_creationTime,
    describeLocationEfsResponse_ec2Config,
    describeLocationEfsResponse_fileSystemAccessRoleArn,
    describeLocationEfsResponse_inTransitEncryption,
    describeLocationEfsResponse_locationArn,
    describeLocationEfsResponse_locationUri,
    describeLocationEfsResponse_httpStatus,

    -- ** DescribeLocationFsxLustre
    describeLocationFsxLustre_locationArn,
    describeLocationFsxLustreResponse_creationTime,
    describeLocationFsxLustreResponse_locationArn,
    describeLocationFsxLustreResponse_locationUri,
    describeLocationFsxLustreResponse_securityGroupArns,
    describeLocationFsxLustreResponse_httpStatus,

    -- ** DescribeLocationFsxOntap
    describeLocationFsxOntap_locationArn,
    describeLocationFsxOntapResponse_creationTime,
    describeLocationFsxOntapResponse_fsxFilesystemArn,
    describeLocationFsxOntapResponse_locationArn,
    describeLocationFsxOntapResponse_locationUri,
    describeLocationFsxOntapResponse_protocol,
    describeLocationFsxOntapResponse_securityGroupArns,
    describeLocationFsxOntapResponse_storageVirtualMachineArn,
    describeLocationFsxOntapResponse_httpStatus,

    -- ** DescribeLocationFsxOpenZfs
    describeLocationFsxOpenZfs_locationArn,
    describeLocationFsxOpenZfsResponse_creationTime,
    describeLocationFsxOpenZfsResponse_locationArn,
    describeLocationFsxOpenZfsResponse_locationUri,
    describeLocationFsxOpenZfsResponse_protocol,
    describeLocationFsxOpenZfsResponse_securityGroupArns,
    describeLocationFsxOpenZfsResponse_httpStatus,

    -- ** DescribeLocationFsxWindows
    describeLocationFsxWindows_locationArn,
    describeLocationFsxWindowsResponse_creationTime,
    describeLocationFsxWindowsResponse_domain,
    describeLocationFsxWindowsResponse_locationArn,
    describeLocationFsxWindowsResponse_locationUri,
    describeLocationFsxWindowsResponse_securityGroupArns,
    describeLocationFsxWindowsResponse_user,
    describeLocationFsxWindowsResponse_httpStatus,

    -- ** DescribeLocationHdfs
    describeLocationHdfs_locationArn,
    describeLocationHdfsResponse_agentArns,
    describeLocationHdfsResponse_authenticationType,
    describeLocationHdfsResponse_blockSize,
    describeLocationHdfsResponse_creationTime,
    describeLocationHdfsResponse_kerberosPrincipal,
    describeLocationHdfsResponse_kmsKeyProviderUri,
    describeLocationHdfsResponse_locationArn,
    describeLocationHdfsResponse_locationUri,
    describeLocationHdfsResponse_nameNodes,
    describeLocationHdfsResponse_qopConfiguration,
    describeLocationHdfsResponse_replicationFactor,
    describeLocationHdfsResponse_simpleUser,
    describeLocationHdfsResponse_httpStatus,

    -- ** DescribeLocationNfs
    describeLocationNfs_locationArn,
    describeLocationNfsResponse_creationTime,
    describeLocationNfsResponse_locationArn,
    describeLocationNfsResponse_locationUri,
    describeLocationNfsResponse_mountOptions,
    describeLocationNfsResponse_onPremConfig,
    describeLocationNfsResponse_httpStatus,

    -- ** DescribeLocationObjectStorage
    describeLocationObjectStorage_locationArn,
    describeLocationObjectStorageResponse_accessKey,
    describeLocationObjectStorageResponse_agentArns,
    describeLocationObjectStorageResponse_creationTime,
    describeLocationObjectStorageResponse_locationArn,
    describeLocationObjectStorageResponse_locationUri,
    describeLocationObjectStorageResponse_serverCertificate,
    describeLocationObjectStorageResponse_serverPort,
    describeLocationObjectStorageResponse_serverProtocol,
    describeLocationObjectStorageResponse_httpStatus,

    -- ** DescribeLocationS3
    describeLocationS3_locationArn,
    describeLocationS3Response_agentArns,
    describeLocationS3Response_creationTime,
    describeLocationS3Response_locationArn,
    describeLocationS3Response_locationUri,
    describeLocationS3Response_s3Config,
    describeLocationS3Response_s3StorageClass,
    describeLocationS3Response_httpStatus,

    -- ** DescribeLocationSmb
    describeLocationSmb_locationArn,
    describeLocationSmbResponse_agentArns,
    describeLocationSmbResponse_creationTime,
    describeLocationSmbResponse_domain,
    describeLocationSmbResponse_locationArn,
    describeLocationSmbResponse_locationUri,
    describeLocationSmbResponse_mountOptions,
    describeLocationSmbResponse_user,
    describeLocationSmbResponse_httpStatus,

    -- ** DescribeStorageSystem
    describeStorageSystem_storageSystemArn,
    describeStorageSystemResponse_agentArns,
    describeStorageSystemResponse_cloudWatchLogGroupArn,
    describeStorageSystemResponse_connectivityStatus,
    describeStorageSystemResponse_creationTime,
    describeStorageSystemResponse_errorMessage,
    describeStorageSystemResponse_name,
    describeStorageSystemResponse_secretsManagerArn,
    describeStorageSystemResponse_serverConfiguration,
    describeStorageSystemResponse_storageSystemArn,
    describeStorageSystemResponse_systemType,
    describeStorageSystemResponse_httpStatus,

    -- ** DescribeStorageSystemResourceMetrics
    describeStorageSystemResourceMetrics_endTime,
    describeStorageSystemResourceMetrics_maxResults,
    describeStorageSystemResourceMetrics_nextToken,
    describeStorageSystemResourceMetrics_startTime,
    describeStorageSystemResourceMetrics_discoveryJobArn,
    describeStorageSystemResourceMetrics_resourceType,
    describeStorageSystemResourceMetrics_resourceId,
    describeStorageSystemResourceMetricsResponse_metrics,
    describeStorageSystemResourceMetricsResponse_nextToken,
    describeStorageSystemResourceMetricsResponse_httpStatus,

    -- ** DescribeStorageSystemResources
    describeStorageSystemResources_filter,
    describeStorageSystemResources_maxResults,
    describeStorageSystemResources_nextToken,
    describeStorageSystemResources_resourceIds,
    describeStorageSystemResources_discoveryJobArn,
    describeStorageSystemResources_resourceType,
    describeStorageSystemResourcesResponse_nextToken,
    describeStorageSystemResourcesResponse_resourceDetails,
    describeStorageSystemResourcesResponse_httpStatus,

    -- ** DescribeTask
    describeTask_taskArn,
    describeTaskResponse_cloudWatchLogGroupArn,
    describeTaskResponse_creationTime,
    describeTaskResponse_currentTaskExecutionArn,
    describeTaskResponse_destinationLocationArn,
    describeTaskResponse_destinationNetworkInterfaceArns,
    describeTaskResponse_errorCode,
    describeTaskResponse_errorDetail,
    describeTaskResponse_excludes,
    describeTaskResponse_includes,
    describeTaskResponse_name,
    describeTaskResponse_options,
    describeTaskResponse_schedule,
    describeTaskResponse_sourceLocationArn,
    describeTaskResponse_sourceNetworkInterfaceArns,
    describeTaskResponse_status,
    describeTaskResponse_taskArn,
    describeTaskResponse_httpStatus,

    -- ** DescribeTaskExecution
    describeTaskExecution_taskExecutionArn,
    describeTaskExecutionResponse_bytesCompressed,
    describeTaskExecutionResponse_bytesTransferred,
    describeTaskExecutionResponse_bytesWritten,
    describeTaskExecutionResponse_estimatedBytesToTransfer,
    describeTaskExecutionResponse_estimatedFilesToTransfer,
    describeTaskExecutionResponse_excludes,
    describeTaskExecutionResponse_filesTransferred,
    describeTaskExecutionResponse_includes,
    describeTaskExecutionResponse_options,
    describeTaskExecutionResponse_result,
    describeTaskExecutionResponse_startTime,
    describeTaskExecutionResponse_status,
    describeTaskExecutionResponse_taskExecutionArn,
    describeTaskExecutionResponse_httpStatus,

    -- ** GenerateRecommendations
    generateRecommendations_discoveryJobArn,
    generateRecommendations_resourceIds,
    generateRecommendations_resourceType,
    generateRecommendationsResponse_httpStatus,

    -- ** ListAgents
    listAgents_maxResults,
    listAgents_nextToken,
    listAgentsResponse_agents,
    listAgentsResponse_nextToken,
    listAgentsResponse_httpStatus,

    -- ** ListDiscoveryJobs
    listDiscoveryJobs_maxResults,
    listDiscoveryJobs_nextToken,
    listDiscoveryJobs_storageSystemArn,
    listDiscoveryJobsResponse_discoveryJobs,
    listDiscoveryJobsResponse_nextToken,
    listDiscoveryJobsResponse_httpStatus,

    -- ** ListLocations
    listLocations_filters,
    listLocations_maxResults,
    listLocations_nextToken,
    listLocationsResponse_locations,
    listLocationsResponse_nextToken,
    listLocationsResponse_httpStatus,

    -- ** ListStorageSystems
    listStorageSystems_maxResults,
    listStorageSystems_nextToken,
    listStorageSystemsResponse_nextToken,
    listStorageSystemsResponse_storageSystems,
    listStorageSystemsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_maxResults,
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTaskExecutions
    listTaskExecutions_maxResults,
    listTaskExecutions_nextToken,
    listTaskExecutions_taskArn,
    listTaskExecutionsResponse_nextToken,
    listTaskExecutionsResponse_taskExecutions,
    listTaskExecutionsResponse_httpStatus,

    -- ** ListTasks
    listTasks_filters,
    listTasks_maxResults,
    listTasks_nextToken,
    listTasksResponse_nextToken,
    listTasksResponse_tasks,
    listTasksResponse_httpStatus,

    -- ** RemoveStorageSystem
    removeStorageSystem_storageSystemArn,
    removeStorageSystemResponse_httpStatus,

    -- ** StartDiscoveryJob
    startDiscoveryJob_tags,
    startDiscoveryJob_storageSystemArn,
    startDiscoveryJob_collectionDurationMinutes,
    startDiscoveryJob_clientToken,
    startDiscoveryJobResponse_discoveryJobArn,
    startDiscoveryJobResponse_httpStatus,

    -- ** StartTaskExecution
    startTaskExecution_excludes,
    startTaskExecution_includes,
    startTaskExecution_overrideOptions,
    startTaskExecution_tags,
    startTaskExecution_taskArn,
    startTaskExecutionResponse_taskExecutionArn,
    startTaskExecutionResponse_httpStatus,

    -- ** StopDiscoveryJob
    stopDiscoveryJob_discoveryJobArn,
    stopDiscoveryJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_keys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAgent
    updateAgent_name,
    updateAgent_agentArn,
    updateAgentResponse_httpStatus,

    -- ** UpdateDiscoveryJob
    updateDiscoveryJob_discoveryJobArn,
    updateDiscoveryJob_collectionDurationMinutes,
    updateDiscoveryJobResponse_httpStatus,

    -- ** UpdateLocationHdfs
    updateLocationHdfs_agentArns,
    updateLocationHdfs_authenticationType,
    updateLocationHdfs_blockSize,
    updateLocationHdfs_kerberosKeytab,
    updateLocationHdfs_kerberosKrb5Conf,
    updateLocationHdfs_kerberosPrincipal,
    updateLocationHdfs_kmsKeyProviderUri,
    updateLocationHdfs_nameNodes,
    updateLocationHdfs_qopConfiguration,
    updateLocationHdfs_replicationFactor,
    updateLocationHdfs_simpleUser,
    updateLocationHdfs_subdirectory,
    updateLocationHdfs_locationArn,
    updateLocationHdfsResponse_httpStatus,

    -- ** UpdateLocationNfs
    updateLocationNfs_mountOptions,
    updateLocationNfs_onPremConfig,
    updateLocationNfs_subdirectory,
    updateLocationNfs_locationArn,
    updateLocationNfsResponse_httpStatus,

    -- ** UpdateLocationObjectStorage
    updateLocationObjectStorage_accessKey,
    updateLocationObjectStorage_agentArns,
    updateLocationObjectStorage_secretKey,
    updateLocationObjectStorage_serverCertificate,
    updateLocationObjectStorage_serverPort,
    updateLocationObjectStorage_serverProtocol,
    updateLocationObjectStorage_subdirectory,
    updateLocationObjectStorage_locationArn,
    updateLocationObjectStorageResponse_httpStatus,

    -- ** UpdateLocationSmb
    updateLocationSmb_agentArns,
    updateLocationSmb_domain,
    updateLocationSmb_mountOptions,
    updateLocationSmb_password,
    updateLocationSmb_subdirectory,
    updateLocationSmb_user,
    updateLocationSmb_locationArn,
    updateLocationSmbResponse_httpStatus,

    -- ** UpdateStorageSystem
    updateStorageSystem_agentArns,
    updateStorageSystem_cloudWatchLogGroupArn,
    updateStorageSystem_credentials,
    updateStorageSystem_name,
    updateStorageSystem_serverConfiguration,
    updateStorageSystem_storageSystemArn,
    updateStorageSystemResponse_httpStatus,

    -- ** UpdateTask
    updateTask_cloudWatchLogGroupArn,
    updateTask_excludes,
    updateTask_includes,
    updateTask_name,
    updateTask_options,
    updateTask_schedule,
    updateTask_taskArn,
    updateTaskResponse_httpStatus,

    -- ** UpdateTaskExecution
    updateTaskExecution_taskExecutionArn,
    updateTaskExecution_options,
    updateTaskExecutionResponse_httpStatus,

    -- * Types

    -- ** AgentListEntry
    agentListEntry_agentArn,
    agentListEntry_name,
    agentListEntry_status,

    -- ** Capacity
    capacity_logicalUsed,
    capacity_provisioned,
    capacity_used,

    -- ** Credentials
    credentials_username,
    credentials_password,

    -- ** DiscoveryJobListEntry
    discoveryJobListEntry_discoveryJobArn,
    discoveryJobListEntry_status,

    -- ** DiscoveryServerConfiguration
    discoveryServerConfiguration_serverPort,
    discoveryServerConfiguration_serverHostname,

    -- ** Ec2Config
    ec2Config_subnetArn,
    ec2Config_securityGroupArns,

    -- ** FilterRule
    filterRule_filterType,
    filterRule_value,

    -- ** FsxProtocol
    fsxProtocol_nfs,
    fsxProtocol_smb,

    -- ** FsxProtocolNfs
    fsxProtocolNfs_mountOptions,

    -- ** FsxProtocolSmb
    fsxProtocolSmb_domain,
    fsxProtocolSmb_mountOptions,
    fsxProtocolSmb_password,
    fsxProtocolSmb_user,

    -- ** HdfsNameNode
    hdfsNameNode_hostname,
    hdfsNameNode_port,

    -- ** IOPS
    iops_other,
    iops_read,
    iops_total,
    iops_write,

    -- ** Latency
    latency_other,
    latency_read,
    latency_write,

    -- ** LocationFilter
    locationFilter_name,
    locationFilter_values,
    locationFilter_operator,

    -- ** LocationListEntry
    locationListEntry_locationArn,
    locationListEntry_locationUri,

    -- ** MaxP95Performance
    maxP95Performance_iopsOther,
    maxP95Performance_iopsRead,
    maxP95Performance_iopsTotal,
    maxP95Performance_iopsWrite,
    maxP95Performance_latencyOther,
    maxP95Performance_latencyRead,
    maxP95Performance_latencyWrite,
    maxP95Performance_throughputOther,
    maxP95Performance_throughputRead,
    maxP95Performance_throughputTotal,
    maxP95Performance_throughputWrite,

    -- ** NetAppONTAPCluster
    netAppONTAPCluster_cifsShareCount,
    netAppONTAPCluster_clusterBlockStorageLogicalUsed,
    netAppONTAPCluster_clusterBlockStorageSize,
    netAppONTAPCluster_clusterBlockStorageUsed,
    netAppONTAPCluster_clusterName,
    netAppONTAPCluster_maxP95Performance,
    netAppONTAPCluster_nfsExportedVolumes,
    netAppONTAPCluster_recommendationStatus,
    netAppONTAPCluster_recommendations,
    netAppONTAPCluster_resourceId,

    -- ** NetAppONTAPSVM
    netAppONTAPSVM_cifsShareCount,
    netAppONTAPSVM_clusterUuid,
    netAppONTAPSVM_enabledProtocols,
    netAppONTAPSVM_maxP95Performance,
    netAppONTAPSVM_nfsExportedVolumes,
    netAppONTAPSVM_recommendationStatus,
    netAppONTAPSVM_recommendations,
    netAppONTAPSVM_resourceId,
    netAppONTAPSVM_svmName,
    netAppONTAPSVM_totalCapacityProvisioned,
    netAppONTAPSVM_totalCapacityUsed,
    netAppONTAPSVM_totalLogicalCapacityUsed,
    netAppONTAPSVM_totalSnapshotCapacityUsed,

    -- ** NetAppONTAPVolume
    netAppONTAPVolume_capacityProvisioned,
    netAppONTAPVolume_capacityUsed,
    netAppONTAPVolume_cifsShareCount,
    netAppONTAPVolume_logicalCapacityUsed,
    netAppONTAPVolume_maxP95Performance,
    netAppONTAPVolume_nfsExported,
    netAppONTAPVolume_recommendationStatus,
    netAppONTAPVolume_recommendations,
    netAppONTAPVolume_resourceId,
    netAppONTAPVolume_securityStyle,
    netAppONTAPVolume_snapshotCapacityUsed,
    netAppONTAPVolume_svmName,
    netAppONTAPVolume_svmUuid,
    netAppONTAPVolume_volumeName,

    -- ** NfsMountOptions
    nfsMountOptions_version,

    -- ** OnPremConfig
    onPremConfig_agentArns,

    -- ** Options
    options_atime,
    options_bytesPerSecond,
    options_gid,
    options_logLevel,
    options_mtime,
    options_objectTags,
    options_overwriteMode,
    options_posixPermissions,
    options_preserveDeletedFiles,
    options_preserveDevices,
    options_securityDescriptorCopyFlags,
    options_taskQueueing,
    options_transferMode,
    options_uid,
    options_verifyMode,

    -- ** P95Metrics
    p95Metrics_iops,
    p95Metrics_latency,
    p95Metrics_throughput,

    -- ** PrivateLinkConfig
    privateLinkConfig_privateLinkEndpoint,
    privateLinkConfig_securityGroupArns,
    privateLinkConfig_subnetArns,
    privateLinkConfig_vpcEndpointId,

    -- ** QopConfiguration
    qopConfiguration_dataTransferProtection,
    qopConfiguration_rpcProtection,

    -- ** Recommendation
    recommendation_estimatedMonthlyStorageCost,
    recommendation_storageConfiguration,
    recommendation_storageType,

    -- ** ResourceDetails
    resourceDetails_netAppONTAPClusters,
    resourceDetails_netAppONTAPSVMs,
    resourceDetails_netAppONTAPVolumes,

    -- ** ResourceMetrics
    resourceMetrics_capacity,
    resourceMetrics_p95Metrics,
    resourceMetrics_resourceId,
    resourceMetrics_resourceType,
    resourceMetrics_timestamp,

    -- ** S3Config
    s3Config_bucketAccessRoleArn,

    -- ** SmbMountOptions
    smbMountOptions_version,

    -- ** StorageSystemListEntry
    storageSystemListEntry_name,
    storageSystemListEntry_storageSystemArn,

    -- ** TagListEntry
    tagListEntry_value,
    tagListEntry_key,

    -- ** TaskExecutionListEntry
    taskExecutionListEntry_status,
    taskExecutionListEntry_taskExecutionArn,

    -- ** TaskExecutionResultDetail
    taskExecutionResultDetail_errorCode,
    taskExecutionResultDetail_errorDetail,
    taskExecutionResultDetail_prepareDuration,
    taskExecutionResultDetail_prepareStatus,
    taskExecutionResultDetail_totalDuration,
    taskExecutionResultDetail_transferDuration,
    taskExecutionResultDetail_transferStatus,
    taskExecutionResultDetail_verifyDuration,
    taskExecutionResultDetail_verifyStatus,

    -- ** TaskFilter
    taskFilter_name,
    taskFilter_values,
    taskFilter_operator,

    -- ** TaskListEntry
    taskListEntry_name,
    taskListEntry_status,
    taskListEntry_taskArn,

    -- ** TaskSchedule
    taskSchedule_scheduleExpression,

    -- ** Throughput
    throughput_other,
    throughput_read,
    throughput_total,
    throughput_write,
  )
where

import Amazonka.DataSync.AddStorageSystem
import Amazonka.DataSync.CancelTaskExecution
import Amazonka.DataSync.CreateAgent
import Amazonka.DataSync.CreateLocationEfs
import Amazonka.DataSync.CreateLocationFsxLustre
import Amazonka.DataSync.CreateLocationFsxOntap
import Amazonka.DataSync.CreateLocationFsxOpenZfs
import Amazonka.DataSync.CreateLocationFsxWindows
import Amazonka.DataSync.CreateLocationHdfs
import Amazonka.DataSync.CreateLocationNfs
import Amazonka.DataSync.CreateLocationObjectStorage
import Amazonka.DataSync.CreateLocationS3
import Amazonka.DataSync.CreateLocationSmb
import Amazonka.DataSync.CreateTask
import Amazonka.DataSync.DeleteAgent
import Amazonka.DataSync.DeleteLocation
import Amazonka.DataSync.DeleteTask
import Amazonka.DataSync.DescribeAgent
import Amazonka.DataSync.DescribeDiscoveryJob
import Amazonka.DataSync.DescribeLocationEfs
import Amazonka.DataSync.DescribeLocationFsxLustre
import Amazonka.DataSync.DescribeLocationFsxOntap
import Amazonka.DataSync.DescribeLocationFsxOpenZfs
import Amazonka.DataSync.DescribeLocationFsxWindows
import Amazonka.DataSync.DescribeLocationHdfs
import Amazonka.DataSync.DescribeLocationNfs
import Amazonka.DataSync.DescribeLocationObjectStorage
import Amazonka.DataSync.DescribeLocationS3
import Amazonka.DataSync.DescribeLocationSmb
import Amazonka.DataSync.DescribeStorageSystem
import Amazonka.DataSync.DescribeStorageSystemResourceMetrics
import Amazonka.DataSync.DescribeStorageSystemResources
import Amazonka.DataSync.DescribeTask
import Amazonka.DataSync.DescribeTaskExecution
import Amazonka.DataSync.GenerateRecommendations
import Amazonka.DataSync.ListAgents
import Amazonka.DataSync.ListDiscoveryJobs
import Amazonka.DataSync.ListLocations
import Amazonka.DataSync.ListStorageSystems
import Amazonka.DataSync.ListTagsForResource
import Amazonka.DataSync.ListTaskExecutions
import Amazonka.DataSync.ListTasks
import Amazonka.DataSync.RemoveStorageSystem
import Amazonka.DataSync.StartDiscoveryJob
import Amazonka.DataSync.StartTaskExecution
import Amazonka.DataSync.StopDiscoveryJob
import Amazonka.DataSync.TagResource
import Amazonka.DataSync.Types.AgentListEntry
import Amazonka.DataSync.Types.Capacity
import Amazonka.DataSync.Types.Credentials
import Amazonka.DataSync.Types.DiscoveryJobListEntry
import Amazonka.DataSync.Types.DiscoveryServerConfiguration
import Amazonka.DataSync.Types.Ec2Config
import Amazonka.DataSync.Types.FilterRule
import Amazonka.DataSync.Types.FsxProtocol
import Amazonka.DataSync.Types.FsxProtocolNfs
import Amazonka.DataSync.Types.FsxProtocolSmb
import Amazonka.DataSync.Types.HdfsNameNode
import Amazonka.DataSync.Types.IOPS
import Amazonka.DataSync.Types.Latency
import Amazonka.DataSync.Types.LocationFilter
import Amazonka.DataSync.Types.LocationListEntry
import Amazonka.DataSync.Types.MaxP95Performance
import Amazonka.DataSync.Types.NetAppONTAPCluster
import Amazonka.DataSync.Types.NetAppONTAPSVM
import Amazonka.DataSync.Types.NetAppONTAPVolume
import Amazonka.DataSync.Types.NfsMountOptions
import Amazonka.DataSync.Types.OnPremConfig
import Amazonka.DataSync.Types.Options
import Amazonka.DataSync.Types.P95Metrics
import Amazonka.DataSync.Types.PrivateLinkConfig
import Amazonka.DataSync.Types.QopConfiguration
import Amazonka.DataSync.Types.Recommendation
import Amazonka.DataSync.Types.ResourceDetails
import Amazonka.DataSync.Types.ResourceMetrics
import Amazonka.DataSync.Types.S3Config
import Amazonka.DataSync.Types.SmbMountOptions
import Amazonka.DataSync.Types.StorageSystemListEntry
import Amazonka.DataSync.Types.TagListEntry
import Amazonka.DataSync.Types.TaskExecutionListEntry
import Amazonka.DataSync.Types.TaskExecutionResultDetail
import Amazonka.DataSync.Types.TaskFilter
import Amazonka.DataSync.Types.TaskListEntry
import Amazonka.DataSync.Types.TaskSchedule
import Amazonka.DataSync.Types.Throughput
import Amazonka.DataSync.UntagResource
import Amazonka.DataSync.UpdateAgent
import Amazonka.DataSync.UpdateDiscoveryJob
import Amazonka.DataSync.UpdateLocationHdfs
import Amazonka.DataSync.UpdateLocationNfs
import Amazonka.DataSync.UpdateLocationObjectStorage
import Amazonka.DataSync.UpdateLocationSmb
import Amazonka.DataSync.UpdateStorageSystem
import Amazonka.DataSync.UpdateTask
import Amazonka.DataSync.UpdateTaskExecution
