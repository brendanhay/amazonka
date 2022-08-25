{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataSync.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Lens
  ( -- * Operations

    -- ** CancelTaskExecution
    cancelTaskExecution_taskExecutionArn,
    cancelTaskExecutionResponse_httpStatus,

    -- ** CreateAgent
    createAgent_tags,
    createAgent_agentName,
    createAgent_subnetArns,
    createAgent_vpcEndpointId,
    createAgent_securityGroupArns,
    createAgent_activationKey,
    createAgentResponse_agentArn,
    createAgentResponse_httpStatus,

    -- ** CreateLocationEfs
    createLocationEfs_tags,
    createLocationEfs_inTransitEncryption,
    createLocationEfs_accessPointArn,
    createLocationEfs_fileSystemAccessRoleArn,
    createLocationEfs_subdirectory,
    createLocationEfs_efsFilesystemArn,
    createLocationEfs_ec2Config,
    createLocationEfsResponse_locationArn,
    createLocationEfsResponse_httpStatus,

    -- ** CreateLocationFsxLustre
    createLocationFsxLustre_tags,
    createLocationFsxLustre_subdirectory,
    createLocationFsxLustre_fsxFilesystemArn,
    createLocationFsxLustre_securityGroupArns,
    createLocationFsxLustreResponse_locationArn,
    createLocationFsxLustreResponse_httpStatus,

    -- ** CreateLocationFsxOntap
    createLocationFsxOntap_tags,
    createLocationFsxOntap_subdirectory,
    createLocationFsxOntap_protocol,
    createLocationFsxOntap_securityGroupArns,
    createLocationFsxOntap_storageVirtualMachineArn,
    createLocationFsxOntapResponse_locationArn,
    createLocationFsxOntapResponse_httpStatus,

    -- ** CreateLocationFsxOpenZfs
    createLocationFsxOpenZfs_tags,
    createLocationFsxOpenZfs_subdirectory,
    createLocationFsxOpenZfs_fsxFilesystemArn,
    createLocationFsxOpenZfs_protocol,
    createLocationFsxOpenZfs_securityGroupArns,
    createLocationFsxOpenZfsResponse_locationArn,
    createLocationFsxOpenZfsResponse_httpStatus,

    -- ** CreateLocationFsxWindows
    createLocationFsxWindows_tags,
    createLocationFsxWindows_domain,
    createLocationFsxWindows_subdirectory,
    createLocationFsxWindows_fsxFilesystemArn,
    createLocationFsxWindows_securityGroupArns,
    createLocationFsxWindows_user,
    createLocationFsxWindows_password,
    createLocationFsxWindowsResponse_locationArn,
    createLocationFsxWindowsResponse_httpStatus,

    -- ** CreateLocationHdfs
    createLocationHdfs_tags,
    createLocationHdfs_kerberosKrb5Conf,
    createLocationHdfs_kerberosPrincipal,
    createLocationHdfs_replicationFactor,
    createLocationHdfs_kmsKeyProviderUri,
    createLocationHdfs_qopConfiguration,
    createLocationHdfs_simpleUser,
    createLocationHdfs_blockSize,
    createLocationHdfs_kerberosKeytab,
    createLocationHdfs_subdirectory,
    createLocationHdfs_nameNodes,
    createLocationHdfs_authenticationType,
    createLocationHdfs_agentArns,
    createLocationHdfsResponse_locationArn,
    createLocationHdfsResponse_httpStatus,

    -- ** CreateLocationNfs
    createLocationNfs_tags,
    createLocationNfs_mountOptions,
    createLocationNfs_subdirectory,
    createLocationNfs_serverHostname,
    createLocationNfs_onPremConfig,
    createLocationNfsResponse_locationArn,
    createLocationNfsResponse_httpStatus,

    -- ** CreateLocationObjectStorage
    createLocationObjectStorage_tags,
    createLocationObjectStorage_serverProtocol,
    createLocationObjectStorage_serverPort,
    createLocationObjectStorage_accessKey,
    createLocationObjectStorage_secretKey,
    createLocationObjectStorage_subdirectory,
    createLocationObjectStorage_serverHostname,
    createLocationObjectStorage_bucketName,
    createLocationObjectStorage_agentArns,
    createLocationObjectStorageResponse_locationArn,
    createLocationObjectStorageResponse_httpStatus,

    -- ** CreateLocationS3
    createLocationS3_tags,
    createLocationS3_s3StorageClass,
    createLocationS3_subdirectory,
    createLocationS3_agentArns,
    createLocationS3_s3BucketArn,
    createLocationS3_s3Config,
    createLocationS3Response_locationArn,
    createLocationS3Response_httpStatus,

    -- ** CreateLocationSmb
    createLocationSmb_tags,
    createLocationSmb_domain,
    createLocationSmb_mountOptions,
    createLocationSmb_subdirectory,
    createLocationSmb_serverHostname,
    createLocationSmb_user,
    createLocationSmb_password,
    createLocationSmb_agentArns,
    createLocationSmbResponse_locationArn,
    createLocationSmbResponse_httpStatus,

    -- ** CreateTask
    createTask_tags,
    createTask_schedule,
    createTask_name,
    createTask_cloudWatchLogGroupArn,
    createTask_excludes,
    createTask_options,
    createTask_includes,
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
    describeAgentResponse_name,
    describeAgentResponse_privateLinkConfig,
    describeAgentResponse_status,
    describeAgentResponse_endpointType,
    describeAgentResponse_creationTime,
    describeAgentResponse_lastConnectionTime,
    describeAgentResponse_agentArn,
    describeAgentResponse_httpStatus,

    -- ** DescribeLocationEfs
    describeLocationEfs_locationArn,
    describeLocationEfsResponse_inTransitEncryption,
    describeLocationEfsResponse_accessPointArn,
    describeLocationEfsResponse_fileSystemAccessRoleArn,
    describeLocationEfsResponse_locationArn,
    describeLocationEfsResponse_ec2Config,
    describeLocationEfsResponse_locationUri,
    describeLocationEfsResponse_creationTime,
    describeLocationEfsResponse_httpStatus,

    -- ** DescribeLocationFsxLustre
    describeLocationFsxLustre_locationArn,
    describeLocationFsxLustreResponse_locationArn,
    describeLocationFsxLustreResponse_locationUri,
    describeLocationFsxLustreResponse_securityGroupArns,
    describeLocationFsxLustreResponse_creationTime,
    describeLocationFsxLustreResponse_httpStatus,

    -- ** DescribeLocationFsxOntap
    describeLocationFsxOntap_locationArn,
    describeLocationFsxOntapResponse_fsxFilesystemArn,
    describeLocationFsxOntapResponse_storageVirtualMachineArn,
    describeLocationFsxOntapResponse_locationArn,
    describeLocationFsxOntapResponse_locationUri,
    describeLocationFsxOntapResponse_securityGroupArns,
    describeLocationFsxOntapResponse_creationTime,
    describeLocationFsxOntapResponse_protocol,
    describeLocationFsxOntapResponse_httpStatus,

    -- ** DescribeLocationFsxOpenZfs
    describeLocationFsxOpenZfs_locationArn,
    describeLocationFsxOpenZfsResponse_locationArn,
    describeLocationFsxOpenZfsResponse_locationUri,
    describeLocationFsxOpenZfsResponse_securityGroupArns,
    describeLocationFsxOpenZfsResponse_creationTime,
    describeLocationFsxOpenZfsResponse_protocol,
    describeLocationFsxOpenZfsResponse_httpStatus,

    -- ** DescribeLocationFsxWindows
    describeLocationFsxWindows_locationArn,
    describeLocationFsxWindowsResponse_user,
    describeLocationFsxWindowsResponse_domain,
    describeLocationFsxWindowsResponse_locationArn,
    describeLocationFsxWindowsResponse_locationUri,
    describeLocationFsxWindowsResponse_securityGroupArns,
    describeLocationFsxWindowsResponse_creationTime,
    describeLocationFsxWindowsResponse_httpStatus,

    -- ** DescribeLocationHdfs
    describeLocationHdfs_locationArn,
    describeLocationHdfsResponse_authenticationType,
    describeLocationHdfsResponse_kerberosPrincipal,
    describeLocationHdfsResponse_replicationFactor,
    describeLocationHdfsResponse_locationArn,
    describeLocationHdfsResponse_kmsKeyProviderUri,
    describeLocationHdfsResponse_qopConfiguration,
    describeLocationHdfsResponse_nameNodes,
    describeLocationHdfsResponse_locationUri,
    describeLocationHdfsResponse_creationTime,
    describeLocationHdfsResponse_simpleUser,
    describeLocationHdfsResponse_blockSize,
    describeLocationHdfsResponse_agentArns,
    describeLocationHdfsResponse_httpStatus,

    -- ** DescribeLocationNfs
    describeLocationNfs_locationArn,
    describeLocationNfsResponse_onPremConfig,
    describeLocationNfsResponse_mountOptions,
    describeLocationNfsResponse_locationArn,
    describeLocationNfsResponse_locationUri,
    describeLocationNfsResponse_creationTime,
    describeLocationNfsResponse_httpStatus,

    -- ** DescribeLocationObjectStorage
    describeLocationObjectStorage_locationArn,
    describeLocationObjectStorageResponse_serverProtocol,
    describeLocationObjectStorageResponse_locationArn,
    describeLocationObjectStorageResponse_serverPort,
    describeLocationObjectStorageResponse_accessKey,
    describeLocationObjectStorageResponse_locationUri,
    describeLocationObjectStorageResponse_creationTime,
    describeLocationObjectStorageResponse_agentArns,
    describeLocationObjectStorageResponse_httpStatus,

    -- ** DescribeLocationS3
    describeLocationS3_locationArn,
    describeLocationS3Response_locationArn,
    describeLocationS3Response_s3Config,
    describeLocationS3Response_s3StorageClass,
    describeLocationS3Response_locationUri,
    describeLocationS3Response_creationTime,
    describeLocationS3Response_agentArns,
    describeLocationS3Response_httpStatus,

    -- ** DescribeLocationSmb
    describeLocationSmb_locationArn,
    describeLocationSmbResponse_user,
    describeLocationSmbResponse_domain,
    describeLocationSmbResponse_mountOptions,
    describeLocationSmbResponse_locationArn,
    describeLocationSmbResponse_locationUri,
    describeLocationSmbResponse_creationTime,
    describeLocationSmbResponse_agentArns,
    describeLocationSmbResponse_httpStatus,

    -- ** DescribeTask
    describeTask_taskArn,
    describeTaskResponse_schedule,
    describeTaskResponse_name,
    describeTaskResponse_taskArn,
    describeTaskResponse_cloudWatchLogGroupArn,
    describeTaskResponse_excludes,
    describeTaskResponse_sourceLocationArn,
    describeTaskResponse_status,
    describeTaskResponse_options,
    describeTaskResponse_destinationNetworkInterfaceArns,
    describeTaskResponse_errorCode,
    describeTaskResponse_destinationLocationArn,
    describeTaskResponse_includes,
    describeTaskResponse_creationTime,
    describeTaskResponse_currentTaskExecutionArn,
    describeTaskResponse_sourceNetworkInterfaceArns,
    describeTaskResponse_errorDetail,
    describeTaskResponse_httpStatus,

    -- ** DescribeTaskExecution
    describeTaskExecution_taskExecutionArn,
    describeTaskExecutionResponse_excludes,
    describeTaskExecutionResponse_filesTransferred,
    describeTaskExecutionResponse_status,
    describeTaskExecutionResponse_options,
    describeTaskExecutionResponse_estimatedFilesToTransfer,
    describeTaskExecutionResponse_includes,
    describeTaskExecutionResponse_result,
    describeTaskExecutionResponse_estimatedBytesToTransfer,
    describeTaskExecutionResponse_bytesTransferred,
    describeTaskExecutionResponse_taskExecutionArn,
    describeTaskExecutionResponse_bytesWritten,
    describeTaskExecutionResponse_startTime,
    describeTaskExecutionResponse_httpStatus,

    -- ** ListAgents
    listAgents_nextToken,
    listAgents_maxResults,
    listAgentsResponse_nextToken,
    listAgentsResponse_agents,
    listAgentsResponse_httpStatus,

    -- ** ListLocations
    listLocations_nextToken,
    listLocations_filters,
    listLocations_maxResults,
    listLocationsResponse_nextToken,
    listLocationsResponse_locations,
    listLocationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTaskExecutions
    listTaskExecutions_nextToken,
    listTaskExecutions_taskArn,
    listTaskExecutions_maxResults,
    listTaskExecutionsResponse_nextToken,
    listTaskExecutionsResponse_taskExecutions,
    listTaskExecutionsResponse_httpStatus,

    -- ** ListTasks
    listTasks_nextToken,
    listTasks_filters,
    listTasks_maxResults,
    listTasksResponse_tasks,
    listTasksResponse_nextToken,
    listTasksResponse_httpStatus,

    -- ** StartTaskExecution
    startTaskExecution_overrideOptions,
    startTaskExecution_excludes,
    startTaskExecution_includes,
    startTaskExecution_taskArn,
    startTaskExecutionResponse_taskExecutionArn,
    startTaskExecutionResponse_httpStatus,

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

    -- ** UpdateLocationHdfs
    updateLocationHdfs_authenticationType,
    updateLocationHdfs_kerberosKrb5Conf,
    updateLocationHdfs_kerberosPrincipal,
    updateLocationHdfs_replicationFactor,
    updateLocationHdfs_kmsKeyProviderUri,
    updateLocationHdfs_qopConfiguration,
    updateLocationHdfs_nameNodes,
    updateLocationHdfs_simpleUser,
    updateLocationHdfs_blockSize,
    updateLocationHdfs_kerberosKeytab,
    updateLocationHdfs_subdirectory,
    updateLocationHdfs_agentArns,
    updateLocationHdfs_locationArn,
    updateLocationHdfsResponse_httpStatus,

    -- ** UpdateLocationNfs
    updateLocationNfs_onPremConfig,
    updateLocationNfs_mountOptions,
    updateLocationNfs_subdirectory,
    updateLocationNfs_locationArn,
    updateLocationNfsResponse_httpStatus,

    -- ** UpdateLocationObjectStorage
    updateLocationObjectStorage_serverProtocol,
    updateLocationObjectStorage_serverPort,
    updateLocationObjectStorage_accessKey,
    updateLocationObjectStorage_secretKey,
    updateLocationObjectStorage_subdirectory,
    updateLocationObjectStorage_agentArns,
    updateLocationObjectStorage_locationArn,
    updateLocationObjectStorageResponse_httpStatus,

    -- ** UpdateLocationSmb
    updateLocationSmb_password,
    updateLocationSmb_user,
    updateLocationSmb_domain,
    updateLocationSmb_mountOptions,
    updateLocationSmb_subdirectory,
    updateLocationSmb_agentArns,
    updateLocationSmb_locationArn,
    updateLocationSmbResponse_httpStatus,

    -- ** UpdateTask
    updateTask_schedule,
    updateTask_name,
    updateTask_cloudWatchLogGroupArn,
    updateTask_excludes,
    updateTask_options,
    updateTask_includes,
    updateTask_taskArn,
    updateTaskResponse_httpStatus,

    -- ** UpdateTaskExecution
    updateTaskExecution_taskExecutionArn,
    updateTaskExecution_options,
    updateTaskExecutionResponse_httpStatus,

    -- * Types

    -- ** AgentListEntry
    agentListEntry_name,
    agentListEntry_status,
    agentListEntry_agentArn,

    -- ** Ec2Config
    ec2Config_subnetArn,
    ec2Config_securityGroupArns,

    -- ** FilterRule
    filterRule_filterType,
    filterRule_value,

    -- ** FsxProtocol
    fsxProtocol_smb,
    fsxProtocol_nfs,

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

    -- ** LocationFilter
    locationFilter_name,
    locationFilter_values,
    locationFilter_operator,

    -- ** LocationListEntry
    locationListEntry_locationArn,
    locationListEntry_locationUri,

    -- ** NfsMountOptions
    nfsMountOptions_version,

    -- ** OnPremConfig
    onPremConfig_agentArns,

    -- ** Options
    options_objectTags,
    options_gid,
    options_logLevel,
    options_taskQueueing,
    options_preserveDevices,
    options_overwriteMode,
    options_mtime,
    options_transferMode,
    options_uid,
    options_verifyMode,
    options_preserveDeletedFiles,
    options_atime,
    options_posixPermissions,
    options_securityDescriptorCopyFlags,
    options_bytesPerSecond,

    -- ** PrivateLinkConfig
    privateLinkConfig_subnetArns,
    privateLinkConfig_privateLinkEndpoint,
    privateLinkConfig_vpcEndpointId,
    privateLinkConfig_securityGroupArns,

    -- ** QopConfiguration
    qopConfiguration_dataTransferProtection,
    qopConfiguration_rpcProtection,

    -- ** S3Config
    s3Config_bucketAccessRoleArn,

    -- ** SmbMountOptions
    smbMountOptions_version,

    -- ** TagListEntry
    tagListEntry_value,
    tagListEntry_key,

    -- ** TaskExecutionListEntry
    taskExecutionListEntry_status,
    taskExecutionListEntry_taskExecutionArn,

    -- ** TaskExecutionResultDetail
    taskExecutionResultDetail_prepareStatus,
    taskExecutionResultDetail_totalDuration,
    taskExecutionResultDetail_transferStatus,
    taskExecutionResultDetail_verifyStatus,
    taskExecutionResultDetail_transferDuration,
    taskExecutionResultDetail_prepareDuration,
    taskExecutionResultDetail_errorCode,
    taskExecutionResultDetail_verifyDuration,
    taskExecutionResultDetail_errorDetail,

    -- ** TaskFilter
    taskFilter_name,
    taskFilter_values,
    taskFilter_operator,

    -- ** TaskListEntry
    taskListEntry_name,
    taskListEntry_taskArn,
    taskListEntry_status,

    -- ** TaskSchedule
    taskSchedule_scheduleExpression,
  )
where

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
import Amazonka.DataSync.DescribeTask
import Amazonka.DataSync.DescribeTaskExecution
import Amazonka.DataSync.ListAgents
import Amazonka.DataSync.ListLocations
import Amazonka.DataSync.ListTagsForResource
import Amazonka.DataSync.ListTaskExecutions
import Amazonka.DataSync.ListTasks
import Amazonka.DataSync.StartTaskExecution
import Amazonka.DataSync.TagResource
import Amazonka.DataSync.Types.AgentListEntry
import Amazonka.DataSync.Types.Ec2Config
import Amazonka.DataSync.Types.FilterRule
import Amazonka.DataSync.Types.FsxProtocol
import Amazonka.DataSync.Types.FsxProtocolNfs
import Amazonka.DataSync.Types.FsxProtocolSmb
import Amazonka.DataSync.Types.HdfsNameNode
import Amazonka.DataSync.Types.LocationFilter
import Amazonka.DataSync.Types.LocationListEntry
import Amazonka.DataSync.Types.NfsMountOptions
import Amazonka.DataSync.Types.OnPremConfig
import Amazonka.DataSync.Types.Options
import Amazonka.DataSync.Types.PrivateLinkConfig
import Amazonka.DataSync.Types.QopConfiguration
import Amazonka.DataSync.Types.S3Config
import Amazonka.DataSync.Types.SmbMountOptions
import Amazonka.DataSync.Types.TagListEntry
import Amazonka.DataSync.Types.TaskExecutionListEntry
import Amazonka.DataSync.Types.TaskExecutionResultDetail
import Amazonka.DataSync.Types.TaskFilter
import Amazonka.DataSync.Types.TaskListEntry
import Amazonka.DataSync.Types.TaskSchedule
import Amazonka.DataSync.UntagResource
import Amazonka.DataSync.UpdateAgent
import Amazonka.DataSync.UpdateLocationHdfs
import Amazonka.DataSync.UpdateLocationNfs
import Amazonka.DataSync.UpdateLocationObjectStorage
import Amazonka.DataSync.UpdateLocationSmb
import Amazonka.DataSync.UpdateTask
import Amazonka.DataSync.UpdateTaskExecution
