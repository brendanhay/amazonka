{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataSync.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataSync.Lens
  ( -- * Operations

    -- ** UpdateTask
    updateTask_schedule,
    updateTask_includes,
    updateTask_name,
    updateTask_excludes,
    updateTask_options,
    updateTask_cloudWatchLogGroupArn,
    updateTask_taskArn,
    updateTaskResponse_httpStatus,

    -- ** DescribeAgent
    describeAgent_agentArn,
    describeAgentResponse_creationTime,
    describeAgentResponse_status,
    describeAgentResponse_privateLinkConfig,
    describeAgentResponse_endpointType,
    describeAgentResponse_lastConnectionTime,
    describeAgentResponse_agentArn,
    describeAgentResponse_name,
    describeAgentResponse_httpStatus,

    -- ** DeleteTask
    deleteTask_taskArn,
    deleteTaskResponse_httpStatus,

    -- ** DescribeLocationSmb
    describeLocationSmb_locationArn,
    describeLocationSmbResponse_creationTime,
    describeLocationSmbResponse_agentArns,
    describeLocationSmbResponse_domain,
    describeLocationSmbResponse_locationUri,
    describeLocationSmbResponse_user,
    describeLocationSmbResponse_mountOptions,
    describeLocationSmbResponse_locationArn,
    describeLocationSmbResponse_httpStatus,

    -- ** ListLocations
    listLocations_filters,
    listLocations_nextToken,
    listLocations_maxResults,
    listLocationsResponse_nextToken,
    listLocationsResponse_locations,
    listLocationsResponse_httpStatus,

    -- ** CreateLocationNfs
    createLocationNfs_mountOptions,
    createLocationNfs_tags,
    createLocationNfs_subdirectory,
    createLocationNfs_serverHostname,
    createLocationNfs_onPremConfig,
    createLocationNfsResponse_locationArn,
    createLocationNfsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeLocationFsxWindows
    describeLocationFsxWindows_locationArn,
    describeLocationFsxWindowsResponse_creationTime,
    describeLocationFsxWindowsResponse_securityGroupArns,
    describeLocationFsxWindowsResponse_domain,
    describeLocationFsxWindowsResponse_locationUri,
    describeLocationFsxWindowsResponse_user,
    describeLocationFsxWindowsResponse_locationArn,
    describeLocationFsxWindowsResponse_httpStatus,

    -- ** CreateLocationObjectStorage
    createLocationObjectStorage_serverPort,
    createLocationObjectStorage_serverProtocol,
    createLocationObjectStorage_secretKey,
    createLocationObjectStorage_subdirectory,
    createLocationObjectStorage_accessKey,
    createLocationObjectStorage_tags,
    createLocationObjectStorage_serverHostname,
    createLocationObjectStorage_bucketName,
    createLocationObjectStorage_agentArns,
    createLocationObjectStorageResponse_locationArn,
    createLocationObjectStorageResponse_httpStatus,

    -- ** DescribeTask
    describeTask_taskArn,
    describeTaskResponse_creationTime,
    describeTaskResponse_status,
    describeTaskResponse_schedule,
    describeTaskResponse_taskArn,
    describeTaskResponse_currentTaskExecutionArn,
    describeTaskResponse_destinationNetworkInterfaceArns,
    describeTaskResponse_destinationLocationArn,
    describeTaskResponse_includes,
    describeTaskResponse_name,
    describeTaskResponse_errorCode,
    describeTaskResponse_sourceLocationArn,
    describeTaskResponse_excludes,
    describeTaskResponse_sourceNetworkInterfaceArns,
    describeTaskResponse_options,
    describeTaskResponse_cloudWatchLogGroupArn,
    describeTaskResponse_errorDetail,
    describeTaskResponse_httpStatus,

    -- ** DescribeLocationS3
    describeLocationS3_locationArn,
    describeLocationS3Response_creationTime,
    describeLocationS3Response_agentArns,
    describeLocationS3Response_s3StorageClass,
    describeLocationS3Response_locationUri,
    describeLocationS3Response_s3Config,
    describeLocationS3Response_locationArn,
    describeLocationS3Response_httpStatus,

    -- ** ListAgents
    listAgents_nextToken,
    listAgents_maxResults,
    listAgentsResponse_agents,
    listAgentsResponse_nextToken,
    listAgentsResponse_httpStatus,

    -- ** UpdateLocationSmb
    updateLocationSmb_agentArns,
    updateLocationSmb_domain,
    updateLocationSmb_user,
    updateLocationSmb_password,
    updateLocationSmb_subdirectory,
    updateLocationSmb_mountOptions,
    updateLocationSmb_locationArn,
    updateLocationSmbResponse_httpStatus,

    -- ** DeleteAgent
    deleteAgent_agentArn,
    deleteAgentResponse_httpStatus,

    -- ** UpdateAgent
    updateAgent_name,
    updateAgent_agentArn,
    updateAgentResponse_httpStatus,

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

    -- ** ListTaskExecutions
    listTaskExecutions_taskArn,
    listTaskExecutions_nextToken,
    listTaskExecutions_maxResults,
    listTaskExecutionsResponse_nextToken,
    listTaskExecutionsResponse_taskExecutions,
    listTaskExecutionsResponse_httpStatus,

    -- ** UpdateTaskExecution
    updateTaskExecution_taskExecutionArn,
    updateTaskExecution_options,
    updateTaskExecutionResponse_httpStatus,

    -- ** CreateLocationS3
    createLocationS3_agentArns,
    createLocationS3_s3StorageClass,
    createLocationS3_subdirectory,
    createLocationS3_tags,
    createLocationS3_s3BucketArn,
    createLocationS3_s3Config,
    createLocationS3Response_locationArn,
    createLocationS3Response_httpStatus,

    -- ** CreateTask
    createTask_schedule,
    createTask_includes,
    createTask_name,
    createTask_excludes,
    createTask_options,
    createTask_cloudWatchLogGroupArn,
    createTask_tags,
    createTask_sourceLocationArn,
    createTask_destinationLocationArn,
    createTaskResponse_taskArn,
    createTaskResponse_httpStatus,

    -- ** CreateLocationEfs
    createLocationEfs_subdirectory,
    createLocationEfs_tags,
    createLocationEfs_efsFilesystemArn,
    createLocationEfs_ec2Config,
    createLocationEfsResponse_locationArn,
    createLocationEfsResponse_httpStatus,

    -- ** DescribeLocationObjectStorage
    describeLocationObjectStorage_locationArn,
    describeLocationObjectStorageResponse_serverPort,
    describeLocationObjectStorageResponse_creationTime,
    describeLocationObjectStorageResponse_agentArns,
    describeLocationObjectStorageResponse_locationUri,
    describeLocationObjectStorageResponse_serverProtocol,
    describeLocationObjectStorageResponse_locationArn,
    describeLocationObjectStorageResponse_accessKey,
    describeLocationObjectStorageResponse_httpStatus,

    -- ** DeleteLocation
    deleteLocation_locationArn,
    deleteLocationResponse_httpStatus,

    -- ** ListTasks
    listTasks_filters,
    listTasks_nextToken,
    listTasks_maxResults,
    listTasksResponse_tasks,
    listTasksResponse_nextToken,
    listTasksResponse_httpStatus,

    -- ** StartTaskExecution
    startTaskExecution_overrideOptions,
    startTaskExecution_includes,
    startTaskExecution_excludes,
    startTaskExecution_taskArn,
    startTaskExecutionResponse_taskExecutionArn,
    startTaskExecutionResponse_httpStatus,

    -- ** UpdateLocationNfs
    updateLocationNfs_onPremConfig,
    updateLocationNfs_subdirectory,
    updateLocationNfs_mountOptions,
    updateLocationNfs_locationArn,
    updateLocationNfsResponse_httpStatus,

    -- ** DescribeTaskExecution
    describeTaskExecution_taskExecutionArn,
    describeTaskExecutionResponse_status,
    describeTaskExecutionResponse_taskExecutionArn,
    describeTaskExecutionResponse_startTime,
    describeTaskExecutionResponse_filesTransferred,
    describeTaskExecutionResponse_bytesWritten,
    describeTaskExecutionResponse_bytesTransferred,
    describeTaskExecutionResponse_result,
    describeTaskExecutionResponse_includes,
    describeTaskExecutionResponse_estimatedFilesToTransfer,
    describeTaskExecutionResponse_excludes,
    describeTaskExecutionResponse_options,
    describeTaskExecutionResponse_estimatedBytesToTransfer,
    describeTaskExecutionResponse_httpStatus,

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

    -- ** CreateAgent
    createAgent_securityGroupArns,
    createAgent_subnetArns,
    createAgent_agentName,
    createAgent_vpcEndpointId,
    createAgent_tags,
    createAgent_activationKey,
    createAgentResponse_agentArn,
    createAgentResponse_httpStatus,

    -- ** UpdateLocationObjectStorage
    updateLocationObjectStorage_serverPort,
    updateLocationObjectStorage_agentArns,
    updateLocationObjectStorage_serverProtocol,
    updateLocationObjectStorage_secretKey,
    updateLocationObjectStorage_subdirectory,
    updateLocationObjectStorage_accessKey,
    updateLocationObjectStorage_locationArn,
    updateLocationObjectStorageResponse_httpStatus,

    -- ** DescribeLocationEfs
    describeLocationEfs_locationArn,
    describeLocationEfsResponse_creationTime,
    describeLocationEfsResponse_locationUri,
    describeLocationEfsResponse_locationArn,
    describeLocationEfsResponse_ec2Config,
    describeLocationEfsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_keys,
    untagResourceResponse_httpStatus,

    -- ** DescribeLocationNfs
    describeLocationNfs_locationArn,
    describeLocationNfsResponse_creationTime,
    describeLocationNfsResponse_locationUri,
    describeLocationNfsResponse_onPremConfig,
    describeLocationNfsResponse_mountOptions,
    describeLocationNfsResponse_locationArn,
    describeLocationNfsResponse_httpStatus,

    -- ** CancelTaskExecution
    cancelTaskExecution_taskExecutionArn,
    cancelTaskExecutionResponse_httpStatus,

    -- * Types

    -- ** AgentListEntry
    agentListEntry_status,
    agentListEntry_agentArn,
    agentListEntry_name,

    -- ** Ec2Config
    ec2Config_subnetArn,
    ec2Config_securityGroupArns,

    -- ** FilterRule
    filterRule_filterType,
    filterRule_value,

    -- ** LocationFilter
    locationFilter_name,
    locationFilter_values,
    locationFilter_operator,

    -- ** LocationListEntry
    locationListEntry_locationUri,
    locationListEntry_locationArn,

    -- ** NfsMountOptions
    nfsMountOptions_version,

    -- ** OnPremConfig
    onPremConfig_agentArns,

    -- ** Options
    options_atime,
    options_verifyMode,
    options_taskQueueing,
    options_logLevel,
    options_posixPermissions,
    options_mtime,
    options_uid,
    options_bytesPerSecond,
    options_securityDescriptorCopyFlags,
    options_gid,
    options_overwriteMode,
    options_transferMode,
    options_preserveDeletedFiles,
    options_preserveDevices,

    -- ** PrivateLinkConfig
    privateLinkConfig_securityGroupArns,
    privateLinkConfig_subnetArns,
    privateLinkConfig_privateLinkEndpoint,
    privateLinkConfig_vpcEndpointId,

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
    taskExecutionResultDetail_prepareDuration,
    taskExecutionResultDetail_prepareStatus,
    taskExecutionResultDetail_verifyStatus,
    taskExecutionResultDetail_verifyDuration,
    taskExecutionResultDetail_totalDuration,
    taskExecutionResultDetail_transferStatus,
    taskExecutionResultDetail_errorCode,
    taskExecutionResultDetail_transferDuration,
    taskExecutionResultDetail_errorDetail,

    -- ** TaskFilter
    taskFilter_name,
    taskFilter_values,
    taskFilter_operator,

    -- ** TaskListEntry
    taskListEntry_status,
    taskListEntry_taskArn,
    taskListEntry_name,

    -- ** TaskSchedule
    taskSchedule_scheduleExpression,
  )
where

import Network.AWS.DataSync.CancelTaskExecution
import Network.AWS.DataSync.CreateAgent
import Network.AWS.DataSync.CreateLocationEfs
import Network.AWS.DataSync.CreateLocationFsxWindows
import Network.AWS.DataSync.CreateLocationNfs
import Network.AWS.DataSync.CreateLocationObjectStorage
import Network.AWS.DataSync.CreateLocationS3
import Network.AWS.DataSync.CreateLocationSmb
import Network.AWS.DataSync.CreateTask
import Network.AWS.DataSync.DeleteAgent
import Network.AWS.DataSync.DeleteLocation
import Network.AWS.DataSync.DeleteTask
import Network.AWS.DataSync.DescribeAgent
import Network.AWS.DataSync.DescribeLocationEfs
import Network.AWS.DataSync.DescribeLocationFsxWindows
import Network.AWS.DataSync.DescribeLocationNfs
import Network.AWS.DataSync.DescribeLocationObjectStorage
import Network.AWS.DataSync.DescribeLocationS3
import Network.AWS.DataSync.DescribeLocationSmb
import Network.AWS.DataSync.DescribeTask
import Network.AWS.DataSync.DescribeTaskExecution
import Network.AWS.DataSync.ListAgents
import Network.AWS.DataSync.ListLocations
import Network.AWS.DataSync.ListTagsForResource
import Network.AWS.DataSync.ListTaskExecutions
import Network.AWS.DataSync.ListTasks
import Network.AWS.DataSync.StartTaskExecution
import Network.AWS.DataSync.TagResource
import Network.AWS.DataSync.Types.AgentListEntry
import Network.AWS.DataSync.Types.Ec2Config
import Network.AWS.DataSync.Types.FilterRule
import Network.AWS.DataSync.Types.LocationFilter
import Network.AWS.DataSync.Types.LocationListEntry
import Network.AWS.DataSync.Types.NfsMountOptions
import Network.AWS.DataSync.Types.OnPremConfig
import Network.AWS.DataSync.Types.Options
import Network.AWS.DataSync.Types.PrivateLinkConfig
import Network.AWS.DataSync.Types.S3Config
import Network.AWS.DataSync.Types.SmbMountOptions
import Network.AWS.DataSync.Types.TagListEntry
import Network.AWS.DataSync.Types.TaskExecutionListEntry
import Network.AWS.DataSync.Types.TaskExecutionResultDetail
import Network.AWS.DataSync.Types.TaskFilter
import Network.AWS.DataSync.Types.TaskListEntry
import Network.AWS.DataSync.Types.TaskSchedule
import Network.AWS.DataSync.UntagResource
import Network.AWS.DataSync.UpdateAgent
import Network.AWS.DataSync.UpdateLocationNfs
import Network.AWS.DataSync.UpdateLocationObjectStorage
import Network.AWS.DataSync.UpdateLocationSmb
import Network.AWS.DataSync.UpdateTask
import Network.AWS.DataSync.UpdateTaskExecution
