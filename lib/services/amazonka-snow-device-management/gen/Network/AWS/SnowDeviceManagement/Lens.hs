{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SnowDeviceManagement.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SnowDeviceManagement.Lens
  ( -- * Operations

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeTask
    describeTask_taskId,
    describeTaskResponse_state,
    describeTaskResponse_lastUpdatedAt,
    describeTaskResponse_createdAt,
    describeTaskResponse_taskId,
    describeTaskResponse_taskArn,
    describeTaskResponse_targets,
    describeTaskResponse_completedAt,
    describeTaskResponse_description,
    describeTaskResponse_tags,
    describeTaskResponse_httpStatus,

    -- ** ListDeviceResources
    listDeviceResources_nextToken,
    listDeviceResources_type,
    listDeviceResources_maxResults,
    listDeviceResources_managedDeviceId,
    listDeviceResourcesResponse_resources,
    listDeviceResourcesResponse_nextToken,
    listDeviceResourcesResponse_httpStatus,

    -- ** ListExecutions
    listExecutions_state,
    listExecutions_nextToken,
    listExecutions_maxResults,
    listExecutions_taskId,
    listExecutionsResponse_executions,
    listExecutionsResponse_nextToken,
    listExecutionsResponse_httpStatus,

    -- ** DescribeDeviceEc2Instances
    describeDeviceEc2Instances_instanceIds,
    describeDeviceEc2Instances_managedDeviceId,
    describeDeviceEc2InstancesResponse_instances,
    describeDeviceEc2InstancesResponse_httpStatus,

    -- ** CreateTask
    createTask_clientToken,
    createTask_description,
    createTask_tags,
    createTask_command,
    createTask_targets,
    createTaskResponse_taskId,
    createTaskResponse_taskArn,
    createTaskResponse_httpStatus,

    -- ** ListTasks
    listTasks_state,
    listTasks_nextToken,
    listTasks_maxResults,
    listTasksResponse_tasks,
    listTasksResponse_nextToken,
    listTasksResponse_httpStatus,

    -- ** DescribeExecution
    describeExecution_managedDeviceId,
    describeExecution_taskId,
    describeExecutionResponse_executionId,
    describeExecutionResponse_state,
    describeExecutionResponse_lastUpdatedAt,
    describeExecutionResponse_taskId,
    describeExecutionResponse_startedAt,
    describeExecutionResponse_managedDeviceId,
    describeExecutionResponse_httpStatus,

    -- ** DescribeDevice
    describeDevice_managedDeviceId,
    describeDeviceResponse_deviceState,
    describeDeviceResponse_deviceCapacities,
    describeDeviceResponse_lastUpdatedAt,
    describeDeviceResponse_associatedWithJob,
    describeDeviceResponse_lastReachedOutAt,
    describeDeviceResponse_software,
    describeDeviceResponse_physicalNetworkInterfaces,
    describeDeviceResponse_managedDeviceId,
    describeDeviceResponse_managedDeviceArn,
    describeDeviceResponse_deviceType,
    describeDeviceResponse_tags,
    describeDeviceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** CancelTask
    cancelTask_taskId,
    cancelTaskResponse_taskId,
    cancelTaskResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** ListDevices
    listDevices_jobId,
    listDevices_nextToken,
    listDevices_maxResults,
    listDevicesResponse_nextToken,
    listDevicesResponse_devices,
    listDevicesResponse_httpStatus,

    -- * Types

    -- ** Capacity
    capacity_used,
    capacity_name,
    capacity_total,
    capacity_unit,
    capacity_available,

    -- ** Command
    command_unlock,
    command_reboot,

    -- ** CpuOptions
    cpuOptions_coreCount,
    cpuOptions_threadsPerCore,

    -- ** DeviceSummary
    deviceSummary_associatedWithJob,
    deviceSummary_managedDeviceId,
    deviceSummary_managedDeviceArn,
    deviceSummary_tags,

    -- ** EbsInstanceBlockDevice
    ebsInstanceBlockDevice_deleteOnTermination,
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_volumeId,
    ebsInstanceBlockDevice_attachTime,

    -- ** ExecutionSummary
    executionSummary_executionId,
    executionSummary_state,
    executionSummary_taskId,
    executionSummary_managedDeviceId,

    -- ** Instance
    instance_instanceId,
    instance_state,
    instance_securityGroups,
    instance_createdAt,
    instance_cpuOptions,
    instance_rootDeviceName,
    instance_instanceType,
    instance_imageId,
    instance_privateIpAddress,
    instance_updatedAt,
    instance_blockDeviceMappings,
    instance_publicIpAddress,
    instance_amiLaunchIndex,

    -- ** InstanceBlockDeviceMapping
    instanceBlockDeviceMapping_ebs,
    instanceBlockDeviceMapping_deviceName,

    -- ** InstanceState
    instanceState_name,
    instanceState_code,

    -- ** InstanceSummary
    instanceSummary_lastUpdatedAt,
    instanceSummary_instance,

    -- ** PhysicalNetworkInterface
    physicalNetworkInterface_ipAddress,
    physicalNetworkInterface_macAddress,
    physicalNetworkInterface_ipAddressAssignment,
    physicalNetworkInterface_defaultGateway,
    physicalNetworkInterface_physicalNetworkInterfaceId,
    physicalNetworkInterface_netmask,
    physicalNetworkInterface_physicalConnectorType,

    -- ** Reboot

    -- ** ResourceSummary
    resourceSummary_arn,
    resourceSummary_id,
    resourceSummary_resourceType,

    -- ** SecurityGroupIdentifier
    securityGroupIdentifier_groupId,
    securityGroupIdentifier_groupName,

    -- ** SoftwareInformation
    softwareInformation_installedVersion,
    softwareInformation_installState,
    softwareInformation_installingVersion,

    -- ** TaskSummary
    taskSummary_state,
    taskSummary_taskArn,
    taskSummary_tags,
    taskSummary_taskId,

    -- ** Unlock
  )
where

import Network.AWS.SnowDeviceManagement.CancelTask
import Network.AWS.SnowDeviceManagement.CreateTask
import Network.AWS.SnowDeviceManagement.DescribeDevice
import Network.AWS.SnowDeviceManagement.DescribeDeviceEc2Instances
import Network.AWS.SnowDeviceManagement.DescribeExecution
import Network.AWS.SnowDeviceManagement.DescribeTask
import Network.AWS.SnowDeviceManagement.ListDeviceResources
import Network.AWS.SnowDeviceManagement.ListDevices
import Network.AWS.SnowDeviceManagement.ListExecutions
import Network.AWS.SnowDeviceManagement.ListTagsForResource
import Network.AWS.SnowDeviceManagement.ListTasks
import Network.AWS.SnowDeviceManagement.TagResource
import Network.AWS.SnowDeviceManagement.Types.Capacity
import Network.AWS.SnowDeviceManagement.Types.Command
import Network.AWS.SnowDeviceManagement.Types.CpuOptions
import Network.AWS.SnowDeviceManagement.Types.DeviceSummary
import Network.AWS.SnowDeviceManagement.Types.EbsInstanceBlockDevice
import Network.AWS.SnowDeviceManagement.Types.ExecutionSummary
import Network.AWS.SnowDeviceManagement.Types.Instance
import Network.AWS.SnowDeviceManagement.Types.InstanceBlockDeviceMapping
import Network.AWS.SnowDeviceManagement.Types.InstanceState
import Network.AWS.SnowDeviceManagement.Types.InstanceSummary
import Network.AWS.SnowDeviceManagement.Types.PhysicalNetworkInterface
import Network.AWS.SnowDeviceManagement.Types.Reboot
import Network.AWS.SnowDeviceManagement.Types.ResourceSummary
import Network.AWS.SnowDeviceManagement.Types.SecurityGroupIdentifier
import Network.AWS.SnowDeviceManagement.Types.SoftwareInformation
import Network.AWS.SnowDeviceManagement.Types.TaskSummary
import Network.AWS.SnowDeviceManagement.Types.Unlock
import Network.AWS.SnowDeviceManagement.UntagResource
