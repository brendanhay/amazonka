{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SnowDeviceManagement.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Lens
  ( -- * Operations

    -- ** CancelTask
    cancelTask_taskId,
    cancelTaskResponse_taskId,
    cancelTaskResponse_httpStatus,

    -- ** CreateTask
    createTask_tags,
    createTask_clientToken,
    createTask_description,
    createTask_command,
    createTask_targets,
    createTaskResponse_taskArn,
    createTaskResponse_taskId,
    createTaskResponse_httpStatus,

    -- ** DescribeDevice
    describeDevice_managedDeviceId,
    describeDeviceResponse_tags,
    describeDeviceResponse_deviceState,
    describeDeviceResponse_lastUpdatedAt,
    describeDeviceResponse_software,
    describeDeviceResponse_lastReachedOutAt,
    describeDeviceResponse_deviceCapacities,
    describeDeviceResponse_physicalNetworkInterfaces,
    describeDeviceResponse_associatedWithJob,
    describeDeviceResponse_managedDeviceArn,
    describeDeviceResponse_managedDeviceId,
    describeDeviceResponse_deviceType,
    describeDeviceResponse_httpStatus,

    -- ** DescribeDeviceEc2Instances
    describeDeviceEc2Instances_instanceIds,
    describeDeviceEc2Instances_managedDeviceId,
    describeDeviceEc2InstancesResponse_instances,
    describeDeviceEc2InstancesResponse_httpStatus,

    -- ** DescribeExecution
    describeExecution_managedDeviceId,
    describeExecution_taskId,
    describeExecutionResponse_lastUpdatedAt,
    describeExecutionResponse_taskId,
    describeExecutionResponse_state,
    describeExecutionResponse_executionId,
    describeExecutionResponse_startedAt,
    describeExecutionResponse_managedDeviceId,
    describeExecutionResponse_httpStatus,

    -- ** DescribeTask
    describeTask_taskId,
    describeTaskResponse_tags,
    describeTaskResponse_lastUpdatedAt,
    describeTaskResponse_taskArn,
    describeTaskResponse_taskId,
    describeTaskResponse_state,
    describeTaskResponse_targets,
    describeTaskResponse_description,
    describeTaskResponse_completedAt,
    describeTaskResponse_createdAt,
    describeTaskResponse_httpStatus,

    -- ** ListDeviceResources
    listDeviceResources_nextToken,
    listDeviceResources_type,
    listDeviceResources_maxResults,
    listDeviceResources_managedDeviceId,
    listDeviceResourcesResponse_nextToken,
    listDeviceResourcesResponse_resources,
    listDeviceResourcesResponse_httpStatus,

    -- ** ListDevices
    listDevices_nextToken,
    listDevices_jobId,
    listDevices_maxResults,
    listDevicesResponse_devices,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,

    -- ** ListExecutions
    listExecutions_nextToken,
    listExecutions_state,
    listExecutions_maxResults,
    listExecutions_taskId,
    listExecutionsResponse_executions,
    listExecutionsResponse_nextToken,
    listExecutionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTasks
    listTasks_nextToken,
    listTasks_state,
    listTasks_maxResults,
    listTasksResponse_tasks,
    listTasksResponse_nextToken,
    listTasksResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- * Types

    -- ** Capacity
    capacity_available,
    capacity_name,
    capacity_total,
    capacity_used,
    capacity_unit,

    -- ** Command
    command_reboot,
    command_unlock,

    -- ** CpuOptions
    cpuOptions_coreCount,
    cpuOptions_threadsPerCore,

    -- ** DeviceSummary
    deviceSummary_tags,
    deviceSummary_associatedWithJob,
    deviceSummary_managedDeviceArn,
    deviceSummary_managedDeviceId,

    -- ** EbsInstanceBlockDevice
    ebsInstanceBlockDevice_deleteOnTermination,
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_attachTime,
    ebsInstanceBlockDevice_volumeId,

    -- ** ExecutionSummary
    executionSummary_taskId,
    executionSummary_state,
    executionSummary_executionId,
    executionSummary_managedDeviceId,

    -- ** Instance
    instance_blockDeviceMappings,
    instance_amiLaunchIndex,
    instance_state,
    instance_instanceType,
    instance_instanceId,
    instance_publicIpAddress,
    instance_securityGroups,
    instance_privateIpAddress,
    instance_cpuOptions,
    instance_createdAt,
    instance_imageId,
    instance_updatedAt,
    instance_rootDeviceName,

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
    physicalNetworkInterface_ipAddressAssignment,
    physicalNetworkInterface_netmask,
    physicalNetworkInterface_physicalConnectorType,
    physicalNetworkInterface_macAddress,
    physicalNetworkInterface_defaultGateway,
    physicalNetworkInterface_physicalNetworkInterfaceId,
    physicalNetworkInterface_ipAddress,

    -- ** Reboot

    -- ** ResourceSummary
    resourceSummary_arn,
    resourceSummary_id,
    resourceSummary_resourceType,

    -- ** SecurityGroupIdentifier
    securityGroupIdentifier_groupName,
    securityGroupIdentifier_groupId,

    -- ** SoftwareInformation
    softwareInformation_installingVersion,
    softwareInformation_installedVersion,
    softwareInformation_installState,

    -- ** TaskSummary
    taskSummary_tags,
    taskSummary_taskArn,
    taskSummary_state,
    taskSummary_taskId,

    -- ** Unlock
  )
where

import Amazonka.SnowDeviceManagement.CancelTask
import Amazonka.SnowDeviceManagement.CreateTask
import Amazonka.SnowDeviceManagement.DescribeDevice
import Amazonka.SnowDeviceManagement.DescribeDeviceEc2Instances
import Amazonka.SnowDeviceManagement.DescribeExecution
import Amazonka.SnowDeviceManagement.DescribeTask
import Amazonka.SnowDeviceManagement.ListDeviceResources
import Amazonka.SnowDeviceManagement.ListDevices
import Amazonka.SnowDeviceManagement.ListExecutions
import Amazonka.SnowDeviceManagement.ListTagsForResource
import Amazonka.SnowDeviceManagement.ListTasks
import Amazonka.SnowDeviceManagement.TagResource
import Amazonka.SnowDeviceManagement.Types.Capacity
import Amazonka.SnowDeviceManagement.Types.Command
import Amazonka.SnowDeviceManagement.Types.CpuOptions
import Amazonka.SnowDeviceManagement.Types.DeviceSummary
import Amazonka.SnowDeviceManagement.Types.EbsInstanceBlockDevice
import Amazonka.SnowDeviceManagement.Types.ExecutionSummary
import Amazonka.SnowDeviceManagement.Types.Instance
import Amazonka.SnowDeviceManagement.Types.InstanceBlockDeviceMapping
import Amazonka.SnowDeviceManagement.Types.InstanceState
import Amazonka.SnowDeviceManagement.Types.InstanceSummary
import Amazonka.SnowDeviceManagement.Types.PhysicalNetworkInterface
import Amazonka.SnowDeviceManagement.Types.Reboot
import Amazonka.SnowDeviceManagement.Types.ResourceSummary
import Amazonka.SnowDeviceManagement.Types.SecurityGroupIdentifier
import Amazonka.SnowDeviceManagement.Types.SoftwareInformation
import Amazonka.SnowDeviceManagement.Types.TaskSummary
import Amazonka.SnowDeviceManagement.Types.Unlock
import Amazonka.SnowDeviceManagement.UntagResource
