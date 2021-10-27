{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.SnowDeviceManagement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-08-04@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Snow Device Management documentation.
module Network.AWS.SnowDeviceManagement
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeTask
    DescribeTask (DescribeTask'),
    newDescribeTask,
    DescribeTaskResponse (DescribeTaskResponse'),
    newDescribeTaskResponse,

    -- ** ListDeviceResources (Paginated)
    ListDeviceResources (ListDeviceResources'),
    newListDeviceResources,
    ListDeviceResourcesResponse (ListDeviceResourcesResponse'),
    newListDeviceResourcesResponse,

    -- ** ListExecutions (Paginated)
    ListExecutions (ListExecutions'),
    newListExecutions,
    ListExecutionsResponse (ListExecutionsResponse'),
    newListExecutionsResponse,

    -- ** DescribeDeviceEc2Instances
    DescribeDeviceEc2Instances (DescribeDeviceEc2Instances'),
    newDescribeDeviceEc2Instances,
    DescribeDeviceEc2InstancesResponse (DescribeDeviceEc2InstancesResponse'),
    newDescribeDeviceEc2InstancesResponse,

    -- ** CreateTask
    CreateTask (CreateTask'),
    newCreateTask,
    CreateTaskResponse (CreateTaskResponse'),
    newCreateTaskResponse,

    -- ** ListTasks (Paginated)
    ListTasks (ListTasks'),
    newListTasks,
    ListTasksResponse (ListTasksResponse'),
    newListTasksResponse,

    -- ** DescribeExecution
    DescribeExecution (DescribeExecution'),
    newDescribeExecution,
    DescribeExecutionResponse (DescribeExecutionResponse'),
    newDescribeExecutionResponse,

    -- ** DescribeDevice
    DescribeDevice (DescribeDevice'),
    newDescribeDevice,
    DescribeDeviceResponse (DescribeDeviceResponse'),
    newDescribeDeviceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CancelTask
    CancelTask (CancelTask'),
    newCancelTask,
    CancelTaskResponse (CancelTaskResponse'),
    newCancelTaskResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListDevices (Paginated)
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- * Types

    -- ** AttachmentStatus
    AttachmentStatus (..),

    -- ** ExecutionState
    ExecutionState (..),

    -- ** InstanceStateName
    InstanceStateName (..),

    -- ** IpAddressAssignment
    IpAddressAssignment (..),

    -- ** PhysicalConnectorType
    PhysicalConnectorType (..),

    -- ** TaskState
    TaskState (..),

    -- ** UnlockState
    UnlockState (..),

    -- ** Capacity
    Capacity (Capacity'),
    newCapacity,

    -- ** Command
    Command (Command'),
    newCommand,

    -- ** CpuOptions
    CpuOptions (CpuOptions'),
    newCpuOptions,

    -- ** DeviceSummary
    DeviceSummary (DeviceSummary'),
    newDeviceSummary,

    -- ** EbsInstanceBlockDevice
    EbsInstanceBlockDevice (EbsInstanceBlockDevice'),
    newEbsInstanceBlockDevice,

    -- ** ExecutionSummary
    ExecutionSummary (ExecutionSummary'),
    newExecutionSummary,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (InstanceBlockDeviceMapping'),
    newInstanceBlockDeviceMapping,

    -- ** InstanceState
    InstanceState (InstanceState'),
    newInstanceState,

    -- ** InstanceSummary
    InstanceSummary (InstanceSummary'),
    newInstanceSummary,

    -- ** PhysicalNetworkInterface
    PhysicalNetworkInterface (PhysicalNetworkInterface'),
    newPhysicalNetworkInterface,

    -- ** Reboot
    Reboot (Reboot'),
    newReboot,

    -- ** ResourceSummary
    ResourceSummary (ResourceSummary'),
    newResourceSummary,

    -- ** SecurityGroupIdentifier
    SecurityGroupIdentifier (SecurityGroupIdentifier'),
    newSecurityGroupIdentifier,

    -- ** SoftwareInformation
    SoftwareInformation (SoftwareInformation'),
    newSoftwareInformation,

    -- ** TaskSummary
    TaskSummary (TaskSummary'),
    newTaskSummary,

    -- ** Unlock
    Unlock (Unlock'),
    newUnlock,
  )
where

import Network.AWS.SnowDeviceManagement.CancelTask
import Network.AWS.SnowDeviceManagement.CreateTask
import Network.AWS.SnowDeviceManagement.DescribeDevice
import Network.AWS.SnowDeviceManagement.DescribeDeviceEc2Instances
import Network.AWS.SnowDeviceManagement.DescribeExecution
import Network.AWS.SnowDeviceManagement.DescribeTask
import Network.AWS.SnowDeviceManagement.Lens
import Network.AWS.SnowDeviceManagement.ListDeviceResources
import Network.AWS.SnowDeviceManagement.ListDevices
import Network.AWS.SnowDeviceManagement.ListExecutions
import Network.AWS.SnowDeviceManagement.ListTagsForResource
import Network.AWS.SnowDeviceManagement.ListTasks
import Network.AWS.SnowDeviceManagement.TagResource
import Network.AWS.SnowDeviceManagement.Types
import Network.AWS.SnowDeviceManagement.UntagResource
import Network.AWS.SnowDeviceManagement.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SnowDeviceManagement'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
