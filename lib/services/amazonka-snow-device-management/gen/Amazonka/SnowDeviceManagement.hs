{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SnowDeviceManagement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-08-04@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Snow Device Management documentation.
module Amazonka.SnowDeviceManagement
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelTask
    CancelTask (CancelTask'),
    newCancelTask,
    CancelTaskResponse (CancelTaskResponse'),
    newCancelTaskResponse,

    -- ** CreateTask
    CreateTask (CreateTask'),
    newCreateTask,
    CreateTaskResponse (CreateTaskResponse'),
    newCreateTaskResponse,

    -- ** DescribeDevice
    DescribeDevice (DescribeDevice'),
    newDescribeDevice,
    DescribeDeviceResponse (DescribeDeviceResponse'),
    newDescribeDeviceResponse,

    -- ** DescribeDeviceEc2Instances
    DescribeDeviceEc2Instances (DescribeDeviceEc2Instances'),
    newDescribeDeviceEc2Instances,
    DescribeDeviceEc2InstancesResponse (DescribeDeviceEc2InstancesResponse'),
    newDescribeDeviceEc2InstancesResponse,

    -- ** DescribeExecution
    DescribeExecution (DescribeExecution'),
    newDescribeExecution,
    DescribeExecutionResponse (DescribeExecutionResponse'),
    newDescribeExecutionResponse,

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

    -- ** ListDevices (Paginated)
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- ** ListExecutions (Paginated)
    ListExecutions (ListExecutions'),
    newListExecutions,
    ListExecutionsResponse (ListExecutionsResponse'),
    newListExecutionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTasks (Paginated)
    ListTasks (ListTasks'),
    newListTasks,
    ListTasksResponse (ListTasksResponse'),
    newListTasksResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

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

import Amazonka.SnowDeviceManagement.CancelTask
import Amazonka.SnowDeviceManagement.CreateTask
import Amazonka.SnowDeviceManagement.DescribeDevice
import Amazonka.SnowDeviceManagement.DescribeDeviceEc2Instances
import Amazonka.SnowDeviceManagement.DescribeExecution
import Amazonka.SnowDeviceManagement.DescribeTask
import Amazonka.SnowDeviceManagement.Lens
import Amazonka.SnowDeviceManagement.ListDeviceResources
import Amazonka.SnowDeviceManagement.ListDevices
import Amazonka.SnowDeviceManagement.ListExecutions
import Amazonka.SnowDeviceManagement.ListTagsForResource
import Amazonka.SnowDeviceManagement.ListTasks
import Amazonka.SnowDeviceManagement.TagResource
import Amazonka.SnowDeviceManagement.Types
import Amazonka.SnowDeviceManagement.UntagResource
import Amazonka.SnowDeviceManagement.Waiters

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
