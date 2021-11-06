{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DataSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-09@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- DataSync
--
-- DataSync is a managed data transfer service that makes it simpler for
-- you to automate moving data between on-premises storage and Amazon
-- Simple Storage Service (Amazon S3) or Amazon Elastic File System (Amazon
-- EFS).
--
-- This API interface reference for DataSync contains documentation for a
-- programming interface that you can use to manage DataSync.
module Amazonka.DataSync
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InternalException
    _InternalException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateTask
    UpdateTask (UpdateTask'),
    newUpdateTask,
    UpdateTaskResponse (UpdateTaskResponse'),
    newUpdateTaskResponse,

    -- ** DescribeAgent
    DescribeAgent (DescribeAgent'),
    newDescribeAgent,
    DescribeAgentResponse (DescribeAgentResponse'),
    newDescribeAgentResponse,

    -- ** DeleteTask
    DeleteTask (DeleteTask'),
    newDeleteTask,
    DeleteTaskResponse (DeleteTaskResponse'),
    newDeleteTaskResponse,

    -- ** DescribeLocationSmb
    DescribeLocationSmb (DescribeLocationSmb'),
    newDescribeLocationSmb,
    DescribeLocationSmbResponse (DescribeLocationSmbResponse'),
    newDescribeLocationSmbResponse,

    -- ** ListLocations (Paginated)
    ListLocations (ListLocations'),
    newListLocations,
    ListLocationsResponse (ListLocationsResponse'),
    newListLocationsResponse,

    -- ** CreateLocationNfs
    CreateLocationNfs (CreateLocationNfs'),
    newCreateLocationNfs,
    CreateLocationNfsResponse (CreateLocationNfsResponse'),
    newCreateLocationNfsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeLocationFsxWindows
    DescribeLocationFsxWindows (DescribeLocationFsxWindows'),
    newDescribeLocationFsxWindows,
    DescribeLocationFsxWindowsResponse (DescribeLocationFsxWindowsResponse'),
    newDescribeLocationFsxWindowsResponse,

    -- ** CreateLocationObjectStorage
    CreateLocationObjectStorage (CreateLocationObjectStorage'),
    newCreateLocationObjectStorage,
    CreateLocationObjectStorageResponse (CreateLocationObjectStorageResponse'),
    newCreateLocationObjectStorageResponse,

    -- ** DescribeTask
    DescribeTask (DescribeTask'),
    newDescribeTask,
    DescribeTaskResponse (DescribeTaskResponse'),
    newDescribeTaskResponse,

    -- ** DescribeLocationS3
    DescribeLocationS3 (DescribeLocationS3'),
    newDescribeLocationS3,
    DescribeLocationS3Response (DescribeLocationS3Response'),
    newDescribeLocationS3Response,

    -- ** ListAgents (Paginated)
    ListAgents (ListAgents'),
    newListAgents,
    ListAgentsResponse (ListAgentsResponse'),
    newListAgentsResponse,

    -- ** UpdateLocationSmb
    UpdateLocationSmb (UpdateLocationSmb'),
    newUpdateLocationSmb,
    UpdateLocationSmbResponse (UpdateLocationSmbResponse'),
    newUpdateLocationSmbResponse,

    -- ** DeleteAgent
    DeleteAgent (DeleteAgent'),
    newDeleteAgent,
    DeleteAgentResponse (DeleteAgentResponse'),
    newDeleteAgentResponse,

    -- ** UpdateAgent
    UpdateAgent (UpdateAgent'),
    newUpdateAgent,
    UpdateAgentResponse (UpdateAgentResponse'),
    newUpdateAgentResponse,

    -- ** CreateLocationFsxWindows
    CreateLocationFsxWindows (CreateLocationFsxWindows'),
    newCreateLocationFsxWindows,
    CreateLocationFsxWindowsResponse (CreateLocationFsxWindowsResponse'),
    newCreateLocationFsxWindowsResponse,

    -- ** ListTaskExecutions (Paginated)
    ListTaskExecutions (ListTaskExecutions'),
    newListTaskExecutions,
    ListTaskExecutionsResponse (ListTaskExecutionsResponse'),
    newListTaskExecutionsResponse,

    -- ** UpdateTaskExecution
    UpdateTaskExecution (UpdateTaskExecution'),
    newUpdateTaskExecution,
    UpdateTaskExecutionResponse (UpdateTaskExecutionResponse'),
    newUpdateTaskExecutionResponse,

    -- ** CreateLocationS3
    CreateLocationS3 (CreateLocationS3'),
    newCreateLocationS3,
    CreateLocationS3Response (CreateLocationS3Response'),
    newCreateLocationS3Response,

    -- ** CreateTask
    CreateTask (CreateTask'),
    newCreateTask,
    CreateTaskResponse (CreateTaskResponse'),
    newCreateTaskResponse,

    -- ** CreateLocationEfs
    CreateLocationEfs (CreateLocationEfs'),
    newCreateLocationEfs,
    CreateLocationEfsResponse (CreateLocationEfsResponse'),
    newCreateLocationEfsResponse,

    -- ** DescribeLocationObjectStorage
    DescribeLocationObjectStorage (DescribeLocationObjectStorage'),
    newDescribeLocationObjectStorage,
    DescribeLocationObjectStorageResponse (DescribeLocationObjectStorageResponse'),
    newDescribeLocationObjectStorageResponse,

    -- ** DeleteLocation
    DeleteLocation (DeleteLocation'),
    newDeleteLocation,
    DeleteLocationResponse (DeleteLocationResponse'),
    newDeleteLocationResponse,

    -- ** ListTasks (Paginated)
    ListTasks (ListTasks'),
    newListTasks,
    ListTasksResponse (ListTasksResponse'),
    newListTasksResponse,

    -- ** StartTaskExecution
    StartTaskExecution (StartTaskExecution'),
    newStartTaskExecution,
    StartTaskExecutionResponse (StartTaskExecutionResponse'),
    newStartTaskExecutionResponse,

    -- ** UpdateLocationNfs
    UpdateLocationNfs (UpdateLocationNfs'),
    newUpdateLocationNfs,
    UpdateLocationNfsResponse (UpdateLocationNfsResponse'),
    newUpdateLocationNfsResponse,

    -- ** DescribeTaskExecution
    DescribeTaskExecution (DescribeTaskExecution'),
    newDescribeTaskExecution,
    DescribeTaskExecutionResponse (DescribeTaskExecutionResponse'),
    newDescribeTaskExecutionResponse,

    -- ** CreateLocationSmb
    CreateLocationSmb (CreateLocationSmb'),
    newCreateLocationSmb,
    CreateLocationSmbResponse (CreateLocationSmbResponse'),
    newCreateLocationSmbResponse,

    -- ** CreateAgent
    CreateAgent (CreateAgent'),
    newCreateAgent,
    CreateAgentResponse (CreateAgentResponse'),
    newCreateAgentResponse,

    -- ** UpdateLocationObjectStorage
    UpdateLocationObjectStorage (UpdateLocationObjectStorage'),
    newUpdateLocationObjectStorage,
    UpdateLocationObjectStorageResponse (UpdateLocationObjectStorageResponse'),
    newUpdateLocationObjectStorageResponse,

    -- ** DescribeLocationEfs
    DescribeLocationEfs (DescribeLocationEfs'),
    newDescribeLocationEfs,
    DescribeLocationEfsResponse (DescribeLocationEfsResponse'),
    newDescribeLocationEfsResponse,

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

    -- ** DescribeLocationNfs
    DescribeLocationNfs (DescribeLocationNfs'),
    newDescribeLocationNfs,
    DescribeLocationNfsResponse (DescribeLocationNfsResponse'),
    newDescribeLocationNfsResponse,

    -- ** CancelTaskExecution
    CancelTaskExecution (CancelTaskExecution'),
    newCancelTaskExecution,
    CancelTaskExecutionResponse (CancelTaskExecutionResponse'),
    newCancelTaskExecutionResponse,

    -- * Types

    -- ** AgentStatus
    AgentStatus (..),

    -- ** Atime
    Atime (..),

    -- ** EndpointType
    EndpointType (..),

    -- ** FilterType
    FilterType (..),

    -- ** Gid
    Gid (..),

    -- ** LocationFilterName
    LocationFilterName (..),

    -- ** LogLevel
    LogLevel (..),

    -- ** Mtime
    Mtime (..),

    -- ** NfsVersion
    NfsVersion (..),

    -- ** ObjectStorageServerProtocol
    ObjectStorageServerProtocol (..),

    -- ** Operator
    Operator (..),

    -- ** OverwriteMode
    OverwriteMode (..),

    -- ** PhaseStatus
    PhaseStatus (..),

    -- ** PosixPermissions
    PosixPermissions (..),

    -- ** PreserveDeletedFiles
    PreserveDeletedFiles (..),

    -- ** PreserveDevices
    PreserveDevices (..),

    -- ** S3StorageClass
    S3StorageClass (..),

    -- ** SmbSecurityDescriptorCopyFlags
    SmbSecurityDescriptorCopyFlags (..),

    -- ** SmbVersion
    SmbVersion (..),

    -- ** TaskExecutionStatus
    TaskExecutionStatus (..),

    -- ** TaskFilterName
    TaskFilterName (..),

    -- ** TaskQueueing
    TaskQueueing (..),

    -- ** TaskStatus
    TaskStatus (..),

    -- ** TransferMode
    TransferMode (..),

    -- ** Uid
    Uid (..),

    -- ** VerifyMode
    VerifyMode (..),

    -- ** AgentListEntry
    AgentListEntry (AgentListEntry'),
    newAgentListEntry,

    -- ** Ec2Config
    Ec2Config (Ec2Config'),
    newEc2Config,

    -- ** FilterRule
    FilterRule (FilterRule'),
    newFilterRule,

    -- ** LocationFilter
    LocationFilter (LocationFilter'),
    newLocationFilter,

    -- ** LocationListEntry
    LocationListEntry (LocationListEntry'),
    newLocationListEntry,

    -- ** NfsMountOptions
    NfsMountOptions (NfsMountOptions'),
    newNfsMountOptions,

    -- ** OnPremConfig
    OnPremConfig (OnPremConfig'),
    newOnPremConfig,

    -- ** Options
    Options (Options'),
    newOptions,

    -- ** PrivateLinkConfig
    PrivateLinkConfig (PrivateLinkConfig'),
    newPrivateLinkConfig,

    -- ** S3Config
    S3Config (S3Config'),
    newS3Config,

    -- ** SmbMountOptions
    SmbMountOptions (SmbMountOptions'),
    newSmbMountOptions,

    -- ** TagListEntry
    TagListEntry (TagListEntry'),
    newTagListEntry,

    -- ** TaskExecutionListEntry
    TaskExecutionListEntry (TaskExecutionListEntry'),
    newTaskExecutionListEntry,

    -- ** TaskExecutionResultDetail
    TaskExecutionResultDetail (TaskExecutionResultDetail'),
    newTaskExecutionResultDetail,

    -- ** TaskFilter
    TaskFilter (TaskFilter'),
    newTaskFilter,

    -- ** TaskListEntry
    TaskListEntry (TaskListEntry'),
    newTaskListEntry,

    -- ** TaskSchedule
    TaskSchedule (TaskSchedule'),
    newTaskSchedule,
  )
where

import Amazonka.DataSync.CancelTaskExecution
import Amazonka.DataSync.CreateAgent
import Amazonka.DataSync.CreateLocationEfs
import Amazonka.DataSync.CreateLocationFsxWindows
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
import Amazonka.DataSync.DescribeLocationFsxWindows
import Amazonka.DataSync.DescribeLocationNfs
import Amazonka.DataSync.DescribeLocationObjectStorage
import Amazonka.DataSync.DescribeLocationS3
import Amazonka.DataSync.DescribeLocationSmb
import Amazonka.DataSync.DescribeTask
import Amazonka.DataSync.DescribeTaskExecution
import Amazonka.DataSync.Lens
import Amazonka.DataSync.ListAgents
import Amazonka.DataSync.ListLocations
import Amazonka.DataSync.ListTagsForResource
import Amazonka.DataSync.ListTaskExecutions
import Amazonka.DataSync.ListTasks
import Amazonka.DataSync.StartTaskExecution
import Amazonka.DataSync.TagResource
import Amazonka.DataSync.Types
import Amazonka.DataSync.UntagResource
import Amazonka.DataSync.UpdateAgent
import Amazonka.DataSync.UpdateLocationNfs
import Amazonka.DataSync.UpdateLocationObjectStorage
import Amazonka.DataSync.UpdateLocationSmb
import Amazonka.DataSync.UpdateTask
import Amazonka.DataSync.UpdateTaskExecution
import Amazonka.DataSync.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DataSync'.

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
