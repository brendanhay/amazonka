{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.DataSync
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
module Network.AWS.DataSync
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
import Network.AWS.DataSync.Lens
import Network.AWS.DataSync.ListAgents
import Network.AWS.DataSync.ListLocations
import Network.AWS.DataSync.ListTagsForResource
import Network.AWS.DataSync.ListTaskExecutions
import Network.AWS.DataSync.ListTasks
import Network.AWS.DataSync.StartTaskExecution
import Network.AWS.DataSync.TagResource
import Network.AWS.DataSync.Types
import Network.AWS.DataSync.UntagResource
import Network.AWS.DataSync.UpdateAgent
import Network.AWS.DataSync.UpdateLocationNfs
import Network.AWS.DataSync.UpdateLocationObjectStorage
import Network.AWS.DataSync.UpdateLocationSmb
import Network.AWS.DataSync.UpdateTask
import Network.AWS.DataSync.UpdateTaskExecution
import Network.AWS.DataSync.Waiters

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
