{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DataSync
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- you to automate moving data between on-premises storage and Amazon Web
-- Services storage services. You also can use DataSync to transfer data
-- between other cloud providers and Amazon Web Services storage services.
--
-- This API interface reference includes documentation for using DataSync
-- programmatically. For complete information, see the
-- /<https://docs.aws.amazon.com/datasync/latest/userguide/what-is-datasync.html DataSync User Guide>/
-- .
module Amazonka.DataSync
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalException
    _InternalException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddStorageSystem
    AddStorageSystem (AddStorageSystem'),
    newAddStorageSystem,
    AddStorageSystemResponse (AddStorageSystemResponse'),
    newAddStorageSystemResponse,

    -- ** CancelTaskExecution
    CancelTaskExecution (CancelTaskExecution'),
    newCancelTaskExecution,
    CancelTaskExecutionResponse (CancelTaskExecutionResponse'),
    newCancelTaskExecutionResponse,

    -- ** CreateAgent
    CreateAgent (CreateAgent'),
    newCreateAgent,
    CreateAgentResponse (CreateAgentResponse'),
    newCreateAgentResponse,

    -- ** CreateLocationEfs
    CreateLocationEfs (CreateLocationEfs'),
    newCreateLocationEfs,
    CreateLocationEfsResponse (CreateLocationEfsResponse'),
    newCreateLocationEfsResponse,

    -- ** CreateLocationFsxLustre
    CreateLocationFsxLustre (CreateLocationFsxLustre'),
    newCreateLocationFsxLustre,
    CreateLocationFsxLustreResponse (CreateLocationFsxLustreResponse'),
    newCreateLocationFsxLustreResponse,

    -- ** CreateLocationFsxOntap
    CreateLocationFsxOntap (CreateLocationFsxOntap'),
    newCreateLocationFsxOntap,
    CreateLocationFsxOntapResponse (CreateLocationFsxOntapResponse'),
    newCreateLocationFsxOntapResponse,

    -- ** CreateLocationFsxOpenZfs
    CreateLocationFsxOpenZfs (CreateLocationFsxOpenZfs'),
    newCreateLocationFsxOpenZfs,
    CreateLocationFsxOpenZfsResponse (CreateLocationFsxOpenZfsResponse'),
    newCreateLocationFsxOpenZfsResponse,

    -- ** CreateLocationFsxWindows
    CreateLocationFsxWindows (CreateLocationFsxWindows'),
    newCreateLocationFsxWindows,
    CreateLocationFsxWindowsResponse (CreateLocationFsxWindowsResponse'),
    newCreateLocationFsxWindowsResponse,

    -- ** CreateLocationHdfs
    CreateLocationHdfs (CreateLocationHdfs'),
    newCreateLocationHdfs,
    CreateLocationHdfsResponse (CreateLocationHdfsResponse'),
    newCreateLocationHdfsResponse,

    -- ** CreateLocationNfs
    CreateLocationNfs (CreateLocationNfs'),
    newCreateLocationNfs,
    CreateLocationNfsResponse (CreateLocationNfsResponse'),
    newCreateLocationNfsResponse,

    -- ** CreateLocationObjectStorage
    CreateLocationObjectStorage (CreateLocationObjectStorage'),
    newCreateLocationObjectStorage,
    CreateLocationObjectStorageResponse (CreateLocationObjectStorageResponse'),
    newCreateLocationObjectStorageResponse,

    -- ** CreateLocationS3
    CreateLocationS3 (CreateLocationS3'),
    newCreateLocationS3,
    CreateLocationS3Response (CreateLocationS3Response'),
    newCreateLocationS3Response,

    -- ** CreateLocationSmb
    CreateLocationSmb (CreateLocationSmb'),
    newCreateLocationSmb,
    CreateLocationSmbResponse (CreateLocationSmbResponse'),
    newCreateLocationSmbResponse,

    -- ** CreateTask
    CreateTask (CreateTask'),
    newCreateTask,
    CreateTaskResponse (CreateTaskResponse'),
    newCreateTaskResponse,

    -- ** DeleteAgent
    DeleteAgent (DeleteAgent'),
    newDeleteAgent,
    DeleteAgentResponse (DeleteAgentResponse'),
    newDeleteAgentResponse,

    -- ** DeleteLocation
    DeleteLocation (DeleteLocation'),
    newDeleteLocation,
    DeleteLocationResponse (DeleteLocationResponse'),
    newDeleteLocationResponse,

    -- ** DeleteTask
    DeleteTask (DeleteTask'),
    newDeleteTask,
    DeleteTaskResponse (DeleteTaskResponse'),
    newDeleteTaskResponse,

    -- ** DescribeAgent
    DescribeAgent (DescribeAgent'),
    newDescribeAgent,
    DescribeAgentResponse (DescribeAgentResponse'),
    newDescribeAgentResponse,

    -- ** DescribeDiscoveryJob
    DescribeDiscoveryJob (DescribeDiscoveryJob'),
    newDescribeDiscoveryJob,
    DescribeDiscoveryJobResponse (DescribeDiscoveryJobResponse'),
    newDescribeDiscoveryJobResponse,

    -- ** DescribeLocationEfs
    DescribeLocationEfs (DescribeLocationEfs'),
    newDescribeLocationEfs,
    DescribeLocationEfsResponse (DescribeLocationEfsResponse'),
    newDescribeLocationEfsResponse,

    -- ** DescribeLocationFsxLustre
    DescribeLocationFsxLustre (DescribeLocationFsxLustre'),
    newDescribeLocationFsxLustre,
    DescribeLocationFsxLustreResponse (DescribeLocationFsxLustreResponse'),
    newDescribeLocationFsxLustreResponse,

    -- ** DescribeLocationFsxOntap
    DescribeLocationFsxOntap (DescribeLocationFsxOntap'),
    newDescribeLocationFsxOntap,
    DescribeLocationFsxOntapResponse (DescribeLocationFsxOntapResponse'),
    newDescribeLocationFsxOntapResponse,

    -- ** DescribeLocationFsxOpenZfs
    DescribeLocationFsxOpenZfs (DescribeLocationFsxOpenZfs'),
    newDescribeLocationFsxOpenZfs,
    DescribeLocationFsxOpenZfsResponse (DescribeLocationFsxOpenZfsResponse'),
    newDescribeLocationFsxOpenZfsResponse,

    -- ** DescribeLocationFsxWindows
    DescribeLocationFsxWindows (DescribeLocationFsxWindows'),
    newDescribeLocationFsxWindows,
    DescribeLocationFsxWindowsResponse (DescribeLocationFsxWindowsResponse'),
    newDescribeLocationFsxWindowsResponse,

    -- ** DescribeLocationHdfs
    DescribeLocationHdfs (DescribeLocationHdfs'),
    newDescribeLocationHdfs,
    DescribeLocationHdfsResponse (DescribeLocationHdfsResponse'),
    newDescribeLocationHdfsResponse,

    -- ** DescribeLocationNfs
    DescribeLocationNfs (DescribeLocationNfs'),
    newDescribeLocationNfs,
    DescribeLocationNfsResponse (DescribeLocationNfsResponse'),
    newDescribeLocationNfsResponse,

    -- ** DescribeLocationObjectStorage
    DescribeLocationObjectStorage (DescribeLocationObjectStorage'),
    newDescribeLocationObjectStorage,
    DescribeLocationObjectStorageResponse (DescribeLocationObjectStorageResponse'),
    newDescribeLocationObjectStorageResponse,

    -- ** DescribeLocationS3
    DescribeLocationS3 (DescribeLocationS3'),
    newDescribeLocationS3,
    DescribeLocationS3Response (DescribeLocationS3Response'),
    newDescribeLocationS3Response,

    -- ** DescribeLocationSmb
    DescribeLocationSmb (DescribeLocationSmb'),
    newDescribeLocationSmb,
    DescribeLocationSmbResponse (DescribeLocationSmbResponse'),
    newDescribeLocationSmbResponse,

    -- ** DescribeStorageSystem
    DescribeStorageSystem (DescribeStorageSystem'),
    newDescribeStorageSystem,
    DescribeStorageSystemResponse (DescribeStorageSystemResponse'),
    newDescribeStorageSystemResponse,

    -- ** DescribeStorageSystemResourceMetrics (Paginated)
    DescribeStorageSystemResourceMetrics (DescribeStorageSystemResourceMetrics'),
    newDescribeStorageSystemResourceMetrics,
    DescribeStorageSystemResourceMetricsResponse (DescribeStorageSystemResourceMetricsResponse'),
    newDescribeStorageSystemResourceMetricsResponse,

    -- ** DescribeStorageSystemResources
    DescribeStorageSystemResources (DescribeStorageSystemResources'),
    newDescribeStorageSystemResources,
    DescribeStorageSystemResourcesResponse (DescribeStorageSystemResourcesResponse'),
    newDescribeStorageSystemResourcesResponse,

    -- ** DescribeTask
    DescribeTask (DescribeTask'),
    newDescribeTask,
    DescribeTaskResponse (DescribeTaskResponse'),
    newDescribeTaskResponse,

    -- ** DescribeTaskExecution
    DescribeTaskExecution (DescribeTaskExecution'),
    newDescribeTaskExecution,
    DescribeTaskExecutionResponse (DescribeTaskExecutionResponse'),
    newDescribeTaskExecutionResponse,

    -- ** GenerateRecommendations
    GenerateRecommendations (GenerateRecommendations'),
    newGenerateRecommendations,
    GenerateRecommendationsResponse (GenerateRecommendationsResponse'),
    newGenerateRecommendationsResponse,

    -- ** ListAgents (Paginated)
    ListAgents (ListAgents'),
    newListAgents,
    ListAgentsResponse (ListAgentsResponse'),
    newListAgentsResponse,

    -- ** ListDiscoveryJobs (Paginated)
    ListDiscoveryJobs (ListDiscoveryJobs'),
    newListDiscoveryJobs,
    ListDiscoveryJobsResponse (ListDiscoveryJobsResponse'),
    newListDiscoveryJobsResponse,

    -- ** ListLocations (Paginated)
    ListLocations (ListLocations'),
    newListLocations,
    ListLocationsResponse (ListLocationsResponse'),
    newListLocationsResponse,

    -- ** ListStorageSystems (Paginated)
    ListStorageSystems (ListStorageSystems'),
    newListStorageSystems,
    ListStorageSystemsResponse (ListStorageSystemsResponse'),
    newListStorageSystemsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTaskExecutions (Paginated)
    ListTaskExecutions (ListTaskExecutions'),
    newListTaskExecutions,
    ListTaskExecutionsResponse (ListTaskExecutionsResponse'),
    newListTaskExecutionsResponse,

    -- ** ListTasks (Paginated)
    ListTasks (ListTasks'),
    newListTasks,
    ListTasksResponse (ListTasksResponse'),
    newListTasksResponse,

    -- ** RemoveStorageSystem
    RemoveStorageSystem (RemoveStorageSystem'),
    newRemoveStorageSystem,
    RemoveStorageSystemResponse (RemoveStorageSystemResponse'),
    newRemoveStorageSystemResponse,

    -- ** StartDiscoveryJob
    StartDiscoveryJob (StartDiscoveryJob'),
    newStartDiscoveryJob,
    StartDiscoveryJobResponse (StartDiscoveryJobResponse'),
    newStartDiscoveryJobResponse,

    -- ** StartTaskExecution
    StartTaskExecution (StartTaskExecution'),
    newStartTaskExecution,
    StartTaskExecutionResponse (StartTaskExecutionResponse'),
    newStartTaskExecutionResponse,

    -- ** StopDiscoveryJob
    StopDiscoveryJob (StopDiscoveryJob'),
    newStopDiscoveryJob,
    StopDiscoveryJobResponse (StopDiscoveryJobResponse'),
    newStopDiscoveryJobResponse,

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

    -- ** UpdateAgent
    UpdateAgent (UpdateAgent'),
    newUpdateAgent,
    UpdateAgentResponse (UpdateAgentResponse'),
    newUpdateAgentResponse,

    -- ** UpdateDiscoveryJob
    UpdateDiscoveryJob (UpdateDiscoveryJob'),
    newUpdateDiscoveryJob,
    UpdateDiscoveryJobResponse (UpdateDiscoveryJobResponse'),
    newUpdateDiscoveryJobResponse,

    -- ** UpdateLocationHdfs
    UpdateLocationHdfs (UpdateLocationHdfs'),
    newUpdateLocationHdfs,
    UpdateLocationHdfsResponse (UpdateLocationHdfsResponse'),
    newUpdateLocationHdfsResponse,

    -- ** UpdateLocationNfs
    UpdateLocationNfs (UpdateLocationNfs'),
    newUpdateLocationNfs,
    UpdateLocationNfsResponse (UpdateLocationNfsResponse'),
    newUpdateLocationNfsResponse,

    -- ** UpdateLocationObjectStorage
    UpdateLocationObjectStorage (UpdateLocationObjectStorage'),
    newUpdateLocationObjectStorage,
    UpdateLocationObjectStorageResponse (UpdateLocationObjectStorageResponse'),
    newUpdateLocationObjectStorageResponse,

    -- ** UpdateLocationSmb
    UpdateLocationSmb (UpdateLocationSmb'),
    newUpdateLocationSmb,
    UpdateLocationSmbResponse (UpdateLocationSmbResponse'),
    newUpdateLocationSmbResponse,

    -- ** UpdateStorageSystem
    UpdateStorageSystem (UpdateStorageSystem'),
    newUpdateStorageSystem,
    UpdateStorageSystemResponse (UpdateStorageSystemResponse'),
    newUpdateStorageSystemResponse,

    -- ** UpdateTask
    UpdateTask (UpdateTask'),
    newUpdateTask,
    UpdateTaskResponse (UpdateTaskResponse'),
    newUpdateTaskResponse,

    -- ** UpdateTaskExecution
    UpdateTaskExecution (UpdateTaskExecution'),
    newUpdateTaskExecution,
    UpdateTaskExecutionResponse (UpdateTaskExecutionResponse'),
    newUpdateTaskExecutionResponse,

    -- * Types

    -- ** AgentStatus
    AgentStatus (..),

    -- ** Atime
    Atime (..),

    -- ** DiscoveryJobStatus
    DiscoveryJobStatus (..),

    -- ** DiscoveryResourceFilter
    DiscoveryResourceFilter (..),

    -- ** DiscoveryResourceType
    DiscoveryResourceType (..),

    -- ** DiscoverySystemType
    DiscoverySystemType (..),

    -- ** EfsInTransitEncryption
    EfsInTransitEncryption (..),

    -- ** EndpointType
    EndpointType (..),

    -- ** FilterType
    FilterType (..),

    -- ** Gid
    Gid (..),

    -- ** HdfsAuthenticationType
    HdfsAuthenticationType (..),

    -- ** HdfsDataTransferProtection
    HdfsDataTransferProtection (..),

    -- ** HdfsRpcProtection
    HdfsRpcProtection (..),

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

    -- ** ObjectTags
    ObjectTags (..),

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

    -- ** RecommendationStatus
    RecommendationStatus (..),

    -- ** S3StorageClass
    S3StorageClass (..),

    -- ** SmbSecurityDescriptorCopyFlags
    SmbSecurityDescriptorCopyFlags (..),

    -- ** SmbVersion
    SmbVersion (..),

    -- ** StorageSystemConnectivityStatus
    StorageSystemConnectivityStatus (..),

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

    -- ** Capacity
    Capacity (Capacity'),
    newCapacity,

    -- ** Credentials
    Credentials (Credentials'),
    newCredentials,

    -- ** DiscoveryJobListEntry
    DiscoveryJobListEntry (DiscoveryJobListEntry'),
    newDiscoveryJobListEntry,

    -- ** DiscoveryServerConfiguration
    DiscoveryServerConfiguration (DiscoveryServerConfiguration'),
    newDiscoveryServerConfiguration,

    -- ** Ec2Config
    Ec2Config (Ec2Config'),
    newEc2Config,

    -- ** FilterRule
    FilterRule (FilterRule'),
    newFilterRule,

    -- ** FsxProtocol
    FsxProtocol (FsxProtocol'),
    newFsxProtocol,

    -- ** FsxProtocolNfs
    FsxProtocolNfs (FsxProtocolNfs'),
    newFsxProtocolNfs,

    -- ** FsxProtocolSmb
    FsxProtocolSmb (FsxProtocolSmb'),
    newFsxProtocolSmb,

    -- ** HdfsNameNode
    HdfsNameNode (HdfsNameNode'),
    newHdfsNameNode,

    -- ** IOPS
    IOPS (IOPS'),
    newIOPS,

    -- ** Latency
    Latency (Latency'),
    newLatency,

    -- ** LocationFilter
    LocationFilter (LocationFilter'),
    newLocationFilter,

    -- ** LocationListEntry
    LocationListEntry (LocationListEntry'),
    newLocationListEntry,

    -- ** MaxP95Performance
    MaxP95Performance (MaxP95Performance'),
    newMaxP95Performance,

    -- ** NetAppONTAPCluster
    NetAppONTAPCluster (NetAppONTAPCluster'),
    newNetAppONTAPCluster,

    -- ** NetAppONTAPSVM
    NetAppONTAPSVM (NetAppONTAPSVM'),
    newNetAppONTAPSVM,

    -- ** NetAppONTAPVolume
    NetAppONTAPVolume (NetAppONTAPVolume'),
    newNetAppONTAPVolume,

    -- ** NfsMountOptions
    NfsMountOptions (NfsMountOptions'),
    newNfsMountOptions,

    -- ** OnPremConfig
    OnPremConfig (OnPremConfig'),
    newOnPremConfig,

    -- ** Options
    Options (Options'),
    newOptions,

    -- ** P95Metrics
    P95Metrics (P95Metrics'),
    newP95Metrics,

    -- ** PrivateLinkConfig
    PrivateLinkConfig (PrivateLinkConfig'),
    newPrivateLinkConfig,

    -- ** QopConfiguration
    QopConfiguration (QopConfiguration'),
    newQopConfiguration,

    -- ** Recommendation
    Recommendation (Recommendation'),
    newRecommendation,

    -- ** ResourceDetails
    ResourceDetails (ResourceDetails'),
    newResourceDetails,

    -- ** ResourceMetrics
    ResourceMetrics (ResourceMetrics'),
    newResourceMetrics,

    -- ** S3Config
    S3Config (S3Config'),
    newS3Config,

    -- ** SmbMountOptions
    SmbMountOptions (SmbMountOptions'),
    newSmbMountOptions,

    -- ** StorageSystemListEntry
    StorageSystemListEntry (StorageSystemListEntry'),
    newStorageSystemListEntry,

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

    -- ** Throughput
    Throughput (Throughput'),
    newThroughput,
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
import Amazonka.DataSync.Lens
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
import Amazonka.DataSync.Types
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
