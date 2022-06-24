{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MGN
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-02-26@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Application Migration Service service.
module Amazonka.MGN
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** UninitializedAccountException
    _UninitializedAccountException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ChangeServerLifeCycleState
    ChangeServerLifeCycleState (ChangeServerLifeCycleState'),
    newChangeServerLifeCycleState,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** CreateReplicationConfigurationTemplate
    CreateReplicationConfigurationTemplate (CreateReplicationConfigurationTemplate'),
    newCreateReplicationConfigurationTemplate,
    ReplicationConfigurationTemplate (ReplicationConfigurationTemplate'),
    newReplicationConfigurationTemplate,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** DeleteReplicationConfigurationTemplate
    DeleteReplicationConfigurationTemplate (DeleteReplicationConfigurationTemplate'),
    newDeleteReplicationConfigurationTemplate,
    DeleteReplicationConfigurationTemplateResponse (DeleteReplicationConfigurationTemplateResponse'),
    newDeleteReplicationConfigurationTemplateResponse,

    -- ** DeleteSourceServer
    DeleteSourceServer (DeleteSourceServer'),
    newDeleteSourceServer,
    DeleteSourceServerResponse (DeleteSourceServerResponse'),
    newDeleteSourceServerResponse,

    -- ** DescribeJobLogItems (Paginated)
    DescribeJobLogItems (DescribeJobLogItems'),
    newDescribeJobLogItems,
    DescribeJobLogItemsResponse (DescribeJobLogItemsResponse'),
    newDescribeJobLogItemsResponse,

    -- ** DescribeJobs (Paginated)
    DescribeJobs (DescribeJobs'),
    newDescribeJobs,
    DescribeJobsResponse (DescribeJobsResponse'),
    newDescribeJobsResponse,

    -- ** DescribeReplicationConfigurationTemplates (Paginated)
    DescribeReplicationConfigurationTemplates (DescribeReplicationConfigurationTemplates'),
    newDescribeReplicationConfigurationTemplates,
    DescribeReplicationConfigurationTemplatesResponse (DescribeReplicationConfigurationTemplatesResponse'),
    newDescribeReplicationConfigurationTemplatesResponse,

    -- ** DescribeSourceServers (Paginated)
    DescribeSourceServers (DescribeSourceServers'),
    newDescribeSourceServers,
    DescribeSourceServersResponse (DescribeSourceServersResponse'),
    newDescribeSourceServersResponse,

    -- ** DisconnectFromService
    DisconnectFromService (DisconnectFromService'),
    newDisconnectFromService,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** FinalizeCutover
    FinalizeCutover (FinalizeCutover'),
    newFinalizeCutover,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** GetLaunchConfiguration
    GetLaunchConfiguration (GetLaunchConfiguration'),
    newGetLaunchConfiguration,
    LaunchConfiguration (LaunchConfiguration'),
    newLaunchConfiguration,

    -- ** GetReplicationConfiguration
    GetReplicationConfiguration (GetReplicationConfiguration'),
    newGetReplicationConfiguration,
    ReplicationConfiguration (ReplicationConfiguration'),
    newReplicationConfiguration,

    -- ** InitializeService
    InitializeService (InitializeService'),
    newInitializeService,
    InitializeServiceResponse (InitializeServiceResponse'),
    newInitializeServiceResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** MarkAsArchived
    MarkAsArchived (MarkAsArchived'),
    newMarkAsArchived,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** RetryDataReplication
    RetryDataReplication (RetryDataReplication'),
    newRetryDataReplication,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** StartCutover
    StartCutover (StartCutover'),
    newStartCutover,
    StartCutoverResponse (StartCutoverResponse'),
    newStartCutoverResponse,

    -- ** StartTest
    StartTest (StartTest'),
    newStartTest,
    StartTestResponse (StartTestResponse'),
    newStartTestResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TerminateTargetInstances
    TerminateTargetInstances (TerminateTargetInstances'),
    newTerminateTargetInstances,
    TerminateTargetInstancesResponse (TerminateTargetInstancesResponse'),
    newTerminateTargetInstancesResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateLaunchConfiguration
    UpdateLaunchConfiguration (UpdateLaunchConfiguration'),
    newUpdateLaunchConfiguration,
    LaunchConfiguration (LaunchConfiguration'),
    newLaunchConfiguration,

    -- ** UpdateReplicationConfiguration
    UpdateReplicationConfiguration (UpdateReplicationConfiguration'),
    newUpdateReplicationConfiguration,
    ReplicationConfiguration (ReplicationConfiguration'),
    newReplicationConfiguration,

    -- ** UpdateReplicationConfigurationTemplate
    UpdateReplicationConfigurationTemplate (UpdateReplicationConfigurationTemplate'),
    newUpdateReplicationConfigurationTemplate,
    ReplicationConfigurationTemplate (ReplicationConfigurationTemplate'),
    newReplicationConfigurationTemplate,

    -- * Types

    -- ** ChangeServerLifeCycleStateSourceServerLifecycleState
    ChangeServerLifeCycleStateSourceServerLifecycleState (..),

    -- ** DataReplicationErrorString
    DataReplicationErrorString (..),

    -- ** DataReplicationInitiationStepName
    DataReplicationInitiationStepName (..),

    -- ** DataReplicationInitiationStepStatus
    DataReplicationInitiationStepStatus (..),

    -- ** DataReplicationState
    DataReplicationState (..),

    -- ** FirstBoot
    FirstBoot (..),

    -- ** InitiatedBy
    InitiatedBy (..),

    -- ** JobLogEvent
    JobLogEvent (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** JobType
    JobType (..),

    -- ** LaunchDisposition
    LaunchDisposition (..),

    -- ** LaunchStatus
    LaunchStatus (..),

    -- ** LifeCycleState
    LifeCycleState (..),

    -- ** ReplicationConfigurationDataPlaneRouting
    ReplicationConfigurationDataPlaneRouting (..),

    -- ** ReplicationConfigurationDefaultLargeStagingDiskType
    ReplicationConfigurationDefaultLargeStagingDiskType (..),

    -- ** ReplicationConfigurationEbsEncryption
    ReplicationConfigurationEbsEncryption (..),

    -- ** ReplicationConfigurationReplicatedDiskStagingDiskType
    ReplicationConfigurationReplicatedDiskStagingDiskType (..),

    -- ** TargetInstanceTypeRightSizingMethod
    TargetInstanceTypeRightSizingMethod (..),

    -- ** CPU
    CPU (CPU'),
    newCPU,

    -- ** ChangeServerLifeCycleStateSourceServerLifecycle
    ChangeServerLifeCycleStateSourceServerLifecycle (ChangeServerLifeCycleStateSourceServerLifecycle'),
    newChangeServerLifeCycleStateSourceServerLifecycle,

    -- ** DataReplicationError
    DataReplicationError (DataReplicationError'),
    newDataReplicationError,

    -- ** DataReplicationInfo
    DataReplicationInfo (DataReplicationInfo'),
    newDataReplicationInfo,

    -- ** DataReplicationInfoReplicatedDisk
    DataReplicationInfoReplicatedDisk (DataReplicationInfoReplicatedDisk'),
    newDataReplicationInfoReplicatedDisk,

    -- ** DataReplicationInitiation
    DataReplicationInitiation (DataReplicationInitiation'),
    newDataReplicationInitiation,

    -- ** DataReplicationInitiationStep
    DataReplicationInitiationStep (DataReplicationInitiationStep'),
    newDataReplicationInitiationStep,

    -- ** DescribeJobsRequestFilters
    DescribeJobsRequestFilters (DescribeJobsRequestFilters'),
    newDescribeJobsRequestFilters,

    -- ** DescribeSourceServersRequestFilters
    DescribeSourceServersRequestFilters (DescribeSourceServersRequestFilters'),
    newDescribeSourceServersRequestFilters,

    -- ** Disk
    Disk (Disk'),
    newDisk,

    -- ** IdentificationHints
    IdentificationHints (IdentificationHints'),
    newIdentificationHints,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** JobLog
    JobLog (JobLog'),
    newJobLog,

    -- ** JobLogEventData
    JobLogEventData (JobLogEventData'),
    newJobLogEventData,

    -- ** LaunchConfiguration
    LaunchConfiguration (LaunchConfiguration'),
    newLaunchConfiguration,

    -- ** LaunchedInstance
    LaunchedInstance (LaunchedInstance'),
    newLaunchedInstance,

    -- ** Licensing
    Licensing (Licensing'),
    newLicensing,

    -- ** LifeCycle
    LifeCycle (LifeCycle'),
    newLifeCycle,

    -- ** LifeCycleLastCutover
    LifeCycleLastCutover (LifeCycleLastCutover'),
    newLifeCycleLastCutover,

    -- ** LifeCycleLastCutoverFinalized
    LifeCycleLastCutoverFinalized (LifeCycleLastCutoverFinalized'),
    newLifeCycleLastCutoverFinalized,

    -- ** LifeCycleLastCutoverInitiated
    LifeCycleLastCutoverInitiated (LifeCycleLastCutoverInitiated'),
    newLifeCycleLastCutoverInitiated,

    -- ** LifeCycleLastCutoverReverted
    LifeCycleLastCutoverReverted (LifeCycleLastCutoverReverted'),
    newLifeCycleLastCutoverReverted,

    -- ** LifeCycleLastTest
    LifeCycleLastTest (LifeCycleLastTest'),
    newLifeCycleLastTest,

    -- ** LifeCycleLastTestFinalized
    LifeCycleLastTestFinalized (LifeCycleLastTestFinalized'),
    newLifeCycleLastTestFinalized,

    -- ** LifeCycleLastTestInitiated
    LifeCycleLastTestInitiated (LifeCycleLastTestInitiated'),
    newLifeCycleLastTestInitiated,

    -- ** LifeCycleLastTestReverted
    LifeCycleLastTestReverted (LifeCycleLastTestReverted'),
    newLifeCycleLastTestReverted,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** OS
    OS (OS'),
    newOS,

    -- ** ParticipatingServer
    ParticipatingServer (ParticipatingServer'),
    newParticipatingServer,

    -- ** ReplicationConfiguration
    ReplicationConfiguration (ReplicationConfiguration'),
    newReplicationConfiguration,

    -- ** ReplicationConfigurationReplicatedDisk
    ReplicationConfigurationReplicatedDisk (ReplicationConfigurationReplicatedDisk'),
    newReplicationConfigurationReplicatedDisk,

    -- ** ReplicationConfigurationTemplate
    ReplicationConfigurationTemplate (ReplicationConfigurationTemplate'),
    newReplicationConfigurationTemplate,

    -- ** SourceProperties
    SourceProperties (SourceProperties'),
    newSourceProperties,

    -- ** SourceServer
    SourceServer (SourceServer'),
    newSourceServer,
  )
where

import Amazonka.MGN.ChangeServerLifeCycleState
import Amazonka.MGN.CreateReplicationConfigurationTemplate
import Amazonka.MGN.DeleteJob
import Amazonka.MGN.DeleteReplicationConfigurationTemplate
import Amazonka.MGN.DeleteSourceServer
import Amazonka.MGN.DescribeJobLogItems
import Amazonka.MGN.DescribeJobs
import Amazonka.MGN.DescribeReplicationConfigurationTemplates
import Amazonka.MGN.DescribeSourceServers
import Amazonka.MGN.DisconnectFromService
import Amazonka.MGN.FinalizeCutover
import Amazonka.MGN.GetLaunchConfiguration
import Amazonka.MGN.GetReplicationConfiguration
import Amazonka.MGN.InitializeService
import Amazonka.MGN.Lens
import Amazonka.MGN.ListTagsForResource
import Amazonka.MGN.MarkAsArchived
import Amazonka.MGN.RetryDataReplication
import Amazonka.MGN.StartCutover
import Amazonka.MGN.StartTest
import Amazonka.MGN.TagResource
import Amazonka.MGN.TerminateTargetInstances
import Amazonka.MGN.Types
import Amazonka.MGN.UntagResource
import Amazonka.MGN.UpdateLaunchConfiguration
import Amazonka.MGN.UpdateReplicationConfiguration
import Amazonka.MGN.UpdateReplicationConfigurationTemplate
import Amazonka.MGN.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MGN'.

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
