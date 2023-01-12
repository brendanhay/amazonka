{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DrS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-02-26@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Elastic Disaster Recovery Service.
module Amazonka.DrS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UninitializedAccountException
    _UninitializedAccountException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateExtendedSourceServer
    CreateExtendedSourceServer (CreateExtendedSourceServer'),
    newCreateExtendedSourceServer,
    CreateExtendedSourceServerResponse (CreateExtendedSourceServerResponse'),
    newCreateExtendedSourceServerResponse,

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

    -- ** DeleteRecoveryInstance
    DeleteRecoveryInstance (DeleteRecoveryInstance'),
    newDeleteRecoveryInstance,
    DeleteRecoveryInstanceResponse (DeleteRecoveryInstanceResponse'),
    newDeleteRecoveryInstanceResponse,

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

    -- ** DescribeRecoveryInstances (Paginated)
    DescribeRecoveryInstances (DescribeRecoveryInstances'),
    newDescribeRecoveryInstances,
    DescribeRecoveryInstancesResponse (DescribeRecoveryInstancesResponse'),
    newDescribeRecoveryInstancesResponse,

    -- ** DescribeRecoverySnapshots (Paginated)
    DescribeRecoverySnapshots (DescribeRecoverySnapshots'),
    newDescribeRecoverySnapshots,
    DescribeRecoverySnapshotsResponse (DescribeRecoverySnapshotsResponse'),
    newDescribeRecoverySnapshotsResponse,

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

    -- ** DisconnectRecoveryInstance
    DisconnectRecoveryInstance (DisconnectRecoveryInstance'),
    newDisconnectRecoveryInstance,
    DisconnectRecoveryInstanceResponse (DisconnectRecoveryInstanceResponse'),
    newDisconnectRecoveryInstanceResponse,

    -- ** DisconnectSourceServer
    DisconnectSourceServer (DisconnectSourceServer'),
    newDisconnectSourceServer,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** GetFailbackReplicationConfiguration
    GetFailbackReplicationConfiguration (GetFailbackReplicationConfiguration'),
    newGetFailbackReplicationConfiguration,
    GetFailbackReplicationConfigurationResponse (GetFailbackReplicationConfigurationResponse'),
    newGetFailbackReplicationConfigurationResponse,

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

    -- ** ListExtensibleSourceServers (Paginated)
    ListExtensibleSourceServers (ListExtensibleSourceServers'),
    newListExtensibleSourceServers,
    ListExtensibleSourceServersResponse (ListExtensibleSourceServersResponse'),
    newListExtensibleSourceServersResponse,

    -- ** ListStagingAccounts (Paginated)
    ListStagingAccounts (ListStagingAccounts'),
    newListStagingAccounts,
    ListStagingAccountsResponse (ListStagingAccountsResponse'),
    newListStagingAccountsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RetryDataReplication
    RetryDataReplication (RetryDataReplication'),
    newRetryDataReplication,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** ReverseReplication
    ReverseReplication (ReverseReplication'),
    newReverseReplication,
    ReverseReplicationResponse (ReverseReplicationResponse'),
    newReverseReplicationResponse,

    -- ** StartFailbackLaunch
    StartFailbackLaunch (StartFailbackLaunch'),
    newStartFailbackLaunch,
    StartFailbackLaunchResponse (StartFailbackLaunchResponse'),
    newStartFailbackLaunchResponse,

    -- ** StartRecovery
    StartRecovery (StartRecovery'),
    newStartRecovery,
    StartRecoveryResponse (StartRecoveryResponse'),
    newStartRecoveryResponse,

    -- ** StartReplication
    StartReplication (StartReplication'),
    newStartReplication,
    StartReplicationResponse (StartReplicationResponse'),
    newStartReplicationResponse,

    -- ** StopFailback
    StopFailback (StopFailback'),
    newStopFailback,
    StopFailbackResponse (StopFailbackResponse'),
    newStopFailbackResponse,

    -- ** StopReplication
    StopReplication (StopReplication'),
    newStopReplication,
    StopReplicationResponse (StopReplicationResponse'),
    newStopReplicationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TerminateRecoveryInstances
    TerminateRecoveryInstances (TerminateRecoveryInstances'),
    newTerminateRecoveryInstances,
    TerminateRecoveryInstancesResponse (TerminateRecoveryInstancesResponse'),
    newTerminateRecoveryInstancesResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateFailbackReplicationConfiguration
    UpdateFailbackReplicationConfiguration (UpdateFailbackReplicationConfiguration'),
    newUpdateFailbackReplicationConfiguration,
    UpdateFailbackReplicationConfigurationResponse (UpdateFailbackReplicationConfigurationResponse'),
    newUpdateFailbackReplicationConfigurationResponse,

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

    -- ** DataReplicationErrorString
    DataReplicationErrorString (..),

    -- ** DataReplicationInitiationStepName
    DataReplicationInitiationStepName (..),

    -- ** DataReplicationInitiationStepStatus
    DataReplicationInitiationStepStatus (..),

    -- ** DataReplicationState
    DataReplicationState (..),

    -- ** EC2InstanceState
    EC2InstanceState (..),

    -- ** ExtensionStatus
    ExtensionStatus (..),

    -- ** FailbackLaunchType
    FailbackLaunchType (..),

    -- ** FailbackReplicationError
    FailbackReplicationError (..),

    -- ** FailbackState
    FailbackState (..),

    -- ** InitiatedBy
    InitiatedBy (..),

    -- ** JobLogEvent
    JobLogEvent (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** JobType
    JobType (..),

    -- ** LastLaunchResult
    LastLaunchResult (..),

    -- ** LastLaunchType
    LastLaunchType (..),

    -- ** LaunchDisposition
    LaunchDisposition (..),

    -- ** LaunchStatus
    LaunchStatus (..),

    -- ** OriginEnvironment
    OriginEnvironment (..),

    -- ** PITPolicyRuleUnits
    PITPolicyRuleUnits (..),

    -- ** RecoveryInstanceDataReplicationInitiationStepName
    RecoveryInstanceDataReplicationInitiationStepName (..),

    -- ** RecoveryInstanceDataReplicationInitiationStepStatus
    RecoveryInstanceDataReplicationInitiationStepStatus (..),

    -- ** RecoveryInstanceDataReplicationState
    RecoveryInstanceDataReplicationState (..),

    -- ** RecoverySnapshotsOrder
    RecoverySnapshotsOrder (..),

    -- ** ReplicationConfigurationDataPlaneRouting
    ReplicationConfigurationDataPlaneRouting (..),

    -- ** ReplicationConfigurationDefaultLargeStagingDiskType
    ReplicationConfigurationDefaultLargeStagingDiskType (..),

    -- ** ReplicationConfigurationEbsEncryption
    ReplicationConfigurationEbsEncryption (..),

    -- ** ReplicationConfigurationReplicatedDiskStagingDiskType
    ReplicationConfigurationReplicatedDiskStagingDiskType (..),

    -- ** ReplicationDirection
    ReplicationDirection (..),

    -- ** TargetInstanceTypeRightSizingMethod
    TargetInstanceTypeRightSizingMethod (..),

    -- ** Account
    Account (Account'),
    newAccount,

    -- ** CPU
    CPU (CPU'),
    newCPU,

    -- ** ConversionProperties
    ConversionProperties (ConversionProperties'),
    newConversionProperties,

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

    -- ** DescribeRecoveryInstancesRequestFilters
    DescribeRecoveryInstancesRequestFilters (DescribeRecoveryInstancesRequestFilters'),
    newDescribeRecoveryInstancesRequestFilters,

    -- ** DescribeRecoverySnapshotsRequestFilters
    DescribeRecoverySnapshotsRequestFilters (DescribeRecoverySnapshotsRequestFilters'),
    newDescribeRecoverySnapshotsRequestFilters,

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

    -- ** Licensing
    Licensing (Licensing'),
    newLicensing,

    -- ** LifeCycle
    LifeCycle (LifeCycle'),
    newLifeCycle,

    -- ** LifeCycleLastLaunch
    LifeCycleLastLaunch (LifeCycleLastLaunch'),
    newLifeCycleLastLaunch,

    -- ** LifeCycleLastLaunchInitiated
    LifeCycleLastLaunchInitiated (LifeCycleLastLaunchInitiated'),
    newLifeCycleLastLaunchInitiated,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** OS
    OS (OS'),
    newOS,

    -- ** PITPolicyRule
    PITPolicyRule (PITPolicyRule'),
    newPITPolicyRule,

    -- ** ParticipatingServer
    ParticipatingServer (ParticipatingServer'),
    newParticipatingServer,

    -- ** RecoveryInstance
    RecoveryInstance (RecoveryInstance'),
    newRecoveryInstance,

    -- ** RecoveryInstanceDataReplicationError
    RecoveryInstanceDataReplicationError (RecoveryInstanceDataReplicationError'),
    newRecoveryInstanceDataReplicationError,

    -- ** RecoveryInstanceDataReplicationInfo
    RecoveryInstanceDataReplicationInfo (RecoveryInstanceDataReplicationInfo'),
    newRecoveryInstanceDataReplicationInfo,

    -- ** RecoveryInstanceDataReplicationInfoReplicatedDisk
    RecoveryInstanceDataReplicationInfoReplicatedDisk (RecoveryInstanceDataReplicationInfoReplicatedDisk'),
    newRecoveryInstanceDataReplicationInfoReplicatedDisk,

    -- ** RecoveryInstanceDataReplicationInitiation
    RecoveryInstanceDataReplicationInitiation (RecoveryInstanceDataReplicationInitiation'),
    newRecoveryInstanceDataReplicationInitiation,

    -- ** RecoveryInstanceDataReplicationInitiationStep
    RecoveryInstanceDataReplicationInitiationStep (RecoveryInstanceDataReplicationInitiationStep'),
    newRecoveryInstanceDataReplicationInitiationStep,

    -- ** RecoveryInstanceDisk
    RecoveryInstanceDisk (RecoveryInstanceDisk'),
    newRecoveryInstanceDisk,

    -- ** RecoveryInstanceFailback
    RecoveryInstanceFailback (RecoveryInstanceFailback'),
    newRecoveryInstanceFailback,

    -- ** RecoveryInstanceProperties
    RecoveryInstanceProperties (RecoveryInstanceProperties'),
    newRecoveryInstanceProperties,

    -- ** RecoverySnapshot
    RecoverySnapshot (RecoverySnapshot'),
    newRecoverySnapshot,

    -- ** ReplicationConfiguration
    ReplicationConfiguration (ReplicationConfiguration'),
    newReplicationConfiguration,

    -- ** ReplicationConfigurationReplicatedDisk
    ReplicationConfigurationReplicatedDisk (ReplicationConfigurationReplicatedDisk'),
    newReplicationConfigurationReplicatedDisk,

    -- ** ReplicationConfigurationTemplate
    ReplicationConfigurationTemplate (ReplicationConfigurationTemplate'),
    newReplicationConfigurationTemplate,

    -- ** SourceCloudProperties
    SourceCloudProperties (SourceCloudProperties'),
    newSourceCloudProperties,

    -- ** SourceProperties
    SourceProperties (SourceProperties'),
    newSourceProperties,

    -- ** SourceServer
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** StagingArea
    StagingArea (StagingArea'),
    newStagingArea,

    -- ** StagingSourceServer
    StagingSourceServer (StagingSourceServer'),
    newStagingSourceServer,

    -- ** StartRecoveryRequestSourceServer
    StartRecoveryRequestSourceServer (StartRecoveryRequestSourceServer'),
    newStartRecoveryRequestSourceServer,
  )
where

import Amazonka.DrS.CreateExtendedSourceServer
import Amazonka.DrS.CreateReplicationConfigurationTemplate
import Amazonka.DrS.DeleteJob
import Amazonka.DrS.DeleteRecoveryInstance
import Amazonka.DrS.DeleteReplicationConfigurationTemplate
import Amazonka.DrS.DeleteSourceServer
import Amazonka.DrS.DescribeJobLogItems
import Amazonka.DrS.DescribeJobs
import Amazonka.DrS.DescribeRecoveryInstances
import Amazonka.DrS.DescribeRecoverySnapshots
import Amazonka.DrS.DescribeReplicationConfigurationTemplates
import Amazonka.DrS.DescribeSourceServers
import Amazonka.DrS.DisconnectRecoveryInstance
import Amazonka.DrS.DisconnectSourceServer
import Amazonka.DrS.GetFailbackReplicationConfiguration
import Amazonka.DrS.GetLaunchConfiguration
import Amazonka.DrS.GetReplicationConfiguration
import Amazonka.DrS.InitializeService
import Amazonka.DrS.Lens
import Amazonka.DrS.ListExtensibleSourceServers
import Amazonka.DrS.ListStagingAccounts
import Amazonka.DrS.ListTagsForResource
import Amazonka.DrS.RetryDataReplication
import Amazonka.DrS.ReverseReplication
import Amazonka.DrS.StartFailbackLaunch
import Amazonka.DrS.StartRecovery
import Amazonka.DrS.StartReplication
import Amazonka.DrS.StopFailback
import Amazonka.DrS.StopReplication
import Amazonka.DrS.TagResource
import Amazonka.DrS.TerminateRecoveryInstances
import Amazonka.DrS.Types
import Amazonka.DrS.UntagResource
import Amazonka.DrS.UpdateFailbackReplicationConfiguration
import Amazonka.DrS.UpdateLaunchConfiguration
import Amazonka.DrS.UpdateReplicationConfiguration
import Amazonka.DrS.UpdateReplicationConfigurationTemplate
import Amazonka.DrS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DrS'.

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
