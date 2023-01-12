{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MGN
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- ** ArchiveApplication
    ArchiveApplication (ArchiveApplication'),
    newArchiveApplication,
    Application (Application'),
    newApplication,

    -- ** ArchiveWave
    ArchiveWave (ArchiveWave'),
    newArchiveWave,
    Wave (Wave'),
    newWave,

    -- ** AssociateApplications
    AssociateApplications (AssociateApplications'),
    newAssociateApplications,
    AssociateApplicationsResponse (AssociateApplicationsResponse'),
    newAssociateApplicationsResponse,

    -- ** AssociateSourceServers
    AssociateSourceServers (AssociateSourceServers'),
    newAssociateSourceServers,
    AssociateSourceServersResponse (AssociateSourceServersResponse'),
    newAssociateSourceServersResponse,

    -- ** ChangeServerLifeCycleState
    ChangeServerLifeCycleState (ChangeServerLifeCycleState'),
    newChangeServerLifeCycleState,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    Application (Application'),
    newApplication,

    -- ** CreateLaunchConfigurationTemplate
    CreateLaunchConfigurationTemplate (CreateLaunchConfigurationTemplate'),
    newCreateLaunchConfigurationTemplate,
    LaunchConfigurationTemplate (LaunchConfigurationTemplate'),
    newLaunchConfigurationTemplate,

    -- ** CreateReplicationConfigurationTemplate
    CreateReplicationConfigurationTemplate (CreateReplicationConfigurationTemplate'),
    newCreateReplicationConfigurationTemplate,
    ReplicationConfigurationTemplate (ReplicationConfigurationTemplate'),
    newReplicationConfigurationTemplate,

    -- ** CreateWave
    CreateWave (CreateWave'),
    newCreateWave,
    Wave (Wave'),
    newWave,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** DeleteLaunchConfigurationTemplate
    DeleteLaunchConfigurationTemplate (DeleteLaunchConfigurationTemplate'),
    newDeleteLaunchConfigurationTemplate,
    DeleteLaunchConfigurationTemplateResponse (DeleteLaunchConfigurationTemplateResponse'),
    newDeleteLaunchConfigurationTemplateResponse,

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

    -- ** DeleteVcenterClient
    DeleteVcenterClient (DeleteVcenterClient'),
    newDeleteVcenterClient,
    DeleteVcenterClientResponse (DeleteVcenterClientResponse'),
    newDeleteVcenterClientResponse,

    -- ** DeleteWave
    DeleteWave (DeleteWave'),
    newDeleteWave,
    DeleteWaveResponse (DeleteWaveResponse'),
    newDeleteWaveResponse,

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

    -- ** DescribeLaunchConfigurationTemplates (Paginated)
    DescribeLaunchConfigurationTemplates (DescribeLaunchConfigurationTemplates'),
    newDescribeLaunchConfigurationTemplates,
    DescribeLaunchConfigurationTemplatesResponse (DescribeLaunchConfigurationTemplatesResponse'),
    newDescribeLaunchConfigurationTemplatesResponse,

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

    -- ** DescribeVcenterClients (Paginated)
    DescribeVcenterClients (DescribeVcenterClients'),
    newDescribeVcenterClients,
    DescribeVcenterClientsResponse (DescribeVcenterClientsResponse'),
    newDescribeVcenterClientsResponse,

    -- ** DisassociateApplications
    DisassociateApplications (DisassociateApplications'),
    newDisassociateApplications,
    DisassociateApplicationsResponse (DisassociateApplicationsResponse'),
    newDisassociateApplicationsResponse,

    -- ** DisassociateSourceServers
    DisassociateSourceServers (DisassociateSourceServers'),
    newDisassociateSourceServers,
    DisassociateSourceServersResponse (DisassociateSourceServersResponse'),
    newDisassociateSourceServersResponse,

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

    -- ** ListApplications (Paginated)
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** ListSourceServerActions (Paginated)
    ListSourceServerActions (ListSourceServerActions'),
    newListSourceServerActions,
    ListSourceServerActionsResponse (ListSourceServerActionsResponse'),
    newListSourceServerActionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTemplateActions (Paginated)
    ListTemplateActions (ListTemplateActions'),
    newListTemplateActions,
    ListTemplateActionsResponse (ListTemplateActionsResponse'),
    newListTemplateActionsResponse,

    -- ** ListWaves (Paginated)
    ListWaves (ListWaves'),
    newListWaves,
    ListWavesResponse (ListWavesResponse'),
    newListWavesResponse,

    -- ** MarkAsArchived
    MarkAsArchived (MarkAsArchived'),
    newMarkAsArchived,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** PutSourceServerAction
    PutSourceServerAction (PutSourceServerAction'),
    newPutSourceServerAction,
    SourceServerActionDocument (SourceServerActionDocument'),
    newSourceServerActionDocument,

    -- ** PutTemplateAction
    PutTemplateAction (PutTemplateAction'),
    newPutTemplateAction,
    TemplateActionDocument (TemplateActionDocument'),
    newTemplateActionDocument,

    -- ** RemoveSourceServerAction
    RemoveSourceServerAction (RemoveSourceServerAction'),
    newRemoveSourceServerAction,
    RemoveSourceServerActionResponse (RemoveSourceServerActionResponse'),
    newRemoveSourceServerActionResponse,

    -- ** RemoveTemplateAction
    RemoveTemplateAction (RemoveTemplateAction'),
    newRemoveTemplateAction,
    RemoveTemplateActionResponse (RemoveTemplateActionResponse'),
    newRemoveTemplateActionResponse,

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

    -- ** StartReplication
    StartReplication (StartReplication'),
    newStartReplication,
    SourceServer (SourceServer'),
    newSourceServer,

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

    -- ** UnarchiveApplication
    UnarchiveApplication (UnarchiveApplication'),
    newUnarchiveApplication,
    Application (Application'),
    newApplication,

    -- ** UnarchiveWave
    UnarchiveWave (UnarchiveWave'),
    newUnarchiveWave,
    Wave (Wave'),
    newWave,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    Application (Application'),
    newApplication,

    -- ** UpdateLaunchConfiguration
    UpdateLaunchConfiguration (UpdateLaunchConfiguration'),
    newUpdateLaunchConfiguration,
    LaunchConfiguration (LaunchConfiguration'),
    newLaunchConfiguration,

    -- ** UpdateLaunchConfigurationTemplate
    UpdateLaunchConfigurationTemplate (UpdateLaunchConfigurationTemplate'),
    newUpdateLaunchConfigurationTemplate,
    LaunchConfigurationTemplate (LaunchConfigurationTemplate'),
    newLaunchConfigurationTemplate,

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

    -- ** UpdateSourceServerReplicationType
    UpdateSourceServerReplicationType (UpdateSourceServerReplicationType'),
    newUpdateSourceServerReplicationType,
    SourceServer (SourceServer'),
    newSourceServer,

    -- ** UpdateWave
    UpdateWave (UpdateWave'),
    newUpdateWave,
    Wave (Wave'),
    newWave,

    -- * Types

    -- ** ApplicationHealthStatus
    ApplicationHealthStatus (..),

    -- ** ApplicationProgressStatus
    ApplicationProgressStatus (..),

    -- ** BootMode
    BootMode (..),

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

    -- ** PostLaunchActionExecutionStatus
    PostLaunchActionExecutionStatus (..),

    -- ** PostLaunchActionsDeploymentType
    PostLaunchActionsDeploymentType (..),

    -- ** ReplicationConfigurationDataPlaneRouting
    ReplicationConfigurationDataPlaneRouting (..),

    -- ** ReplicationConfigurationDefaultLargeStagingDiskType
    ReplicationConfigurationDefaultLargeStagingDiskType (..),

    -- ** ReplicationConfigurationEbsEncryption
    ReplicationConfigurationEbsEncryption (..),

    -- ** ReplicationConfigurationReplicatedDiskStagingDiskType
    ReplicationConfigurationReplicatedDiskStagingDiskType (..),

    -- ** ReplicationType
    ReplicationType (..),

    -- ** SsmDocumentType
    SsmDocumentType (..),

    -- ** SsmParameterStoreParameterType
    SsmParameterStoreParameterType (..),

    -- ** TargetInstanceTypeRightSizingMethod
    TargetInstanceTypeRightSizingMethod (..),

    -- ** VolumeType
    VolumeType (..),

    -- ** WaveHealthStatus
    WaveHealthStatus (..),

    -- ** WaveProgressStatus
    WaveProgressStatus (..),

    -- ** Application
    Application (Application'),
    newApplication,

    -- ** ApplicationAggregatedStatus
    ApplicationAggregatedStatus (ApplicationAggregatedStatus'),
    newApplicationAggregatedStatus,

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

    -- ** JobPostLaunchActionsLaunchStatus
    JobPostLaunchActionsLaunchStatus (JobPostLaunchActionsLaunchStatus'),
    newJobPostLaunchActionsLaunchStatus,

    -- ** LaunchConfiguration
    LaunchConfiguration (LaunchConfiguration'),
    newLaunchConfiguration,

    -- ** LaunchConfigurationTemplate
    LaunchConfigurationTemplate (LaunchConfigurationTemplate'),
    newLaunchConfigurationTemplate,

    -- ** LaunchTemplateDiskConf
    LaunchTemplateDiskConf (LaunchTemplateDiskConf'),
    newLaunchTemplateDiskConf,

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

    -- ** ListApplicationsRequestFilters
    ListApplicationsRequestFilters (ListApplicationsRequestFilters'),
    newListApplicationsRequestFilters,

    -- ** ListWavesRequestFilters
    ListWavesRequestFilters (ListWavesRequestFilters'),
    newListWavesRequestFilters,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** OS
    OS (OS'),
    newOS,

    -- ** ParticipatingServer
    ParticipatingServer (ParticipatingServer'),
    newParticipatingServer,

    -- ** PostLaunchActions
    PostLaunchActions (PostLaunchActions'),
    newPostLaunchActions,

    -- ** PostLaunchActionsStatus
    PostLaunchActionsStatus (PostLaunchActionsStatus'),
    newPostLaunchActionsStatus,

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

    -- ** SourceServerActionDocument
    SourceServerActionDocument (SourceServerActionDocument'),
    newSourceServerActionDocument,

    -- ** SourceServerActionsRequestFilters
    SourceServerActionsRequestFilters (SourceServerActionsRequestFilters'),
    newSourceServerActionsRequestFilters,

    -- ** SsmDocument
    SsmDocument (SsmDocument'),
    newSsmDocument,

    -- ** SsmParameterStoreParameter
    SsmParameterStoreParameter (SsmParameterStoreParameter'),
    newSsmParameterStoreParameter,

    -- ** TemplateActionDocument
    TemplateActionDocument (TemplateActionDocument'),
    newTemplateActionDocument,

    -- ** TemplateActionsRequestFilters
    TemplateActionsRequestFilters (TemplateActionsRequestFilters'),
    newTemplateActionsRequestFilters,

    -- ** VcenterClient
    VcenterClient (VcenterClient'),
    newVcenterClient,

    -- ** Wave
    Wave (Wave'),
    newWave,

    -- ** WaveAggregatedStatus
    WaveAggregatedStatus (WaveAggregatedStatus'),
    newWaveAggregatedStatus,
  )
where

import Amazonka.MGN.ArchiveApplication
import Amazonka.MGN.ArchiveWave
import Amazonka.MGN.AssociateApplications
import Amazonka.MGN.AssociateSourceServers
import Amazonka.MGN.ChangeServerLifeCycleState
import Amazonka.MGN.CreateApplication
import Amazonka.MGN.CreateLaunchConfigurationTemplate
import Amazonka.MGN.CreateReplicationConfigurationTemplate
import Amazonka.MGN.CreateWave
import Amazonka.MGN.DeleteApplication
import Amazonka.MGN.DeleteJob
import Amazonka.MGN.DeleteLaunchConfigurationTemplate
import Amazonka.MGN.DeleteReplicationConfigurationTemplate
import Amazonka.MGN.DeleteSourceServer
import Amazonka.MGN.DeleteVcenterClient
import Amazonka.MGN.DeleteWave
import Amazonka.MGN.DescribeJobLogItems
import Amazonka.MGN.DescribeJobs
import Amazonka.MGN.DescribeLaunchConfigurationTemplates
import Amazonka.MGN.DescribeReplicationConfigurationTemplates
import Amazonka.MGN.DescribeSourceServers
import Amazonka.MGN.DescribeVcenterClients
import Amazonka.MGN.DisassociateApplications
import Amazonka.MGN.DisassociateSourceServers
import Amazonka.MGN.DisconnectFromService
import Amazonka.MGN.FinalizeCutover
import Amazonka.MGN.GetLaunchConfiguration
import Amazonka.MGN.GetReplicationConfiguration
import Amazonka.MGN.InitializeService
import Amazonka.MGN.Lens
import Amazonka.MGN.ListApplications
import Amazonka.MGN.ListSourceServerActions
import Amazonka.MGN.ListTagsForResource
import Amazonka.MGN.ListTemplateActions
import Amazonka.MGN.ListWaves
import Amazonka.MGN.MarkAsArchived
import Amazonka.MGN.PutSourceServerAction
import Amazonka.MGN.PutTemplateAction
import Amazonka.MGN.RemoveSourceServerAction
import Amazonka.MGN.RemoveTemplateAction
import Amazonka.MGN.RetryDataReplication
import Amazonka.MGN.StartCutover
import Amazonka.MGN.StartReplication
import Amazonka.MGN.StartTest
import Amazonka.MGN.TagResource
import Amazonka.MGN.TerminateTargetInstances
import Amazonka.MGN.Types
import Amazonka.MGN.UnarchiveApplication
import Amazonka.MGN.UnarchiveWave
import Amazonka.MGN.UntagResource
import Amazonka.MGN.UpdateApplication
import Amazonka.MGN.UpdateLaunchConfiguration
import Amazonka.MGN.UpdateLaunchConfigurationTemplate
import Amazonka.MGN.UpdateReplicationConfiguration
import Amazonka.MGN.UpdateReplicationConfigurationTemplate
import Amazonka.MGN.UpdateSourceServerReplicationType
import Amazonka.MGN.UpdateWave
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
