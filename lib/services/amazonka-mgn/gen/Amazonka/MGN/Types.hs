{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MGN.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _UninitializedAccountException,
    _ValidationException,

    -- * ApplicationHealthStatus
    ApplicationHealthStatus (..),

    -- * ApplicationProgressStatus
    ApplicationProgressStatus (..),

    -- * BootMode
    BootMode (..),

    -- * ChangeServerLifeCycleStateSourceServerLifecycleState
    ChangeServerLifeCycleStateSourceServerLifecycleState (..),

    -- * DataReplicationErrorString
    DataReplicationErrorString (..),

    -- * DataReplicationInitiationStepName
    DataReplicationInitiationStepName (..),

    -- * DataReplicationInitiationStepStatus
    DataReplicationInitiationStepStatus (..),

    -- * DataReplicationState
    DataReplicationState (..),

    -- * FirstBoot
    FirstBoot (..),

    -- * InitiatedBy
    InitiatedBy (..),

    -- * JobLogEvent
    JobLogEvent (..),

    -- * JobStatus
    JobStatus (..),

    -- * JobType
    JobType (..),

    -- * LaunchDisposition
    LaunchDisposition (..),

    -- * LaunchStatus
    LaunchStatus (..),

    -- * LifeCycleState
    LifeCycleState (..),

    -- * PostLaunchActionExecutionStatus
    PostLaunchActionExecutionStatus (..),

    -- * PostLaunchActionsDeploymentType
    PostLaunchActionsDeploymentType (..),

    -- * ReplicationConfigurationDataPlaneRouting
    ReplicationConfigurationDataPlaneRouting (..),

    -- * ReplicationConfigurationDefaultLargeStagingDiskType
    ReplicationConfigurationDefaultLargeStagingDiskType (..),

    -- * ReplicationConfigurationEbsEncryption
    ReplicationConfigurationEbsEncryption (..),

    -- * ReplicationConfigurationReplicatedDiskStagingDiskType
    ReplicationConfigurationReplicatedDiskStagingDiskType (..),

    -- * ReplicationType
    ReplicationType (..),

    -- * SsmDocumentType
    SsmDocumentType (..),

    -- * SsmParameterStoreParameterType
    SsmParameterStoreParameterType (..),

    -- * TargetInstanceTypeRightSizingMethod
    TargetInstanceTypeRightSizingMethod (..),

    -- * VolumeType
    VolumeType (..),

    -- * WaveHealthStatus
    WaveHealthStatus (..),

    -- * WaveProgressStatus
    WaveProgressStatus (..),

    -- * Application
    Application (..),
    newApplication,
    application_applicationAggregatedStatus,
    application_applicationID,
    application_arn,
    application_creationDateTime,
    application_description,
    application_isArchived,
    application_lastModifiedDateTime,
    application_name,
    application_tags,
    application_waveID,

    -- * ApplicationAggregatedStatus
    ApplicationAggregatedStatus (..),
    newApplicationAggregatedStatus,
    applicationAggregatedStatus_healthStatus,
    applicationAggregatedStatus_lastUpdateDateTime,
    applicationAggregatedStatus_progressStatus,
    applicationAggregatedStatus_totalSourceServers,

    -- * CPU
    CPU (..),
    newCPU,
    cpu_cores,
    cpu_modelName,

    -- * ChangeServerLifeCycleStateSourceServerLifecycle
    ChangeServerLifeCycleStateSourceServerLifecycle (..),
    newChangeServerLifeCycleStateSourceServerLifecycle,
    changeServerLifeCycleStateSourceServerLifecycle_state,

    -- * DataReplicationError
    DataReplicationError (..),
    newDataReplicationError,
    dataReplicationError_error,
    dataReplicationError_rawError,

    -- * DataReplicationInfo
    DataReplicationInfo (..),
    newDataReplicationInfo,
    dataReplicationInfo_dataReplicationError,
    dataReplicationInfo_dataReplicationInitiation,
    dataReplicationInfo_dataReplicationState,
    dataReplicationInfo_etaDateTime,
    dataReplicationInfo_lagDuration,
    dataReplicationInfo_lastSnapshotDateTime,
    dataReplicationInfo_replicatedDisks,

    -- * DataReplicationInfoReplicatedDisk
    DataReplicationInfoReplicatedDisk (..),
    newDataReplicationInfoReplicatedDisk,
    dataReplicationInfoReplicatedDisk_backloggedStorageBytes,
    dataReplicationInfoReplicatedDisk_deviceName,
    dataReplicationInfoReplicatedDisk_replicatedStorageBytes,
    dataReplicationInfoReplicatedDisk_rescannedStorageBytes,
    dataReplicationInfoReplicatedDisk_totalStorageBytes,

    -- * DataReplicationInitiation
    DataReplicationInitiation (..),
    newDataReplicationInitiation,
    dataReplicationInitiation_nextAttemptDateTime,
    dataReplicationInitiation_startDateTime,
    dataReplicationInitiation_steps,

    -- * DataReplicationInitiationStep
    DataReplicationInitiationStep (..),
    newDataReplicationInitiationStep,
    dataReplicationInitiationStep_name,
    dataReplicationInitiationStep_status,

    -- * DescribeJobsRequestFilters
    DescribeJobsRequestFilters (..),
    newDescribeJobsRequestFilters,
    describeJobsRequestFilters_fromDate,
    describeJobsRequestFilters_jobIDs,
    describeJobsRequestFilters_toDate,

    -- * DescribeSourceServersRequestFilters
    DescribeSourceServersRequestFilters (..),
    newDescribeSourceServersRequestFilters,
    describeSourceServersRequestFilters_applicationIDs,
    describeSourceServersRequestFilters_isArchived,
    describeSourceServersRequestFilters_lifeCycleStates,
    describeSourceServersRequestFilters_replicationTypes,
    describeSourceServersRequestFilters_sourceServerIDs,

    -- * Disk
    Disk (..),
    newDisk,
    disk_bytes,
    disk_deviceName,

    -- * IdentificationHints
    IdentificationHints (..),
    newIdentificationHints,
    identificationHints_awsInstanceID,
    identificationHints_fqdn,
    identificationHints_hostname,
    identificationHints_vmPath,
    identificationHints_vmWareUuid,

    -- * Job
    Job (..),
    newJob,
    job_arn,
    job_creationDateTime,
    job_endDateTime,
    job_initiatedBy,
    job_participatingServers,
    job_status,
    job_tags,
    job_type,
    job_jobID,

    -- * JobLog
    JobLog (..),
    newJobLog,
    jobLog_event,
    jobLog_eventData,
    jobLog_logDateTime,

    -- * JobLogEventData
    JobLogEventData (..),
    newJobLogEventData,
    jobLogEventData_conversionServerID,
    jobLogEventData_rawError,
    jobLogEventData_sourceServerID,
    jobLogEventData_targetInstanceID,

    -- * JobPostLaunchActionsLaunchStatus
    JobPostLaunchActionsLaunchStatus (..),
    newJobPostLaunchActionsLaunchStatus,
    jobPostLaunchActionsLaunchStatus_executionID,
    jobPostLaunchActionsLaunchStatus_executionStatus,
    jobPostLaunchActionsLaunchStatus_failureReason,
    jobPostLaunchActionsLaunchStatus_ssmDocument,
    jobPostLaunchActionsLaunchStatus_ssmDocumentType,

    -- * LaunchConfiguration
    LaunchConfiguration (..),
    newLaunchConfiguration,
    launchConfiguration_bootMode,
    launchConfiguration_copyPrivateIp,
    launchConfiguration_copyTags,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_enableMapAutoTagging,
    launchConfiguration_launchDisposition,
    launchConfiguration_licensing,
    launchConfiguration_mapAutoTaggingMpeID,
    launchConfiguration_name,
    launchConfiguration_postLaunchActions,
    launchConfiguration_sourceServerID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,

    -- * LaunchConfigurationTemplate
    LaunchConfigurationTemplate (..),
    newLaunchConfigurationTemplate,
    launchConfigurationTemplate_arn,
    launchConfigurationTemplate_associatePublicIpAddress,
    launchConfigurationTemplate_bootMode,
    launchConfigurationTemplate_copyPrivateIp,
    launchConfigurationTemplate_copyTags,
    launchConfigurationTemplate_ec2LaunchTemplateID,
    launchConfigurationTemplate_enableMapAutoTagging,
    launchConfigurationTemplate_largeVolumeConf,
    launchConfigurationTemplate_launchDisposition,
    launchConfigurationTemplate_licensing,
    launchConfigurationTemplate_mapAutoTaggingMpeID,
    launchConfigurationTemplate_postLaunchActions,
    launchConfigurationTemplate_smallVolumeConf,
    launchConfigurationTemplate_smallVolumeMaxSize,
    launchConfigurationTemplate_tags,
    launchConfigurationTemplate_targetInstanceTypeRightSizingMethod,
    launchConfigurationTemplate_launchConfigurationTemplateID,

    -- * LaunchTemplateDiskConf
    LaunchTemplateDiskConf (..),
    newLaunchTemplateDiskConf,
    launchTemplateDiskConf_iops,
    launchTemplateDiskConf_throughput,
    launchTemplateDiskConf_volumeType,

    -- * LaunchedInstance
    LaunchedInstance (..),
    newLaunchedInstance,
    launchedInstance_ec2InstanceID,
    launchedInstance_firstBoot,
    launchedInstance_jobID,

    -- * Licensing
    Licensing (..),
    newLicensing,
    licensing_osByol,

    -- * LifeCycle
    LifeCycle (..),
    newLifeCycle,
    lifeCycle_addedToServiceDateTime,
    lifeCycle_elapsedReplicationDuration,
    lifeCycle_firstByteDateTime,
    lifeCycle_lastCutover,
    lifeCycle_lastSeenByServiceDateTime,
    lifeCycle_lastTest,
    lifeCycle_state,

    -- * LifeCycleLastCutover
    LifeCycleLastCutover (..),
    newLifeCycleLastCutover,
    lifeCycleLastCutover_finalized,
    lifeCycleLastCutover_initiated,
    lifeCycleLastCutover_reverted,

    -- * LifeCycleLastCutoverFinalized
    LifeCycleLastCutoverFinalized (..),
    newLifeCycleLastCutoverFinalized,
    lifeCycleLastCutoverFinalized_apiCallDateTime,

    -- * LifeCycleLastCutoverInitiated
    LifeCycleLastCutoverInitiated (..),
    newLifeCycleLastCutoverInitiated,
    lifeCycleLastCutoverInitiated_apiCallDateTime,
    lifeCycleLastCutoverInitiated_jobID,

    -- * LifeCycleLastCutoverReverted
    LifeCycleLastCutoverReverted (..),
    newLifeCycleLastCutoverReverted,
    lifeCycleLastCutoverReverted_apiCallDateTime,

    -- * LifeCycleLastTest
    LifeCycleLastTest (..),
    newLifeCycleLastTest,
    lifeCycleLastTest_finalized,
    lifeCycleLastTest_initiated,
    lifeCycleLastTest_reverted,

    -- * LifeCycleLastTestFinalized
    LifeCycleLastTestFinalized (..),
    newLifeCycleLastTestFinalized,
    lifeCycleLastTestFinalized_apiCallDateTime,

    -- * LifeCycleLastTestInitiated
    LifeCycleLastTestInitiated (..),
    newLifeCycleLastTestInitiated,
    lifeCycleLastTestInitiated_apiCallDateTime,
    lifeCycleLastTestInitiated_jobID,

    -- * LifeCycleLastTestReverted
    LifeCycleLastTestReverted (..),
    newLifeCycleLastTestReverted,
    lifeCycleLastTestReverted_apiCallDateTime,

    -- * ListApplicationsRequestFilters
    ListApplicationsRequestFilters (..),
    newListApplicationsRequestFilters,
    listApplicationsRequestFilters_applicationIDs,
    listApplicationsRequestFilters_isArchived,
    listApplicationsRequestFilters_waveIDs,

    -- * ListWavesRequestFilters
    ListWavesRequestFilters (..),
    newListWavesRequestFilters,
    listWavesRequestFilters_isArchived,
    listWavesRequestFilters_waveIDs,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_ips,
    networkInterface_isPrimary,
    networkInterface_macAddress,

    -- * OS
    OS (..),
    newOS,
    os_fullString,

    -- * ParticipatingServer
    ParticipatingServer (..),
    newParticipatingServer,
    participatingServer_launchStatus,
    participatingServer_launchedEc2InstanceID,
    participatingServer_postLaunchActionsStatus,
    participatingServer_sourceServerID,

    -- * PostLaunchActions
    PostLaunchActions (..),
    newPostLaunchActions,
    postLaunchActions_cloudWatchLogGroupName,
    postLaunchActions_deployment,
    postLaunchActions_s3LogBucket,
    postLaunchActions_s3OutputKeyPrefix,
    postLaunchActions_ssmDocuments,

    -- * PostLaunchActionsStatus
    PostLaunchActionsStatus (..),
    newPostLaunchActionsStatus,
    postLaunchActionsStatus_postLaunchActionsLaunchStatusList,
    postLaunchActionsStatus_ssmAgentDiscoveryDatetime,

    -- * ReplicationConfiguration
    ReplicationConfiguration (..),
    newReplicationConfiguration,
    replicationConfiguration_associateDefaultSecurityGroup,
    replicationConfiguration_bandwidthThrottling,
    replicationConfiguration_createPublicIP,
    replicationConfiguration_dataPlaneRouting,
    replicationConfiguration_defaultLargeStagingDiskType,
    replicationConfiguration_ebsEncryption,
    replicationConfiguration_ebsEncryptionKeyArn,
    replicationConfiguration_name,
    replicationConfiguration_replicatedDisks,
    replicationConfiguration_replicationServerInstanceType,
    replicationConfiguration_replicationServersSecurityGroupsIDs,
    replicationConfiguration_sourceServerID,
    replicationConfiguration_stagingAreaSubnetId,
    replicationConfiguration_stagingAreaTags,
    replicationConfiguration_useDedicatedReplicationServer,

    -- * ReplicationConfigurationReplicatedDisk
    ReplicationConfigurationReplicatedDisk (..),
    newReplicationConfigurationReplicatedDisk,
    replicationConfigurationReplicatedDisk_deviceName,
    replicationConfigurationReplicatedDisk_iops,
    replicationConfigurationReplicatedDisk_isBootDisk,
    replicationConfigurationReplicatedDisk_stagingDiskType,
    replicationConfigurationReplicatedDisk_throughput,

    -- * ReplicationConfigurationTemplate
    ReplicationConfigurationTemplate (..),
    newReplicationConfigurationTemplate,
    replicationConfigurationTemplate_arn,
    replicationConfigurationTemplate_associateDefaultSecurityGroup,
    replicationConfigurationTemplate_bandwidthThrottling,
    replicationConfigurationTemplate_createPublicIP,
    replicationConfigurationTemplate_dataPlaneRouting,
    replicationConfigurationTemplate_defaultLargeStagingDiskType,
    replicationConfigurationTemplate_ebsEncryption,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_replicationServerInstanceType,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_stagingAreaSubnetId,
    replicationConfigurationTemplate_stagingAreaTags,
    replicationConfigurationTemplate_tags,
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- * SourceProperties
    SourceProperties (..),
    newSourceProperties,
    sourceProperties_cpus,
    sourceProperties_disks,
    sourceProperties_identificationHints,
    sourceProperties_lastUpdatedDateTime,
    sourceProperties_networkInterfaces,
    sourceProperties_os,
    sourceProperties_ramBytes,
    sourceProperties_recommendedInstanceType,

    -- * SourceServer
    SourceServer (..),
    newSourceServer,
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_vcenterClientID,

    -- * SourceServerActionDocument
    SourceServerActionDocument (..),
    newSourceServerActionDocument,
    sourceServerActionDocument_actionID,
    sourceServerActionDocument_actionName,
    sourceServerActionDocument_active,
    sourceServerActionDocument_documentIdentifier,
    sourceServerActionDocument_documentVersion,
    sourceServerActionDocument_mustSucceedForCutover,
    sourceServerActionDocument_order,
    sourceServerActionDocument_parameters,
    sourceServerActionDocument_timeoutSeconds,

    -- * SourceServerActionsRequestFilters
    SourceServerActionsRequestFilters (..),
    newSourceServerActionsRequestFilters,
    sourceServerActionsRequestFilters_actionIDs,

    -- * SsmDocument
    SsmDocument (..),
    newSsmDocument,
    ssmDocument_mustSucceedForCutover,
    ssmDocument_parameters,
    ssmDocument_timeoutSeconds,
    ssmDocument_actionName,
    ssmDocument_ssmDocumentName,

    -- * SsmParameterStoreParameter
    SsmParameterStoreParameter (..),
    newSsmParameterStoreParameter,
    ssmParameterStoreParameter_parameterName,
    ssmParameterStoreParameter_parameterType,

    -- * TemplateActionDocument
    TemplateActionDocument (..),
    newTemplateActionDocument,
    templateActionDocument_actionID,
    templateActionDocument_actionName,
    templateActionDocument_active,
    templateActionDocument_documentIdentifier,
    templateActionDocument_documentVersion,
    templateActionDocument_mustSucceedForCutover,
    templateActionDocument_operatingSystem,
    templateActionDocument_order,
    templateActionDocument_parameters,
    templateActionDocument_timeoutSeconds,

    -- * TemplateActionsRequestFilters
    TemplateActionsRequestFilters (..),
    newTemplateActionsRequestFilters,
    templateActionsRequestFilters_actionIDs,

    -- * VcenterClient
    VcenterClient (..),
    newVcenterClient,
    vcenterClient_arn,
    vcenterClient_datacenterName,
    vcenterClient_hostname,
    vcenterClient_lastSeenDatetime,
    vcenterClient_sourceServerTags,
    vcenterClient_tags,
    vcenterClient_vcenterClientID,
    vcenterClient_vcenterUUID,

    -- * Wave
    Wave (..),
    newWave,
    wave_arn,
    wave_creationDateTime,
    wave_description,
    wave_isArchived,
    wave_lastModifiedDateTime,
    wave_name,
    wave_tags,
    wave_waveAggregatedStatus,
    wave_waveID,

    -- * WaveAggregatedStatus
    WaveAggregatedStatus (..),
    newWaveAggregatedStatus,
    waveAggregatedStatus_healthStatus,
    waveAggregatedStatus_lastUpdateDateTime,
    waveAggregatedStatus_progressStatus,
    waveAggregatedStatus_replicationStartedDateTime,
    waveAggregatedStatus_totalApplications,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types.Application
import Amazonka.MGN.Types.ApplicationAggregatedStatus
import Amazonka.MGN.Types.ApplicationHealthStatus
import Amazonka.MGN.Types.ApplicationProgressStatus
import Amazonka.MGN.Types.BootMode
import Amazonka.MGN.Types.CPU
import Amazonka.MGN.Types.ChangeServerLifeCycleStateSourceServerLifecycle
import Amazonka.MGN.Types.ChangeServerLifeCycleStateSourceServerLifecycleState
import Amazonka.MGN.Types.DataReplicationError
import Amazonka.MGN.Types.DataReplicationErrorString
import Amazonka.MGN.Types.DataReplicationInfo
import Amazonka.MGN.Types.DataReplicationInfoReplicatedDisk
import Amazonka.MGN.Types.DataReplicationInitiation
import Amazonka.MGN.Types.DataReplicationInitiationStep
import Amazonka.MGN.Types.DataReplicationInitiationStepName
import Amazonka.MGN.Types.DataReplicationInitiationStepStatus
import Amazonka.MGN.Types.DataReplicationState
import Amazonka.MGN.Types.DescribeJobsRequestFilters
import Amazonka.MGN.Types.DescribeSourceServersRequestFilters
import Amazonka.MGN.Types.Disk
import Amazonka.MGN.Types.FirstBoot
import Amazonka.MGN.Types.IdentificationHints
import Amazonka.MGN.Types.InitiatedBy
import Amazonka.MGN.Types.Job
import Amazonka.MGN.Types.JobLog
import Amazonka.MGN.Types.JobLogEvent
import Amazonka.MGN.Types.JobLogEventData
import Amazonka.MGN.Types.JobPostLaunchActionsLaunchStatus
import Amazonka.MGN.Types.JobStatus
import Amazonka.MGN.Types.JobType
import Amazonka.MGN.Types.LaunchConfiguration
import Amazonka.MGN.Types.LaunchConfigurationTemplate
import Amazonka.MGN.Types.LaunchDisposition
import Amazonka.MGN.Types.LaunchStatus
import Amazonka.MGN.Types.LaunchTemplateDiskConf
import Amazonka.MGN.Types.LaunchedInstance
import Amazonka.MGN.Types.Licensing
import Amazonka.MGN.Types.LifeCycle
import Amazonka.MGN.Types.LifeCycleLastCutover
import Amazonka.MGN.Types.LifeCycleLastCutoverFinalized
import Amazonka.MGN.Types.LifeCycleLastCutoverInitiated
import Amazonka.MGN.Types.LifeCycleLastCutoverReverted
import Amazonka.MGN.Types.LifeCycleLastTest
import Amazonka.MGN.Types.LifeCycleLastTestFinalized
import Amazonka.MGN.Types.LifeCycleLastTestInitiated
import Amazonka.MGN.Types.LifeCycleLastTestReverted
import Amazonka.MGN.Types.LifeCycleState
import Amazonka.MGN.Types.ListApplicationsRequestFilters
import Amazonka.MGN.Types.ListWavesRequestFilters
import Amazonka.MGN.Types.NetworkInterface
import Amazonka.MGN.Types.OS
import Amazonka.MGN.Types.ParticipatingServer
import Amazonka.MGN.Types.PostLaunchActionExecutionStatus
import Amazonka.MGN.Types.PostLaunchActions
import Amazonka.MGN.Types.PostLaunchActionsDeploymentType
import Amazonka.MGN.Types.PostLaunchActionsStatus
import Amazonka.MGN.Types.ReplicationConfiguration
import Amazonka.MGN.Types.ReplicationConfigurationDataPlaneRouting
import Amazonka.MGN.Types.ReplicationConfigurationDefaultLargeStagingDiskType
import Amazonka.MGN.Types.ReplicationConfigurationEbsEncryption
import Amazonka.MGN.Types.ReplicationConfigurationReplicatedDisk
import Amazonka.MGN.Types.ReplicationConfigurationReplicatedDiskStagingDiskType
import Amazonka.MGN.Types.ReplicationConfigurationTemplate
import Amazonka.MGN.Types.ReplicationType
import Amazonka.MGN.Types.SourceProperties
import Amazonka.MGN.Types.SourceServer
import Amazonka.MGN.Types.SourceServerActionDocument
import Amazonka.MGN.Types.SourceServerActionsRequestFilters
import Amazonka.MGN.Types.SsmDocument
import Amazonka.MGN.Types.SsmDocumentType
import Amazonka.MGN.Types.SsmParameterStoreParameter
import Amazonka.MGN.Types.SsmParameterStoreParameterType
import Amazonka.MGN.Types.TargetInstanceTypeRightSizingMethod
import Amazonka.MGN.Types.TemplateActionDocument
import Amazonka.MGN.Types.TemplateActionsRequestFilters
import Amazonka.MGN.Types.VcenterClient
import Amazonka.MGN.Types.VolumeType
import Amazonka.MGN.Types.Wave
import Amazonka.MGN.Types.WaveAggregatedStatus
import Amazonka.MGN.Types.WaveHealthStatus
import Amazonka.MGN.Types.WaveProgressStatus
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-02-26@ of the Amazon Application Migration Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MGN",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mgn",
      Core.signingName = "mgn",
      Core.version = "2020-02-26",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MGN",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Operating denied due to a file permission or access check error.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request could not be completed due to a conflict with the current
-- state of the target resource.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The server encountered an unexpected condition that prevented it from
-- fulfilling the request.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Resource not found exception.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request could not be completed because its exceeded the service
-- quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Reached throttling quota exception.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Uninitialized account exception.
_UninitializedAccountException :: Core.AsError a => Lens.Fold a Core.ServiceError
_UninitializedAccountException =
  Core._MatchServiceError
    defaultService
    "UninitializedAccountException"
    Prelude.. Core.hasStatus 400

-- | Validate exception.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
