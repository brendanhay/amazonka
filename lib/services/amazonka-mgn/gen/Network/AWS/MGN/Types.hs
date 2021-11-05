{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MGN.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _UninitializedAccountException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

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

    -- * ReplicationConfigurationDataPlaneRouting
    ReplicationConfigurationDataPlaneRouting (..),

    -- * ReplicationConfigurationDefaultLargeStagingDiskType
    ReplicationConfigurationDefaultLargeStagingDiskType (..),

    -- * ReplicationConfigurationEbsEncryption
    ReplicationConfigurationEbsEncryption (..),

    -- * ReplicationConfigurationReplicatedDiskStagingDiskType
    ReplicationConfigurationReplicatedDiskStagingDiskType (..),

    -- * TargetInstanceTypeRightSizingMethod
    TargetInstanceTypeRightSizingMethod (..),

    -- * CPU
    CPU (..),
    newCPU,
    cpu_modelName,
    cpu_cores,

    -- * ChangeServerLifeCycleStateSourceServerLifecycle
    ChangeServerLifeCycleStateSourceServerLifecycle (..),
    newChangeServerLifeCycleStateSourceServerLifecycle,
    changeServerLifeCycleStateSourceServerLifecycle_state,

    -- * DataReplicationError
    DataReplicationError (..),
    newDataReplicationError,
    dataReplicationError_rawError,
    dataReplicationError_error,

    -- * DataReplicationInfo
    DataReplicationInfo (..),
    newDataReplicationInfo,
    dataReplicationInfo_dataReplicationInitiation,
    dataReplicationInfo_dataReplicationError,
    dataReplicationInfo_lagDuration,
    dataReplicationInfo_dataReplicationState,
    dataReplicationInfo_replicatedDisks,
    dataReplicationInfo_etaDateTime,

    -- * DataReplicationInfoReplicatedDisk
    DataReplicationInfoReplicatedDisk (..),
    newDataReplicationInfoReplicatedDisk,
    dataReplicationInfoReplicatedDisk_replicatedStorageBytes,
    dataReplicationInfoReplicatedDisk_backloggedStorageBytes,
    dataReplicationInfoReplicatedDisk_deviceName,
    dataReplicationInfoReplicatedDisk_rescannedStorageBytes,
    dataReplicationInfoReplicatedDisk_totalStorageBytes,

    -- * DataReplicationInitiation
    DataReplicationInitiation (..),
    newDataReplicationInitiation,
    dataReplicationInitiation_steps,
    dataReplicationInitiation_nextAttemptDateTime,
    dataReplicationInitiation_startDateTime,

    -- * DataReplicationInitiationStep
    DataReplicationInitiationStep (..),
    newDataReplicationInitiationStep,
    dataReplicationInitiationStep_status,
    dataReplicationInitiationStep_name,

    -- * DescribeJobsRequestFilters
    DescribeJobsRequestFilters (..),
    newDescribeJobsRequestFilters,
    describeJobsRequestFilters_fromDate,
    describeJobsRequestFilters_toDate,
    describeJobsRequestFilters_jobIDs,

    -- * DescribeSourceServersRequestFilters
    DescribeSourceServersRequestFilters (..),
    newDescribeSourceServersRequestFilters,
    describeSourceServersRequestFilters_sourceServerIDs,
    describeSourceServersRequestFilters_isArchived,

    -- * Disk
    Disk (..),
    newDisk,
    disk_deviceName,
    disk_bytes,

    -- * IdentificationHints
    IdentificationHints (..),
    newIdentificationHints,
    identificationHints_hostname,
    identificationHints_fqdn,
    identificationHints_awsInstanceID,
    identificationHints_vmWareUuid,

    -- * Job
    Job (..),
    newJob,
    job_initiatedBy,
    job_status,
    job_participatingServers,
    job_arn,
    job_creationDateTime,
    job_type,
    job_endDateTime,
    job_tags,
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
    jobLogEventData_rawError,
    jobLogEventData_targetInstanceID,
    jobLogEventData_sourceServerID,
    jobLogEventData_conversionServerID,

    -- * LaunchConfiguration
    LaunchConfiguration (..),
    newLaunchConfiguration,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
    launchConfiguration_launchDisposition,
    launchConfiguration_copyTags,
    launchConfiguration_name,
    launchConfiguration_sourceServerID,
    launchConfiguration_licensing,
    launchConfiguration_copyPrivateIp,

    -- * LaunchedInstance
    LaunchedInstance (..),
    newLaunchedInstance,
    launchedInstance_jobID,
    launchedInstance_ec2InstanceID,
    launchedInstance_firstBoot,

    -- * Licensing
    Licensing (..),
    newLicensing,
    licensing_osByol,

    -- * LifeCycle
    LifeCycle (..),
    newLifeCycle,
    lifeCycle_lastTest,
    lifeCycle_state,
    lifeCycle_elapsedReplicationDuration,
    lifeCycle_lastSeenByServiceDateTime,
    lifeCycle_addedToServiceDateTime,
    lifeCycle_lastCutover,
    lifeCycle_firstByteDateTime,

    -- * LifeCycleLastCutover
    LifeCycleLastCutover (..),
    newLifeCycleLastCutover,
    lifeCycleLastCutover_initiated,
    lifeCycleLastCutover_reverted,
    lifeCycleLastCutover_finalized,

    -- * LifeCycleLastCutoverFinalized
    LifeCycleLastCutoverFinalized (..),
    newLifeCycleLastCutoverFinalized,
    lifeCycleLastCutoverFinalized_apiCallDateTime,

    -- * LifeCycleLastCutoverInitiated
    LifeCycleLastCutoverInitiated (..),
    newLifeCycleLastCutoverInitiated,
    lifeCycleLastCutoverInitiated_jobID,
    lifeCycleLastCutoverInitiated_apiCallDateTime,

    -- * LifeCycleLastCutoverReverted
    LifeCycleLastCutoverReverted (..),
    newLifeCycleLastCutoverReverted,
    lifeCycleLastCutoverReverted_apiCallDateTime,

    -- * LifeCycleLastTest
    LifeCycleLastTest (..),
    newLifeCycleLastTest,
    lifeCycleLastTest_initiated,
    lifeCycleLastTest_reverted,
    lifeCycleLastTest_finalized,

    -- * LifeCycleLastTestFinalized
    LifeCycleLastTestFinalized (..),
    newLifeCycleLastTestFinalized,
    lifeCycleLastTestFinalized_apiCallDateTime,

    -- * LifeCycleLastTestInitiated
    LifeCycleLastTestInitiated (..),
    newLifeCycleLastTestInitiated,
    lifeCycleLastTestInitiated_jobID,
    lifeCycleLastTestInitiated_apiCallDateTime,

    -- * LifeCycleLastTestReverted
    LifeCycleLastTestReverted (..),
    newLifeCycleLastTestReverted,
    lifeCycleLastTestReverted_apiCallDateTime,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_ips,
    networkInterface_macAddress,
    networkInterface_isPrimary,

    -- * OS
    OS (..),
    newOS,
    os_fullString,

    -- * ParticipatingServer
    ParticipatingServer (..),
    newParticipatingServer,
    participatingServer_launchStatus,
    participatingServer_sourceServerID,

    -- * ReplicationConfiguration
    ReplicationConfiguration (..),
    newReplicationConfiguration,
    replicationConfiguration_createPublicIP,
    replicationConfiguration_stagingAreaTags,
    replicationConfiguration_stagingAreaSubnetId,
    replicationConfiguration_replicationServerInstanceType,
    replicationConfiguration_ebsEncryption,
    replicationConfiguration_associateDefaultSecurityGroup,
    replicationConfiguration_replicationServersSecurityGroupsIDs,
    replicationConfiguration_ebsEncryptionKeyArn,
    replicationConfiguration_name,
    replicationConfiguration_defaultLargeStagingDiskType,
    replicationConfiguration_bandwidthThrottling,
    replicationConfiguration_dataPlaneRouting,
    replicationConfiguration_sourceServerID,
    replicationConfiguration_replicatedDisks,
    replicationConfiguration_useDedicatedReplicationServer,

    -- * ReplicationConfigurationReplicatedDisk
    ReplicationConfigurationReplicatedDisk (..),
    newReplicationConfigurationReplicatedDisk,
    replicationConfigurationReplicatedDisk_stagingDiskType,
    replicationConfigurationReplicatedDisk_iops,
    replicationConfigurationReplicatedDisk_isBootDisk,
    replicationConfigurationReplicatedDisk_deviceName,

    -- * ReplicationConfigurationTemplate
    ReplicationConfigurationTemplate (..),
    newReplicationConfigurationTemplate,
    replicationConfigurationTemplate_createPublicIP,
    replicationConfigurationTemplate_stagingAreaTags,
    replicationConfigurationTemplate_arn,
    replicationConfigurationTemplate_stagingAreaSubnetId,
    replicationConfigurationTemplate_replicationServerInstanceType,
    replicationConfigurationTemplate_ebsEncryption,
    replicationConfigurationTemplate_associateDefaultSecurityGroup,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_defaultLargeStagingDiskType,
    replicationConfigurationTemplate_bandwidthThrottling,
    replicationConfigurationTemplate_dataPlaneRouting,
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_tags,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- * SourceProperties
    SourceProperties (..),
    newSourceProperties,
    sourceProperties_identificationHints,
    sourceProperties_networkInterfaces,
    sourceProperties_lastUpdatedDateTime,
    sourceProperties_recommendedInstanceType,
    sourceProperties_os,
    sourceProperties_ramBytes,
    sourceProperties_cpus,
    sourceProperties_disks,

    -- * SourceServer
    SourceServer (..),
    newSourceServer,
    sourceServer_sourceProperties,
    sourceServer_arn,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_isArchived,
    sourceServer_dataReplicationInfo,
    sourceServer_sourceServerID,
    sourceServer_tags,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MGN.Types.CPU
import Network.AWS.MGN.Types.ChangeServerLifeCycleStateSourceServerLifecycle
import Network.AWS.MGN.Types.ChangeServerLifeCycleStateSourceServerLifecycleState
import Network.AWS.MGN.Types.DataReplicationError
import Network.AWS.MGN.Types.DataReplicationErrorString
import Network.AWS.MGN.Types.DataReplicationInfo
import Network.AWS.MGN.Types.DataReplicationInfoReplicatedDisk
import Network.AWS.MGN.Types.DataReplicationInitiation
import Network.AWS.MGN.Types.DataReplicationInitiationStep
import Network.AWS.MGN.Types.DataReplicationInitiationStepName
import Network.AWS.MGN.Types.DataReplicationInitiationStepStatus
import Network.AWS.MGN.Types.DataReplicationState
import Network.AWS.MGN.Types.DescribeJobsRequestFilters
import Network.AWS.MGN.Types.DescribeSourceServersRequestFilters
import Network.AWS.MGN.Types.Disk
import Network.AWS.MGN.Types.FirstBoot
import Network.AWS.MGN.Types.IdentificationHints
import Network.AWS.MGN.Types.InitiatedBy
import Network.AWS.MGN.Types.Job
import Network.AWS.MGN.Types.JobLog
import Network.AWS.MGN.Types.JobLogEvent
import Network.AWS.MGN.Types.JobLogEventData
import Network.AWS.MGN.Types.JobStatus
import Network.AWS.MGN.Types.JobType
import Network.AWS.MGN.Types.LaunchConfiguration
import Network.AWS.MGN.Types.LaunchDisposition
import Network.AWS.MGN.Types.LaunchStatus
import Network.AWS.MGN.Types.LaunchedInstance
import Network.AWS.MGN.Types.Licensing
import Network.AWS.MGN.Types.LifeCycle
import Network.AWS.MGN.Types.LifeCycleLastCutover
import Network.AWS.MGN.Types.LifeCycleLastCutoverFinalized
import Network.AWS.MGN.Types.LifeCycleLastCutoverInitiated
import Network.AWS.MGN.Types.LifeCycleLastCutoverReverted
import Network.AWS.MGN.Types.LifeCycleLastTest
import Network.AWS.MGN.Types.LifeCycleLastTestFinalized
import Network.AWS.MGN.Types.LifeCycleLastTestInitiated
import Network.AWS.MGN.Types.LifeCycleLastTestReverted
import Network.AWS.MGN.Types.LifeCycleState
import Network.AWS.MGN.Types.NetworkInterface
import Network.AWS.MGN.Types.OS
import Network.AWS.MGN.Types.ParticipatingServer
import Network.AWS.MGN.Types.ReplicationConfiguration
import Network.AWS.MGN.Types.ReplicationConfigurationDataPlaneRouting
import Network.AWS.MGN.Types.ReplicationConfigurationDefaultLargeStagingDiskType
import Network.AWS.MGN.Types.ReplicationConfigurationEbsEncryption
import Network.AWS.MGN.Types.ReplicationConfigurationReplicatedDisk
import Network.AWS.MGN.Types.ReplicationConfigurationReplicatedDiskStagingDiskType
import Network.AWS.MGN.Types.ReplicationConfigurationTemplate
import Network.AWS.MGN.Types.SourceProperties
import Network.AWS.MGN.Types.SourceServer
import Network.AWS.MGN.Types.TargetInstanceTypeRightSizingMethod
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-02-26@ of the Amazon Application Migration Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MGN",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "mgn",
      Core._serviceSigningName = "mgn",
      Core._serviceVersion = "2020-02-26",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "MGN",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Validate exception.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Operating denied due to a file permission or access check error.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request could not be completed due to a conflict with the current
-- state of the target resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Unitialized account exception.
_UninitializedAccountException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UninitializedAccountException =
  Core._MatchServiceError
    defaultService
    "UninitializedAccountException"
    Prelude.. Core.hasStatus 400

-- | Reached throttling quota exception.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The server encountered an unexpected condition that prevented it from
-- fulfilling the request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Resource not found exception.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
