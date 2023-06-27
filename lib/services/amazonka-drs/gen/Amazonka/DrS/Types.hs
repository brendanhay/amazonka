{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DrS.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types
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

    -- * DataReplicationErrorString
    DataReplicationErrorString (..),

    -- * DataReplicationInitiationStepName
    DataReplicationInitiationStepName (..),

    -- * DataReplicationInitiationStepStatus
    DataReplicationInitiationStepStatus (..),

    -- * DataReplicationState
    DataReplicationState (..),

    -- * EC2InstanceState
    EC2InstanceState (..),

    -- * ExtensionStatus
    ExtensionStatus (..),

    -- * FailbackLaunchType
    FailbackLaunchType (..),

    -- * FailbackReplicationError
    FailbackReplicationError (..),

    -- * FailbackState
    FailbackState (..),

    -- * InitiatedBy
    InitiatedBy (..),

    -- * JobLogEvent
    JobLogEvent (..),

    -- * JobStatus
    JobStatus (..),

    -- * JobType
    JobType (..),

    -- * LastLaunchResult
    LastLaunchResult (..),

    -- * LastLaunchType
    LastLaunchType (..),

    -- * LaunchDisposition
    LaunchDisposition (..),

    -- * LaunchStatus
    LaunchStatus (..),

    -- * OriginEnvironment
    OriginEnvironment (..),

    -- * PITPolicyRuleUnits
    PITPolicyRuleUnits (..),

    -- * RecoveryInstanceDataReplicationInitiationStepName
    RecoveryInstanceDataReplicationInitiationStepName (..),

    -- * RecoveryInstanceDataReplicationInitiationStepStatus
    RecoveryInstanceDataReplicationInitiationStepStatus (..),

    -- * RecoveryInstanceDataReplicationState
    RecoveryInstanceDataReplicationState (..),

    -- * RecoveryResult
    RecoveryResult (..),

    -- * RecoverySnapshotsOrder
    RecoverySnapshotsOrder (..),

    -- * ReplicationConfigurationDataPlaneRouting
    ReplicationConfigurationDataPlaneRouting (..),

    -- * ReplicationConfigurationDefaultLargeStagingDiskType
    ReplicationConfigurationDefaultLargeStagingDiskType (..),

    -- * ReplicationConfigurationEbsEncryption
    ReplicationConfigurationEbsEncryption (..),

    -- * ReplicationConfigurationReplicatedDiskStagingDiskType
    ReplicationConfigurationReplicatedDiskStagingDiskType (..),

    -- * ReplicationDirection
    ReplicationDirection (..),

    -- * ReplicationStatus
    ReplicationStatus (..),

    -- * TargetInstanceTypeRightSizingMethod
    TargetInstanceTypeRightSizingMethod (..),

    -- * Account
    Account (..),
    newAccount,
    account_accountID,

    -- * CPU
    CPU (..),
    newCPU,
    cpu_cores,
    cpu_modelName,

    -- * ConversionProperties
    ConversionProperties (..),
    newConversionProperties,
    conversionProperties_dataTimestamp,
    conversionProperties_forceUefi,
    conversionProperties_rootVolumeName,
    conversionProperties_volumeToConversionMap,
    conversionProperties_volumeToVolumeSize,

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
    dataReplicationInfo_replicatedDisks,
    dataReplicationInfo_stagingAvailabilityZone,

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

    -- * DescribeRecoveryInstancesRequestFilters
    DescribeRecoveryInstancesRequestFilters (..),
    newDescribeRecoveryInstancesRequestFilters,
    describeRecoveryInstancesRequestFilters_recoveryInstanceIDs,
    describeRecoveryInstancesRequestFilters_sourceServerIDs,

    -- * DescribeRecoverySnapshotsRequestFilters
    DescribeRecoverySnapshotsRequestFilters (..),
    newDescribeRecoverySnapshotsRequestFilters,
    describeRecoverySnapshotsRequestFilters_fromDateTime,
    describeRecoverySnapshotsRequestFilters_toDateTime,

    -- * DescribeSourceNetworksRequestFilters
    DescribeSourceNetworksRequestFilters (..),
    newDescribeSourceNetworksRequestFilters,
    describeSourceNetworksRequestFilters_originAccountID,
    describeSourceNetworksRequestFilters_originRegion,
    describeSourceNetworksRequestFilters_sourceNetworkIDs,

    -- * DescribeSourceServersRequestFilters
    DescribeSourceServersRequestFilters (..),
    newDescribeSourceServersRequestFilters,
    describeSourceServersRequestFilters_hardwareId,
    describeSourceServersRequestFilters_sourceServerIDs,
    describeSourceServersRequestFilters_stagingAccountIDs,

    -- * Disk
    Disk (..),
    newDisk,
    disk_bytes,
    disk_deviceName,

    -- * EventResourceData
    EventResourceData (..),
    newEventResourceData,
    eventResourceData_sourceNetworkData,

    -- * IdentificationHints
    IdentificationHints (..),
    newIdentificationHints,
    identificationHints_awsInstanceID,
    identificationHints_fqdn,
    identificationHints_hostname,
    identificationHints_vmWareUuid,

    -- * Job
    Job (..),
    newJob,
    job_arn,
    job_creationDateTime,
    job_endDateTime,
    job_initiatedBy,
    job_participatingResources,
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
    jobLogEventData_conversionProperties,
    jobLogEventData_conversionServerID,
    jobLogEventData_eventResourceData,
    jobLogEventData_rawError,
    jobLogEventData_sourceServerID,
    jobLogEventData_targetInstanceID,

    -- * LaunchConfiguration
    LaunchConfiguration (..),
    newLaunchConfiguration,
    launchConfiguration_copyPrivateIp,
    launchConfiguration_copyTags,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_launchDisposition,
    launchConfiguration_licensing,
    launchConfiguration_name,
    launchConfiguration_sourceServerID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,

    -- * LaunchConfigurationTemplate
    LaunchConfigurationTemplate (..),
    newLaunchConfigurationTemplate,
    launchConfigurationTemplate_arn,
    launchConfigurationTemplate_copyPrivateIp,
    launchConfigurationTemplate_copyTags,
    launchConfigurationTemplate_exportBucketArn,
    launchConfigurationTemplate_launchConfigurationTemplateID,
    launchConfigurationTemplate_launchDisposition,
    launchConfigurationTemplate_licensing,
    launchConfigurationTemplate_tags,
    launchConfigurationTemplate_targetInstanceTypeRightSizingMethod,

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
    lifeCycle_lastLaunch,
    lifeCycle_lastSeenByServiceDateTime,

    -- * LifeCycleLastLaunch
    LifeCycleLastLaunch (..),
    newLifeCycleLastLaunch,
    lifeCycleLastLaunch_initiated,
    lifeCycleLastLaunch_status,

    -- * LifeCycleLastLaunchInitiated
    LifeCycleLastLaunchInitiated (..),
    newLifeCycleLastLaunchInitiated,
    lifeCycleLastLaunchInitiated_apiCallDateTime,
    lifeCycleLastLaunchInitiated_jobID,
    lifeCycleLastLaunchInitiated_type,

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

    -- * PITPolicyRule
    PITPolicyRule (..),
    newPITPolicyRule,
    pITPolicyRule_enabled,
    pITPolicyRule_ruleID,
    pITPolicyRule_interval,
    pITPolicyRule_retentionDuration,
    pITPolicyRule_units,

    -- * ParticipatingResource
    ParticipatingResource (..),
    newParticipatingResource,
    participatingResource_launchStatus,
    participatingResource_participatingResourceID,

    -- * ParticipatingResourceID
    ParticipatingResourceID (..),
    newParticipatingResourceID,
    participatingResourceID_sourceNetworkID,

    -- * ParticipatingServer
    ParticipatingServer (..),
    newParticipatingServer,
    participatingServer_launchStatus,
    participatingServer_recoveryInstanceID,
    participatingServer_sourceServerID,

    -- * RecoveryInstance
    RecoveryInstance (..),
    newRecoveryInstance,
    recoveryInstance_arn,
    recoveryInstance_dataReplicationInfo,
    recoveryInstance_ec2InstanceID,
    recoveryInstance_ec2InstanceState,
    recoveryInstance_failback,
    recoveryInstance_isDrill,
    recoveryInstance_jobID,
    recoveryInstance_originAvailabilityZone,
    recoveryInstance_originEnvironment,
    recoveryInstance_pointInTimeSnapshotDateTime,
    recoveryInstance_recoveryInstanceID,
    recoveryInstance_recoveryInstanceProperties,
    recoveryInstance_sourceServerID,
    recoveryInstance_tags,

    -- * RecoveryInstanceDataReplicationError
    RecoveryInstanceDataReplicationError (..),
    newRecoveryInstanceDataReplicationError,
    recoveryInstanceDataReplicationError_error,
    recoveryInstanceDataReplicationError_rawError,

    -- * RecoveryInstanceDataReplicationInfo
    RecoveryInstanceDataReplicationInfo (..),
    newRecoveryInstanceDataReplicationInfo,
    recoveryInstanceDataReplicationInfo_dataReplicationError,
    recoveryInstanceDataReplicationInfo_dataReplicationInitiation,
    recoveryInstanceDataReplicationInfo_dataReplicationState,
    recoveryInstanceDataReplicationInfo_etaDateTime,
    recoveryInstanceDataReplicationInfo_lagDuration,
    recoveryInstanceDataReplicationInfo_replicatedDisks,
    recoveryInstanceDataReplicationInfo_stagingAvailabilityZone,

    -- * RecoveryInstanceDataReplicationInfoReplicatedDisk
    RecoveryInstanceDataReplicationInfoReplicatedDisk (..),
    newRecoveryInstanceDataReplicationInfoReplicatedDisk,
    recoveryInstanceDataReplicationInfoReplicatedDisk_backloggedStorageBytes,
    recoveryInstanceDataReplicationInfoReplicatedDisk_deviceName,
    recoveryInstanceDataReplicationInfoReplicatedDisk_replicatedStorageBytes,
    recoveryInstanceDataReplicationInfoReplicatedDisk_rescannedStorageBytes,
    recoveryInstanceDataReplicationInfoReplicatedDisk_totalStorageBytes,

    -- * RecoveryInstanceDataReplicationInitiation
    RecoveryInstanceDataReplicationInitiation (..),
    newRecoveryInstanceDataReplicationInitiation,
    recoveryInstanceDataReplicationInitiation_startDateTime,
    recoveryInstanceDataReplicationInitiation_steps,

    -- * RecoveryInstanceDataReplicationInitiationStep
    RecoveryInstanceDataReplicationInitiationStep (..),
    newRecoveryInstanceDataReplicationInitiationStep,
    recoveryInstanceDataReplicationInitiationStep_name,
    recoveryInstanceDataReplicationInitiationStep_status,

    -- * RecoveryInstanceDisk
    RecoveryInstanceDisk (..),
    newRecoveryInstanceDisk,
    recoveryInstanceDisk_bytes,
    recoveryInstanceDisk_ebsVolumeID,
    recoveryInstanceDisk_internalDeviceName,

    -- * RecoveryInstanceFailback
    RecoveryInstanceFailback (..),
    newRecoveryInstanceFailback,
    recoveryInstanceFailback_agentLastSeenByServiceDateTime,
    recoveryInstanceFailback_elapsedReplicationDuration,
    recoveryInstanceFailback_failbackClientID,
    recoveryInstanceFailback_failbackClientLastSeenByServiceDateTime,
    recoveryInstanceFailback_failbackInitiationTime,
    recoveryInstanceFailback_failbackJobID,
    recoveryInstanceFailback_failbackLaunchType,
    recoveryInstanceFailback_failbackToOriginalServer,
    recoveryInstanceFailback_firstByteDateTime,
    recoveryInstanceFailback_state,

    -- * RecoveryInstanceProperties
    RecoveryInstanceProperties (..),
    newRecoveryInstanceProperties,
    recoveryInstanceProperties_cpus,
    recoveryInstanceProperties_disks,
    recoveryInstanceProperties_identificationHints,
    recoveryInstanceProperties_lastUpdatedDateTime,
    recoveryInstanceProperties_networkInterfaces,
    recoveryInstanceProperties_os,
    recoveryInstanceProperties_ramBytes,

    -- * RecoveryLifeCycle
    RecoveryLifeCycle (..),
    newRecoveryLifeCycle,
    recoveryLifeCycle_apiCallDateTime,
    recoveryLifeCycle_jobID,
    recoveryLifeCycle_lastRecoveryResult,

    -- * RecoverySnapshot
    RecoverySnapshot (..),
    newRecoverySnapshot,
    recoverySnapshot_ebsSnapshots,
    recoverySnapshot_timestamp,
    recoverySnapshot_expectedTimestamp,
    recoverySnapshot_snapshotID,
    recoverySnapshot_sourceServerID,

    -- * ReplicationConfiguration
    ReplicationConfiguration (..),
    newReplicationConfiguration,
    replicationConfiguration_associateDefaultSecurityGroup,
    replicationConfiguration_autoReplicateNewDisks,
    replicationConfiguration_bandwidthThrottling,
    replicationConfiguration_createPublicIP,
    replicationConfiguration_dataPlaneRouting,
    replicationConfiguration_defaultLargeStagingDiskType,
    replicationConfiguration_ebsEncryption,
    replicationConfiguration_ebsEncryptionKeyArn,
    replicationConfiguration_name,
    replicationConfiguration_pitPolicy,
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
    replicationConfigurationReplicatedDisk_optimizedStagingDiskType,
    replicationConfigurationReplicatedDisk_stagingDiskType,
    replicationConfigurationReplicatedDisk_throughput,

    -- * ReplicationConfigurationTemplate
    ReplicationConfigurationTemplate (..),
    newReplicationConfigurationTemplate,
    replicationConfigurationTemplate_arn,
    replicationConfigurationTemplate_associateDefaultSecurityGroup,
    replicationConfigurationTemplate_autoReplicateNewDisks,
    replicationConfigurationTemplate_bandwidthThrottling,
    replicationConfigurationTemplate_createPublicIP,
    replicationConfigurationTemplate_dataPlaneRouting,
    replicationConfigurationTemplate_defaultLargeStagingDiskType,
    replicationConfigurationTemplate_ebsEncryption,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_pitPolicy,
    replicationConfigurationTemplate_replicationServerInstanceType,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_stagingAreaSubnetId,
    replicationConfigurationTemplate_stagingAreaTags,
    replicationConfigurationTemplate_tags,
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- * SourceCloudProperties
    SourceCloudProperties (..),
    newSourceCloudProperties,
    sourceCloudProperties_originAccountID,
    sourceCloudProperties_originAvailabilityZone,
    sourceCloudProperties_originRegion,

    -- * SourceNetwork
    SourceNetwork (..),
    newSourceNetwork,
    sourceNetwork_arn,
    sourceNetwork_cfnStackName,
    sourceNetwork_lastRecovery,
    sourceNetwork_launchedVpcID,
    sourceNetwork_replicationStatus,
    sourceNetwork_replicationStatusDetails,
    sourceNetwork_sourceAccountID,
    sourceNetwork_sourceNetworkID,
    sourceNetwork_sourceRegion,
    sourceNetwork_sourceVpcID,
    sourceNetwork_tags,

    -- * SourceNetworkData
    SourceNetworkData (..),
    newSourceNetworkData,
    sourceNetworkData_sourceNetworkID,
    sourceNetworkData_sourceVpc,
    sourceNetworkData_stackName,
    sourceNetworkData_targetVpc,

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
    sourceProperties_supportsNitroInstances,

    -- * SourceServer
    SourceServer (..),
    newSourceServer,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_lastLaunchResult,
    sourceServer_lifeCycle,
    sourceServer_recoveryInstanceId,
    sourceServer_replicationDirection,
    sourceServer_reversedDirectionSourceServerArn,
    sourceServer_sourceCloudProperties,
    sourceServer_sourceNetworkID,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_stagingArea,
    sourceServer_tags,

    -- * StagingArea
    StagingArea (..),
    newStagingArea,
    stagingArea_errorMessage,
    stagingArea_stagingAccountID,
    stagingArea_stagingSourceServerArn,
    stagingArea_status,

    -- * StagingSourceServer
    StagingSourceServer (..),
    newStagingSourceServer,
    stagingSourceServer_arn,
    stagingSourceServer_hostname,
    stagingSourceServer_tags,

    -- * StartRecoveryRequestSourceServer
    StartRecoveryRequestSourceServer (..),
    newStartRecoveryRequestSourceServer,
    startRecoveryRequestSourceServer_recoverySnapshotID,
    startRecoveryRequestSourceServer_sourceServerID,

    -- * StartSourceNetworkRecoveryRequestNetworkEntry
    StartSourceNetworkRecoveryRequestNetworkEntry (..),
    newStartSourceNetworkRecoveryRequestNetworkEntry,
    startSourceNetworkRecoveryRequestNetworkEntry_cfnStackName,
    startSourceNetworkRecoveryRequestNetworkEntry_sourceNetworkID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types.Account
import Amazonka.DrS.Types.CPU
import Amazonka.DrS.Types.ConversionProperties
import Amazonka.DrS.Types.DataReplicationError
import Amazonka.DrS.Types.DataReplicationErrorString
import Amazonka.DrS.Types.DataReplicationInfo
import Amazonka.DrS.Types.DataReplicationInfoReplicatedDisk
import Amazonka.DrS.Types.DataReplicationInitiation
import Amazonka.DrS.Types.DataReplicationInitiationStep
import Amazonka.DrS.Types.DataReplicationInitiationStepName
import Amazonka.DrS.Types.DataReplicationInitiationStepStatus
import Amazonka.DrS.Types.DataReplicationState
import Amazonka.DrS.Types.DescribeJobsRequestFilters
import Amazonka.DrS.Types.DescribeRecoveryInstancesRequestFilters
import Amazonka.DrS.Types.DescribeRecoverySnapshotsRequestFilters
import Amazonka.DrS.Types.DescribeSourceNetworksRequestFilters
import Amazonka.DrS.Types.DescribeSourceServersRequestFilters
import Amazonka.DrS.Types.Disk
import Amazonka.DrS.Types.EC2InstanceState
import Amazonka.DrS.Types.EventResourceData
import Amazonka.DrS.Types.ExtensionStatus
import Amazonka.DrS.Types.FailbackLaunchType
import Amazonka.DrS.Types.FailbackReplicationError
import Amazonka.DrS.Types.FailbackState
import Amazonka.DrS.Types.IdentificationHints
import Amazonka.DrS.Types.InitiatedBy
import Amazonka.DrS.Types.Job
import Amazonka.DrS.Types.JobLog
import Amazonka.DrS.Types.JobLogEvent
import Amazonka.DrS.Types.JobLogEventData
import Amazonka.DrS.Types.JobStatus
import Amazonka.DrS.Types.JobType
import Amazonka.DrS.Types.LastLaunchResult
import Amazonka.DrS.Types.LastLaunchType
import Amazonka.DrS.Types.LaunchConfiguration
import Amazonka.DrS.Types.LaunchConfigurationTemplate
import Amazonka.DrS.Types.LaunchDisposition
import Amazonka.DrS.Types.LaunchStatus
import Amazonka.DrS.Types.Licensing
import Amazonka.DrS.Types.LifeCycle
import Amazonka.DrS.Types.LifeCycleLastLaunch
import Amazonka.DrS.Types.LifeCycleLastLaunchInitiated
import Amazonka.DrS.Types.NetworkInterface
import Amazonka.DrS.Types.OS
import Amazonka.DrS.Types.OriginEnvironment
import Amazonka.DrS.Types.PITPolicyRule
import Amazonka.DrS.Types.PITPolicyRuleUnits
import Amazonka.DrS.Types.ParticipatingResource
import Amazonka.DrS.Types.ParticipatingResourceID
import Amazonka.DrS.Types.ParticipatingServer
import Amazonka.DrS.Types.RecoveryInstance
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationError
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfo
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfoReplicatedDisk
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiation
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStep
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStepName
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStepStatus
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationState
import Amazonka.DrS.Types.RecoveryInstanceDisk
import Amazonka.DrS.Types.RecoveryInstanceFailback
import Amazonka.DrS.Types.RecoveryInstanceProperties
import Amazonka.DrS.Types.RecoveryLifeCycle
import Amazonka.DrS.Types.RecoveryResult
import Amazonka.DrS.Types.RecoverySnapshot
import Amazonka.DrS.Types.RecoverySnapshotsOrder
import Amazonka.DrS.Types.ReplicationConfiguration
import Amazonka.DrS.Types.ReplicationConfigurationDataPlaneRouting
import Amazonka.DrS.Types.ReplicationConfigurationDefaultLargeStagingDiskType
import Amazonka.DrS.Types.ReplicationConfigurationEbsEncryption
import Amazonka.DrS.Types.ReplicationConfigurationReplicatedDisk
import Amazonka.DrS.Types.ReplicationConfigurationReplicatedDiskStagingDiskType
import Amazonka.DrS.Types.ReplicationConfigurationTemplate
import Amazonka.DrS.Types.ReplicationDirection
import Amazonka.DrS.Types.ReplicationStatus
import Amazonka.DrS.Types.SourceCloudProperties
import Amazonka.DrS.Types.SourceNetwork
import Amazonka.DrS.Types.SourceNetworkData
import Amazonka.DrS.Types.SourceProperties
import Amazonka.DrS.Types.SourceServer
import Amazonka.DrS.Types.StagingArea
import Amazonka.DrS.Types.StagingSourceServer
import Amazonka.DrS.Types.StartRecoveryRequestSourceServer
import Amazonka.DrS.Types.StartSourceNetworkRecoveryRequestNetworkEntry
import Amazonka.DrS.Types.TargetInstanceTypeRightSizingMethod
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-02-26@ of the Amazon Elastic Disaster Recovery Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DrS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "drs",
      Core.signingName = "drs",
      Core.version = "2020-02-26",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DrS",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request could not be completed due to a conflict with the current
-- state of the target resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource for this operation was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request could not be completed because its exceeded the service
-- quota.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The account performing the request has not been initialized.
_UninitializedAccountException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UninitializedAccountException =
  Core._MatchServiceError
    defaultService
    "UninitializedAccountException"
    Prelude.. Core.hasStatus 400

-- | The input fails to satisfy the constraints specified by the AWS service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
