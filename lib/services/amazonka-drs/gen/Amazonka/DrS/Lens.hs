{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DrS.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Lens
  ( -- * Operations

    -- ** CreateExtendedSourceServer
    createExtendedSourceServer_tags,
    createExtendedSourceServer_sourceServerArn,
    createExtendedSourceServerResponse_sourceServer,
    createExtendedSourceServerResponse_httpStatus,

    -- ** CreateReplicationConfigurationTemplate
    createReplicationConfigurationTemplate_tags,
    createReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    createReplicationConfigurationTemplate_associateDefaultSecurityGroup,
    createReplicationConfigurationTemplate_bandwidthThrottling,
    createReplicationConfigurationTemplate_createPublicIP,
    createReplicationConfigurationTemplate_dataPlaneRouting,
    createReplicationConfigurationTemplate_defaultLargeStagingDiskType,
    createReplicationConfigurationTemplate_ebsEncryption,
    createReplicationConfigurationTemplate_pitPolicy,
    createReplicationConfigurationTemplate_replicationServerInstanceType,
    createReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    createReplicationConfigurationTemplate_stagingAreaSubnetId,
    createReplicationConfigurationTemplate_stagingAreaTags,
    createReplicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_tags,
    replicationConfigurationTemplate_bandwidthThrottling,
    replicationConfigurationTemplate_replicationServerInstanceType,
    replicationConfigurationTemplate_stagingAreaTags,
    replicationConfigurationTemplate_associateDefaultSecurityGroup,
    replicationConfigurationTemplate_defaultLargeStagingDiskType,
    replicationConfigurationTemplate_arn,
    replicationConfigurationTemplate_stagingAreaSubnetId,
    replicationConfigurationTemplate_createPublicIP,
    replicationConfigurationTemplate_dataPlaneRouting,
    replicationConfigurationTemplate_ebsEncryption,
    replicationConfigurationTemplate_pitPolicy,
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- ** DeleteJob
    deleteJob_jobID,
    deleteJobResponse_httpStatus,

    -- ** DeleteRecoveryInstance
    deleteRecoveryInstance_recoveryInstanceID,

    -- ** DeleteReplicationConfigurationTemplate
    deleteReplicationConfigurationTemplate_replicationConfigurationTemplateID,
    deleteReplicationConfigurationTemplateResponse_httpStatus,

    -- ** DeleteSourceServer
    deleteSourceServer_sourceServerID,
    deleteSourceServerResponse_httpStatus,

    -- ** DescribeJobLogItems
    describeJobLogItems_nextToken,
    describeJobLogItems_maxResults,
    describeJobLogItems_jobID,
    describeJobLogItemsResponse_items,
    describeJobLogItemsResponse_nextToken,
    describeJobLogItemsResponse_httpStatus,

    -- ** DescribeJobs
    describeJobs_nextToken,
    describeJobs_filters,
    describeJobs_maxResults,
    describeJobsResponse_items,
    describeJobsResponse_nextToken,
    describeJobsResponse_httpStatus,

    -- ** DescribeRecoveryInstances
    describeRecoveryInstances_nextToken,
    describeRecoveryInstances_filters,
    describeRecoveryInstances_maxResults,
    describeRecoveryInstancesResponse_items,
    describeRecoveryInstancesResponse_nextToken,
    describeRecoveryInstancesResponse_httpStatus,

    -- ** DescribeRecoverySnapshots
    describeRecoverySnapshots_nextToken,
    describeRecoverySnapshots_filters,
    describeRecoverySnapshots_maxResults,
    describeRecoverySnapshots_order,
    describeRecoverySnapshots_sourceServerID,
    describeRecoverySnapshotsResponse_items,
    describeRecoverySnapshotsResponse_nextToken,
    describeRecoverySnapshotsResponse_httpStatus,

    -- ** DescribeReplicationConfigurationTemplates
    describeReplicationConfigurationTemplates_nextToken,
    describeReplicationConfigurationTemplates_replicationConfigurationTemplateIDs,
    describeReplicationConfigurationTemplates_maxResults,
    describeReplicationConfigurationTemplatesResponse_items,
    describeReplicationConfigurationTemplatesResponse_nextToken,
    describeReplicationConfigurationTemplatesResponse_httpStatus,

    -- ** DescribeSourceServers
    describeSourceServers_nextToken,
    describeSourceServers_filters,
    describeSourceServers_maxResults,
    describeSourceServersResponse_items,
    describeSourceServersResponse_nextToken,
    describeSourceServersResponse_httpStatus,

    -- ** DisconnectRecoveryInstance
    disconnectRecoveryInstance_recoveryInstanceID,

    -- ** DisconnectSourceServer
    disconnectSourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_lifeCycle,
    sourceServer_recoveryInstanceId,
    sourceServer_arn,
    sourceServer_lastLaunchResult,
    sourceServer_dataReplicationInfo,
    sourceServer_stagingArea,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,

    -- ** GetFailbackReplicationConfiguration
    getFailbackReplicationConfiguration_recoveryInstanceID,
    getFailbackReplicationConfigurationResponse_bandwidthThrottling,
    getFailbackReplicationConfigurationResponse_name,
    getFailbackReplicationConfigurationResponse_usePrivateIP,
    getFailbackReplicationConfigurationResponse_httpStatus,
    getFailbackReplicationConfigurationResponse_recoveryInstanceID,

    -- ** GetLaunchConfiguration
    getLaunchConfiguration_sourceServerID,
    launchConfiguration_name,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
    launchConfiguration_copyTags,
    launchConfiguration_launchDisposition,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_sourceServerID,
    launchConfiguration_licensing,
    launchConfiguration_copyPrivateIp,

    -- ** GetReplicationConfiguration
    getReplicationConfiguration_sourceServerID,
    replicationConfiguration_bandwidthThrottling,
    replicationConfiguration_name,
    replicationConfiguration_replicationServerInstanceType,
    replicationConfiguration_stagingAreaTags,
    replicationConfiguration_associateDefaultSecurityGroup,
    replicationConfiguration_defaultLargeStagingDiskType,
    replicationConfiguration_stagingAreaSubnetId,
    replicationConfiguration_createPublicIP,
    replicationConfiguration_dataPlaneRouting,
    replicationConfiguration_ebsEncryption,
    replicationConfiguration_replicatedDisks,
    replicationConfiguration_sourceServerID,
    replicationConfiguration_pitPolicy,
    replicationConfiguration_useDedicatedReplicationServer,
    replicationConfiguration_replicationServersSecurityGroupsIDs,
    replicationConfiguration_ebsEncryptionKeyArn,

    -- ** InitializeService
    initializeServiceResponse_httpStatus,

    -- ** ListExtensibleSourceServers
    listExtensibleSourceServers_nextToken,
    listExtensibleSourceServers_maxResults,
    listExtensibleSourceServers_stagingAccountID,
    listExtensibleSourceServersResponse_items,
    listExtensibleSourceServersResponse_nextToken,
    listExtensibleSourceServersResponse_httpStatus,

    -- ** ListStagingAccounts
    listStagingAccounts_nextToken,
    listStagingAccounts_maxResults,
    listStagingAccountsResponse_nextToken,
    listStagingAccountsResponse_accounts,
    listStagingAccountsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RetryDataReplication
    retryDataReplication_sourceServerID,
    sourceServer_tags,
    sourceServer_lifeCycle,
    sourceServer_recoveryInstanceId,
    sourceServer_arn,
    sourceServer_lastLaunchResult,
    sourceServer_dataReplicationInfo,
    sourceServer_stagingArea,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,

    -- ** StartFailbackLaunch
    startFailbackLaunch_tags,
    startFailbackLaunch_recoveryInstanceIDs,
    startFailbackLaunchResponse_job,
    startFailbackLaunchResponse_httpStatus,

    -- ** StartRecovery
    startRecovery_tags,
    startRecovery_isDrill,
    startRecovery_sourceServers,
    startRecoveryResponse_job,
    startRecoveryResponse_httpStatus,

    -- ** StopFailback
    stopFailback_recoveryInstanceID,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** TerminateRecoveryInstances
    terminateRecoveryInstances_recoveryInstanceIDs,
    terminateRecoveryInstancesResponse_job,
    terminateRecoveryInstancesResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateFailbackReplicationConfiguration
    updateFailbackReplicationConfiguration_bandwidthThrottling,
    updateFailbackReplicationConfiguration_name,
    updateFailbackReplicationConfiguration_usePrivateIP,
    updateFailbackReplicationConfiguration_recoveryInstanceID,

    -- ** UpdateLaunchConfiguration
    updateLaunchConfiguration_name,
    updateLaunchConfiguration_targetInstanceTypeRightSizingMethod,
    updateLaunchConfiguration_copyTags,
    updateLaunchConfiguration_launchDisposition,
    updateLaunchConfiguration_licensing,
    updateLaunchConfiguration_copyPrivateIp,
    updateLaunchConfiguration_sourceServerID,
    launchConfiguration_name,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
    launchConfiguration_copyTags,
    launchConfiguration_launchDisposition,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_sourceServerID,
    launchConfiguration_licensing,
    launchConfiguration_copyPrivateIp,

    -- ** UpdateReplicationConfiguration
    updateReplicationConfiguration_bandwidthThrottling,
    updateReplicationConfiguration_name,
    updateReplicationConfiguration_replicationServerInstanceType,
    updateReplicationConfiguration_stagingAreaTags,
    updateReplicationConfiguration_associateDefaultSecurityGroup,
    updateReplicationConfiguration_defaultLargeStagingDiskType,
    updateReplicationConfiguration_stagingAreaSubnetId,
    updateReplicationConfiguration_createPublicIP,
    updateReplicationConfiguration_dataPlaneRouting,
    updateReplicationConfiguration_ebsEncryption,
    updateReplicationConfiguration_replicatedDisks,
    updateReplicationConfiguration_pitPolicy,
    updateReplicationConfiguration_useDedicatedReplicationServer,
    updateReplicationConfiguration_replicationServersSecurityGroupsIDs,
    updateReplicationConfiguration_ebsEncryptionKeyArn,
    updateReplicationConfiguration_sourceServerID,
    replicationConfiguration_bandwidthThrottling,
    replicationConfiguration_name,
    replicationConfiguration_replicationServerInstanceType,
    replicationConfiguration_stagingAreaTags,
    replicationConfiguration_associateDefaultSecurityGroup,
    replicationConfiguration_defaultLargeStagingDiskType,
    replicationConfiguration_stagingAreaSubnetId,
    replicationConfiguration_createPublicIP,
    replicationConfiguration_dataPlaneRouting,
    replicationConfiguration_ebsEncryption,
    replicationConfiguration_replicatedDisks,
    replicationConfiguration_sourceServerID,
    replicationConfiguration_pitPolicy,
    replicationConfiguration_useDedicatedReplicationServer,
    replicationConfiguration_replicationServersSecurityGroupsIDs,
    replicationConfiguration_ebsEncryptionKeyArn,

    -- ** UpdateReplicationConfigurationTemplate
    updateReplicationConfigurationTemplate_bandwidthThrottling,
    updateReplicationConfigurationTemplate_replicationServerInstanceType,
    updateReplicationConfigurationTemplate_stagingAreaTags,
    updateReplicationConfigurationTemplate_associateDefaultSecurityGroup,
    updateReplicationConfigurationTemplate_defaultLargeStagingDiskType,
    updateReplicationConfigurationTemplate_arn,
    updateReplicationConfigurationTemplate_stagingAreaSubnetId,
    updateReplicationConfigurationTemplate_createPublicIP,
    updateReplicationConfigurationTemplate_dataPlaneRouting,
    updateReplicationConfigurationTemplate_ebsEncryption,
    updateReplicationConfigurationTemplate_pitPolicy,
    updateReplicationConfigurationTemplate_useDedicatedReplicationServer,
    updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    updateReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    updateReplicationConfigurationTemplate_replicationConfigurationTemplateID,
    replicationConfigurationTemplate_tags,
    replicationConfigurationTemplate_bandwidthThrottling,
    replicationConfigurationTemplate_replicationServerInstanceType,
    replicationConfigurationTemplate_stagingAreaTags,
    replicationConfigurationTemplate_associateDefaultSecurityGroup,
    replicationConfigurationTemplate_defaultLargeStagingDiskType,
    replicationConfigurationTemplate_arn,
    replicationConfigurationTemplate_stagingAreaSubnetId,
    replicationConfigurationTemplate_createPublicIP,
    replicationConfigurationTemplate_dataPlaneRouting,
    replicationConfigurationTemplate_ebsEncryption,
    replicationConfigurationTemplate_pitPolicy,
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- * Types

    -- ** Account
    account_accountID,

    -- ** CPU
    cpu_cores,
    cpu_modelName,

    -- ** ConversionProperties
    conversionProperties_volumeToVolumeSize,
    conversionProperties_rootVolumeName,
    conversionProperties_dataTimestamp,
    conversionProperties_volumeToConversionMap,
    conversionProperties_forceUefi,

    -- ** DataReplicationError
    dataReplicationError_rawError,
    dataReplicationError_error,

    -- ** DataReplicationInfo
    dataReplicationInfo_dataReplicationError,
    dataReplicationInfo_lagDuration,
    dataReplicationInfo_dataReplicationInitiation,
    dataReplicationInfo_replicatedDisks,
    dataReplicationInfo_dataReplicationState,
    dataReplicationInfo_etaDateTime,

    -- ** DataReplicationInfoReplicatedDisk
    dataReplicationInfoReplicatedDisk_rescannedStorageBytes,
    dataReplicationInfoReplicatedDisk_backloggedStorageBytes,
    dataReplicationInfoReplicatedDisk_deviceName,
    dataReplicationInfoReplicatedDisk_totalStorageBytes,
    dataReplicationInfoReplicatedDisk_replicatedStorageBytes,

    -- ** DataReplicationInitiation
    dataReplicationInitiation_nextAttemptDateTime,
    dataReplicationInitiation_startDateTime,
    dataReplicationInitiation_steps,

    -- ** DataReplicationInitiationStep
    dataReplicationInitiationStep_name,
    dataReplicationInitiationStep_status,

    -- ** DescribeJobsRequestFilters
    describeJobsRequestFilters_toDate,
    describeJobsRequestFilters_fromDate,
    describeJobsRequestFilters_jobIDs,

    -- ** DescribeRecoveryInstancesRequestFilters
    describeRecoveryInstancesRequestFilters_recoveryInstanceIDs,
    describeRecoveryInstancesRequestFilters_sourceServerIDs,

    -- ** DescribeRecoverySnapshotsRequestFilters
    describeRecoverySnapshotsRequestFilters_fromDateTime,
    describeRecoverySnapshotsRequestFilters_toDateTime,

    -- ** DescribeSourceServersRequestFilters
    describeSourceServersRequestFilters_hardwareId,
    describeSourceServersRequestFilters_sourceServerIDs,
    describeSourceServersRequestFilters_stagingAccountIDs,

    -- ** Disk
    disk_bytes,
    disk_deviceName,

    -- ** IdentificationHints
    identificationHints_awsInstanceID,
    identificationHints_fqdn,
    identificationHints_hostname,
    identificationHints_vmWareUuid,

    -- ** Job
    job_tags,
    job_initiatedBy,
    job_type,
    job_creationDateTime,
    job_arn,
    job_status,
    job_participatingServers,
    job_endDateTime,
    job_jobID,

    -- ** JobLog
    jobLog_event,
    jobLog_eventData,
    jobLog_logDateTime,

    -- ** JobLogEventData
    jobLogEventData_targetInstanceID,
    jobLogEventData_rawError,
    jobLogEventData_conversionServerID,
    jobLogEventData_conversionProperties,
    jobLogEventData_sourceServerID,

    -- ** LaunchConfiguration
    launchConfiguration_name,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
    launchConfiguration_copyTags,
    launchConfiguration_launchDisposition,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_sourceServerID,
    launchConfiguration_licensing,
    launchConfiguration_copyPrivateIp,

    -- ** Licensing
    licensing_osByol,

    -- ** LifeCycle
    lifeCycle_addedToServiceDateTime,
    lifeCycle_lastLaunch,
    lifeCycle_elapsedReplicationDuration,
    lifeCycle_lastSeenByServiceDateTime,
    lifeCycle_firstByteDateTime,

    -- ** LifeCycleLastLaunch
    lifeCycleLastLaunch_initiated,

    -- ** LifeCycleLastLaunchInitiated
    lifeCycleLastLaunchInitiated_apiCallDateTime,
    lifeCycleLastLaunchInitiated_type,
    lifeCycleLastLaunchInitiated_jobID,

    -- ** NetworkInterface
    networkInterface_ips,
    networkInterface_macAddress,
    networkInterface_isPrimary,

    -- ** OS
    os_fullString,

    -- ** PITPolicyRule
    pITPolicyRule_ruleID,
    pITPolicyRule_enabled,
    pITPolicyRule_interval,
    pITPolicyRule_retentionDuration,
    pITPolicyRule_units,

    -- ** ParticipatingServer
    participatingServer_launchStatus,
    participatingServer_recoveryInstanceID,
    participatingServer_sourceServerID,

    -- ** RecoveryInstance
    recoveryInstance_tags,
    recoveryInstance_ec2InstanceID,
    recoveryInstance_recoveryInstanceID,
    recoveryInstance_arn,
    recoveryInstance_jobID,
    recoveryInstance_pointInTimeSnapshotDateTime,
    recoveryInstance_ec2InstanceState,
    recoveryInstance_dataReplicationInfo,
    recoveryInstance_recoveryInstanceProperties,
    recoveryInstance_sourceServerID,
    recoveryInstance_failback,
    recoveryInstance_isDrill,

    -- ** RecoveryInstanceDataReplicationError
    recoveryInstanceDataReplicationError_rawError,
    recoveryInstanceDataReplicationError_error,

    -- ** RecoveryInstanceDataReplicationInfo
    recoveryInstanceDataReplicationInfo_dataReplicationError,
    recoveryInstanceDataReplicationInfo_lagDuration,
    recoveryInstanceDataReplicationInfo_dataReplicationInitiation,
    recoveryInstanceDataReplicationInfo_replicatedDisks,
    recoveryInstanceDataReplicationInfo_dataReplicationState,
    recoveryInstanceDataReplicationInfo_etaDateTime,

    -- ** RecoveryInstanceDataReplicationInfoReplicatedDisk
    recoveryInstanceDataReplicationInfoReplicatedDisk_rescannedStorageBytes,
    recoveryInstanceDataReplicationInfoReplicatedDisk_backloggedStorageBytes,
    recoveryInstanceDataReplicationInfoReplicatedDisk_deviceName,
    recoveryInstanceDataReplicationInfoReplicatedDisk_totalStorageBytes,
    recoveryInstanceDataReplicationInfoReplicatedDisk_replicatedStorageBytes,

    -- ** RecoveryInstanceDataReplicationInitiation
    recoveryInstanceDataReplicationInitiation_startDateTime,
    recoveryInstanceDataReplicationInitiation_steps,

    -- ** RecoveryInstanceDataReplicationInitiationStep
    recoveryInstanceDataReplicationInitiationStep_name,
    recoveryInstanceDataReplicationInitiationStep_status,

    -- ** RecoveryInstanceDisk
    recoveryInstanceDisk_bytes,
    recoveryInstanceDisk_internalDeviceName,
    recoveryInstanceDisk_ebsVolumeID,

    -- ** RecoveryInstanceFailback
    recoveryInstanceFailback_failbackToOriginalServer,
    recoveryInstanceFailback_state,
    recoveryInstanceFailback_failbackClientID,
    recoveryInstanceFailback_elapsedReplicationDuration,
    recoveryInstanceFailback_agentLastSeenByServiceDateTime,
    recoveryInstanceFailback_failbackJobID,
    recoveryInstanceFailback_failbackClientLastSeenByServiceDateTime,
    recoveryInstanceFailback_firstByteDateTime,
    recoveryInstanceFailback_failbackInitiationTime,

    -- ** RecoveryInstanceProperties
    recoveryInstanceProperties_os,
    recoveryInstanceProperties_cpus,
    recoveryInstanceProperties_ramBytes,
    recoveryInstanceProperties_disks,
    recoveryInstanceProperties_identificationHints,
    recoveryInstanceProperties_lastUpdatedDateTime,
    recoveryInstanceProperties_networkInterfaces,

    -- ** RecoverySnapshot
    recoverySnapshot_timestamp,
    recoverySnapshot_ebsSnapshots,
    recoverySnapshot_expectedTimestamp,
    recoverySnapshot_snapshotID,
    recoverySnapshot_sourceServerID,

    -- ** ReplicationConfiguration
    replicationConfiguration_bandwidthThrottling,
    replicationConfiguration_name,
    replicationConfiguration_replicationServerInstanceType,
    replicationConfiguration_stagingAreaTags,
    replicationConfiguration_associateDefaultSecurityGroup,
    replicationConfiguration_defaultLargeStagingDiskType,
    replicationConfiguration_stagingAreaSubnetId,
    replicationConfiguration_createPublicIP,
    replicationConfiguration_dataPlaneRouting,
    replicationConfiguration_ebsEncryption,
    replicationConfiguration_replicatedDisks,
    replicationConfiguration_sourceServerID,
    replicationConfiguration_pitPolicy,
    replicationConfiguration_useDedicatedReplicationServer,
    replicationConfiguration_replicationServersSecurityGroupsIDs,
    replicationConfiguration_ebsEncryptionKeyArn,

    -- ** ReplicationConfigurationReplicatedDisk
    replicationConfigurationReplicatedDisk_isBootDisk,
    replicationConfigurationReplicatedDisk_deviceName,
    replicationConfigurationReplicatedDisk_optimizedStagingDiskType,
    replicationConfigurationReplicatedDisk_stagingDiskType,
    replicationConfigurationReplicatedDisk_throughput,
    replicationConfigurationReplicatedDisk_iops,

    -- ** ReplicationConfigurationTemplate
    replicationConfigurationTemplate_tags,
    replicationConfigurationTemplate_bandwidthThrottling,
    replicationConfigurationTemplate_replicationServerInstanceType,
    replicationConfigurationTemplate_stagingAreaTags,
    replicationConfigurationTemplate_associateDefaultSecurityGroup,
    replicationConfigurationTemplate_defaultLargeStagingDiskType,
    replicationConfigurationTemplate_arn,
    replicationConfigurationTemplate_stagingAreaSubnetId,
    replicationConfigurationTemplate_createPublicIP,
    replicationConfigurationTemplate_dataPlaneRouting,
    replicationConfigurationTemplate_ebsEncryption,
    replicationConfigurationTemplate_pitPolicy,
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- ** SourceProperties
    sourceProperties_os,
    sourceProperties_cpus,
    sourceProperties_ramBytes,
    sourceProperties_disks,
    sourceProperties_identificationHints,
    sourceProperties_recommendedInstanceType,
    sourceProperties_lastUpdatedDateTime,
    sourceProperties_networkInterfaces,

    -- ** SourceServer
    sourceServer_tags,
    sourceServer_lifeCycle,
    sourceServer_recoveryInstanceId,
    sourceServer_arn,
    sourceServer_lastLaunchResult,
    sourceServer_dataReplicationInfo,
    sourceServer_stagingArea,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,

    -- ** StagingArea
    stagingArea_stagingAccountID,
    stagingArea_errorMessage,
    stagingArea_stagingSourceServerArn,
    stagingArea_status,

    -- ** StagingSourceServer
    stagingSourceServer_tags,
    stagingSourceServer_arn,
    stagingSourceServer_hostname,

    -- ** StartRecoveryRequestSourceServer
    startRecoveryRequestSourceServer_recoverySnapshotID,
    startRecoveryRequestSourceServer_sourceServerID,
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
import Amazonka.DrS.ListExtensibleSourceServers
import Amazonka.DrS.ListStagingAccounts
import Amazonka.DrS.ListTagsForResource
import Amazonka.DrS.RetryDataReplication
import Amazonka.DrS.StartFailbackLaunch
import Amazonka.DrS.StartRecovery
import Amazonka.DrS.StopFailback
import Amazonka.DrS.TagResource
import Amazonka.DrS.TerminateRecoveryInstances
import Amazonka.DrS.Types.Account
import Amazonka.DrS.Types.CPU
import Amazonka.DrS.Types.ConversionProperties
import Amazonka.DrS.Types.DataReplicationError
import Amazonka.DrS.Types.DataReplicationInfo
import Amazonka.DrS.Types.DataReplicationInfoReplicatedDisk
import Amazonka.DrS.Types.DataReplicationInitiation
import Amazonka.DrS.Types.DataReplicationInitiationStep
import Amazonka.DrS.Types.DescribeJobsRequestFilters
import Amazonka.DrS.Types.DescribeRecoveryInstancesRequestFilters
import Amazonka.DrS.Types.DescribeRecoverySnapshotsRequestFilters
import Amazonka.DrS.Types.DescribeSourceServersRequestFilters
import Amazonka.DrS.Types.Disk
import Amazonka.DrS.Types.IdentificationHints
import Amazonka.DrS.Types.Job
import Amazonka.DrS.Types.JobLog
import Amazonka.DrS.Types.JobLogEventData
import Amazonka.DrS.Types.LaunchConfiguration
import Amazonka.DrS.Types.Licensing
import Amazonka.DrS.Types.LifeCycle
import Amazonka.DrS.Types.LifeCycleLastLaunch
import Amazonka.DrS.Types.LifeCycleLastLaunchInitiated
import Amazonka.DrS.Types.NetworkInterface
import Amazonka.DrS.Types.OS
import Amazonka.DrS.Types.PITPolicyRule
import Amazonka.DrS.Types.ParticipatingServer
import Amazonka.DrS.Types.RecoveryInstance
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationError
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfo
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInfoReplicatedDisk
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiation
import Amazonka.DrS.Types.RecoveryInstanceDataReplicationInitiationStep
import Amazonka.DrS.Types.RecoveryInstanceDisk
import Amazonka.DrS.Types.RecoveryInstanceFailback
import Amazonka.DrS.Types.RecoveryInstanceProperties
import Amazonka.DrS.Types.RecoverySnapshot
import Amazonka.DrS.Types.ReplicationConfiguration
import Amazonka.DrS.Types.ReplicationConfigurationReplicatedDisk
import Amazonka.DrS.Types.ReplicationConfigurationTemplate
import Amazonka.DrS.Types.SourceProperties
import Amazonka.DrS.Types.SourceServer
import Amazonka.DrS.Types.StagingArea
import Amazonka.DrS.Types.StagingSourceServer
import Amazonka.DrS.Types.StartRecoveryRequestSourceServer
import Amazonka.DrS.UntagResource
import Amazonka.DrS.UpdateFailbackReplicationConfiguration
import Amazonka.DrS.UpdateLaunchConfiguration
import Amazonka.DrS.UpdateReplicationConfiguration
import Amazonka.DrS.UpdateReplicationConfigurationTemplate
