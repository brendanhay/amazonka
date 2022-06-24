{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MGN.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Lens
  ( -- * Operations

    -- ** ChangeServerLifeCycleState
    changeServerLifeCycleState_lifeCycle,
    changeServerLifeCycleState_sourceServerID,
    sourceServer_tags,
    sourceServer_lifeCycle,
    sourceServer_launchedInstance,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_isArchived,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,

    -- ** CreateReplicationConfigurationTemplate
    createReplicationConfigurationTemplate_tags,
    createReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    createReplicationConfigurationTemplate_associateDefaultSecurityGroup,
    createReplicationConfigurationTemplate_bandwidthThrottling,
    createReplicationConfigurationTemplate_createPublicIP,
    createReplicationConfigurationTemplate_dataPlaneRouting,
    createReplicationConfigurationTemplate_defaultLargeStagingDiskType,
    createReplicationConfigurationTemplate_ebsEncryption,
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
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- ** DeleteJob
    deleteJob_jobID,
    deleteJobResponse_httpStatus,

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
    describeJobs_maxResults,
    describeJobs_filters,
    describeJobsResponse_items,
    describeJobsResponse_nextToken,
    describeJobsResponse_httpStatus,

    -- ** DescribeReplicationConfigurationTemplates
    describeReplicationConfigurationTemplates_nextToken,
    describeReplicationConfigurationTemplates_maxResults,
    describeReplicationConfigurationTemplates_replicationConfigurationTemplateIDs,
    describeReplicationConfigurationTemplatesResponse_items,
    describeReplicationConfigurationTemplatesResponse_nextToken,
    describeReplicationConfigurationTemplatesResponse_httpStatus,

    -- ** DescribeSourceServers
    describeSourceServers_nextToken,
    describeSourceServers_maxResults,
    describeSourceServers_filters,
    describeSourceServersResponse_items,
    describeSourceServersResponse_nextToken,
    describeSourceServersResponse_httpStatus,

    -- ** DisconnectFromService
    disconnectFromService_sourceServerID,
    sourceServer_tags,
    sourceServer_lifeCycle,
    sourceServer_launchedInstance,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_isArchived,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,

    -- ** FinalizeCutover
    finalizeCutover_sourceServerID,
    sourceServer_tags,
    sourceServer_lifeCycle,
    sourceServer_launchedInstance,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_isArchived,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,

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
    replicationConfiguration_useDedicatedReplicationServer,
    replicationConfiguration_replicationServersSecurityGroupsIDs,
    replicationConfiguration_ebsEncryptionKeyArn,

    -- ** InitializeService
    initializeServiceResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** MarkAsArchived
    markAsArchived_sourceServerID,
    sourceServer_tags,
    sourceServer_lifeCycle,
    sourceServer_launchedInstance,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_isArchived,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,

    -- ** RetryDataReplication
    retryDataReplication_sourceServerID,
    sourceServer_tags,
    sourceServer_lifeCycle,
    sourceServer_launchedInstance,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_isArchived,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,

    -- ** StartCutover
    startCutover_tags,
    startCutover_sourceServerIDs,
    startCutoverResponse_job,
    startCutoverResponse_httpStatus,

    -- ** StartTest
    startTest_tags,
    startTest_sourceServerIDs,
    startTestResponse_job,
    startTestResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** TerminateTargetInstances
    terminateTargetInstances_tags,
    terminateTargetInstances_sourceServerIDs,
    terminateTargetInstancesResponse_job,
    terminateTargetInstancesResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

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
    replicationConfigurationTemplate_useDedicatedReplicationServer,
    replicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    replicationConfigurationTemplate_ebsEncryptionKeyArn,
    replicationConfigurationTemplate_replicationConfigurationTemplateID,

    -- * Types

    -- ** CPU
    cpu_cores,
    cpu_modelName,

    -- ** ChangeServerLifeCycleStateSourceServerLifecycle
    changeServerLifeCycleStateSourceServerLifecycle_state,

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

    -- ** DescribeSourceServersRequestFilters
    describeSourceServersRequestFilters_isArchived,
    describeSourceServersRequestFilters_sourceServerIDs,

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

    -- ** LaunchedInstance
    launchedInstance_ec2InstanceID,
    launchedInstance_jobID,
    launchedInstance_firstBoot,

    -- ** Licensing
    licensing_osByol,

    -- ** LifeCycle
    lifeCycle_lastTest,
    lifeCycle_addedToServiceDateTime,
    lifeCycle_state,
    lifeCycle_elapsedReplicationDuration,
    lifeCycle_lastSeenByServiceDateTime,
    lifeCycle_firstByteDateTime,
    lifeCycle_lastCutover,

    -- ** LifeCycleLastCutover
    lifeCycleLastCutover_reverted,
    lifeCycleLastCutover_finalized,
    lifeCycleLastCutover_initiated,

    -- ** LifeCycleLastCutoverFinalized
    lifeCycleLastCutoverFinalized_apiCallDateTime,

    -- ** LifeCycleLastCutoverInitiated
    lifeCycleLastCutoverInitiated_apiCallDateTime,
    lifeCycleLastCutoverInitiated_jobID,

    -- ** LifeCycleLastCutoverReverted
    lifeCycleLastCutoverReverted_apiCallDateTime,

    -- ** LifeCycleLastTest
    lifeCycleLastTest_reverted,
    lifeCycleLastTest_finalized,
    lifeCycleLastTest_initiated,

    -- ** LifeCycleLastTestFinalized
    lifeCycleLastTestFinalized_apiCallDateTime,

    -- ** LifeCycleLastTestInitiated
    lifeCycleLastTestInitiated_apiCallDateTime,
    lifeCycleLastTestInitiated_jobID,

    -- ** LifeCycleLastTestReverted
    lifeCycleLastTestReverted_apiCallDateTime,

    -- ** NetworkInterface
    networkInterface_ips,
    networkInterface_macAddress,
    networkInterface_isPrimary,

    -- ** OS
    os_fullString,

    -- ** ParticipatingServer
    participatingServer_launchStatus,
    participatingServer_sourceServerID,

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
    replicationConfiguration_useDedicatedReplicationServer,
    replicationConfiguration_replicationServersSecurityGroupsIDs,
    replicationConfiguration_ebsEncryptionKeyArn,

    -- ** ReplicationConfigurationReplicatedDisk
    replicationConfigurationReplicatedDisk_isBootDisk,
    replicationConfigurationReplicatedDisk_deviceName,
    replicationConfigurationReplicatedDisk_stagingDiskType,
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
    sourceServer_launchedInstance,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_isArchived,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,
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
import Amazonka.MGN.ListTagsForResource
import Amazonka.MGN.MarkAsArchived
import Amazonka.MGN.RetryDataReplication
import Amazonka.MGN.StartCutover
import Amazonka.MGN.StartTest
import Amazonka.MGN.TagResource
import Amazonka.MGN.TerminateTargetInstances
import Amazonka.MGN.Types.CPU
import Amazonka.MGN.Types.ChangeServerLifeCycleStateSourceServerLifecycle
import Amazonka.MGN.Types.DataReplicationError
import Amazonka.MGN.Types.DataReplicationInfo
import Amazonka.MGN.Types.DataReplicationInfoReplicatedDisk
import Amazonka.MGN.Types.DataReplicationInitiation
import Amazonka.MGN.Types.DataReplicationInitiationStep
import Amazonka.MGN.Types.DescribeJobsRequestFilters
import Amazonka.MGN.Types.DescribeSourceServersRequestFilters
import Amazonka.MGN.Types.Disk
import Amazonka.MGN.Types.IdentificationHints
import Amazonka.MGN.Types.Job
import Amazonka.MGN.Types.JobLog
import Amazonka.MGN.Types.JobLogEventData
import Amazonka.MGN.Types.LaunchConfiguration
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
import Amazonka.MGN.Types.NetworkInterface
import Amazonka.MGN.Types.OS
import Amazonka.MGN.Types.ParticipatingServer
import Amazonka.MGN.Types.ReplicationConfiguration
import Amazonka.MGN.Types.ReplicationConfigurationReplicatedDisk
import Amazonka.MGN.Types.ReplicationConfigurationTemplate
import Amazonka.MGN.Types.SourceProperties
import Amazonka.MGN.Types.SourceServer
import Amazonka.MGN.UntagResource
import Amazonka.MGN.UpdateLaunchConfiguration
import Amazonka.MGN.UpdateReplicationConfiguration
import Amazonka.MGN.UpdateReplicationConfigurationTemplate
