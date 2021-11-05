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

    -- ** UpdateLaunchConfiguration
    updateLaunchConfiguration_targetInstanceTypeRightSizingMethod,
    updateLaunchConfiguration_launchDisposition,
    updateLaunchConfiguration_copyTags,
    updateLaunchConfiguration_name,
    updateLaunchConfiguration_licensing,
    updateLaunchConfiguration_copyPrivateIp,
    updateLaunchConfiguration_sourceServerID,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
    launchConfiguration_launchDisposition,
    launchConfiguration_copyTags,
    launchConfiguration_name,
    launchConfiguration_sourceServerID,
    launchConfiguration_licensing,
    launchConfiguration_copyPrivateIp,

    -- ** DescribeReplicationConfigurationTemplates
    describeReplicationConfigurationTemplates_nextToken,
    describeReplicationConfigurationTemplates_maxResults,
    describeReplicationConfigurationTemplates_replicationConfigurationTemplateIDs,
    describeReplicationConfigurationTemplatesResponse_items,
    describeReplicationConfigurationTemplatesResponse_nextToken,
    describeReplicationConfigurationTemplatesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** InitializeService
    initializeServiceResponse_httpStatus,

    -- ** UpdateReplicationConfigurationTemplate
    updateReplicationConfigurationTemplate_createPublicIP,
    updateReplicationConfigurationTemplate_stagingAreaTags,
    updateReplicationConfigurationTemplate_arn,
    updateReplicationConfigurationTemplate_stagingAreaSubnetId,
    updateReplicationConfigurationTemplate_replicationServerInstanceType,
    updateReplicationConfigurationTemplate_ebsEncryption,
    updateReplicationConfigurationTemplate_associateDefaultSecurityGroup,
    updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    updateReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    updateReplicationConfigurationTemplate_defaultLargeStagingDiskType,
    updateReplicationConfigurationTemplate_bandwidthThrottling,
    updateReplicationConfigurationTemplate_dataPlaneRouting,
    updateReplicationConfigurationTemplate_useDedicatedReplicationServer,
    updateReplicationConfigurationTemplate_replicationConfigurationTemplateID,
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

    -- ** DeleteReplicationConfigurationTemplate
    deleteReplicationConfigurationTemplate_replicationConfigurationTemplateID,
    deleteReplicationConfigurationTemplateResponse_httpStatus,

    -- ** CreateReplicationConfigurationTemplate
    createReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    createReplicationConfigurationTemplate_tags,
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

    -- ** DescribeJobLogItems
    describeJobLogItems_nextToken,
    describeJobLogItems_maxResults,
    describeJobLogItems_jobID,
    describeJobLogItemsResponse_items,
    describeJobLogItemsResponse_nextToken,
    describeJobLogItemsResponse_httpStatus,

    -- ** DisconnectFromService
    disconnectFromService_sourceServerID,
    sourceServer_sourceProperties,
    sourceServer_arn,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_isArchived,
    sourceServer_dataReplicationInfo,
    sourceServer_sourceServerID,
    sourceServer_tags,

    -- ** StartTest
    startTest_tags,
    startTest_sourceServerIDs,
    startTestResponse_job,
    startTestResponse_httpStatus,

    -- ** DescribeSourceServers
    describeSourceServers_nextToken,
    describeSourceServers_maxResults,
    describeSourceServers_filters,
    describeSourceServersResponse_items,
    describeSourceServersResponse_nextToken,
    describeSourceServersResponse_httpStatus,

    -- ** DeleteJob
    deleteJob_jobID,
    deleteJobResponse_httpStatus,

    -- ** FinalizeCutover
    finalizeCutover_sourceServerID,
    sourceServer_sourceProperties,
    sourceServer_arn,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_isArchived,
    sourceServer_dataReplicationInfo,
    sourceServer_sourceServerID,
    sourceServer_tags,

    -- ** DescribeJobs
    describeJobs_nextToken,
    describeJobs_maxResults,
    describeJobs_filters,
    describeJobsResponse_items,
    describeJobsResponse_nextToken,
    describeJobsResponse_httpStatus,

    -- ** MarkAsArchived
    markAsArchived_sourceServerID,
    sourceServer_sourceProperties,
    sourceServer_arn,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_isArchived,
    sourceServer_dataReplicationInfo,
    sourceServer_sourceServerID,
    sourceServer_tags,

    -- ** StartCutover
    startCutover_tags,
    startCutover_sourceServerIDs,
    startCutoverResponse_job,
    startCutoverResponse_httpStatus,

    -- ** RetryDataReplication
    retryDataReplication_sourceServerID,
    sourceServer_sourceProperties,
    sourceServer_arn,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_isArchived,
    sourceServer_dataReplicationInfo,
    sourceServer_sourceServerID,
    sourceServer_tags,

    -- ** GetReplicationConfiguration
    getReplicationConfiguration_sourceServerID,
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

    -- ** ChangeServerLifeCycleState
    changeServerLifeCycleState_lifeCycle,
    changeServerLifeCycleState_sourceServerID,
    sourceServer_sourceProperties,
    sourceServer_arn,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_isArchived,
    sourceServer_dataReplicationInfo,
    sourceServer_sourceServerID,
    sourceServer_tags,

    -- ** TerminateTargetInstances
    terminateTargetInstances_tags,
    terminateTargetInstances_sourceServerIDs,
    terminateTargetInstancesResponse_job,
    terminateTargetInstancesResponse_httpStatus,

    -- ** UpdateReplicationConfiguration
    updateReplicationConfiguration_createPublicIP,
    updateReplicationConfiguration_stagingAreaTags,
    updateReplicationConfiguration_stagingAreaSubnetId,
    updateReplicationConfiguration_replicationServerInstanceType,
    updateReplicationConfiguration_ebsEncryption,
    updateReplicationConfiguration_associateDefaultSecurityGroup,
    updateReplicationConfiguration_replicationServersSecurityGroupsIDs,
    updateReplicationConfiguration_ebsEncryptionKeyArn,
    updateReplicationConfiguration_name,
    updateReplicationConfiguration_defaultLargeStagingDiskType,
    updateReplicationConfiguration_bandwidthThrottling,
    updateReplicationConfiguration_dataPlaneRouting,
    updateReplicationConfiguration_replicatedDisks,
    updateReplicationConfiguration_useDedicatedReplicationServer,
    updateReplicationConfiguration_sourceServerID,
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

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** GetLaunchConfiguration
    getLaunchConfiguration_sourceServerID,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
    launchConfiguration_launchDisposition,
    launchConfiguration_copyTags,
    launchConfiguration_name,
    launchConfiguration_sourceServerID,
    launchConfiguration_licensing,
    launchConfiguration_copyPrivateIp,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** DeleteSourceServer
    deleteSourceServer_sourceServerID,
    deleteSourceServerResponse_httpStatus,

    -- * Types

    -- ** CPU
    cpu_modelName,
    cpu_cores,

    -- ** ChangeServerLifeCycleStateSourceServerLifecycle
    changeServerLifeCycleStateSourceServerLifecycle_state,

    -- ** DataReplicationError
    dataReplicationError_rawError,
    dataReplicationError_error,

    -- ** DataReplicationInfo
    dataReplicationInfo_dataReplicationInitiation,
    dataReplicationInfo_dataReplicationError,
    dataReplicationInfo_lagDuration,
    dataReplicationInfo_dataReplicationState,
    dataReplicationInfo_replicatedDisks,
    dataReplicationInfo_etaDateTime,

    -- ** DataReplicationInfoReplicatedDisk
    dataReplicationInfoReplicatedDisk_replicatedStorageBytes,
    dataReplicationInfoReplicatedDisk_backloggedStorageBytes,
    dataReplicationInfoReplicatedDisk_deviceName,
    dataReplicationInfoReplicatedDisk_rescannedStorageBytes,
    dataReplicationInfoReplicatedDisk_totalStorageBytes,

    -- ** DataReplicationInitiation
    dataReplicationInitiation_steps,
    dataReplicationInitiation_nextAttemptDateTime,
    dataReplicationInitiation_startDateTime,

    -- ** DataReplicationInitiationStep
    dataReplicationInitiationStep_status,
    dataReplicationInitiationStep_name,

    -- ** DescribeJobsRequestFilters
    describeJobsRequestFilters_fromDate,
    describeJobsRequestFilters_toDate,
    describeJobsRequestFilters_jobIDs,

    -- ** DescribeSourceServersRequestFilters
    describeSourceServersRequestFilters_sourceServerIDs,
    describeSourceServersRequestFilters_isArchived,

    -- ** Disk
    disk_deviceName,
    disk_bytes,

    -- ** IdentificationHints
    identificationHints_hostname,
    identificationHints_fqdn,
    identificationHints_awsInstanceID,
    identificationHints_vmWareUuid,

    -- ** Job
    job_initiatedBy,
    job_status,
    job_participatingServers,
    job_arn,
    job_creationDateTime,
    job_type,
    job_endDateTime,
    job_tags,
    job_jobID,

    -- ** JobLog
    jobLog_event,
    jobLog_eventData,
    jobLog_logDateTime,

    -- ** JobLogEventData
    jobLogEventData_rawError,
    jobLogEventData_targetInstanceID,
    jobLogEventData_sourceServerID,
    jobLogEventData_conversionServerID,

    -- ** LaunchConfiguration
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
    launchConfiguration_launchDisposition,
    launchConfiguration_copyTags,
    launchConfiguration_name,
    launchConfiguration_sourceServerID,
    launchConfiguration_licensing,
    launchConfiguration_copyPrivateIp,

    -- ** LaunchedInstance
    launchedInstance_jobID,
    launchedInstance_ec2InstanceID,
    launchedInstance_firstBoot,

    -- ** Licensing
    licensing_osByol,

    -- ** LifeCycle
    lifeCycle_lastTest,
    lifeCycle_state,
    lifeCycle_elapsedReplicationDuration,
    lifeCycle_lastSeenByServiceDateTime,
    lifeCycle_addedToServiceDateTime,
    lifeCycle_lastCutover,
    lifeCycle_firstByteDateTime,

    -- ** LifeCycleLastCutover
    lifeCycleLastCutover_initiated,
    lifeCycleLastCutover_reverted,
    lifeCycleLastCutover_finalized,

    -- ** LifeCycleLastCutoverFinalized
    lifeCycleLastCutoverFinalized_apiCallDateTime,

    -- ** LifeCycleLastCutoverInitiated
    lifeCycleLastCutoverInitiated_jobID,
    lifeCycleLastCutoverInitiated_apiCallDateTime,

    -- ** LifeCycleLastCutoverReverted
    lifeCycleLastCutoverReverted_apiCallDateTime,

    -- ** LifeCycleLastTest
    lifeCycleLastTest_initiated,
    lifeCycleLastTest_reverted,
    lifeCycleLastTest_finalized,

    -- ** LifeCycleLastTestFinalized
    lifeCycleLastTestFinalized_apiCallDateTime,

    -- ** LifeCycleLastTestInitiated
    lifeCycleLastTestInitiated_jobID,
    lifeCycleLastTestInitiated_apiCallDateTime,

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

    -- ** ReplicationConfigurationReplicatedDisk
    replicationConfigurationReplicatedDisk_stagingDiskType,
    replicationConfigurationReplicatedDisk_iops,
    replicationConfigurationReplicatedDisk_isBootDisk,
    replicationConfigurationReplicatedDisk_deviceName,

    -- ** ReplicationConfigurationTemplate
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

    -- ** SourceProperties
    sourceProperties_identificationHints,
    sourceProperties_networkInterfaces,
    sourceProperties_lastUpdatedDateTime,
    sourceProperties_recommendedInstanceType,
    sourceProperties_os,
    sourceProperties_ramBytes,
    sourceProperties_cpus,
    sourceProperties_disks,

    -- ** SourceServer
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
