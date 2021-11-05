{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MGN.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Lens
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

import Network.AWS.MGN.ChangeServerLifeCycleState
import Network.AWS.MGN.CreateReplicationConfigurationTemplate
import Network.AWS.MGN.DeleteJob
import Network.AWS.MGN.DeleteReplicationConfigurationTemplate
import Network.AWS.MGN.DeleteSourceServer
import Network.AWS.MGN.DescribeJobLogItems
import Network.AWS.MGN.DescribeJobs
import Network.AWS.MGN.DescribeReplicationConfigurationTemplates
import Network.AWS.MGN.DescribeSourceServers
import Network.AWS.MGN.DisconnectFromService
import Network.AWS.MGN.FinalizeCutover
import Network.AWS.MGN.GetLaunchConfiguration
import Network.AWS.MGN.GetReplicationConfiguration
import Network.AWS.MGN.InitializeService
import Network.AWS.MGN.ListTagsForResource
import Network.AWS.MGN.MarkAsArchived
import Network.AWS.MGN.RetryDataReplication
import Network.AWS.MGN.StartCutover
import Network.AWS.MGN.StartTest
import Network.AWS.MGN.TagResource
import Network.AWS.MGN.TerminateTargetInstances
import Network.AWS.MGN.Types.CPU
import Network.AWS.MGN.Types.ChangeServerLifeCycleStateSourceServerLifecycle
import Network.AWS.MGN.Types.DataReplicationError
import Network.AWS.MGN.Types.DataReplicationInfo
import Network.AWS.MGN.Types.DataReplicationInfoReplicatedDisk
import Network.AWS.MGN.Types.DataReplicationInitiation
import Network.AWS.MGN.Types.DataReplicationInitiationStep
import Network.AWS.MGN.Types.DescribeJobsRequestFilters
import Network.AWS.MGN.Types.DescribeSourceServersRequestFilters
import Network.AWS.MGN.Types.Disk
import Network.AWS.MGN.Types.IdentificationHints
import Network.AWS.MGN.Types.Job
import Network.AWS.MGN.Types.JobLog
import Network.AWS.MGN.Types.JobLogEventData
import Network.AWS.MGN.Types.LaunchConfiguration
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
import Network.AWS.MGN.Types.NetworkInterface
import Network.AWS.MGN.Types.OS
import Network.AWS.MGN.Types.ParticipatingServer
import Network.AWS.MGN.Types.ReplicationConfiguration
import Network.AWS.MGN.Types.ReplicationConfigurationReplicatedDisk
import Network.AWS.MGN.Types.ReplicationConfigurationTemplate
import Network.AWS.MGN.Types.SourceProperties
import Network.AWS.MGN.Types.SourceServer
import Network.AWS.MGN.UntagResource
import Network.AWS.MGN.UpdateLaunchConfiguration
import Network.AWS.MGN.UpdateReplicationConfiguration
import Network.AWS.MGN.UpdateReplicationConfigurationTemplate
