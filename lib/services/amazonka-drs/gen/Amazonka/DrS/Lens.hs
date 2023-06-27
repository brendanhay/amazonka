{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DrS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Lens
  ( -- * Operations

    -- ** AssociateSourceNetworkStack
    associateSourceNetworkStack_cfnStackName,
    associateSourceNetworkStack_sourceNetworkID,
    associateSourceNetworkStackResponse_job,
    associateSourceNetworkStackResponse_httpStatus,

    -- ** CreateExtendedSourceServer
    createExtendedSourceServer_tags,
    createExtendedSourceServer_sourceServerArn,
    createExtendedSourceServerResponse_sourceServer,
    createExtendedSourceServerResponse_httpStatus,

    -- ** CreateLaunchConfigurationTemplate
    createLaunchConfigurationTemplate_copyPrivateIp,
    createLaunchConfigurationTemplate_copyTags,
    createLaunchConfigurationTemplate_exportBucketArn,
    createLaunchConfigurationTemplate_launchDisposition,
    createLaunchConfigurationTemplate_licensing,
    createLaunchConfigurationTemplate_tags,
    createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod,
    createLaunchConfigurationTemplateResponse_launchConfigurationTemplate,
    createLaunchConfigurationTemplateResponse_httpStatus,

    -- ** CreateReplicationConfigurationTemplate
    createReplicationConfigurationTemplate_autoReplicateNewDisks,
    createReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    createReplicationConfigurationTemplate_tags,
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

    -- ** CreateSourceNetwork
    createSourceNetwork_tags,
    createSourceNetwork_originAccountID,
    createSourceNetwork_originRegion,
    createSourceNetwork_vpcID,
    createSourceNetworkResponse_sourceNetworkID,
    createSourceNetworkResponse_httpStatus,

    -- ** DeleteJob
    deleteJob_jobID,
    deleteJobResponse_httpStatus,

    -- ** DeleteLaunchConfigurationTemplate
    deleteLaunchConfigurationTemplate_launchConfigurationTemplateID,
    deleteLaunchConfigurationTemplateResponse_httpStatus,

    -- ** DeleteRecoveryInstance
    deleteRecoveryInstance_recoveryInstanceID,

    -- ** DeleteReplicationConfigurationTemplate
    deleteReplicationConfigurationTemplate_replicationConfigurationTemplateID,
    deleteReplicationConfigurationTemplateResponse_httpStatus,

    -- ** DeleteSourceNetwork
    deleteSourceNetwork_sourceNetworkID,
    deleteSourceNetworkResponse_httpStatus,

    -- ** DeleteSourceServer
    deleteSourceServer_sourceServerID,
    deleteSourceServerResponse_httpStatus,

    -- ** DescribeJobLogItems
    describeJobLogItems_maxResults,
    describeJobLogItems_nextToken,
    describeJobLogItems_jobID,
    describeJobLogItemsResponse_items,
    describeJobLogItemsResponse_nextToken,
    describeJobLogItemsResponse_httpStatus,

    -- ** DescribeJobs
    describeJobs_filters,
    describeJobs_maxResults,
    describeJobs_nextToken,
    describeJobsResponse_items,
    describeJobsResponse_nextToken,
    describeJobsResponse_httpStatus,

    -- ** DescribeLaunchConfigurationTemplates
    describeLaunchConfigurationTemplates_launchConfigurationTemplateIDs,
    describeLaunchConfigurationTemplates_maxResults,
    describeLaunchConfigurationTemplates_nextToken,
    describeLaunchConfigurationTemplatesResponse_items,
    describeLaunchConfigurationTemplatesResponse_nextToken,
    describeLaunchConfigurationTemplatesResponse_httpStatus,

    -- ** DescribeRecoveryInstances
    describeRecoveryInstances_filters,
    describeRecoveryInstances_maxResults,
    describeRecoveryInstances_nextToken,
    describeRecoveryInstancesResponse_items,
    describeRecoveryInstancesResponse_nextToken,
    describeRecoveryInstancesResponse_httpStatus,

    -- ** DescribeRecoverySnapshots
    describeRecoverySnapshots_filters,
    describeRecoverySnapshots_maxResults,
    describeRecoverySnapshots_nextToken,
    describeRecoverySnapshots_order,
    describeRecoverySnapshots_sourceServerID,
    describeRecoverySnapshotsResponse_items,
    describeRecoverySnapshotsResponse_nextToken,
    describeRecoverySnapshotsResponse_httpStatus,

    -- ** DescribeReplicationConfigurationTemplates
    describeReplicationConfigurationTemplates_maxResults,
    describeReplicationConfigurationTemplates_nextToken,
    describeReplicationConfigurationTemplates_replicationConfigurationTemplateIDs,
    describeReplicationConfigurationTemplatesResponse_items,
    describeReplicationConfigurationTemplatesResponse_nextToken,
    describeReplicationConfigurationTemplatesResponse_httpStatus,

    -- ** DescribeSourceNetworks
    describeSourceNetworks_filters,
    describeSourceNetworks_maxResults,
    describeSourceNetworks_nextToken,
    describeSourceNetworksResponse_items,
    describeSourceNetworksResponse_nextToken,
    describeSourceNetworksResponse_httpStatus,

    -- ** DescribeSourceServers
    describeSourceServers_filters,
    describeSourceServers_maxResults,
    describeSourceServers_nextToken,
    describeSourceServersResponse_items,
    describeSourceServersResponse_nextToken,
    describeSourceServersResponse_httpStatus,

    -- ** DisconnectRecoveryInstance
    disconnectRecoveryInstance_recoveryInstanceID,

    -- ** DisconnectSourceServer
    disconnectSourceServer_sourceServerID,
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

    -- ** ExportSourceNetworkCfnTemplate
    exportSourceNetworkCfnTemplate_sourceNetworkID,
    exportSourceNetworkCfnTemplateResponse_s3DestinationUrl,
    exportSourceNetworkCfnTemplateResponse_httpStatus,

    -- ** GetFailbackReplicationConfiguration
    getFailbackReplicationConfiguration_recoveryInstanceID,
    getFailbackReplicationConfigurationResponse_bandwidthThrottling,
    getFailbackReplicationConfigurationResponse_name,
    getFailbackReplicationConfigurationResponse_usePrivateIP,
    getFailbackReplicationConfigurationResponse_httpStatus,
    getFailbackReplicationConfigurationResponse_recoveryInstanceID,

    -- ** GetLaunchConfiguration
    getLaunchConfiguration_sourceServerID,
    launchConfiguration_copyPrivateIp,
    launchConfiguration_copyTags,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_launchDisposition,
    launchConfiguration_licensing,
    launchConfiguration_name,
    launchConfiguration_sourceServerID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,

    -- ** GetReplicationConfiguration
    getReplicationConfiguration_sourceServerID,
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

    -- ** InitializeService
    initializeServiceResponse_httpStatus,

    -- ** ListExtensibleSourceServers
    listExtensibleSourceServers_maxResults,
    listExtensibleSourceServers_nextToken,
    listExtensibleSourceServers_stagingAccountID,
    listExtensibleSourceServersResponse_items,
    listExtensibleSourceServersResponse_nextToken,
    listExtensibleSourceServersResponse_httpStatus,

    -- ** ListStagingAccounts
    listStagingAccounts_maxResults,
    listStagingAccounts_nextToken,
    listStagingAccountsResponse_accounts,
    listStagingAccountsResponse_nextToken,
    listStagingAccountsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ReverseReplication
    reverseReplication_recoveryInstanceID,
    reverseReplicationResponse_reversedDirectionSourceServerArn,
    reverseReplicationResponse_httpStatus,

    -- ** StartFailbackLaunch
    startFailbackLaunch_tags,
    startFailbackLaunch_recoveryInstanceIDs,
    startFailbackLaunchResponse_job,
    startFailbackLaunchResponse_httpStatus,

    -- ** StartRecovery
    startRecovery_isDrill,
    startRecovery_tags,
    startRecovery_sourceServers,
    startRecoveryResponse_job,
    startRecoveryResponse_httpStatus,

    -- ** StartReplication
    startReplication_sourceServerID,
    startReplicationResponse_sourceServer,
    startReplicationResponse_httpStatus,

    -- ** StartSourceNetworkRecovery
    startSourceNetworkRecovery_deployAsNew,
    startSourceNetworkRecovery_tags,
    startSourceNetworkRecovery_sourceNetworks,
    startSourceNetworkRecoveryResponse_job,
    startSourceNetworkRecoveryResponse_httpStatus,

    -- ** StartSourceNetworkReplication
    startSourceNetworkReplication_sourceNetworkID,
    startSourceNetworkReplicationResponse_sourceNetwork,
    startSourceNetworkReplicationResponse_httpStatus,

    -- ** StopFailback
    stopFailback_recoveryInstanceID,

    -- ** StopReplication
    stopReplication_sourceServerID,
    stopReplicationResponse_sourceServer,
    stopReplicationResponse_httpStatus,

    -- ** StopSourceNetworkReplication
    stopSourceNetworkReplication_sourceNetworkID,
    stopSourceNetworkReplicationResponse_sourceNetwork,
    stopSourceNetworkReplicationResponse_httpStatus,

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
    updateLaunchConfiguration_copyPrivateIp,
    updateLaunchConfiguration_copyTags,
    updateLaunchConfiguration_launchDisposition,
    updateLaunchConfiguration_licensing,
    updateLaunchConfiguration_name,
    updateLaunchConfiguration_targetInstanceTypeRightSizingMethod,
    updateLaunchConfiguration_sourceServerID,
    launchConfiguration_copyPrivateIp,
    launchConfiguration_copyTags,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_launchDisposition,
    launchConfiguration_licensing,
    launchConfiguration_name,
    launchConfiguration_sourceServerID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,

    -- ** UpdateLaunchConfigurationTemplate
    updateLaunchConfigurationTemplate_copyPrivateIp,
    updateLaunchConfigurationTemplate_copyTags,
    updateLaunchConfigurationTemplate_exportBucketArn,
    updateLaunchConfigurationTemplate_launchDisposition,
    updateLaunchConfigurationTemplate_licensing,
    updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod,
    updateLaunchConfigurationTemplate_launchConfigurationTemplateID,
    updateLaunchConfigurationTemplateResponse_launchConfigurationTemplate,
    updateLaunchConfigurationTemplateResponse_httpStatus,

    -- ** UpdateReplicationConfiguration
    updateReplicationConfiguration_associateDefaultSecurityGroup,
    updateReplicationConfiguration_autoReplicateNewDisks,
    updateReplicationConfiguration_bandwidthThrottling,
    updateReplicationConfiguration_createPublicIP,
    updateReplicationConfiguration_dataPlaneRouting,
    updateReplicationConfiguration_defaultLargeStagingDiskType,
    updateReplicationConfiguration_ebsEncryption,
    updateReplicationConfiguration_ebsEncryptionKeyArn,
    updateReplicationConfiguration_name,
    updateReplicationConfiguration_pitPolicy,
    updateReplicationConfiguration_replicatedDisks,
    updateReplicationConfiguration_replicationServerInstanceType,
    updateReplicationConfiguration_replicationServersSecurityGroupsIDs,
    updateReplicationConfiguration_stagingAreaSubnetId,
    updateReplicationConfiguration_stagingAreaTags,
    updateReplicationConfiguration_useDedicatedReplicationServer,
    updateReplicationConfiguration_sourceServerID,
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

    -- ** UpdateReplicationConfigurationTemplate
    updateReplicationConfigurationTemplate_arn,
    updateReplicationConfigurationTemplate_associateDefaultSecurityGroup,
    updateReplicationConfigurationTemplate_autoReplicateNewDisks,
    updateReplicationConfigurationTemplate_bandwidthThrottling,
    updateReplicationConfigurationTemplate_createPublicIP,
    updateReplicationConfigurationTemplate_dataPlaneRouting,
    updateReplicationConfigurationTemplate_defaultLargeStagingDiskType,
    updateReplicationConfigurationTemplate_ebsEncryption,
    updateReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    updateReplicationConfigurationTemplate_pitPolicy,
    updateReplicationConfigurationTemplate_replicationServerInstanceType,
    updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    updateReplicationConfigurationTemplate_stagingAreaSubnetId,
    updateReplicationConfigurationTemplate_stagingAreaTags,
    updateReplicationConfigurationTemplate_useDedicatedReplicationServer,
    updateReplicationConfigurationTemplate_replicationConfigurationTemplateID,
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

    -- * Types

    -- ** Account
    account_accountID,

    -- ** CPU
    cpu_cores,
    cpu_modelName,

    -- ** ConversionProperties
    conversionProperties_dataTimestamp,
    conversionProperties_forceUefi,
    conversionProperties_rootVolumeName,
    conversionProperties_volumeToConversionMap,
    conversionProperties_volumeToVolumeSize,

    -- ** DataReplicationError
    dataReplicationError_error,
    dataReplicationError_rawError,

    -- ** DataReplicationInfo
    dataReplicationInfo_dataReplicationError,
    dataReplicationInfo_dataReplicationInitiation,
    dataReplicationInfo_dataReplicationState,
    dataReplicationInfo_etaDateTime,
    dataReplicationInfo_lagDuration,
    dataReplicationInfo_replicatedDisks,
    dataReplicationInfo_stagingAvailabilityZone,

    -- ** DataReplicationInfoReplicatedDisk
    dataReplicationInfoReplicatedDisk_backloggedStorageBytes,
    dataReplicationInfoReplicatedDisk_deviceName,
    dataReplicationInfoReplicatedDisk_replicatedStorageBytes,
    dataReplicationInfoReplicatedDisk_rescannedStorageBytes,
    dataReplicationInfoReplicatedDisk_totalStorageBytes,

    -- ** DataReplicationInitiation
    dataReplicationInitiation_nextAttemptDateTime,
    dataReplicationInitiation_startDateTime,
    dataReplicationInitiation_steps,

    -- ** DataReplicationInitiationStep
    dataReplicationInitiationStep_name,
    dataReplicationInitiationStep_status,

    -- ** DescribeJobsRequestFilters
    describeJobsRequestFilters_fromDate,
    describeJobsRequestFilters_jobIDs,
    describeJobsRequestFilters_toDate,

    -- ** DescribeRecoveryInstancesRequestFilters
    describeRecoveryInstancesRequestFilters_recoveryInstanceIDs,
    describeRecoveryInstancesRequestFilters_sourceServerIDs,

    -- ** DescribeRecoverySnapshotsRequestFilters
    describeRecoverySnapshotsRequestFilters_fromDateTime,
    describeRecoverySnapshotsRequestFilters_toDateTime,

    -- ** DescribeSourceNetworksRequestFilters
    describeSourceNetworksRequestFilters_originAccountID,
    describeSourceNetworksRequestFilters_originRegion,
    describeSourceNetworksRequestFilters_sourceNetworkIDs,

    -- ** DescribeSourceServersRequestFilters
    describeSourceServersRequestFilters_hardwareId,
    describeSourceServersRequestFilters_sourceServerIDs,
    describeSourceServersRequestFilters_stagingAccountIDs,

    -- ** Disk
    disk_bytes,
    disk_deviceName,

    -- ** EventResourceData
    eventResourceData_sourceNetworkData,

    -- ** IdentificationHints
    identificationHints_awsInstanceID,
    identificationHints_fqdn,
    identificationHints_hostname,
    identificationHints_vmWareUuid,

    -- ** Job
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

    -- ** JobLog
    jobLog_event,
    jobLog_eventData,
    jobLog_logDateTime,

    -- ** JobLogEventData
    jobLogEventData_conversionProperties,
    jobLogEventData_conversionServerID,
    jobLogEventData_eventResourceData,
    jobLogEventData_rawError,
    jobLogEventData_sourceServerID,
    jobLogEventData_targetInstanceID,

    -- ** LaunchConfiguration
    launchConfiguration_copyPrivateIp,
    launchConfiguration_copyTags,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_launchDisposition,
    launchConfiguration_licensing,
    launchConfiguration_name,
    launchConfiguration_sourceServerID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,

    -- ** LaunchConfigurationTemplate
    launchConfigurationTemplate_arn,
    launchConfigurationTemplate_copyPrivateIp,
    launchConfigurationTemplate_copyTags,
    launchConfigurationTemplate_exportBucketArn,
    launchConfigurationTemplate_launchConfigurationTemplateID,
    launchConfigurationTemplate_launchDisposition,
    launchConfigurationTemplate_licensing,
    launchConfigurationTemplate_tags,
    launchConfigurationTemplate_targetInstanceTypeRightSizingMethod,

    -- ** Licensing
    licensing_osByol,

    -- ** LifeCycle
    lifeCycle_addedToServiceDateTime,
    lifeCycle_elapsedReplicationDuration,
    lifeCycle_firstByteDateTime,
    lifeCycle_lastLaunch,
    lifeCycle_lastSeenByServiceDateTime,

    -- ** LifeCycleLastLaunch
    lifeCycleLastLaunch_initiated,
    lifeCycleLastLaunch_status,

    -- ** LifeCycleLastLaunchInitiated
    lifeCycleLastLaunchInitiated_apiCallDateTime,
    lifeCycleLastLaunchInitiated_jobID,
    lifeCycleLastLaunchInitiated_type,

    -- ** NetworkInterface
    networkInterface_ips,
    networkInterface_isPrimary,
    networkInterface_macAddress,

    -- ** OS
    os_fullString,

    -- ** PITPolicyRule
    pITPolicyRule_enabled,
    pITPolicyRule_ruleID,
    pITPolicyRule_interval,
    pITPolicyRule_retentionDuration,
    pITPolicyRule_units,

    -- ** ParticipatingResource
    participatingResource_launchStatus,
    participatingResource_participatingResourceID,

    -- ** ParticipatingResourceID
    participatingResourceID_sourceNetworkID,

    -- ** ParticipatingServer
    participatingServer_launchStatus,
    participatingServer_recoveryInstanceID,
    participatingServer_sourceServerID,

    -- ** RecoveryInstance
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

    -- ** RecoveryInstanceDataReplicationError
    recoveryInstanceDataReplicationError_error,
    recoveryInstanceDataReplicationError_rawError,

    -- ** RecoveryInstanceDataReplicationInfo
    recoveryInstanceDataReplicationInfo_dataReplicationError,
    recoveryInstanceDataReplicationInfo_dataReplicationInitiation,
    recoveryInstanceDataReplicationInfo_dataReplicationState,
    recoveryInstanceDataReplicationInfo_etaDateTime,
    recoveryInstanceDataReplicationInfo_lagDuration,
    recoveryInstanceDataReplicationInfo_replicatedDisks,
    recoveryInstanceDataReplicationInfo_stagingAvailabilityZone,

    -- ** RecoveryInstanceDataReplicationInfoReplicatedDisk
    recoveryInstanceDataReplicationInfoReplicatedDisk_backloggedStorageBytes,
    recoveryInstanceDataReplicationInfoReplicatedDisk_deviceName,
    recoveryInstanceDataReplicationInfoReplicatedDisk_replicatedStorageBytes,
    recoveryInstanceDataReplicationInfoReplicatedDisk_rescannedStorageBytes,
    recoveryInstanceDataReplicationInfoReplicatedDisk_totalStorageBytes,

    -- ** RecoveryInstanceDataReplicationInitiation
    recoveryInstanceDataReplicationInitiation_startDateTime,
    recoveryInstanceDataReplicationInitiation_steps,

    -- ** RecoveryInstanceDataReplicationInitiationStep
    recoveryInstanceDataReplicationInitiationStep_name,
    recoveryInstanceDataReplicationInitiationStep_status,

    -- ** RecoveryInstanceDisk
    recoveryInstanceDisk_bytes,
    recoveryInstanceDisk_ebsVolumeID,
    recoveryInstanceDisk_internalDeviceName,

    -- ** RecoveryInstanceFailback
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

    -- ** RecoveryInstanceProperties
    recoveryInstanceProperties_cpus,
    recoveryInstanceProperties_disks,
    recoveryInstanceProperties_identificationHints,
    recoveryInstanceProperties_lastUpdatedDateTime,
    recoveryInstanceProperties_networkInterfaces,
    recoveryInstanceProperties_os,
    recoveryInstanceProperties_ramBytes,

    -- ** RecoveryLifeCycle
    recoveryLifeCycle_apiCallDateTime,
    recoveryLifeCycle_jobID,
    recoveryLifeCycle_lastRecoveryResult,

    -- ** RecoverySnapshot
    recoverySnapshot_ebsSnapshots,
    recoverySnapshot_timestamp,
    recoverySnapshot_expectedTimestamp,
    recoverySnapshot_snapshotID,
    recoverySnapshot_sourceServerID,

    -- ** ReplicationConfiguration
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

    -- ** ReplicationConfigurationReplicatedDisk
    replicationConfigurationReplicatedDisk_deviceName,
    replicationConfigurationReplicatedDisk_iops,
    replicationConfigurationReplicatedDisk_isBootDisk,
    replicationConfigurationReplicatedDisk_optimizedStagingDiskType,
    replicationConfigurationReplicatedDisk_stagingDiskType,
    replicationConfigurationReplicatedDisk_throughput,

    -- ** ReplicationConfigurationTemplate
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

    -- ** SourceCloudProperties
    sourceCloudProperties_originAccountID,
    sourceCloudProperties_originAvailabilityZone,
    sourceCloudProperties_originRegion,

    -- ** SourceNetwork
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

    -- ** SourceNetworkData
    sourceNetworkData_sourceNetworkID,
    sourceNetworkData_sourceVpc,
    sourceNetworkData_stackName,
    sourceNetworkData_targetVpc,

    -- ** SourceProperties
    sourceProperties_cpus,
    sourceProperties_disks,
    sourceProperties_identificationHints,
    sourceProperties_lastUpdatedDateTime,
    sourceProperties_networkInterfaces,
    sourceProperties_os,
    sourceProperties_ramBytes,
    sourceProperties_recommendedInstanceType,
    sourceProperties_supportsNitroInstances,

    -- ** SourceServer
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

    -- ** StagingArea
    stagingArea_errorMessage,
    stagingArea_stagingAccountID,
    stagingArea_stagingSourceServerArn,
    stagingArea_status,

    -- ** StagingSourceServer
    stagingSourceServer_arn,
    stagingSourceServer_hostname,
    stagingSourceServer_tags,

    -- ** StartRecoveryRequestSourceServer
    startRecoveryRequestSourceServer_recoverySnapshotID,
    startRecoveryRequestSourceServer_sourceServerID,

    -- ** StartSourceNetworkRecoveryRequestNetworkEntry
    startSourceNetworkRecoveryRequestNetworkEntry_cfnStackName,
    startSourceNetworkRecoveryRequestNetworkEntry_sourceNetworkID,
  )
where

import Amazonka.DrS.AssociateSourceNetworkStack
import Amazonka.DrS.CreateExtendedSourceServer
import Amazonka.DrS.CreateLaunchConfigurationTemplate
import Amazonka.DrS.CreateReplicationConfigurationTemplate
import Amazonka.DrS.CreateSourceNetwork
import Amazonka.DrS.DeleteJob
import Amazonka.DrS.DeleteLaunchConfigurationTemplate
import Amazonka.DrS.DeleteRecoveryInstance
import Amazonka.DrS.DeleteReplicationConfigurationTemplate
import Amazonka.DrS.DeleteSourceNetwork
import Amazonka.DrS.DeleteSourceServer
import Amazonka.DrS.DescribeJobLogItems
import Amazonka.DrS.DescribeJobs
import Amazonka.DrS.DescribeLaunchConfigurationTemplates
import Amazonka.DrS.DescribeRecoveryInstances
import Amazonka.DrS.DescribeRecoverySnapshots
import Amazonka.DrS.DescribeReplicationConfigurationTemplates
import Amazonka.DrS.DescribeSourceNetworks
import Amazonka.DrS.DescribeSourceServers
import Amazonka.DrS.DisconnectRecoveryInstance
import Amazonka.DrS.DisconnectSourceServer
import Amazonka.DrS.ExportSourceNetworkCfnTemplate
import Amazonka.DrS.GetFailbackReplicationConfiguration
import Amazonka.DrS.GetLaunchConfiguration
import Amazonka.DrS.GetReplicationConfiguration
import Amazonka.DrS.InitializeService
import Amazonka.DrS.ListExtensibleSourceServers
import Amazonka.DrS.ListStagingAccounts
import Amazonka.DrS.ListTagsForResource
import Amazonka.DrS.ReverseReplication
import Amazonka.DrS.StartFailbackLaunch
import Amazonka.DrS.StartRecovery
import Amazonka.DrS.StartReplication
import Amazonka.DrS.StartSourceNetworkRecovery
import Amazonka.DrS.StartSourceNetworkReplication
import Amazonka.DrS.StopFailback
import Amazonka.DrS.StopReplication
import Amazonka.DrS.StopSourceNetworkReplication
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
import Amazonka.DrS.Types.DescribeSourceNetworksRequestFilters
import Amazonka.DrS.Types.DescribeSourceServersRequestFilters
import Amazonka.DrS.Types.Disk
import Amazonka.DrS.Types.EventResourceData
import Amazonka.DrS.Types.IdentificationHints
import Amazonka.DrS.Types.Job
import Amazonka.DrS.Types.JobLog
import Amazonka.DrS.Types.JobLogEventData
import Amazonka.DrS.Types.LaunchConfiguration
import Amazonka.DrS.Types.LaunchConfigurationTemplate
import Amazonka.DrS.Types.Licensing
import Amazonka.DrS.Types.LifeCycle
import Amazonka.DrS.Types.LifeCycleLastLaunch
import Amazonka.DrS.Types.LifeCycleLastLaunchInitiated
import Amazonka.DrS.Types.NetworkInterface
import Amazonka.DrS.Types.OS
import Amazonka.DrS.Types.PITPolicyRule
import Amazonka.DrS.Types.ParticipatingResource
import Amazonka.DrS.Types.ParticipatingResourceID
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
import Amazonka.DrS.Types.RecoveryLifeCycle
import Amazonka.DrS.Types.RecoverySnapshot
import Amazonka.DrS.Types.ReplicationConfiguration
import Amazonka.DrS.Types.ReplicationConfigurationReplicatedDisk
import Amazonka.DrS.Types.ReplicationConfigurationTemplate
import Amazonka.DrS.Types.SourceCloudProperties
import Amazonka.DrS.Types.SourceNetwork
import Amazonka.DrS.Types.SourceNetworkData
import Amazonka.DrS.Types.SourceProperties
import Amazonka.DrS.Types.SourceServer
import Amazonka.DrS.Types.StagingArea
import Amazonka.DrS.Types.StagingSourceServer
import Amazonka.DrS.Types.StartRecoveryRequestSourceServer
import Amazonka.DrS.Types.StartSourceNetworkRecoveryRequestNetworkEntry
import Amazonka.DrS.UntagResource
import Amazonka.DrS.UpdateFailbackReplicationConfiguration
import Amazonka.DrS.UpdateLaunchConfiguration
import Amazonka.DrS.UpdateLaunchConfigurationTemplate
import Amazonka.DrS.UpdateReplicationConfiguration
import Amazonka.DrS.UpdateReplicationConfigurationTemplate
