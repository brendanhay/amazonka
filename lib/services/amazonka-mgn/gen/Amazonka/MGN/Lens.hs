{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MGN.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Lens
  ( -- * Operations

    -- ** ArchiveApplication
    archiveApplication_applicationID,
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

    -- ** ArchiveWave
    archiveWave_waveID,
    wave_arn,
    wave_creationDateTime,
    wave_description,
    wave_isArchived,
    wave_lastModifiedDateTime,
    wave_name,
    wave_tags,
    wave_waveAggregatedStatus,
    wave_waveID,

    -- ** AssociateApplications
    associateApplications_applicationIDs,
    associateApplications_waveID,
    associateApplicationsResponse_httpStatus,

    -- ** AssociateSourceServers
    associateSourceServers_applicationID,
    associateSourceServers_sourceServerIDs,
    associateSourceServersResponse_httpStatus,

    -- ** ChangeServerLifeCycleState
    changeServerLifeCycleState_lifeCycle,
    changeServerLifeCycleState_sourceServerID,
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_fqdnForActionFramework,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_userProvidedID,
    sourceServer_vcenterClientID,

    -- ** CreateApplication
    createApplication_description,
    createApplication_tags,
    createApplication_name,
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

    -- ** CreateLaunchConfigurationTemplate
    createLaunchConfigurationTemplate_associatePublicIpAddress,
    createLaunchConfigurationTemplate_bootMode,
    createLaunchConfigurationTemplate_copyPrivateIp,
    createLaunchConfigurationTemplate_copyTags,
    createLaunchConfigurationTemplate_enableMapAutoTagging,
    createLaunchConfigurationTemplate_largeVolumeConf,
    createLaunchConfigurationTemplate_launchDisposition,
    createLaunchConfigurationTemplate_licensing,
    createLaunchConfigurationTemplate_mapAutoTaggingMpeID,
    createLaunchConfigurationTemplate_postLaunchActions,
    createLaunchConfigurationTemplate_smallVolumeConf,
    createLaunchConfigurationTemplate_smallVolumeMaxSize,
    createLaunchConfigurationTemplate_tags,
    createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod,
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

    -- ** CreateWave
    createWave_description,
    createWave_tags,
    createWave_name,
    wave_arn,
    wave_creationDateTime,
    wave_description,
    wave_isArchived,
    wave_lastModifiedDateTime,
    wave_name,
    wave_tags,
    wave_waveAggregatedStatus,
    wave_waveID,

    -- ** DeleteApplication
    deleteApplication_applicationID,
    deleteApplicationResponse_httpStatus,

    -- ** DeleteJob
    deleteJob_jobID,
    deleteJobResponse_httpStatus,

    -- ** DeleteLaunchConfigurationTemplate
    deleteLaunchConfigurationTemplate_launchConfigurationTemplateID,
    deleteLaunchConfigurationTemplateResponse_httpStatus,

    -- ** DeleteReplicationConfigurationTemplate
    deleteReplicationConfigurationTemplate_replicationConfigurationTemplateID,
    deleteReplicationConfigurationTemplateResponse_httpStatus,

    -- ** DeleteSourceServer
    deleteSourceServer_sourceServerID,
    deleteSourceServerResponse_httpStatus,

    -- ** DeleteVcenterClient
    deleteVcenterClient_vcenterClientID,

    -- ** DeleteWave
    deleteWave_waveID,
    deleteWaveResponse_httpStatus,

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

    -- ** DescribeReplicationConfigurationTemplates
    describeReplicationConfigurationTemplates_maxResults,
    describeReplicationConfigurationTemplates_nextToken,
    describeReplicationConfigurationTemplates_replicationConfigurationTemplateIDs,
    describeReplicationConfigurationTemplatesResponse_items,
    describeReplicationConfigurationTemplatesResponse_nextToken,
    describeReplicationConfigurationTemplatesResponse_httpStatus,

    -- ** DescribeSourceServers
    describeSourceServers_filters,
    describeSourceServers_maxResults,
    describeSourceServers_nextToken,
    describeSourceServersResponse_items,
    describeSourceServersResponse_nextToken,
    describeSourceServersResponse_httpStatus,

    -- ** DescribeVcenterClients
    describeVcenterClients_maxResults,
    describeVcenterClients_nextToken,
    describeVcenterClientsResponse_items,
    describeVcenterClientsResponse_nextToken,
    describeVcenterClientsResponse_httpStatus,

    -- ** DisassociateApplications
    disassociateApplications_applicationIDs,
    disassociateApplications_waveID,
    disassociateApplicationsResponse_httpStatus,

    -- ** DisassociateSourceServers
    disassociateSourceServers_applicationID,
    disassociateSourceServers_sourceServerIDs,
    disassociateSourceServersResponse_httpStatus,

    -- ** DisconnectFromService
    disconnectFromService_sourceServerID,
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_fqdnForActionFramework,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_userProvidedID,
    sourceServer_vcenterClientID,

    -- ** FinalizeCutover
    finalizeCutover_sourceServerID,
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_fqdnForActionFramework,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_userProvidedID,
    sourceServer_vcenterClientID,

    -- ** GetLaunchConfiguration
    getLaunchConfiguration_sourceServerID,
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

    -- ** GetReplicationConfiguration
    getReplicationConfiguration_sourceServerID,
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

    -- ** InitializeService
    initializeServiceResponse_httpStatus,

    -- ** ListApplications
    listApplications_filters,
    listApplications_maxResults,
    listApplications_nextToken,
    listApplicationsResponse_items,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** ListExportErrors
    listExportErrors_maxResults,
    listExportErrors_nextToken,
    listExportErrors_exportID,
    listExportErrorsResponse_items,
    listExportErrorsResponse_nextToken,
    listExportErrorsResponse_httpStatus,

    -- ** ListExports
    listExports_filters,
    listExports_maxResults,
    listExports_nextToken,
    listExportsResponse_items,
    listExportsResponse_nextToken,
    listExportsResponse_httpStatus,

    -- ** ListImportErrors
    listImportErrors_maxResults,
    listImportErrors_nextToken,
    listImportErrors_importID,
    listImportErrorsResponse_items,
    listImportErrorsResponse_nextToken,
    listImportErrorsResponse_httpStatus,

    -- ** ListImports
    listImports_filters,
    listImports_maxResults,
    listImports_nextToken,
    listImportsResponse_items,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,

    -- ** ListSourceServerActions
    listSourceServerActions_filters,
    listSourceServerActions_maxResults,
    listSourceServerActions_nextToken,
    listSourceServerActions_sourceServerID,
    listSourceServerActionsResponse_items,
    listSourceServerActionsResponse_nextToken,
    listSourceServerActionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTemplateActions
    listTemplateActions_filters,
    listTemplateActions_maxResults,
    listTemplateActions_nextToken,
    listTemplateActions_launchConfigurationTemplateID,
    listTemplateActionsResponse_items,
    listTemplateActionsResponse_nextToken,
    listTemplateActionsResponse_httpStatus,

    -- ** ListWaves
    listWaves_filters,
    listWaves_maxResults,
    listWaves_nextToken,
    listWavesResponse_items,
    listWavesResponse_nextToken,
    listWavesResponse_httpStatus,

    -- ** MarkAsArchived
    markAsArchived_sourceServerID,
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_fqdnForActionFramework,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_userProvidedID,
    sourceServer_vcenterClientID,

    -- ** PutSourceServerAction
    putSourceServerAction_active,
    putSourceServerAction_category,
    putSourceServerAction_description,
    putSourceServerAction_documentVersion,
    putSourceServerAction_externalParameters,
    putSourceServerAction_mustSucceedForCutover,
    putSourceServerAction_parameters,
    putSourceServerAction_timeoutSeconds,
    putSourceServerAction_actionID,
    putSourceServerAction_actionName,
    putSourceServerAction_documentIdentifier,
    putSourceServerAction_order,
    putSourceServerAction_sourceServerID,
    sourceServerActionDocument_actionID,
    sourceServerActionDocument_actionName,
    sourceServerActionDocument_active,
    sourceServerActionDocument_category,
    sourceServerActionDocument_description,
    sourceServerActionDocument_documentIdentifier,
    sourceServerActionDocument_documentVersion,
    sourceServerActionDocument_externalParameters,
    sourceServerActionDocument_mustSucceedForCutover,
    sourceServerActionDocument_order,
    sourceServerActionDocument_parameters,
    sourceServerActionDocument_timeoutSeconds,

    -- ** PutTemplateAction
    putTemplateAction_active,
    putTemplateAction_category,
    putTemplateAction_description,
    putTemplateAction_documentVersion,
    putTemplateAction_externalParameters,
    putTemplateAction_mustSucceedForCutover,
    putTemplateAction_operatingSystem,
    putTemplateAction_parameters,
    putTemplateAction_timeoutSeconds,
    putTemplateAction_actionID,
    putTemplateAction_actionName,
    putTemplateAction_documentIdentifier,
    putTemplateAction_launchConfigurationTemplateID,
    putTemplateAction_order,
    templateActionDocument_actionID,
    templateActionDocument_actionName,
    templateActionDocument_active,
    templateActionDocument_category,
    templateActionDocument_description,
    templateActionDocument_documentIdentifier,
    templateActionDocument_documentVersion,
    templateActionDocument_externalParameters,
    templateActionDocument_mustSucceedForCutover,
    templateActionDocument_operatingSystem,
    templateActionDocument_order,
    templateActionDocument_parameters,
    templateActionDocument_timeoutSeconds,

    -- ** RemoveSourceServerAction
    removeSourceServerAction_actionID,
    removeSourceServerAction_sourceServerID,
    removeSourceServerActionResponse_httpStatus,

    -- ** RemoveTemplateAction
    removeTemplateAction_actionID,
    removeTemplateAction_launchConfigurationTemplateID,
    removeTemplateActionResponse_httpStatus,

    -- ** RetryDataReplication
    retryDataReplication_sourceServerID,
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_fqdnForActionFramework,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_userProvidedID,
    sourceServer_vcenterClientID,

    -- ** StartCutover
    startCutover_tags,
    startCutover_sourceServerIDs,
    startCutoverResponse_job,
    startCutoverResponse_httpStatus,

    -- ** StartExport
    startExport_s3BucketOwner,
    startExport_s3Bucket,
    startExport_s3Key,
    startExportResponse_exportTask,
    startExportResponse_httpStatus,

    -- ** StartImport
    startImport_clientToken,
    startImport_s3BucketSource,
    startImportResponse_importTask,
    startImportResponse_httpStatus,

    -- ** StartReplication
    startReplication_sourceServerID,
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_fqdnForActionFramework,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_userProvidedID,
    sourceServer_vcenterClientID,

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

    -- ** UnarchiveApplication
    unarchiveApplication_applicationID,
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

    -- ** UnarchiveWave
    unarchiveWave_waveID,
    wave_arn,
    wave_creationDateTime,
    wave_description,
    wave_isArchived,
    wave_lastModifiedDateTime,
    wave_name,
    wave_tags,
    wave_waveAggregatedStatus,
    wave_waveID,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateApplication
    updateApplication_description,
    updateApplication_name,
    updateApplication_applicationID,
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

    -- ** UpdateLaunchConfiguration
    updateLaunchConfiguration_bootMode,
    updateLaunchConfiguration_copyPrivateIp,
    updateLaunchConfiguration_copyTags,
    updateLaunchConfiguration_enableMapAutoTagging,
    updateLaunchConfiguration_launchDisposition,
    updateLaunchConfiguration_licensing,
    updateLaunchConfiguration_mapAutoTaggingMpeID,
    updateLaunchConfiguration_name,
    updateLaunchConfiguration_postLaunchActions,
    updateLaunchConfiguration_targetInstanceTypeRightSizingMethod,
    updateLaunchConfiguration_sourceServerID,
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

    -- ** UpdateLaunchConfigurationTemplate
    updateLaunchConfigurationTemplate_associatePublicIpAddress,
    updateLaunchConfigurationTemplate_bootMode,
    updateLaunchConfigurationTemplate_copyPrivateIp,
    updateLaunchConfigurationTemplate_copyTags,
    updateLaunchConfigurationTemplate_enableMapAutoTagging,
    updateLaunchConfigurationTemplate_largeVolumeConf,
    updateLaunchConfigurationTemplate_launchDisposition,
    updateLaunchConfigurationTemplate_licensing,
    updateLaunchConfigurationTemplate_mapAutoTaggingMpeID,
    updateLaunchConfigurationTemplate_postLaunchActions,
    updateLaunchConfigurationTemplate_smallVolumeConf,
    updateLaunchConfigurationTemplate_smallVolumeMaxSize,
    updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod,
    updateLaunchConfigurationTemplate_launchConfigurationTemplateID,
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

    -- ** UpdateReplicationConfiguration
    updateReplicationConfiguration_associateDefaultSecurityGroup,
    updateReplicationConfiguration_bandwidthThrottling,
    updateReplicationConfiguration_createPublicIP,
    updateReplicationConfiguration_dataPlaneRouting,
    updateReplicationConfiguration_defaultLargeStagingDiskType,
    updateReplicationConfiguration_ebsEncryption,
    updateReplicationConfiguration_ebsEncryptionKeyArn,
    updateReplicationConfiguration_name,
    updateReplicationConfiguration_replicatedDisks,
    updateReplicationConfiguration_replicationServerInstanceType,
    updateReplicationConfiguration_replicationServersSecurityGroupsIDs,
    updateReplicationConfiguration_stagingAreaSubnetId,
    updateReplicationConfiguration_stagingAreaTags,
    updateReplicationConfiguration_useDedicatedReplicationServer,
    updateReplicationConfiguration_sourceServerID,
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

    -- ** UpdateReplicationConfigurationTemplate
    updateReplicationConfigurationTemplate_arn,
    updateReplicationConfigurationTemplate_associateDefaultSecurityGroup,
    updateReplicationConfigurationTemplate_bandwidthThrottling,
    updateReplicationConfigurationTemplate_createPublicIP,
    updateReplicationConfigurationTemplate_dataPlaneRouting,
    updateReplicationConfigurationTemplate_defaultLargeStagingDiskType,
    updateReplicationConfigurationTemplate_ebsEncryption,
    updateReplicationConfigurationTemplate_ebsEncryptionKeyArn,
    updateReplicationConfigurationTemplate_replicationServerInstanceType,
    updateReplicationConfigurationTemplate_replicationServersSecurityGroupsIDs,
    updateReplicationConfigurationTemplate_stagingAreaSubnetId,
    updateReplicationConfigurationTemplate_stagingAreaTags,
    updateReplicationConfigurationTemplate_useDedicatedReplicationServer,
    updateReplicationConfigurationTemplate_replicationConfigurationTemplateID,
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

    -- ** UpdateSourceServerReplicationType
    updateSourceServerReplicationType_replicationType,
    updateSourceServerReplicationType_sourceServerID,
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_fqdnForActionFramework,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_userProvidedID,
    sourceServer_vcenterClientID,

    -- ** UpdateWave
    updateWave_description,
    updateWave_name,
    updateWave_waveID,
    wave_arn,
    wave_creationDateTime,
    wave_description,
    wave_isArchived,
    wave_lastModifiedDateTime,
    wave_name,
    wave_tags,
    wave_waveAggregatedStatus,
    wave_waveID,

    -- * Types

    -- ** Application
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

    -- ** ApplicationAggregatedStatus
    applicationAggregatedStatus_healthStatus,
    applicationAggregatedStatus_lastUpdateDateTime,
    applicationAggregatedStatus_progressStatus,
    applicationAggregatedStatus_totalSourceServers,

    -- ** CPU
    cpu_cores,
    cpu_modelName,

    -- ** ChangeServerLifeCycleStateSourceServerLifecycle
    changeServerLifeCycleStateSourceServerLifecycle_state,

    -- ** DataReplicationError
    dataReplicationError_error,
    dataReplicationError_rawError,

    -- ** DataReplicationInfo
    dataReplicationInfo_dataReplicationError,
    dataReplicationInfo_dataReplicationInitiation,
    dataReplicationInfo_dataReplicationState,
    dataReplicationInfo_etaDateTime,
    dataReplicationInfo_lagDuration,
    dataReplicationInfo_lastSnapshotDateTime,
    dataReplicationInfo_replicatedDisks,

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

    -- ** DescribeSourceServersRequestFilters
    describeSourceServersRequestFilters_applicationIDs,
    describeSourceServersRequestFilters_isArchived,
    describeSourceServersRequestFilters_lifeCycleStates,
    describeSourceServersRequestFilters_replicationTypes,
    describeSourceServersRequestFilters_sourceServerIDs,

    -- ** Disk
    disk_bytes,
    disk_deviceName,

    -- ** ExportErrorData
    exportErrorData_rawError,

    -- ** ExportTask
    exportTask_creationDateTime,
    exportTask_endDateTime,
    exportTask_exportID,
    exportTask_progressPercentage,
    exportTask_s3Bucket,
    exportTask_s3BucketOwner,
    exportTask_s3Key,
    exportTask_status,
    exportTask_summary,

    -- ** ExportTaskError
    exportTaskError_errorData,
    exportTaskError_errorDateTime,

    -- ** ExportTaskSummary
    exportTaskSummary_applicationsCount,
    exportTaskSummary_serversCount,
    exportTaskSummary_wavesCount,

    -- ** IdentificationHints
    identificationHints_awsInstanceID,
    identificationHints_fqdn,
    identificationHints_hostname,
    identificationHints_vmPath,
    identificationHints_vmWareUuid,

    -- ** ImportErrorData
    importErrorData_applicationID,
    importErrorData_ec2LaunchTemplateID,
    importErrorData_rawError,
    importErrorData_rowNumber,
    importErrorData_sourceServerID,
    importErrorData_waveID,

    -- ** ImportTask
    importTask_creationDateTime,
    importTask_endDateTime,
    importTask_importID,
    importTask_progressPercentage,
    importTask_s3BucketSource,
    importTask_status,
    importTask_summary,

    -- ** ImportTaskError
    importTaskError_errorData,
    importTaskError_errorDateTime,
    importTaskError_errorType,

    -- ** ImportTaskSummary
    importTaskSummary_applications,
    importTaskSummary_servers,
    importTaskSummary_waves,

    -- ** ImportTaskSummaryApplications
    importTaskSummaryApplications_createdCount,
    importTaskSummaryApplications_modifiedCount,

    -- ** ImportTaskSummaryServers
    importTaskSummaryServers_createdCount,
    importTaskSummaryServers_modifiedCount,

    -- ** ImportTaskSummaryWaves
    importTaskSummaryWaves_createdCount,
    importTaskSummaryWaves_modifiedCount,

    -- ** Job
    job_arn,
    job_creationDateTime,
    job_endDateTime,
    job_initiatedBy,
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
    jobLogEventData_conversionServerID,
    jobLogEventData_rawError,
    jobLogEventData_sourceServerID,
    jobLogEventData_targetInstanceID,

    -- ** JobPostLaunchActionsLaunchStatus
    jobPostLaunchActionsLaunchStatus_executionID,
    jobPostLaunchActionsLaunchStatus_executionStatus,
    jobPostLaunchActionsLaunchStatus_failureReason,
    jobPostLaunchActionsLaunchStatus_ssmDocument,
    jobPostLaunchActionsLaunchStatus_ssmDocumentType,

    -- ** LaunchConfiguration
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

    -- ** LaunchConfigurationTemplate
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

    -- ** LaunchTemplateDiskConf
    launchTemplateDiskConf_iops,
    launchTemplateDiskConf_throughput,
    launchTemplateDiskConf_volumeType,

    -- ** LaunchedInstance
    launchedInstance_ec2InstanceID,
    launchedInstance_firstBoot,
    launchedInstance_jobID,

    -- ** Licensing
    licensing_osByol,

    -- ** LifeCycle
    lifeCycle_addedToServiceDateTime,
    lifeCycle_elapsedReplicationDuration,
    lifeCycle_firstByteDateTime,
    lifeCycle_lastCutover,
    lifeCycle_lastSeenByServiceDateTime,
    lifeCycle_lastTest,
    lifeCycle_state,

    -- ** LifeCycleLastCutover
    lifeCycleLastCutover_finalized,
    lifeCycleLastCutover_initiated,
    lifeCycleLastCutover_reverted,

    -- ** LifeCycleLastCutoverFinalized
    lifeCycleLastCutoverFinalized_apiCallDateTime,

    -- ** LifeCycleLastCutoverInitiated
    lifeCycleLastCutoverInitiated_apiCallDateTime,
    lifeCycleLastCutoverInitiated_jobID,

    -- ** LifeCycleLastCutoverReverted
    lifeCycleLastCutoverReverted_apiCallDateTime,

    -- ** LifeCycleLastTest
    lifeCycleLastTest_finalized,
    lifeCycleLastTest_initiated,
    lifeCycleLastTest_reverted,

    -- ** LifeCycleLastTestFinalized
    lifeCycleLastTestFinalized_apiCallDateTime,

    -- ** LifeCycleLastTestInitiated
    lifeCycleLastTestInitiated_apiCallDateTime,
    lifeCycleLastTestInitiated_jobID,

    -- ** LifeCycleLastTestReverted
    lifeCycleLastTestReverted_apiCallDateTime,

    -- ** ListApplicationsRequestFilters
    listApplicationsRequestFilters_applicationIDs,
    listApplicationsRequestFilters_isArchived,
    listApplicationsRequestFilters_waveIDs,

    -- ** ListExportsRequestFilters
    listExportsRequestFilters_exportIDs,

    -- ** ListImportsRequestFilters
    listImportsRequestFilters_importIDs,

    -- ** ListWavesRequestFilters
    listWavesRequestFilters_isArchived,
    listWavesRequestFilters_waveIDs,

    -- ** NetworkInterface
    networkInterface_ips,
    networkInterface_isPrimary,
    networkInterface_macAddress,

    -- ** OS
    os_fullString,

    -- ** ParticipatingServer
    participatingServer_launchStatus,
    participatingServer_launchedEc2InstanceID,
    participatingServer_postLaunchActionsStatus,
    participatingServer_sourceServerID,

    -- ** PostLaunchActions
    postLaunchActions_cloudWatchLogGroupName,
    postLaunchActions_deployment,
    postLaunchActions_s3LogBucket,
    postLaunchActions_s3OutputKeyPrefix,
    postLaunchActions_ssmDocuments,

    -- ** PostLaunchActionsStatus
    postLaunchActionsStatus_postLaunchActionsLaunchStatusList,
    postLaunchActionsStatus_ssmAgentDiscoveryDatetime,

    -- ** ReplicationConfiguration
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

    -- ** ReplicationConfigurationReplicatedDisk
    replicationConfigurationReplicatedDisk_deviceName,
    replicationConfigurationReplicatedDisk_iops,
    replicationConfigurationReplicatedDisk_isBootDisk,
    replicationConfigurationReplicatedDisk_stagingDiskType,
    replicationConfigurationReplicatedDisk_throughput,

    -- ** ReplicationConfigurationTemplate
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

    -- ** S3BucketSource
    s3BucketSource_s3BucketOwner,
    s3BucketSource_s3Bucket,
    s3BucketSource_s3Key,

    -- ** SourceProperties
    sourceProperties_cpus,
    sourceProperties_disks,
    sourceProperties_identificationHints,
    sourceProperties_lastUpdatedDateTime,
    sourceProperties_networkInterfaces,
    sourceProperties_os,
    sourceProperties_ramBytes,
    sourceProperties_recommendedInstanceType,

    -- ** SourceServer
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_fqdnForActionFramework,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_userProvidedID,
    sourceServer_vcenterClientID,

    -- ** SourceServerActionDocument
    sourceServerActionDocument_actionID,
    sourceServerActionDocument_actionName,
    sourceServerActionDocument_active,
    sourceServerActionDocument_category,
    sourceServerActionDocument_description,
    sourceServerActionDocument_documentIdentifier,
    sourceServerActionDocument_documentVersion,
    sourceServerActionDocument_externalParameters,
    sourceServerActionDocument_mustSucceedForCutover,
    sourceServerActionDocument_order,
    sourceServerActionDocument_parameters,
    sourceServerActionDocument_timeoutSeconds,

    -- ** SourceServerActionsRequestFilters
    sourceServerActionsRequestFilters_actionIDs,

    -- ** SsmDocument
    ssmDocument_externalParameters,
    ssmDocument_mustSucceedForCutover,
    ssmDocument_parameters,
    ssmDocument_timeoutSeconds,
    ssmDocument_actionName,
    ssmDocument_ssmDocumentName,

    -- ** SsmExternalParameter
    ssmExternalParameter_dynamicPath,

    -- ** SsmParameterStoreParameter
    ssmParameterStoreParameter_parameterName,
    ssmParameterStoreParameter_parameterType,

    -- ** TemplateActionDocument
    templateActionDocument_actionID,
    templateActionDocument_actionName,
    templateActionDocument_active,
    templateActionDocument_category,
    templateActionDocument_description,
    templateActionDocument_documentIdentifier,
    templateActionDocument_documentVersion,
    templateActionDocument_externalParameters,
    templateActionDocument_mustSucceedForCutover,
    templateActionDocument_operatingSystem,
    templateActionDocument_order,
    templateActionDocument_parameters,
    templateActionDocument_timeoutSeconds,

    -- ** TemplateActionsRequestFilters
    templateActionsRequestFilters_actionIDs,

    -- ** VcenterClient
    vcenterClient_arn,
    vcenterClient_datacenterName,
    vcenterClient_hostname,
    vcenterClient_lastSeenDatetime,
    vcenterClient_sourceServerTags,
    vcenterClient_tags,
    vcenterClient_vcenterClientID,
    vcenterClient_vcenterUUID,

    -- ** Wave
    wave_arn,
    wave_creationDateTime,
    wave_description,
    wave_isArchived,
    wave_lastModifiedDateTime,
    wave_name,
    wave_tags,
    wave_waveAggregatedStatus,
    wave_waveID,

    -- ** WaveAggregatedStatus
    waveAggregatedStatus_healthStatus,
    waveAggregatedStatus_lastUpdateDateTime,
    waveAggregatedStatus_progressStatus,
    waveAggregatedStatus_replicationStartedDateTime,
    waveAggregatedStatus_totalApplications,
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
import Amazonka.MGN.ListApplications
import Amazonka.MGN.ListExportErrors
import Amazonka.MGN.ListExports
import Amazonka.MGN.ListImportErrors
import Amazonka.MGN.ListImports
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
import Amazonka.MGN.StartExport
import Amazonka.MGN.StartImport
import Amazonka.MGN.StartReplication
import Amazonka.MGN.StartTest
import Amazonka.MGN.TagResource
import Amazonka.MGN.TerminateTargetInstances
import Amazonka.MGN.Types.Application
import Amazonka.MGN.Types.ApplicationAggregatedStatus
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
import Amazonka.MGN.Types.ExportErrorData
import Amazonka.MGN.Types.ExportTask
import Amazonka.MGN.Types.ExportTaskError
import Amazonka.MGN.Types.ExportTaskSummary
import Amazonka.MGN.Types.IdentificationHints
import Amazonka.MGN.Types.ImportErrorData
import Amazonka.MGN.Types.ImportTask
import Amazonka.MGN.Types.ImportTaskError
import Amazonka.MGN.Types.ImportTaskSummary
import Amazonka.MGN.Types.ImportTaskSummaryApplications
import Amazonka.MGN.Types.ImportTaskSummaryServers
import Amazonka.MGN.Types.ImportTaskSummaryWaves
import Amazonka.MGN.Types.Job
import Amazonka.MGN.Types.JobLog
import Amazonka.MGN.Types.JobLogEventData
import Amazonka.MGN.Types.JobPostLaunchActionsLaunchStatus
import Amazonka.MGN.Types.LaunchConfiguration
import Amazonka.MGN.Types.LaunchConfigurationTemplate
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
import Amazonka.MGN.Types.ListApplicationsRequestFilters
import Amazonka.MGN.Types.ListExportsRequestFilters
import Amazonka.MGN.Types.ListImportsRequestFilters
import Amazonka.MGN.Types.ListWavesRequestFilters
import Amazonka.MGN.Types.NetworkInterface
import Amazonka.MGN.Types.OS
import Amazonka.MGN.Types.ParticipatingServer
import Amazonka.MGN.Types.PostLaunchActions
import Amazonka.MGN.Types.PostLaunchActionsStatus
import Amazonka.MGN.Types.ReplicationConfiguration
import Amazonka.MGN.Types.ReplicationConfigurationReplicatedDisk
import Amazonka.MGN.Types.ReplicationConfigurationTemplate
import Amazonka.MGN.Types.S3BucketSource
import Amazonka.MGN.Types.SourceProperties
import Amazonka.MGN.Types.SourceServer
import Amazonka.MGN.Types.SourceServerActionDocument
import Amazonka.MGN.Types.SourceServerActionsRequestFilters
import Amazonka.MGN.Types.SsmDocument
import Amazonka.MGN.Types.SsmExternalParameter
import Amazonka.MGN.Types.SsmParameterStoreParameter
import Amazonka.MGN.Types.TemplateActionDocument
import Amazonka.MGN.Types.TemplateActionsRequestFilters
import Amazonka.MGN.Types.VcenterClient
import Amazonka.MGN.Types.Wave
import Amazonka.MGN.Types.WaveAggregatedStatus
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
