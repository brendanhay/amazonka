{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.M2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Lens
  ( -- * Operations

    -- ** CancelBatchJobExecution
    cancelBatchJobExecution_applicationId,
    cancelBatchJobExecution_executionId,
    cancelBatchJobExecutionResponse_httpStatus,

    -- ** CreateApplication
    createApplication_tags,
    createApplication_clientToken,
    createApplication_description,
    createApplication_definition,
    createApplication_engineType,
    createApplication_name,
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationArn,
    createApplicationResponse_applicationId,
    createApplicationResponse_applicationVersion,

    -- ** CreateDataSetImportTask
    createDataSetImportTask_clientToken,
    createDataSetImportTask_applicationId,
    createDataSetImportTask_importConfig,
    createDataSetImportTaskResponse_httpStatus,
    createDataSetImportTaskResponse_taskId,

    -- ** CreateDeployment
    createDeployment_clientToken,
    createDeployment_applicationId,
    createDeployment_applicationVersion,
    createDeployment_environmentId,
    createDeploymentResponse_httpStatus,
    createDeploymentResponse_deploymentId,

    -- ** CreateEnvironment
    createEnvironment_tags,
    createEnvironment_clientToken,
    createEnvironment_securityGroupIds,
    createEnvironment_storageConfigurations,
    createEnvironment_description,
    createEnvironment_publiclyAccessible,
    createEnvironment_preferredMaintenanceWindow,
    createEnvironment_highAvailabilityConfig,
    createEnvironment_subnetIds,
    createEnvironment_engineVersion,
    createEnvironment_engineType,
    createEnvironment_instanceType,
    createEnvironment_name,
    createEnvironmentResponse_httpStatus,
    createEnvironmentResponse_environmentId,

    -- ** DeleteApplication
    deleteApplication_applicationId,
    deleteApplicationResponse_httpStatus,

    -- ** DeleteApplicationFromEnvironment
    deleteApplicationFromEnvironment_applicationId,
    deleteApplicationFromEnvironment_environmentId,
    deleteApplicationFromEnvironmentResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_environmentId,
    deleteEnvironmentResponse_httpStatus,

    -- ** GetApplication
    getApplication_applicationId,
    getApplicationResponse_tags,
    getApplicationResponse_listenerArns,
    getApplicationResponse_deployedVersion,
    getApplicationResponse_lastStartTime,
    getApplicationResponse_statusReason,
    getApplicationResponse_description,
    getApplicationResponse_loadBalancerDnsName,
    getApplicationResponse_targetGroupArns,
    getApplicationResponse_logGroups,
    getApplicationResponse_listenerPorts,
    getApplicationResponse_environmentId,
    getApplicationResponse_httpStatus,
    getApplicationResponse_applicationArn,
    getApplicationResponse_applicationId,
    getApplicationResponse_creationTime,
    getApplicationResponse_engineType,
    getApplicationResponse_latestVersion,
    getApplicationResponse_name,
    getApplicationResponse_status,

    -- ** GetApplicationVersion
    getApplicationVersion_applicationId,
    getApplicationVersion_applicationVersion,
    getApplicationVersionResponse_statusReason,
    getApplicationVersionResponse_description,
    getApplicationVersionResponse_httpStatus,
    getApplicationVersionResponse_applicationVersion,
    getApplicationVersionResponse_creationTime,
    getApplicationVersionResponse_definitionContent,
    getApplicationVersionResponse_name,
    getApplicationVersionResponse_status,

    -- ** GetBatchJobExecution
    getBatchJobExecution_applicationId,
    getBatchJobExecution_executionId,
    getBatchJobExecutionResponse_jobName,
    getBatchJobExecutionResponse_statusReason,
    getBatchJobExecutionResponse_jobId,
    getBatchJobExecutionResponse_endTime,
    getBatchJobExecutionResponse_jobUser,
    getBatchJobExecutionResponse_jobType,
    getBatchJobExecutionResponse_httpStatus,
    getBatchJobExecutionResponse_applicationId,
    getBatchJobExecutionResponse_executionId,
    getBatchJobExecutionResponse_startTime,
    getBatchJobExecutionResponse_status,

    -- ** GetDataSetDetails
    getDataSetDetails_applicationId,
    getDataSetDetails_dataSetName,
    getDataSetDetailsResponse_lastUpdatedTime,
    getDataSetDetailsResponse_location,
    getDataSetDetailsResponse_dataSetOrg,
    getDataSetDetailsResponse_lastReferencedTime,
    getDataSetDetailsResponse_creationTime,
    getDataSetDetailsResponse_recordLength,
    getDataSetDetailsResponse_blocksize,
    getDataSetDetailsResponse_httpStatus,
    getDataSetDetailsResponse_dataSetName,

    -- ** GetDataSetImportTask
    getDataSetImportTask_applicationId,
    getDataSetImportTask_taskId,
    getDataSetImportTaskResponse_summary,
    getDataSetImportTaskResponse_httpStatus,
    getDataSetImportTaskResponse_status,
    getDataSetImportTaskResponse_taskId,

    -- ** GetDeployment
    getDeployment_applicationId,
    getDeployment_deploymentId,
    getDeploymentResponse_statusReason,
    getDeploymentResponse_httpStatus,
    getDeploymentResponse_applicationId,
    getDeploymentResponse_applicationVersion,
    getDeploymentResponse_creationTime,
    getDeploymentResponse_deploymentId,
    getDeploymentResponse_environmentId,
    getDeploymentResponse_status,

    -- ** GetEnvironment
    getEnvironment_environmentId,
    getEnvironmentResponse_tags,
    getEnvironmentResponse_actualCapacity,
    getEnvironmentResponse_pendingMaintenance,
    getEnvironmentResponse_statusReason,
    getEnvironmentResponse_storageConfigurations,
    getEnvironmentResponse_loadBalancerArn,
    getEnvironmentResponse_description,
    getEnvironmentResponse_publiclyAccessible,
    getEnvironmentResponse_preferredMaintenanceWindow,
    getEnvironmentResponse_highAvailabilityConfig,
    getEnvironmentResponse_httpStatus,
    getEnvironmentResponse_creationTime,
    getEnvironmentResponse_engineType,
    getEnvironmentResponse_engineVersion,
    getEnvironmentResponse_environmentArn,
    getEnvironmentResponse_environmentId,
    getEnvironmentResponse_instanceType,
    getEnvironmentResponse_name,
    getEnvironmentResponse_securityGroupIds,
    getEnvironmentResponse_status,
    getEnvironmentResponse_subnetIds,
    getEnvironmentResponse_vpcId,

    -- ** ListApplicationVersions
    listApplicationVersions_nextToken,
    listApplicationVersions_maxResults,
    listApplicationVersions_applicationId,
    listApplicationVersionsResponse_nextToken,
    listApplicationVersionsResponse_httpStatus,
    listApplicationVersionsResponse_applicationVersions,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_names,
    listApplications_maxResults,
    listApplications_environmentId,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,
    listApplicationsResponse_applications,

    -- ** ListBatchJobDefinitions
    listBatchJobDefinitions_nextToken,
    listBatchJobDefinitions_maxResults,
    listBatchJobDefinitions_prefix,
    listBatchJobDefinitions_applicationId,
    listBatchJobDefinitionsResponse_nextToken,
    listBatchJobDefinitionsResponse_httpStatus,
    listBatchJobDefinitionsResponse_batchJobDefinitions,

    -- ** ListBatchJobExecutions
    listBatchJobExecutions_startedBefore,
    listBatchJobExecutions_nextToken,
    listBatchJobExecutions_jobName,
    listBatchJobExecutions_status,
    listBatchJobExecutions_maxResults,
    listBatchJobExecutions_startedAfter,
    listBatchJobExecutions_executionIds,
    listBatchJobExecutions_applicationId,
    listBatchJobExecutionsResponse_nextToken,
    listBatchJobExecutionsResponse_httpStatus,
    listBatchJobExecutionsResponse_batchJobExecutions,

    -- ** ListDataSetImportHistory
    listDataSetImportHistory_nextToken,
    listDataSetImportHistory_maxResults,
    listDataSetImportHistory_applicationId,
    listDataSetImportHistoryResponse_nextToken,
    listDataSetImportHistoryResponse_httpStatus,
    listDataSetImportHistoryResponse_dataSetImportTasks,

    -- ** ListDataSets
    listDataSets_nextToken,
    listDataSets_maxResults,
    listDataSets_prefix,
    listDataSets_applicationId,
    listDataSetsResponse_nextToken,
    listDataSetsResponse_httpStatus,
    listDataSetsResponse_dataSets,

    -- ** ListDeployments
    listDeployments_nextToken,
    listDeployments_maxResults,
    listDeployments_applicationId,
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_httpStatus,
    listDeploymentsResponse_deployments,

    -- ** ListEngineVersions
    listEngineVersions_nextToken,
    listEngineVersions_engineType,
    listEngineVersions_maxResults,
    listEngineVersionsResponse_nextToken,
    listEngineVersionsResponse_httpStatus,
    listEngineVersionsResponse_engineVersions,

    -- ** ListEnvironments
    listEnvironments_nextToken,
    listEnvironments_engineType,
    listEnvironments_names,
    listEnvironments_maxResults,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,
    listEnvironmentsResponse_environments,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** StartApplication
    startApplication_applicationId,
    startApplicationResponse_httpStatus,

    -- ** StartBatchJob
    startBatchJob_jobParams,
    startBatchJob_applicationId,
    startBatchJob_batchJobIdentifier,
    startBatchJobResponse_httpStatus,
    startBatchJobResponse_executionId,

    -- ** StopApplication
    stopApplication_forceStop,
    stopApplication_applicationId,
    stopApplicationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_description,
    updateApplication_definition,
    updateApplication_applicationId,
    updateApplication_currentApplicationVersion,
    updateApplicationResponse_httpStatus,
    updateApplicationResponse_applicationVersion,

    -- ** UpdateEnvironment
    updateEnvironment_instanceType,
    updateEnvironment_preferredMaintenanceWindow,
    updateEnvironment_desiredCapacity,
    updateEnvironment_applyDuringMaintenanceWindow,
    updateEnvironment_engineVersion,
    updateEnvironment_environmentId,
    updateEnvironmentResponse_httpStatus,
    updateEnvironmentResponse_environmentId,

    -- * Types

    -- ** AlternateKey
    alternateKey_name,
    alternateKey_allowDuplicates,
    alternateKey_length,
    alternateKey_offset,

    -- ** ApplicationSummary
    applicationSummary_deploymentStatus,
    applicationSummary_versionStatus,
    applicationSummary_lastStartTime,
    applicationSummary_description,
    applicationSummary_environmentId,
    applicationSummary_applicationArn,
    applicationSummary_applicationId,
    applicationSummary_applicationVersion,
    applicationSummary_creationTime,
    applicationSummary_engineType,
    applicationSummary_name,
    applicationSummary_status,

    -- ** ApplicationVersionSummary
    applicationVersionSummary_statusReason,
    applicationVersionSummary_applicationVersion,
    applicationVersionSummary_creationTime,
    applicationVersionSummary_status,

    -- ** BatchJobDefinition
    batchJobDefinition_fileBatchJobDefinition,
    batchJobDefinition_scriptBatchJobDefinition,

    -- ** BatchJobExecutionSummary
    batchJobExecutionSummary_jobName,
    batchJobExecutionSummary_jobId,
    batchJobExecutionSummary_endTime,
    batchJobExecutionSummary_jobType,
    batchJobExecutionSummary_applicationId,
    batchJobExecutionSummary_executionId,
    batchJobExecutionSummary_startTime,
    batchJobExecutionSummary_status,

    -- ** BatchJobIdentifier
    batchJobIdentifier_scriptBatchJobIdentifier,
    batchJobIdentifier_fileBatchJobIdentifier,

    -- ** DataSet
    dataSet_storageType,
    dataSet_relativePath,
    dataSet_datasetName,
    dataSet_datasetOrg,
    dataSet_recordLength,

    -- ** DataSetImportConfig
    dataSetImportConfig_dataSets,
    dataSetImportConfig_s3Location,

    -- ** DataSetImportItem
    dataSetImportItem_dataSet,
    dataSetImportItem_externalLocation,

    -- ** DataSetImportSummary
    dataSetImportSummary_failed,
    dataSetImportSummary_inProgress,
    dataSetImportSummary_pending,
    dataSetImportSummary_succeeded,
    dataSetImportSummary_total,

    -- ** DataSetImportTask
    dataSetImportTask_status,
    dataSetImportTask_summary,
    dataSetImportTask_taskId,

    -- ** DataSetSummary
    dataSetSummary_format,
    dataSetSummary_lastUpdatedTime,
    dataSetSummary_dataSetOrg,
    dataSetSummary_lastReferencedTime,
    dataSetSummary_creationTime,
    dataSetSummary_dataSetName,

    -- ** DatasetDetailOrgAttributes
    datasetDetailOrgAttributes_gdg,
    datasetDetailOrgAttributes_vsam,

    -- ** DatasetOrgAttributes
    datasetOrgAttributes_gdg,
    datasetOrgAttributes_vsam,

    -- ** Definition
    definition_s3Location,
    definition_content,

    -- ** DeployedVersionSummary
    deployedVersionSummary_statusReason,
    deployedVersionSummary_applicationVersion,
    deployedVersionSummary_status,

    -- ** DeploymentSummary
    deploymentSummary_statusReason,
    deploymentSummary_applicationId,
    deploymentSummary_applicationVersion,
    deploymentSummary_creationTime,
    deploymentSummary_deploymentId,
    deploymentSummary_environmentId,
    deploymentSummary_status,

    -- ** EfsStorageConfiguration
    efsStorageConfiguration_fileSystemId,
    efsStorageConfiguration_mountPoint,

    -- ** EngineVersionsSummary
    engineVersionsSummary_engineType,
    engineVersionsSummary_engineVersion,

    -- ** EnvironmentSummary
    environmentSummary_creationTime,
    environmentSummary_engineType,
    environmentSummary_engineVersion,
    environmentSummary_environmentArn,
    environmentSummary_environmentId,
    environmentSummary_instanceType,
    environmentSummary_name,
    environmentSummary_status,

    -- ** ExternalLocation
    externalLocation_s3Location,

    -- ** FileBatchJobDefinition
    fileBatchJobDefinition_folderPath,
    fileBatchJobDefinition_fileName,

    -- ** FileBatchJobIdentifier
    fileBatchJobIdentifier_folderPath,
    fileBatchJobIdentifier_fileName,

    -- ** FsxStorageConfiguration
    fsxStorageConfiguration_fileSystemId,
    fsxStorageConfiguration_mountPoint,

    -- ** GdgAttributes
    gdgAttributes_limit,
    gdgAttributes_rollDisposition,

    -- ** GdgDetailAttributes
    gdgDetailAttributes_limit,
    gdgDetailAttributes_rollDisposition,

    -- ** HighAvailabilityConfig
    highAvailabilityConfig_desiredCapacity,

    -- ** LogGroupSummary
    logGroupSummary_logGroupName,
    logGroupSummary_logType,

    -- ** MaintenanceSchedule
    maintenanceSchedule_endTime,
    maintenanceSchedule_startTime,

    -- ** PendingMaintenance
    pendingMaintenance_schedule,
    pendingMaintenance_engineVersion,

    -- ** PrimaryKey
    primaryKey_name,
    primaryKey_length,
    primaryKey_offset,

    -- ** RecordLength
    recordLength_max,
    recordLength_min,

    -- ** ScriptBatchJobDefinition
    scriptBatchJobDefinition_scriptName,

    -- ** ScriptBatchJobIdentifier
    scriptBatchJobIdentifier_scriptName,

    -- ** StorageConfiguration
    storageConfiguration_efs,
    storageConfiguration_fsx,

    -- ** VsamAttributes
    vsamAttributes_primaryKey,
    vsamAttributes_encoding,
    vsamAttributes_compressed,
    vsamAttributes_alternateKeys,
    vsamAttributes_format,

    -- ** VsamDetailAttributes
    vsamDetailAttributes_primaryKey,
    vsamDetailAttributes_encoding,
    vsamDetailAttributes_recordFormat,
    vsamDetailAttributes_compressed,
    vsamDetailAttributes_cacheAtStartup,
    vsamDetailAttributes_alternateKeys,
  )
where

import Amazonka.M2.CancelBatchJobExecution
import Amazonka.M2.CreateApplication
import Amazonka.M2.CreateDataSetImportTask
import Amazonka.M2.CreateDeployment
import Amazonka.M2.CreateEnvironment
import Amazonka.M2.DeleteApplication
import Amazonka.M2.DeleteApplicationFromEnvironment
import Amazonka.M2.DeleteEnvironment
import Amazonka.M2.GetApplication
import Amazonka.M2.GetApplicationVersion
import Amazonka.M2.GetBatchJobExecution
import Amazonka.M2.GetDataSetDetails
import Amazonka.M2.GetDataSetImportTask
import Amazonka.M2.GetDeployment
import Amazonka.M2.GetEnvironment
import Amazonka.M2.ListApplicationVersions
import Amazonka.M2.ListApplications
import Amazonka.M2.ListBatchJobDefinitions
import Amazonka.M2.ListBatchJobExecutions
import Amazonka.M2.ListDataSetImportHistory
import Amazonka.M2.ListDataSets
import Amazonka.M2.ListDeployments
import Amazonka.M2.ListEngineVersions
import Amazonka.M2.ListEnvironments
import Amazonka.M2.ListTagsForResource
import Amazonka.M2.StartApplication
import Amazonka.M2.StartBatchJob
import Amazonka.M2.StopApplication
import Amazonka.M2.TagResource
import Amazonka.M2.Types.AlternateKey
import Amazonka.M2.Types.ApplicationSummary
import Amazonka.M2.Types.ApplicationVersionSummary
import Amazonka.M2.Types.BatchJobDefinition
import Amazonka.M2.Types.BatchJobExecutionSummary
import Amazonka.M2.Types.BatchJobIdentifier
import Amazonka.M2.Types.DataSet
import Amazonka.M2.Types.DataSetImportConfig
import Amazonka.M2.Types.DataSetImportItem
import Amazonka.M2.Types.DataSetImportSummary
import Amazonka.M2.Types.DataSetImportTask
import Amazonka.M2.Types.DataSetSummary
import Amazonka.M2.Types.DatasetDetailOrgAttributes
import Amazonka.M2.Types.DatasetOrgAttributes
import Amazonka.M2.Types.Definition
import Amazonka.M2.Types.DeployedVersionSummary
import Amazonka.M2.Types.DeploymentSummary
import Amazonka.M2.Types.EfsStorageConfiguration
import Amazonka.M2.Types.EngineVersionsSummary
import Amazonka.M2.Types.EnvironmentSummary
import Amazonka.M2.Types.ExternalLocation
import Amazonka.M2.Types.FileBatchJobDefinition
import Amazonka.M2.Types.FileBatchJobIdentifier
import Amazonka.M2.Types.FsxStorageConfiguration
import Amazonka.M2.Types.GdgAttributes
import Amazonka.M2.Types.GdgDetailAttributes
import Amazonka.M2.Types.HighAvailabilityConfig
import Amazonka.M2.Types.LogGroupSummary
import Amazonka.M2.Types.MaintenanceSchedule
import Amazonka.M2.Types.PendingMaintenance
import Amazonka.M2.Types.PrimaryKey
import Amazonka.M2.Types.RecordLength
import Amazonka.M2.Types.ScriptBatchJobDefinition
import Amazonka.M2.Types.ScriptBatchJobIdentifier
import Amazonka.M2.Types.StorageConfiguration
import Amazonka.M2.Types.VsamAttributes
import Amazonka.M2.Types.VsamDetailAttributes
import Amazonka.M2.UntagResource
import Amazonka.M2.UpdateApplication
import Amazonka.M2.UpdateEnvironment
