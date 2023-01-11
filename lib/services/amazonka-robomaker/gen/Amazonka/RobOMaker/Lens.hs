{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RobOMaker.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Lens
  ( -- * Operations

    -- ** BatchDeleteWorlds
    batchDeleteWorlds_worlds,
    batchDeleteWorldsResponse_unprocessedWorlds,
    batchDeleteWorldsResponse_httpStatus,

    -- ** BatchDescribeSimulationJob
    batchDescribeSimulationJob_jobs,
    batchDescribeSimulationJobResponse_jobs,
    batchDescribeSimulationJobResponse_unprocessedJobs,
    batchDescribeSimulationJobResponse_httpStatus,

    -- ** CancelSimulationJob
    cancelSimulationJob_job,
    cancelSimulationJobResponse_httpStatus,

    -- ** CancelSimulationJobBatch
    cancelSimulationJobBatch_batch,
    cancelSimulationJobBatchResponse_httpStatus,

    -- ** CancelWorldExportJob
    cancelWorldExportJob_job,
    cancelWorldExportJobResponse_httpStatus,

    -- ** CancelWorldGenerationJob
    cancelWorldGenerationJob_job,
    cancelWorldGenerationJobResponse_httpStatus,

    -- ** CreateRobotApplication
    createRobotApplication_environment,
    createRobotApplication_sources,
    createRobotApplication_tags,
    createRobotApplication_name,
    createRobotApplication_robotSoftwareSuite,
    createRobotApplicationResponse_arn,
    createRobotApplicationResponse_environment,
    createRobotApplicationResponse_lastUpdatedAt,
    createRobotApplicationResponse_name,
    createRobotApplicationResponse_revisionId,
    createRobotApplicationResponse_robotSoftwareSuite,
    createRobotApplicationResponse_sources,
    createRobotApplicationResponse_tags,
    createRobotApplicationResponse_version,
    createRobotApplicationResponse_httpStatus,

    -- ** CreateRobotApplicationVersion
    createRobotApplicationVersion_currentRevisionId,
    createRobotApplicationVersion_imageDigest,
    createRobotApplicationVersion_s3Etags,
    createRobotApplicationVersion_application,
    createRobotApplicationVersionResponse_arn,
    createRobotApplicationVersionResponse_environment,
    createRobotApplicationVersionResponse_lastUpdatedAt,
    createRobotApplicationVersionResponse_name,
    createRobotApplicationVersionResponse_revisionId,
    createRobotApplicationVersionResponse_robotSoftwareSuite,
    createRobotApplicationVersionResponse_sources,
    createRobotApplicationVersionResponse_version,
    createRobotApplicationVersionResponse_httpStatus,

    -- ** CreateSimulationApplication
    createSimulationApplication_environment,
    createSimulationApplication_renderingEngine,
    createSimulationApplication_sources,
    createSimulationApplication_tags,
    createSimulationApplication_name,
    createSimulationApplication_simulationSoftwareSuite,
    createSimulationApplication_robotSoftwareSuite,
    createSimulationApplicationResponse_arn,
    createSimulationApplicationResponse_environment,
    createSimulationApplicationResponse_lastUpdatedAt,
    createSimulationApplicationResponse_name,
    createSimulationApplicationResponse_renderingEngine,
    createSimulationApplicationResponse_revisionId,
    createSimulationApplicationResponse_robotSoftwareSuite,
    createSimulationApplicationResponse_simulationSoftwareSuite,
    createSimulationApplicationResponse_sources,
    createSimulationApplicationResponse_tags,
    createSimulationApplicationResponse_version,
    createSimulationApplicationResponse_httpStatus,

    -- ** CreateSimulationApplicationVersion
    createSimulationApplicationVersion_currentRevisionId,
    createSimulationApplicationVersion_imageDigest,
    createSimulationApplicationVersion_s3Etags,
    createSimulationApplicationVersion_application,
    createSimulationApplicationVersionResponse_arn,
    createSimulationApplicationVersionResponse_environment,
    createSimulationApplicationVersionResponse_lastUpdatedAt,
    createSimulationApplicationVersionResponse_name,
    createSimulationApplicationVersionResponse_renderingEngine,
    createSimulationApplicationVersionResponse_revisionId,
    createSimulationApplicationVersionResponse_robotSoftwareSuite,
    createSimulationApplicationVersionResponse_simulationSoftwareSuite,
    createSimulationApplicationVersionResponse_sources,
    createSimulationApplicationVersionResponse_version,
    createSimulationApplicationVersionResponse_httpStatus,

    -- ** CreateSimulationJob
    createSimulationJob_clientRequestToken,
    createSimulationJob_compute,
    createSimulationJob_dataSources,
    createSimulationJob_failureBehavior,
    createSimulationJob_loggingConfig,
    createSimulationJob_outputLocation,
    createSimulationJob_robotApplications,
    createSimulationJob_simulationApplications,
    createSimulationJob_tags,
    createSimulationJob_vpcConfig,
    createSimulationJob_maxJobDurationInSeconds,
    createSimulationJob_iamRole,
    createSimulationJobResponse_arn,
    createSimulationJobResponse_clientRequestToken,
    createSimulationJobResponse_compute,
    createSimulationJobResponse_dataSources,
    createSimulationJobResponse_failureBehavior,
    createSimulationJobResponse_failureCode,
    createSimulationJobResponse_iamRole,
    createSimulationJobResponse_lastStartedAt,
    createSimulationJobResponse_lastUpdatedAt,
    createSimulationJobResponse_loggingConfig,
    createSimulationJobResponse_maxJobDurationInSeconds,
    createSimulationJobResponse_outputLocation,
    createSimulationJobResponse_robotApplications,
    createSimulationJobResponse_simulationApplications,
    createSimulationJobResponse_simulationTimeMillis,
    createSimulationJobResponse_status,
    createSimulationJobResponse_tags,
    createSimulationJobResponse_vpcConfig,
    createSimulationJobResponse_httpStatus,

    -- ** CreateWorldExportJob
    createWorldExportJob_clientRequestToken,
    createWorldExportJob_tags,
    createWorldExportJob_worlds,
    createWorldExportJob_outputLocation,
    createWorldExportJob_iamRole,
    createWorldExportJobResponse_arn,
    createWorldExportJobResponse_clientRequestToken,
    createWorldExportJobResponse_createdAt,
    createWorldExportJobResponse_failureCode,
    createWorldExportJobResponse_iamRole,
    createWorldExportJobResponse_outputLocation,
    createWorldExportJobResponse_status,
    createWorldExportJobResponse_tags,
    createWorldExportJobResponse_httpStatus,

    -- ** CreateWorldGenerationJob
    createWorldGenerationJob_clientRequestToken,
    createWorldGenerationJob_tags,
    createWorldGenerationJob_worldTags,
    createWorldGenerationJob_template,
    createWorldGenerationJob_worldCount,
    createWorldGenerationJobResponse_arn,
    createWorldGenerationJobResponse_clientRequestToken,
    createWorldGenerationJobResponse_createdAt,
    createWorldGenerationJobResponse_failureCode,
    createWorldGenerationJobResponse_status,
    createWorldGenerationJobResponse_tags,
    createWorldGenerationJobResponse_template,
    createWorldGenerationJobResponse_worldCount,
    createWorldGenerationJobResponse_worldTags,
    createWorldGenerationJobResponse_httpStatus,

    -- ** CreateWorldTemplate
    createWorldTemplate_clientRequestToken,
    createWorldTemplate_name,
    createWorldTemplate_tags,
    createWorldTemplate_templateBody,
    createWorldTemplate_templateLocation,
    createWorldTemplateResponse_arn,
    createWorldTemplateResponse_clientRequestToken,
    createWorldTemplateResponse_createdAt,
    createWorldTemplateResponse_name,
    createWorldTemplateResponse_tags,
    createWorldTemplateResponse_httpStatus,

    -- ** DeleteRobotApplication
    deleteRobotApplication_applicationVersion,
    deleteRobotApplication_application,
    deleteRobotApplicationResponse_httpStatus,

    -- ** DeleteSimulationApplication
    deleteSimulationApplication_applicationVersion,
    deleteSimulationApplication_application,
    deleteSimulationApplicationResponse_httpStatus,

    -- ** DeleteWorldTemplate
    deleteWorldTemplate_template,
    deleteWorldTemplateResponse_httpStatus,

    -- ** DescribeRobotApplication
    describeRobotApplication_applicationVersion,
    describeRobotApplication_application,
    describeRobotApplicationResponse_arn,
    describeRobotApplicationResponse_environment,
    describeRobotApplicationResponse_imageDigest,
    describeRobotApplicationResponse_lastUpdatedAt,
    describeRobotApplicationResponse_name,
    describeRobotApplicationResponse_revisionId,
    describeRobotApplicationResponse_robotSoftwareSuite,
    describeRobotApplicationResponse_sources,
    describeRobotApplicationResponse_tags,
    describeRobotApplicationResponse_version,
    describeRobotApplicationResponse_httpStatus,

    -- ** DescribeSimulationApplication
    describeSimulationApplication_applicationVersion,
    describeSimulationApplication_application,
    describeSimulationApplicationResponse_arn,
    describeSimulationApplicationResponse_environment,
    describeSimulationApplicationResponse_imageDigest,
    describeSimulationApplicationResponse_lastUpdatedAt,
    describeSimulationApplicationResponse_name,
    describeSimulationApplicationResponse_renderingEngine,
    describeSimulationApplicationResponse_revisionId,
    describeSimulationApplicationResponse_robotSoftwareSuite,
    describeSimulationApplicationResponse_simulationSoftwareSuite,
    describeSimulationApplicationResponse_sources,
    describeSimulationApplicationResponse_tags,
    describeSimulationApplicationResponse_version,
    describeSimulationApplicationResponse_httpStatus,

    -- ** DescribeSimulationJob
    describeSimulationJob_job,
    describeSimulationJobResponse_arn,
    describeSimulationJobResponse_clientRequestToken,
    describeSimulationJobResponse_compute,
    describeSimulationJobResponse_dataSources,
    describeSimulationJobResponse_failureBehavior,
    describeSimulationJobResponse_failureCode,
    describeSimulationJobResponse_failureReason,
    describeSimulationJobResponse_iamRole,
    describeSimulationJobResponse_lastStartedAt,
    describeSimulationJobResponse_lastUpdatedAt,
    describeSimulationJobResponse_loggingConfig,
    describeSimulationJobResponse_maxJobDurationInSeconds,
    describeSimulationJobResponse_name,
    describeSimulationJobResponse_networkInterface,
    describeSimulationJobResponse_outputLocation,
    describeSimulationJobResponse_robotApplications,
    describeSimulationJobResponse_simulationApplications,
    describeSimulationJobResponse_simulationTimeMillis,
    describeSimulationJobResponse_status,
    describeSimulationJobResponse_tags,
    describeSimulationJobResponse_vpcConfig,
    describeSimulationJobResponse_httpStatus,

    -- ** DescribeSimulationJobBatch
    describeSimulationJobBatch_batch,
    describeSimulationJobBatchResponse_arn,
    describeSimulationJobBatchResponse_batchPolicy,
    describeSimulationJobBatchResponse_clientRequestToken,
    describeSimulationJobBatchResponse_createdAt,
    describeSimulationJobBatchResponse_createdRequests,
    describeSimulationJobBatchResponse_failedRequests,
    describeSimulationJobBatchResponse_failureCode,
    describeSimulationJobBatchResponse_failureReason,
    describeSimulationJobBatchResponse_lastUpdatedAt,
    describeSimulationJobBatchResponse_pendingRequests,
    describeSimulationJobBatchResponse_status,
    describeSimulationJobBatchResponse_tags,
    describeSimulationJobBatchResponse_httpStatus,

    -- ** DescribeWorld
    describeWorld_world,
    describeWorldResponse_arn,
    describeWorldResponse_createdAt,
    describeWorldResponse_generationJob,
    describeWorldResponse_tags,
    describeWorldResponse_template,
    describeWorldResponse_worldDescriptionBody,
    describeWorldResponse_httpStatus,

    -- ** DescribeWorldExportJob
    describeWorldExportJob_job,
    describeWorldExportJobResponse_arn,
    describeWorldExportJobResponse_clientRequestToken,
    describeWorldExportJobResponse_createdAt,
    describeWorldExportJobResponse_failureCode,
    describeWorldExportJobResponse_failureReason,
    describeWorldExportJobResponse_iamRole,
    describeWorldExportJobResponse_outputLocation,
    describeWorldExportJobResponse_status,
    describeWorldExportJobResponse_tags,
    describeWorldExportJobResponse_worlds,
    describeWorldExportJobResponse_httpStatus,

    -- ** DescribeWorldGenerationJob
    describeWorldGenerationJob_job,
    describeWorldGenerationJobResponse_arn,
    describeWorldGenerationJobResponse_clientRequestToken,
    describeWorldGenerationJobResponse_createdAt,
    describeWorldGenerationJobResponse_failureCode,
    describeWorldGenerationJobResponse_failureReason,
    describeWorldGenerationJobResponse_finishedWorldsSummary,
    describeWorldGenerationJobResponse_status,
    describeWorldGenerationJobResponse_tags,
    describeWorldGenerationJobResponse_template,
    describeWorldGenerationJobResponse_worldCount,
    describeWorldGenerationJobResponse_worldTags,
    describeWorldGenerationJobResponse_httpStatus,

    -- ** DescribeWorldTemplate
    describeWorldTemplate_template,
    describeWorldTemplateResponse_arn,
    describeWorldTemplateResponse_clientRequestToken,
    describeWorldTemplateResponse_createdAt,
    describeWorldTemplateResponse_lastUpdatedAt,
    describeWorldTemplateResponse_name,
    describeWorldTemplateResponse_tags,
    describeWorldTemplateResponse_version,
    describeWorldTemplateResponse_httpStatus,

    -- ** GetWorldTemplateBody
    getWorldTemplateBody_generationJob,
    getWorldTemplateBody_template,
    getWorldTemplateBodyResponse_templateBody,
    getWorldTemplateBodyResponse_httpStatus,

    -- ** ListRobotApplications
    listRobotApplications_filters,
    listRobotApplications_maxResults,
    listRobotApplications_nextToken,
    listRobotApplications_versionQualifier,
    listRobotApplicationsResponse_nextToken,
    listRobotApplicationsResponse_robotApplicationSummaries,
    listRobotApplicationsResponse_httpStatus,

    -- ** ListSimulationApplications
    listSimulationApplications_filters,
    listSimulationApplications_maxResults,
    listSimulationApplications_nextToken,
    listSimulationApplications_versionQualifier,
    listSimulationApplicationsResponse_nextToken,
    listSimulationApplicationsResponse_simulationApplicationSummaries,
    listSimulationApplicationsResponse_httpStatus,

    -- ** ListSimulationJobBatches
    listSimulationJobBatches_filters,
    listSimulationJobBatches_maxResults,
    listSimulationJobBatches_nextToken,
    listSimulationJobBatchesResponse_nextToken,
    listSimulationJobBatchesResponse_simulationJobBatchSummaries,
    listSimulationJobBatchesResponse_httpStatus,

    -- ** ListSimulationJobs
    listSimulationJobs_filters,
    listSimulationJobs_maxResults,
    listSimulationJobs_nextToken,
    listSimulationJobsResponse_nextToken,
    listSimulationJobsResponse_httpStatus,
    listSimulationJobsResponse_simulationJobSummaries,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorldExportJobs
    listWorldExportJobs_filters,
    listWorldExportJobs_maxResults,
    listWorldExportJobs_nextToken,
    listWorldExportJobsResponse_nextToken,
    listWorldExportJobsResponse_httpStatus,
    listWorldExportJobsResponse_worldExportJobSummaries,

    -- ** ListWorldGenerationJobs
    listWorldGenerationJobs_filters,
    listWorldGenerationJobs_maxResults,
    listWorldGenerationJobs_nextToken,
    listWorldGenerationJobsResponse_nextToken,
    listWorldGenerationJobsResponse_httpStatus,
    listWorldGenerationJobsResponse_worldGenerationJobSummaries,

    -- ** ListWorldTemplates
    listWorldTemplates_maxResults,
    listWorldTemplates_nextToken,
    listWorldTemplatesResponse_nextToken,
    listWorldTemplatesResponse_templateSummaries,
    listWorldTemplatesResponse_httpStatus,

    -- ** ListWorlds
    listWorlds_filters,
    listWorlds_maxResults,
    listWorlds_nextToken,
    listWorldsResponse_nextToken,
    listWorldsResponse_worldSummaries,
    listWorldsResponse_httpStatus,

    -- ** RestartSimulationJob
    restartSimulationJob_job,
    restartSimulationJobResponse_httpStatus,

    -- ** StartSimulationJobBatch
    startSimulationJobBatch_batchPolicy,
    startSimulationJobBatch_clientRequestToken,
    startSimulationJobBatch_tags,
    startSimulationJobBatch_createSimulationJobRequests,
    startSimulationJobBatchResponse_arn,
    startSimulationJobBatchResponse_batchPolicy,
    startSimulationJobBatchResponse_clientRequestToken,
    startSimulationJobBatchResponse_createdAt,
    startSimulationJobBatchResponse_createdRequests,
    startSimulationJobBatchResponse_failedRequests,
    startSimulationJobBatchResponse_failureCode,
    startSimulationJobBatchResponse_failureReason,
    startSimulationJobBatchResponse_pendingRequests,
    startSimulationJobBatchResponse_status,
    startSimulationJobBatchResponse_tags,
    startSimulationJobBatchResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateRobotApplication
    updateRobotApplication_currentRevisionId,
    updateRobotApplication_environment,
    updateRobotApplication_sources,
    updateRobotApplication_application,
    updateRobotApplication_robotSoftwareSuite,
    updateRobotApplicationResponse_arn,
    updateRobotApplicationResponse_environment,
    updateRobotApplicationResponse_lastUpdatedAt,
    updateRobotApplicationResponse_name,
    updateRobotApplicationResponse_revisionId,
    updateRobotApplicationResponse_robotSoftwareSuite,
    updateRobotApplicationResponse_sources,
    updateRobotApplicationResponse_version,
    updateRobotApplicationResponse_httpStatus,

    -- ** UpdateSimulationApplication
    updateSimulationApplication_currentRevisionId,
    updateSimulationApplication_environment,
    updateSimulationApplication_renderingEngine,
    updateSimulationApplication_sources,
    updateSimulationApplication_application,
    updateSimulationApplication_simulationSoftwareSuite,
    updateSimulationApplication_robotSoftwareSuite,
    updateSimulationApplicationResponse_arn,
    updateSimulationApplicationResponse_environment,
    updateSimulationApplicationResponse_lastUpdatedAt,
    updateSimulationApplicationResponse_name,
    updateSimulationApplicationResponse_renderingEngine,
    updateSimulationApplicationResponse_revisionId,
    updateSimulationApplicationResponse_robotSoftwareSuite,
    updateSimulationApplicationResponse_simulationSoftwareSuite,
    updateSimulationApplicationResponse_sources,
    updateSimulationApplicationResponse_version,
    updateSimulationApplicationResponse_httpStatus,

    -- ** UpdateWorldTemplate
    updateWorldTemplate_name,
    updateWorldTemplate_templateBody,
    updateWorldTemplate_templateLocation,
    updateWorldTemplate_template,
    updateWorldTemplateResponse_arn,
    updateWorldTemplateResponse_createdAt,
    updateWorldTemplateResponse_lastUpdatedAt,
    updateWorldTemplateResponse_name,
    updateWorldTemplateResponse_httpStatus,

    -- * Types

    -- ** BatchPolicy
    batchPolicy_maxConcurrency,
    batchPolicy_timeoutInSeconds,

    -- ** Compute
    compute_computeType,
    compute_gpuUnitLimit,
    compute_simulationUnitLimit,

    -- ** ComputeResponse
    computeResponse_computeType,
    computeResponse_gpuUnitLimit,
    computeResponse_simulationUnitLimit,

    -- ** DataSource
    dataSource_destination,
    dataSource_name,
    dataSource_s3Bucket,
    dataSource_s3Keys,
    dataSource_type,

    -- ** DataSourceConfig
    dataSourceConfig_destination,
    dataSourceConfig_type,
    dataSourceConfig_name,
    dataSourceConfig_s3Bucket,
    dataSourceConfig_s3Keys,

    -- ** Environment
    environment_uri,

    -- ** FailedCreateSimulationJobRequest
    failedCreateSimulationJobRequest_failedAt,
    failedCreateSimulationJobRequest_failureCode,
    failedCreateSimulationJobRequest_failureReason,
    failedCreateSimulationJobRequest_request,

    -- ** FailureSummary
    failureSummary_failures,
    failureSummary_totalFailureCount,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** FinishedWorldsSummary
    finishedWorldsSummary_failureSummary,
    finishedWorldsSummary_finishedCount,
    finishedWorldsSummary_succeededWorlds,

    -- ** LaunchConfig
    launchConfig_command,
    launchConfig_environmentVariables,
    launchConfig_launchFile,
    launchConfig_packageName,
    launchConfig_portForwardingConfig,
    launchConfig_streamUI,

    -- ** LoggingConfig
    loggingConfig_recordAllRosTopics,

    -- ** NetworkInterface
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,
    networkInterface_publicIpAddress,

    -- ** OutputLocation
    outputLocation_s3Bucket,
    outputLocation_s3Prefix,

    -- ** PortForwardingConfig
    portForwardingConfig_portMappings,

    -- ** PortMapping
    portMapping_enableOnPublicIp,
    portMapping_jobPort,
    portMapping_applicationPort,

    -- ** RenderingEngine
    renderingEngine_name,
    renderingEngine_version,

    -- ** RobotApplicationConfig
    robotApplicationConfig_applicationVersion,
    robotApplicationConfig_tools,
    robotApplicationConfig_uploadConfigurations,
    robotApplicationConfig_useDefaultTools,
    robotApplicationConfig_useDefaultUploadConfigurations,
    robotApplicationConfig_application,
    robotApplicationConfig_launchConfig,

    -- ** RobotApplicationSummary
    robotApplicationSummary_arn,
    robotApplicationSummary_lastUpdatedAt,
    robotApplicationSummary_name,
    robotApplicationSummary_robotSoftwareSuite,
    robotApplicationSummary_version,

    -- ** RobotSoftwareSuite
    robotSoftwareSuite_name,
    robotSoftwareSuite_version,

    -- ** S3KeyOutput
    s3KeyOutput_etag,
    s3KeyOutput_s3Key,

    -- ** SimulationApplicationConfig
    simulationApplicationConfig_applicationVersion,
    simulationApplicationConfig_tools,
    simulationApplicationConfig_uploadConfigurations,
    simulationApplicationConfig_useDefaultTools,
    simulationApplicationConfig_useDefaultUploadConfigurations,
    simulationApplicationConfig_worldConfigs,
    simulationApplicationConfig_application,
    simulationApplicationConfig_launchConfig,

    -- ** SimulationApplicationSummary
    simulationApplicationSummary_arn,
    simulationApplicationSummary_lastUpdatedAt,
    simulationApplicationSummary_name,
    simulationApplicationSummary_robotSoftwareSuite,
    simulationApplicationSummary_simulationSoftwareSuite,
    simulationApplicationSummary_version,

    -- ** SimulationJob
    simulationJob_arn,
    simulationJob_clientRequestToken,
    simulationJob_compute,
    simulationJob_dataSources,
    simulationJob_failureBehavior,
    simulationJob_failureCode,
    simulationJob_failureReason,
    simulationJob_iamRole,
    simulationJob_lastStartedAt,
    simulationJob_lastUpdatedAt,
    simulationJob_loggingConfig,
    simulationJob_maxJobDurationInSeconds,
    simulationJob_name,
    simulationJob_networkInterface,
    simulationJob_outputLocation,
    simulationJob_robotApplications,
    simulationJob_simulationApplications,
    simulationJob_simulationTimeMillis,
    simulationJob_status,
    simulationJob_tags,
    simulationJob_vpcConfig,

    -- ** SimulationJobBatchSummary
    simulationJobBatchSummary_arn,
    simulationJobBatchSummary_createdAt,
    simulationJobBatchSummary_createdRequestCount,
    simulationJobBatchSummary_failedRequestCount,
    simulationJobBatchSummary_lastUpdatedAt,
    simulationJobBatchSummary_pendingRequestCount,
    simulationJobBatchSummary_status,

    -- ** SimulationJobRequest
    simulationJobRequest_compute,
    simulationJobRequest_dataSources,
    simulationJobRequest_failureBehavior,
    simulationJobRequest_iamRole,
    simulationJobRequest_loggingConfig,
    simulationJobRequest_outputLocation,
    simulationJobRequest_robotApplications,
    simulationJobRequest_simulationApplications,
    simulationJobRequest_tags,
    simulationJobRequest_useDefaultApplications,
    simulationJobRequest_vpcConfig,
    simulationJobRequest_maxJobDurationInSeconds,

    -- ** SimulationJobSummary
    simulationJobSummary_arn,
    simulationJobSummary_computeType,
    simulationJobSummary_dataSourceNames,
    simulationJobSummary_lastUpdatedAt,
    simulationJobSummary_name,
    simulationJobSummary_robotApplicationNames,
    simulationJobSummary_simulationApplicationNames,
    simulationJobSummary_status,

    -- ** SimulationSoftwareSuite
    simulationSoftwareSuite_name,
    simulationSoftwareSuite_version,

    -- ** Source
    source_architecture,
    source_etag,
    source_s3Bucket,
    source_s3Key,

    -- ** SourceConfig
    sourceConfig_architecture,
    sourceConfig_s3Bucket,
    sourceConfig_s3Key,

    -- ** TemplateLocation
    templateLocation_s3Bucket,
    templateLocation_s3Key,

    -- ** TemplateSummary
    templateSummary_arn,
    templateSummary_createdAt,
    templateSummary_lastUpdatedAt,
    templateSummary_name,
    templateSummary_version,

    -- ** Tool
    tool_exitBehavior,
    tool_streamOutputToCloudWatch,
    tool_streamUI,
    tool_name,
    tool_command,

    -- ** UploadConfiguration
    uploadConfiguration_name,
    uploadConfiguration_path,
    uploadConfiguration_uploadBehavior,

    -- ** VPCConfig
    vPCConfig_assignPublicIp,
    vPCConfig_securityGroups,
    vPCConfig_subnets,

    -- ** VPCConfigResponse
    vPCConfigResponse_assignPublicIp,
    vPCConfigResponse_securityGroups,
    vPCConfigResponse_subnets,
    vPCConfigResponse_vpcId,

    -- ** WorldConfig
    worldConfig_world,

    -- ** WorldCount
    worldCount_floorplanCount,
    worldCount_interiorCountPerFloorplan,

    -- ** WorldExportJobSummary
    worldExportJobSummary_arn,
    worldExportJobSummary_createdAt,
    worldExportJobSummary_outputLocation,
    worldExportJobSummary_status,
    worldExportJobSummary_worlds,

    -- ** WorldFailure
    worldFailure_failureCode,
    worldFailure_failureCount,
    worldFailure_sampleFailureReason,

    -- ** WorldGenerationJobSummary
    worldGenerationJobSummary_arn,
    worldGenerationJobSummary_createdAt,
    worldGenerationJobSummary_failedWorldCount,
    worldGenerationJobSummary_status,
    worldGenerationJobSummary_succeededWorldCount,
    worldGenerationJobSummary_template,
    worldGenerationJobSummary_worldCount,

    -- ** WorldSummary
    worldSummary_arn,
    worldSummary_createdAt,
    worldSummary_generationJob,
    worldSummary_template,
  )
where

import Amazonka.RobOMaker.BatchDeleteWorlds
import Amazonka.RobOMaker.BatchDescribeSimulationJob
import Amazonka.RobOMaker.CancelSimulationJob
import Amazonka.RobOMaker.CancelSimulationJobBatch
import Amazonka.RobOMaker.CancelWorldExportJob
import Amazonka.RobOMaker.CancelWorldGenerationJob
import Amazonka.RobOMaker.CreateRobotApplication
import Amazonka.RobOMaker.CreateRobotApplicationVersion
import Amazonka.RobOMaker.CreateSimulationApplication
import Amazonka.RobOMaker.CreateSimulationApplicationVersion
import Amazonka.RobOMaker.CreateSimulationJob
import Amazonka.RobOMaker.CreateWorldExportJob
import Amazonka.RobOMaker.CreateWorldGenerationJob
import Amazonka.RobOMaker.CreateWorldTemplate
import Amazonka.RobOMaker.DeleteRobotApplication
import Amazonka.RobOMaker.DeleteSimulationApplication
import Amazonka.RobOMaker.DeleteWorldTemplate
import Amazonka.RobOMaker.DescribeRobotApplication
import Amazonka.RobOMaker.DescribeSimulationApplication
import Amazonka.RobOMaker.DescribeSimulationJob
import Amazonka.RobOMaker.DescribeSimulationJobBatch
import Amazonka.RobOMaker.DescribeWorld
import Amazonka.RobOMaker.DescribeWorldExportJob
import Amazonka.RobOMaker.DescribeWorldGenerationJob
import Amazonka.RobOMaker.DescribeWorldTemplate
import Amazonka.RobOMaker.GetWorldTemplateBody
import Amazonka.RobOMaker.ListRobotApplications
import Amazonka.RobOMaker.ListSimulationApplications
import Amazonka.RobOMaker.ListSimulationJobBatches
import Amazonka.RobOMaker.ListSimulationJobs
import Amazonka.RobOMaker.ListTagsForResource
import Amazonka.RobOMaker.ListWorldExportJobs
import Amazonka.RobOMaker.ListWorldGenerationJobs
import Amazonka.RobOMaker.ListWorldTemplates
import Amazonka.RobOMaker.ListWorlds
import Amazonka.RobOMaker.RestartSimulationJob
import Amazonka.RobOMaker.StartSimulationJobBatch
import Amazonka.RobOMaker.TagResource
import Amazonka.RobOMaker.Types.BatchPolicy
import Amazonka.RobOMaker.Types.Compute
import Amazonka.RobOMaker.Types.ComputeResponse
import Amazonka.RobOMaker.Types.DataSource
import Amazonka.RobOMaker.Types.DataSourceConfig
import Amazonka.RobOMaker.Types.Environment
import Amazonka.RobOMaker.Types.FailedCreateSimulationJobRequest
import Amazonka.RobOMaker.Types.FailureSummary
import Amazonka.RobOMaker.Types.Filter
import Amazonka.RobOMaker.Types.FinishedWorldsSummary
import Amazonka.RobOMaker.Types.LaunchConfig
import Amazonka.RobOMaker.Types.LoggingConfig
import Amazonka.RobOMaker.Types.NetworkInterface
import Amazonka.RobOMaker.Types.OutputLocation
import Amazonka.RobOMaker.Types.PortForwardingConfig
import Amazonka.RobOMaker.Types.PortMapping
import Amazonka.RobOMaker.Types.RenderingEngine
import Amazonka.RobOMaker.Types.RobotApplicationConfig
import Amazonka.RobOMaker.Types.RobotApplicationSummary
import Amazonka.RobOMaker.Types.RobotSoftwareSuite
import Amazonka.RobOMaker.Types.S3KeyOutput
import Amazonka.RobOMaker.Types.SimulationApplicationConfig
import Amazonka.RobOMaker.Types.SimulationApplicationSummary
import Amazonka.RobOMaker.Types.SimulationJob
import Amazonka.RobOMaker.Types.SimulationJobBatchSummary
import Amazonka.RobOMaker.Types.SimulationJobRequest
import Amazonka.RobOMaker.Types.SimulationJobSummary
import Amazonka.RobOMaker.Types.SimulationSoftwareSuite
import Amazonka.RobOMaker.Types.Source
import Amazonka.RobOMaker.Types.SourceConfig
import Amazonka.RobOMaker.Types.TemplateLocation
import Amazonka.RobOMaker.Types.TemplateSummary
import Amazonka.RobOMaker.Types.Tool
import Amazonka.RobOMaker.Types.UploadConfiguration
import Amazonka.RobOMaker.Types.VPCConfig
import Amazonka.RobOMaker.Types.VPCConfigResponse
import Amazonka.RobOMaker.Types.WorldConfig
import Amazonka.RobOMaker.Types.WorldCount
import Amazonka.RobOMaker.Types.WorldExportJobSummary
import Amazonka.RobOMaker.Types.WorldFailure
import Amazonka.RobOMaker.Types.WorldGenerationJobSummary
import Amazonka.RobOMaker.Types.WorldSummary
import Amazonka.RobOMaker.UntagResource
import Amazonka.RobOMaker.UpdateRobotApplication
import Amazonka.RobOMaker.UpdateSimulationApplication
import Amazonka.RobOMaker.UpdateWorldTemplate
