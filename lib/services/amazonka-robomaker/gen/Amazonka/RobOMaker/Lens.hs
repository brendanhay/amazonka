{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RobOMaker.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    batchDescribeSimulationJobResponse_unprocessedJobs,
    batchDescribeSimulationJobResponse_jobs,
    batchDescribeSimulationJobResponse_httpStatus,

    -- ** CancelDeploymentJob
    cancelDeploymentJob_job,
    cancelDeploymentJobResponse_httpStatus,

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

    -- ** CreateDeploymentJob
    createDeploymentJob_tags,
    createDeploymentJob_deploymentConfig,
    createDeploymentJob_clientRequestToken,
    createDeploymentJob_fleet,
    createDeploymentJob_deploymentApplicationConfigs,
    createDeploymentJobResponse_tags,
    createDeploymentJobResponse_deploymentApplicationConfigs,
    createDeploymentJobResponse_failureCode,
    createDeploymentJobResponse_fleet,
    createDeploymentJobResponse_arn,
    createDeploymentJobResponse_status,
    createDeploymentJobResponse_deploymentConfig,
    createDeploymentJobResponse_createdAt,
    createDeploymentJobResponse_failureReason,
    createDeploymentJobResponse_httpStatus,

    -- ** CreateFleet
    createFleet_tags,
    createFleet_name,
    createFleetResponse_tags,
    createFleetResponse_name,
    createFleetResponse_arn,
    createFleetResponse_createdAt,
    createFleetResponse_httpStatus,

    -- ** CreateRobot
    createRobot_tags,
    createRobot_name,
    createRobot_architecture,
    createRobot_greengrassGroupId,
    createRobotResponse_tags,
    createRobotResponse_name,
    createRobotResponse_arn,
    createRobotResponse_greengrassGroupId,
    createRobotResponse_architecture,
    createRobotResponse_createdAt,
    createRobotResponse_httpStatus,

    -- ** CreateRobotApplication
    createRobotApplication_tags,
    createRobotApplication_sources,
    createRobotApplication_environment,
    createRobotApplication_name,
    createRobotApplication_robotSoftwareSuite,
    createRobotApplicationResponse_tags,
    createRobotApplicationResponse_name,
    createRobotApplicationResponse_sources,
    createRobotApplicationResponse_environment,
    createRobotApplicationResponse_lastUpdatedAt,
    createRobotApplicationResponse_arn,
    createRobotApplicationResponse_robotSoftwareSuite,
    createRobotApplicationResponse_revisionId,
    createRobotApplicationResponse_version,
    createRobotApplicationResponse_httpStatus,

    -- ** CreateRobotApplicationVersion
    createRobotApplicationVersion_s3Etags,
    createRobotApplicationVersion_currentRevisionId,
    createRobotApplicationVersion_imageDigest,
    createRobotApplicationVersion_application,
    createRobotApplicationVersionResponse_name,
    createRobotApplicationVersionResponse_sources,
    createRobotApplicationVersionResponse_environment,
    createRobotApplicationVersionResponse_lastUpdatedAt,
    createRobotApplicationVersionResponse_arn,
    createRobotApplicationVersionResponse_robotSoftwareSuite,
    createRobotApplicationVersionResponse_revisionId,
    createRobotApplicationVersionResponse_version,
    createRobotApplicationVersionResponse_httpStatus,

    -- ** CreateSimulationApplication
    createSimulationApplication_tags,
    createSimulationApplication_sources,
    createSimulationApplication_environment,
    createSimulationApplication_renderingEngine,
    createSimulationApplication_name,
    createSimulationApplication_simulationSoftwareSuite,
    createSimulationApplication_robotSoftwareSuite,
    createSimulationApplicationResponse_tags,
    createSimulationApplicationResponse_name,
    createSimulationApplicationResponse_sources,
    createSimulationApplicationResponse_environment,
    createSimulationApplicationResponse_renderingEngine,
    createSimulationApplicationResponse_lastUpdatedAt,
    createSimulationApplicationResponse_arn,
    createSimulationApplicationResponse_robotSoftwareSuite,
    createSimulationApplicationResponse_simulationSoftwareSuite,
    createSimulationApplicationResponse_revisionId,
    createSimulationApplicationResponse_version,
    createSimulationApplicationResponse_httpStatus,

    -- ** CreateSimulationApplicationVersion
    createSimulationApplicationVersion_s3Etags,
    createSimulationApplicationVersion_currentRevisionId,
    createSimulationApplicationVersion_imageDigest,
    createSimulationApplicationVersion_application,
    createSimulationApplicationVersionResponse_name,
    createSimulationApplicationVersionResponse_sources,
    createSimulationApplicationVersionResponse_environment,
    createSimulationApplicationVersionResponse_renderingEngine,
    createSimulationApplicationVersionResponse_lastUpdatedAt,
    createSimulationApplicationVersionResponse_arn,
    createSimulationApplicationVersionResponse_robotSoftwareSuite,
    createSimulationApplicationVersionResponse_simulationSoftwareSuite,
    createSimulationApplicationVersionResponse_revisionId,
    createSimulationApplicationVersionResponse_version,
    createSimulationApplicationVersionResponse_httpStatus,

    -- ** CreateSimulationJob
    createSimulationJob_tags,
    createSimulationJob_clientRequestToken,
    createSimulationJob_dataSources,
    createSimulationJob_vpcConfig,
    createSimulationJob_outputLocation,
    createSimulationJob_simulationApplications,
    createSimulationJob_failureBehavior,
    createSimulationJob_robotApplications,
    createSimulationJob_loggingConfig,
    createSimulationJob_compute,
    createSimulationJob_maxJobDurationInSeconds,
    createSimulationJob_iamRole,
    createSimulationJobResponse_tags,
    createSimulationJobResponse_lastStartedAt,
    createSimulationJobResponse_iamRole,
    createSimulationJobResponse_maxJobDurationInSeconds,
    createSimulationJobResponse_failureCode,
    createSimulationJobResponse_clientRequestToken,
    createSimulationJobResponse_lastUpdatedAt,
    createSimulationJobResponse_dataSources,
    createSimulationJobResponse_vpcConfig,
    createSimulationJobResponse_arn,
    createSimulationJobResponse_outputLocation,
    createSimulationJobResponse_status,
    createSimulationJobResponse_simulationApplications,
    createSimulationJobResponse_simulationTimeMillis,
    createSimulationJobResponse_failureBehavior,
    createSimulationJobResponse_robotApplications,
    createSimulationJobResponse_loggingConfig,
    createSimulationJobResponse_compute,
    createSimulationJobResponse_httpStatus,

    -- ** CreateWorldExportJob
    createWorldExportJob_tags,
    createWorldExportJob_clientRequestToken,
    createWorldExportJob_worlds,
    createWorldExportJob_outputLocation,
    createWorldExportJob_iamRole,
    createWorldExportJobResponse_tags,
    createWorldExportJobResponse_iamRole,
    createWorldExportJobResponse_failureCode,
    createWorldExportJobResponse_clientRequestToken,
    createWorldExportJobResponse_arn,
    createWorldExportJobResponse_outputLocation,
    createWorldExportJobResponse_status,
    createWorldExportJobResponse_createdAt,
    createWorldExportJobResponse_httpStatus,

    -- ** CreateWorldGenerationJob
    createWorldGenerationJob_tags,
    createWorldGenerationJob_worldTags,
    createWorldGenerationJob_clientRequestToken,
    createWorldGenerationJob_template,
    createWorldGenerationJob_worldCount,
    createWorldGenerationJobResponse_tags,
    createWorldGenerationJobResponse_worldCount,
    createWorldGenerationJobResponse_worldTags,
    createWorldGenerationJobResponse_failureCode,
    createWorldGenerationJobResponse_clientRequestToken,
    createWorldGenerationJobResponse_arn,
    createWorldGenerationJobResponse_status,
    createWorldGenerationJobResponse_createdAt,
    createWorldGenerationJobResponse_template,
    createWorldGenerationJobResponse_httpStatus,

    -- ** CreateWorldTemplate
    createWorldTemplate_tags,
    createWorldTemplate_name,
    createWorldTemplate_clientRequestToken,
    createWorldTemplate_templateBody,
    createWorldTemplate_templateLocation,
    createWorldTemplateResponse_tags,
    createWorldTemplateResponse_name,
    createWorldTemplateResponse_clientRequestToken,
    createWorldTemplateResponse_arn,
    createWorldTemplateResponse_createdAt,
    createWorldTemplateResponse_httpStatus,

    -- ** DeleteFleet
    deleteFleet_fleet,
    deleteFleetResponse_httpStatus,

    -- ** DeleteRobot
    deleteRobot_robot,
    deleteRobotResponse_httpStatus,

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

    -- ** DeregisterRobot
    deregisterRobot_fleet,
    deregisterRobot_robot,
    deregisterRobotResponse_fleet,
    deregisterRobotResponse_robot,
    deregisterRobotResponse_httpStatus,

    -- ** DescribeDeploymentJob
    describeDeploymentJob_job,
    describeDeploymentJobResponse_tags,
    describeDeploymentJobResponse_deploymentApplicationConfigs,
    describeDeploymentJobResponse_failureCode,
    describeDeploymentJobResponse_fleet,
    describeDeploymentJobResponse_arn,
    describeDeploymentJobResponse_status,
    describeDeploymentJobResponse_robotDeploymentSummary,
    describeDeploymentJobResponse_deploymentConfig,
    describeDeploymentJobResponse_createdAt,
    describeDeploymentJobResponse_failureReason,
    describeDeploymentJobResponse_httpStatus,

    -- ** DescribeFleet
    describeFleet_fleet,
    describeFleetResponse_tags,
    describeFleetResponse_name,
    describeFleetResponse_arn,
    describeFleetResponse_lastDeploymentJob,
    describeFleetResponse_lastDeploymentStatus,
    describeFleetResponse_lastDeploymentTime,
    describeFleetResponse_robots,
    describeFleetResponse_createdAt,
    describeFleetResponse_httpStatus,

    -- ** DescribeRobot
    describeRobot_robot,
    describeRobotResponse_tags,
    describeRobotResponse_name,
    describeRobotResponse_arn,
    describeRobotResponse_status,
    describeRobotResponse_lastDeploymentJob,
    describeRobotResponse_fleetArn,
    describeRobotResponse_greengrassGroupId,
    describeRobotResponse_lastDeploymentTime,
    describeRobotResponse_architecture,
    describeRobotResponse_createdAt,
    describeRobotResponse_httpStatus,

    -- ** DescribeRobotApplication
    describeRobotApplication_applicationVersion,
    describeRobotApplication_application,
    describeRobotApplicationResponse_tags,
    describeRobotApplicationResponse_name,
    describeRobotApplicationResponse_sources,
    describeRobotApplicationResponse_environment,
    describeRobotApplicationResponse_lastUpdatedAt,
    describeRobotApplicationResponse_arn,
    describeRobotApplicationResponse_robotSoftwareSuite,
    describeRobotApplicationResponse_revisionId,
    describeRobotApplicationResponse_imageDigest,
    describeRobotApplicationResponse_version,
    describeRobotApplicationResponse_httpStatus,

    -- ** DescribeSimulationApplication
    describeSimulationApplication_applicationVersion,
    describeSimulationApplication_application,
    describeSimulationApplicationResponse_tags,
    describeSimulationApplicationResponse_name,
    describeSimulationApplicationResponse_sources,
    describeSimulationApplicationResponse_environment,
    describeSimulationApplicationResponse_renderingEngine,
    describeSimulationApplicationResponse_lastUpdatedAt,
    describeSimulationApplicationResponse_arn,
    describeSimulationApplicationResponse_robotSoftwareSuite,
    describeSimulationApplicationResponse_simulationSoftwareSuite,
    describeSimulationApplicationResponse_revisionId,
    describeSimulationApplicationResponse_imageDigest,
    describeSimulationApplicationResponse_version,
    describeSimulationApplicationResponse_httpStatus,

    -- ** DescribeSimulationJob
    describeSimulationJob_job,
    describeSimulationJobResponse_tags,
    describeSimulationJobResponse_lastStartedAt,
    describeSimulationJobResponse_name,
    describeSimulationJobResponse_iamRole,
    describeSimulationJobResponse_maxJobDurationInSeconds,
    describeSimulationJobResponse_failureCode,
    describeSimulationJobResponse_clientRequestToken,
    describeSimulationJobResponse_lastUpdatedAt,
    describeSimulationJobResponse_dataSources,
    describeSimulationJobResponse_vpcConfig,
    describeSimulationJobResponse_arn,
    describeSimulationJobResponse_outputLocation,
    describeSimulationJobResponse_status,
    describeSimulationJobResponse_simulationApplications,
    describeSimulationJobResponse_simulationTimeMillis,
    describeSimulationJobResponse_failureBehavior,
    describeSimulationJobResponse_robotApplications,
    describeSimulationJobResponse_networkInterface,
    describeSimulationJobResponse_loggingConfig,
    describeSimulationJobResponse_failureReason,
    describeSimulationJobResponse_compute,
    describeSimulationJobResponse_httpStatus,

    -- ** DescribeSimulationJobBatch
    describeSimulationJobBatch_batch,
    describeSimulationJobBatchResponse_tags,
    describeSimulationJobBatchResponse_failureCode,
    describeSimulationJobBatchResponse_clientRequestToken,
    describeSimulationJobBatchResponse_lastUpdatedAt,
    describeSimulationJobBatchResponse_arn,
    describeSimulationJobBatchResponse_status,
    describeSimulationJobBatchResponse_failedRequests,
    describeSimulationJobBatchResponse_pendingRequests,
    describeSimulationJobBatchResponse_createdRequests,
    describeSimulationJobBatchResponse_batchPolicy,
    describeSimulationJobBatchResponse_createdAt,
    describeSimulationJobBatchResponse_failureReason,
    describeSimulationJobBatchResponse_httpStatus,

    -- ** DescribeWorld
    describeWorld_world,
    describeWorldResponse_tags,
    describeWorldResponse_arn,
    describeWorldResponse_generationJob,
    describeWorldResponse_worldDescriptionBody,
    describeWorldResponse_createdAt,
    describeWorldResponse_template,
    describeWorldResponse_httpStatus,

    -- ** DescribeWorldExportJob
    describeWorldExportJob_job,
    describeWorldExportJobResponse_tags,
    describeWorldExportJobResponse_iamRole,
    describeWorldExportJobResponse_failureCode,
    describeWorldExportJobResponse_clientRequestToken,
    describeWorldExportJobResponse_arn,
    describeWorldExportJobResponse_outputLocation,
    describeWorldExportJobResponse_status,
    describeWorldExportJobResponse_worlds,
    describeWorldExportJobResponse_createdAt,
    describeWorldExportJobResponse_failureReason,
    describeWorldExportJobResponse_httpStatus,

    -- ** DescribeWorldGenerationJob
    describeWorldGenerationJob_job,
    describeWorldGenerationJobResponse_tags,
    describeWorldGenerationJobResponse_worldCount,
    describeWorldGenerationJobResponse_worldTags,
    describeWorldGenerationJobResponse_failureCode,
    describeWorldGenerationJobResponse_clientRequestToken,
    describeWorldGenerationJobResponse_arn,
    describeWorldGenerationJobResponse_status,
    describeWorldGenerationJobResponse_finishedWorldsSummary,
    describeWorldGenerationJobResponse_createdAt,
    describeWorldGenerationJobResponse_failureReason,
    describeWorldGenerationJobResponse_template,
    describeWorldGenerationJobResponse_httpStatus,

    -- ** DescribeWorldTemplate
    describeWorldTemplate_template,
    describeWorldTemplateResponse_tags,
    describeWorldTemplateResponse_name,
    describeWorldTemplateResponse_clientRequestToken,
    describeWorldTemplateResponse_lastUpdatedAt,
    describeWorldTemplateResponse_arn,
    describeWorldTemplateResponse_createdAt,
    describeWorldTemplateResponse_version,
    describeWorldTemplateResponse_httpStatus,

    -- ** GetWorldTemplateBody
    getWorldTemplateBody_generationJob,
    getWorldTemplateBody_template,
    getWorldTemplateBodyResponse_templateBody,
    getWorldTemplateBodyResponse_httpStatus,

    -- ** ListDeploymentJobs
    listDeploymentJobs_nextToken,
    listDeploymentJobs_filters,
    listDeploymentJobs_maxResults,
    listDeploymentJobsResponse_deploymentJobs,
    listDeploymentJobsResponse_nextToken,
    listDeploymentJobsResponse_httpStatus,

    -- ** ListFleets
    listFleets_nextToken,
    listFleets_filters,
    listFleets_maxResults,
    listFleetsResponse_nextToken,
    listFleetsResponse_fleetDetails,
    listFleetsResponse_httpStatus,

    -- ** ListRobotApplications
    listRobotApplications_nextToken,
    listRobotApplications_filters,
    listRobotApplications_versionQualifier,
    listRobotApplications_maxResults,
    listRobotApplicationsResponse_nextToken,
    listRobotApplicationsResponse_robotApplicationSummaries,
    listRobotApplicationsResponse_httpStatus,

    -- ** ListRobots
    listRobots_nextToken,
    listRobots_filters,
    listRobots_maxResults,
    listRobotsResponse_nextToken,
    listRobotsResponse_robots,
    listRobotsResponse_httpStatus,

    -- ** ListSimulationApplications
    listSimulationApplications_nextToken,
    listSimulationApplications_filters,
    listSimulationApplications_versionQualifier,
    listSimulationApplications_maxResults,
    listSimulationApplicationsResponse_nextToken,
    listSimulationApplicationsResponse_simulationApplicationSummaries,
    listSimulationApplicationsResponse_httpStatus,

    -- ** ListSimulationJobBatches
    listSimulationJobBatches_nextToken,
    listSimulationJobBatches_filters,
    listSimulationJobBatches_maxResults,
    listSimulationJobBatchesResponse_nextToken,
    listSimulationJobBatchesResponse_simulationJobBatchSummaries,
    listSimulationJobBatchesResponse_httpStatus,

    -- ** ListSimulationJobs
    listSimulationJobs_nextToken,
    listSimulationJobs_filters,
    listSimulationJobs_maxResults,
    listSimulationJobsResponse_nextToken,
    listSimulationJobsResponse_httpStatus,
    listSimulationJobsResponse_simulationJobSummaries,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorldExportJobs
    listWorldExportJobs_nextToken,
    listWorldExportJobs_filters,
    listWorldExportJobs_maxResults,
    listWorldExportJobsResponse_nextToken,
    listWorldExportJobsResponse_httpStatus,
    listWorldExportJobsResponse_worldExportJobSummaries,

    -- ** ListWorldGenerationJobs
    listWorldGenerationJobs_nextToken,
    listWorldGenerationJobs_filters,
    listWorldGenerationJobs_maxResults,
    listWorldGenerationJobsResponse_nextToken,
    listWorldGenerationJobsResponse_httpStatus,
    listWorldGenerationJobsResponse_worldGenerationJobSummaries,

    -- ** ListWorldTemplates
    listWorldTemplates_nextToken,
    listWorldTemplates_maxResults,
    listWorldTemplatesResponse_templateSummaries,
    listWorldTemplatesResponse_nextToken,
    listWorldTemplatesResponse_httpStatus,

    -- ** ListWorlds
    listWorlds_nextToken,
    listWorlds_filters,
    listWorlds_maxResults,
    listWorldsResponse_nextToken,
    listWorldsResponse_worldSummaries,
    listWorldsResponse_httpStatus,

    -- ** RegisterRobot
    registerRobot_fleet,
    registerRobot_robot,
    registerRobotResponse_fleet,
    registerRobotResponse_robot,
    registerRobotResponse_httpStatus,

    -- ** RestartSimulationJob
    restartSimulationJob_job,
    restartSimulationJobResponse_httpStatus,

    -- ** StartSimulationJobBatch
    startSimulationJobBatch_tags,
    startSimulationJobBatch_clientRequestToken,
    startSimulationJobBatch_batchPolicy,
    startSimulationJobBatch_createSimulationJobRequests,
    startSimulationJobBatchResponse_tags,
    startSimulationJobBatchResponse_failureCode,
    startSimulationJobBatchResponse_clientRequestToken,
    startSimulationJobBatchResponse_arn,
    startSimulationJobBatchResponse_status,
    startSimulationJobBatchResponse_failedRequests,
    startSimulationJobBatchResponse_pendingRequests,
    startSimulationJobBatchResponse_createdRequests,
    startSimulationJobBatchResponse_batchPolicy,
    startSimulationJobBatchResponse_createdAt,
    startSimulationJobBatchResponse_failureReason,
    startSimulationJobBatchResponse_httpStatus,

    -- ** SyncDeploymentJob
    syncDeploymentJob_clientRequestToken,
    syncDeploymentJob_fleet,
    syncDeploymentJobResponse_deploymentApplicationConfigs,
    syncDeploymentJobResponse_failureCode,
    syncDeploymentJobResponse_fleet,
    syncDeploymentJobResponse_arn,
    syncDeploymentJobResponse_status,
    syncDeploymentJobResponse_deploymentConfig,
    syncDeploymentJobResponse_createdAt,
    syncDeploymentJobResponse_failureReason,
    syncDeploymentJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateRobotApplication
    updateRobotApplication_sources,
    updateRobotApplication_environment,
    updateRobotApplication_currentRevisionId,
    updateRobotApplication_application,
    updateRobotApplication_robotSoftwareSuite,
    updateRobotApplicationResponse_name,
    updateRobotApplicationResponse_sources,
    updateRobotApplicationResponse_environment,
    updateRobotApplicationResponse_lastUpdatedAt,
    updateRobotApplicationResponse_arn,
    updateRobotApplicationResponse_robotSoftwareSuite,
    updateRobotApplicationResponse_revisionId,
    updateRobotApplicationResponse_version,
    updateRobotApplicationResponse_httpStatus,

    -- ** UpdateSimulationApplication
    updateSimulationApplication_sources,
    updateSimulationApplication_environment,
    updateSimulationApplication_renderingEngine,
    updateSimulationApplication_currentRevisionId,
    updateSimulationApplication_application,
    updateSimulationApplication_simulationSoftwareSuite,
    updateSimulationApplication_robotSoftwareSuite,
    updateSimulationApplicationResponse_name,
    updateSimulationApplicationResponse_sources,
    updateSimulationApplicationResponse_environment,
    updateSimulationApplicationResponse_renderingEngine,
    updateSimulationApplicationResponse_lastUpdatedAt,
    updateSimulationApplicationResponse_arn,
    updateSimulationApplicationResponse_robotSoftwareSuite,
    updateSimulationApplicationResponse_simulationSoftwareSuite,
    updateSimulationApplicationResponse_revisionId,
    updateSimulationApplicationResponse_version,
    updateSimulationApplicationResponse_httpStatus,

    -- ** UpdateWorldTemplate
    updateWorldTemplate_name,
    updateWorldTemplate_templateBody,
    updateWorldTemplate_templateLocation,
    updateWorldTemplate_template,
    updateWorldTemplateResponse_name,
    updateWorldTemplateResponse_lastUpdatedAt,
    updateWorldTemplateResponse_arn,
    updateWorldTemplateResponse_createdAt,
    updateWorldTemplateResponse_httpStatus,

    -- * Types

    -- ** BatchPolicy
    batchPolicy_timeoutInSeconds,
    batchPolicy_maxConcurrency,

    -- ** Compute
    compute_gpuUnitLimit,
    compute_simulationUnitLimit,
    compute_computeType,

    -- ** ComputeResponse
    computeResponse_gpuUnitLimit,
    computeResponse_simulationUnitLimit,
    computeResponse_computeType,

    -- ** DataSource
    dataSource_destination,
    dataSource_s3Bucket,
    dataSource_name,
    dataSource_type,
    dataSource_s3Keys,

    -- ** DataSourceConfig
    dataSourceConfig_destination,
    dataSourceConfig_type,
    dataSourceConfig_name,
    dataSourceConfig_s3Bucket,
    dataSourceConfig_s3Keys,

    -- ** DeploymentApplicationConfig
    deploymentApplicationConfig_application,
    deploymentApplicationConfig_applicationVersion,
    deploymentApplicationConfig_launchConfig,

    -- ** DeploymentConfig
    deploymentConfig_robotDeploymentTimeoutInSeconds,
    deploymentConfig_downloadConditionFile,
    deploymentConfig_concurrentDeploymentPercentage,
    deploymentConfig_failureThresholdPercentage,

    -- ** DeploymentJob
    deploymentJob_deploymentApplicationConfigs,
    deploymentJob_failureCode,
    deploymentJob_fleet,
    deploymentJob_arn,
    deploymentJob_status,
    deploymentJob_deploymentConfig,
    deploymentJob_createdAt,
    deploymentJob_failureReason,

    -- ** DeploymentLaunchConfig
    deploymentLaunchConfig_postLaunchFile,
    deploymentLaunchConfig_preLaunchFile,
    deploymentLaunchConfig_environmentVariables,
    deploymentLaunchConfig_packageName,
    deploymentLaunchConfig_launchFile,

    -- ** Environment
    environment_uri,

    -- ** FailedCreateSimulationJobRequest
    failedCreateSimulationJobRequest_failureCode,
    failedCreateSimulationJobRequest_request,
    failedCreateSimulationJobRequest_failedAt,
    failedCreateSimulationJobRequest_failureReason,

    -- ** FailureSummary
    failureSummary_totalFailureCount,
    failureSummary_failures,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** FinishedWorldsSummary
    finishedWorldsSummary_failureSummary,
    finishedWorldsSummary_succeededWorlds,
    finishedWorldsSummary_finishedCount,

    -- ** Fleet
    fleet_name,
    fleet_arn,
    fleet_lastDeploymentJob,
    fleet_lastDeploymentStatus,
    fleet_lastDeploymentTime,
    fleet_createdAt,

    -- ** LaunchConfig
    launchConfig_packageName,
    launchConfig_launchFile,
    launchConfig_command,
    launchConfig_streamUI,
    launchConfig_environmentVariables,
    launchConfig_portForwardingConfig,

    -- ** LoggingConfig
    loggingConfig_recordAllRosTopics,

    -- ** NetworkInterface
    networkInterface_publicIpAddress,
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,

    -- ** OutputLocation
    outputLocation_s3Bucket,
    outputLocation_s3Prefix,

    -- ** PortForwardingConfig
    portForwardingConfig_portMappings,

    -- ** PortMapping
    portMapping_enableOnPublicIp,
    portMapping_jobPort,
    portMapping_applicationPort,

    -- ** ProgressDetail
    progressDetail_targetResource,
    progressDetail_estimatedTimeRemainingSeconds,
    progressDetail_currentProgress,
    progressDetail_percentDone,

    -- ** RenderingEngine
    renderingEngine_name,
    renderingEngine_version,

    -- ** Robot
    robot_name,
    robot_arn,
    robot_status,
    robot_lastDeploymentJob,
    robot_fleetArn,
    robot_greenGrassGroupId,
    robot_lastDeploymentTime,
    robot_architecture,
    robot_createdAt,

    -- ** RobotApplicationConfig
    robotApplicationConfig_tools,
    robotApplicationConfig_useDefaultTools,
    robotApplicationConfig_useDefaultUploadConfigurations,
    robotApplicationConfig_uploadConfigurations,
    robotApplicationConfig_applicationVersion,
    robotApplicationConfig_application,
    robotApplicationConfig_launchConfig,

    -- ** RobotApplicationSummary
    robotApplicationSummary_name,
    robotApplicationSummary_lastUpdatedAt,
    robotApplicationSummary_arn,
    robotApplicationSummary_robotSoftwareSuite,
    robotApplicationSummary_version,

    -- ** RobotDeployment
    robotDeployment_deploymentFinishTime,
    robotDeployment_failureCode,
    robotDeployment_arn,
    robotDeployment_progressDetail,
    robotDeployment_status,
    robotDeployment_deploymentStartTime,
    robotDeployment_failureReason,

    -- ** RobotSoftwareSuite
    robotSoftwareSuite_name,
    robotSoftwareSuite_version,

    -- ** S3KeyOutput
    s3KeyOutput_s3Key,
    s3KeyOutput_etag,

    -- ** S3Object
    s3Object_etag,
    s3Object_bucket,
    s3Object_key,

    -- ** SimulationApplicationConfig
    simulationApplicationConfig_worldConfigs,
    simulationApplicationConfig_tools,
    simulationApplicationConfig_useDefaultTools,
    simulationApplicationConfig_useDefaultUploadConfigurations,
    simulationApplicationConfig_uploadConfigurations,
    simulationApplicationConfig_applicationVersion,
    simulationApplicationConfig_application,
    simulationApplicationConfig_launchConfig,

    -- ** SimulationApplicationSummary
    simulationApplicationSummary_name,
    simulationApplicationSummary_lastUpdatedAt,
    simulationApplicationSummary_arn,
    simulationApplicationSummary_robotSoftwareSuite,
    simulationApplicationSummary_simulationSoftwareSuite,
    simulationApplicationSummary_version,

    -- ** SimulationJob
    simulationJob_tags,
    simulationJob_lastStartedAt,
    simulationJob_name,
    simulationJob_iamRole,
    simulationJob_maxJobDurationInSeconds,
    simulationJob_failureCode,
    simulationJob_clientRequestToken,
    simulationJob_lastUpdatedAt,
    simulationJob_dataSources,
    simulationJob_vpcConfig,
    simulationJob_arn,
    simulationJob_outputLocation,
    simulationJob_status,
    simulationJob_simulationApplications,
    simulationJob_simulationTimeMillis,
    simulationJob_failureBehavior,
    simulationJob_robotApplications,
    simulationJob_networkInterface,
    simulationJob_loggingConfig,
    simulationJob_failureReason,
    simulationJob_compute,

    -- ** SimulationJobBatchSummary
    simulationJobBatchSummary_failedRequestCount,
    simulationJobBatchSummary_lastUpdatedAt,
    simulationJobBatchSummary_createdRequestCount,
    simulationJobBatchSummary_arn,
    simulationJobBatchSummary_status,
    simulationJobBatchSummary_pendingRequestCount,
    simulationJobBatchSummary_createdAt,

    -- ** SimulationJobRequest
    simulationJobRequest_tags,
    simulationJobRequest_iamRole,
    simulationJobRequest_useDefaultApplications,
    simulationJobRequest_dataSources,
    simulationJobRequest_vpcConfig,
    simulationJobRequest_outputLocation,
    simulationJobRequest_simulationApplications,
    simulationJobRequest_failureBehavior,
    simulationJobRequest_robotApplications,
    simulationJobRequest_loggingConfig,
    simulationJobRequest_compute,
    simulationJobRequest_maxJobDurationInSeconds,

    -- ** SimulationJobSummary
    simulationJobSummary_name,
    simulationJobSummary_lastUpdatedAt,
    simulationJobSummary_arn,
    simulationJobSummary_dataSourceNames,
    simulationJobSummary_robotApplicationNames,
    simulationJobSummary_status,
    simulationJobSummary_simulationApplicationNames,
    simulationJobSummary_computeType,

    -- ** SimulationSoftwareSuite
    simulationSoftwareSuite_name,
    simulationSoftwareSuite_version,

    -- ** Source
    source_s3Bucket,
    source_s3Key,
    source_etag,
    source_architecture,

    -- ** SourceConfig
    sourceConfig_s3Bucket,
    sourceConfig_s3Key,
    sourceConfig_architecture,

    -- ** TemplateLocation
    templateLocation_s3Bucket,
    templateLocation_s3Key,

    -- ** TemplateSummary
    templateSummary_name,
    templateSummary_lastUpdatedAt,
    templateSummary_arn,
    templateSummary_createdAt,
    templateSummary_version,

    -- ** Tool
    tool_streamOutputToCloudWatch,
    tool_streamUI,
    tool_exitBehavior,
    tool_name,
    tool_command,

    -- ** UploadConfiguration
    uploadConfiguration_name,
    uploadConfiguration_path,
    uploadConfiguration_uploadBehavior,

    -- ** VPCConfig
    vPCConfig_securityGroups,
    vPCConfig_assignPublicIp,
    vPCConfig_subnets,

    -- ** VPCConfigResponse
    vPCConfigResponse_subnets,
    vPCConfigResponse_securityGroups,
    vPCConfigResponse_vpcId,
    vPCConfigResponse_assignPublicIp,

    -- ** WorldConfig
    worldConfig_world,

    -- ** WorldCount
    worldCount_floorplanCount,
    worldCount_interiorCountPerFloorplan,

    -- ** WorldExportJobSummary
    worldExportJobSummary_arn,
    worldExportJobSummary_status,
    worldExportJobSummary_worlds,
    worldExportJobSummary_createdAt,

    -- ** WorldFailure
    worldFailure_failureCode,
    worldFailure_failureCount,
    worldFailure_sampleFailureReason,

    -- ** WorldGenerationJobSummary
    worldGenerationJobSummary_worldCount,
    worldGenerationJobSummary_succeededWorldCount,
    worldGenerationJobSummary_failedWorldCount,
    worldGenerationJobSummary_arn,
    worldGenerationJobSummary_status,
    worldGenerationJobSummary_createdAt,
    worldGenerationJobSummary_template,

    -- ** WorldSummary
    worldSummary_arn,
    worldSummary_generationJob,
    worldSummary_createdAt,
    worldSummary_template,
  )
where

import Amazonka.RobOMaker.BatchDeleteWorlds
import Amazonka.RobOMaker.BatchDescribeSimulationJob
import Amazonka.RobOMaker.CancelDeploymentJob
import Amazonka.RobOMaker.CancelSimulationJob
import Amazonka.RobOMaker.CancelSimulationJobBatch
import Amazonka.RobOMaker.CancelWorldExportJob
import Amazonka.RobOMaker.CancelWorldGenerationJob
import Amazonka.RobOMaker.CreateDeploymentJob
import Amazonka.RobOMaker.CreateFleet
import Amazonka.RobOMaker.CreateRobot
import Amazonka.RobOMaker.CreateRobotApplication
import Amazonka.RobOMaker.CreateRobotApplicationVersion
import Amazonka.RobOMaker.CreateSimulationApplication
import Amazonka.RobOMaker.CreateSimulationApplicationVersion
import Amazonka.RobOMaker.CreateSimulationJob
import Amazonka.RobOMaker.CreateWorldExportJob
import Amazonka.RobOMaker.CreateWorldGenerationJob
import Amazonka.RobOMaker.CreateWorldTemplate
import Amazonka.RobOMaker.DeleteFleet
import Amazonka.RobOMaker.DeleteRobot
import Amazonka.RobOMaker.DeleteRobotApplication
import Amazonka.RobOMaker.DeleteSimulationApplication
import Amazonka.RobOMaker.DeleteWorldTemplate
import Amazonka.RobOMaker.DeregisterRobot
import Amazonka.RobOMaker.DescribeDeploymentJob
import Amazonka.RobOMaker.DescribeFleet
import Amazonka.RobOMaker.DescribeRobot
import Amazonka.RobOMaker.DescribeRobotApplication
import Amazonka.RobOMaker.DescribeSimulationApplication
import Amazonka.RobOMaker.DescribeSimulationJob
import Amazonka.RobOMaker.DescribeSimulationJobBatch
import Amazonka.RobOMaker.DescribeWorld
import Amazonka.RobOMaker.DescribeWorldExportJob
import Amazonka.RobOMaker.DescribeWorldGenerationJob
import Amazonka.RobOMaker.DescribeWorldTemplate
import Amazonka.RobOMaker.GetWorldTemplateBody
import Amazonka.RobOMaker.ListDeploymentJobs
import Amazonka.RobOMaker.ListFleets
import Amazonka.RobOMaker.ListRobotApplications
import Amazonka.RobOMaker.ListRobots
import Amazonka.RobOMaker.ListSimulationApplications
import Amazonka.RobOMaker.ListSimulationJobBatches
import Amazonka.RobOMaker.ListSimulationJobs
import Amazonka.RobOMaker.ListTagsForResource
import Amazonka.RobOMaker.ListWorldExportJobs
import Amazonka.RobOMaker.ListWorldGenerationJobs
import Amazonka.RobOMaker.ListWorldTemplates
import Amazonka.RobOMaker.ListWorlds
import Amazonka.RobOMaker.RegisterRobot
import Amazonka.RobOMaker.RestartSimulationJob
import Amazonka.RobOMaker.StartSimulationJobBatch
import Amazonka.RobOMaker.SyncDeploymentJob
import Amazonka.RobOMaker.TagResource
import Amazonka.RobOMaker.Types.BatchPolicy
import Amazonka.RobOMaker.Types.Compute
import Amazonka.RobOMaker.Types.ComputeResponse
import Amazonka.RobOMaker.Types.DataSource
import Amazonka.RobOMaker.Types.DataSourceConfig
import Amazonka.RobOMaker.Types.DeploymentApplicationConfig
import Amazonka.RobOMaker.Types.DeploymentConfig
import Amazonka.RobOMaker.Types.DeploymentJob
import Amazonka.RobOMaker.Types.DeploymentLaunchConfig
import Amazonka.RobOMaker.Types.Environment
import Amazonka.RobOMaker.Types.FailedCreateSimulationJobRequest
import Amazonka.RobOMaker.Types.FailureSummary
import Amazonka.RobOMaker.Types.Filter
import Amazonka.RobOMaker.Types.FinishedWorldsSummary
import Amazonka.RobOMaker.Types.Fleet
import Amazonka.RobOMaker.Types.LaunchConfig
import Amazonka.RobOMaker.Types.LoggingConfig
import Amazonka.RobOMaker.Types.NetworkInterface
import Amazonka.RobOMaker.Types.OutputLocation
import Amazonka.RobOMaker.Types.PortForwardingConfig
import Amazonka.RobOMaker.Types.PortMapping
import Amazonka.RobOMaker.Types.ProgressDetail
import Amazonka.RobOMaker.Types.RenderingEngine
import Amazonka.RobOMaker.Types.Robot
import Amazonka.RobOMaker.Types.RobotApplicationConfig
import Amazonka.RobOMaker.Types.RobotApplicationSummary
import Amazonka.RobOMaker.Types.RobotDeployment
import Amazonka.RobOMaker.Types.RobotSoftwareSuite
import Amazonka.RobOMaker.Types.S3KeyOutput
import Amazonka.RobOMaker.Types.S3Object
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
