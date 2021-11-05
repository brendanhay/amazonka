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

    -- ** DescribeWorldExportJob
    describeWorldExportJob_job,
    describeWorldExportJobResponse_failureReason,
    describeWorldExportJobResponse_status,
    describeWorldExportJobResponse_arn,
    describeWorldExportJobResponse_createdAt,
    describeWorldExportJobResponse_failureCode,
    describeWorldExportJobResponse_outputLocation,
    describeWorldExportJobResponse_clientRequestToken,
    describeWorldExportJobResponse_iamRole,
    describeWorldExportJobResponse_worlds,
    describeWorldExportJobResponse_tags,
    describeWorldExportJobResponse_httpStatus,

    -- ** BatchDeleteWorlds
    batchDeleteWorlds_worlds,
    batchDeleteWorldsResponse_unprocessedWorlds,
    batchDeleteWorldsResponse_httpStatus,

    -- ** GetWorldTemplateBody
    getWorldTemplateBody_template,
    getWorldTemplateBody_generationJob,
    getWorldTemplateBodyResponse_templateBody,
    getWorldTemplateBodyResponse_httpStatus,

    -- ** DeleteFleet
    deleteFleet_fleet,
    deleteFleetResponse_httpStatus,

    -- ** CreateWorldGenerationJob
    createWorldGenerationJob_worldTags,
    createWorldGenerationJob_clientRequestToken,
    createWorldGenerationJob_tags,
    createWorldGenerationJob_template,
    createWorldGenerationJob_worldCount,
    createWorldGenerationJobResponse_status,
    createWorldGenerationJobResponse_arn,
    createWorldGenerationJobResponse_createdAt,
    createWorldGenerationJobResponse_failureCode,
    createWorldGenerationJobResponse_worldCount,
    createWorldGenerationJobResponse_template,
    createWorldGenerationJobResponse_worldTags,
    createWorldGenerationJobResponse_clientRequestToken,
    createWorldGenerationJobResponse_tags,
    createWorldGenerationJobResponse_httpStatus,

    -- ** ListRobotApplications
    listRobotApplications_versionQualifier,
    listRobotApplications_filters,
    listRobotApplications_nextToken,
    listRobotApplications_maxResults,
    listRobotApplicationsResponse_robotApplicationSummaries,
    listRobotApplicationsResponse_nextToken,
    listRobotApplicationsResponse_httpStatus,

    -- ** UpdateRobotApplication
    updateRobotApplication_currentRevisionId,
    updateRobotApplication_environment,
    updateRobotApplication_sources,
    updateRobotApplication_application,
    updateRobotApplication_robotSoftwareSuite,
    updateRobotApplicationResponse_lastUpdatedAt,
    updateRobotApplicationResponse_arn,
    updateRobotApplicationResponse_environment,
    updateRobotApplicationResponse_sources,
    updateRobotApplicationResponse_name,
    updateRobotApplicationResponse_version,
    updateRobotApplicationResponse_robotSoftwareSuite,
    updateRobotApplicationResponse_revisionId,
    updateRobotApplicationResponse_httpStatus,

    -- ** DeleteRobotApplication
    deleteRobotApplication_applicationVersion,
    deleteRobotApplication_application,
    deleteRobotApplicationResponse_httpStatus,

    -- ** CreateSimulationApplicationVersion
    createSimulationApplicationVersion_currentRevisionId,
    createSimulationApplicationVersion_s3Etags,
    createSimulationApplicationVersion_imageDigest,
    createSimulationApplicationVersion_application,
    createSimulationApplicationVersionResponse_renderingEngine,
    createSimulationApplicationVersionResponse_lastUpdatedAt,
    createSimulationApplicationVersionResponse_arn,
    createSimulationApplicationVersionResponse_environment,
    createSimulationApplicationVersionResponse_sources,
    createSimulationApplicationVersionResponse_name,
    createSimulationApplicationVersionResponse_version,
    createSimulationApplicationVersionResponse_simulationSoftwareSuite,
    createSimulationApplicationVersionResponse_robotSoftwareSuite,
    createSimulationApplicationVersionResponse_revisionId,
    createSimulationApplicationVersionResponse_httpStatus,

    -- ** ListDeploymentJobs
    listDeploymentJobs_filters,
    listDeploymentJobs_nextToken,
    listDeploymentJobs_maxResults,
    listDeploymentJobsResponse_deploymentJobs,
    listDeploymentJobsResponse_nextToken,
    listDeploymentJobsResponse_httpStatus,

    -- ** DescribeWorld
    describeWorld_world,
    describeWorldResponse_worldDescriptionBody,
    describeWorldResponse_arn,
    describeWorldResponse_createdAt,
    describeWorldResponse_template,
    describeWorldResponse_tags,
    describeWorldResponse_generationJob,
    describeWorldResponse_httpStatus,

    -- ** CancelSimulationJob
    cancelSimulationJob_job,
    cancelSimulationJobResponse_httpStatus,

    -- ** CreateRobotApplication
    createRobotApplication_environment,
    createRobotApplication_sources,
    createRobotApplication_tags,
    createRobotApplication_name,
    createRobotApplication_robotSoftwareSuite,
    createRobotApplicationResponse_lastUpdatedAt,
    createRobotApplicationResponse_arn,
    createRobotApplicationResponse_environment,
    createRobotApplicationResponse_sources,
    createRobotApplicationResponse_name,
    createRobotApplicationResponse_version,
    createRobotApplicationResponse_robotSoftwareSuite,
    createRobotApplicationResponse_revisionId,
    createRobotApplicationResponse_tags,
    createRobotApplicationResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateDeploymentJob
    createDeploymentJob_deploymentConfig,
    createDeploymentJob_tags,
    createDeploymentJob_clientRequestToken,
    createDeploymentJob_fleet,
    createDeploymentJob_deploymentApplicationConfigs,
    createDeploymentJobResponse_failureReason,
    createDeploymentJobResponse_status,
    createDeploymentJobResponse_deploymentApplicationConfigs,
    createDeploymentJobResponse_arn,
    createDeploymentJobResponse_createdAt,
    createDeploymentJobResponse_failureCode,
    createDeploymentJobResponse_deploymentConfig,
    createDeploymentJobResponse_fleet,
    createDeploymentJobResponse_tags,
    createDeploymentJobResponse_httpStatus,

    -- ** RegisterRobot
    registerRobot_fleet,
    registerRobot_robot,
    registerRobotResponse_robot,
    registerRobotResponse_fleet,
    registerRobotResponse_httpStatus,

    -- ** ListRobots
    listRobots_filters,
    listRobots_nextToken,
    listRobots_maxResults,
    listRobotsResponse_robots,
    listRobotsResponse_nextToken,
    listRobotsResponse_httpStatus,

    -- ** CreateWorldExportJob
    createWorldExportJob_clientRequestToken,
    createWorldExportJob_tags,
    createWorldExportJob_worlds,
    createWorldExportJob_outputLocation,
    createWorldExportJob_iamRole,
    createWorldExportJobResponse_status,
    createWorldExportJobResponse_arn,
    createWorldExportJobResponse_createdAt,
    createWorldExportJobResponse_failureCode,
    createWorldExportJobResponse_outputLocation,
    createWorldExportJobResponse_clientRequestToken,
    createWorldExportJobResponse_iamRole,
    createWorldExportJobResponse_tags,
    createWorldExportJobResponse_httpStatus,

    -- ** BatchDescribeSimulationJob
    batchDescribeSimulationJob_jobs,
    batchDescribeSimulationJobResponse_unprocessedJobs,
    batchDescribeSimulationJobResponse_jobs,
    batchDescribeSimulationJobResponse_httpStatus,

    -- ** CreateSimulationApplication
    createSimulationApplication_renderingEngine,
    createSimulationApplication_environment,
    createSimulationApplication_sources,
    createSimulationApplication_tags,
    createSimulationApplication_name,
    createSimulationApplication_simulationSoftwareSuite,
    createSimulationApplication_robotSoftwareSuite,
    createSimulationApplicationResponse_renderingEngine,
    createSimulationApplicationResponse_lastUpdatedAt,
    createSimulationApplicationResponse_arn,
    createSimulationApplicationResponse_environment,
    createSimulationApplicationResponse_sources,
    createSimulationApplicationResponse_name,
    createSimulationApplicationResponse_version,
    createSimulationApplicationResponse_simulationSoftwareSuite,
    createSimulationApplicationResponse_robotSoftwareSuite,
    createSimulationApplicationResponse_revisionId,
    createSimulationApplicationResponse_tags,
    createSimulationApplicationResponse_httpStatus,

    -- ** StartSimulationJobBatch
    startSimulationJobBatch_batchPolicy,
    startSimulationJobBatch_clientRequestToken,
    startSimulationJobBatch_tags,
    startSimulationJobBatch_createSimulationJobRequests,
    startSimulationJobBatchResponse_failureReason,
    startSimulationJobBatchResponse_status,
    startSimulationJobBatchResponse_arn,
    startSimulationJobBatchResponse_createdAt,
    startSimulationJobBatchResponse_failureCode,
    startSimulationJobBatchResponse_failedRequests,
    startSimulationJobBatchResponse_batchPolicy,
    startSimulationJobBatchResponse_createdRequests,
    startSimulationJobBatchResponse_pendingRequests,
    startSimulationJobBatchResponse_clientRequestToken,
    startSimulationJobBatchResponse_tags,
    startSimulationJobBatchResponse_httpStatus,

    -- ** CreateRobot
    createRobot_tags,
    createRobot_name,
    createRobot_architecture,
    createRobot_greengrassGroupId,
    createRobotResponse_arn,
    createRobotResponse_createdAt,
    createRobotResponse_greengrassGroupId,
    createRobotResponse_name,
    createRobotResponse_architecture,
    createRobotResponse_tags,
    createRobotResponse_httpStatus,

    -- ** DescribeFleet
    describeFleet_fleet,
    describeFleetResponse_lastDeploymentJob,
    describeFleetResponse_lastDeploymentStatus,
    describeFleetResponse_robots,
    describeFleetResponse_arn,
    describeFleetResponse_createdAt,
    describeFleetResponse_name,
    describeFleetResponse_lastDeploymentTime,
    describeFleetResponse_tags,
    describeFleetResponse_httpStatus,

    -- ** ListWorldTemplates
    listWorldTemplates_nextToken,
    listWorldTemplates_maxResults,
    listWorldTemplatesResponse_templateSummaries,
    listWorldTemplatesResponse_nextToken,
    listWorldTemplatesResponse_httpStatus,

    -- ** DescribeRobotApplication
    describeRobotApplication_applicationVersion,
    describeRobotApplication_application,
    describeRobotApplicationResponse_lastUpdatedAt,
    describeRobotApplicationResponse_arn,
    describeRobotApplicationResponse_environment,
    describeRobotApplicationResponse_sources,
    describeRobotApplicationResponse_name,
    describeRobotApplicationResponse_imageDigest,
    describeRobotApplicationResponse_version,
    describeRobotApplicationResponse_robotSoftwareSuite,
    describeRobotApplicationResponse_revisionId,
    describeRobotApplicationResponse_tags,
    describeRobotApplicationResponse_httpStatus,

    -- ** RestartSimulationJob
    restartSimulationJob_job,
    restartSimulationJobResponse_httpStatus,

    -- ** DescribeSimulationJob
    describeSimulationJob_job,
    describeSimulationJobResponse_failureReason,
    describeSimulationJobResponse_failureBehavior,
    describeSimulationJobResponse_status,
    describeSimulationJobResponse_lastUpdatedAt,
    describeSimulationJobResponse_arn,
    describeSimulationJobResponse_robotApplications,
    describeSimulationJobResponse_failureCode,
    describeSimulationJobResponse_compute,
    describeSimulationJobResponse_networkInterface,
    describeSimulationJobResponse_dataSources,
    describeSimulationJobResponse_name,
    describeSimulationJobResponse_vpcConfig,
    describeSimulationJobResponse_outputLocation,
    describeSimulationJobResponse_simulationApplications,
    describeSimulationJobResponse_simulationTimeMillis,
    describeSimulationJobResponse_clientRequestToken,
    describeSimulationJobResponse_lastStartedAt,
    describeSimulationJobResponse_loggingConfig,
    describeSimulationJobResponse_iamRole,
    describeSimulationJobResponse_maxJobDurationInSeconds,
    describeSimulationJobResponse_tags,
    describeSimulationJobResponse_httpStatus,

    -- ** DeregisterRobot
    deregisterRobot_fleet,
    deregisterRobot_robot,
    deregisterRobotResponse_robot,
    deregisterRobotResponse_fleet,
    deregisterRobotResponse_httpStatus,

    -- ** DescribeSimulationApplication
    describeSimulationApplication_applicationVersion,
    describeSimulationApplication_application,
    describeSimulationApplicationResponse_renderingEngine,
    describeSimulationApplicationResponse_lastUpdatedAt,
    describeSimulationApplicationResponse_arn,
    describeSimulationApplicationResponse_environment,
    describeSimulationApplicationResponse_sources,
    describeSimulationApplicationResponse_name,
    describeSimulationApplicationResponse_imageDigest,
    describeSimulationApplicationResponse_version,
    describeSimulationApplicationResponse_simulationSoftwareSuite,
    describeSimulationApplicationResponse_robotSoftwareSuite,
    describeSimulationApplicationResponse_revisionId,
    describeSimulationApplicationResponse_tags,
    describeSimulationApplicationResponse_httpStatus,

    -- ** ListSimulationJobBatches
    listSimulationJobBatches_filters,
    listSimulationJobBatches_nextToken,
    listSimulationJobBatches_maxResults,
    listSimulationJobBatchesResponse_nextToken,
    listSimulationJobBatchesResponse_simulationJobBatchSummaries,
    listSimulationJobBatchesResponse_httpStatus,

    -- ** ListFleets
    listFleets_filters,
    listFleets_nextToken,
    listFleets_maxResults,
    listFleetsResponse_fleetDetails,
    listFleetsResponse_nextToken,
    listFleetsResponse_httpStatus,

    -- ** DescribeWorldTemplate
    describeWorldTemplate_template,
    describeWorldTemplateResponse_lastUpdatedAt,
    describeWorldTemplateResponse_arn,
    describeWorldTemplateResponse_createdAt,
    describeWorldTemplateResponse_name,
    describeWorldTemplateResponse_version,
    describeWorldTemplateResponse_clientRequestToken,
    describeWorldTemplateResponse_tags,
    describeWorldTemplateResponse_httpStatus,

    -- ** CancelWorldExportJob
    cancelWorldExportJob_job,
    cancelWorldExportJobResponse_httpStatus,

    -- ** ListWorldGenerationJobs
    listWorldGenerationJobs_filters,
    listWorldGenerationJobs_nextToken,
    listWorldGenerationJobs_maxResults,
    listWorldGenerationJobsResponse_nextToken,
    listWorldGenerationJobsResponse_httpStatus,
    listWorldGenerationJobsResponse_worldGenerationJobSummaries,

    -- ** CreateFleet
    createFleet_tags,
    createFleet_name,
    createFleetResponse_arn,
    createFleetResponse_createdAt,
    createFleetResponse_name,
    createFleetResponse_tags,
    createFleetResponse_httpStatus,

    -- ** CancelWorldGenerationJob
    cancelWorldGenerationJob_job,
    cancelWorldGenerationJobResponse_httpStatus,

    -- ** DescribeSimulationJobBatch
    describeSimulationJobBatch_batch,
    describeSimulationJobBatchResponse_failureReason,
    describeSimulationJobBatchResponse_status,
    describeSimulationJobBatchResponse_lastUpdatedAt,
    describeSimulationJobBatchResponse_arn,
    describeSimulationJobBatchResponse_createdAt,
    describeSimulationJobBatchResponse_failureCode,
    describeSimulationJobBatchResponse_failedRequests,
    describeSimulationJobBatchResponse_batchPolicy,
    describeSimulationJobBatchResponse_createdRequests,
    describeSimulationJobBatchResponse_pendingRequests,
    describeSimulationJobBatchResponse_clientRequestToken,
    describeSimulationJobBatchResponse_tags,
    describeSimulationJobBatchResponse_httpStatus,

    -- ** ListSimulationJobs
    listSimulationJobs_filters,
    listSimulationJobs_nextToken,
    listSimulationJobs_maxResults,
    listSimulationJobsResponse_nextToken,
    listSimulationJobsResponse_httpStatus,
    listSimulationJobsResponse_simulationJobSummaries,

    -- ** DeleteRobot
    deleteRobot_robot,
    deleteRobotResponse_httpStatus,

    -- ** DeleteSimulationApplication
    deleteSimulationApplication_applicationVersion,
    deleteSimulationApplication_application,
    deleteSimulationApplicationResponse_httpStatus,

    -- ** UpdateSimulationApplication
    updateSimulationApplication_renderingEngine,
    updateSimulationApplication_currentRevisionId,
    updateSimulationApplication_environment,
    updateSimulationApplication_sources,
    updateSimulationApplication_application,
    updateSimulationApplication_simulationSoftwareSuite,
    updateSimulationApplication_robotSoftwareSuite,
    updateSimulationApplicationResponse_renderingEngine,
    updateSimulationApplicationResponse_lastUpdatedAt,
    updateSimulationApplicationResponse_arn,
    updateSimulationApplicationResponse_environment,
    updateSimulationApplicationResponse_sources,
    updateSimulationApplicationResponse_name,
    updateSimulationApplicationResponse_version,
    updateSimulationApplicationResponse_simulationSoftwareSuite,
    updateSimulationApplicationResponse_robotSoftwareSuite,
    updateSimulationApplicationResponse_revisionId,
    updateSimulationApplicationResponse_httpStatus,

    -- ** CreateSimulationJob
    createSimulationJob_failureBehavior,
    createSimulationJob_robotApplications,
    createSimulationJob_compute,
    createSimulationJob_dataSources,
    createSimulationJob_vpcConfig,
    createSimulationJob_outputLocation,
    createSimulationJob_simulationApplications,
    createSimulationJob_clientRequestToken,
    createSimulationJob_loggingConfig,
    createSimulationJob_tags,
    createSimulationJob_maxJobDurationInSeconds,
    createSimulationJob_iamRole,
    createSimulationJobResponse_failureBehavior,
    createSimulationJobResponse_status,
    createSimulationJobResponse_lastUpdatedAt,
    createSimulationJobResponse_arn,
    createSimulationJobResponse_robotApplications,
    createSimulationJobResponse_failureCode,
    createSimulationJobResponse_compute,
    createSimulationJobResponse_dataSources,
    createSimulationJobResponse_vpcConfig,
    createSimulationJobResponse_outputLocation,
    createSimulationJobResponse_simulationApplications,
    createSimulationJobResponse_simulationTimeMillis,
    createSimulationJobResponse_clientRequestToken,
    createSimulationJobResponse_lastStartedAt,
    createSimulationJobResponse_loggingConfig,
    createSimulationJobResponse_iamRole,
    createSimulationJobResponse_maxJobDurationInSeconds,
    createSimulationJobResponse_tags,
    createSimulationJobResponse_httpStatus,

    -- ** ListWorldExportJobs
    listWorldExportJobs_filters,
    listWorldExportJobs_nextToken,
    listWorldExportJobs_maxResults,
    listWorldExportJobsResponse_nextToken,
    listWorldExportJobsResponse_httpStatus,
    listWorldExportJobsResponse_worldExportJobSummaries,

    -- ** ListSimulationApplications
    listSimulationApplications_versionQualifier,
    listSimulationApplications_filters,
    listSimulationApplications_nextToken,
    listSimulationApplications_maxResults,
    listSimulationApplicationsResponse_nextToken,
    listSimulationApplicationsResponse_simulationApplicationSummaries,
    listSimulationApplicationsResponse_httpStatus,

    -- ** CreateRobotApplicationVersion
    createRobotApplicationVersion_currentRevisionId,
    createRobotApplicationVersion_s3Etags,
    createRobotApplicationVersion_imageDigest,
    createRobotApplicationVersion_application,
    createRobotApplicationVersionResponse_lastUpdatedAt,
    createRobotApplicationVersionResponse_arn,
    createRobotApplicationVersionResponse_environment,
    createRobotApplicationVersionResponse_sources,
    createRobotApplicationVersionResponse_name,
    createRobotApplicationVersionResponse_version,
    createRobotApplicationVersionResponse_robotSoftwareSuite,
    createRobotApplicationVersionResponse_revisionId,
    createRobotApplicationVersionResponse_httpStatus,

    -- ** CancelDeploymentJob
    cancelDeploymentJob_job,
    cancelDeploymentJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListWorlds
    listWorlds_filters,
    listWorlds_nextToken,
    listWorlds_maxResults,
    listWorldsResponse_nextToken,
    listWorldsResponse_worldSummaries,
    listWorldsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeDeploymentJob
    describeDeploymentJob_job,
    describeDeploymentJobResponse_failureReason,
    describeDeploymentJobResponse_status,
    describeDeploymentJobResponse_deploymentApplicationConfigs,
    describeDeploymentJobResponse_arn,
    describeDeploymentJobResponse_createdAt,
    describeDeploymentJobResponse_failureCode,
    describeDeploymentJobResponse_deploymentConfig,
    describeDeploymentJobResponse_fleet,
    describeDeploymentJobResponse_robotDeploymentSummary,
    describeDeploymentJobResponse_tags,
    describeDeploymentJobResponse_httpStatus,

    -- ** DeleteWorldTemplate
    deleteWorldTemplate_template,
    deleteWorldTemplateResponse_httpStatus,

    -- ** UpdateWorldTemplate
    updateWorldTemplate_name,
    updateWorldTemplate_templateLocation,
    updateWorldTemplate_templateBody,
    updateWorldTemplate_template,
    updateWorldTemplateResponse_lastUpdatedAt,
    updateWorldTemplateResponse_arn,
    updateWorldTemplateResponse_createdAt,
    updateWorldTemplateResponse_name,
    updateWorldTemplateResponse_httpStatus,

    -- ** DescribeWorldGenerationJob
    describeWorldGenerationJob_job,
    describeWorldGenerationJobResponse_failureReason,
    describeWorldGenerationJobResponse_status,
    describeWorldGenerationJobResponse_arn,
    describeWorldGenerationJobResponse_createdAt,
    describeWorldGenerationJobResponse_failureCode,
    describeWorldGenerationJobResponse_worldCount,
    describeWorldGenerationJobResponse_finishedWorldsSummary,
    describeWorldGenerationJobResponse_template,
    describeWorldGenerationJobResponse_worldTags,
    describeWorldGenerationJobResponse_clientRequestToken,
    describeWorldGenerationJobResponse_tags,
    describeWorldGenerationJobResponse_httpStatus,

    -- ** CreateWorldTemplate
    createWorldTemplate_name,
    createWorldTemplate_templateLocation,
    createWorldTemplate_templateBody,
    createWorldTemplate_clientRequestToken,
    createWorldTemplate_tags,
    createWorldTemplateResponse_arn,
    createWorldTemplateResponse_createdAt,
    createWorldTemplateResponse_name,
    createWorldTemplateResponse_clientRequestToken,
    createWorldTemplateResponse_tags,
    createWorldTemplateResponse_httpStatus,

    -- ** CancelSimulationJobBatch
    cancelSimulationJobBatch_batch,
    cancelSimulationJobBatchResponse_httpStatus,

    -- ** DescribeRobot
    describeRobot_robot,
    describeRobotResponse_lastDeploymentJob,
    describeRobotResponse_status,
    describeRobotResponse_arn,
    describeRobotResponse_createdAt,
    describeRobotResponse_greengrassGroupId,
    describeRobotResponse_fleetArn,
    describeRobotResponse_name,
    describeRobotResponse_architecture,
    describeRobotResponse_lastDeploymentTime,
    describeRobotResponse_tags,
    describeRobotResponse_httpStatus,

    -- ** SyncDeploymentJob
    syncDeploymentJob_clientRequestToken,
    syncDeploymentJob_fleet,
    syncDeploymentJobResponse_failureReason,
    syncDeploymentJobResponse_status,
    syncDeploymentJobResponse_deploymentApplicationConfigs,
    syncDeploymentJobResponse_arn,
    syncDeploymentJobResponse_createdAt,
    syncDeploymentJobResponse_failureCode,
    syncDeploymentJobResponse_deploymentConfig,
    syncDeploymentJobResponse_fleet,
    syncDeploymentJobResponse_httpStatus,

    -- * Types

    -- ** BatchPolicy
    batchPolicy_timeoutInSeconds,
    batchPolicy_maxConcurrency,

    -- ** Compute
    compute_simulationUnitLimit,
    compute_gpuUnitLimit,
    compute_computeType,

    -- ** ComputeResponse
    computeResponse_simulationUnitLimit,
    computeResponse_gpuUnitLimit,
    computeResponse_computeType,

    -- ** DataSource
    dataSource_s3Keys,
    dataSource_destination,
    dataSource_name,
    dataSource_type,
    dataSource_s3Bucket,

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
    deploymentConfig_concurrentDeploymentPercentage,
    deploymentConfig_downloadConditionFile,
    deploymentConfig_failureThresholdPercentage,
    deploymentConfig_robotDeploymentTimeoutInSeconds,

    -- ** DeploymentJob
    deploymentJob_failureReason,
    deploymentJob_status,
    deploymentJob_deploymentApplicationConfigs,
    deploymentJob_arn,
    deploymentJob_createdAt,
    deploymentJob_failureCode,
    deploymentJob_deploymentConfig,
    deploymentJob_fleet,

    -- ** DeploymentLaunchConfig
    deploymentLaunchConfig_preLaunchFile,
    deploymentLaunchConfig_postLaunchFile,
    deploymentLaunchConfig_environmentVariables,
    deploymentLaunchConfig_packageName,
    deploymentLaunchConfig_launchFile,

    -- ** Environment
    environment_uri,

    -- ** FailedCreateSimulationJobRequest
    failedCreateSimulationJobRequest_failureReason,
    failedCreateSimulationJobRequest_failureCode,
    failedCreateSimulationJobRequest_failedAt,
    failedCreateSimulationJobRequest_request,

    -- ** FailureSummary
    failureSummary_failures,
    failureSummary_totalFailureCount,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** FinishedWorldsSummary
    finishedWorldsSummary_succeededWorlds,
    finishedWorldsSummary_failureSummary,
    finishedWorldsSummary_finishedCount,

    -- ** Fleet
    fleet_lastDeploymentJob,
    fleet_lastDeploymentStatus,
    fleet_arn,
    fleet_createdAt,
    fleet_name,
    fleet_lastDeploymentTime,

    -- ** LaunchConfig
    launchConfig_command,
    launchConfig_packageName,
    launchConfig_portForwardingConfig,
    launchConfig_launchFile,
    launchConfig_environmentVariables,
    launchConfig_streamUI,

    -- ** LoggingConfig
    loggingConfig_recordAllRosTopics,

    -- ** NetworkInterface
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,
    networkInterface_publicIpAddress,

    -- ** OutputLocation
    outputLocation_s3Prefix,
    outputLocation_s3Bucket,

    -- ** PortForwardingConfig
    portForwardingConfig_portMappings,

    -- ** PortMapping
    portMapping_enableOnPublicIp,
    portMapping_jobPort,
    portMapping_applicationPort,

    -- ** ProgressDetail
    progressDetail_currentProgress,
    progressDetail_estimatedTimeRemainingSeconds,
    progressDetail_targetResource,
    progressDetail_percentDone,

    -- ** RenderingEngine
    renderingEngine_name,
    renderingEngine_version,

    -- ** Robot
    robot_lastDeploymentJob,
    robot_status,
    robot_arn,
    robot_createdAt,
    robot_greenGrassGroupId,
    robot_fleetArn,
    robot_name,
    robot_architecture,
    robot_lastDeploymentTime,

    -- ** RobotApplicationConfig
    robotApplicationConfig_useDefaultUploadConfigurations,
    robotApplicationConfig_useDefaultTools,
    robotApplicationConfig_applicationVersion,
    robotApplicationConfig_uploadConfigurations,
    robotApplicationConfig_tools,
    robotApplicationConfig_application,
    robotApplicationConfig_launchConfig,

    -- ** RobotApplicationSummary
    robotApplicationSummary_lastUpdatedAt,
    robotApplicationSummary_arn,
    robotApplicationSummary_name,
    robotApplicationSummary_version,
    robotApplicationSummary_robotSoftwareSuite,

    -- ** RobotDeployment
    robotDeployment_deploymentStartTime,
    robotDeployment_failureReason,
    robotDeployment_status,
    robotDeployment_arn,
    robotDeployment_failureCode,
    robotDeployment_progressDetail,
    robotDeployment_deploymentFinishTime,

    -- ** RobotSoftwareSuite
    robotSoftwareSuite_name,
    robotSoftwareSuite_version,

    -- ** S3KeyOutput
    s3KeyOutput_etag,
    s3KeyOutput_s3Key,

    -- ** S3Object
    s3Object_etag,
    s3Object_bucket,
    s3Object_key,

    -- ** SimulationApplicationConfig
    simulationApplicationConfig_useDefaultUploadConfigurations,
    simulationApplicationConfig_useDefaultTools,
    simulationApplicationConfig_applicationVersion,
    simulationApplicationConfig_uploadConfigurations,
    simulationApplicationConfig_tools,
    simulationApplicationConfig_worldConfigs,
    simulationApplicationConfig_application,
    simulationApplicationConfig_launchConfig,

    -- ** SimulationApplicationSummary
    simulationApplicationSummary_lastUpdatedAt,
    simulationApplicationSummary_arn,
    simulationApplicationSummary_name,
    simulationApplicationSummary_version,
    simulationApplicationSummary_simulationSoftwareSuite,
    simulationApplicationSummary_robotSoftwareSuite,

    -- ** SimulationJob
    simulationJob_failureReason,
    simulationJob_failureBehavior,
    simulationJob_status,
    simulationJob_lastUpdatedAt,
    simulationJob_arn,
    simulationJob_robotApplications,
    simulationJob_failureCode,
    simulationJob_compute,
    simulationJob_networkInterface,
    simulationJob_dataSources,
    simulationJob_name,
    simulationJob_vpcConfig,
    simulationJob_outputLocation,
    simulationJob_simulationApplications,
    simulationJob_simulationTimeMillis,
    simulationJob_clientRequestToken,
    simulationJob_lastStartedAt,
    simulationJob_loggingConfig,
    simulationJob_iamRole,
    simulationJob_maxJobDurationInSeconds,
    simulationJob_tags,

    -- ** SimulationJobBatchSummary
    simulationJobBatchSummary_status,
    simulationJobBatchSummary_createdRequestCount,
    simulationJobBatchSummary_lastUpdatedAt,
    simulationJobBatchSummary_arn,
    simulationJobBatchSummary_createdAt,
    simulationJobBatchSummary_pendingRequestCount,
    simulationJobBatchSummary_failedRequestCount,

    -- ** SimulationJobRequest
    simulationJobRequest_failureBehavior,
    simulationJobRequest_robotApplications,
    simulationJobRequest_compute,
    simulationJobRequest_dataSources,
    simulationJobRequest_useDefaultApplications,
    simulationJobRequest_vpcConfig,
    simulationJobRequest_outputLocation,
    simulationJobRequest_simulationApplications,
    simulationJobRequest_loggingConfig,
    simulationJobRequest_iamRole,
    simulationJobRequest_tags,
    simulationJobRequest_maxJobDurationInSeconds,

    -- ** SimulationJobSummary
    simulationJobSummary_status,
    simulationJobSummary_robotApplicationNames,
    simulationJobSummary_lastUpdatedAt,
    simulationJobSummary_arn,
    simulationJobSummary_name,
    simulationJobSummary_simulationApplicationNames,
    simulationJobSummary_computeType,
    simulationJobSummary_dataSourceNames,

    -- ** SimulationSoftwareSuite
    simulationSoftwareSuite_name,
    simulationSoftwareSuite_version,

    -- ** Source
    source_etag,
    source_s3Key,
    source_architecture,
    source_s3Bucket,

    -- ** SourceConfig
    sourceConfig_s3Key,
    sourceConfig_architecture,
    sourceConfig_s3Bucket,

    -- ** TemplateLocation
    templateLocation_s3Bucket,
    templateLocation_s3Key,

    -- ** TemplateSummary
    templateSummary_lastUpdatedAt,
    templateSummary_arn,
    templateSummary_createdAt,
    templateSummary_name,
    templateSummary_version,

    -- ** Tool
    tool_streamOutputToCloudWatch,
    tool_exitBehavior,
    tool_streamUI,
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
    vPCConfigResponse_securityGroups,
    vPCConfigResponse_vpcId,
    vPCConfigResponse_subnets,
    vPCConfigResponse_assignPublicIp,

    -- ** WorldConfig
    worldConfig_world,

    -- ** WorldCount
    worldCount_interiorCountPerFloorplan,
    worldCount_floorplanCount,

    -- ** WorldExportJobSummary
    worldExportJobSummary_status,
    worldExportJobSummary_arn,
    worldExportJobSummary_createdAt,
    worldExportJobSummary_worlds,

    -- ** WorldFailure
    worldFailure_sampleFailureReason,
    worldFailure_failureCode,
    worldFailure_failureCount,

    -- ** WorldGenerationJobSummary
    worldGenerationJobSummary_status,
    worldGenerationJobSummary_arn,
    worldGenerationJobSummary_createdAt,
    worldGenerationJobSummary_worldCount,
    worldGenerationJobSummary_succeededWorldCount,
    worldGenerationJobSummary_failedWorldCount,
    worldGenerationJobSummary_template,

    -- ** WorldSummary
    worldSummary_arn,
    worldSummary_createdAt,
    worldSummary_template,
    worldSummary_generationJob,
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
