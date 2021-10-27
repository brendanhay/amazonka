{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RobOMaker.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidParameterException,
    _ResourceAlreadyExistsException,
    _ThrottlingException,
    _InternalServerException,
    _ConcurrentDeploymentException,
    _ServiceUnavailableException,
    _IdempotentParameterMismatchException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * Architecture
    Architecture (..),

    -- * ComputeType
    ComputeType (..),

    -- * DataSourceType
    DataSourceType (..),

    -- * DeploymentJobErrorCode
    DeploymentJobErrorCode (..),

    -- * DeploymentStatus
    DeploymentStatus (..),

    -- * ExitBehavior
    ExitBehavior (..),

    -- * FailureBehavior
    FailureBehavior (..),

    -- * RenderingEngineType
    RenderingEngineType (..),

    -- * RobotDeploymentStep
    RobotDeploymentStep (..),

    -- * RobotSoftwareSuiteType
    RobotSoftwareSuiteType (..),

    -- * RobotSoftwareSuiteVersionType
    RobotSoftwareSuiteVersionType (..),

    -- * RobotStatus
    RobotStatus (..),

    -- * SimulationJobBatchErrorCode
    SimulationJobBatchErrorCode (..),

    -- * SimulationJobBatchStatus
    SimulationJobBatchStatus (..),

    -- * SimulationJobErrorCode
    SimulationJobErrorCode (..),

    -- * SimulationJobStatus
    SimulationJobStatus (..),

    -- * SimulationSoftwareSuiteType
    SimulationSoftwareSuiteType (..),

    -- * UploadBehavior
    UploadBehavior (..),

    -- * WorldExportJobErrorCode
    WorldExportJobErrorCode (..),

    -- * WorldExportJobStatus
    WorldExportJobStatus (..),

    -- * WorldGenerationJobErrorCode
    WorldGenerationJobErrorCode (..),

    -- * WorldGenerationJobStatus
    WorldGenerationJobStatus (..),

    -- * BatchPolicy
    BatchPolicy (..),
    newBatchPolicy,
    batchPolicy_timeoutInSeconds,
    batchPolicy_maxConcurrency,

    -- * Compute
    Compute (..),
    newCompute,
    compute_simulationUnitLimit,
    compute_gpuUnitLimit,
    compute_computeType,

    -- * ComputeResponse
    ComputeResponse (..),
    newComputeResponse,
    computeResponse_simulationUnitLimit,
    computeResponse_gpuUnitLimit,
    computeResponse_computeType,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_s3Keys,
    dataSource_destination,
    dataSource_name,
    dataSource_type,
    dataSource_s3Bucket,

    -- * DataSourceConfig
    DataSourceConfig (..),
    newDataSourceConfig,
    dataSourceConfig_destination,
    dataSourceConfig_type,
    dataSourceConfig_name,
    dataSourceConfig_s3Bucket,
    dataSourceConfig_s3Keys,

    -- * DeploymentApplicationConfig
    DeploymentApplicationConfig (..),
    newDeploymentApplicationConfig,
    deploymentApplicationConfig_application,
    deploymentApplicationConfig_applicationVersion,
    deploymentApplicationConfig_launchConfig,

    -- * DeploymentConfig
    DeploymentConfig (..),
    newDeploymentConfig,
    deploymentConfig_concurrentDeploymentPercentage,
    deploymentConfig_downloadConditionFile,
    deploymentConfig_failureThresholdPercentage,
    deploymentConfig_robotDeploymentTimeoutInSeconds,

    -- * DeploymentJob
    DeploymentJob (..),
    newDeploymentJob,
    deploymentJob_failureReason,
    deploymentJob_status,
    deploymentJob_deploymentApplicationConfigs,
    deploymentJob_arn,
    deploymentJob_createdAt,
    deploymentJob_failureCode,
    deploymentJob_deploymentConfig,
    deploymentJob_fleet,

    -- * DeploymentLaunchConfig
    DeploymentLaunchConfig (..),
    newDeploymentLaunchConfig,
    deploymentLaunchConfig_preLaunchFile,
    deploymentLaunchConfig_postLaunchFile,
    deploymentLaunchConfig_environmentVariables,
    deploymentLaunchConfig_packageName,
    deploymentLaunchConfig_launchFile,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_uri,

    -- * FailedCreateSimulationJobRequest
    FailedCreateSimulationJobRequest (..),
    newFailedCreateSimulationJobRequest,
    failedCreateSimulationJobRequest_failureReason,
    failedCreateSimulationJobRequest_failureCode,
    failedCreateSimulationJobRequest_failedAt,
    failedCreateSimulationJobRequest_request,

    -- * FailureSummary
    FailureSummary (..),
    newFailureSummary,
    failureSummary_failures,
    failureSummary_totalFailureCount,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * FinishedWorldsSummary
    FinishedWorldsSummary (..),
    newFinishedWorldsSummary,
    finishedWorldsSummary_succeededWorlds,
    finishedWorldsSummary_failureSummary,
    finishedWorldsSummary_finishedCount,

    -- * Fleet
    Fleet (..),
    newFleet,
    fleet_lastDeploymentJob,
    fleet_lastDeploymentStatus,
    fleet_arn,
    fleet_createdAt,
    fleet_name,
    fleet_lastDeploymentTime,

    -- * LaunchConfig
    LaunchConfig (..),
    newLaunchConfig,
    launchConfig_command,
    launchConfig_packageName,
    launchConfig_portForwardingConfig,
    launchConfig_launchFile,
    launchConfig_environmentVariables,
    launchConfig_streamUI,

    -- * LoggingConfig
    LoggingConfig (..),
    newLoggingConfig,
    loggingConfig_recordAllRosTopics,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,
    networkInterface_publicIpAddress,

    -- * OutputLocation
    OutputLocation (..),
    newOutputLocation,
    outputLocation_s3Prefix,
    outputLocation_s3Bucket,

    -- * PortForwardingConfig
    PortForwardingConfig (..),
    newPortForwardingConfig,
    portForwardingConfig_portMappings,

    -- * PortMapping
    PortMapping (..),
    newPortMapping,
    portMapping_enableOnPublicIp,
    portMapping_jobPort,
    portMapping_applicationPort,

    -- * ProgressDetail
    ProgressDetail (..),
    newProgressDetail,
    progressDetail_currentProgress,
    progressDetail_estimatedTimeRemainingSeconds,
    progressDetail_targetResource,
    progressDetail_percentDone,

    -- * RenderingEngine
    RenderingEngine (..),
    newRenderingEngine,
    renderingEngine_name,
    renderingEngine_version,

    -- * Robot
    Robot (..),
    newRobot,
    robot_lastDeploymentJob,
    robot_status,
    robot_arn,
    robot_createdAt,
    robot_greenGrassGroupId,
    robot_fleetArn,
    robot_name,
    robot_architecture,
    robot_lastDeploymentTime,

    -- * RobotApplicationConfig
    RobotApplicationConfig (..),
    newRobotApplicationConfig,
    robotApplicationConfig_useDefaultUploadConfigurations,
    robotApplicationConfig_useDefaultTools,
    robotApplicationConfig_applicationVersion,
    robotApplicationConfig_uploadConfigurations,
    robotApplicationConfig_tools,
    robotApplicationConfig_application,
    robotApplicationConfig_launchConfig,

    -- * RobotApplicationSummary
    RobotApplicationSummary (..),
    newRobotApplicationSummary,
    robotApplicationSummary_lastUpdatedAt,
    robotApplicationSummary_arn,
    robotApplicationSummary_name,
    robotApplicationSummary_version,
    robotApplicationSummary_robotSoftwareSuite,

    -- * RobotDeployment
    RobotDeployment (..),
    newRobotDeployment,
    robotDeployment_deploymentStartTime,
    robotDeployment_failureReason,
    robotDeployment_status,
    robotDeployment_arn,
    robotDeployment_failureCode,
    robotDeployment_progressDetail,
    robotDeployment_deploymentFinishTime,

    -- * RobotSoftwareSuite
    RobotSoftwareSuite (..),
    newRobotSoftwareSuite,
    robotSoftwareSuite_name,
    robotSoftwareSuite_version,

    -- * S3KeyOutput
    S3KeyOutput (..),
    newS3KeyOutput,
    s3KeyOutput_etag,
    s3KeyOutput_s3Key,

    -- * S3Object
    S3Object (..),
    newS3Object,
    s3Object_etag,
    s3Object_bucket,
    s3Object_key,

    -- * SimulationApplicationConfig
    SimulationApplicationConfig (..),
    newSimulationApplicationConfig,
    simulationApplicationConfig_useDefaultUploadConfigurations,
    simulationApplicationConfig_useDefaultTools,
    simulationApplicationConfig_applicationVersion,
    simulationApplicationConfig_uploadConfigurations,
    simulationApplicationConfig_tools,
    simulationApplicationConfig_worldConfigs,
    simulationApplicationConfig_application,
    simulationApplicationConfig_launchConfig,

    -- * SimulationApplicationSummary
    SimulationApplicationSummary (..),
    newSimulationApplicationSummary,
    simulationApplicationSummary_lastUpdatedAt,
    simulationApplicationSummary_arn,
    simulationApplicationSummary_name,
    simulationApplicationSummary_version,
    simulationApplicationSummary_simulationSoftwareSuite,
    simulationApplicationSummary_robotSoftwareSuite,

    -- * SimulationJob
    SimulationJob (..),
    newSimulationJob,
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

    -- * SimulationJobBatchSummary
    SimulationJobBatchSummary (..),
    newSimulationJobBatchSummary,
    simulationJobBatchSummary_status,
    simulationJobBatchSummary_createdRequestCount,
    simulationJobBatchSummary_lastUpdatedAt,
    simulationJobBatchSummary_arn,
    simulationJobBatchSummary_createdAt,
    simulationJobBatchSummary_pendingRequestCount,
    simulationJobBatchSummary_failedRequestCount,

    -- * SimulationJobRequest
    SimulationJobRequest (..),
    newSimulationJobRequest,
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

    -- * SimulationJobSummary
    SimulationJobSummary (..),
    newSimulationJobSummary,
    simulationJobSummary_status,
    simulationJobSummary_robotApplicationNames,
    simulationJobSummary_lastUpdatedAt,
    simulationJobSummary_arn,
    simulationJobSummary_name,
    simulationJobSummary_simulationApplicationNames,
    simulationJobSummary_computeType,
    simulationJobSummary_dataSourceNames,

    -- * SimulationSoftwareSuite
    SimulationSoftwareSuite (..),
    newSimulationSoftwareSuite,
    simulationSoftwareSuite_name,
    simulationSoftwareSuite_version,

    -- * Source
    Source (..),
    newSource,
    source_etag,
    source_s3Key,
    source_architecture,
    source_s3Bucket,

    -- * SourceConfig
    SourceConfig (..),
    newSourceConfig,
    sourceConfig_s3Key,
    sourceConfig_architecture,
    sourceConfig_s3Bucket,

    -- * TemplateLocation
    TemplateLocation (..),
    newTemplateLocation,
    templateLocation_s3Bucket,
    templateLocation_s3Key,

    -- * TemplateSummary
    TemplateSummary (..),
    newTemplateSummary,
    templateSummary_lastUpdatedAt,
    templateSummary_arn,
    templateSummary_createdAt,
    templateSummary_name,
    templateSummary_version,

    -- * Tool
    Tool (..),
    newTool,
    tool_streamOutputToCloudWatch,
    tool_exitBehavior,
    tool_streamUI,
    tool_name,
    tool_command,

    -- * UploadConfiguration
    UploadConfiguration (..),
    newUploadConfiguration,
    uploadConfiguration_name,
    uploadConfiguration_path,
    uploadConfiguration_uploadBehavior,

    -- * VPCConfig
    VPCConfig (..),
    newVPCConfig,
    vPCConfig_securityGroups,
    vPCConfig_assignPublicIp,
    vPCConfig_subnets,

    -- * VPCConfigResponse
    VPCConfigResponse (..),
    newVPCConfigResponse,
    vPCConfigResponse_securityGroups,
    vPCConfigResponse_vpcId,
    vPCConfigResponse_subnets,
    vPCConfigResponse_assignPublicIp,

    -- * WorldConfig
    WorldConfig (..),
    newWorldConfig,
    worldConfig_world,

    -- * WorldCount
    WorldCount (..),
    newWorldCount,
    worldCount_interiorCountPerFloorplan,
    worldCount_floorplanCount,

    -- * WorldExportJobSummary
    WorldExportJobSummary (..),
    newWorldExportJobSummary,
    worldExportJobSummary_status,
    worldExportJobSummary_arn,
    worldExportJobSummary_createdAt,
    worldExportJobSummary_worlds,

    -- * WorldFailure
    WorldFailure (..),
    newWorldFailure,
    worldFailure_sampleFailureReason,
    worldFailure_failureCode,
    worldFailure_failureCount,

    -- * WorldGenerationJobSummary
    WorldGenerationJobSummary (..),
    newWorldGenerationJobSummary,
    worldGenerationJobSummary_status,
    worldGenerationJobSummary_arn,
    worldGenerationJobSummary_createdAt,
    worldGenerationJobSummary_worldCount,
    worldGenerationJobSummary_succeededWorldCount,
    worldGenerationJobSummary_failedWorldCount,
    worldGenerationJobSummary_template,

    -- * WorldSummary
    WorldSummary (..),
    newWorldSummary,
    worldSummary_arn,
    worldSummary_createdAt,
    worldSummary_template,
    worldSummary_generationJob,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RobOMaker.Types.Architecture
import Network.AWS.RobOMaker.Types.BatchPolicy
import Network.AWS.RobOMaker.Types.Compute
import Network.AWS.RobOMaker.Types.ComputeResponse
import Network.AWS.RobOMaker.Types.ComputeType
import Network.AWS.RobOMaker.Types.DataSource
import Network.AWS.RobOMaker.Types.DataSourceConfig
import Network.AWS.RobOMaker.Types.DataSourceType
import Network.AWS.RobOMaker.Types.DeploymentApplicationConfig
import Network.AWS.RobOMaker.Types.DeploymentConfig
import Network.AWS.RobOMaker.Types.DeploymentJob
import Network.AWS.RobOMaker.Types.DeploymentJobErrorCode
import Network.AWS.RobOMaker.Types.DeploymentLaunchConfig
import Network.AWS.RobOMaker.Types.DeploymentStatus
import Network.AWS.RobOMaker.Types.Environment
import Network.AWS.RobOMaker.Types.ExitBehavior
import Network.AWS.RobOMaker.Types.FailedCreateSimulationJobRequest
import Network.AWS.RobOMaker.Types.FailureBehavior
import Network.AWS.RobOMaker.Types.FailureSummary
import Network.AWS.RobOMaker.Types.Filter
import Network.AWS.RobOMaker.Types.FinishedWorldsSummary
import Network.AWS.RobOMaker.Types.Fleet
import Network.AWS.RobOMaker.Types.LaunchConfig
import Network.AWS.RobOMaker.Types.LoggingConfig
import Network.AWS.RobOMaker.Types.NetworkInterface
import Network.AWS.RobOMaker.Types.OutputLocation
import Network.AWS.RobOMaker.Types.PortForwardingConfig
import Network.AWS.RobOMaker.Types.PortMapping
import Network.AWS.RobOMaker.Types.ProgressDetail
import Network.AWS.RobOMaker.Types.RenderingEngine
import Network.AWS.RobOMaker.Types.RenderingEngineType
import Network.AWS.RobOMaker.Types.Robot
import Network.AWS.RobOMaker.Types.RobotApplicationConfig
import Network.AWS.RobOMaker.Types.RobotApplicationSummary
import Network.AWS.RobOMaker.Types.RobotDeployment
import Network.AWS.RobOMaker.Types.RobotDeploymentStep
import Network.AWS.RobOMaker.Types.RobotSoftwareSuite
import Network.AWS.RobOMaker.Types.RobotSoftwareSuiteType
import Network.AWS.RobOMaker.Types.RobotSoftwareSuiteVersionType
import Network.AWS.RobOMaker.Types.RobotStatus
import Network.AWS.RobOMaker.Types.S3KeyOutput
import Network.AWS.RobOMaker.Types.S3Object
import Network.AWS.RobOMaker.Types.SimulationApplicationConfig
import Network.AWS.RobOMaker.Types.SimulationApplicationSummary
import Network.AWS.RobOMaker.Types.SimulationJob
import Network.AWS.RobOMaker.Types.SimulationJobBatchErrorCode
import Network.AWS.RobOMaker.Types.SimulationJobBatchStatus
import Network.AWS.RobOMaker.Types.SimulationJobBatchSummary
import Network.AWS.RobOMaker.Types.SimulationJobErrorCode
import Network.AWS.RobOMaker.Types.SimulationJobRequest
import Network.AWS.RobOMaker.Types.SimulationJobStatus
import Network.AWS.RobOMaker.Types.SimulationJobSummary
import Network.AWS.RobOMaker.Types.SimulationSoftwareSuite
import Network.AWS.RobOMaker.Types.SimulationSoftwareSuiteType
import Network.AWS.RobOMaker.Types.Source
import Network.AWS.RobOMaker.Types.SourceConfig
import Network.AWS.RobOMaker.Types.TemplateLocation
import Network.AWS.RobOMaker.Types.TemplateSummary
import Network.AWS.RobOMaker.Types.Tool
import Network.AWS.RobOMaker.Types.UploadBehavior
import Network.AWS.RobOMaker.Types.UploadConfiguration
import Network.AWS.RobOMaker.Types.VPCConfig
import Network.AWS.RobOMaker.Types.VPCConfigResponse
import Network.AWS.RobOMaker.Types.WorldConfig
import Network.AWS.RobOMaker.Types.WorldCount
import Network.AWS.RobOMaker.Types.WorldExportJobErrorCode
import Network.AWS.RobOMaker.Types.WorldExportJobStatus
import Network.AWS.RobOMaker.Types.WorldExportJobSummary
import Network.AWS.RobOMaker.Types.WorldFailure
import Network.AWS.RobOMaker.Types.WorldGenerationJobErrorCode
import Network.AWS.RobOMaker.Types.WorldGenerationJobStatus
import Network.AWS.RobOMaker.Types.WorldGenerationJobSummary
import Network.AWS.RobOMaker.Types.WorldSummary
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-06-29@ of the Amazon RoboMaker SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "RobOMaker",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "robomaker",
      Core._serviceSigningName = "robomaker",
      Core._serviceVersion = "2018-06-29",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "RobOMaker",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | A parameter specified in a request is not valid, is unsupported, or
-- cannot be used. The returned message provides an explanation of the
-- error value.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | AWS RoboMaker is temporarily unable to process the request. Try your
-- call again.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 400

-- | AWS RoboMaker experienced a service issue. Try your call again.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The failure percentage threshold percentage was met.
_ConcurrentDeploymentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentDeploymentException =
  Core._MatchServiceError
    defaultService
    "ConcurrentDeploymentException"
    Prelude.. Core.hasStatus 400

-- | The request has failed due to a temporary failure of the server.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The request uses the same client token as a previous, but non-identical
-- request. Do not reuse a client token with different requests, unless the
-- requests are identical.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"
    Prelude.. Core.hasStatus 400

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 400

-- | The requested resource exceeds the maximum number allowed, or the number
-- of concurrent stream requests exceeds the maximum number allowed.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400
