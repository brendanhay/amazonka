{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RobOMaker.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentDeploymentException,
    _IdempotentParameterMismatchException,
    _InternalServerException,
    _InvalidParameterException,
    _LimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,
    _ThrottlingException,

    -- * Architecture
    Architecture (..),

    -- * ComputeType
    ComputeType (..),

    -- * DataSourceType
    DataSourceType (..),

    -- * ExitBehavior
    ExitBehavior (..),

    -- * FailureBehavior
    FailureBehavior (..),

    -- * RenderingEngineType
    RenderingEngineType (..),

    -- * RobotSoftwareSuiteType
    RobotSoftwareSuiteType (..),

    -- * RobotSoftwareSuiteVersionType
    RobotSoftwareSuiteVersionType (..),

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
    batchPolicy_maxConcurrency,
    batchPolicy_timeoutInSeconds,

    -- * Compute
    Compute (..),
    newCompute,
    compute_computeType,
    compute_gpuUnitLimit,
    compute_simulationUnitLimit,

    -- * ComputeResponse
    ComputeResponse (..),
    newComputeResponse,
    computeResponse_computeType,
    computeResponse_gpuUnitLimit,
    computeResponse_simulationUnitLimit,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_destination,
    dataSource_name,
    dataSource_s3Bucket,
    dataSource_s3Keys,
    dataSource_type,

    -- * DataSourceConfig
    DataSourceConfig (..),
    newDataSourceConfig,
    dataSourceConfig_destination,
    dataSourceConfig_type,
    dataSourceConfig_name,
    dataSourceConfig_s3Bucket,
    dataSourceConfig_s3Keys,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_uri,

    -- * FailedCreateSimulationJobRequest
    FailedCreateSimulationJobRequest (..),
    newFailedCreateSimulationJobRequest,
    failedCreateSimulationJobRequest_failedAt,
    failedCreateSimulationJobRequest_failureCode,
    failedCreateSimulationJobRequest_failureReason,
    failedCreateSimulationJobRequest_request,

    -- * FailureSummary
    FailureSummary (..),
    newFailureSummary,
    failureSummary_failures,
    failureSummary_totalFailureCount,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * FinishedWorldsSummary
    FinishedWorldsSummary (..),
    newFinishedWorldsSummary,
    finishedWorldsSummary_failureSummary,
    finishedWorldsSummary_finishedCount,
    finishedWorldsSummary_succeededWorlds,

    -- * LaunchConfig
    LaunchConfig (..),
    newLaunchConfig,
    launchConfig_command,
    launchConfig_environmentVariables,
    launchConfig_launchFile,
    launchConfig_packageName,
    launchConfig_portForwardingConfig,
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
    outputLocation_s3Bucket,
    outputLocation_s3Prefix,

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

    -- * RenderingEngine
    RenderingEngine (..),
    newRenderingEngine,
    renderingEngine_name,
    renderingEngine_version,

    -- * RobotApplicationConfig
    RobotApplicationConfig (..),
    newRobotApplicationConfig,
    robotApplicationConfig_applicationVersion,
    robotApplicationConfig_tools,
    robotApplicationConfig_uploadConfigurations,
    robotApplicationConfig_useDefaultTools,
    robotApplicationConfig_useDefaultUploadConfigurations,
    robotApplicationConfig_application,
    robotApplicationConfig_launchConfig,

    -- * RobotApplicationSummary
    RobotApplicationSummary (..),
    newRobotApplicationSummary,
    robotApplicationSummary_arn,
    robotApplicationSummary_lastUpdatedAt,
    robotApplicationSummary_name,
    robotApplicationSummary_robotSoftwareSuite,
    robotApplicationSummary_version,

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

    -- * SimulationApplicationConfig
    SimulationApplicationConfig (..),
    newSimulationApplicationConfig,
    simulationApplicationConfig_applicationVersion,
    simulationApplicationConfig_tools,
    simulationApplicationConfig_uploadConfigurations,
    simulationApplicationConfig_useDefaultTools,
    simulationApplicationConfig_useDefaultUploadConfigurations,
    simulationApplicationConfig_worldConfigs,
    simulationApplicationConfig_application,
    simulationApplicationConfig_launchConfig,

    -- * SimulationApplicationSummary
    SimulationApplicationSummary (..),
    newSimulationApplicationSummary,
    simulationApplicationSummary_arn,
    simulationApplicationSummary_lastUpdatedAt,
    simulationApplicationSummary_name,
    simulationApplicationSummary_robotSoftwareSuite,
    simulationApplicationSummary_simulationSoftwareSuite,
    simulationApplicationSummary_version,

    -- * SimulationJob
    SimulationJob (..),
    newSimulationJob,
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

    -- * SimulationJobBatchSummary
    SimulationJobBatchSummary (..),
    newSimulationJobBatchSummary,
    simulationJobBatchSummary_arn,
    simulationJobBatchSummary_createdAt,
    simulationJobBatchSummary_createdRequestCount,
    simulationJobBatchSummary_failedRequestCount,
    simulationJobBatchSummary_lastUpdatedAt,
    simulationJobBatchSummary_pendingRequestCount,
    simulationJobBatchSummary_status,

    -- * SimulationJobRequest
    SimulationJobRequest (..),
    newSimulationJobRequest,
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

    -- * SimulationJobSummary
    SimulationJobSummary (..),
    newSimulationJobSummary,
    simulationJobSummary_arn,
    simulationJobSummary_computeType,
    simulationJobSummary_dataSourceNames,
    simulationJobSummary_lastUpdatedAt,
    simulationJobSummary_name,
    simulationJobSummary_robotApplicationNames,
    simulationJobSummary_simulationApplicationNames,
    simulationJobSummary_status,

    -- * SimulationSoftwareSuite
    SimulationSoftwareSuite (..),
    newSimulationSoftwareSuite,
    simulationSoftwareSuite_name,
    simulationSoftwareSuite_version,

    -- * Source
    Source (..),
    newSource,
    source_architecture,
    source_etag,
    source_s3Bucket,
    source_s3Key,

    -- * SourceConfig
    SourceConfig (..),
    newSourceConfig,
    sourceConfig_architecture,
    sourceConfig_s3Bucket,
    sourceConfig_s3Key,

    -- * TemplateLocation
    TemplateLocation (..),
    newTemplateLocation,
    templateLocation_s3Bucket,
    templateLocation_s3Key,

    -- * TemplateSummary
    TemplateSummary (..),
    newTemplateSummary,
    templateSummary_arn,
    templateSummary_createdAt,
    templateSummary_lastUpdatedAt,
    templateSummary_name,
    templateSummary_version,

    -- * Tool
    Tool (..),
    newTool,
    tool_exitBehavior,
    tool_streamOutputToCloudWatch,
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
    vPCConfig_assignPublicIp,
    vPCConfig_securityGroups,
    vPCConfig_subnets,

    -- * VPCConfigResponse
    VPCConfigResponse (..),
    newVPCConfigResponse,
    vPCConfigResponse_assignPublicIp,
    vPCConfigResponse_securityGroups,
    vPCConfigResponse_subnets,
    vPCConfigResponse_vpcId,

    -- * WorldConfig
    WorldConfig (..),
    newWorldConfig,
    worldConfig_world,

    -- * WorldCount
    WorldCount (..),
    newWorldCount,
    worldCount_floorplanCount,
    worldCount_interiorCountPerFloorplan,

    -- * WorldExportJobSummary
    WorldExportJobSummary (..),
    newWorldExportJobSummary,
    worldExportJobSummary_arn,
    worldExportJobSummary_createdAt,
    worldExportJobSummary_outputLocation,
    worldExportJobSummary_status,
    worldExportJobSummary_worlds,

    -- * WorldFailure
    WorldFailure (..),
    newWorldFailure,
    worldFailure_failureCode,
    worldFailure_failureCount,
    worldFailure_sampleFailureReason,

    -- * WorldGenerationJobSummary
    WorldGenerationJobSummary (..),
    newWorldGenerationJobSummary,
    worldGenerationJobSummary_arn,
    worldGenerationJobSummary_createdAt,
    worldGenerationJobSummary_failedWorldCount,
    worldGenerationJobSummary_status,
    worldGenerationJobSummary_succeededWorldCount,
    worldGenerationJobSummary_template,
    worldGenerationJobSummary_worldCount,

    -- * WorldSummary
    WorldSummary (..),
    newWorldSummary,
    worldSummary_arn,
    worldSummary_createdAt,
    worldSummary_generationJob,
    worldSummary_template,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.Architecture
import Amazonka.RobOMaker.Types.BatchPolicy
import Amazonka.RobOMaker.Types.Compute
import Amazonka.RobOMaker.Types.ComputeResponse
import Amazonka.RobOMaker.Types.ComputeType
import Amazonka.RobOMaker.Types.DataSource
import Amazonka.RobOMaker.Types.DataSourceConfig
import Amazonka.RobOMaker.Types.DataSourceType
import Amazonka.RobOMaker.Types.Environment
import Amazonka.RobOMaker.Types.ExitBehavior
import Amazonka.RobOMaker.Types.FailedCreateSimulationJobRequest
import Amazonka.RobOMaker.Types.FailureBehavior
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
import Amazonka.RobOMaker.Types.RenderingEngineType
import Amazonka.RobOMaker.Types.RobotApplicationConfig
import Amazonka.RobOMaker.Types.RobotApplicationSummary
import Amazonka.RobOMaker.Types.RobotSoftwareSuite
import Amazonka.RobOMaker.Types.RobotSoftwareSuiteType
import Amazonka.RobOMaker.Types.RobotSoftwareSuiteVersionType
import Amazonka.RobOMaker.Types.S3KeyOutput
import Amazonka.RobOMaker.Types.SimulationApplicationConfig
import Amazonka.RobOMaker.Types.SimulationApplicationSummary
import Amazonka.RobOMaker.Types.SimulationJob
import Amazonka.RobOMaker.Types.SimulationJobBatchErrorCode
import Amazonka.RobOMaker.Types.SimulationJobBatchStatus
import Amazonka.RobOMaker.Types.SimulationJobBatchSummary
import Amazonka.RobOMaker.Types.SimulationJobErrorCode
import Amazonka.RobOMaker.Types.SimulationJobRequest
import Amazonka.RobOMaker.Types.SimulationJobStatus
import Amazonka.RobOMaker.Types.SimulationJobSummary
import Amazonka.RobOMaker.Types.SimulationSoftwareSuite
import Amazonka.RobOMaker.Types.SimulationSoftwareSuiteType
import Amazonka.RobOMaker.Types.Source
import Amazonka.RobOMaker.Types.SourceConfig
import Amazonka.RobOMaker.Types.TemplateLocation
import Amazonka.RobOMaker.Types.TemplateSummary
import Amazonka.RobOMaker.Types.Tool
import Amazonka.RobOMaker.Types.UploadBehavior
import Amazonka.RobOMaker.Types.UploadConfiguration
import Amazonka.RobOMaker.Types.VPCConfig
import Amazonka.RobOMaker.Types.VPCConfigResponse
import Amazonka.RobOMaker.Types.WorldConfig
import Amazonka.RobOMaker.Types.WorldCount
import Amazonka.RobOMaker.Types.WorldExportJobErrorCode
import Amazonka.RobOMaker.Types.WorldExportJobStatus
import Amazonka.RobOMaker.Types.WorldExportJobSummary
import Amazonka.RobOMaker.Types.WorldFailure
import Amazonka.RobOMaker.Types.WorldGenerationJobErrorCode
import Amazonka.RobOMaker.Types.WorldGenerationJobStatus
import Amazonka.RobOMaker.Types.WorldGenerationJobSummary
import Amazonka.RobOMaker.Types.WorldSummary
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-06-29@ of the Amazon RoboMaker SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "RobOMaker",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "robomaker",
      Core.signingName = "robomaker",
      Core.version = "2018-06-29",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "RobOMaker",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The failure percentage threshold percentage was met.
_ConcurrentDeploymentException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentDeploymentException =
  Core._MatchServiceError
    defaultService
    "ConcurrentDeploymentException"
    Prelude.. Core.hasStatus 400

-- | The request uses the same client token as a previous, but non-identical
-- request. Do not reuse a client token with different requests, unless the
-- requests are identical.
_IdempotentParameterMismatchException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"
    Prelude.. Core.hasStatus 400

-- | AWS RoboMaker experienced a service issue. Try your call again.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | A parameter specified in a request is not valid, is unsupported, or
-- cannot be used. The returned message provides an explanation of the
-- error value.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The requested resource exceeds the maximum number allowed, or the number
-- of concurrent stream requests exceeds the maximum number allowed.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | The specified resource does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 400

-- | The request has failed due to a temporary failure of the server.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | AWS RoboMaker is temporarily unable to process the request. Try your
-- call again.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 400
