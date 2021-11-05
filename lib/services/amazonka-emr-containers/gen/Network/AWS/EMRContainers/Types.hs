{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMRContainers.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMRContainers.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * ContainerProviderType
    ContainerProviderType (..),

    -- * EndpointState
    EndpointState (..),

    -- * FailureReason
    FailureReason (..),

    -- * JobRunState
    JobRunState (..),

    -- * PersistentAppUI
    PersistentAppUI (..),

    -- * VirtualClusterState
    VirtualClusterState (..),

    -- * CloudWatchMonitoringConfiguration
    CloudWatchMonitoringConfiguration (..),
    newCloudWatchMonitoringConfiguration,
    cloudWatchMonitoringConfiguration_logStreamNamePrefix,
    cloudWatchMonitoringConfiguration_logGroupName,

    -- * Configuration
    Configuration (..),
    newConfiguration,
    configuration_configurations,
    configuration_properties,
    configuration_classification,

    -- * ConfigurationOverrides
    ConfigurationOverrides (..),
    newConfigurationOverrides,
    configurationOverrides_monitoringConfiguration,
    configurationOverrides_applicationConfiguration,

    -- * ContainerInfo
    ContainerInfo (..),
    newContainerInfo,
    containerInfo_eksInfo,

    -- * ContainerProvider
    ContainerProvider (..),
    newContainerProvider,
    containerProvider_info,
    containerProvider_type,
    containerProvider_id,

    -- * EksInfo
    EksInfo (..),
    newEksInfo,
    eksInfo_namespace,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_failureReason,
    endpoint_state,
    endpoint_arn,
    endpoint_createdAt,
    endpoint_subnetIds,
    endpoint_stateDetails,
    endpoint_certificateArn,
    endpoint_executionRoleArn,
    endpoint_securityGroup,
    endpoint_configurationOverrides,
    endpoint_name,
    endpoint_releaseLabel,
    endpoint_id,
    endpoint_type,
    endpoint_serverUrl,
    endpoint_virtualClusterId,
    endpoint_tags,

    -- * JobDriver
    JobDriver (..),
    newJobDriver,
    jobDriver_sparkSubmitJobDriver,

    -- * JobRun
    JobRun (..),
    newJobRun,
    jobRun_failureReason,
    jobRun_state,
    jobRun_clientToken,
    jobRun_arn,
    jobRun_createdAt,
    jobRun_stateDetails,
    jobRun_createdBy,
    jobRun_executionRoleArn,
    jobRun_jobDriver,
    jobRun_configurationOverrides,
    jobRun_finishedAt,
    jobRun_name,
    jobRun_releaseLabel,
    jobRun_id,
    jobRun_virtualClusterId,
    jobRun_tags,

    -- * MonitoringConfiguration
    MonitoringConfiguration (..),
    newMonitoringConfiguration,
    monitoringConfiguration_persistentAppUI,
    monitoringConfiguration_s3MonitoringConfiguration,
    monitoringConfiguration_cloudWatchMonitoringConfiguration,

    -- * S3MonitoringConfiguration
    S3MonitoringConfiguration (..),
    newS3MonitoringConfiguration,
    s3MonitoringConfiguration_logUri,

    -- * SparkSubmitJobDriver
    SparkSubmitJobDriver (..),
    newSparkSubmitJobDriver,
    sparkSubmitJobDriver_sparkSubmitParameters,
    sparkSubmitJobDriver_entryPointArguments,
    sparkSubmitJobDriver_entryPoint,

    -- * VirtualCluster
    VirtualCluster (..),
    newVirtualCluster,
    virtualCluster_state,
    virtualCluster_arn,
    virtualCluster_createdAt,
    virtualCluster_name,
    virtualCluster_id,
    virtualCluster_containerProvider,
    virtualCluster_tags,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMRContainers.Types.CloudWatchMonitoringConfiguration
import Network.AWS.EMRContainers.Types.Configuration
import Network.AWS.EMRContainers.Types.ConfigurationOverrides
import Network.AWS.EMRContainers.Types.ContainerInfo
import Network.AWS.EMRContainers.Types.ContainerProvider
import Network.AWS.EMRContainers.Types.ContainerProviderType
import Network.AWS.EMRContainers.Types.EksInfo
import Network.AWS.EMRContainers.Types.Endpoint
import Network.AWS.EMRContainers.Types.EndpointState
import Network.AWS.EMRContainers.Types.FailureReason
import Network.AWS.EMRContainers.Types.JobDriver
import Network.AWS.EMRContainers.Types.JobRun
import Network.AWS.EMRContainers.Types.JobRunState
import Network.AWS.EMRContainers.Types.MonitoringConfiguration
import Network.AWS.EMRContainers.Types.PersistentAppUI
import Network.AWS.EMRContainers.Types.S3MonitoringConfiguration
import Network.AWS.EMRContainers.Types.SparkSubmitJobDriver
import Network.AWS.EMRContainers.Types.VirtualCluster
import Network.AWS.EMRContainers.Types.VirtualClusterState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-10-01@ of the Amazon EMR Containers SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "EMRContainers",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "emr-containers",
      Core._serviceSigningName = "emr-containers",
      Core._serviceVersion = "2020-10-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "EMRContainers",
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

-- | There are invalid parameters in the client request.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | This is an internal server exception.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 400
