{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMRContainers.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerException,
    _ResourceNotFoundException,
    _ValidationException,

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

    -- * TemplateParameterDataType
    TemplateParameterDataType (..),

    -- * VirtualClusterState
    VirtualClusterState (..),

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateArn,
    certificate_certificateData,

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
    configurationOverrides_applicationConfiguration,
    configurationOverrides_monitoringConfiguration,

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
    endpoint_arn,
    endpoint_certificateArn,
    endpoint_certificateAuthority,
    endpoint_configurationOverrides,
    endpoint_createdAt,
    endpoint_executionRoleArn,
    endpoint_failureReason,
    endpoint_id,
    endpoint_name,
    endpoint_releaseLabel,
    endpoint_securityGroup,
    endpoint_serverUrl,
    endpoint_state,
    endpoint_stateDetails,
    endpoint_subnetIds,
    endpoint_tags,
    endpoint_type,
    endpoint_virtualClusterId,

    -- * JobDriver
    JobDriver (..),
    newJobDriver,
    jobDriver_sparkSqlJobDriver,
    jobDriver_sparkSubmitJobDriver,

    -- * JobRun
    JobRun (..),
    newJobRun,
    jobRun_arn,
    jobRun_clientToken,
    jobRun_configurationOverrides,
    jobRun_createdAt,
    jobRun_createdBy,
    jobRun_executionRoleArn,
    jobRun_failureReason,
    jobRun_finishedAt,
    jobRun_id,
    jobRun_jobDriver,
    jobRun_name,
    jobRun_releaseLabel,
    jobRun_state,
    jobRun_stateDetails,
    jobRun_tags,
    jobRun_virtualClusterId,

    -- * JobTemplate
    JobTemplate (..),
    newJobTemplate,
    jobTemplate_arn,
    jobTemplate_createdAt,
    jobTemplate_createdBy,
    jobTemplate_decryptionError,
    jobTemplate_id,
    jobTemplate_kmsKeyArn,
    jobTemplate_name,
    jobTemplate_tags,
    jobTemplate_jobTemplateData,

    -- * JobTemplateData
    JobTemplateData (..),
    newJobTemplateData,
    jobTemplateData_configurationOverrides,
    jobTemplateData_jobTags,
    jobTemplateData_parameterConfiguration,
    jobTemplateData_executionRoleArn,
    jobTemplateData_releaseLabel,
    jobTemplateData_jobDriver,

    -- * MonitoringConfiguration
    MonitoringConfiguration (..),
    newMonitoringConfiguration,
    monitoringConfiguration_cloudWatchMonitoringConfiguration,
    monitoringConfiguration_persistentAppUI,
    monitoringConfiguration_s3MonitoringConfiguration,

    -- * ParametricCloudWatchMonitoringConfiguration
    ParametricCloudWatchMonitoringConfiguration (..),
    newParametricCloudWatchMonitoringConfiguration,
    parametricCloudWatchMonitoringConfiguration_logGroupName,
    parametricCloudWatchMonitoringConfiguration_logStreamNamePrefix,

    -- * ParametricConfigurationOverrides
    ParametricConfigurationOverrides (..),
    newParametricConfigurationOverrides,
    parametricConfigurationOverrides_applicationConfiguration,
    parametricConfigurationOverrides_monitoringConfiguration,

    -- * ParametricMonitoringConfiguration
    ParametricMonitoringConfiguration (..),
    newParametricMonitoringConfiguration,
    parametricMonitoringConfiguration_cloudWatchMonitoringConfiguration,
    parametricMonitoringConfiguration_persistentAppUI,
    parametricMonitoringConfiguration_s3MonitoringConfiguration,

    -- * ParametricS3MonitoringConfiguration
    ParametricS3MonitoringConfiguration (..),
    newParametricS3MonitoringConfiguration,
    parametricS3MonitoringConfiguration_logUri,

    -- * S3MonitoringConfiguration
    S3MonitoringConfiguration (..),
    newS3MonitoringConfiguration,
    s3MonitoringConfiguration_logUri,

    -- * SparkSqlJobDriver
    SparkSqlJobDriver (..),
    newSparkSqlJobDriver,
    sparkSqlJobDriver_entryPoint,
    sparkSqlJobDriver_sparkSqlParameters,

    -- * SparkSubmitJobDriver
    SparkSubmitJobDriver (..),
    newSparkSubmitJobDriver,
    sparkSubmitJobDriver_entryPointArguments,
    sparkSubmitJobDriver_sparkSubmitParameters,
    sparkSubmitJobDriver_entryPoint,

    -- * TemplateParameterConfiguration
    TemplateParameterConfiguration (..),
    newTemplateParameterConfiguration,
    templateParameterConfiguration_defaultValue,
    templateParameterConfiguration_type,

    -- * VirtualCluster
    VirtualCluster (..),
    newVirtualCluster,
    virtualCluster_arn,
    virtualCluster_containerProvider,
    virtualCluster_createdAt,
    virtualCluster_id,
    virtualCluster_name,
    virtualCluster_state,
    virtualCluster_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMRContainers.Types.Certificate
import Amazonka.EMRContainers.Types.CloudWatchMonitoringConfiguration
import Amazonka.EMRContainers.Types.Configuration
import Amazonka.EMRContainers.Types.ConfigurationOverrides
import Amazonka.EMRContainers.Types.ContainerInfo
import Amazonka.EMRContainers.Types.ContainerProvider
import Amazonka.EMRContainers.Types.ContainerProviderType
import Amazonka.EMRContainers.Types.EksInfo
import Amazonka.EMRContainers.Types.Endpoint
import Amazonka.EMRContainers.Types.EndpointState
import Amazonka.EMRContainers.Types.FailureReason
import Amazonka.EMRContainers.Types.JobDriver
import Amazonka.EMRContainers.Types.JobRun
import Amazonka.EMRContainers.Types.JobRunState
import Amazonka.EMRContainers.Types.JobTemplate
import Amazonka.EMRContainers.Types.JobTemplateData
import Amazonka.EMRContainers.Types.MonitoringConfiguration
import Amazonka.EMRContainers.Types.ParametricCloudWatchMonitoringConfiguration
import Amazonka.EMRContainers.Types.ParametricConfigurationOverrides
import Amazonka.EMRContainers.Types.ParametricMonitoringConfiguration
import Amazonka.EMRContainers.Types.ParametricS3MonitoringConfiguration
import Amazonka.EMRContainers.Types.PersistentAppUI
import Amazonka.EMRContainers.Types.S3MonitoringConfiguration
import Amazonka.EMRContainers.Types.SparkSqlJobDriver
import Amazonka.EMRContainers.Types.SparkSubmitJobDriver
import Amazonka.EMRContainers.Types.TemplateParameterConfiguration
import Amazonka.EMRContainers.Types.TemplateParameterDataType
import Amazonka.EMRContainers.Types.VirtualCluster
import Amazonka.EMRContainers.Types.VirtualClusterState
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-10-01@ of the Amazon EMR Containers SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "EMRContainers",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "emr-containers",
      Core.signingName = "emr-containers",
      Core.version = "2020-10-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "EMRContainers",
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

-- | This is an internal server exception.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 400

-- | There are invalid parameters in the client request.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
