{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MwAA.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * EnvironmentStatus
    EnvironmentStatus (..),

    -- * LoggingLevel
    LoggingLevel (..),

    -- * Unit
    Unit (..),

    -- * UpdateStatus
    UpdateStatus (..),

    -- * WebserverAccessMode
    WebserverAccessMode (..),

    -- * Dimension
    Dimension (..),
    newDimension,
    dimension_name,
    dimension_value,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_serviceRoleArn,
    environment_schedulers,
    environment_status,
    environment_minWorkers,
    environment_pluginsS3Path,
    environment_webserverAccessMode,
    environment_airflowVersion,
    environment_kmsKey,
    environment_arn,
    environment_createdAt,
    environment_weeklyMaintenanceWindowStart,
    environment_executionRoleArn,
    environment_requirementsS3ObjectVersion,
    environment_lastUpdate,
    environment_sourceBucketArn,
    environment_webserverUrl,
    environment_dagS3Path,
    environment_name,
    environment_pluginsS3ObjectVersion,
    environment_airflowConfigurationOptions,
    environment_loggingConfiguration,
    environment_environmentClass,
    environment_networkConfiguration,
    environment_tags,
    environment_requirementsS3Path,
    environment_maxWorkers,

    -- * LastUpdate
    LastUpdate (..),
    newLastUpdate,
    lastUpdate_status,
    lastUpdate_createdAt,
    lastUpdate_error,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_taskLogs,
    loggingConfiguration_webserverLogs,
    loggingConfiguration_schedulerLogs,
    loggingConfiguration_dagProcessingLogs,
    loggingConfiguration_workerLogs,

    -- * LoggingConfigurationInput
    LoggingConfigurationInput (..),
    newLoggingConfigurationInput,
    loggingConfigurationInput_taskLogs,
    loggingConfigurationInput_webserverLogs,
    loggingConfigurationInput_schedulerLogs,
    loggingConfigurationInput_dagProcessingLogs,
    loggingConfigurationInput_workerLogs,

    -- * MetricDatum
    MetricDatum (..),
    newMetricDatum,
    metricDatum_value,
    metricDatum_dimensions,
    metricDatum_unit,
    metricDatum_statisticValues,
    metricDatum_metricName,
    metricDatum_timestamp,

    -- * ModuleLoggingConfiguration
    ModuleLoggingConfiguration (..),
    newModuleLoggingConfiguration,
    moduleLoggingConfiguration_logLevel,
    moduleLoggingConfiguration_enabled,
    moduleLoggingConfiguration_cloudWatchLogGroupArn,

    -- * ModuleLoggingConfigurationInput
    ModuleLoggingConfigurationInput (..),
    newModuleLoggingConfigurationInput,
    moduleLoggingConfigurationInput_enabled,
    moduleLoggingConfigurationInput_logLevel,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_securityGroupIds,
    networkConfiguration_subnetIds,

    -- * StatisticSet
    StatisticSet (..),
    newStatisticSet,
    statisticSet_sampleCount,
    statisticSet_maximum,
    statisticSet_minimum,
    statisticSet_sum,

    -- * UpdateError
    UpdateError (..),
    newUpdateError,
    updateError_errorCode,
    updateError_errorMessage,

    -- * UpdateNetworkConfigurationInput
    UpdateNetworkConfigurationInput (..),
    newUpdateNetworkConfigurationInput,
    updateNetworkConfigurationInput_securityGroupIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MwAA.Types.Dimension
import Amazonka.MwAA.Types.Environment
import Amazonka.MwAA.Types.EnvironmentStatus
import Amazonka.MwAA.Types.LastUpdate
import Amazonka.MwAA.Types.LoggingConfiguration
import Amazonka.MwAA.Types.LoggingConfigurationInput
import Amazonka.MwAA.Types.LoggingLevel
import Amazonka.MwAA.Types.MetricDatum
import Amazonka.MwAA.Types.ModuleLoggingConfiguration
import Amazonka.MwAA.Types.ModuleLoggingConfigurationInput
import Amazonka.MwAA.Types.NetworkConfiguration
import Amazonka.MwAA.Types.StatisticSet
import Amazonka.MwAA.Types.Unit
import Amazonka.MwAA.Types.UpdateError
import Amazonka.MwAA.Types.UpdateNetworkConfigurationInput
import Amazonka.MwAA.Types.UpdateStatus
import Amazonka.MwAA.Types.WebserverAccessMode
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-07-01@ of the Amazon MWAA SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MwAA",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "airflow",
      Core._serviceSigningName = "airflow",
      Core._serviceVersion = "2020-07-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "MwAA",
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

-- | ValidationException: The provided input is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Access to the Airflow Web UI or CLI has been Denied. Please follow the
-- MWAA user guide to setup permissions to access the Web UI and CLI
-- functionality.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | InternalServerException: An internal error has occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | ResourceNotFoundException: The resource is not available.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
