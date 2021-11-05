{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppConfig.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppConfig.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _PayloadTooLargeException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _InternalServerException,
    _ResourceNotFoundException,
    _BadRequestException,

    -- * DeploymentEventType
    DeploymentEventType (..),

    -- * DeploymentState
    DeploymentState (..),

    -- * EnvironmentState
    EnvironmentState (..),

    -- * GrowthType
    GrowthType (..),

    -- * ReplicateTo
    ReplicateTo (..),

    -- * TriggeredBy
    TriggeredBy (..),

    -- * ValidatorType
    ValidatorType (..),

    -- * Application
    Application (..),
    newApplication,
    application_name,
    application_id,
    application_description,

    -- * ConfigurationProfile
    ConfigurationProfile (..),
    newConfigurationProfile,
    configurationProfile_retrievalRoleArn,
    configurationProfile_validators,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_name,
    configurationProfile_id,
    configurationProfile_description,

    -- * ConfigurationProfileSummary
    ConfigurationProfileSummary (..),
    newConfigurationProfileSummary,
    configurationProfileSummary_locationUri,
    configurationProfileSummary_applicationId,
    configurationProfileSummary_name,
    configurationProfileSummary_id,
    configurationProfileSummary_validatorTypes,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_growthFactor,
    deployment_configurationName,
    deployment_state,
    deployment_deploymentStrategyId,
    deployment_deploymentNumber,
    deployment_configurationVersion,
    deployment_eventLog,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_applicationId,
    deployment_deploymentDurationInMinutes,
    deployment_environmentId,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_finalBakeTimeInMinutes,
    deployment_description,
    deployment_configurationProfileId,
    deployment_growthType,

    -- * DeploymentEvent
    DeploymentEvent (..),
    newDeploymentEvent,
    deploymentEvent_triggeredBy,
    deploymentEvent_occurredAt,
    deploymentEvent_eventType,
    deploymentEvent_description,

    -- * DeploymentStrategy
    DeploymentStrategy (..),
    newDeploymentStrategy,
    deploymentStrategy_growthFactor,
    deploymentStrategy_replicateTo,
    deploymentStrategy_name,
    deploymentStrategy_id,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_growthType,

    -- * DeploymentSummary
    DeploymentSummary (..),
    newDeploymentSummary,
    deploymentSummary_growthFactor,
    deploymentSummary_configurationName,
    deploymentSummary_state,
    deploymentSummary_deploymentNumber,
    deploymentSummary_configurationVersion,
    deploymentSummary_percentageComplete,
    deploymentSummary_startedAt,
    deploymentSummary_deploymentDurationInMinutes,
    deploymentSummary_completedAt,
    deploymentSummary_finalBakeTimeInMinutes,
    deploymentSummary_growthType,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_state,
    environment_monitors,
    environment_applicationId,
    environment_name,
    environment_id,
    environment_description,

    -- * HostedConfigurationVersion
    HostedConfigurationVersion (..),
    newHostedConfigurationVersion,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_contentType,

    -- * HostedConfigurationVersionSummary
    HostedConfigurationVersionSummary (..),
    newHostedConfigurationVersionSummary,
    hostedConfigurationVersionSummary_versionNumber,
    hostedConfigurationVersionSummary_applicationId,
    hostedConfigurationVersionSummary_description,
    hostedConfigurationVersionSummary_configurationProfileId,
    hostedConfigurationVersionSummary_contentType,

    -- * Monitor
    Monitor (..),
    newMonitor,
    monitor_alarmRoleArn,
    monitor_alarmArn,

    -- * Validator
    Validator (..),
    newValidator,
    validator_type,
    validator_content,
  )
where

import Network.AWS.AppConfig.Types.Application
import Network.AWS.AppConfig.Types.ConfigurationProfile
import Network.AWS.AppConfig.Types.ConfigurationProfileSummary
import Network.AWS.AppConfig.Types.Deployment
import Network.AWS.AppConfig.Types.DeploymentEvent
import Network.AWS.AppConfig.Types.DeploymentEventType
import Network.AWS.AppConfig.Types.DeploymentState
import Network.AWS.AppConfig.Types.DeploymentStrategy
import Network.AWS.AppConfig.Types.DeploymentSummary
import Network.AWS.AppConfig.Types.Environment
import Network.AWS.AppConfig.Types.EnvironmentState
import Network.AWS.AppConfig.Types.GrowthType
import Network.AWS.AppConfig.Types.HostedConfigurationVersion
import Network.AWS.AppConfig.Types.HostedConfigurationVersionSummary
import Network.AWS.AppConfig.Types.Monitor
import Network.AWS.AppConfig.Types.ReplicateTo
import Network.AWS.AppConfig.Types.TriggeredBy
import Network.AWS.AppConfig.Types.Validator
import Network.AWS.AppConfig.Types.ValidatorType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-10-09@ of the Amazon AppConfig SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AppConfig",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "appconfig",
      Core._serviceSigningName = "appconfig",
      Core._serviceVersion = "2019-10-09",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "AppConfig",
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

-- | The configuration size is too large.
_PayloadTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PayloadTooLargeException =
  Core._MatchServiceError
    defaultService
    "PayloadTooLargeException"
    Prelude.. Core.hasStatus 413

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The number of hosted configuration versions exceeds the limit for the
-- AppConfig configuration store. Delete one or more versions and try
-- again.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | There was an internal failure in the AppConfig service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The requested resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The input fails to satisfy the constraints specified by an AWS service.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
