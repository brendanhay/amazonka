{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppConfig.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _PayloadTooLargeException,
    _BadRequestException,

    -- * ActionPoint
    ActionPoint (..),

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

    -- * Action
    Action (..),
    newAction,
    action_name,
    action_roleArn,
    action_uri,
    action_description,

    -- * ActionInvocation
    ActionInvocation (..),
    newActionInvocation,
    actionInvocation_actionName,
    actionInvocation_roleArn,
    actionInvocation_invocationId,
    actionInvocation_errorMessage,
    actionInvocation_uri,
    actionInvocation_errorCode,
    actionInvocation_extensionIdentifier,

    -- * Application
    Application (..),
    newApplication,
    application_name,
    application_id,
    application_description,

    -- * AppliedExtension
    AppliedExtension (..),
    newAppliedExtension,
    appliedExtension_extensionAssociationId,
    appliedExtension_versionNumber,
    appliedExtension_extensionId,
    appliedExtension_parameters,

    -- * ConfigurationProfile
    ConfigurationProfile (..),
    newConfigurationProfile,
    configurationProfile_name,
    configurationProfile_type,
    configurationProfile_retrievalRoleArn,
    configurationProfile_id,
    configurationProfile_description,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_validators,

    -- * ConfigurationProfileSummary
    ConfigurationProfileSummary (..),
    newConfigurationProfileSummary,
    configurationProfileSummary_name,
    configurationProfileSummary_type,
    configurationProfileSummary_validatorTypes,
    configurationProfileSummary_id,
    configurationProfileSummary_locationUri,
    configurationProfileSummary_applicationId,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_deploymentStrategyId,
    deployment_growthType,
    deployment_state,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_description,
    deployment_finalBakeTimeInMinutes,
    deployment_startedAt,
    deployment_configurationName,
    deployment_growthFactor,
    deployment_appliedExtensions,
    deployment_eventLog,
    deployment_configurationVersion,
    deployment_environmentId,
    deployment_percentageComplete,
    deployment_configurationLocationUri,
    deployment_applicationId,
    deployment_completedAt,
    deployment_configurationProfileId,

    -- * DeploymentEvent
    DeploymentEvent (..),
    newDeploymentEvent,
    deploymentEvent_eventType,
    deploymentEvent_occurredAt,
    deploymentEvent_actionInvocations,
    deploymentEvent_description,
    deploymentEvent_triggeredBy,

    -- * DeploymentStrategy
    DeploymentStrategy (..),
    newDeploymentStrategy,
    deploymentStrategy_name,
    deploymentStrategy_growthType,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_id,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_replicateTo,
    deploymentStrategy_growthFactor,

    -- * DeploymentSummary
    DeploymentSummary (..),
    newDeploymentSummary,
    deploymentSummary_growthType,
    deploymentSummary_state,
    deploymentSummary_deploymentDurationInMinutes,
    deploymentSummary_deploymentNumber,
    deploymentSummary_finalBakeTimeInMinutes,
    deploymentSummary_startedAt,
    deploymentSummary_configurationName,
    deploymentSummary_growthFactor,
    deploymentSummary_configurationVersion,
    deploymentSummary_percentageComplete,
    deploymentSummary_completedAt,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_name,
    environment_state,
    environment_monitors,
    environment_id,
    environment_description,
    environment_applicationId,

    -- * Extension
    Extension (..),
    newExtension,
    extension_name,
    extension_arn,
    extension_id,
    extension_description,
    extension_versionNumber,
    extension_actions,
    extension_parameters,

    -- * ExtensionAssociation
    ExtensionAssociation (..),
    newExtensionAssociation,
    extensionAssociation_extensionVersionNumber,
    extensionAssociation_arn,
    extensionAssociation_id,
    extensionAssociation_resourceArn,
    extensionAssociation_extensionArn,
    extensionAssociation_parameters,

    -- * ExtensionAssociationSummary
    ExtensionAssociationSummary (..),
    newExtensionAssociationSummary,
    extensionAssociationSummary_id,
    extensionAssociationSummary_resourceArn,
    extensionAssociationSummary_extensionArn,

    -- * ExtensionSummary
    ExtensionSummary (..),
    newExtensionSummary,
    extensionSummary_name,
    extensionSummary_arn,
    extensionSummary_id,
    extensionSummary_description,
    extensionSummary_versionNumber,

    -- * HostedConfigurationVersion
    HostedConfigurationVersion (..),
    newHostedConfigurationVersion,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_contentType,

    -- * HostedConfigurationVersionSummary
    HostedConfigurationVersionSummary (..),
    newHostedConfigurationVersionSummary,
    hostedConfigurationVersionSummary_description,
    hostedConfigurationVersionSummary_versionNumber,
    hostedConfigurationVersionSummary_applicationId,
    hostedConfigurationVersionSummary_configurationProfileId,
    hostedConfigurationVersionSummary_contentType,

    -- * Monitor
    Monitor (..),
    newMonitor,
    monitor_alarmRoleArn,
    monitor_alarmArn,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_required,
    parameter_description,

    -- * Validator
    Validator (..),
    newValidator,
    validator_type,
    validator_content,
  )
where

import Amazonka.AppConfig.Types.Action
import Amazonka.AppConfig.Types.ActionInvocation
import Amazonka.AppConfig.Types.ActionPoint
import Amazonka.AppConfig.Types.Application
import Amazonka.AppConfig.Types.AppliedExtension
import Amazonka.AppConfig.Types.ConfigurationProfile
import Amazonka.AppConfig.Types.ConfigurationProfileSummary
import Amazonka.AppConfig.Types.Deployment
import Amazonka.AppConfig.Types.DeploymentEvent
import Amazonka.AppConfig.Types.DeploymentEventType
import Amazonka.AppConfig.Types.DeploymentState
import Amazonka.AppConfig.Types.DeploymentStrategy
import Amazonka.AppConfig.Types.DeploymentSummary
import Amazonka.AppConfig.Types.Environment
import Amazonka.AppConfig.Types.EnvironmentState
import Amazonka.AppConfig.Types.Extension
import Amazonka.AppConfig.Types.ExtensionAssociation
import Amazonka.AppConfig.Types.ExtensionAssociationSummary
import Amazonka.AppConfig.Types.ExtensionSummary
import Amazonka.AppConfig.Types.GrowthType
import Amazonka.AppConfig.Types.HostedConfigurationVersion
import Amazonka.AppConfig.Types.HostedConfigurationVersionSummary
import Amazonka.AppConfig.Types.Monitor
import Amazonka.AppConfig.Types.Parameter
import Amazonka.AppConfig.Types.ReplicateTo
import Amazonka.AppConfig.Types.TriggeredBy
import Amazonka.AppConfig.Types.Validator
import Amazonka.AppConfig.Types.ValidatorType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-10-09@ of the Amazon AppConfig SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AppConfig",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "appconfig",
      Core.signingName = "appconfig",
      Core.version = "2019-10-09",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AppConfig",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | There was an internal failure in the AppConfig service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The number of hosted configuration versions exceeds the limit for the
-- AppConfig hosted configuration store. Delete one or more versions and
-- try again.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The requested resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The configuration size is too large.
_PayloadTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PayloadTooLargeException =
  Core._MatchServiceError
    defaultService
    "PayloadTooLargeException"
    Prelude.. Core.hasStatus 413

-- | The input fails to satisfy the constraints specified by an Amazon Web
-- Services service.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
