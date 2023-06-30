{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppConfig.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _ConflictException,
    _InternalServerException,
    _PayloadTooLargeException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,

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
    action_description,
    action_name,
    action_roleArn,
    action_uri,

    -- * ActionInvocation
    ActionInvocation (..),
    newActionInvocation,
    actionInvocation_actionName,
    actionInvocation_errorCode,
    actionInvocation_errorMessage,
    actionInvocation_extensionIdentifier,
    actionInvocation_invocationId,
    actionInvocation_roleArn,
    actionInvocation_uri,

    -- * Application
    Application (..),
    newApplication,
    application_description,
    application_id,
    application_name,

    -- * AppliedExtension
    AppliedExtension (..),
    newAppliedExtension,
    appliedExtension_extensionAssociationId,
    appliedExtension_extensionId,
    appliedExtension_parameters,
    appliedExtension_versionNumber,

    -- * ConfigurationProfile
    ConfigurationProfile (..),
    newConfigurationProfile,
    configurationProfile_applicationId,
    configurationProfile_description,
    configurationProfile_id,
    configurationProfile_locationUri,
    configurationProfile_name,
    configurationProfile_retrievalRoleArn,
    configurationProfile_type,
    configurationProfile_validators,

    -- * ConfigurationProfileSummary
    ConfigurationProfileSummary (..),
    newConfigurationProfileSummary,
    configurationProfileSummary_applicationId,
    configurationProfileSummary_id,
    configurationProfileSummary_locationUri,
    configurationProfileSummary_name,
    configurationProfileSummary_type,
    configurationProfileSummary_validatorTypes,

    -- * Deployment
    Deployment (..),
    newDeployment,
    deployment_applicationId,
    deployment_appliedExtensions,
    deployment_completedAt,
    deployment_configurationLocationUri,
    deployment_configurationName,
    deployment_configurationProfileId,
    deployment_configurationVersion,
    deployment_deploymentDurationInMinutes,
    deployment_deploymentNumber,
    deployment_deploymentStrategyId,
    deployment_description,
    deployment_environmentId,
    deployment_eventLog,
    deployment_finalBakeTimeInMinutes,
    deployment_growthFactor,
    deployment_growthType,
    deployment_percentageComplete,
    deployment_startedAt,
    deployment_state,

    -- * DeploymentEvent
    DeploymentEvent (..),
    newDeploymentEvent,
    deploymentEvent_actionInvocations,
    deploymentEvent_description,
    deploymentEvent_eventType,
    deploymentEvent_occurredAt,
    deploymentEvent_triggeredBy,

    -- * DeploymentStrategy
    DeploymentStrategy (..),
    newDeploymentStrategy,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_growthFactor,
    deploymentStrategy_growthType,
    deploymentStrategy_id,
    deploymentStrategy_name,
    deploymentStrategy_replicateTo,

    -- * DeploymentSummary
    DeploymentSummary (..),
    newDeploymentSummary,
    deploymentSummary_completedAt,
    deploymentSummary_configurationName,
    deploymentSummary_configurationVersion,
    deploymentSummary_deploymentDurationInMinutes,
    deploymentSummary_deploymentNumber,
    deploymentSummary_finalBakeTimeInMinutes,
    deploymentSummary_growthFactor,
    deploymentSummary_growthType,
    deploymentSummary_percentageComplete,
    deploymentSummary_startedAt,
    deploymentSummary_state,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_applicationId,
    environment_description,
    environment_id,
    environment_monitors,
    environment_name,
    environment_state,

    -- * Extension
    Extension (..),
    newExtension,
    extension_actions,
    extension_arn,
    extension_description,
    extension_id,
    extension_name,
    extension_parameters,
    extension_versionNumber,

    -- * ExtensionAssociation
    ExtensionAssociation (..),
    newExtensionAssociation,
    extensionAssociation_arn,
    extensionAssociation_extensionArn,
    extensionAssociation_extensionVersionNumber,
    extensionAssociation_id,
    extensionAssociation_parameters,
    extensionAssociation_resourceArn,

    -- * ExtensionAssociationSummary
    ExtensionAssociationSummary (..),
    newExtensionAssociationSummary,
    extensionAssociationSummary_extensionArn,
    extensionAssociationSummary_id,
    extensionAssociationSummary_resourceArn,

    -- * ExtensionSummary
    ExtensionSummary (..),
    newExtensionSummary,
    extensionSummary_arn,
    extensionSummary_description,
    extensionSummary_id,
    extensionSummary_name,
    extensionSummary_versionNumber,

    -- * HostedConfigurationVersion
    HostedConfigurationVersion (..),
    newHostedConfigurationVersion,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_contentType,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,

    -- * HostedConfigurationVersionSummary
    HostedConfigurationVersionSummary (..),
    newHostedConfigurationVersionSummary,
    hostedConfigurationVersionSummary_applicationId,
    hostedConfigurationVersionSummary_configurationProfileId,
    hostedConfigurationVersionSummary_contentType,
    hostedConfigurationVersionSummary_description,
    hostedConfigurationVersionSummary_versionNumber,

    -- * Monitor
    Monitor (..),
    newMonitor,
    monitor_alarmRoleArn,
    monitor_alarmArn,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_description,
    parameter_required,

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

-- | The input fails to satisfy the constraints specified by an Amazon Web
-- Services service.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request could not be processed because of conflict in the current
-- state of the resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | There was an internal failure in the AppConfig service.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The configuration size is too large.
_PayloadTooLargeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PayloadTooLargeException =
  Core._MatchServiceError
    defaultService
    "PayloadTooLargeException"
    Prelude.. Core.hasStatus 413

-- | The requested resource could not be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The number of hosted configuration versions exceeds the limit for the
-- AppConfig hosted configuration store. Delete one or more versions and
-- try again.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402
