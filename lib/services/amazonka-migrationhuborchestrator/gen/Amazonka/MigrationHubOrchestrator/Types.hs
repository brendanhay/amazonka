{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubOrchestrator.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _ValidationException,

    -- * DataType
    DataType (..),

    -- * MigrationWorkflowStatusEnum
    MigrationWorkflowStatusEnum (..),

    -- * Owner
    Owner (..),

    -- * PluginHealth
    PluginHealth (..),

    -- * RunEnvironment
    RunEnvironment (..),

    -- * StepActionType
    StepActionType (..),

    -- * StepGroupStatus
    StepGroupStatus (..),

    -- * StepStatus
    StepStatus (..),

    -- * TargetType
    TargetType (..),

    -- * TemplateStatus
    TemplateStatus (..),

    -- * MigrationWorkflowSummary
    MigrationWorkflowSummary (..),
    newMigrationWorkflowSummary,
    migrationWorkflowSummary_adsApplicationConfigurationName,
    migrationWorkflowSummary_completedSteps,
    migrationWorkflowSummary_creationTime,
    migrationWorkflowSummary_endTime,
    migrationWorkflowSummary_id,
    migrationWorkflowSummary_name,
    migrationWorkflowSummary_status,
    migrationWorkflowSummary_statusMessage,
    migrationWorkflowSummary_templateId,
    migrationWorkflowSummary_totalSteps,

    -- * PlatformCommand
    PlatformCommand (..),
    newPlatformCommand,
    platformCommand_linux,
    platformCommand_windows,

    -- * PlatformScriptKey
    PlatformScriptKey (..),
    newPlatformScriptKey,
    platformScriptKey_linux,
    platformScriptKey_windows,

    -- * PluginSummary
    PluginSummary (..),
    newPluginSummary,
    pluginSummary_hostname,
    pluginSummary_ipAddress,
    pluginSummary_pluginId,
    pluginSummary_registeredTime,
    pluginSummary_status,
    pluginSummary_version,

    -- * StepAutomationConfiguration
    StepAutomationConfiguration (..),
    newStepAutomationConfiguration,
    stepAutomationConfiguration_command,
    stepAutomationConfiguration_runEnvironment,
    stepAutomationConfiguration_scriptLocationS3Bucket,
    stepAutomationConfiguration_scriptLocationS3Key,
    stepAutomationConfiguration_targetType,

    -- * StepInput
    StepInput (..),
    newStepInput,
    stepInput_integerValue,
    stepInput_listOfStringsValue,
    stepInput_mapOfStringValue,
    stepInput_stringValue,

    -- * StepOutput
    StepOutput (..),
    newStepOutput,
    stepOutput_dataType,
    stepOutput_name,
    stepOutput_required,

    -- * TemplateInput
    TemplateInput (..),
    newTemplateInput,
    templateInput_dataType,
    templateInput_inputName,
    templateInput_required,

    -- * TemplateStepGroupSummary
    TemplateStepGroupSummary (..),
    newTemplateStepGroupSummary,
    templateStepGroupSummary_id,
    templateStepGroupSummary_name,
    templateStepGroupSummary_next,
    templateStepGroupSummary_previous,

    -- * TemplateStepSummary
    TemplateStepSummary (..),
    newTemplateStepSummary,
    templateStepSummary_id,
    templateStepSummary_name,
    templateStepSummary_next,
    templateStepSummary_owner,
    templateStepSummary_previous,
    templateStepSummary_stepActionType,
    templateStepSummary_stepGroupId,
    templateStepSummary_targetType,
    templateStepSummary_templateId,

    -- * TemplateSummary
    TemplateSummary (..),
    newTemplateSummary,
    templateSummary_arn,
    templateSummary_description,
    templateSummary_id,
    templateSummary_name,

    -- * Tool
    Tool (..),
    newTool,
    tool_name,
    tool_url,

    -- * WorkflowStepAutomationConfiguration
    WorkflowStepAutomationConfiguration (..),
    newWorkflowStepAutomationConfiguration,
    workflowStepAutomationConfiguration_command,
    workflowStepAutomationConfiguration_runEnvironment,
    workflowStepAutomationConfiguration_scriptLocationS3Bucket,
    workflowStepAutomationConfiguration_scriptLocationS3Key,
    workflowStepAutomationConfiguration_targetType,

    -- * WorkflowStepGroupSummary
    WorkflowStepGroupSummary (..),
    newWorkflowStepGroupSummary,
    workflowStepGroupSummary_id,
    workflowStepGroupSummary_name,
    workflowStepGroupSummary_next,
    workflowStepGroupSummary_owner,
    workflowStepGroupSummary_previous,
    workflowStepGroupSummary_status,

    -- * WorkflowStepOutput
    WorkflowStepOutput (..),
    newWorkflowStepOutput,
    workflowStepOutput_dataType,
    workflowStepOutput_name,
    workflowStepOutput_required,
    workflowStepOutput_value,

    -- * WorkflowStepOutputUnion
    WorkflowStepOutputUnion (..),
    newWorkflowStepOutputUnion,
    workflowStepOutputUnion_integerValue,
    workflowStepOutputUnion_listOfStringValue,
    workflowStepOutputUnion_stringValue,

    -- * WorkflowStepSummary
    WorkflowStepSummary (..),
    newWorkflowStepSummary,
    workflowStepSummary_description,
    workflowStepSummary_name,
    workflowStepSummary_next,
    workflowStepSummary_noOfSrvCompleted,
    workflowStepSummary_noOfSrvFailed,
    workflowStepSummary_owner,
    workflowStepSummary_previous,
    workflowStepSummary_scriptLocation,
    workflowStepSummary_status,
    workflowStepSummary_statusMessage,
    workflowStepSummary_stepActionType,
    workflowStepSummary_stepId,
    workflowStepSummary_totalNoOfSrv,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubOrchestrator.Types.DataType
import Amazonka.MigrationHubOrchestrator.Types.MigrationWorkflowStatusEnum
import Amazonka.MigrationHubOrchestrator.Types.MigrationWorkflowSummary
import Amazonka.MigrationHubOrchestrator.Types.Owner
import Amazonka.MigrationHubOrchestrator.Types.PlatformCommand
import Amazonka.MigrationHubOrchestrator.Types.PlatformScriptKey
import Amazonka.MigrationHubOrchestrator.Types.PluginHealth
import Amazonka.MigrationHubOrchestrator.Types.PluginSummary
import Amazonka.MigrationHubOrchestrator.Types.RunEnvironment
import Amazonka.MigrationHubOrchestrator.Types.StepActionType
import Amazonka.MigrationHubOrchestrator.Types.StepAutomationConfiguration
import Amazonka.MigrationHubOrchestrator.Types.StepGroupStatus
import Amazonka.MigrationHubOrchestrator.Types.StepInput
import Amazonka.MigrationHubOrchestrator.Types.StepOutput
import Amazonka.MigrationHubOrchestrator.Types.StepStatus
import Amazonka.MigrationHubOrchestrator.Types.TargetType
import Amazonka.MigrationHubOrchestrator.Types.TemplateInput
import Amazonka.MigrationHubOrchestrator.Types.TemplateStatus
import Amazonka.MigrationHubOrchestrator.Types.TemplateStepGroupSummary
import Amazonka.MigrationHubOrchestrator.Types.TemplateStepSummary
import Amazonka.MigrationHubOrchestrator.Types.TemplateSummary
import Amazonka.MigrationHubOrchestrator.Types.Tool
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepAutomationConfiguration
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepGroupSummary
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepOutput
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepOutputUnion
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-08-28@ of the Amazon Migration Hub Orchestrator SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "MigrationHubOrchestrator",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "migrationhub-orchestrator",
      Core.signingName = "migrationhub-orchestrator",
      Core.version = "2021-08-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "MigrationHubOrchestrator",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An internal error has occurred.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource is not available.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
