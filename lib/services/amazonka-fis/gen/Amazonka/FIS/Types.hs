{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FIS.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ValidationException,

    -- * ExperimentActionStatus
    ExperimentActionStatus (..),

    -- * ExperimentStatus
    ExperimentStatus (..),

    -- * Action
    Action (..),
    newAction,
    action_description,
    action_id,
    action_parameters,
    action_tags,
    action_targets,

    -- * ActionParameter
    ActionParameter (..),
    newActionParameter,
    actionParameter_description,
    actionParameter_required,

    -- * ActionSummary
    ActionSummary (..),
    newActionSummary,
    actionSummary_description,
    actionSummary_id,
    actionSummary_tags,
    actionSummary_targets,

    -- * ActionTarget
    ActionTarget (..),
    newActionTarget,
    actionTarget_resourceType,

    -- * CreateExperimentTemplateActionInput
    CreateExperimentTemplateActionInput (..),
    newCreateExperimentTemplateActionInput,
    createExperimentTemplateActionInput_description,
    createExperimentTemplateActionInput_parameters,
    createExperimentTemplateActionInput_startAfter,
    createExperimentTemplateActionInput_targets,
    createExperimentTemplateActionInput_actionId,

    -- * CreateExperimentTemplateLogConfigurationInput
    CreateExperimentTemplateLogConfigurationInput (..),
    newCreateExperimentTemplateLogConfigurationInput,
    createExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration,
    createExperimentTemplateLogConfigurationInput_s3Configuration,
    createExperimentTemplateLogConfigurationInput_logSchemaVersion,

    -- * CreateExperimentTemplateStopConditionInput
    CreateExperimentTemplateStopConditionInput (..),
    newCreateExperimentTemplateStopConditionInput,
    createExperimentTemplateStopConditionInput_value,
    createExperimentTemplateStopConditionInput_source,

    -- * CreateExperimentTemplateTargetInput
    CreateExperimentTemplateTargetInput (..),
    newCreateExperimentTemplateTargetInput,
    createExperimentTemplateTargetInput_filters,
    createExperimentTemplateTargetInput_parameters,
    createExperimentTemplateTargetInput_resourceArns,
    createExperimentTemplateTargetInput_resourceTags,
    createExperimentTemplateTargetInput_resourceType,
    createExperimentTemplateTargetInput_selectionMode,

    -- * Experiment
    Experiment (..),
    newExperiment,
    experiment_actions,
    experiment_creationTime,
    experiment_endTime,
    experiment_experimentTemplateId,
    experiment_id,
    experiment_logConfiguration,
    experiment_roleArn,
    experiment_startTime,
    experiment_state,
    experiment_stopConditions,
    experiment_tags,
    experiment_targets,

    -- * ExperimentAction
    ExperimentAction (..),
    newExperimentAction,
    experimentAction_actionId,
    experimentAction_description,
    experimentAction_endTime,
    experimentAction_parameters,
    experimentAction_startAfter,
    experimentAction_startTime,
    experimentAction_state,
    experimentAction_targets,

    -- * ExperimentActionState
    ExperimentActionState (..),
    newExperimentActionState,
    experimentActionState_reason,
    experimentActionState_status,

    -- * ExperimentCloudWatchLogsLogConfiguration
    ExperimentCloudWatchLogsLogConfiguration (..),
    newExperimentCloudWatchLogsLogConfiguration,
    experimentCloudWatchLogsLogConfiguration_logGroupArn,

    -- * ExperimentLogConfiguration
    ExperimentLogConfiguration (..),
    newExperimentLogConfiguration,
    experimentLogConfiguration_cloudWatchLogsConfiguration,
    experimentLogConfiguration_logSchemaVersion,
    experimentLogConfiguration_s3Configuration,

    -- * ExperimentS3LogConfiguration
    ExperimentS3LogConfiguration (..),
    newExperimentS3LogConfiguration,
    experimentS3LogConfiguration_bucketName,
    experimentS3LogConfiguration_prefix,

    -- * ExperimentState
    ExperimentState (..),
    newExperimentState,
    experimentState_reason,
    experimentState_status,

    -- * ExperimentStopCondition
    ExperimentStopCondition (..),
    newExperimentStopCondition,
    experimentStopCondition_source,
    experimentStopCondition_value,

    -- * ExperimentSummary
    ExperimentSummary (..),
    newExperimentSummary,
    experimentSummary_creationTime,
    experimentSummary_experimentTemplateId,
    experimentSummary_id,
    experimentSummary_state,
    experimentSummary_tags,

    -- * ExperimentTarget
    ExperimentTarget (..),
    newExperimentTarget,
    experimentTarget_filters,
    experimentTarget_parameters,
    experimentTarget_resourceArns,
    experimentTarget_resourceTags,
    experimentTarget_resourceType,
    experimentTarget_selectionMode,

    -- * ExperimentTargetFilter
    ExperimentTargetFilter (..),
    newExperimentTargetFilter,
    experimentTargetFilter_path,
    experimentTargetFilter_values,

    -- * ExperimentTemplate
    ExperimentTemplate (..),
    newExperimentTemplate,
    experimentTemplate_actions,
    experimentTemplate_creationTime,
    experimentTemplate_description,
    experimentTemplate_id,
    experimentTemplate_lastUpdateTime,
    experimentTemplate_logConfiguration,
    experimentTemplate_roleArn,
    experimentTemplate_stopConditions,
    experimentTemplate_tags,
    experimentTemplate_targets,

    -- * ExperimentTemplateAction
    ExperimentTemplateAction (..),
    newExperimentTemplateAction,
    experimentTemplateAction_actionId,
    experimentTemplateAction_description,
    experimentTemplateAction_parameters,
    experimentTemplateAction_startAfter,
    experimentTemplateAction_targets,

    -- * ExperimentTemplateCloudWatchLogsLogConfiguration
    ExperimentTemplateCloudWatchLogsLogConfiguration (..),
    newExperimentTemplateCloudWatchLogsLogConfiguration,
    experimentTemplateCloudWatchLogsLogConfiguration_logGroupArn,

    -- * ExperimentTemplateCloudWatchLogsLogConfigurationInput
    ExperimentTemplateCloudWatchLogsLogConfigurationInput (..),
    newExperimentTemplateCloudWatchLogsLogConfigurationInput,
    experimentTemplateCloudWatchLogsLogConfigurationInput_logGroupArn,

    -- * ExperimentTemplateLogConfiguration
    ExperimentTemplateLogConfiguration (..),
    newExperimentTemplateLogConfiguration,
    experimentTemplateLogConfiguration_cloudWatchLogsConfiguration,
    experimentTemplateLogConfiguration_logSchemaVersion,
    experimentTemplateLogConfiguration_s3Configuration,

    -- * ExperimentTemplateS3LogConfiguration
    ExperimentTemplateS3LogConfiguration (..),
    newExperimentTemplateS3LogConfiguration,
    experimentTemplateS3LogConfiguration_bucketName,
    experimentTemplateS3LogConfiguration_prefix,

    -- * ExperimentTemplateS3LogConfigurationInput
    ExperimentTemplateS3LogConfigurationInput (..),
    newExperimentTemplateS3LogConfigurationInput,
    experimentTemplateS3LogConfigurationInput_prefix,
    experimentTemplateS3LogConfigurationInput_bucketName,

    -- * ExperimentTemplateStopCondition
    ExperimentTemplateStopCondition (..),
    newExperimentTemplateStopCondition,
    experimentTemplateStopCondition_source,
    experimentTemplateStopCondition_value,

    -- * ExperimentTemplateSummary
    ExperimentTemplateSummary (..),
    newExperimentTemplateSummary,
    experimentTemplateSummary_creationTime,
    experimentTemplateSummary_description,
    experimentTemplateSummary_id,
    experimentTemplateSummary_lastUpdateTime,
    experimentTemplateSummary_tags,

    -- * ExperimentTemplateTarget
    ExperimentTemplateTarget (..),
    newExperimentTemplateTarget,
    experimentTemplateTarget_filters,
    experimentTemplateTarget_parameters,
    experimentTemplateTarget_resourceArns,
    experimentTemplateTarget_resourceTags,
    experimentTemplateTarget_resourceType,
    experimentTemplateTarget_selectionMode,

    -- * ExperimentTemplateTargetFilter
    ExperimentTemplateTargetFilter (..),
    newExperimentTemplateTargetFilter,
    experimentTemplateTargetFilter_path,
    experimentTemplateTargetFilter_values,

    -- * ExperimentTemplateTargetInputFilter
    ExperimentTemplateTargetInputFilter (..),
    newExperimentTemplateTargetInputFilter,
    experimentTemplateTargetInputFilter_path,
    experimentTemplateTargetInputFilter_values,

    -- * TargetResourceType
    TargetResourceType (..),
    newTargetResourceType,
    targetResourceType_description,
    targetResourceType_parameters,
    targetResourceType_resourceType,

    -- * TargetResourceTypeParameter
    TargetResourceTypeParameter (..),
    newTargetResourceTypeParameter,
    targetResourceTypeParameter_description,
    targetResourceTypeParameter_required,

    -- * TargetResourceTypeSummary
    TargetResourceTypeSummary (..),
    newTargetResourceTypeSummary,
    targetResourceTypeSummary_description,
    targetResourceTypeSummary_resourceType,

    -- * UpdateExperimentTemplateActionInputItem
    UpdateExperimentTemplateActionInputItem (..),
    newUpdateExperimentTemplateActionInputItem,
    updateExperimentTemplateActionInputItem_actionId,
    updateExperimentTemplateActionInputItem_description,
    updateExperimentTemplateActionInputItem_parameters,
    updateExperimentTemplateActionInputItem_startAfter,
    updateExperimentTemplateActionInputItem_targets,

    -- * UpdateExperimentTemplateLogConfigurationInput
    UpdateExperimentTemplateLogConfigurationInput (..),
    newUpdateExperimentTemplateLogConfigurationInput,
    updateExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration,
    updateExperimentTemplateLogConfigurationInput_logSchemaVersion,
    updateExperimentTemplateLogConfigurationInput_s3Configuration,

    -- * UpdateExperimentTemplateStopConditionInput
    UpdateExperimentTemplateStopConditionInput (..),
    newUpdateExperimentTemplateStopConditionInput,
    updateExperimentTemplateStopConditionInput_value,
    updateExperimentTemplateStopConditionInput_source,

    -- * UpdateExperimentTemplateTargetInput
    UpdateExperimentTemplateTargetInput (..),
    newUpdateExperimentTemplateTargetInput,
    updateExperimentTemplateTargetInput_filters,
    updateExperimentTemplateTargetInput_parameters,
    updateExperimentTemplateTargetInput_resourceArns,
    updateExperimentTemplateTargetInput_resourceTags,
    updateExperimentTemplateTargetInput_resourceType,
    updateExperimentTemplateTargetInput_selectionMode,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FIS.Types.Action
import Amazonka.FIS.Types.ActionParameter
import Amazonka.FIS.Types.ActionSummary
import Amazonka.FIS.Types.ActionTarget
import Amazonka.FIS.Types.CreateExperimentTemplateActionInput
import Amazonka.FIS.Types.CreateExperimentTemplateLogConfigurationInput
import Amazonka.FIS.Types.CreateExperimentTemplateStopConditionInput
import Amazonka.FIS.Types.CreateExperimentTemplateTargetInput
import Amazonka.FIS.Types.Experiment
import Amazonka.FIS.Types.ExperimentAction
import Amazonka.FIS.Types.ExperimentActionState
import Amazonka.FIS.Types.ExperimentActionStatus
import Amazonka.FIS.Types.ExperimentCloudWatchLogsLogConfiguration
import Amazonka.FIS.Types.ExperimentLogConfiguration
import Amazonka.FIS.Types.ExperimentS3LogConfiguration
import Amazonka.FIS.Types.ExperimentState
import Amazonka.FIS.Types.ExperimentStatus
import Amazonka.FIS.Types.ExperimentStopCondition
import Amazonka.FIS.Types.ExperimentSummary
import Amazonka.FIS.Types.ExperimentTarget
import Amazonka.FIS.Types.ExperimentTargetFilter
import Amazonka.FIS.Types.ExperimentTemplate
import Amazonka.FIS.Types.ExperimentTemplateAction
import Amazonka.FIS.Types.ExperimentTemplateCloudWatchLogsLogConfiguration
import Amazonka.FIS.Types.ExperimentTemplateCloudWatchLogsLogConfigurationInput
import Amazonka.FIS.Types.ExperimentTemplateLogConfiguration
import Amazonka.FIS.Types.ExperimentTemplateS3LogConfiguration
import Amazonka.FIS.Types.ExperimentTemplateS3LogConfigurationInput
import Amazonka.FIS.Types.ExperimentTemplateStopCondition
import Amazonka.FIS.Types.ExperimentTemplateSummary
import Amazonka.FIS.Types.ExperimentTemplateTarget
import Amazonka.FIS.Types.ExperimentTemplateTargetFilter
import Amazonka.FIS.Types.ExperimentTemplateTargetInputFilter
import Amazonka.FIS.Types.TargetResourceType
import Amazonka.FIS.Types.TargetResourceTypeParameter
import Amazonka.FIS.Types.TargetResourceTypeSummary
import Amazonka.FIS.Types.UpdateExperimentTemplateActionInputItem
import Amazonka.FIS.Types.UpdateExperimentTemplateLogConfigurationInput
import Amazonka.FIS.Types.UpdateExperimentTemplateStopConditionInput
import Amazonka.FIS.Types.UpdateExperimentTemplateTargetInput
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-12-01@ of the Amazon Fault Injection Simulator SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "FIS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "fis",
      Core.signingName = "fis",
      Core.version = "2020-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "FIS",
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

-- | The request could not be processed because of a conflict.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The specified resource cannot be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You have exceeded your service quota.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The specified input is not valid, or fails to satisfy the constraints
-- for the request.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
