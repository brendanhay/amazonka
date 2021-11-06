{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FIS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,

    -- * ExperimentActionStatus
    ExperimentActionStatus (..),

    -- * ExperimentStatus
    ExperimentStatus (..),

    -- * Action
    Action (..),
    newAction,
    action_parameters,
    action_targets,
    action_id,
    action_description,
    action_tags,

    -- * ActionParameter
    ActionParameter (..),
    newActionParameter,
    actionParameter_required,
    actionParameter_description,

    -- * ActionSummary
    ActionSummary (..),
    newActionSummary,
    actionSummary_targets,
    actionSummary_id,
    actionSummary_description,
    actionSummary_tags,

    -- * ActionTarget
    ActionTarget (..),
    newActionTarget,
    actionTarget_resourceType,

    -- * CreateExperimentTemplateActionInput
    CreateExperimentTemplateActionInput (..),
    newCreateExperimentTemplateActionInput,
    createExperimentTemplateActionInput_startAfter,
    createExperimentTemplateActionInput_parameters,
    createExperimentTemplateActionInput_targets,
    createExperimentTemplateActionInput_description,
    createExperimentTemplateActionInput_actionId,

    -- * CreateExperimentTemplateStopConditionInput
    CreateExperimentTemplateStopConditionInput (..),
    newCreateExperimentTemplateStopConditionInput,
    createExperimentTemplateStopConditionInput_value,
    createExperimentTemplateStopConditionInput_source,

    -- * CreateExperimentTemplateTargetInput
    CreateExperimentTemplateTargetInput (..),
    newCreateExperimentTemplateTargetInput,
    createExperimentTemplateTargetInput_resourceTags,
    createExperimentTemplateTargetInput_filters,
    createExperimentTemplateTargetInput_resourceArns,
    createExperimentTemplateTargetInput_resourceType,
    createExperimentTemplateTargetInput_selectionMode,

    -- * Experiment
    Experiment (..),
    newExperiment,
    experiment_creationTime,
    experiment_experimentTemplateId,
    experiment_state,
    experiment_actions,
    experiment_startTime,
    experiment_stopConditions,
    experiment_endTime,
    experiment_targets,
    experiment_id,
    experiment_tags,
    experiment_roleArn,

    -- * ExperimentAction
    ExperimentAction (..),
    newExperimentAction,
    experimentAction_startAfter,
    experimentAction_state,
    experimentAction_actionId,
    experimentAction_parameters,
    experimentAction_targets,
    experimentAction_description,

    -- * ExperimentActionState
    ExperimentActionState (..),
    newExperimentActionState,
    experimentActionState_status,
    experimentActionState_reason,

    -- * ExperimentState
    ExperimentState (..),
    newExperimentState,
    experimentState_status,
    experimentState_reason,

    -- * ExperimentStopCondition
    ExperimentStopCondition (..),
    newExperimentStopCondition,
    experimentStopCondition_value,
    experimentStopCondition_source,

    -- * ExperimentSummary
    ExperimentSummary (..),
    newExperimentSummary,
    experimentSummary_creationTime,
    experimentSummary_experimentTemplateId,
    experimentSummary_state,
    experimentSummary_id,
    experimentSummary_tags,

    -- * ExperimentTarget
    ExperimentTarget (..),
    newExperimentTarget,
    experimentTarget_resourceType,
    experimentTarget_resourceTags,
    experimentTarget_filters,
    experimentTarget_resourceArns,
    experimentTarget_selectionMode,

    -- * ExperimentTargetFilter
    ExperimentTargetFilter (..),
    newExperimentTargetFilter,
    experimentTargetFilter_path,
    experimentTargetFilter_values,

    -- * ExperimentTemplate
    ExperimentTemplate (..),
    newExperimentTemplate,
    experimentTemplate_creationTime,
    experimentTemplate_actions,
    experimentTemplate_stopConditions,
    experimentTemplate_targets,
    experimentTemplate_id,
    experimentTemplate_lastUpdateTime,
    experimentTemplate_description,
    experimentTemplate_tags,
    experimentTemplate_roleArn,

    -- * ExperimentTemplateAction
    ExperimentTemplateAction (..),
    newExperimentTemplateAction,
    experimentTemplateAction_startAfter,
    experimentTemplateAction_actionId,
    experimentTemplateAction_parameters,
    experimentTemplateAction_targets,
    experimentTemplateAction_description,

    -- * ExperimentTemplateStopCondition
    ExperimentTemplateStopCondition (..),
    newExperimentTemplateStopCondition,
    experimentTemplateStopCondition_value,
    experimentTemplateStopCondition_source,

    -- * ExperimentTemplateSummary
    ExperimentTemplateSummary (..),
    newExperimentTemplateSummary,
    experimentTemplateSummary_creationTime,
    experimentTemplateSummary_id,
    experimentTemplateSummary_lastUpdateTime,
    experimentTemplateSummary_description,
    experimentTemplateSummary_tags,

    -- * ExperimentTemplateTarget
    ExperimentTemplateTarget (..),
    newExperimentTemplateTarget,
    experimentTemplateTarget_resourceType,
    experimentTemplateTarget_resourceTags,
    experimentTemplateTarget_filters,
    experimentTemplateTarget_resourceArns,
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

    -- * UpdateExperimentTemplateActionInputItem
    UpdateExperimentTemplateActionInputItem (..),
    newUpdateExperimentTemplateActionInputItem,
    updateExperimentTemplateActionInputItem_startAfter,
    updateExperimentTemplateActionInputItem_actionId,
    updateExperimentTemplateActionInputItem_parameters,
    updateExperimentTemplateActionInputItem_targets,
    updateExperimentTemplateActionInputItem_description,

    -- * UpdateExperimentTemplateStopConditionInput
    UpdateExperimentTemplateStopConditionInput (..),
    newUpdateExperimentTemplateStopConditionInput,
    updateExperimentTemplateStopConditionInput_value,
    updateExperimentTemplateStopConditionInput_source,

    -- * UpdateExperimentTemplateTargetInput
    UpdateExperimentTemplateTargetInput (..),
    newUpdateExperimentTemplateTargetInput,
    updateExperimentTemplateTargetInput_resourceTags,
    updateExperimentTemplateTargetInput_filters,
    updateExperimentTemplateTargetInput_resourceArns,
    updateExperimentTemplateTargetInput_resourceType,
    updateExperimentTemplateTargetInput_selectionMode,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.FIS.Types.Action
import Amazonka.FIS.Types.ActionParameter
import Amazonka.FIS.Types.ActionSummary
import Amazonka.FIS.Types.ActionTarget
import Amazonka.FIS.Types.CreateExperimentTemplateActionInput
import Amazonka.FIS.Types.CreateExperimentTemplateStopConditionInput
import Amazonka.FIS.Types.CreateExperimentTemplateTargetInput
import Amazonka.FIS.Types.Experiment
import Amazonka.FIS.Types.ExperimentAction
import Amazonka.FIS.Types.ExperimentActionState
import Amazonka.FIS.Types.ExperimentActionStatus
import Amazonka.FIS.Types.ExperimentState
import Amazonka.FIS.Types.ExperimentStatus
import Amazonka.FIS.Types.ExperimentStopCondition
import Amazonka.FIS.Types.ExperimentSummary
import Amazonka.FIS.Types.ExperimentTarget
import Amazonka.FIS.Types.ExperimentTargetFilter
import Amazonka.FIS.Types.ExperimentTemplate
import Amazonka.FIS.Types.ExperimentTemplateAction
import Amazonka.FIS.Types.ExperimentTemplateStopCondition
import Amazonka.FIS.Types.ExperimentTemplateSummary
import Amazonka.FIS.Types.ExperimentTemplateTarget
import Amazonka.FIS.Types.ExperimentTemplateTargetFilter
import Amazonka.FIS.Types.ExperimentTemplateTargetInputFilter
import Amazonka.FIS.Types.UpdateExperimentTemplateActionInputItem
import Amazonka.FIS.Types.UpdateExperimentTemplateStopConditionInput
import Amazonka.FIS.Types.UpdateExperimentTemplateTargetInput
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-12-01@ of the Amazon Fault Injection Simulator SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "FIS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "fis",
      Core._serviceSigningName = "fis",
      Core._serviceVersion = "2020-12-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "FIS",
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

-- | The specified input is not valid, or fails to satisfy the constraints
-- for the request.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | The request could not be processed because of a conflict.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | You have exceeded your service quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The specified resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
