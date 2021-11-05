{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FIS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FIS.Lens
  ( -- * Operations

    -- ** GetExperimentTemplate
    getExperimentTemplate_id,
    getExperimentTemplateResponse_experimentTemplate,
    getExperimentTemplateResponse_httpStatus,

    -- ** ListActions
    listActions_nextToken,
    listActions_maxResults,
    listActionsResponse_actions,
    listActionsResponse_nextToken,
    listActionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateExperimentTemplate
    createExperimentTemplate_targets,
    createExperimentTemplate_tags,
    createExperimentTemplate_clientToken,
    createExperimentTemplate_description,
    createExperimentTemplate_stopConditions,
    createExperimentTemplate_actions,
    createExperimentTemplate_roleArn,
    createExperimentTemplateResponse_experimentTemplate,
    createExperimentTemplateResponse_httpStatus,

    -- ** ListExperiments
    listExperiments_nextToken,
    listExperiments_maxResults,
    listExperimentsResponse_experiments,
    listExperimentsResponse_nextToken,
    listExperimentsResponse_httpStatus,

    -- ** UpdateExperimentTemplate
    updateExperimentTemplate_actions,
    updateExperimentTemplate_stopConditions,
    updateExperimentTemplate_targets,
    updateExperimentTemplate_description,
    updateExperimentTemplate_roleArn,
    updateExperimentTemplate_id,
    updateExperimentTemplateResponse_experimentTemplate,
    updateExperimentTemplateResponse_httpStatus,

    -- ** GetAction
    getAction_id,
    getActionResponse_action,
    getActionResponse_httpStatus,

    -- ** DeleteExperimentTemplate
    deleteExperimentTemplate_id,
    deleteExperimentTemplateResponse_experimentTemplate,
    deleteExperimentTemplateResponse_httpStatus,

    -- ** StartExperiment
    startExperiment_tags,
    startExperiment_clientToken,
    startExperiment_experimentTemplateId,
    startExperimentResponse_experiment,
    startExperimentResponse_httpStatus,

    -- ** GetExperiment
    getExperiment_id,
    getExperimentResponse_experiment,
    getExperimentResponse_httpStatus,

    -- ** ListExperimentTemplates
    listExperimentTemplates_nextToken,
    listExperimentTemplates_maxResults,
    listExperimentTemplatesResponse_nextToken,
    listExperimentTemplatesResponse_experimentTemplates,
    listExperimentTemplatesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,
    untagResourceResponse_httpStatus,

    -- ** StopExperiment
    stopExperiment_id,
    stopExperimentResponse_experiment,
    stopExperimentResponse_httpStatus,

    -- * Types

    -- ** Action
    action_parameters,
    action_targets,
    action_id,
    action_description,
    action_tags,

    -- ** ActionParameter
    actionParameter_required,
    actionParameter_description,

    -- ** ActionSummary
    actionSummary_targets,
    actionSummary_id,
    actionSummary_description,
    actionSummary_tags,

    -- ** ActionTarget
    actionTarget_resourceType,

    -- ** CreateExperimentTemplateActionInput
    createExperimentTemplateActionInput_startAfter,
    createExperimentTemplateActionInput_parameters,
    createExperimentTemplateActionInput_targets,
    createExperimentTemplateActionInput_description,
    createExperimentTemplateActionInput_actionId,

    -- ** CreateExperimentTemplateStopConditionInput
    createExperimentTemplateStopConditionInput_value,
    createExperimentTemplateStopConditionInput_source,

    -- ** CreateExperimentTemplateTargetInput
    createExperimentTemplateTargetInput_resourceTags,
    createExperimentTemplateTargetInput_filters,
    createExperimentTemplateTargetInput_resourceArns,
    createExperimentTemplateTargetInput_resourceType,
    createExperimentTemplateTargetInput_selectionMode,

    -- ** Experiment
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

    -- ** ExperimentAction
    experimentAction_startAfter,
    experimentAction_state,
    experimentAction_actionId,
    experimentAction_parameters,
    experimentAction_targets,
    experimentAction_description,

    -- ** ExperimentActionState
    experimentActionState_status,
    experimentActionState_reason,

    -- ** ExperimentState
    experimentState_status,
    experimentState_reason,

    -- ** ExperimentStopCondition
    experimentStopCondition_value,
    experimentStopCondition_source,

    -- ** ExperimentSummary
    experimentSummary_creationTime,
    experimentSummary_experimentTemplateId,
    experimentSummary_state,
    experimentSummary_id,
    experimentSummary_tags,

    -- ** ExperimentTarget
    experimentTarget_resourceType,
    experimentTarget_resourceTags,
    experimentTarget_filters,
    experimentTarget_resourceArns,
    experimentTarget_selectionMode,

    -- ** ExperimentTargetFilter
    experimentTargetFilter_path,
    experimentTargetFilter_values,

    -- ** ExperimentTemplate
    experimentTemplate_creationTime,
    experimentTemplate_actions,
    experimentTemplate_stopConditions,
    experimentTemplate_targets,
    experimentTemplate_id,
    experimentTemplate_lastUpdateTime,
    experimentTemplate_description,
    experimentTemplate_tags,
    experimentTemplate_roleArn,

    -- ** ExperimentTemplateAction
    experimentTemplateAction_startAfter,
    experimentTemplateAction_actionId,
    experimentTemplateAction_parameters,
    experimentTemplateAction_targets,
    experimentTemplateAction_description,

    -- ** ExperimentTemplateStopCondition
    experimentTemplateStopCondition_value,
    experimentTemplateStopCondition_source,

    -- ** ExperimentTemplateSummary
    experimentTemplateSummary_creationTime,
    experimentTemplateSummary_id,
    experimentTemplateSummary_lastUpdateTime,
    experimentTemplateSummary_description,
    experimentTemplateSummary_tags,

    -- ** ExperimentTemplateTarget
    experimentTemplateTarget_resourceType,
    experimentTemplateTarget_resourceTags,
    experimentTemplateTarget_filters,
    experimentTemplateTarget_resourceArns,
    experimentTemplateTarget_selectionMode,

    -- ** ExperimentTemplateTargetFilter
    experimentTemplateTargetFilter_path,
    experimentTemplateTargetFilter_values,

    -- ** ExperimentTemplateTargetInputFilter
    experimentTemplateTargetInputFilter_path,
    experimentTemplateTargetInputFilter_values,

    -- ** UpdateExperimentTemplateActionInputItem
    updateExperimentTemplateActionInputItem_startAfter,
    updateExperimentTemplateActionInputItem_actionId,
    updateExperimentTemplateActionInputItem_parameters,
    updateExperimentTemplateActionInputItem_targets,
    updateExperimentTemplateActionInputItem_description,

    -- ** UpdateExperimentTemplateStopConditionInput
    updateExperimentTemplateStopConditionInput_value,
    updateExperimentTemplateStopConditionInput_source,

    -- ** UpdateExperimentTemplateTargetInput
    updateExperimentTemplateTargetInput_resourceTags,
    updateExperimentTemplateTargetInput_filters,
    updateExperimentTemplateTargetInput_resourceArns,
    updateExperimentTemplateTargetInput_resourceType,
    updateExperimentTemplateTargetInput_selectionMode,
  )
where

import Network.AWS.FIS.CreateExperimentTemplate
import Network.AWS.FIS.DeleteExperimentTemplate
import Network.AWS.FIS.GetAction
import Network.AWS.FIS.GetExperiment
import Network.AWS.FIS.GetExperimentTemplate
import Network.AWS.FIS.ListActions
import Network.AWS.FIS.ListExperimentTemplates
import Network.AWS.FIS.ListExperiments
import Network.AWS.FIS.ListTagsForResource
import Network.AWS.FIS.StartExperiment
import Network.AWS.FIS.StopExperiment
import Network.AWS.FIS.TagResource
import Network.AWS.FIS.Types.Action
import Network.AWS.FIS.Types.ActionParameter
import Network.AWS.FIS.Types.ActionSummary
import Network.AWS.FIS.Types.ActionTarget
import Network.AWS.FIS.Types.CreateExperimentTemplateActionInput
import Network.AWS.FIS.Types.CreateExperimentTemplateStopConditionInput
import Network.AWS.FIS.Types.CreateExperimentTemplateTargetInput
import Network.AWS.FIS.Types.Experiment
import Network.AWS.FIS.Types.ExperimentAction
import Network.AWS.FIS.Types.ExperimentActionState
import Network.AWS.FIS.Types.ExperimentState
import Network.AWS.FIS.Types.ExperimentStopCondition
import Network.AWS.FIS.Types.ExperimentSummary
import Network.AWS.FIS.Types.ExperimentTarget
import Network.AWS.FIS.Types.ExperimentTargetFilter
import Network.AWS.FIS.Types.ExperimentTemplate
import Network.AWS.FIS.Types.ExperimentTemplateAction
import Network.AWS.FIS.Types.ExperimentTemplateStopCondition
import Network.AWS.FIS.Types.ExperimentTemplateSummary
import Network.AWS.FIS.Types.ExperimentTemplateTarget
import Network.AWS.FIS.Types.ExperimentTemplateTargetFilter
import Network.AWS.FIS.Types.ExperimentTemplateTargetInputFilter
import Network.AWS.FIS.Types.UpdateExperimentTemplateActionInputItem
import Network.AWS.FIS.Types.UpdateExperimentTemplateStopConditionInput
import Network.AWS.FIS.Types.UpdateExperimentTemplateTargetInput
import Network.AWS.FIS.UntagResource
import Network.AWS.FIS.UpdateExperimentTemplate
