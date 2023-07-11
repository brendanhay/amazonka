{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FIS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Lens
  ( -- * Operations

    -- ** CreateExperimentTemplate
    createExperimentTemplate_logConfiguration,
    createExperimentTemplate_tags,
    createExperimentTemplate_targets,
    createExperimentTemplate_clientToken,
    createExperimentTemplate_description,
    createExperimentTemplate_stopConditions,
    createExperimentTemplate_actions,
    createExperimentTemplate_roleArn,
    createExperimentTemplateResponse_experimentTemplate,
    createExperimentTemplateResponse_httpStatus,

    -- ** DeleteExperimentTemplate
    deleteExperimentTemplate_id,
    deleteExperimentTemplateResponse_experimentTemplate,
    deleteExperimentTemplateResponse_httpStatus,

    -- ** GetAction
    getAction_id,
    getActionResponse_action,
    getActionResponse_httpStatus,

    -- ** GetExperiment
    getExperiment_id,
    getExperimentResponse_experiment,
    getExperimentResponse_httpStatus,

    -- ** GetExperimentTemplate
    getExperimentTemplate_id,
    getExperimentTemplateResponse_experimentTemplate,
    getExperimentTemplateResponse_httpStatus,

    -- ** GetTargetResourceType
    getTargetResourceType_resourceType,
    getTargetResourceTypeResponse_targetResourceType,
    getTargetResourceTypeResponse_httpStatus,

    -- ** ListActions
    listActions_maxResults,
    listActions_nextToken,
    listActionsResponse_actions,
    listActionsResponse_nextToken,
    listActionsResponse_httpStatus,

    -- ** ListExperimentTemplates
    listExperimentTemplates_maxResults,
    listExperimentTemplates_nextToken,
    listExperimentTemplatesResponse_experimentTemplates,
    listExperimentTemplatesResponse_nextToken,
    listExperimentTemplatesResponse_httpStatus,

    -- ** ListExperiments
    listExperiments_maxResults,
    listExperiments_nextToken,
    listExperimentsResponse_experiments,
    listExperimentsResponse_nextToken,
    listExperimentsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTargetResourceTypes
    listTargetResourceTypes_maxResults,
    listTargetResourceTypes_nextToken,
    listTargetResourceTypesResponse_nextToken,
    listTargetResourceTypesResponse_targetResourceTypes,
    listTargetResourceTypesResponse_httpStatus,

    -- ** StartExperiment
    startExperiment_tags,
    startExperiment_clientToken,
    startExperiment_experimentTemplateId,
    startExperimentResponse_experiment,
    startExperimentResponse_httpStatus,

    -- ** StopExperiment
    stopExperiment_id,
    stopExperimentResponse_experiment,
    stopExperimentResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,
    untagResourceResponse_httpStatus,

    -- ** UpdateExperimentTemplate
    updateExperimentTemplate_actions,
    updateExperimentTemplate_description,
    updateExperimentTemplate_logConfiguration,
    updateExperimentTemplate_roleArn,
    updateExperimentTemplate_stopConditions,
    updateExperimentTemplate_targets,
    updateExperimentTemplate_id,
    updateExperimentTemplateResponse_experimentTemplate,
    updateExperimentTemplateResponse_httpStatus,

    -- * Types

    -- ** Action
    action_description,
    action_id,
    action_parameters,
    action_tags,
    action_targets,

    -- ** ActionParameter
    actionParameter_description,
    actionParameter_required,

    -- ** ActionSummary
    actionSummary_description,
    actionSummary_id,
    actionSummary_tags,
    actionSummary_targets,

    -- ** ActionTarget
    actionTarget_resourceType,

    -- ** CreateExperimentTemplateActionInput
    createExperimentTemplateActionInput_description,
    createExperimentTemplateActionInput_parameters,
    createExperimentTemplateActionInput_startAfter,
    createExperimentTemplateActionInput_targets,
    createExperimentTemplateActionInput_actionId,

    -- ** CreateExperimentTemplateLogConfigurationInput
    createExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration,
    createExperimentTemplateLogConfigurationInput_s3Configuration,
    createExperimentTemplateLogConfigurationInput_logSchemaVersion,

    -- ** CreateExperimentTemplateStopConditionInput
    createExperimentTemplateStopConditionInput_value,
    createExperimentTemplateStopConditionInput_source,

    -- ** CreateExperimentTemplateTargetInput
    createExperimentTemplateTargetInput_filters,
    createExperimentTemplateTargetInput_parameters,
    createExperimentTemplateTargetInput_resourceArns,
    createExperimentTemplateTargetInput_resourceTags,
    createExperimentTemplateTargetInput_resourceType,
    createExperimentTemplateTargetInput_selectionMode,

    -- ** Experiment
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

    -- ** ExperimentAction
    experimentAction_actionId,
    experimentAction_description,
    experimentAction_endTime,
    experimentAction_parameters,
    experimentAction_startAfter,
    experimentAction_startTime,
    experimentAction_state,
    experimentAction_targets,

    -- ** ExperimentActionState
    experimentActionState_reason,
    experimentActionState_status,

    -- ** ExperimentCloudWatchLogsLogConfiguration
    experimentCloudWatchLogsLogConfiguration_logGroupArn,

    -- ** ExperimentLogConfiguration
    experimentLogConfiguration_cloudWatchLogsConfiguration,
    experimentLogConfiguration_logSchemaVersion,
    experimentLogConfiguration_s3Configuration,

    -- ** ExperimentS3LogConfiguration
    experimentS3LogConfiguration_bucketName,
    experimentS3LogConfiguration_prefix,

    -- ** ExperimentState
    experimentState_reason,
    experimentState_status,

    -- ** ExperimentStopCondition
    experimentStopCondition_source,
    experimentStopCondition_value,

    -- ** ExperimentSummary
    experimentSummary_creationTime,
    experimentSummary_experimentTemplateId,
    experimentSummary_id,
    experimentSummary_state,
    experimentSummary_tags,

    -- ** ExperimentTarget
    experimentTarget_filters,
    experimentTarget_parameters,
    experimentTarget_resourceArns,
    experimentTarget_resourceTags,
    experimentTarget_resourceType,
    experimentTarget_selectionMode,

    -- ** ExperimentTargetFilter
    experimentTargetFilter_path,
    experimentTargetFilter_values,

    -- ** ExperimentTemplate
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

    -- ** ExperimentTemplateAction
    experimentTemplateAction_actionId,
    experimentTemplateAction_description,
    experimentTemplateAction_parameters,
    experimentTemplateAction_startAfter,
    experimentTemplateAction_targets,

    -- ** ExperimentTemplateCloudWatchLogsLogConfiguration
    experimentTemplateCloudWatchLogsLogConfiguration_logGroupArn,

    -- ** ExperimentTemplateCloudWatchLogsLogConfigurationInput
    experimentTemplateCloudWatchLogsLogConfigurationInput_logGroupArn,

    -- ** ExperimentTemplateLogConfiguration
    experimentTemplateLogConfiguration_cloudWatchLogsConfiguration,
    experimentTemplateLogConfiguration_logSchemaVersion,
    experimentTemplateLogConfiguration_s3Configuration,

    -- ** ExperimentTemplateS3LogConfiguration
    experimentTemplateS3LogConfiguration_bucketName,
    experimentTemplateS3LogConfiguration_prefix,

    -- ** ExperimentTemplateS3LogConfigurationInput
    experimentTemplateS3LogConfigurationInput_prefix,
    experimentTemplateS3LogConfigurationInput_bucketName,

    -- ** ExperimentTemplateStopCondition
    experimentTemplateStopCondition_source,
    experimentTemplateStopCondition_value,

    -- ** ExperimentTemplateSummary
    experimentTemplateSummary_creationTime,
    experimentTemplateSummary_description,
    experimentTemplateSummary_id,
    experimentTemplateSummary_lastUpdateTime,
    experimentTemplateSummary_tags,

    -- ** ExperimentTemplateTarget
    experimentTemplateTarget_filters,
    experimentTemplateTarget_parameters,
    experimentTemplateTarget_resourceArns,
    experimentTemplateTarget_resourceTags,
    experimentTemplateTarget_resourceType,
    experimentTemplateTarget_selectionMode,

    -- ** ExperimentTemplateTargetFilter
    experimentTemplateTargetFilter_path,
    experimentTemplateTargetFilter_values,

    -- ** ExperimentTemplateTargetInputFilter
    experimentTemplateTargetInputFilter_path,
    experimentTemplateTargetInputFilter_values,

    -- ** TargetResourceType
    targetResourceType_description,
    targetResourceType_parameters,
    targetResourceType_resourceType,

    -- ** TargetResourceTypeParameter
    targetResourceTypeParameter_description,
    targetResourceTypeParameter_required,

    -- ** TargetResourceTypeSummary
    targetResourceTypeSummary_description,
    targetResourceTypeSummary_resourceType,

    -- ** UpdateExperimentTemplateActionInputItem
    updateExperimentTemplateActionInputItem_actionId,
    updateExperimentTemplateActionInputItem_description,
    updateExperimentTemplateActionInputItem_parameters,
    updateExperimentTemplateActionInputItem_startAfter,
    updateExperimentTemplateActionInputItem_targets,

    -- ** UpdateExperimentTemplateLogConfigurationInput
    updateExperimentTemplateLogConfigurationInput_cloudWatchLogsConfiguration,
    updateExperimentTemplateLogConfigurationInput_logSchemaVersion,
    updateExperimentTemplateLogConfigurationInput_s3Configuration,

    -- ** UpdateExperimentTemplateStopConditionInput
    updateExperimentTemplateStopConditionInput_value,
    updateExperimentTemplateStopConditionInput_source,

    -- ** UpdateExperimentTemplateTargetInput
    updateExperimentTemplateTargetInput_filters,
    updateExperimentTemplateTargetInput_parameters,
    updateExperimentTemplateTargetInput_resourceArns,
    updateExperimentTemplateTargetInput_resourceTags,
    updateExperimentTemplateTargetInput_resourceType,
    updateExperimentTemplateTargetInput_selectionMode,
  )
where

import Amazonka.FIS.CreateExperimentTemplate
import Amazonka.FIS.DeleteExperimentTemplate
import Amazonka.FIS.GetAction
import Amazonka.FIS.GetExperiment
import Amazonka.FIS.GetExperimentTemplate
import Amazonka.FIS.GetTargetResourceType
import Amazonka.FIS.ListActions
import Amazonka.FIS.ListExperimentTemplates
import Amazonka.FIS.ListExperiments
import Amazonka.FIS.ListTagsForResource
import Amazonka.FIS.ListTargetResourceTypes
import Amazonka.FIS.StartExperiment
import Amazonka.FIS.StopExperiment
import Amazonka.FIS.TagResource
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
import Amazonka.FIS.Types.ExperimentCloudWatchLogsLogConfiguration
import Amazonka.FIS.Types.ExperimentLogConfiguration
import Amazonka.FIS.Types.ExperimentS3LogConfiguration
import Amazonka.FIS.Types.ExperimentState
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
import Amazonka.FIS.UntagResource
import Amazonka.FIS.UpdateExperimentTemplate
