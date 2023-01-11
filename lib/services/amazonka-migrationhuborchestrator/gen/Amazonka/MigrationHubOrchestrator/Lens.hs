{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubOrchestrator.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Lens
  ( -- * Operations

    -- ** CreateWorkflow
    createWorkflow_description,
    createWorkflow_stepTargets,
    createWorkflow_tags,
    createWorkflow_name,
    createWorkflow_templateId,
    createWorkflow_applicationConfigurationId,
    createWorkflow_inputParameters,
    createWorkflowResponse_adsApplicationConfigurationId,
    createWorkflowResponse_arn,
    createWorkflowResponse_creationTime,
    createWorkflowResponse_description,
    createWorkflowResponse_id,
    createWorkflowResponse_name,
    createWorkflowResponse_status,
    createWorkflowResponse_stepTargets,
    createWorkflowResponse_tags,
    createWorkflowResponse_templateId,
    createWorkflowResponse_workflowInputs,
    createWorkflowResponse_httpStatus,

    -- ** CreateWorkflowStep
    createWorkflowStep_description,
    createWorkflowStep_next,
    createWorkflowStep_outputs,
    createWorkflowStep_previous,
    createWorkflowStep_stepTarget,
    createWorkflowStep_workflowStepAutomationConfiguration,
    createWorkflowStep_name,
    createWorkflowStep_stepGroupId,
    createWorkflowStep_workflowId,
    createWorkflowStep_stepActionType,
    createWorkflowStepResponse_id,
    createWorkflowStepResponse_name,
    createWorkflowStepResponse_stepGroupId,
    createWorkflowStepResponse_workflowId,
    createWorkflowStepResponse_httpStatus,

    -- ** CreateWorkflowStepGroup
    createWorkflowStepGroup_description,
    createWorkflowStepGroup_next,
    createWorkflowStepGroup_previous,
    createWorkflowStepGroup_workflowId,
    createWorkflowStepGroup_name,
    createWorkflowStepGroupResponse_creationTime,
    createWorkflowStepGroupResponse_description,
    createWorkflowStepGroupResponse_id,
    createWorkflowStepGroupResponse_name,
    createWorkflowStepGroupResponse_next,
    createWorkflowStepGroupResponse_previous,
    createWorkflowStepGroupResponse_tools,
    createWorkflowStepGroupResponse_workflowId,
    createWorkflowStepGroupResponse_httpStatus,

    -- ** DeleteWorkflow
    deleteWorkflow_id,
    deleteWorkflowResponse_arn,
    deleteWorkflowResponse_id,
    deleteWorkflowResponse_status,
    deleteWorkflowResponse_httpStatus,

    -- ** DeleteWorkflowStep
    deleteWorkflowStep_id,
    deleteWorkflowStep_stepGroupId,
    deleteWorkflowStep_workflowId,
    deleteWorkflowStepResponse_httpStatus,

    -- ** DeleteWorkflowStepGroup
    deleteWorkflowStepGroup_workflowId,
    deleteWorkflowStepGroup_id,
    deleteWorkflowStepGroupResponse_httpStatus,

    -- ** GetTemplate
    getTemplate_id,
    getTemplateResponse_creationTime,
    getTemplateResponse_description,
    getTemplateResponse_id,
    getTemplateResponse_inputs,
    getTemplateResponse_name,
    getTemplateResponse_status,
    getTemplateResponse_tools,
    getTemplateResponse_httpStatus,

    -- ** GetTemplateStep
    getTemplateStep_id,
    getTemplateStep_templateId,
    getTemplateStep_stepGroupId,
    getTemplateStepResponse_creationTime,
    getTemplateStepResponse_description,
    getTemplateStepResponse_id,
    getTemplateStepResponse_name,
    getTemplateStepResponse_next,
    getTemplateStepResponse_outputs,
    getTemplateStepResponse_previous,
    getTemplateStepResponse_stepActionType,
    getTemplateStepResponse_stepAutomationConfiguration,
    getTemplateStepResponse_stepGroupId,
    getTemplateStepResponse_templateId,
    getTemplateStepResponse_httpStatus,

    -- ** GetTemplateStepGroup
    getTemplateStepGroup_templateId,
    getTemplateStepGroup_id,
    getTemplateStepGroupResponse_creationTime,
    getTemplateStepGroupResponse_description,
    getTemplateStepGroupResponse_id,
    getTemplateStepGroupResponse_lastModifiedTime,
    getTemplateStepGroupResponse_name,
    getTemplateStepGroupResponse_next,
    getTemplateStepGroupResponse_previous,
    getTemplateStepGroupResponse_status,
    getTemplateStepGroupResponse_templateId,
    getTemplateStepGroupResponse_tools,
    getTemplateStepGroupResponse_httpStatus,

    -- ** GetWorkflow
    getWorkflow_id,
    getWorkflowResponse_adsApplicationConfigurationId,
    getWorkflowResponse_adsApplicationName,
    getWorkflowResponse_arn,
    getWorkflowResponse_completedSteps,
    getWorkflowResponse_creationTime,
    getWorkflowResponse_description,
    getWorkflowResponse_endTime,
    getWorkflowResponse_id,
    getWorkflowResponse_lastModifiedTime,
    getWorkflowResponse_lastStartTime,
    getWorkflowResponse_lastStopTime,
    getWorkflowResponse_name,
    getWorkflowResponse_status,
    getWorkflowResponse_statusMessage,
    getWorkflowResponse_tags,
    getWorkflowResponse_templateId,
    getWorkflowResponse_tools,
    getWorkflowResponse_totalSteps,
    getWorkflowResponse_workflowBucket,
    getWorkflowResponse_workflowInputs,
    getWorkflowResponse_httpStatus,

    -- ** GetWorkflowStep
    getWorkflowStep_workflowId,
    getWorkflowStep_stepGroupId,
    getWorkflowStep_id,
    getWorkflowStepResponse_creationTime,
    getWorkflowStepResponse_description,
    getWorkflowStepResponse_endTime,
    getWorkflowStepResponse_lastStartTime,
    getWorkflowStepResponse_name,
    getWorkflowStepResponse_next,
    getWorkflowStepResponse_noOfSrvCompleted,
    getWorkflowStepResponse_noOfSrvFailed,
    getWorkflowStepResponse_outputs,
    getWorkflowStepResponse_owner,
    getWorkflowStepResponse_previous,
    getWorkflowStepResponse_scriptOutputLocation,
    getWorkflowStepResponse_status,
    getWorkflowStepResponse_statusMessage,
    getWorkflowStepResponse_stepActionType,
    getWorkflowStepResponse_stepGroupId,
    getWorkflowStepResponse_stepId,
    getWorkflowStepResponse_stepTarget,
    getWorkflowStepResponse_totalNoOfSrv,
    getWorkflowStepResponse_workflowId,
    getWorkflowStepResponse_workflowStepAutomationConfiguration,
    getWorkflowStepResponse_httpStatus,

    -- ** GetWorkflowStepGroup
    getWorkflowStepGroup_id,
    getWorkflowStepGroup_workflowId,
    getWorkflowStepGroupResponse_creationTime,
    getWorkflowStepGroupResponse_description,
    getWorkflowStepGroupResponse_endTime,
    getWorkflowStepGroupResponse_id,
    getWorkflowStepGroupResponse_lastModifiedTime,
    getWorkflowStepGroupResponse_name,
    getWorkflowStepGroupResponse_next,
    getWorkflowStepGroupResponse_owner,
    getWorkflowStepGroupResponse_previous,
    getWorkflowStepGroupResponse_status,
    getWorkflowStepGroupResponse_tools,
    getWorkflowStepGroupResponse_workflowId,
    getWorkflowStepGroupResponse_httpStatus,

    -- ** ListPlugins
    listPlugins_maxResults,
    listPlugins_nextToken,
    listPluginsResponse_nextToken,
    listPluginsResponse_plugins,
    listPluginsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTemplateStepGroups
    listTemplateStepGroups_maxResults,
    listTemplateStepGroups_nextToken,
    listTemplateStepGroups_templateId,
    listTemplateStepGroupsResponse_nextToken,
    listTemplateStepGroupsResponse_httpStatus,
    listTemplateStepGroupsResponse_templateStepGroupSummary,

    -- ** ListTemplateSteps
    listTemplateSteps_maxResults,
    listTemplateSteps_nextToken,
    listTemplateSteps_templateId,
    listTemplateSteps_stepGroupId,
    listTemplateStepsResponse_nextToken,
    listTemplateStepsResponse_templateStepSummaryList,
    listTemplateStepsResponse_httpStatus,

    -- ** ListTemplates
    listTemplates_maxResults,
    listTemplates_name,
    listTemplates_nextToken,
    listTemplatesResponse_nextToken,
    listTemplatesResponse_httpStatus,
    listTemplatesResponse_templateSummary,

    -- ** ListWorkflowStepGroups
    listWorkflowStepGroups_maxResults,
    listWorkflowStepGroups_nextToken,
    listWorkflowStepGroups_workflowId,
    listWorkflowStepGroupsResponse_nextToken,
    listWorkflowStepGroupsResponse_httpStatus,
    listWorkflowStepGroupsResponse_workflowStepGroupsSummary,

    -- ** ListWorkflowSteps
    listWorkflowSteps_maxResults,
    listWorkflowSteps_nextToken,
    listWorkflowSteps_workflowId,
    listWorkflowSteps_stepGroupId,
    listWorkflowStepsResponse_nextToken,
    listWorkflowStepsResponse_httpStatus,
    listWorkflowStepsResponse_workflowStepsSummary,

    -- ** ListWorkflows
    listWorkflows_adsApplicationConfigurationName,
    listWorkflows_maxResults,
    listWorkflows_name,
    listWorkflows_nextToken,
    listWorkflows_status,
    listWorkflows_templateId,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_httpStatus,
    listWorkflowsResponse_migrationWorkflowSummary,

    -- ** RetryWorkflowStep
    retryWorkflowStep_workflowId,
    retryWorkflowStep_stepGroupId,
    retryWorkflowStep_id,
    retryWorkflowStepResponse_id,
    retryWorkflowStepResponse_status,
    retryWorkflowStepResponse_stepGroupId,
    retryWorkflowStepResponse_workflowId,
    retryWorkflowStepResponse_httpStatus,

    -- ** StartWorkflow
    startWorkflow_id,
    startWorkflowResponse_arn,
    startWorkflowResponse_id,
    startWorkflowResponse_lastStartTime,
    startWorkflowResponse_status,
    startWorkflowResponse_statusMessage,
    startWorkflowResponse_httpStatus,

    -- ** StopWorkflow
    stopWorkflow_id,
    stopWorkflowResponse_arn,
    stopWorkflowResponse_id,
    stopWorkflowResponse_lastStopTime,
    stopWorkflowResponse_status,
    stopWorkflowResponse_statusMessage,
    stopWorkflowResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateWorkflow
    updateWorkflow_description,
    updateWorkflow_inputParameters,
    updateWorkflow_name,
    updateWorkflow_stepTargets,
    updateWorkflow_id,
    updateWorkflowResponse_adsApplicationConfigurationId,
    updateWorkflowResponse_arn,
    updateWorkflowResponse_creationTime,
    updateWorkflowResponse_description,
    updateWorkflowResponse_id,
    updateWorkflowResponse_lastModifiedTime,
    updateWorkflowResponse_name,
    updateWorkflowResponse_status,
    updateWorkflowResponse_stepTargets,
    updateWorkflowResponse_tags,
    updateWorkflowResponse_templateId,
    updateWorkflowResponse_workflowInputs,
    updateWorkflowResponse_httpStatus,

    -- ** UpdateWorkflowStep
    updateWorkflowStep_description,
    updateWorkflowStep_name,
    updateWorkflowStep_next,
    updateWorkflowStep_outputs,
    updateWorkflowStep_previous,
    updateWorkflowStep_status,
    updateWorkflowStep_stepActionType,
    updateWorkflowStep_stepTarget,
    updateWorkflowStep_workflowStepAutomationConfiguration,
    updateWorkflowStep_id,
    updateWorkflowStep_stepGroupId,
    updateWorkflowStep_workflowId,
    updateWorkflowStepResponse_id,
    updateWorkflowStepResponse_name,
    updateWorkflowStepResponse_stepGroupId,
    updateWorkflowStepResponse_workflowId,
    updateWorkflowStepResponse_httpStatus,

    -- ** UpdateWorkflowStepGroup
    updateWorkflowStepGroup_description,
    updateWorkflowStepGroup_name,
    updateWorkflowStepGroup_next,
    updateWorkflowStepGroup_previous,
    updateWorkflowStepGroup_workflowId,
    updateWorkflowStepGroup_id,
    updateWorkflowStepGroupResponse_description,
    updateWorkflowStepGroupResponse_id,
    updateWorkflowStepGroupResponse_lastModifiedTime,
    updateWorkflowStepGroupResponse_name,
    updateWorkflowStepGroupResponse_next,
    updateWorkflowStepGroupResponse_previous,
    updateWorkflowStepGroupResponse_tools,
    updateWorkflowStepGroupResponse_workflowId,
    updateWorkflowStepGroupResponse_httpStatus,

    -- * Types

    -- ** MigrationWorkflowSummary
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

    -- ** PlatformCommand
    platformCommand_linux,
    platformCommand_windows,

    -- ** PlatformScriptKey
    platformScriptKey_linux,
    platformScriptKey_windows,

    -- ** PluginSummary
    pluginSummary_hostname,
    pluginSummary_ipAddress,
    pluginSummary_pluginId,
    pluginSummary_registeredTime,
    pluginSummary_status,
    pluginSummary_version,

    -- ** StepAutomationConfiguration
    stepAutomationConfiguration_command,
    stepAutomationConfiguration_runEnvironment,
    stepAutomationConfiguration_scriptLocationS3Bucket,
    stepAutomationConfiguration_scriptLocationS3Key,
    stepAutomationConfiguration_targetType,

    -- ** StepInput
    stepInput_integerValue,
    stepInput_listOfStringsValue,
    stepInput_mapOfStringValue,
    stepInput_stringValue,

    -- ** StepOutput
    stepOutput_dataType,
    stepOutput_name,
    stepOutput_required,

    -- ** TemplateInput
    templateInput_dataType,
    templateInput_inputName,
    templateInput_required,

    -- ** TemplateStepGroupSummary
    templateStepGroupSummary_id,
    templateStepGroupSummary_name,
    templateStepGroupSummary_next,
    templateStepGroupSummary_previous,

    -- ** TemplateStepSummary
    templateStepSummary_id,
    templateStepSummary_name,
    templateStepSummary_next,
    templateStepSummary_owner,
    templateStepSummary_previous,
    templateStepSummary_stepActionType,
    templateStepSummary_stepGroupId,
    templateStepSummary_targetType,
    templateStepSummary_templateId,

    -- ** TemplateSummary
    templateSummary_arn,
    templateSummary_description,
    templateSummary_id,
    templateSummary_name,

    -- ** Tool
    tool_name,
    tool_url,

    -- ** WorkflowStepAutomationConfiguration
    workflowStepAutomationConfiguration_command,
    workflowStepAutomationConfiguration_runEnvironment,
    workflowStepAutomationConfiguration_scriptLocationS3Bucket,
    workflowStepAutomationConfiguration_scriptLocationS3Key,
    workflowStepAutomationConfiguration_targetType,

    -- ** WorkflowStepGroupSummary
    workflowStepGroupSummary_id,
    workflowStepGroupSummary_name,
    workflowStepGroupSummary_next,
    workflowStepGroupSummary_owner,
    workflowStepGroupSummary_previous,
    workflowStepGroupSummary_status,

    -- ** WorkflowStepOutput
    workflowStepOutput_dataType,
    workflowStepOutput_name,
    workflowStepOutput_required,
    workflowStepOutput_value,

    -- ** WorkflowStepOutputUnion
    workflowStepOutputUnion_integerValue,
    workflowStepOutputUnion_listOfStringValue,
    workflowStepOutputUnion_stringValue,

    -- ** WorkflowStepSummary
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

import Amazonka.MigrationHubOrchestrator.CreateWorkflow
import Amazonka.MigrationHubOrchestrator.CreateWorkflowStep
import Amazonka.MigrationHubOrchestrator.CreateWorkflowStepGroup
import Amazonka.MigrationHubOrchestrator.DeleteWorkflow
import Amazonka.MigrationHubOrchestrator.DeleteWorkflowStep
import Amazonka.MigrationHubOrchestrator.DeleteWorkflowStepGroup
import Amazonka.MigrationHubOrchestrator.GetTemplate
import Amazonka.MigrationHubOrchestrator.GetTemplateStep
import Amazonka.MigrationHubOrchestrator.GetTemplateStepGroup
import Amazonka.MigrationHubOrchestrator.GetWorkflow
import Amazonka.MigrationHubOrchestrator.GetWorkflowStep
import Amazonka.MigrationHubOrchestrator.GetWorkflowStepGroup
import Amazonka.MigrationHubOrchestrator.ListPlugins
import Amazonka.MigrationHubOrchestrator.ListTagsForResource
import Amazonka.MigrationHubOrchestrator.ListTemplateStepGroups
import Amazonka.MigrationHubOrchestrator.ListTemplateSteps
import Amazonka.MigrationHubOrchestrator.ListTemplates
import Amazonka.MigrationHubOrchestrator.ListWorkflowStepGroups
import Amazonka.MigrationHubOrchestrator.ListWorkflowSteps
import Amazonka.MigrationHubOrchestrator.ListWorkflows
import Amazonka.MigrationHubOrchestrator.RetryWorkflowStep
import Amazonka.MigrationHubOrchestrator.StartWorkflow
import Amazonka.MigrationHubOrchestrator.StopWorkflow
import Amazonka.MigrationHubOrchestrator.TagResource
import Amazonka.MigrationHubOrchestrator.Types.MigrationWorkflowSummary
import Amazonka.MigrationHubOrchestrator.Types.PlatformCommand
import Amazonka.MigrationHubOrchestrator.Types.PlatformScriptKey
import Amazonka.MigrationHubOrchestrator.Types.PluginSummary
import Amazonka.MigrationHubOrchestrator.Types.StepAutomationConfiguration
import Amazonka.MigrationHubOrchestrator.Types.StepInput
import Amazonka.MigrationHubOrchestrator.Types.StepOutput
import Amazonka.MigrationHubOrchestrator.Types.TemplateInput
import Amazonka.MigrationHubOrchestrator.Types.TemplateStepGroupSummary
import Amazonka.MigrationHubOrchestrator.Types.TemplateStepSummary
import Amazonka.MigrationHubOrchestrator.Types.TemplateSummary
import Amazonka.MigrationHubOrchestrator.Types.Tool
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepAutomationConfiguration
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepGroupSummary
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepOutput
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepOutputUnion
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepSummary
import Amazonka.MigrationHubOrchestrator.UntagResource
import Amazonka.MigrationHubOrchestrator.UpdateWorkflow
import Amazonka.MigrationHubOrchestrator.UpdateWorkflowStep
import Amazonka.MigrationHubOrchestrator.UpdateWorkflowStepGroup
