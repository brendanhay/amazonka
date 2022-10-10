{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubOrchestrator.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Lens
  ( -- * Operations

    -- ** CreateWorkflow
    createWorkflow_tags,
    createWorkflow_stepTargets,
    createWorkflow_description,
    createWorkflow_name,
    createWorkflow_templateId,
    createWorkflow_applicationConfigurationId,
    createWorkflow_inputParameters,
    createWorkflowResponse_tags,
    createWorkflowResponse_name,
    createWorkflowResponse_stepTargets,
    createWorkflowResponse_adsApplicationConfigurationId,
    createWorkflowResponse_arn,
    createWorkflowResponse_status,
    createWorkflowResponse_description,
    createWorkflowResponse_templateId,
    createWorkflowResponse_id,
    createWorkflowResponse_creationTime,
    createWorkflowResponse_workflowInputs,
    createWorkflowResponse_httpStatus,

    -- ** CreateWorkflowStep
    createWorkflowStep_next,
    createWorkflowStep_description,
    createWorkflowStep_workflowStepAutomationConfiguration,
    createWorkflowStep_stepTarget,
    createWorkflowStep_outputs,
    createWorkflowStep_previous,
    createWorkflowStep_name,
    createWorkflowStep_stepGroupId,
    createWorkflowStep_workflowId,
    createWorkflowStep_stepActionType,
    createWorkflowStepResponse_name,
    createWorkflowStepResponse_workflowId,
    createWorkflowStepResponse_id,
    createWorkflowStepResponse_stepGroupId,
    createWorkflowStepResponse_httpStatus,

    -- ** CreateWorkflowStepGroup
    createWorkflowStepGroup_next,
    createWorkflowStepGroup_description,
    createWorkflowStepGroup_previous,
    createWorkflowStepGroup_workflowId,
    createWorkflowStepGroup_name,
    createWorkflowStepGroupResponse_name,
    createWorkflowStepGroupResponse_workflowId,
    createWorkflowStepGroupResponse_tools,
    createWorkflowStepGroupResponse_next,
    createWorkflowStepGroupResponse_description,
    createWorkflowStepGroupResponse_id,
    createWorkflowStepGroupResponse_creationTime,
    createWorkflowStepGroupResponse_previous,
    createWorkflowStepGroupResponse_httpStatus,

    -- ** DeleteWorkflow
    deleteWorkflow_id,
    deleteWorkflowResponse_arn,
    deleteWorkflowResponse_status,
    deleteWorkflowResponse_id,
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
    getTemplateResponse_name,
    getTemplateResponse_tools,
    getTemplateResponse_status,
    getTemplateResponse_description,
    getTemplateResponse_id,
    getTemplateResponse_creationTime,
    getTemplateResponse_inputs,
    getTemplateResponse_httpStatus,

    -- ** GetTemplateStep
    getTemplateStep_id,
    getTemplateStep_templateId,
    getTemplateStep_stepGroupId,
    getTemplateStepResponse_name,
    getTemplateStepResponse_next,
    getTemplateStepResponse_stepAutomationConfiguration,
    getTemplateStepResponse_stepActionType,
    getTemplateStepResponse_description,
    getTemplateStepResponse_templateId,
    getTemplateStepResponse_id,
    getTemplateStepResponse_stepGroupId,
    getTemplateStepResponse_outputs,
    getTemplateStepResponse_creationTime,
    getTemplateStepResponse_previous,
    getTemplateStepResponse_httpStatus,

    -- ** GetTemplateStepGroup
    getTemplateStepGroup_templateId,
    getTemplateStepGroup_id,
    getTemplateStepGroupResponse_name,
    getTemplateStepGroupResponse_tools,
    getTemplateStepGroupResponse_next,
    getTemplateStepGroupResponse_status,
    getTemplateStepGroupResponse_description,
    getTemplateStepGroupResponse_templateId,
    getTemplateStepGroupResponse_id,
    getTemplateStepGroupResponse_lastModifiedTime,
    getTemplateStepGroupResponse_creationTime,
    getTemplateStepGroupResponse_previous,
    getTemplateStepGroupResponse_httpStatus,

    -- ** GetWorkflow
    getWorkflow_id,
    getWorkflowResponse_tags,
    getWorkflowResponse_name,
    getWorkflowResponse_workflowBucket,
    getWorkflowResponse_adsApplicationName,
    getWorkflowResponse_adsApplicationConfigurationId,
    getWorkflowResponse_tools,
    getWorkflowResponse_lastStartTime,
    getWorkflowResponse_arn,
    getWorkflowResponse_status,
    getWorkflowResponse_description,
    getWorkflowResponse_templateId,
    getWorkflowResponse_endTime,
    getWorkflowResponse_id,
    getWorkflowResponse_lastModifiedTime,
    getWorkflowResponse_lastStopTime,
    getWorkflowResponse_completedSteps,
    getWorkflowResponse_creationTime,
    getWorkflowResponse_statusMessage,
    getWorkflowResponse_totalSteps,
    getWorkflowResponse_workflowInputs,
    getWorkflowResponse_httpStatus,

    -- ** GetWorkflowStep
    getWorkflowStep_workflowId,
    getWorkflowStep_stepGroupId,
    getWorkflowStep_id,
    getWorkflowStepResponse_name,
    getWorkflowStepResponse_workflowId,
    getWorkflowStepResponse_scriptOutputLocation,
    getWorkflowStepResponse_noOfSrvFailed,
    getWorkflowStepResponse_next,
    getWorkflowStepResponse_lastStartTime,
    getWorkflowStepResponse_status,
    getWorkflowStepResponse_owner,
    getWorkflowStepResponse_noOfSrvCompleted,
    getWorkflowStepResponse_stepActionType,
    getWorkflowStepResponse_description,
    getWorkflowStepResponse_workflowStepAutomationConfiguration,
    getWorkflowStepResponse_endTime,
    getWorkflowStepResponse_stepTarget,
    getWorkflowStepResponse_stepGroupId,
    getWorkflowStepResponse_outputs,
    getWorkflowStepResponse_creationTime,
    getWorkflowStepResponse_statusMessage,
    getWorkflowStepResponse_stepId,
    getWorkflowStepResponse_previous,
    getWorkflowStepResponse_totalNoOfSrv,
    getWorkflowStepResponse_httpStatus,

    -- ** GetWorkflowStepGroup
    getWorkflowStepGroup_id,
    getWorkflowStepGroup_workflowId,
    getWorkflowStepGroupResponse_name,
    getWorkflowStepGroupResponse_workflowId,
    getWorkflowStepGroupResponse_tools,
    getWorkflowStepGroupResponse_next,
    getWorkflowStepGroupResponse_status,
    getWorkflowStepGroupResponse_owner,
    getWorkflowStepGroupResponse_description,
    getWorkflowStepGroupResponse_endTime,
    getWorkflowStepGroupResponse_id,
    getWorkflowStepGroupResponse_lastModifiedTime,
    getWorkflowStepGroupResponse_creationTime,
    getWorkflowStepGroupResponse_previous,
    getWorkflowStepGroupResponse_httpStatus,

    -- ** ListPlugins
    listPlugins_nextToken,
    listPlugins_maxResults,
    listPluginsResponse_nextToken,
    listPluginsResponse_plugins,
    listPluginsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTemplateStepGroups
    listTemplateStepGroups_nextToken,
    listTemplateStepGroups_maxResults,
    listTemplateStepGroups_templateId,
    listTemplateStepGroupsResponse_nextToken,
    listTemplateStepGroupsResponse_httpStatus,
    listTemplateStepGroupsResponse_templateStepGroupSummary,

    -- ** ListTemplateSteps
    listTemplateSteps_nextToken,
    listTemplateSteps_maxResults,
    listTemplateSteps_templateId,
    listTemplateSteps_stepGroupId,
    listTemplateStepsResponse_nextToken,
    listTemplateStepsResponse_templateStepSummaryList,
    listTemplateStepsResponse_httpStatus,

    -- ** ListTemplates
    listTemplates_name,
    listTemplates_nextToken,
    listTemplates_maxResults,
    listTemplatesResponse_nextToken,
    listTemplatesResponse_httpStatus,
    listTemplatesResponse_templateSummary,

    -- ** ListWorkflowStepGroups
    listWorkflowStepGroups_nextToken,
    listWorkflowStepGroups_maxResults,
    listWorkflowStepGroups_workflowId,
    listWorkflowStepGroupsResponse_nextToken,
    listWorkflowStepGroupsResponse_httpStatus,
    listWorkflowStepGroupsResponse_workflowStepGroupsSummary,

    -- ** ListWorkflowSteps
    listWorkflowSteps_nextToken,
    listWorkflowSteps_maxResults,
    listWorkflowSteps_workflowId,
    listWorkflowSteps_stepGroupId,
    listWorkflowStepsResponse_nextToken,
    listWorkflowStepsResponse_httpStatus,
    listWorkflowStepsResponse_workflowStepsSummary,

    -- ** ListWorkflows
    listWorkflows_adsApplicationConfigurationName,
    listWorkflows_name,
    listWorkflows_nextToken,
    listWorkflows_status,
    listWorkflows_templateId,
    listWorkflows_maxResults,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_httpStatus,
    listWorkflowsResponse_migrationWorkflowSummary,

    -- ** RetryWorkflowStep
    retryWorkflowStep_workflowId,
    retryWorkflowStep_stepGroupId,
    retryWorkflowStep_id,
    retryWorkflowStepResponse_workflowId,
    retryWorkflowStepResponse_status,
    retryWorkflowStepResponse_id,
    retryWorkflowStepResponse_stepGroupId,
    retryWorkflowStepResponse_httpStatus,

    -- ** StartWorkflow
    startWorkflow_id,
    startWorkflowResponse_lastStartTime,
    startWorkflowResponse_arn,
    startWorkflowResponse_status,
    startWorkflowResponse_id,
    startWorkflowResponse_statusMessage,
    startWorkflowResponse_httpStatus,

    -- ** StopWorkflow
    stopWorkflow_id,
    stopWorkflowResponse_arn,
    stopWorkflowResponse_status,
    stopWorkflowResponse_id,
    stopWorkflowResponse_lastStopTime,
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
    updateWorkflow_name,
    updateWorkflow_stepTargets,
    updateWorkflow_inputParameters,
    updateWorkflow_description,
    updateWorkflow_id,
    updateWorkflowResponse_tags,
    updateWorkflowResponse_name,
    updateWorkflowResponse_stepTargets,
    updateWorkflowResponse_adsApplicationConfigurationId,
    updateWorkflowResponse_arn,
    updateWorkflowResponse_status,
    updateWorkflowResponse_description,
    updateWorkflowResponse_templateId,
    updateWorkflowResponse_id,
    updateWorkflowResponse_lastModifiedTime,
    updateWorkflowResponse_creationTime,
    updateWorkflowResponse_workflowInputs,
    updateWorkflowResponse_httpStatus,

    -- ** UpdateWorkflowStep
    updateWorkflowStep_name,
    updateWorkflowStep_next,
    updateWorkflowStep_status,
    updateWorkflowStep_stepActionType,
    updateWorkflowStep_description,
    updateWorkflowStep_workflowStepAutomationConfiguration,
    updateWorkflowStep_stepTarget,
    updateWorkflowStep_outputs,
    updateWorkflowStep_previous,
    updateWorkflowStep_id,
    updateWorkflowStep_stepGroupId,
    updateWorkflowStep_workflowId,
    updateWorkflowStepResponse_name,
    updateWorkflowStepResponse_workflowId,
    updateWorkflowStepResponse_id,
    updateWorkflowStepResponse_stepGroupId,
    updateWorkflowStepResponse_httpStatus,

    -- ** UpdateWorkflowStepGroup
    updateWorkflowStepGroup_name,
    updateWorkflowStepGroup_next,
    updateWorkflowStepGroup_description,
    updateWorkflowStepGroup_previous,
    updateWorkflowStepGroup_workflowId,
    updateWorkflowStepGroup_id,
    updateWorkflowStepGroupResponse_name,
    updateWorkflowStepGroupResponse_workflowId,
    updateWorkflowStepGroupResponse_tools,
    updateWorkflowStepGroupResponse_next,
    updateWorkflowStepGroupResponse_description,
    updateWorkflowStepGroupResponse_id,
    updateWorkflowStepGroupResponse_lastModifiedTime,
    updateWorkflowStepGroupResponse_previous,
    updateWorkflowStepGroupResponse_httpStatus,

    -- * Types

    -- ** MigrationWorkflowSummary
    migrationWorkflowSummary_adsApplicationConfigurationName,
    migrationWorkflowSummary_name,
    migrationWorkflowSummary_status,
    migrationWorkflowSummary_templateId,
    migrationWorkflowSummary_endTime,
    migrationWorkflowSummary_id,
    migrationWorkflowSummary_completedSteps,
    migrationWorkflowSummary_creationTime,
    migrationWorkflowSummary_statusMessage,
    migrationWorkflowSummary_totalSteps,

    -- ** PlatformCommand
    platformCommand_windows,
    platformCommand_linux,

    -- ** PlatformScriptKey
    platformScriptKey_windows,
    platformScriptKey_linux,

    -- ** PluginSummary
    pluginSummary_pluginId,
    pluginSummary_status,
    pluginSummary_hostname,
    pluginSummary_registeredTime,
    pluginSummary_version,
    pluginSummary_ipAddress,

    -- ** StepAutomationConfiguration
    stepAutomationConfiguration_command,
    stepAutomationConfiguration_targetType,
    stepAutomationConfiguration_scriptLocationS3Key,
    stepAutomationConfiguration_scriptLocationS3Bucket,
    stepAutomationConfiguration_runEnvironment,

    -- ** StepInput
    stepInput_integerValue,
    stepInput_listOfStringsValue,
    stepInput_mapOfStringValue,
    stepInput_stringValue,

    -- ** StepOutput
    stepOutput_name,
    stepOutput_required,
    stepOutput_dataType,

    -- ** TemplateInput
    templateInput_required,
    templateInput_inputName,
    templateInput_dataType,

    -- ** TemplateStepGroupSummary
    templateStepGroupSummary_name,
    templateStepGroupSummary_next,
    templateStepGroupSummary_id,
    templateStepGroupSummary_previous,

    -- ** TemplateStepSummary
    templateStepSummary_name,
    templateStepSummary_next,
    templateStepSummary_owner,
    templateStepSummary_stepActionType,
    templateStepSummary_templateId,
    templateStepSummary_id,
    templateStepSummary_targetType,
    templateStepSummary_stepGroupId,
    templateStepSummary_previous,

    -- ** TemplateSummary
    templateSummary_name,
    templateSummary_arn,
    templateSummary_description,
    templateSummary_id,

    -- ** Tool
    tool_name,
    tool_url,

    -- ** WorkflowStepAutomationConfiguration
    workflowStepAutomationConfiguration_command,
    workflowStepAutomationConfiguration_targetType,
    workflowStepAutomationConfiguration_scriptLocationS3Key,
    workflowStepAutomationConfiguration_scriptLocationS3Bucket,
    workflowStepAutomationConfiguration_runEnvironment,

    -- ** WorkflowStepGroupSummary
    workflowStepGroupSummary_name,
    workflowStepGroupSummary_next,
    workflowStepGroupSummary_status,
    workflowStepGroupSummary_owner,
    workflowStepGroupSummary_id,
    workflowStepGroupSummary_previous,

    -- ** WorkflowStepOutput
    workflowStepOutput_name,
    workflowStepOutput_required,
    workflowStepOutput_value,
    workflowStepOutput_dataType,

    -- ** WorkflowStepOutputUnion
    workflowStepOutputUnion_integerValue,
    workflowStepOutputUnion_listOfStringValue,
    workflowStepOutputUnion_stringValue,

    -- ** WorkflowStepSummary
    workflowStepSummary_name,
    workflowStepSummary_scriptLocation,
    workflowStepSummary_noOfSrvFailed,
    workflowStepSummary_next,
    workflowStepSummary_status,
    workflowStepSummary_owner,
    workflowStepSummary_noOfSrvCompleted,
    workflowStepSummary_stepActionType,
    workflowStepSummary_description,
    workflowStepSummary_statusMessage,
    workflowStepSummary_stepId,
    workflowStepSummary_previous,
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
