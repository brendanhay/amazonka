{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MigrationHubOrchestrator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-08-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This API reference provides descriptions, syntax, and other details
-- about each of the actions and data types for AWS Migration Hub
-- Orchestrator. he topic for each action shows the API request parameters
-- and the response. Alternatively, you can use one of the AWS SDKs to
-- access an API that is tailored to the programming language or platform
-- that you\'re using.
module Amazonka.MigrationHubOrchestrator
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateWorkflow
    CreateWorkflow (CreateWorkflow'),
    newCreateWorkflow,
    CreateWorkflowResponse (CreateWorkflowResponse'),
    newCreateWorkflowResponse,

    -- ** CreateWorkflowStep
    CreateWorkflowStep (CreateWorkflowStep'),
    newCreateWorkflowStep,
    CreateWorkflowStepResponse (CreateWorkflowStepResponse'),
    newCreateWorkflowStepResponse,

    -- ** CreateWorkflowStepGroup
    CreateWorkflowStepGroup (CreateWorkflowStepGroup'),
    newCreateWorkflowStepGroup,
    CreateWorkflowStepGroupResponse (CreateWorkflowStepGroupResponse'),
    newCreateWorkflowStepGroupResponse,

    -- ** DeleteWorkflow
    DeleteWorkflow (DeleteWorkflow'),
    newDeleteWorkflow,
    DeleteWorkflowResponse (DeleteWorkflowResponse'),
    newDeleteWorkflowResponse,

    -- ** DeleteWorkflowStep
    DeleteWorkflowStep (DeleteWorkflowStep'),
    newDeleteWorkflowStep,
    DeleteWorkflowStepResponse (DeleteWorkflowStepResponse'),
    newDeleteWorkflowStepResponse,

    -- ** DeleteWorkflowStepGroup
    DeleteWorkflowStepGroup (DeleteWorkflowStepGroup'),
    newDeleteWorkflowStepGroup,
    DeleteWorkflowStepGroupResponse (DeleteWorkflowStepGroupResponse'),
    newDeleteWorkflowStepGroupResponse,

    -- ** GetTemplate
    GetTemplate (GetTemplate'),
    newGetTemplate,
    GetTemplateResponse (GetTemplateResponse'),
    newGetTemplateResponse,

    -- ** GetTemplateStep
    GetTemplateStep (GetTemplateStep'),
    newGetTemplateStep,
    GetTemplateStepResponse (GetTemplateStepResponse'),
    newGetTemplateStepResponse,

    -- ** GetTemplateStepGroup
    GetTemplateStepGroup (GetTemplateStepGroup'),
    newGetTemplateStepGroup,
    GetTemplateStepGroupResponse (GetTemplateStepGroupResponse'),
    newGetTemplateStepGroupResponse,

    -- ** GetWorkflow
    GetWorkflow (GetWorkflow'),
    newGetWorkflow,
    GetWorkflowResponse (GetWorkflowResponse'),
    newGetWorkflowResponse,

    -- ** GetWorkflowStep
    GetWorkflowStep (GetWorkflowStep'),
    newGetWorkflowStep,
    GetWorkflowStepResponse (GetWorkflowStepResponse'),
    newGetWorkflowStepResponse,

    -- ** GetWorkflowStepGroup
    GetWorkflowStepGroup (GetWorkflowStepGroup'),
    newGetWorkflowStepGroup,
    GetWorkflowStepGroupResponse (GetWorkflowStepGroupResponse'),
    newGetWorkflowStepGroupResponse,

    -- ** ListPlugins (Paginated)
    ListPlugins (ListPlugins'),
    newListPlugins,
    ListPluginsResponse (ListPluginsResponse'),
    newListPluginsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTemplateStepGroups (Paginated)
    ListTemplateStepGroups (ListTemplateStepGroups'),
    newListTemplateStepGroups,
    ListTemplateStepGroupsResponse (ListTemplateStepGroupsResponse'),
    newListTemplateStepGroupsResponse,

    -- ** ListTemplateSteps (Paginated)
    ListTemplateSteps (ListTemplateSteps'),
    newListTemplateSteps,
    ListTemplateStepsResponse (ListTemplateStepsResponse'),
    newListTemplateStepsResponse,

    -- ** ListTemplates (Paginated)
    ListTemplates (ListTemplates'),
    newListTemplates,
    ListTemplatesResponse (ListTemplatesResponse'),
    newListTemplatesResponse,

    -- ** ListWorkflowStepGroups (Paginated)
    ListWorkflowStepGroups (ListWorkflowStepGroups'),
    newListWorkflowStepGroups,
    ListWorkflowStepGroupsResponse (ListWorkflowStepGroupsResponse'),
    newListWorkflowStepGroupsResponse,

    -- ** ListWorkflowSteps (Paginated)
    ListWorkflowSteps (ListWorkflowSteps'),
    newListWorkflowSteps,
    ListWorkflowStepsResponse (ListWorkflowStepsResponse'),
    newListWorkflowStepsResponse,

    -- ** ListWorkflows (Paginated)
    ListWorkflows (ListWorkflows'),
    newListWorkflows,
    ListWorkflowsResponse (ListWorkflowsResponse'),
    newListWorkflowsResponse,

    -- ** RetryWorkflowStep
    RetryWorkflowStep (RetryWorkflowStep'),
    newRetryWorkflowStep,
    RetryWorkflowStepResponse (RetryWorkflowStepResponse'),
    newRetryWorkflowStepResponse,

    -- ** StartWorkflow
    StartWorkflow (StartWorkflow'),
    newStartWorkflow,
    StartWorkflowResponse (StartWorkflowResponse'),
    newStartWorkflowResponse,

    -- ** StopWorkflow
    StopWorkflow (StopWorkflow'),
    newStopWorkflow,
    StopWorkflowResponse (StopWorkflowResponse'),
    newStopWorkflowResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateWorkflow
    UpdateWorkflow (UpdateWorkflow'),
    newUpdateWorkflow,
    UpdateWorkflowResponse (UpdateWorkflowResponse'),
    newUpdateWorkflowResponse,

    -- ** UpdateWorkflowStep
    UpdateWorkflowStep (UpdateWorkflowStep'),
    newUpdateWorkflowStep,
    UpdateWorkflowStepResponse (UpdateWorkflowStepResponse'),
    newUpdateWorkflowStepResponse,

    -- ** UpdateWorkflowStepGroup
    UpdateWorkflowStepGroup (UpdateWorkflowStepGroup'),
    newUpdateWorkflowStepGroup,
    UpdateWorkflowStepGroupResponse (UpdateWorkflowStepGroupResponse'),
    newUpdateWorkflowStepGroupResponse,

    -- * Types

    -- ** DataType
    DataType (..),

    -- ** MigrationWorkflowStatusEnum
    MigrationWorkflowStatusEnum (..),

    -- ** Owner
    Owner (..),

    -- ** PluginHealth
    PluginHealth (..),

    -- ** RunEnvironment
    RunEnvironment (..),

    -- ** StepActionType
    StepActionType (..),

    -- ** StepGroupStatus
    StepGroupStatus (..),

    -- ** StepStatus
    StepStatus (..),

    -- ** TargetType
    TargetType (..),

    -- ** TemplateStatus
    TemplateStatus (..),

    -- ** MigrationWorkflowSummary
    MigrationWorkflowSummary (MigrationWorkflowSummary'),
    newMigrationWorkflowSummary,

    -- ** PlatformCommand
    PlatformCommand (PlatformCommand'),
    newPlatformCommand,

    -- ** PlatformScriptKey
    PlatformScriptKey (PlatformScriptKey'),
    newPlatformScriptKey,

    -- ** PluginSummary
    PluginSummary (PluginSummary'),
    newPluginSummary,

    -- ** StepAutomationConfiguration
    StepAutomationConfiguration (StepAutomationConfiguration'),
    newStepAutomationConfiguration,

    -- ** StepInput
    StepInput (StepInput'),
    newStepInput,

    -- ** StepOutput
    StepOutput (StepOutput'),
    newStepOutput,

    -- ** TemplateInput
    TemplateInput (TemplateInput'),
    newTemplateInput,

    -- ** TemplateStepGroupSummary
    TemplateStepGroupSummary (TemplateStepGroupSummary'),
    newTemplateStepGroupSummary,

    -- ** TemplateStepSummary
    TemplateStepSummary (TemplateStepSummary'),
    newTemplateStepSummary,

    -- ** TemplateSummary
    TemplateSummary (TemplateSummary'),
    newTemplateSummary,

    -- ** Tool
    Tool (Tool'),
    newTool,

    -- ** WorkflowStepAutomationConfiguration
    WorkflowStepAutomationConfiguration (WorkflowStepAutomationConfiguration'),
    newWorkflowStepAutomationConfiguration,

    -- ** WorkflowStepGroupSummary
    WorkflowStepGroupSummary (WorkflowStepGroupSummary'),
    newWorkflowStepGroupSummary,

    -- ** WorkflowStepOutput
    WorkflowStepOutput (WorkflowStepOutput'),
    newWorkflowStepOutput,

    -- ** WorkflowStepOutputUnion
    WorkflowStepOutputUnion (WorkflowStepOutputUnion'),
    newWorkflowStepOutputUnion,

    -- ** WorkflowStepSummary
    WorkflowStepSummary (WorkflowStepSummary'),
    newWorkflowStepSummary,
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
import Amazonka.MigrationHubOrchestrator.Lens
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
import Amazonka.MigrationHubOrchestrator.Types
import Amazonka.MigrationHubOrchestrator.UntagResource
import Amazonka.MigrationHubOrchestrator.UpdateWorkflow
import Amazonka.MigrationHubOrchestrator.UpdateWorkflowStep
import Amazonka.MigrationHubOrchestrator.UpdateWorkflowStepGroup
import Amazonka.MigrationHubOrchestrator.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MigrationHubOrchestrator'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
