{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS CodePipeline
--
-- __Overview__
--
-- This is the AWS CodePipeline API Reference. This guide provides
-- descriptions of the actions and data types for AWS CodePipeline. Some
-- functionality for your pipeline can only be configured through the API.
-- For more information, see the
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/welcome.html AWS CodePipeline User Guide>.
--
-- You can use the AWS CodePipeline API to work with pipelines, stages,
-- actions, and transitions.
--
-- /Pipelines/ are models of automated release processes. Each pipeline is
-- uniquely named, and consists of stages, actions, and transitions.
--
-- You can work with pipelines by calling:
--
-- -   CreatePipeline, which creates a uniquely named pipeline.
--
-- -   DeletePipeline, which deletes the specified pipeline.
--
-- -   GetPipeline, which returns information about the pipeline structure
--     and pipeline metadata, including the pipeline Amazon Resource Name
--     (ARN).
--
-- -   GetPipelineExecution, which returns information about a specific
--     execution of a pipeline.
--
-- -   GetPipelineState, which returns information about the current state
--     of the stages and actions of a pipeline.
--
-- -   ListActionExecutions, which returns action-level details for past
--     executions. The details include full stage and action-level details,
--     including individual action duration, status, any errors that
--     occurred during the execution, and input and output artifact
--     location details.
--
-- -   ListPipelines, which gets a summary of all of the pipelines
--     associated with your account.
--
-- -   ListPipelineExecutions, which gets a summary of the most recent
--     executions for a pipeline.
--
-- -   StartPipelineExecution, which runs the most recent revision of an
--     artifact through the pipeline.
--
-- -   StopPipelineExecution, which stops the specified pipeline execution
--     from continuing through the pipeline.
--
-- -   UpdatePipeline, which updates a pipeline with edits or changes to
--     the structure of the pipeline.
--
-- Pipelines include /stages/. Each stage contains one or more actions that
-- must complete before the next stage begins. A stage results in success
-- or failure. If a stage fails, the pipeline stops at that stage and
-- remains stopped until either a new version of an artifact appears in the
-- source location, or a user takes action to rerun the most recent
-- artifact through the pipeline. You can call GetPipelineState, which
-- displays the status of a pipeline, including the status of stages in the
-- pipeline, or GetPipeline, which returns the entire structure of the
-- pipeline, including the stages of that pipeline. For more information
-- about the structure of stages and actions, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/pipeline-structure.html AWS CodePipeline Pipeline Structure Reference>.
--
-- Pipeline stages include /actions/ that are categorized into categories
-- such as source or build actions performed in a stage of a pipeline. For
-- example, you can use a source action to import artifacts into a pipeline
-- from a source such as Amazon S3. Like stages, you do not work with
-- actions directly in most cases, but you do define and interact with
-- actions when working with pipeline operations such as CreatePipeline and
-- GetPipelineState. Valid action categories are:
--
-- -   Source
--
-- -   Build
--
-- -   Test
--
-- -   Deploy
--
-- -   Approval
--
-- -   Invoke
--
-- Pipelines also include /transitions/, which allow the transition of
-- artifacts from one stage to the next in a pipeline after the actions in
-- one stage complete.
--
-- You can work with transitions by calling:
--
-- -   DisableStageTransition, which prevents artifacts from transitioning
--     to the next stage in a pipeline.
--
-- -   EnableStageTransition, which enables transition of artifacts between
--     stages in a pipeline.
--
-- __Using the API to integrate with AWS CodePipeline__
--
-- For third-party integrators or developers who want to create their own
-- integrations with AWS CodePipeline, the expected sequence varies from
-- the standard API user. To integrate with AWS CodePipeline, developers
-- need to work with the following items:
--
-- __Jobs__, which are instances of an action. For example, a job for a
-- source action might import a revision of an artifact from a source.
--
-- You can work with jobs by calling:
--
-- -   AcknowledgeJob, which confirms whether a job worker has received the
--     specified job.
--
-- -   GetJobDetails, which returns the details of a job.
--
-- -   PollForJobs, which determines whether there are any jobs to act on.
--
-- -   PutJobFailureResult, which provides details of a job failure.
--
-- -   PutJobSuccessResult, which provides details of a job success.
--
-- __Third party jobs__, which are instances of an action created by a
-- partner action and integrated into AWS CodePipeline. Partner actions are
-- created by members of the AWS Partner Network.
--
-- You can work with third party jobs by calling:
--
-- -   AcknowledgeThirdPartyJob, which confirms whether a job worker has
--     received the specified job.
--
-- -   GetThirdPartyJobDetails, which requests the details of a job for a
--     partner action.
--
-- -   PollForThirdPartyJobs, which determines whether there are any jobs
--     to act on.
--
-- -   PutThirdPartyJobFailureResult, which provides details of a job
--     failure.
--
-- -   PutThirdPartyJobSuccessResult, which provides details of a job
--     success.
module Network.AWS.CodePipeline
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidWebhookAuthenticationParametersException
    _InvalidWebhookAuthenticationParametersException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** NotLatestPipelineExecutionException
    _NotLatestPipelineExecutionException,

    -- ** PipelineNameInUseException
    _PipelineNameInUseException,

    -- ** ApprovalAlreadyCompletedException
    _ApprovalAlreadyCompletedException,

    -- ** OutputVariablesSizeExceededException
    _OutputVariablesSizeExceededException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** InvalidStructureException
    _InvalidStructureException,

    -- ** InvalidBlockerDeclarationException
    _InvalidBlockerDeclarationException,

    -- ** PipelineExecutionNotStoppableException
    _PipelineExecutionNotStoppableException,

    -- ** InvalidActionDeclarationException
    _InvalidActionDeclarationException,

    -- ** InvalidWebhookFilterPatternException
    _InvalidWebhookFilterPatternException,

    -- ** DuplicatedStopRequestException
    _DuplicatedStopRequestException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ActionTypeNotFoundException
    _ActionTypeNotFoundException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidTagsException
    _InvalidTagsException,

    -- ** InvalidNonceException
    _InvalidNonceException,

    -- ** ActionTypeAlreadyExistsException
    _ActionTypeAlreadyExistsException,

    -- ** WebhookNotFoundException
    _WebhookNotFoundException,

    -- ** PipelineVersionNotFoundException
    _PipelineVersionNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- ** StageNotRetryableException
    _StageNotRetryableException,

    -- ** InvalidClientTokenException
    _InvalidClientTokenException,

    -- ** PipelineExecutionNotFoundException
    _PipelineExecutionNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** InvalidJobException
    _InvalidJobException,

    -- ** InvalidJobStateException
    _InvalidJobStateException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** PipelineNotFoundException
    _PipelineNotFoundException,

    -- ** JobNotFoundException
    _JobNotFoundException,

    -- ** StageNotFoundException
    _StageNotFoundException,

    -- ** RequestFailedException
    _RequestFailedException,

    -- ** ActionNotFoundException
    _ActionNotFoundException,

    -- ** InvalidStageDeclarationException
    _InvalidStageDeclarationException,

    -- ** InvalidApprovalTokenException
    _InvalidApprovalTokenException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListActionTypes (Paginated)
    ListActionTypes (ListActionTypes'),
    newListActionTypes,
    ListActionTypesResponse (ListActionTypesResponse'),
    newListActionTypesResponse,

    -- ** DeregisterWebhookWithThirdParty
    DeregisterWebhookWithThirdParty (DeregisterWebhookWithThirdParty'),
    newDeregisterWebhookWithThirdParty,
    DeregisterWebhookWithThirdPartyResponse (DeregisterWebhookWithThirdPartyResponse'),
    newDeregisterWebhookWithThirdPartyResponse,

    -- ** PutActionRevision
    PutActionRevision (PutActionRevision'),
    newPutActionRevision,
    PutActionRevisionResponse (PutActionRevisionResponse'),
    newPutActionRevisionResponse,

    -- ** PutJobSuccessResult
    PutJobSuccessResult (PutJobSuccessResult'),
    newPutJobSuccessResult,
    PutJobSuccessResultResponse (PutJobSuccessResultResponse'),
    newPutJobSuccessResultResponse,

    -- ** PutThirdPartyJobSuccessResult
    PutThirdPartyJobSuccessResult (PutThirdPartyJobSuccessResult'),
    newPutThirdPartyJobSuccessResult,
    PutThirdPartyJobSuccessResultResponse (PutThirdPartyJobSuccessResultResponse'),
    newPutThirdPartyJobSuccessResultResponse,

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** RetryStageExecution
    RetryStageExecution (RetryStageExecution'),
    newRetryStageExecution,
    RetryStageExecutionResponse (RetryStageExecutionResponse'),
    newRetryStageExecutionResponse,

    -- ** UpdatePipeline
    UpdatePipeline (UpdatePipeline'),
    newUpdatePipeline,
    UpdatePipelineResponse (UpdatePipelineResponse'),
    newUpdatePipelineResponse,

    -- ** GetPipelineState
    GetPipelineState (GetPipelineState'),
    newGetPipelineState,
    GetPipelineStateResponse (GetPipelineStateResponse'),
    newGetPipelineStateResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** StartPipelineExecution
    StartPipelineExecution (StartPipelineExecution'),
    newStartPipelineExecution,
    StartPipelineExecutionResponse (StartPipelineExecutionResponse'),
    newStartPipelineExecutionResponse,

    -- ** GetActionType
    GetActionType (GetActionType'),
    newGetActionType,
    GetActionTypeResponse (GetActionTypeResponse'),
    newGetActionTypeResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** StopPipelineExecution
    StopPipelineExecution (StopPipelineExecution'),
    newStopPipelineExecution,
    StopPipelineExecutionResponse (StopPipelineExecutionResponse'),
    newStopPipelineExecutionResponse,

    -- ** RegisterWebhookWithThirdParty
    RegisterWebhookWithThirdParty (RegisterWebhookWithThirdParty'),
    newRegisterWebhookWithThirdParty,
    RegisterWebhookWithThirdPartyResponse (RegisterWebhookWithThirdPartyResponse'),
    newRegisterWebhookWithThirdPartyResponse,

    -- ** ListActionExecutions (Paginated)
    ListActionExecutions (ListActionExecutions'),
    newListActionExecutions,
    ListActionExecutionsResponse (ListActionExecutionsResponse'),
    newListActionExecutionsResponse,

    -- ** PollForThirdPartyJobs
    PollForThirdPartyJobs (PollForThirdPartyJobs'),
    newPollForThirdPartyJobs,
    PollForThirdPartyJobsResponse (PollForThirdPartyJobsResponse'),
    newPollForThirdPartyJobsResponse,

    -- ** EnableStageTransition
    EnableStageTransition (EnableStageTransition'),
    newEnableStageTransition,
    EnableStageTransitionResponse (EnableStageTransitionResponse'),
    newEnableStageTransitionResponse,

    -- ** DeleteWebhook
    DeleteWebhook (DeleteWebhook'),
    newDeleteWebhook,
    DeleteWebhookResponse (DeleteWebhookResponse'),
    newDeleteWebhookResponse,

    -- ** AcknowledgeThirdPartyJob
    AcknowledgeThirdPartyJob (AcknowledgeThirdPartyJob'),
    newAcknowledgeThirdPartyJob,
    AcknowledgeThirdPartyJobResponse (AcknowledgeThirdPartyJobResponse'),
    newAcknowledgeThirdPartyJobResponse,

    -- ** AcknowledgeJob
    AcknowledgeJob (AcknowledgeJob'),
    newAcknowledgeJob,
    AcknowledgeJobResponse (AcknowledgeJobResponse'),
    newAcknowledgeJobResponse,

    -- ** DisableStageTransition
    DisableStageTransition (DisableStageTransition'),
    newDisableStageTransition,
    DisableStageTransitionResponse (DisableStageTransitionResponse'),
    newDisableStageTransitionResponse,

    -- ** UpdateActionType
    UpdateActionType (UpdateActionType'),
    newUpdateActionType,
    UpdateActionTypeResponse (UpdateActionTypeResponse'),
    newUpdateActionTypeResponse,

    -- ** PutApprovalResult
    PutApprovalResult (PutApprovalResult'),
    newPutApprovalResult,
    PutApprovalResultResponse (PutApprovalResultResponse'),
    newPutApprovalResultResponse,

    -- ** PutJobFailureResult
    PutJobFailureResult (PutJobFailureResult'),
    newPutJobFailureResult,
    PutJobFailureResultResponse (PutJobFailureResultResponse'),
    newPutJobFailureResultResponse,

    -- ** DeleteCustomActionType
    DeleteCustomActionType (DeleteCustomActionType'),
    newDeleteCustomActionType,
    DeleteCustomActionTypeResponse (DeleteCustomActionTypeResponse'),
    newDeleteCustomActionTypeResponse,

    -- ** GetPipeline
    GetPipeline (GetPipeline'),
    newGetPipeline,
    GetPipelineResponse (GetPipelineResponse'),
    newGetPipelineResponse,

    -- ** CreateCustomActionType
    CreateCustomActionType (CreateCustomActionType'),
    newCreateCustomActionType,
    CreateCustomActionTypeResponse (CreateCustomActionTypeResponse'),
    newCreateCustomActionTypeResponse,

    -- ** ListPipelineExecutions (Paginated)
    ListPipelineExecutions (ListPipelineExecutions'),
    newListPipelineExecutions,
    ListPipelineExecutionsResponse (ListPipelineExecutionsResponse'),
    newListPipelineExecutionsResponse,

    -- ** GetThirdPartyJobDetails
    GetThirdPartyJobDetails (GetThirdPartyJobDetails'),
    newGetThirdPartyJobDetails,
    GetThirdPartyJobDetailsResponse (GetThirdPartyJobDetailsResponse'),
    newGetThirdPartyJobDetailsResponse,

    -- ** GetPipelineExecution
    GetPipelineExecution (GetPipelineExecution'),
    newGetPipelineExecution,
    GetPipelineExecutionResponse (GetPipelineExecutionResponse'),
    newGetPipelineExecutionResponse,

    -- ** GetJobDetails
    GetJobDetails (GetJobDetails'),
    newGetJobDetails,
    GetJobDetailsResponse (GetJobDetailsResponse'),
    newGetJobDetailsResponse,

    -- ** ListPipelines (Paginated)
    ListPipelines (ListPipelines'),
    newListPipelines,
    ListPipelinesResponse (ListPipelinesResponse'),
    newListPipelinesResponse,

    -- ** PollForJobs
    PollForJobs (PollForJobs'),
    newPollForJobs,
    PollForJobsResponse (PollForJobsResponse'),
    newPollForJobsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutWebhook
    PutWebhook (PutWebhook'),
    newPutWebhook,
    PutWebhookResponse (PutWebhookResponse'),
    newPutWebhookResponse,

    -- ** PutThirdPartyJobFailureResult
    PutThirdPartyJobFailureResult (PutThirdPartyJobFailureResult'),
    newPutThirdPartyJobFailureResult,
    PutThirdPartyJobFailureResultResponse (PutThirdPartyJobFailureResultResponse'),
    newPutThirdPartyJobFailureResultResponse,

    -- ** ListWebhooks (Paginated)
    ListWebhooks (ListWebhooks'),
    newListWebhooks,
    ListWebhooksResponse (ListWebhooksResponse'),
    newListWebhooksResponse,

    -- * Types

    -- ** ActionCategory
    ActionCategory (..),

    -- ** ActionConfigurationPropertyType
    ActionConfigurationPropertyType (..),

    -- ** ActionExecutionStatus
    ActionExecutionStatus (..),

    -- ** ActionOwner
    ActionOwner (..),

    -- ** ApprovalStatus
    ApprovalStatus (..),

    -- ** ArtifactLocationType
    ArtifactLocationType (..),

    -- ** ArtifactStoreType
    ArtifactStoreType (..),

    -- ** BlockerType
    BlockerType (..),

    -- ** EncryptionKeyType
    EncryptionKeyType (..),

    -- ** ExecutorType
    ExecutorType (..),

    -- ** FailureType
    FailureType (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** PipelineExecutionStatus
    PipelineExecutionStatus (..),

    -- ** StageExecutionStatus
    StageExecutionStatus (..),

    -- ** StageRetryMode
    StageRetryMode (..),

    -- ** StageTransitionType
    StageTransitionType (..),

    -- ** TriggerType
    TriggerType (..),

    -- ** WebhookAuthenticationType
    WebhookAuthenticationType (..),

    -- ** AWSSessionCredentials
    AWSSessionCredentials (AWSSessionCredentials'),
    newAWSSessionCredentials,

    -- ** ActionConfiguration
    ActionConfiguration (ActionConfiguration'),
    newActionConfiguration,

    -- ** ActionConfigurationProperty
    ActionConfigurationProperty (ActionConfigurationProperty'),
    newActionConfigurationProperty,

    -- ** ActionContext
    ActionContext (ActionContext'),
    newActionContext,

    -- ** ActionDeclaration
    ActionDeclaration (ActionDeclaration'),
    newActionDeclaration,

    -- ** ActionExecution
    ActionExecution (ActionExecution'),
    newActionExecution,

    -- ** ActionExecutionDetail
    ActionExecutionDetail (ActionExecutionDetail'),
    newActionExecutionDetail,

    -- ** ActionExecutionFilter
    ActionExecutionFilter (ActionExecutionFilter'),
    newActionExecutionFilter,

    -- ** ActionExecutionInput
    ActionExecutionInput (ActionExecutionInput'),
    newActionExecutionInput,

    -- ** ActionExecutionOutput
    ActionExecutionOutput (ActionExecutionOutput'),
    newActionExecutionOutput,

    -- ** ActionExecutionResult
    ActionExecutionResult (ActionExecutionResult'),
    newActionExecutionResult,

    -- ** ActionRevision
    ActionRevision (ActionRevision'),
    newActionRevision,

    -- ** ActionState
    ActionState (ActionState'),
    newActionState,

    -- ** ActionType
    ActionType (ActionType'),
    newActionType,

    -- ** ActionTypeArtifactDetails
    ActionTypeArtifactDetails (ActionTypeArtifactDetails'),
    newActionTypeArtifactDetails,

    -- ** ActionTypeDeclaration
    ActionTypeDeclaration (ActionTypeDeclaration'),
    newActionTypeDeclaration,

    -- ** ActionTypeExecutor
    ActionTypeExecutor (ActionTypeExecutor'),
    newActionTypeExecutor,

    -- ** ActionTypeId
    ActionTypeId (ActionTypeId'),
    newActionTypeId,

    -- ** ActionTypeIdentifier
    ActionTypeIdentifier (ActionTypeIdentifier'),
    newActionTypeIdentifier,

    -- ** ActionTypePermissions
    ActionTypePermissions (ActionTypePermissions'),
    newActionTypePermissions,

    -- ** ActionTypeProperty
    ActionTypeProperty (ActionTypeProperty'),
    newActionTypeProperty,

    -- ** ActionTypeSettings
    ActionTypeSettings (ActionTypeSettings'),
    newActionTypeSettings,

    -- ** ActionTypeUrls
    ActionTypeUrls (ActionTypeUrls'),
    newActionTypeUrls,

    -- ** ApprovalResult
    ApprovalResult (ApprovalResult'),
    newApprovalResult,

    -- ** Artifact
    Artifact (Artifact'),
    newArtifact,

    -- ** ArtifactDetail
    ArtifactDetail (ArtifactDetail'),
    newArtifactDetail,

    -- ** ArtifactDetails
    ArtifactDetails (ArtifactDetails'),
    newArtifactDetails,

    -- ** ArtifactLocation
    ArtifactLocation (ArtifactLocation'),
    newArtifactLocation,

    -- ** ArtifactRevision
    ArtifactRevision (ArtifactRevision'),
    newArtifactRevision,

    -- ** ArtifactStore
    ArtifactStore (ArtifactStore'),
    newArtifactStore,

    -- ** BlockerDeclaration
    BlockerDeclaration (BlockerDeclaration'),
    newBlockerDeclaration,

    -- ** CurrentRevision
    CurrentRevision (CurrentRevision'),
    newCurrentRevision,

    -- ** EncryptionKey
    EncryptionKey (EncryptionKey'),
    newEncryptionKey,

    -- ** ErrorDetails
    ErrorDetails (ErrorDetails'),
    newErrorDetails,

    -- ** ExecutionDetails
    ExecutionDetails (ExecutionDetails'),
    newExecutionDetails,

    -- ** ExecutionTrigger
    ExecutionTrigger (ExecutionTrigger'),
    newExecutionTrigger,

    -- ** ExecutorConfiguration
    ExecutorConfiguration (ExecutorConfiguration'),
    newExecutorConfiguration,

    -- ** FailureDetails
    FailureDetails (FailureDetails'),
    newFailureDetails,

    -- ** InputArtifact
    InputArtifact (InputArtifact'),
    newInputArtifact,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** JobData
    JobData (JobData'),
    newJobData,

    -- ** JobDetails
    JobDetails (JobDetails'),
    newJobDetails,

    -- ** JobWorkerExecutorConfiguration
    JobWorkerExecutorConfiguration (JobWorkerExecutorConfiguration'),
    newJobWorkerExecutorConfiguration,

    -- ** LambdaExecutorConfiguration
    LambdaExecutorConfiguration (LambdaExecutorConfiguration'),
    newLambdaExecutorConfiguration,

    -- ** ListWebhookItem
    ListWebhookItem (ListWebhookItem'),
    newListWebhookItem,

    -- ** OutputArtifact
    OutputArtifact (OutputArtifact'),
    newOutputArtifact,

    -- ** PipelineContext
    PipelineContext (PipelineContext'),
    newPipelineContext,

    -- ** PipelineDeclaration
    PipelineDeclaration (PipelineDeclaration'),
    newPipelineDeclaration,

    -- ** PipelineExecution
    PipelineExecution (PipelineExecution'),
    newPipelineExecution,

    -- ** PipelineExecutionSummary
    PipelineExecutionSummary (PipelineExecutionSummary'),
    newPipelineExecutionSummary,

    -- ** PipelineMetadata
    PipelineMetadata (PipelineMetadata'),
    newPipelineMetadata,

    -- ** PipelineSummary
    PipelineSummary (PipelineSummary'),
    newPipelineSummary,

    -- ** S3ArtifactLocation
    S3ArtifactLocation (S3ArtifactLocation'),
    newS3ArtifactLocation,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** SourceRevision
    SourceRevision (SourceRevision'),
    newSourceRevision,

    -- ** StageContext
    StageContext (StageContext'),
    newStageContext,

    -- ** StageDeclaration
    StageDeclaration (StageDeclaration'),
    newStageDeclaration,

    -- ** StageExecution
    StageExecution (StageExecution'),
    newStageExecution,

    -- ** StageState
    StageState (StageState'),
    newStageState,

    -- ** StopExecutionTrigger
    StopExecutionTrigger (StopExecutionTrigger'),
    newStopExecutionTrigger,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** ThirdPartyJob
    ThirdPartyJob (ThirdPartyJob'),
    newThirdPartyJob,

    -- ** ThirdPartyJobData
    ThirdPartyJobData (ThirdPartyJobData'),
    newThirdPartyJobData,

    -- ** ThirdPartyJobDetails
    ThirdPartyJobDetails (ThirdPartyJobDetails'),
    newThirdPartyJobDetails,

    -- ** TransitionState
    TransitionState (TransitionState'),
    newTransitionState,

    -- ** WebhookAuthConfiguration
    WebhookAuthConfiguration (WebhookAuthConfiguration'),
    newWebhookAuthConfiguration,

    -- ** WebhookDefinition
    WebhookDefinition (WebhookDefinition'),
    newWebhookDefinition,

    -- ** WebhookFilterRule
    WebhookFilterRule (WebhookFilterRule'),
    newWebhookFilterRule,
  )
where

import Network.AWS.CodePipeline.AcknowledgeJob
import Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
import Network.AWS.CodePipeline.CreateCustomActionType
import Network.AWS.CodePipeline.CreatePipeline
import Network.AWS.CodePipeline.DeleteCustomActionType
import Network.AWS.CodePipeline.DeletePipeline
import Network.AWS.CodePipeline.DeleteWebhook
import Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty
import Network.AWS.CodePipeline.DisableStageTransition
import Network.AWS.CodePipeline.EnableStageTransition
import Network.AWS.CodePipeline.GetActionType
import Network.AWS.CodePipeline.GetJobDetails
import Network.AWS.CodePipeline.GetPipeline
import Network.AWS.CodePipeline.GetPipelineExecution
import Network.AWS.CodePipeline.GetPipelineState
import Network.AWS.CodePipeline.GetThirdPartyJobDetails
import Network.AWS.CodePipeline.Lens
import Network.AWS.CodePipeline.ListActionExecutions
import Network.AWS.CodePipeline.ListActionTypes
import Network.AWS.CodePipeline.ListPipelineExecutions
import Network.AWS.CodePipeline.ListPipelines
import Network.AWS.CodePipeline.ListTagsForResource
import Network.AWS.CodePipeline.ListWebhooks
import Network.AWS.CodePipeline.PollForJobs
import Network.AWS.CodePipeline.PollForThirdPartyJobs
import Network.AWS.CodePipeline.PutActionRevision
import Network.AWS.CodePipeline.PutApprovalResult
import Network.AWS.CodePipeline.PutJobFailureResult
import Network.AWS.CodePipeline.PutJobSuccessResult
import Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
import Network.AWS.CodePipeline.PutThirdPartyJobSuccessResult
import Network.AWS.CodePipeline.PutWebhook
import Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
import Network.AWS.CodePipeline.RetryStageExecution
import Network.AWS.CodePipeline.StartPipelineExecution
import Network.AWS.CodePipeline.StopPipelineExecution
import Network.AWS.CodePipeline.TagResource
import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.UntagResource
import Network.AWS.CodePipeline.UpdateActionType
import Network.AWS.CodePipeline.UpdatePipeline
import Network.AWS.CodePipeline.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodePipeline'.

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
