{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodePipeline__
--
-- __Overview__
-- This is the AWS CodePipeline API Reference. This guide provides descriptions of the actions and data types for AWS CodePipeline. Some functionality for your pipeline can only be configured through the API. For more information, see the <https://docs.aws.amazon.com/codepipeline/latest/userguide/welcome.html AWS CodePipeline User Guide> .
-- You can use the AWS CodePipeline API to work with pipelines, stages, actions, and transitions.
-- /Pipelines/ are models of automated release processes. Each pipeline is uniquely named, and consists of stages, actions, and transitions.
-- You can work with pipelines by calling:
--
--     * 'CreatePipeline' , which creates a uniquely named pipeline.
--
--
--     * 'DeletePipeline' , which deletes the specified pipeline.
--
--
--     * 'GetPipeline' , which returns information about the pipeline structure and pipeline metadata, including the pipeline Amazon Resource Name (ARN).
--
--
--     * 'GetPipelineExecution' , which returns information about a specific execution of a pipeline.
--
--
--     * 'GetPipelineState' , which returns information about the current state of the stages and actions of a pipeline.
--
--
--     * 'ListActionExecutions' , which returns action-level details for past executions. The details include full stage and action-level details, including individual action duration, status, any errors that occurred during the execution, and input and output artifact location details.
--
--
--     * 'ListPipelines' , which gets a summary of all of the pipelines associated with your account.
--
--
--     * 'ListPipelineExecutions' , which gets a summary of the most recent executions for a pipeline.
--
--
--     * 'StartPipelineExecution' , which runs the most recent revision of an artifact through the pipeline.
--
--
--     * 'StopPipelineExecution' , which stops the specified pipeline execution from continuing through the pipeline.
--
--
--     * 'UpdatePipeline' , which updates a pipeline with edits or changes to the structure of the pipeline.
--
--
-- Pipelines include /stages/ . Each stage contains one or more actions that must complete before the next stage begins. A stage results in success or failure. If a stage fails, the pipeline stops at that stage and remains stopped until either a new version of an artifact appears in the source location, or a user takes action to rerun the most recent artifact through the pipeline. You can call 'GetPipelineState' , which displays the status of a pipeline, including the status of stages in the pipeline, or 'GetPipeline' , which returns the entire structure of the pipeline, including the stages of that pipeline. For more information about the structure of stages and actions, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/pipeline-structure.html AWS CodePipeline Pipeline Structure Reference> .
-- Pipeline stages include /actions/ that are categorized into categories such as source or build actions performed in a stage of a pipeline. For example, you can use a source action to import artifacts into a pipeline from a source such as Amazon S3. Like stages, you do not work with actions directly in most cases, but you do define and interact with actions when working with pipeline operations such as 'CreatePipeline' and 'GetPipelineState' . Valid action categories are:
--
--     * Source
--
--
--     * Build
--
--
--     * Test
--
--
--     * Deploy
--
--
--     * Approval
--
--
--     * Invoke
--
--
-- Pipelines also include /transitions/ , which allow the transition of artifacts from one stage to the next in a pipeline after the actions in one stage complete.
-- You can work with transitions by calling:
--
--     * 'DisableStageTransition' , which prevents artifacts from transitioning to the next stage in a pipeline.
--
--
--     * 'EnableStageTransition' , which enables transition of artifacts between stages in a pipeline.
--
--
-- __Using the API to integrate with AWS CodePipeline__
-- For third-party integrators or developers who want to create their own integrations with AWS CodePipeline, the expected sequence varies from the standard API user. To integrate with AWS CodePipeline, developers need to work with the following items:
-- __Jobs__ , which are instances of an action. For example, a job for a source action might import a revision of an artifact from a source.
-- You can work with jobs by calling:
--
--     * 'AcknowledgeJob' , which confirms whether a job worker has received the specified job.
--
--
--     * 'GetJobDetails' , which returns the details of a job.
--
--
--     * 'PollForJobs' , which determines whether there are any jobs to act on.
--
--
--     * 'PutJobFailureResult' , which provides details of a job failure.
--
--
--     * 'PutJobSuccessResult' , which provides details of a job success.
--
--
-- __Third party jobs__ , which are instances of an action created by a partner action and integrated into AWS CodePipeline. Partner actions are created by members of the AWS Partner Network.
-- You can work with third party jobs by calling:
--
--     * 'AcknowledgeThirdPartyJob' , which confirms whether a job worker has received the specified job.
--
--
--     * 'GetThirdPartyJobDetails' , which requests the details of a job for a partner action.
--
--
--     * 'PollForThirdPartyJobs' , which determines whether there are any jobs to act on.
--
--
--     * 'PutThirdPartyJobFailureResult' , which provides details of a job failure.
--
--
--     * 'PutThirdPartyJobSuccessResult' , which provides details of a job success.
module Network.AWS.CodePipeline
  ( -- * Service configuration
    codePipelineService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetPipeline
    module Network.AWS.CodePipeline.GetPipeline,

    -- ** PutJobFailureResult
    module Network.AWS.CodePipeline.PutJobFailureResult,

    -- ** PutApprovalResult
    module Network.AWS.CodePipeline.PutApprovalResult,

    -- ** AcknowledgeThirdPartyJob
    module Network.AWS.CodePipeline.AcknowledgeThirdPartyJob,

    -- ** PutThirdPartyJobFailureResult
    module Network.AWS.CodePipeline.PutThirdPartyJobFailureResult,

    -- ** ListTagsForResource (Paginated)
    module Network.AWS.CodePipeline.ListTagsForResource,

    -- ** RegisterWebhookWithThirdParty
    module Network.AWS.CodePipeline.RegisterWebhookWithThirdParty,

    -- ** PollForThirdPartyJobs
    module Network.AWS.CodePipeline.PollForThirdPartyJobs,

    -- ** PollForJobs
    module Network.AWS.CodePipeline.PollForJobs,

    -- ** StartPipelineExecution
    module Network.AWS.CodePipeline.StartPipelineExecution,

    -- ** UpdatePipeline
    module Network.AWS.CodePipeline.UpdatePipeline,

    -- ** DeletePipeline
    module Network.AWS.CodePipeline.DeletePipeline,

    -- ** GetPipelineState
    module Network.AWS.CodePipeline.GetPipelineState,

    -- ** GetJobDetails
    module Network.AWS.CodePipeline.GetJobDetails,

    -- ** ListPipelines (Paginated)
    module Network.AWS.CodePipeline.ListPipelines,

    -- ** RetryStageExecution
    module Network.AWS.CodePipeline.RetryStageExecution,

    -- ** GetPipelineExecution
    module Network.AWS.CodePipeline.GetPipelineExecution,

    -- ** PutJobSuccessResult
    module Network.AWS.CodePipeline.PutJobSuccessResult,

    -- ** DeregisterWebhookWithThirdParty
    module Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty,

    -- ** DeleteCustomActionType
    module Network.AWS.CodePipeline.DeleteCustomActionType,

    -- ** PutActionRevision
    module Network.AWS.CodePipeline.PutActionRevision,

    -- ** DisableStageTransition
    module Network.AWS.CodePipeline.DisableStageTransition,

    -- ** ListActionTypes (Paginated)
    module Network.AWS.CodePipeline.ListActionTypes,

    -- ** AcknowledgeJob
    module Network.AWS.CodePipeline.AcknowledgeJob,

    -- ** EnableStageTransition
    module Network.AWS.CodePipeline.EnableStageTransition,

    -- ** DeleteWebhook
    module Network.AWS.CodePipeline.DeleteWebhook,

    -- ** PutWebhook
    module Network.AWS.CodePipeline.PutWebhook,

    -- ** ListWebhooks (Paginated)
    module Network.AWS.CodePipeline.ListWebhooks,

    -- ** ListActionExecutions (Paginated)
    module Network.AWS.CodePipeline.ListActionExecutions,

    -- ** StopPipelineExecution
    module Network.AWS.CodePipeline.StopPipelineExecution,

    -- ** TagResource
    module Network.AWS.CodePipeline.TagResource,

    -- ** UntagResource
    module Network.AWS.CodePipeline.UntagResource,

    -- ** CreatePipeline
    module Network.AWS.CodePipeline.CreatePipeline,

    -- ** GetThirdPartyJobDetails
    module Network.AWS.CodePipeline.GetThirdPartyJobDetails,

    -- ** PutThirdPartyJobSuccessResult
    module Network.AWS.CodePipeline.PutThirdPartyJobSuccessResult,

    -- ** CreateCustomActionType
    module Network.AWS.CodePipeline.CreateCustomActionType,

    -- ** ListPipelineExecutions (Paginated)
    module Network.AWS.CodePipeline.ListPipelineExecutions,

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
    AWSSessionCredentials (..),
    mkAWSSessionCredentials,
    ascSecretAccessKey,
    ascSessionToken,
    ascAccessKeyId,

    -- ** ActionConfiguration
    ActionConfiguration (..),
    mkActionConfiguration,
    acConfiguration,

    -- ** ActionConfigurationProperty
    ActionConfigurationProperty (..),
    mkActionConfigurationProperty,
    acpQueryable,
    acpSecret,
    acpRequired,
    acpKey,
    acpName,
    acpType,
    acpDescription,

    -- ** ActionContext
    ActionContext (..),
    mkActionContext,
    acName,
    acActionExecutionId,

    -- ** ActionDeclaration
    ActionDeclaration (..),
    mkActionDeclaration,
    adOutputArtifacts,
    adNamespace,
    adRunOrder,
    adName,
    adRegion,
    adConfiguration,
    adActionTypeId,
    adInputArtifacts,
    adRoleARN,

    -- ** ActionExecution
    ActionExecution (..),
    mkActionExecution,
    aeLastUpdatedBy,
    aeSummary,
    aeStatus,
    aeLastStatusChange,
    aeToken,
    aeExternalExecutionURL,
    aeExternalExecutionId,
    aeErrorDetails,
    aePercentComplete,
    aeActionExecutionId,

    -- ** ActionExecutionDetail
    ActionExecutionDetail (..),
    mkActionExecutionDetail,
    aedStatus,
    aedStartTime,
    aedPipelineVersion,
    aedInput,
    aedActionName,
    aedOutput,
    aedPipelineExecutionId,
    aedStageName,
    aedLastUpdateTime,
    aedActionExecutionId,

    -- ** ActionExecutionFilter
    ActionExecutionFilter (..),
    mkActionExecutionFilter,
    aefPipelineExecutionId,

    -- ** ActionExecutionInput
    ActionExecutionInput (..),
    mkActionExecutionInput,
    aeiNamespace,
    aeiResolvedConfiguration,
    aeiRegion,
    aeiConfiguration,
    aeiActionTypeId,
    aeiInputArtifacts,
    aeiRoleARN,

    -- ** ActionExecutionOutput
    ActionExecutionOutput (..),
    mkActionExecutionOutput,
    aeoOutputVariables,
    aeoOutputArtifacts,
    aeoExecutionResult,

    -- ** ActionExecutionResult
    ActionExecutionResult (..),
    mkActionExecutionResult,
    aerExternalExecutionURL,
    aerExternalExecutionId,
    aerExternalExecutionSummary,

    -- ** ActionRevision
    ActionRevision (..),
    mkActionRevision,
    aCreated,
    aRevisionChangeId,
    aRevisionId,

    -- ** ActionState
    ActionState (..),
    mkActionState,
    asRevisionURL,
    asEntityURL,
    asActionName,
    asCurrentRevision,
    asLatestExecution,

    -- ** ActionType
    ActionType (..),
    mkActionType,
    atSettings,
    atOutputArtifactDetails,
    atActionConfigurationProperties,
    atInputArtifactDetails,
    atId,

    -- ** ActionTypeId
    ActionTypeId (..),
    mkActionTypeId,
    atiCategory,
    atiOwner,
    atiVersion,
    atiProvider,

    -- ** ActionTypeSettings
    ActionTypeSettings (..),
    mkActionTypeSettings,
    atsThirdPartyConfigurationURL,
    atsExecutionURLTemplate,
    atsRevisionURLTemplate,
    atsEntityURLTemplate,

    -- ** ApprovalResult
    ApprovalResult (..),
    mkApprovalResult,
    arSummary,
    arStatus,

    -- ** Artifact
    Artifact (..),
    mkArtifact,
    afLocation,
    afName,
    afRevision,

    -- ** ArtifactDetail
    ArtifactDetail (..),
    mkArtifactDetail,
    aName,
    aS3location,

    -- ** ArtifactDetails
    ArtifactDetails (..),
    mkArtifactDetails,
    adMaximumCount,
    adMinimumCount,

    -- ** ArtifactLocation
    ArtifactLocation (..),
    mkArtifactLocation,
    alS3Location,
    alType,

    -- ** ArtifactRevision
    ArtifactRevision (..),
    mkArtifactRevision,
    arRevisionSummary,
    arRevisionURL,
    arCreated,
    arName,
    arRevisionId,
    arRevisionChangeIdentifier,

    -- ** ArtifactStore
    ArtifactStore (..),
    mkArtifactStore,
    asLocation,
    asEncryptionKey,
    asType,

    -- ** BlockerDeclaration
    BlockerDeclaration (..),
    mkBlockerDeclaration,
    bdName,
    bdType,

    -- ** CurrentRevision
    CurrentRevision (..),
    mkCurrentRevision,
    crRevisionSummary,
    crCreated,
    crChangeIdentifier,
    crRevision,

    -- ** EncryptionKey
    EncryptionKey (..),
    mkEncryptionKey,
    ekId,
    ekType,

    -- ** ErrorDetails
    ErrorDetails (..),
    mkErrorDetails,
    edCode,
    edMessage,

    -- ** ExecutionDetails
    ExecutionDetails (..),
    mkExecutionDetails,
    edSummary,
    edExternalExecutionId,
    edPercentComplete,

    -- ** ExecutionTrigger
    ExecutionTrigger (..),
    mkExecutionTrigger,
    etTriggerType,
    etTriggerDetail,

    -- ** FailureDetails
    FailureDetails (..),
    mkFailureDetails,
    fdExternalExecutionId,
    fdType,
    fdMessage,

    -- ** InputArtifact
    InputArtifact (..),
    mkInputArtifact,
    iaName,

    -- ** Job
    Job (..),
    mkJob,
    jData,
    jAccountId,
    jId,
    jNonce,

    -- ** JobData
    JobData (..),
    mkJobData,
    jdContinuationToken,
    jdOutputArtifacts,
    jdArtifactCredentials,
    jdPipelineContext,
    jdEncryptionKey,
    jdActionTypeId,
    jdInputArtifacts,
    jdActionConfiguration,

    -- ** JobDetails
    JobDetails (..),
    mkJobDetails,
    jdData,
    jdAccountId,
    jdId,

    -- ** ListWebhookItem
    ListWebhookItem (..),
    mkListWebhookItem,
    lwiArn,
    lwiDefinition,
    lwiUrl,
    lwiErrorCode,
    lwiLastTriggered,
    lwiErrorMessage,
    lwiTags,

    -- ** OutputArtifact
    OutputArtifact (..),
    mkOutputArtifact,
    oaName,

    -- ** PipelineContext
    PipelineContext (..),
    mkPipelineContext,
    pcStage,
    pcPipelineName,
    pcAction,
    pcPipelineARN,
    pcPipelineExecutionId,

    -- ** PipelineDeclaration
    PipelineDeclaration (..),
    mkPipelineDeclaration,
    pdArtifactStores,
    pdArtifactStore,
    pdName,
    pdStages,
    pdVersion,
    pdRoleARN,

    -- ** PipelineExecution
    PipelineExecution (..),
    mkPipelineExecution,
    peStatus,
    pePipelineName,
    pePipelineVersion,
    pePipelineExecutionId,
    peArtifactRevisions,

    -- ** PipelineExecutionSummary
    PipelineExecutionSummary (..),
    mkPipelineExecutionSummary,
    pesStatus,
    pesStartTime,
    pesStopTrigger,
    pesPipelineExecutionId,
    pesSourceRevisions,
    pesTrigger,
    pesLastUpdateTime,

    -- ** PipelineMetadata
    PipelineMetadata (..),
    mkPipelineMetadata,
    pmCreated,
    pmPipelineARN,
    pmUpdated,

    -- ** PipelineSummary
    PipelineSummary (..),
    mkPipelineSummary,
    psCreated,
    psName,
    psVersion,
    psUpdated,

    -- ** S3ArtifactLocation
    S3ArtifactLocation (..),
    mkS3ArtifactLocation,
    salBucketName,
    salObjectKey,

    -- ** S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,

    -- ** SourceRevision
    SourceRevision (..),
    mkSourceRevision,
    srRevisionSummary,
    srRevisionURL,
    srActionName,
    srRevisionId,

    -- ** StageContext
    StageContext (..),
    mkStageContext,
    scName,

    -- ** StageDeclaration
    StageDeclaration (..),
    mkStageDeclaration,
    sdActions,
    sdBlockers,
    sdName,

    -- ** StageExecution
    StageExecution (..),
    mkStageExecution,
    seStatus,
    sePipelineExecutionId,

    -- ** StageState
    StageState (..),
    mkStageState,
    ssInboundExecution,
    ssInboundTransitionState,
    ssActionStates,
    ssStageName,
    ssLatestExecution,

    -- ** StopExecutionTrigger
    StopExecutionTrigger (..),
    mkStopExecutionTrigger,
    setReason,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** ThirdPartyJob
    ThirdPartyJob (..),
    mkThirdPartyJob,
    tpjClientId,
    tpjJobId,

    -- ** ThirdPartyJobData
    ThirdPartyJobData (..),
    mkThirdPartyJobData,
    tpjdContinuationToken,
    tpjdOutputArtifacts,
    tpjdArtifactCredentials,
    tpjdPipelineContext,
    tpjdEncryptionKey,
    tpjdActionTypeId,
    tpjdInputArtifacts,
    tpjdActionConfiguration,

    -- ** ThirdPartyJobDetails
    ThirdPartyJobDetails (..),
    mkThirdPartyJobDetails,
    tpjdData,
    tpjdId,
    tpjdNonce,

    -- ** TransitionState
    TransitionState (..),
    mkTransitionState,
    tsEnabled,
    tsDisabledReason,
    tsLastChangedAt,
    tsLastChangedBy,

    -- ** WebhookAuthConfiguration
    WebhookAuthConfiguration (..),
    mkWebhookAuthConfiguration,
    wacAllowedIPRange,
    wacSecretToken,

    -- ** WebhookDefinition
    WebhookDefinition (..),
    mkWebhookDefinition,
    wdTargetAction,
    wdAuthentication,
    wdFilters,
    wdTargetPipeline,
    wdName,
    wdAuthenticationConfiguration,

    -- ** WebhookFilterRule
    WebhookFilterRule (..),
    mkWebhookFilterRule,
    wfrMatchEquals,
    wfrJsonPath,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import Network.AWS.CodePipeline.GetJobDetails
import Network.AWS.CodePipeline.GetPipeline
import Network.AWS.CodePipeline.GetPipelineExecution
import Network.AWS.CodePipeline.GetPipelineState
import Network.AWS.CodePipeline.GetThirdPartyJobDetails
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
import Network.AWS.CodePipeline.UpdatePipeline
import Network.AWS.CodePipeline.Waiters
import qualified Network.AWS.Prelude as Lude

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
