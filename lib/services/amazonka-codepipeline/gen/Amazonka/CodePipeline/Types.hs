{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodePipeline.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ActionNotFoundException,
    _ActionTypeAlreadyExistsException,
    _ActionTypeNotFoundException,
    _ApprovalAlreadyCompletedException,
    _ConcurrentModificationException,
    _ConflictException,
    _DuplicatedStopRequestException,
    _InvalidActionDeclarationException,
    _InvalidApprovalTokenException,
    _InvalidArnException,
    _InvalidBlockerDeclarationException,
    _InvalidClientTokenException,
    _InvalidJobException,
    _InvalidJobStateException,
    _InvalidNextTokenException,
    _InvalidNonceException,
    _InvalidStageDeclarationException,
    _InvalidStructureException,
    _InvalidTagsException,
    _InvalidWebhookAuthenticationParametersException,
    _InvalidWebhookFilterPatternException,
    _JobNotFoundException,
    _LimitExceededException,
    _NotLatestPipelineExecutionException,
    _OutputVariablesSizeExceededException,
    _PipelineExecutionNotFoundException,
    _PipelineExecutionNotStoppableException,
    _PipelineNameInUseException,
    _PipelineNotFoundException,
    _PipelineVersionNotFoundException,
    _RequestFailedException,
    _ResourceNotFoundException,
    _StageNotFoundException,
    _StageNotRetryableException,
    _TooManyTagsException,
    _ValidationException,
    _WebhookNotFoundException,

    -- * ActionCategory
    ActionCategory (..),

    -- * ActionConfigurationPropertyType
    ActionConfigurationPropertyType (..),

    -- * ActionExecutionStatus
    ActionExecutionStatus (..),

    -- * ActionOwner
    ActionOwner (..),

    -- * ApprovalStatus
    ApprovalStatus (..),

    -- * ArtifactLocationType
    ArtifactLocationType (..),

    -- * ArtifactStoreType
    ArtifactStoreType (..),

    -- * BlockerType
    BlockerType (..),

    -- * EncryptionKeyType
    EncryptionKeyType (..),

    -- * ExecutorType
    ExecutorType (..),

    -- * FailureType
    FailureType (..),

    -- * JobStatus
    JobStatus (..),

    -- * PipelineExecutionStatus
    PipelineExecutionStatus (..),

    -- * StageExecutionStatus
    StageExecutionStatus (..),

    -- * StageRetryMode
    StageRetryMode (..),

    -- * StageTransitionType
    StageTransitionType (..),

    -- * TriggerType
    TriggerType (..),

    -- * WebhookAuthenticationType
    WebhookAuthenticationType (..),

    -- * AWSSessionCredentials
    AWSSessionCredentials (..),
    newAWSSessionCredentials,
    aWSSessionCredentials_accessKeyId,
    aWSSessionCredentials_secretAccessKey,
    aWSSessionCredentials_sessionToken,

    -- * ActionConfiguration
    ActionConfiguration (..),
    newActionConfiguration,
    actionConfiguration_configuration,

    -- * ActionConfigurationProperty
    ActionConfigurationProperty (..),
    newActionConfigurationProperty,
    actionConfigurationProperty_description,
    actionConfigurationProperty_queryable,
    actionConfigurationProperty_type,
    actionConfigurationProperty_name,
    actionConfigurationProperty_required,
    actionConfigurationProperty_key,
    actionConfigurationProperty_secret,

    -- * ActionContext
    ActionContext (..),
    newActionContext,
    actionContext_actionExecutionId,
    actionContext_name,

    -- * ActionDeclaration
    ActionDeclaration (..),
    newActionDeclaration,
    actionDeclaration_configuration,
    actionDeclaration_inputArtifacts,
    actionDeclaration_namespace,
    actionDeclaration_outputArtifacts,
    actionDeclaration_region,
    actionDeclaration_roleArn,
    actionDeclaration_runOrder,
    actionDeclaration_name,
    actionDeclaration_actionTypeId,

    -- * ActionExecution
    ActionExecution (..),
    newActionExecution,
    actionExecution_actionExecutionId,
    actionExecution_errorDetails,
    actionExecution_externalExecutionId,
    actionExecution_externalExecutionUrl,
    actionExecution_lastStatusChange,
    actionExecution_lastUpdatedBy,
    actionExecution_percentComplete,
    actionExecution_status,
    actionExecution_summary,
    actionExecution_token,

    -- * ActionExecutionDetail
    ActionExecutionDetail (..),
    newActionExecutionDetail,
    actionExecutionDetail_actionExecutionId,
    actionExecutionDetail_actionName,
    actionExecutionDetail_input,
    actionExecutionDetail_lastUpdateTime,
    actionExecutionDetail_output,
    actionExecutionDetail_pipelineExecutionId,
    actionExecutionDetail_pipelineVersion,
    actionExecutionDetail_stageName,
    actionExecutionDetail_startTime,
    actionExecutionDetail_status,

    -- * ActionExecutionFilter
    ActionExecutionFilter (..),
    newActionExecutionFilter,
    actionExecutionFilter_pipelineExecutionId,

    -- * ActionExecutionInput
    ActionExecutionInput (..),
    newActionExecutionInput,
    actionExecutionInput_actionTypeId,
    actionExecutionInput_configuration,
    actionExecutionInput_inputArtifacts,
    actionExecutionInput_namespace,
    actionExecutionInput_region,
    actionExecutionInput_resolvedConfiguration,
    actionExecutionInput_roleArn,

    -- * ActionExecutionOutput
    ActionExecutionOutput (..),
    newActionExecutionOutput,
    actionExecutionOutput_executionResult,
    actionExecutionOutput_outputArtifacts,
    actionExecutionOutput_outputVariables,

    -- * ActionExecutionResult
    ActionExecutionResult (..),
    newActionExecutionResult,
    actionExecutionResult_externalExecutionId,
    actionExecutionResult_externalExecutionSummary,
    actionExecutionResult_externalExecutionUrl,

    -- * ActionRevision
    ActionRevision (..),
    newActionRevision,
    actionRevision_revisionId,
    actionRevision_revisionChangeId,
    actionRevision_created,

    -- * ActionState
    ActionState (..),
    newActionState,
    actionState_actionName,
    actionState_currentRevision,
    actionState_entityUrl,
    actionState_latestExecution,
    actionState_revisionUrl,

    -- * ActionType
    ActionType (..),
    newActionType,
    actionType_actionConfigurationProperties,
    actionType_settings,
    actionType_id,
    actionType_inputArtifactDetails,
    actionType_outputArtifactDetails,

    -- * ActionTypeArtifactDetails
    ActionTypeArtifactDetails (..),
    newActionTypeArtifactDetails,
    actionTypeArtifactDetails_minimumCount,
    actionTypeArtifactDetails_maximumCount,

    -- * ActionTypeDeclaration
    ActionTypeDeclaration (..),
    newActionTypeDeclaration,
    actionTypeDeclaration_description,
    actionTypeDeclaration_permissions,
    actionTypeDeclaration_properties,
    actionTypeDeclaration_urls,
    actionTypeDeclaration_executor,
    actionTypeDeclaration_id,
    actionTypeDeclaration_inputArtifactDetails,
    actionTypeDeclaration_outputArtifactDetails,

    -- * ActionTypeExecutor
    ActionTypeExecutor (..),
    newActionTypeExecutor,
    actionTypeExecutor_jobTimeout,
    actionTypeExecutor_policyStatementsTemplate,
    actionTypeExecutor_configuration,
    actionTypeExecutor_type,

    -- * ActionTypeId
    ActionTypeId (..),
    newActionTypeId,
    actionTypeId_category,
    actionTypeId_owner,
    actionTypeId_provider,
    actionTypeId_version,

    -- * ActionTypeIdentifier
    ActionTypeIdentifier (..),
    newActionTypeIdentifier,
    actionTypeIdentifier_category,
    actionTypeIdentifier_owner,
    actionTypeIdentifier_provider,
    actionTypeIdentifier_version,

    -- * ActionTypePermissions
    ActionTypePermissions (..),
    newActionTypePermissions,
    actionTypePermissions_allowedAccounts,

    -- * ActionTypeProperty
    ActionTypeProperty (..),
    newActionTypeProperty,
    actionTypeProperty_description,
    actionTypeProperty_queryable,
    actionTypeProperty_name,
    actionTypeProperty_optional,
    actionTypeProperty_key,
    actionTypeProperty_noEcho,

    -- * ActionTypeSettings
    ActionTypeSettings (..),
    newActionTypeSettings,
    actionTypeSettings_entityUrlTemplate,
    actionTypeSettings_executionUrlTemplate,
    actionTypeSettings_revisionUrlTemplate,
    actionTypeSettings_thirdPartyConfigurationUrl,

    -- * ActionTypeUrls
    ActionTypeUrls (..),
    newActionTypeUrls,
    actionTypeUrls_configurationUrl,
    actionTypeUrls_entityUrlTemplate,
    actionTypeUrls_executionUrlTemplate,
    actionTypeUrls_revisionUrlTemplate,

    -- * ApprovalResult
    ApprovalResult (..),
    newApprovalResult,
    approvalResult_summary,
    approvalResult_status,

    -- * Artifact
    Artifact (..),
    newArtifact,
    artifact_location,
    artifact_name,
    artifact_revision,

    -- * ArtifactDetail
    ArtifactDetail (..),
    newArtifactDetail,
    artifactDetail_name,
    artifactDetail_s3location,

    -- * ArtifactDetails
    ArtifactDetails (..),
    newArtifactDetails,
    artifactDetails_minimumCount,
    artifactDetails_maximumCount,

    -- * ArtifactLocation
    ArtifactLocation (..),
    newArtifactLocation,
    artifactLocation_s3Location,
    artifactLocation_type,

    -- * ArtifactRevision
    ArtifactRevision (..),
    newArtifactRevision,
    artifactRevision_created,
    artifactRevision_name,
    artifactRevision_revisionChangeIdentifier,
    artifactRevision_revisionId,
    artifactRevision_revisionSummary,
    artifactRevision_revisionUrl,

    -- * ArtifactStore
    ArtifactStore (..),
    newArtifactStore,
    artifactStore_encryptionKey,
    artifactStore_type,
    artifactStore_location,

    -- * BlockerDeclaration
    BlockerDeclaration (..),
    newBlockerDeclaration,
    blockerDeclaration_name,
    blockerDeclaration_type,

    -- * CurrentRevision
    CurrentRevision (..),
    newCurrentRevision,
    currentRevision_created,
    currentRevision_revisionSummary,
    currentRevision_revision,
    currentRevision_changeIdentifier,

    -- * EncryptionKey
    EncryptionKey (..),
    newEncryptionKey,
    encryptionKey_id,
    encryptionKey_type,

    -- * ErrorDetails
    ErrorDetails (..),
    newErrorDetails,
    errorDetails_code,
    errorDetails_message,

    -- * ExecutionDetails
    ExecutionDetails (..),
    newExecutionDetails,
    executionDetails_externalExecutionId,
    executionDetails_percentComplete,
    executionDetails_summary,

    -- * ExecutionTrigger
    ExecutionTrigger (..),
    newExecutionTrigger,
    executionTrigger_triggerDetail,
    executionTrigger_triggerType,

    -- * ExecutorConfiguration
    ExecutorConfiguration (..),
    newExecutorConfiguration,
    executorConfiguration_jobWorkerExecutorConfiguration,
    executorConfiguration_lambdaExecutorConfiguration,

    -- * FailureDetails
    FailureDetails (..),
    newFailureDetails,
    failureDetails_externalExecutionId,
    failureDetails_type,
    failureDetails_message,

    -- * InputArtifact
    InputArtifact (..),
    newInputArtifact,
    inputArtifact_name,

    -- * Job
    Job (..),
    newJob,
    job_accountId,
    job_data,
    job_id,
    job_nonce,

    -- * JobData
    JobData (..),
    newJobData,
    jobData_actionConfiguration,
    jobData_actionTypeId,
    jobData_artifactCredentials,
    jobData_continuationToken,
    jobData_encryptionKey,
    jobData_inputArtifacts,
    jobData_outputArtifacts,
    jobData_pipelineContext,

    -- * JobDetails
    JobDetails (..),
    newJobDetails,
    jobDetails_accountId,
    jobDetails_data,
    jobDetails_id,

    -- * JobWorkerExecutorConfiguration
    JobWorkerExecutorConfiguration (..),
    newJobWorkerExecutorConfiguration,
    jobWorkerExecutorConfiguration_pollingAccounts,
    jobWorkerExecutorConfiguration_pollingServicePrincipals,

    -- * LambdaExecutorConfiguration
    LambdaExecutorConfiguration (..),
    newLambdaExecutorConfiguration,
    lambdaExecutorConfiguration_lambdaFunctionArn,

    -- * ListWebhookItem
    ListWebhookItem (..),
    newListWebhookItem,
    listWebhookItem_arn,
    listWebhookItem_errorCode,
    listWebhookItem_errorMessage,
    listWebhookItem_lastTriggered,
    listWebhookItem_tags,
    listWebhookItem_definition,
    listWebhookItem_url,

    -- * OutputArtifact
    OutputArtifact (..),
    newOutputArtifact,
    outputArtifact_name,

    -- * PipelineContext
    PipelineContext (..),
    newPipelineContext,
    pipelineContext_action,
    pipelineContext_pipelineArn,
    pipelineContext_pipelineExecutionId,
    pipelineContext_pipelineName,
    pipelineContext_stage,

    -- * PipelineDeclaration
    PipelineDeclaration (..),
    newPipelineDeclaration,
    pipelineDeclaration_artifactStore,
    pipelineDeclaration_artifactStores,
    pipelineDeclaration_version,
    pipelineDeclaration_name,
    pipelineDeclaration_roleArn,
    pipelineDeclaration_stages,

    -- * PipelineExecution
    PipelineExecution (..),
    newPipelineExecution,
    pipelineExecution_artifactRevisions,
    pipelineExecution_pipelineExecutionId,
    pipelineExecution_pipelineName,
    pipelineExecution_pipelineVersion,
    pipelineExecution_status,
    pipelineExecution_statusSummary,

    -- * PipelineExecutionSummary
    PipelineExecutionSummary (..),
    newPipelineExecutionSummary,
    pipelineExecutionSummary_lastUpdateTime,
    pipelineExecutionSummary_pipelineExecutionId,
    pipelineExecutionSummary_sourceRevisions,
    pipelineExecutionSummary_startTime,
    pipelineExecutionSummary_status,
    pipelineExecutionSummary_stopTrigger,
    pipelineExecutionSummary_trigger,

    -- * PipelineMetadata
    PipelineMetadata (..),
    newPipelineMetadata,
    pipelineMetadata_created,
    pipelineMetadata_pipelineArn,
    pipelineMetadata_updated,

    -- * PipelineSummary
    PipelineSummary (..),
    newPipelineSummary,
    pipelineSummary_created,
    pipelineSummary_name,
    pipelineSummary_updated,
    pipelineSummary_version,

    -- * S3ArtifactLocation
    S3ArtifactLocation (..),
    newS3ArtifactLocation,
    s3ArtifactLocation_bucketName,
    s3ArtifactLocation_objectKey,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucket,
    s3Location_key,

    -- * SourceRevision
    SourceRevision (..),
    newSourceRevision,
    sourceRevision_revisionId,
    sourceRevision_revisionSummary,
    sourceRevision_revisionUrl,
    sourceRevision_actionName,

    -- * StageContext
    StageContext (..),
    newStageContext,
    stageContext_name,

    -- * StageDeclaration
    StageDeclaration (..),
    newStageDeclaration,
    stageDeclaration_blockers,
    stageDeclaration_name,
    stageDeclaration_actions,

    -- * StageExecution
    StageExecution (..),
    newStageExecution,
    stageExecution_pipelineExecutionId,
    stageExecution_status,

    -- * StageState
    StageState (..),
    newStageState,
    stageState_actionStates,
    stageState_inboundExecution,
    stageState_inboundTransitionState,
    stageState_latestExecution,
    stageState_stageName,

    -- * StopExecutionTrigger
    StopExecutionTrigger (..),
    newStopExecutionTrigger,
    stopExecutionTrigger_reason,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * ThirdPartyJob
    ThirdPartyJob (..),
    newThirdPartyJob,
    thirdPartyJob_clientId,
    thirdPartyJob_jobId,

    -- * ThirdPartyJobData
    ThirdPartyJobData (..),
    newThirdPartyJobData,
    thirdPartyJobData_actionConfiguration,
    thirdPartyJobData_actionTypeId,
    thirdPartyJobData_artifactCredentials,
    thirdPartyJobData_continuationToken,
    thirdPartyJobData_encryptionKey,
    thirdPartyJobData_inputArtifacts,
    thirdPartyJobData_outputArtifacts,
    thirdPartyJobData_pipelineContext,

    -- * ThirdPartyJobDetails
    ThirdPartyJobDetails (..),
    newThirdPartyJobDetails,
    thirdPartyJobDetails_data,
    thirdPartyJobDetails_id,
    thirdPartyJobDetails_nonce,

    -- * TransitionState
    TransitionState (..),
    newTransitionState,
    transitionState_disabledReason,
    transitionState_enabled,
    transitionState_lastChangedAt,
    transitionState_lastChangedBy,

    -- * WebhookAuthConfiguration
    WebhookAuthConfiguration (..),
    newWebhookAuthConfiguration,
    webhookAuthConfiguration_allowedIPRange,
    webhookAuthConfiguration_secretToken,

    -- * WebhookDefinition
    WebhookDefinition (..),
    newWebhookDefinition,
    webhookDefinition_name,
    webhookDefinition_targetPipeline,
    webhookDefinition_targetAction,
    webhookDefinition_filters,
    webhookDefinition_authentication,
    webhookDefinition_authenticationConfiguration,

    -- * WebhookFilterRule
    WebhookFilterRule (..),
    newWebhookFilterRule,
    webhookFilterRule_matchEquals,
    webhookFilterRule_jsonPath,
  )
where

import Amazonka.CodePipeline.Types.AWSSessionCredentials
import Amazonka.CodePipeline.Types.ActionCategory
import Amazonka.CodePipeline.Types.ActionConfiguration
import Amazonka.CodePipeline.Types.ActionConfigurationProperty
import Amazonka.CodePipeline.Types.ActionConfigurationPropertyType
import Amazonka.CodePipeline.Types.ActionContext
import Amazonka.CodePipeline.Types.ActionDeclaration
import Amazonka.CodePipeline.Types.ActionExecution
import Amazonka.CodePipeline.Types.ActionExecutionDetail
import Amazonka.CodePipeline.Types.ActionExecutionFilter
import Amazonka.CodePipeline.Types.ActionExecutionInput
import Amazonka.CodePipeline.Types.ActionExecutionOutput
import Amazonka.CodePipeline.Types.ActionExecutionResult
import Amazonka.CodePipeline.Types.ActionExecutionStatus
import Amazonka.CodePipeline.Types.ActionOwner
import Amazonka.CodePipeline.Types.ActionRevision
import Amazonka.CodePipeline.Types.ActionState
import Amazonka.CodePipeline.Types.ActionType
import Amazonka.CodePipeline.Types.ActionTypeArtifactDetails
import Amazonka.CodePipeline.Types.ActionTypeDeclaration
import Amazonka.CodePipeline.Types.ActionTypeExecutor
import Amazonka.CodePipeline.Types.ActionTypeId
import Amazonka.CodePipeline.Types.ActionTypeIdentifier
import Amazonka.CodePipeline.Types.ActionTypePermissions
import Amazonka.CodePipeline.Types.ActionTypeProperty
import Amazonka.CodePipeline.Types.ActionTypeSettings
import Amazonka.CodePipeline.Types.ActionTypeUrls
import Amazonka.CodePipeline.Types.ApprovalResult
import Amazonka.CodePipeline.Types.ApprovalStatus
import Amazonka.CodePipeline.Types.Artifact
import Amazonka.CodePipeline.Types.ArtifactDetail
import Amazonka.CodePipeline.Types.ArtifactDetails
import Amazonka.CodePipeline.Types.ArtifactLocation
import Amazonka.CodePipeline.Types.ArtifactLocationType
import Amazonka.CodePipeline.Types.ArtifactRevision
import Amazonka.CodePipeline.Types.ArtifactStore
import Amazonka.CodePipeline.Types.ArtifactStoreType
import Amazonka.CodePipeline.Types.BlockerDeclaration
import Amazonka.CodePipeline.Types.BlockerType
import Amazonka.CodePipeline.Types.CurrentRevision
import Amazonka.CodePipeline.Types.EncryptionKey
import Amazonka.CodePipeline.Types.EncryptionKeyType
import Amazonka.CodePipeline.Types.ErrorDetails
import Amazonka.CodePipeline.Types.ExecutionDetails
import Amazonka.CodePipeline.Types.ExecutionTrigger
import Amazonka.CodePipeline.Types.ExecutorConfiguration
import Amazonka.CodePipeline.Types.ExecutorType
import Amazonka.CodePipeline.Types.FailureDetails
import Amazonka.CodePipeline.Types.FailureType
import Amazonka.CodePipeline.Types.InputArtifact
import Amazonka.CodePipeline.Types.Job
import Amazonka.CodePipeline.Types.JobData
import Amazonka.CodePipeline.Types.JobDetails
import Amazonka.CodePipeline.Types.JobStatus
import Amazonka.CodePipeline.Types.JobWorkerExecutorConfiguration
import Amazonka.CodePipeline.Types.LambdaExecutorConfiguration
import Amazonka.CodePipeline.Types.ListWebhookItem
import Amazonka.CodePipeline.Types.OutputArtifact
import Amazonka.CodePipeline.Types.PipelineContext
import Amazonka.CodePipeline.Types.PipelineDeclaration
import Amazonka.CodePipeline.Types.PipelineExecution
import Amazonka.CodePipeline.Types.PipelineExecutionStatus
import Amazonka.CodePipeline.Types.PipelineExecutionSummary
import Amazonka.CodePipeline.Types.PipelineMetadata
import Amazonka.CodePipeline.Types.PipelineSummary
import Amazonka.CodePipeline.Types.S3ArtifactLocation
import Amazonka.CodePipeline.Types.S3Location
import Amazonka.CodePipeline.Types.SourceRevision
import Amazonka.CodePipeline.Types.StageContext
import Amazonka.CodePipeline.Types.StageDeclaration
import Amazonka.CodePipeline.Types.StageExecution
import Amazonka.CodePipeline.Types.StageExecutionStatus
import Amazonka.CodePipeline.Types.StageRetryMode
import Amazonka.CodePipeline.Types.StageState
import Amazonka.CodePipeline.Types.StageTransitionType
import Amazonka.CodePipeline.Types.StopExecutionTrigger
import Amazonka.CodePipeline.Types.Tag
import Amazonka.CodePipeline.Types.ThirdPartyJob
import Amazonka.CodePipeline.Types.ThirdPartyJobData
import Amazonka.CodePipeline.Types.ThirdPartyJobDetails
import Amazonka.CodePipeline.Types.TransitionState
import Amazonka.CodePipeline.Types.TriggerType
import Amazonka.CodePipeline.Types.WebhookAuthConfiguration
import Amazonka.CodePipeline.Types.WebhookAuthenticationType
import Amazonka.CodePipeline.Types.WebhookDefinition
import Amazonka.CodePipeline.Types.WebhookFilterRule
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-07-09@ of the Amazon CodePipeline SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CodePipeline",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "codepipeline",
      Core.signingName = "codepipeline",
      Core.version = "2015-07-09",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CodePipeline",
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

-- | The specified action cannot be found.
_ActionNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ActionNotFoundException =
  Core._MatchServiceError
    defaultService
    "ActionNotFoundException"

-- | The specified action type already exists with a different definition.
_ActionTypeAlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ActionTypeAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ActionTypeAlreadyExistsException"

-- | The specified action type cannot be found.
_ActionTypeNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ActionTypeNotFoundException =
  Core._MatchServiceError
    defaultService
    "ActionTypeNotFoundException"

-- | The approval action has already been approved or rejected.
_ApprovalAlreadyCompletedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ApprovalAlreadyCompletedException =
  Core._MatchServiceError
    defaultService
    "ApprovalAlreadyCompletedException"

-- | Unable to modify the tag due to a simultaneous update request.
_ConcurrentModificationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | Your request cannot be handled because the pipeline is busy handling
-- ongoing activities. Try again later.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The pipeline execution is already in a @Stopping@ state. If you already
-- chose to stop and wait, you cannot make that request again. You can
-- choose to stop and abandon now, but be aware that this option can lead
-- to failed tasks or out of sequence tasks. If you already chose to stop
-- and abandon, you cannot make that request again.
_DuplicatedStopRequestException :: Core.AsError a => Lens.Fold a Core.ServiceError
_DuplicatedStopRequestException =
  Core._MatchServiceError
    defaultService
    "DuplicatedStopRequestException"

-- | The action declaration was specified in an invalid format.
_InvalidActionDeclarationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidActionDeclarationException =
  Core._MatchServiceError
    defaultService
    "InvalidActionDeclarationException"

-- | The approval request already received a response or has expired.
_InvalidApprovalTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidApprovalTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalTokenException"

-- | The specified resource ARN is invalid.
_InvalidArnException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"

-- | Reserved for future use.
_InvalidBlockerDeclarationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidBlockerDeclarationException =
  Core._MatchServiceError
    defaultService
    "InvalidBlockerDeclarationException"

-- | The client token was specified in an invalid format
_InvalidClientTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidClientTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidClientTokenException"

-- | The job was specified in an invalid format or cannot be found.
_InvalidJobException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidJobException =
  Core._MatchServiceError
    defaultService
    "InvalidJobException"

-- | The job state was specified in an invalid format.
_InvalidJobStateException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidJobStateException =
  Core._MatchServiceError
    defaultService
    "InvalidJobStateException"

-- | The next token was specified in an invalid format. Make sure that the
-- next token you provide is the token returned by a previous call.
_InvalidNextTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The nonce was specified in an invalid format.
_InvalidNonceException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidNonceException =
  Core._MatchServiceError
    defaultService
    "InvalidNonceException"

-- | The stage declaration was specified in an invalid format.
_InvalidStageDeclarationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidStageDeclarationException =
  Core._MatchServiceError
    defaultService
    "InvalidStageDeclarationException"

-- | The structure was specified in an invalid format.
_InvalidStructureException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidStructureException =
  Core._MatchServiceError
    defaultService
    "InvalidStructureException"

-- | The specified resource tags are invalid.
_InvalidTagsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidTagsException =
  Core._MatchServiceError
    defaultService
    "InvalidTagsException"

-- | The specified authentication type is in an invalid format.
_InvalidWebhookAuthenticationParametersException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidWebhookAuthenticationParametersException =
  Core._MatchServiceError
    defaultService
    "InvalidWebhookAuthenticationParametersException"

-- | The specified event filter rule is in an invalid format.
_InvalidWebhookFilterPatternException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidWebhookFilterPatternException =
  Core._MatchServiceError
    defaultService
    "InvalidWebhookFilterPatternException"

-- | The job was specified in an invalid format or cannot be found.
_JobNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_JobNotFoundException =
  Core._MatchServiceError
    defaultService
    "JobNotFoundException"

-- | The number of pipelines associated with the AWS account has exceeded the
-- limit allowed for the account.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The stage has failed in a later run of the pipeline and the
-- pipelineExecutionId associated with the request is out of date.
_NotLatestPipelineExecutionException :: Core.AsError a => Lens.Fold a Core.ServiceError
_NotLatestPipelineExecutionException =
  Core._MatchServiceError
    defaultService
    "NotLatestPipelineExecutionException"

-- | Exceeded the total size limit for all variables in the pipeline.
_OutputVariablesSizeExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_OutputVariablesSizeExceededException =
  Core._MatchServiceError
    defaultService
    "OutputVariablesSizeExceededException"

-- | The pipeline execution was specified in an invalid format or cannot be
-- found, or an execution ID does not belong to the specified pipeline.
_PipelineExecutionNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PipelineExecutionNotFoundException =
  Core._MatchServiceError
    defaultService
    "PipelineExecutionNotFoundException"

-- | Unable to stop the pipeline execution. The execution might already be in
-- a @Stopped@ state, or it might no longer be in progress.
_PipelineExecutionNotStoppableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PipelineExecutionNotStoppableException =
  Core._MatchServiceError
    defaultService
    "PipelineExecutionNotStoppableException"

-- | The specified pipeline name is already in use.
_PipelineNameInUseException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PipelineNameInUseException =
  Core._MatchServiceError
    defaultService
    "PipelineNameInUseException"

-- | The pipeline was specified in an invalid format or cannot be found.
_PipelineNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PipelineNotFoundException =
  Core._MatchServiceError
    defaultService
    "PipelineNotFoundException"

-- | The pipeline version was specified in an invalid format or cannot be
-- found.
_PipelineVersionNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PipelineVersionNotFoundException =
  Core._MatchServiceError
    defaultService
    "PipelineVersionNotFoundException"

-- | The request failed because of an unknown error, exception, or failure.
_RequestFailedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_RequestFailedException =
  Core._MatchServiceError
    defaultService
    "RequestFailedException"

-- | The resource was specified in an invalid format.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The stage was specified in an invalid format or cannot be found.
_StageNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_StageNotFoundException =
  Core._MatchServiceError
    defaultService
    "StageNotFoundException"

-- | Unable to retry. The pipeline structure or stage state might have
-- changed while actions awaited retry, or the stage contains no failed
-- actions.
_StageNotRetryableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_StageNotRetryableException =
  Core._MatchServiceError
    defaultService
    "StageNotRetryableException"

-- | The tags limit for a resource has been exceeded.
_TooManyTagsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The validation was specified in an invalid format.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The specified webhook was entered in an invalid format or cannot be
-- found.
_WebhookNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_WebhookNotFoundException =
  Core._MatchServiceError
    defaultService
    "WebhookNotFoundException"
