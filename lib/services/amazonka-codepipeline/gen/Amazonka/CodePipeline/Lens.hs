{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodePipeline.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Lens
  ( -- * Operations

    -- ** AcknowledgeJob
    acknowledgeJob_jobId,
    acknowledgeJob_nonce,
    acknowledgeJobResponse_status,
    acknowledgeJobResponse_httpStatus,

    -- ** AcknowledgeThirdPartyJob
    acknowledgeThirdPartyJob_jobId,
    acknowledgeThirdPartyJob_nonce,
    acknowledgeThirdPartyJob_clientToken,
    acknowledgeThirdPartyJobResponse_status,
    acknowledgeThirdPartyJobResponse_httpStatus,

    -- ** CreateCustomActionType
    createCustomActionType_configurationProperties,
    createCustomActionType_settings,
    createCustomActionType_tags,
    createCustomActionType_category,
    createCustomActionType_provider,
    createCustomActionType_version,
    createCustomActionType_inputArtifactDetails,
    createCustomActionType_outputArtifactDetails,
    createCustomActionTypeResponse_tags,
    createCustomActionTypeResponse_httpStatus,
    createCustomActionTypeResponse_actionType,

    -- ** CreatePipeline
    createPipeline_tags,
    createPipeline_pipeline,
    createPipelineResponse_pipeline,
    createPipelineResponse_tags,
    createPipelineResponse_httpStatus,

    -- ** DeleteCustomActionType
    deleteCustomActionType_category,
    deleteCustomActionType_provider,
    deleteCustomActionType_version,

    -- ** DeletePipeline
    deletePipeline_name,

    -- ** DeleteWebhook
    deleteWebhook_name,
    deleteWebhookResponse_httpStatus,

    -- ** DeregisterWebhookWithThirdParty
    deregisterWebhookWithThirdParty_webhookName,
    deregisterWebhookWithThirdPartyResponse_httpStatus,

    -- ** DisableStageTransition
    disableStageTransition_pipelineName,
    disableStageTransition_stageName,
    disableStageTransition_transitionType,
    disableStageTransition_reason,

    -- ** EnableStageTransition
    enableStageTransition_pipelineName,
    enableStageTransition_stageName,
    enableStageTransition_transitionType,

    -- ** GetActionType
    getActionType_category,
    getActionType_owner,
    getActionType_provider,
    getActionType_version,
    getActionTypeResponse_actionType,
    getActionTypeResponse_httpStatus,

    -- ** GetJobDetails
    getJobDetails_jobId,
    getJobDetailsResponse_jobDetails,
    getJobDetailsResponse_httpStatus,

    -- ** GetPipeline
    getPipeline_version,
    getPipeline_name,
    getPipelineResponse_metadata,
    getPipelineResponse_pipeline,
    getPipelineResponse_httpStatus,

    -- ** GetPipelineExecution
    getPipelineExecution_pipelineName,
    getPipelineExecution_pipelineExecutionId,
    getPipelineExecutionResponse_pipelineExecution,
    getPipelineExecutionResponse_httpStatus,

    -- ** GetPipelineState
    getPipelineState_name,
    getPipelineStateResponse_created,
    getPipelineStateResponse_pipelineName,
    getPipelineStateResponse_pipelineVersion,
    getPipelineStateResponse_stageStates,
    getPipelineStateResponse_updated,
    getPipelineStateResponse_httpStatus,

    -- ** GetThirdPartyJobDetails
    getThirdPartyJobDetails_jobId,
    getThirdPartyJobDetails_clientToken,
    getThirdPartyJobDetailsResponse_jobDetails,
    getThirdPartyJobDetailsResponse_httpStatus,

    -- ** ListActionExecutions
    listActionExecutions_filter,
    listActionExecutions_maxResults,
    listActionExecutions_nextToken,
    listActionExecutions_pipelineName,
    listActionExecutionsResponse_actionExecutionDetails,
    listActionExecutionsResponse_nextToken,
    listActionExecutionsResponse_httpStatus,

    -- ** ListActionTypes
    listActionTypes_actionOwnerFilter,
    listActionTypes_nextToken,
    listActionTypes_regionFilter,
    listActionTypesResponse_nextToken,
    listActionTypesResponse_httpStatus,
    listActionTypesResponse_actionTypes,

    -- ** ListPipelineExecutions
    listPipelineExecutions_maxResults,
    listPipelineExecutions_nextToken,
    listPipelineExecutions_pipelineName,
    listPipelineExecutionsResponse_nextToken,
    listPipelineExecutionsResponse_pipelineExecutionSummaries,
    listPipelineExecutionsResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_maxResults,
    listPipelines_nextToken,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_maxResults,
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWebhooks
    listWebhooks_maxResults,
    listWebhooks_nextToken,
    listWebhooksResponse_nextToken,
    listWebhooksResponse_webhooks,
    listWebhooksResponse_httpStatus,

    -- ** PollForJobs
    pollForJobs_maxBatchSize,
    pollForJobs_queryParam,
    pollForJobs_actionTypeId,
    pollForJobsResponse_jobs,
    pollForJobsResponse_httpStatus,

    -- ** PollForThirdPartyJobs
    pollForThirdPartyJobs_maxBatchSize,
    pollForThirdPartyJobs_actionTypeId,
    pollForThirdPartyJobsResponse_jobs,
    pollForThirdPartyJobsResponse_httpStatus,

    -- ** PutActionRevision
    putActionRevision_pipelineName,
    putActionRevision_stageName,
    putActionRevision_actionName,
    putActionRevision_actionRevision,
    putActionRevisionResponse_newRevision,
    putActionRevisionResponse_pipelineExecutionId,
    putActionRevisionResponse_httpStatus,

    -- ** PutApprovalResult
    putApprovalResult_pipelineName,
    putApprovalResult_stageName,
    putApprovalResult_actionName,
    putApprovalResult_result,
    putApprovalResult_token,
    putApprovalResultResponse_approvedAt,
    putApprovalResultResponse_httpStatus,

    -- ** PutJobFailureResult
    putJobFailureResult_jobId,
    putJobFailureResult_failureDetails,

    -- ** PutJobSuccessResult
    putJobSuccessResult_continuationToken,
    putJobSuccessResult_currentRevision,
    putJobSuccessResult_executionDetails,
    putJobSuccessResult_outputVariables,
    putJobSuccessResult_jobId,

    -- ** PutThirdPartyJobFailureResult
    putThirdPartyJobFailureResult_jobId,
    putThirdPartyJobFailureResult_clientToken,
    putThirdPartyJobFailureResult_failureDetails,

    -- ** PutThirdPartyJobSuccessResult
    putThirdPartyJobSuccessResult_continuationToken,
    putThirdPartyJobSuccessResult_currentRevision,
    putThirdPartyJobSuccessResult_executionDetails,
    putThirdPartyJobSuccessResult_jobId,
    putThirdPartyJobSuccessResult_clientToken,

    -- ** PutWebhook
    putWebhook_tags,
    putWebhook_webhook,
    putWebhookResponse_webhook,
    putWebhookResponse_httpStatus,

    -- ** RegisterWebhookWithThirdParty
    registerWebhookWithThirdParty_webhookName,
    registerWebhookWithThirdPartyResponse_httpStatus,

    -- ** RetryStageExecution
    retryStageExecution_pipelineName,
    retryStageExecution_stageName,
    retryStageExecution_pipelineExecutionId,
    retryStageExecution_retryMode,
    retryStageExecutionResponse_pipelineExecutionId,
    retryStageExecutionResponse_httpStatus,

    -- ** StartPipelineExecution
    startPipelineExecution_clientRequestToken,
    startPipelineExecution_name,
    startPipelineExecutionResponse_pipelineExecutionId,
    startPipelineExecutionResponse_httpStatus,

    -- ** StopPipelineExecution
    stopPipelineExecution_abandon,
    stopPipelineExecution_reason,
    stopPipelineExecution_pipelineName,
    stopPipelineExecution_pipelineExecutionId,
    stopPipelineExecutionResponse_pipelineExecutionId,
    stopPipelineExecutionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateActionType
    updateActionType_actionType,

    -- ** UpdatePipeline
    updatePipeline_pipeline,
    updatePipelineResponse_pipeline,
    updatePipelineResponse_httpStatus,

    -- * Types

    -- ** AWSSessionCredentials
    aWSSessionCredentials_accessKeyId,
    aWSSessionCredentials_secretAccessKey,
    aWSSessionCredentials_sessionToken,

    -- ** ActionConfiguration
    actionConfiguration_configuration,

    -- ** ActionConfigurationProperty
    actionConfigurationProperty_description,
    actionConfigurationProperty_queryable,
    actionConfigurationProperty_type,
    actionConfigurationProperty_name,
    actionConfigurationProperty_required,
    actionConfigurationProperty_key,
    actionConfigurationProperty_secret,

    -- ** ActionContext
    actionContext_actionExecutionId,
    actionContext_name,

    -- ** ActionDeclaration
    actionDeclaration_configuration,
    actionDeclaration_inputArtifacts,
    actionDeclaration_namespace,
    actionDeclaration_outputArtifacts,
    actionDeclaration_region,
    actionDeclaration_roleArn,
    actionDeclaration_runOrder,
    actionDeclaration_name,
    actionDeclaration_actionTypeId,

    -- ** ActionExecution
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

    -- ** ActionExecutionDetail
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

    -- ** ActionExecutionFilter
    actionExecutionFilter_pipelineExecutionId,

    -- ** ActionExecutionInput
    actionExecutionInput_actionTypeId,
    actionExecutionInput_configuration,
    actionExecutionInput_inputArtifacts,
    actionExecutionInput_namespace,
    actionExecutionInput_region,
    actionExecutionInput_resolvedConfiguration,
    actionExecutionInput_roleArn,

    -- ** ActionExecutionOutput
    actionExecutionOutput_executionResult,
    actionExecutionOutput_outputArtifacts,
    actionExecutionOutput_outputVariables,

    -- ** ActionExecutionResult
    actionExecutionResult_externalExecutionId,
    actionExecutionResult_externalExecutionSummary,
    actionExecutionResult_externalExecutionUrl,

    -- ** ActionRevision
    actionRevision_revisionId,
    actionRevision_revisionChangeId,
    actionRevision_created,

    -- ** ActionState
    actionState_actionName,
    actionState_currentRevision,
    actionState_entityUrl,
    actionState_latestExecution,
    actionState_revisionUrl,

    -- ** ActionType
    actionType_actionConfigurationProperties,
    actionType_settings,
    actionType_id,
    actionType_inputArtifactDetails,
    actionType_outputArtifactDetails,

    -- ** ActionTypeArtifactDetails
    actionTypeArtifactDetails_minimumCount,
    actionTypeArtifactDetails_maximumCount,

    -- ** ActionTypeDeclaration
    actionTypeDeclaration_description,
    actionTypeDeclaration_permissions,
    actionTypeDeclaration_properties,
    actionTypeDeclaration_urls,
    actionTypeDeclaration_executor,
    actionTypeDeclaration_id,
    actionTypeDeclaration_inputArtifactDetails,
    actionTypeDeclaration_outputArtifactDetails,

    -- ** ActionTypeExecutor
    actionTypeExecutor_jobTimeout,
    actionTypeExecutor_policyStatementsTemplate,
    actionTypeExecutor_configuration,
    actionTypeExecutor_type,

    -- ** ActionTypeId
    actionTypeId_category,
    actionTypeId_owner,
    actionTypeId_provider,
    actionTypeId_version,

    -- ** ActionTypeIdentifier
    actionTypeIdentifier_category,
    actionTypeIdentifier_owner,
    actionTypeIdentifier_provider,
    actionTypeIdentifier_version,

    -- ** ActionTypePermissions
    actionTypePermissions_allowedAccounts,

    -- ** ActionTypeProperty
    actionTypeProperty_description,
    actionTypeProperty_queryable,
    actionTypeProperty_name,
    actionTypeProperty_optional,
    actionTypeProperty_key,
    actionTypeProperty_noEcho,

    -- ** ActionTypeSettings
    actionTypeSettings_entityUrlTemplate,
    actionTypeSettings_executionUrlTemplate,
    actionTypeSettings_revisionUrlTemplate,
    actionTypeSettings_thirdPartyConfigurationUrl,

    -- ** ActionTypeUrls
    actionTypeUrls_configurationUrl,
    actionTypeUrls_entityUrlTemplate,
    actionTypeUrls_executionUrlTemplate,
    actionTypeUrls_revisionUrlTemplate,

    -- ** ApprovalResult
    approvalResult_summary,
    approvalResult_status,

    -- ** Artifact
    artifact_location,
    artifact_name,
    artifact_revision,

    -- ** ArtifactDetail
    artifactDetail_name,
    artifactDetail_s3location,

    -- ** ArtifactDetails
    artifactDetails_minimumCount,
    artifactDetails_maximumCount,

    -- ** ArtifactLocation
    artifactLocation_s3Location,
    artifactLocation_type,

    -- ** ArtifactRevision
    artifactRevision_created,
    artifactRevision_name,
    artifactRevision_revisionChangeIdentifier,
    artifactRevision_revisionId,
    artifactRevision_revisionSummary,
    artifactRevision_revisionUrl,

    -- ** ArtifactStore
    artifactStore_encryptionKey,
    artifactStore_type,
    artifactStore_location,

    -- ** BlockerDeclaration
    blockerDeclaration_name,
    blockerDeclaration_type,

    -- ** CurrentRevision
    currentRevision_created,
    currentRevision_revisionSummary,
    currentRevision_revision,
    currentRevision_changeIdentifier,

    -- ** EncryptionKey
    encryptionKey_id,
    encryptionKey_type,

    -- ** ErrorDetails
    errorDetails_code,
    errorDetails_message,

    -- ** ExecutionDetails
    executionDetails_externalExecutionId,
    executionDetails_percentComplete,
    executionDetails_summary,

    -- ** ExecutionTrigger
    executionTrigger_triggerDetail,
    executionTrigger_triggerType,

    -- ** ExecutorConfiguration
    executorConfiguration_jobWorkerExecutorConfiguration,
    executorConfiguration_lambdaExecutorConfiguration,

    -- ** FailureDetails
    failureDetails_externalExecutionId,
    failureDetails_type,
    failureDetails_message,

    -- ** InputArtifact
    inputArtifact_name,

    -- ** Job
    job_accountId,
    job_data,
    job_id,
    job_nonce,

    -- ** JobData
    jobData_actionConfiguration,
    jobData_actionTypeId,
    jobData_artifactCredentials,
    jobData_continuationToken,
    jobData_encryptionKey,
    jobData_inputArtifacts,
    jobData_outputArtifacts,
    jobData_pipelineContext,

    -- ** JobDetails
    jobDetails_accountId,
    jobDetails_data,
    jobDetails_id,

    -- ** JobWorkerExecutorConfiguration
    jobWorkerExecutorConfiguration_pollingAccounts,
    jobWorkerExecutorConfiguration_pollingServicePrincipals,

    -- ** LambdaExecutorConfiguration
    lambdaExecutorConfiguration_lambdaFunctionArn,

    -- ** ListWebhookItem
    listWebhookItem_arn,
    listWebhookItem_errorCode,
    listWebhookItem_errorMessage,
    listWebhookItem_lastTriggered,
    listWebhookItem_tags,
    listWebhookItem_definition,
    listWebhookItem_url,

    -- ** OutputArtifact
    outputArtifact_name,

    -- ** PipelineContext
    pipelineContext_action,
    pipelineContext_pipelineArn,
    pipelineContext_pipelineExecutionId,
    pipelineContext_pipelineName,
    pipelineContext_stage,

    -- ** PipelineDeclaration
    pipelineDeclaration_artifactStore,
    pipelineDeclaration_artifactStores,
    pipelineDeclaration_version,
    pipelineDeclaration_name,
    pipelineDeclaration_roleArn,
    pipelineDeclaration_stages,

    -- ** PipelineExecution
    pipelineExecution_artifactRevisions,
    pipelineExecution_pipelineExecutionId,
    pipelineExecution_pipelineName,
    pipelineExecution_pipelineVersion,
    pipelineExecution_status,
    pipelineExecution_statusSummary,

    -- ** PipelineExecutionSummary
    pipelineExecutionSummary_lastUpdateTime,
    pipelineExecutionSummary_pipelineExecutionId,
    pipelineExecutionSummary_sourceRevisions,
    pipelineExecutionSummary_startTime,
    pipelineExecutionSummary_status,
    pipelineExecutionSummary_stopTrigger,
    pipelineExecutionSummary_trigger,

    -- ** PipelineMetadata
    pipelineMetadata_created,
    pipelineMetadata_pipelineArn,
    pipelineMetadata_updated,

    -- ** PipelineSummary
    pipelineSummary_created,
    pipelineSummary_name,
    pipelineSummary_updated,
    pipelineSummary_version,

    -- ** S3ArtifactLocation
    s3ArtifactLocation_bucketName,
    s3ArtifactLocation_objectKey,

    -- ** S3Location
    s3Location_bucket,
    s3Location_key,

    -- ** SourceRevision
    sourceRevision_revisionId,
    sourceRevision_revisionSummary,
    sourceRevision_revisionUrl,
    sourceRevision_actionName,

    -- ** StageContext
    stageContext_name,

    -- ** StageDeclaration
    stageDeclaration_blockers,
    stageDeclaration_name,
    stageDeclaration_actions,

    -- ** StageExecution
    stageExecution_pipelineExecutionId,
    stageExecution_status,

    -- ** StageState
    stageState_actionStates,
    stageState_inboundExecution,
    stageState_inboundTransitionState,
    stageState_latestExecution,
    stageState_stageName,

    -- ** StopExecutionTrigger
    stopExecutionTrigger_reason,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ThirdPartyJob
    thirdPartyJob_clientId,
    thirdPartyJob_jobId,

    -- ** ThirdPartyJobData
    thirdPartyJobData_actionConfiguration,
    thirdPartyJobData_actionTypeId,
    thirdPartyJobData_artifactCredentials,
    thirdPartyJobData_continuationToken,
    thirdPartyJobData_encryptionKey,
    thirdPartyJobData_inputArtifacts,
    thirdPartyJobData_outputArtifacts,
    thirdPartyJobData_pipelineContext,

    -- ** ThirdPartyJobDetails
    thirdPartyJobDetails_data,
    thirdPartyJobDetails_id,
    thirdPartyJobDetails_nonce,

    -- ** TransitionState
    transitionState_disabledReason,
    transitionState_enabled,
    transitionState_lastChangedAt,
    transitionState_lastChangedBy,

    -- ** WebhookAuthConfiguration
    webhookAuthConfiguration_allowedIPRange,
    webhookAuthConfiguration_secretToken,

    -- ** WebhookDefinition
    webhookDefinition_name,
    webhookDefinition_targetPipeline,
    webhookDefinition_targetAction,
    webhookDefinition_filters,
    webhookDefinition_authentication,
    webhookDefinition_authenticationConfiguration,

    -- ** WebhookFilterRule
    webhookFilterRule_matchEquals,
    webhookFilterRule_jsonPath,
  )
where

import Amazonka.CodePipeline.AcknowledgeJob
import Amazonka.CodePipeline.AcknowledgeThirdPartyJob
import Amazonka.CodePipeline.CreateCustomActionType
import Amazonka.CodePipeline.CreatePipeline
import Amazonka.CodePipeline.DeleteCustomActionType
import Amazonka.CodePipeline.DeletePipeline
import Amazonka.CodePipeline.DeleteWebhook
import Amazonka.CodePipeline.DeregisterWebhookWithThirdParty
import Amazonka.CodePipeline.DisableStageTransition
import Amazonka.CodePipeline.EnableStageTransition
import Amazonka.CodePipeline.GetActionType
import Amazonka.CodePipeline.GetJobDetails
import Amazonka.CodePipeline.GetPipeline
import Amazonka.CodePipeline.GetPipelineExecution
import Amazonka.CodePipeline.GetPipelineState
import Amazonka.CodePipeline.GetThirdPartyJobDetails
import Amazonka.CodePipeline.ListActionExecutions
import Amazonka.CodePipeline.ListActionTypes
import Amazonka.CodePipeline.ListPipelineExecutions
import Amazonka.CodePipeline.ListPipelines
import Amazonka.CodePipeline.ListTagsForResource
import Amazonka.CodePipeline.ListWebhooks
import Amazonka.CodePipeline.PollForJobs
import Amazonka.CodePipeline.PollForThirdPartyJobs
import Amazonka.CodePipeline.PutActionRevision
import Amazonka.CodePipeline.PutApprovalResult
import Amazonka.CodePipeline.PutJobFailureResult
import Amazonka.CodePipeline.PutJobSuccessResult
import Amazonka.CodePipeline.PutThirdPartyJobFailureResult
import Amazonka.CodePipeline.PutThirdPartyJobSuccessResult
import Amazonka.CodePipeline.PutWebhook
import Amazonka.CodePipeline.RegisterWebhookWithThirdParty
import Amazonka.CodePipeline.RetryStageExecution
import Amazonka.CodePipeline.StartPipelineExecution
import Amazonka.CodePipeline.StopPipelineExecution
import Amazonka.CodePipeline.TagResource
import Amazonka.CodePipeline.Types.AWSSessionCredentials
import Amazonka.CodePipeline.Types.ActionConfiguration
import Amazonka.CodePipeline.Types.ActionConfigurationProperty
import Amazonka.CodePipeline.Types.ActionContext
import Amazonka.CodePipeline.Types.ActionDeclaration
import Amazonka.CodePipeline.Types.ActionExecution
import Amazonka.CodePipeline.Types.ActionExecutionDetail
import Amazonka.CodePipeline.Types.ActionExecutionFilter
import Amazonka.CodePipeline.Types.ActionExecutionInput
import Amazonka.CodePipeline.Types.ActionExecutionOutput
import Amazonka.CodePipeline.Types.ActionExecutionResult
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
import Amazonka.CodePipeline.Types.Artifact
import Amazonka.CodePipeline.Types.ArtifactDetail
import Amazonka.CodePipeline.Types.ArtifactDetails
import Amazonka.CodePipeline.Types.ArtifactLocation
import Amazonka.CodePipeline.Types.ArtifactRevision
import Amazonka.CodePipeline.Types.ArtifactStore
import Amazonka.CodePipeline.Types.BlockerDeclaration
import Amazonka.CodePipeline.Types.CurrentRevision
import Amazonka.CodePipeline.Types.EncryptionKey
import Amazonka.CodePipeline.Types.ErrorDetails
import Amazonka.CodePipeline.Types.ExecutionDetails
import Amazonka.CodePipeline.Types.ExecutionTrigger
import Amazonka.CodePipeline.Types.ExecutorConfiguration
import Amazonka.CodePipeline.Types.FailureDetails
import Amazonka.CodePipeline.Types.InputArtifact
import Amazonka.CodePipeline.Types.Job
import Amazonka.CodePipeline.Types.JobData
import Amazonka.CodePipeline.Types.JobDetails
import Amazonka.CodePipeline.Types.JobWorkerExecutorConfiguration
import Amazonka.CodePipeline.Types.LambdaExecutorConfiguration
import Amazonka.CodePipeline.Types.ListWebhookItem
import Amazonka.CodePipeline.Types.OutputArtifact
import Amazonka.CodePipeline.Types.PipelineContext
import Amazonka.CodePipeline.Types.PipelineDeclaration
import Amazonka.CodePipeline.Types.PipelineExecution
import Amazonka.CodePipeline.Types.PipelineExecutionSummary
import Amazonka.CodePipeline.Types.PipelineMetadata
import Amazonka.CodePipeline.Types.PipelineSummary
import Amazonka.CodePipeline.Types.S3ArtifactLocation
import Amazonka.CodePipeline.Types.S3Location
import Amazonka.CodePipeline.Types.SourceRevision
import Amazonka.CodePipeline.Types.StageContext
import Amazonka.CodePipeline.Types.StageDeclaration
import Amazonka.CodePipeline.Types.StageExecution
import Amazonka.CodePipeline.Types.StageState
import Amazonka.CodePipeline.Types.StopExecutionTrigger
import Amazonka.CodePipeline.Types.Tag
import Amazonka.CodePipeline.Types.ThirdPartyJob
import Amazonka.CodePipeline.Types.ThirdPartyJobData
import Amazonka.CodePipeline.Types.ThirdPartyJobDetails
import Amazonka.CodePipeline.Types.TransitionState
import Amazonka.CodePipeline.Types.WebhookAuthConfiguration
import Amazonka.CodePipeline.Types.WebhookDefinition
import Amazonka.CodePipeline.Types.WebhookFilterRule
import Amazonka.CodePipeline.UntagResource
import Amazonka.CodePipeline.UpdateActionType
import Amazonka.CodePipeline.UpdatePipeline
