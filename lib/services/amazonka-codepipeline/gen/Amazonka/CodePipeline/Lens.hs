{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodePipeline.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Lens
  ( -- * Operations

    -- ** GetPipeline
    getPipeline_version,
    getPipeline_name,
    getPipelineResponse_pipeline,
    getPipelineResponse_metadata,
    getPipelineResponse_httpStatus,

    -- ** PutJobFailureResult
    putJobFailureResult_jobId,
    putJobFailureResult_failureDetails,

    -- ** PutApprovalResult
    putApprovalResult_pipelineName,
    putApprovalResult_stageName,
    putApprovalResult_actionName,
    putApprovalResult_result,
    putApprovalResult_token,
    putApprovalResultResponse_approvedAt,
    putApprovalResultResponse_httpStatus,

    -- ** AcknowledgeThirdPartyJob
    acknowledgeThirdPartyJob_jobId,
    acknowledgeThirdPartyJob_nonce,
    acknowledgeThirdPartyJob_clientToken,
    acknowledgeThirdPartyJobResponse_status,
    acknowledgeThirdPartyJobResponse_httpStatus,

    -- ** PutThirdPartyJobFailureResult
    putThirdPartyJobFailureResult_jobId,
    putThirdPartyJobFailureResult_clientToken,
    putThirdPartyJobFailureResult_failureDetails,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterWebhookWithThirdParty
    registerWebhookWithThirdParty_webhookName,
    registerWebhookWithThirdPartyResponse_httpStatus,

    -- ** PollForThirdPartyJobs
    pollForThirdPartyJobs_maxBatchSize,
    pollForThirdPartyJobs_actionTypeId,
    pollForThirdPartyJobsResponse_jobs,
    pollForThirdPartyJobsResponse_httpStatus,

    -- ** PollForJobs
    pollForJobs_maxBatchSize,
    pollForJobs_queryParam,
    pollForJobs_actionTypeId,
    pollForJobsResponse_jobs,
    pollForJobsResponse_httpStatus,

    -- ** StartPipelineExecution
    startPipelineExecution_clientRequestToken,
    startPipelineExecution_name,
    startPipelineExecutionResponse_pipelineExecutionId,
    startPipelineExecutionResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_pipeline,
    updatePipelineResponse_pipeline,
    updatePipelineResponse_httpStatus,

    -- ** DeletePipeline
    deletePipeline_name,

    -- ** GetPipelineState
    getPipelineState_name,
    getPipelineStateResponse_pipelineName,
    getPipelineStateResponse_created,
    getPipelineStateResponse_stageStates,
    getPipelineStateResponse_pipelineVersion,
    getPipelineStateResponse_updated,
    getPipelineStateResponse_httpStatus,

    -- ** GetJobDetails
    getJobDetails_jobId,
    getJobDetailsResponse_jobDetails,
    getJobDetailsResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_nextToken,
    listPipelines_maxResults,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_httpStatus,

    -- ** RetryStageExecution
    retryStageExecution_pipelineName,
    retryStageExecution_stageName,
    retryStageExecution_pipelineExecutionId,
    retryStageExecution_retryMode,
    retryStageExecutionResponse_pipelineExecutionId,
    retryStageExecutionResponse_httpStatus,

    -- ** GetPipelineExecution
    getPipelineExecution_pipelineName,
    getPipelineExecution_pipelineExecutionId,
    getPipelineExecutionResponse_pipelineExecution,
    getPipelineExecutionResponse_httpStatus,

    -- ** PutJobSuccessResult
    putJobSuccessResult_outputVariables,
    putJobSuccessResult_continuationToken,
    putJobSuccessResult_executionDetails,
    putJobSuccessResult_currentRevision,
    putJobSuccessResult_jobId,

    -- ** DeregisterWebhookWithThirdParty
    deregisterWebhookWithThirdParty_webhookName,
    deregisterWebhookWithThirdPartyResponse_httpStatus,

    -- ** DeleteCustomActionType
    deleteCustomActionType_category,
    deleteCustomActionType_provider,
    deleteCustomActionType_version,

    -- ** PutActionRevision
    putActionRevision_pipelineName,
    putActionRevision_stageName,
    putActionRevision_actionName,
    putActionRevision_actionRevision,
    putActionRevisionResponse_newRevision,
    putActionRevisionResponse_pipelineExecutionId,
    putActionRevisionResponse_httpStatus,

    -- ** DisableStageTransition
    disableStageTransition_pipelineName,
    disableStageTransition_stageName,
    disableStageTransition_transitionType,
    disableStageTransition_reason,

    -- ** UpdateActionType
    updateActionType_actionType,

    -- ** ListActionTypes
    listActionTypes_actionOwnerFilter,
    listActionTypes_regionFilter,
    listActionTypes_nextToken,
    listActionTypesResponse_nextToken,
    listActionTypesResponse_httpStatus,
    listActionTypesResponse_actionTypes,

    -- ** AcknowledgeJob
    acknowledgeJob_jobId,
    acknowledgeJob_nonce,
    acknowledgeJobResponse_status,
    acknowledgeJobResponse_httpStatus,

    -- ** EnableStageTransition
    enableStageTransition_pipelineName,
    enableStageTransition_stageName,
    enableStageTransition_transitionType,

    -- ** DeleteWebhook
    deleteWebhook_name,
    deleteWebhookResponse_httpStatus,

    -- ** PutWebhook
    putWebhook_tags,
    putWebhook_webhook,
    putWebhookResponse_webhook,
    putWebhookResponse_httpStatus,

    -- ** ListWebhooks
    listWebhooks_nextToken,
    listWebhooks_maxResults,
    listWebhooksResponse_nextToken,
    listWebhooksResponse_webhooks,
    listWebhooksResponse_httpStatus,

    -- ** ListActionExecutions
    listActionExecutions_nextToken,
    listActionExecutions_filter,
    listActionExecutions_maxResults,
    listActionExecutions_pipelineName,
    listActionExecutionsResponse_actionExecutionDetails,
    listActionExecutionsResponse_nextToken,
    listActionExecutionsResponse_httpStatus,

    -- ** GetActionType
    getActionType_category,
    getActionType_owner,
    getActionType_provider,
    getActionType_version,
    getActionTypeResponse_actionType,
    getActionTypeResponse_httpStatus,

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

    -- ** CreatePipeline
    createPipeline_tags,
    createPipeline_pipeline,
    createPipelineResponse_pipeline,
    createPipelineResponse_tags,
    createPipelineResponse_httpStatus,

    -- ** GetThirdPartyJobDetails
    getThirdPartyJobDetails_jobId,
    getThirdPartyJobDetails_clientToken,
    getThirdPartyJobDetailsResponse_jobDetails,
    getThirdPartyJobDetailsResponse_httpStatus,

    -- ** PutThirdPartyJobSuccessResult
    putThirdPartyJobSuccessResult_continuationToken,
    putThirdPartyJobSuccessResult_executionDetails,
    putThirdPartyJobSuccessResult_currentRevision,
    putThirdPartyJobSuccessResult_jobId,
    putThirdPartyJobSuccessResult_clientToken,

    -- ** CreateCustomActionType
    createCustomActionType_settings,
    createCustomActionType_configurationProperties,
    createCustomActionType_tags,
    createCustomActionType_category,
    createCustomActionType_provider,
    createCustomActionType_version,
    createCustomActionType_inputArtifactDetails,
    createCustomActionType_outputArtifactDetails,
    createCustomActionTypeResponse_tags,
    createCustomActionTypeResponse_httpStatus,
    createCustomActionTypeResponse_actionType,

    -- ** ListPipelineExecutions
    listPipelineExecutions_nextToken,
    listPipelineExecutions_maxResults,
    listPipelineExecutions_pipelineName,
    listPipelineExecutionsResponse_nextToken,
    listPipelineExecutionsResponse_pipelineExecutionSummaries,
    listPipelineExecutionsResponse_httpStatus,

    -- * Types

    -- ** AWSSessionCredentials
    aWSSessionCredentials_accessKeyId,
    aWSSessionCredentials_secretAccessKey,
    aWSSessionCredentials_sessionToken,

    -- ** ActionConfiguration
    actionConfiguration_configuration,

    -- ** ActionConfigurationProperty
    actionConfigurationProperty_queryable,
    actionConfigurationProperty_type,
    actionConfigurationProperty_description,
    actionConfigurationProperty_name,
    actionConfigurationProperty_required,
    actionConfigurationProperty_key,
    actionConfigurationProperty_secret,

    -- ** ActionContext
    actionContext_name,
    actionContext_actionExecutionId,

    -- ** ActionDeclaration
    actionDeclaration_outputArtifacts,
    actionDeclaration_namespace,
    actionDeclaration_runOrder,
    actionDeclaration_region,
    actionDeclaration_configuration,
    actionDeclaration_inputArtifacts,
    actionDeclaration_roleArn,
    actionDeclaration_name,
    actionDeclaration_actionTypeId,

    -- ** ActionExecution
    actionExecution_lastUpdatedBy,
    actionExecution_summary,
    actionExecution_status,
    actionExecution_lastStatusChange,
    actionExecution_token,
    actionExecution_externalExecutionUrl,
    actionExecution_externalExecutionId,
    actionExecution_errorDetails,
    actionExecution_percentComplete,
    actionExecution_actionExecutionId,

    -- ** ActionExecutionDetail
    actionExecutionDetail_status,
    actionExecutionDetail_startTime,
    actionExecutionDetail_pipelineVersion,
    actionExecutionDetail_input,
    actionExecutionDetail_actionName,
    actionExecutionDetail_output,
    actionExecutionDetail_pipelineExecutionId,
    actionExecutionDetail_stageName,
    actionExecutionDetail_lastUpdateTime,
    actionExecutionDetail_actionExecutionId,

    -- ** ActionExecutionFilter
    actionExecutionFilter_pipelineExecutionId,

    -- ** ActionExecutionInput
    actionExecutionInput_namespace,
    actionExecutionInput_resolvedConfiguration,
    actionExecutionInput_region,
    actionExecutionInput_configuration,
    actionExecutionInput_actionTypeId,
    actionExecutionInput_inputArtifacts,
    actionExecutionInput_roleArn,

    -- ** ActionExecutionOutput
    actionExecutionOutput_outputVariables,
    actionExecutionOutput_outputArtifacts,
    actionExecutionOutput_executionResult,

    -- ** ActionExecutionResult
    actionExecutionResult_externalExecutionUrl,
    actionExecutionResult_externalExecutionId,
    actionExecutionResult_externalExecutionSummary,

    -- ** ActionRevision
    actionRevision_revisionId,
    actionRevision_revisionChangeId,
    actionRevision_created,

    -- ** ActionState
    actionState_revisionUrl,
    actionState_entityUrl,
    actionState_actionName,
    actionState_currentRevision,
    actionState_latestExecution,

    -- ** ActionType
    actionType_settings,
    actionType_actionConfigurationProperties,
    actionType_id,
    actionType_inputArtifactDetails,
    actionType_outputArtifactDetails,

    -- ** ActionTypeArtifactDetails
    actionTypeArtifactDetails_minimumCount,
    actionTypeArtifactDetails_maximumCount,

    -- ** ActionTypeDeclaration
    actionTypeDeclaration_urls,
    actionTypeDeclaration_permissions,
    actionTypeDeclaration_description,
    actionTypeDeclaration_properties,
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
    actionTypeProperty_queryable,
    actionTypeProperty_description,
    actionTypeProperty_name,
    actionTypeProperty_optional,
    actionTypeProperty_key,
    actionTypeProperty_noEcho,

    -- ** ActionTypeSettings
    actionTypeSettings_thirdPartyConfigurationUrl,
    actionTypeSettings_executionUrlTemplate,
    actionTypeSettings_revisionUrlTemplate,
    actionTypeSettings_entityUrlTemplate,

    -- ** ActionTypeUrls
    actionTypeUrls_executionUrlTemplate,
    actionTypeUrls_revisionUrlTemplate,
    actionTypeUrls_entityUrlTemplate,
    actionTypeUrls_configurationUrl,

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
    artifactRevision_revisionSummary,
    artifactRevision_revisionUrl,
    artifactRevision_created,
    artifactRevision_name,
    artifactRevision_revisionId,
    artifactRevision_revisionChangeIdentifier,

    -- ** ArtifactStore
    artifactStore_encryptionKey,
    artifactStore_type,
    artifactStore_location,

    -- ** BlockerDeclaration
    blockerDeclaration_name,
    blockerDeclaration_type,

    -- ** CurrentRevision
    currentRevision_revisionSummary,
    currentRevision_created,
    currentRevision_revision,
    currentRevision_changeIdentifier,

    -- ** EncryptionKey
    encryptionKey_id,
    encryptionKey_type,

    -- ** ErrorDetails
    errorDetails_code,
    errorDetails_message,

    -- ** ExecutionDetails
    executionDetails_summary,
    executionDetails_externalExecutionId,
    executionDetails_percentComplete,

    -- ** ExecutionTrigger
    executionTrigger_triggerType,
    executionTrigger_triggerDetail,

    -- ** ExecutorConfiguration
    executorConfiguration_lambdaExecutorConfiguration,
    executorConfiguration_jobWorkerExecutorConfiguration,

    -- ** FailureDetails
    failureDetails_externalExecutionId,
    failureDetails_type,
    failureDetails_message,

    -- ** InputArtifact
    inputArtifact_name,

    -- ** Job
    job_data,
    job_accountId,
    job_id,
    job_nonce,

    -- ** JobData
    jobData_continuationToken,
    jobData_outputArtifacts,
    jobData_artifactCredentials,
    jobData_pipelineContext,
    jobData_encryptionKey,
    jobData_actionTypeId,
    jobData_inputArtifacts,
    jobData_actionConfiguration,

    -- ** JobDetails
    jobDetails_data,
    jobDetails_accountId,
    jobDetails_id,

    -- ** JobWorkerExecutorConfiguration
    jobWorkerExecutorConfiguration_pollingAccounts,
    jobWorkerExecutorConfiguration_pollingServicePrincipals,

    -- ** LambdaExecutorConfiguration
    lambdaExecutorConfiguration_lambdaFunctionArn,

    -- ** ListWebhookItem
    listWebhookItem_arn,
    listWebhookItem_errorCode,
    listWebhookItem_lastTriggered,
    listWebhookItem_errorMessage,
    listWebhookItem_tags,
    listWebhookItem_definition,
    listWebhookItem_url,

    -- ** OutputArtifact
    outputArtifact_name,

    -- ** PipelineContext
    pipelineContext_stage,
    pipelineContext_pipelineName,
    pipelineContext_action,
    pipelineContext_pipelineArn,
    pipelineContext_pipelineExecutionId,

    -- ** PipelineDeclaration
    pipelineDeclaration_artifactStores,
    pipelineDeclaration_artifactStore,
    pipelineDeclaration_version,
    pipelineDeclaration_name,
    pipelineDeclaration_roleArn,
    pipelineDeclaration_stages,

    -- ** PipelineExecution
    pipelineExecution_status,
    pipelineExecution_pipelineName,
    pipelineExecution_statusSummary,
    pipelineExecution_pipelineVersion,
    pipelineExecution_pipelineExecutionId,
    pipelineExecution_artifactRevisions,

    -- ** PipelineExecutionSummary
    pipelineExecutionSummary_status,
    pipelineExecutionSummary_startTime,
    pipelineExecutionSummary_stopTrigger,
    pipelineExecutionSummary_pipelineExecutionId,
    pipelineExecutionSummary_sourceRevisions,
    pipelineExecutionSummary_trigger,
    pipelineExecutionSummary_lastUpdateTime,

    -- ** PipelineMetadata
    pipelineMetadata_created,
    pipelineMetadata_pipelineArn,
    pipelineMetadata_updated,

    -- ** PipelineSummary
    pipelineSummary_created,
    pipelineSummary_name,
    pipelineSummary_version,
    pipelineSummary_updated,

    -- ** S3ArtifactLocation
    s3ArtifactLocation_bucketName,
    s3ArtifactLocation_objectKey,

    -- ** S3Location
    s3Location_bucket,
    s3Location_key,

    -- ** SourceRevision
    sourceRevision_revisionSummary,
    sourceRevision_revisionUrl,
    sourceRevision_revisionId,
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
    stageState_inboundExecution,
    stageState_inboundTransitionState,
    stageState_actionStates,
    stageState_stageName,
    stageState_latestExecution,

    -- ** StopExecutionTrigger
    stopExecutionTrigger_reason,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ThirdPartyJob
    thirdPartyJob_clientId,
    thirdPartyJob_jobId,

    -- ** ThirdPartyJobData
    thirdPartyJobData_continuationToken,
    thirdPartyJobData_outputArtifacts,
    thirdPartyJobData_artifactCredentials,
    thirdPartyJobData_pipelineContext,
    thirdPartyJobData_encryptionKey,
    thirdPartyJobData_actionTypeId,
    thirdPartyJobData_inputArtifacts,
    thirdPartyJobData_actionConfiguration,

    -- ** ThirdPartyJobDetails
    thirdPartyJobDetails_data,
    thirdPartyJobDetails_id,
    thirdPartyJobDetails_nonce,

    -- ** TransitionState
    transitionState_enabled,
    transitionState_disabledReason,
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
