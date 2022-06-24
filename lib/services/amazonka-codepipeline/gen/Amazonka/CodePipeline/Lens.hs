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
    createCustomActionType_tags,
    createCustomActionType_settings,
    createCustomActionType_configurationProperties,
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
    createPipelineResponse_tags,
    createPipelineResponse_pipeline,
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
    getPipelineStateResponse_stageStates,
    getPipelineStateResponse_updated,
    getPipelineStateResponse_created,
    getPipelineStateResponse_pipelineVersion,
    getPipelineStateResponse_pipelineName,
    getPipelineStateResponse_httpStatus,

    -- ** GetThirdPartyJobDetails
    getThirdPartyJobDetails_jobId,
    getThirdPartyJobDetails_clientToken,
    getThirdPartyJobDetailsResponse_jobDetails,
    getThirdPartyJobDetailsResponse_httpStatus,

    -- ** ListActionExecutions
    listActionExecutions_nextToken,
    listActionExecutions_filter,
    listActionExecutions_maxResults,
    listActionExecutions_pipelineName,
    listActionExecutionsResponse_nextToken,
    listActionExecutionsResponse_actionExecutionDetails,
    listActionExecutionsResponse_httpStatus,

    -- ** ListActionTypes
    listActionTypes_nextToken,
    listActionTypes_regionFilter,
    listActionTypes_actionOwnerFilter,
    listActionTypesResponse_nextToken,
    listActionTypesResponse_httpStatus,
    listActionTypesResponse_actionTypes,

    -- ** ListPipelineExecutions
    listPipelineExecutions_nextToken,
    listPipelineExecutions_maxResults,
    listPipelineExecutions_pipelineName,
    listPipelineExecutionsResponse_nextToken,
    listPipelineExecutionsResponse_pipelineExecutionSummaries,
    listPipelineExecutionsResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_nextToken,
    listPipelines_maxResults,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWebhooks
    listWebhooks_nextToken,
    listWebhooks_maxResults,
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
    putJobSuccessResult_executionDetails,
    putJobSuccessResult_continuationToken,
    putJobSuccessResult_currentRevision,
    putJobSuccessResult_outputVariables,
    putJobSuccessResult_jobId,

    -- ** PutThirdPartyJobFailureResult
    putThirdPartyJobFailureResult_jobId,
    putThirdPartyJobFailureResult_clientToken,
    putThirdPartyJobFailureResult_failureDetails,

    -- ** PutThirdPartyJobSuccessResult
    putThirdPartyJobSuccessResult_executionDetails,
    putThirdPartyJobSuccessResult_continuationToken,
    putThirdPartyJobSuccessResult_currentRevision,
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
    stopPipelineExecution_reason,
    stopPipelineExecution_abandon,
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
    actionConfigurationProperty_type,
    actionConfigurationProperty_queryable,
    actionConfigurationProperty_description,
    actionConfigurationProperty_name,
    actionConfigurationProperty_required,
    actionConfigurationProperty_key,
    actionConfigurationProperty_secret,

    -- ** ActionContext
    actionContext_name,
    actionContext_actionExecutionId,

    -- ** ActionDeclaration
    actionDeclaration_roleArn,
    actionDeclaration_runOrder,
    actionDeclaration_configuration,
    actionDeclaration_outputArtifacts,
    actionDeclaration_region,
    actionDeclaration_inputArtifacts,
    actionDeclaration_namespace,
    actionDeclaration_name,
    actionDeclaration_actionTypeId,

    -- ** ActionExecution
    actionExecution_errorDetails,
    actionExecution_actionExecutionId,
    actionExecution_summary,
    actionExecution_status,
    actionExecution_lastStatusChange,
    actionExecution_externalExecutionId,
    actionExecution_percentComplete,
    actionExecution_externalExecutionUrl,
    actionExecution_lastUpdatedBy,
    actionExecution_token,

    -- ** ActionExecutionDetail
    actionExecutionDetail_actionName,
    actionExecutionDetail_stageName,
    actionExecutionDetail_pipelineExecutionId,
    actionExecutionDetail_actionExecutionId,
    actionExecutionDetail_status,
    actionExecutionDetail_input,
    actionExecutionDetail_pipelineVersion,
    actionExecutionDetail_output,
    actionExecutionDetail_lastUpdateTime,
    actionExecutionDetail_startTime,

    -- ** ActionExecutionFilter
    actionExecutionFilter_pipelineExecutionId,

    -- ** ActionExecutionInput
    actionExecutionInput_resolvedConfiguration,
    actionExecutionInput_roleArn,
    actionExecutionInput_actionTypeId,
    actionExecutionInput_configuration,
    actionExecutionInput_region,
    actionExecutionInput_inputArtifacts,
    actionExecutionInput_namespace,

    -- ** ActionExecutionOutput
    actionExecutionOutput_outputArtifacts,
    actionExecutionOutput_executionResult,
    actionExecutionOutput_outputVariables,

    -- ** ActionExecutionResult
    actionExecutionResult_externalExecutionId,
    actionExecutionResult_externalExecutionUrl,
    actionExecutionResult_externalExecutionSummary,

    -- ** ActionRevision
    actionRevision_revisionId,
    actionRevision_revisionChangeId,
    actionRevision_created,

    -- ** ActionState
    actionState_actionName,
    actionState_entityUrl,
    actionState_revisionUrl,
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
    actionTypeDeclaration_properties,
    actionTypeDeclaration_permissions,
    actionTypeDeclaration_description,
    actionTypeDeclaration_urls,
    actionTypeDeclaration_executor,
    actionTypeDeclaration_id,
    actionTypeDeclaration_inputArtifactDetails,
    actionTypeDeclaration_outputArtifactDetails,

    -- ** ActionTypeExecutor
    actionTypeExecutor_policyStatementsTemplate,
    actionTypeExecutor_jobTimeout,
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
    actionTypeSettings_revisionUrlTemplate,
    actionTypeSettings_entityUrlTemplate,
    actionTypeSettings_executionUrlTemplate,

    -- ** ActionTypeUrls
    actionTypeUrls_configurationUrl,
    actionTypeUrls_revisionUrlTemplate,
    actionTypeUrls_entityUrlTemplate,
    actionTypeUrls_executionUrlTemplate,

    -- ** ApprovalResult
    approvalResult_summary,
    approvalResult_status,

    -- ** Artifact
    artifact_name,
    artifact_revision,
    artifact_location,

    -- ** ArtifactDetail
    artifactDetail_name,
    artifactDetail_s3location,

    -- ** ArtifactDetails
    artifactDetails_minimumCount,
    artifactDetails_maximumCount,

    -- ** ArtifactLocation
    artifactLocation_type,
    artifactLocation_s3Location,

    -- ** ArtifactRevision
    artifactRevision_revisionChangeIdentifier,
    artifactRevision_name,
    artifactRevision_created,
    artifactRevision_revisionId,
    artifactRevision_revisionUrl,
    artifactRevision_revisionSummary,

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
    errorDetails_message,
    errorDetails_code,

    -- ** ExecutionDetails
    executionDetails_summary,
    executionDetails_externalExecutionId,
    executionDetails_percentComplete,

    -- ** ExecutionTrigger
    executionTrigger_triggerType,
    executionTrigger_triggerDetail,

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
    job_nonce,
    job_id,
    job_accountId,
    job_data,

    -- ** JobData
    jobData_actionTypeId,
    jobData_outputArtifacts,
    jobData_artifactCredentials,
    jobData_continuationToken,
    jobData_actionConfiguration,
    jobData_inputArtifacts,
    jobData_encryptionKey,
    jobData_pipelineContext,

    -- ** JobDetails
    jobDetails_id,
    jobDetails_accountId,
    jobDetails_data,

    -- ** JobWorkerExecutorConfiguration
    jobWorkerExecutorConfiguration_pollingAccounts,
    jobWorkerExecutorConfiguration_pollingServicePrincipals,

    -- ** LambdaExecutorConfiguration
    lambdaExecutorConfiguration_lambdaFunctionArn,

    -- ** ListWebhookItem
    listWebhookItem_tags,
    listWebhookItem_errorMessage,
    listWebhookItem_arn,
    listWebhookItem_lastTriggered,
    listWebhookItem_errorCode,
    listWebhookItem_definition,
    listWebhookItem_url,

    -- ** OutputArtifact
    outputArtifact_name,

    -- ** PipelineContext
    pipelineContext_pipelineExecutionId,
    pipelineContext_pipelineArn,
    pipelineContext_pipelineName,
    pipelineContext_stage,
    pipelineContext_action,

    -- ** PipelineDeclaration
    pipelineDeclaration_artifactStores,
    pipelineDeclaration_artifactStore,
    pipelineDeclaration_version,
    pipelineDeclaration_name,
    pipelineDeclaration_roleArn,
    pipelineDeclaration_stages,

    -- ** PipelineExecution
    pipelineExecution_pipelineExecutionId,
    pipelineExecution_artifactRevisions,
    pipelineExecution_status,
    pipelineExecution_pipelineVersion,
    pipelineExecution_statusSummary,
    pipelineExecution_pipelineName,

    -- ** PipelineExecutionSummary
    pipelineExecutionSummary_trigger,
    pipelineExecutionSummary_pipelineExecutionId,
    pipelineExecutionSummary_status,
    pipelineExecutionSummary_sourceRevisions,
    pipelineExecutionSummary_stopTrigger,
    pipelineExecutionSummary_lastUpdateTime,
    pipelineExecutionSummary_startTime,

    -- ** PipelineMetadata
    pipelineMetadata_updated,
    pipelineMetadata_created,
    pipelineMetadata_pipelineArn,

    -- ** PipelineSummary
    pipelineSummary_name,
    pipelineSummary_updated,
    pipelineSummary_created,
    pipelineSummary_version,

    -- ** S3ArtifactLocation
    s3ArtifactLocation_bucketName,
    s3ArtifactLocation_objectKey,

    -- ** S3Location
    s3Location_key,
    s3Location_bucket,

    -- ** SourceRevision
    sourceRevision_revisionId,
    sourceRevision_revisionUrl,
    sourceRevision_revisionSummary,
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
    stageState_stageName,
    stageState_inboundExecution,
    stageState_actionStates,
    stageState_inboundTransitionState,
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
    thirdPartyJobData_actionTypeId,
    thirdPartyJobData_outputArtifacts,
    thirdPartyJobData_artifactCredentials,
    thirdPartyJobData_continuationToken,
    thirdPartyJobData_actionConfiguration,
    thirdPartyJobData_inputArtifacts,
    thirdPartyJobData_encryptionKey,
    thirdPartyJobData_pipelineContext,

    -- ** ThirdPartyJobDetails
    thirdPartyJobDetails_nonce,
    thirdPartyJobDetails_id,
    thirdPartyJobDetails_data,

    -- ** TransitionState
    transitionState_lastChangedAt,
    transitionState_disabledReason,
    transitionState_enabled,
    transitionState_lastChangedBy,

    -- ** WebhookAuthConfiguration
    webhookAuthConfiguration_secretToken,
    webhookAuthConfiguration_allowedIPRange,

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
