{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Lens
  ( -- * Operations

    -- ** ListActionTypes
    listActionTypes_nextToken,
    listActionTypes_regionFilter,
    listActionTypes_actionOwnerFilter,
    listActionTypesResponse_nextToken,
    listActionTypesResponse_httpStatus,
    listActionTypesResponse_actionTypes,

    -- ** DeregisterWebhookWithThirdParty
    deregisterWebhookWithThirdParty_webhookName,
    deregisterWebhookWithThirdPartyResponse_httpStatus,

    -- ** PutActionRevision
    putActionRevision_pipelineName,
    putActionRevision_stageName,
    putActionRevision_actionName,
    putActionRevision_actionRevision,
    putActionRevisionResponse_newRevision,
    putActionRevisionResponse_pipelineExecutionId,
    putActionRevisionResponse_httpStatus,

    -- ** PutJobSuccessResult
    putJobSuccessResult_executionDetails,
    putJobSuccessResult_currentRevision,
    putJobSuccessResult_outputVariables,
    putJobSuccessResult_continuationToken,
    putJobSuccessResult_jobId,

    -- ** PutThirdPartyJobSuccessResult
    putThirdPartyJobSuccessResult_executionDetails,
    putThirdPartyJobSuccessResult_currentRevision,
    putThirdPartyJobSuccessResult_continuationToken,
    putThirdPartyJobSuccessResult_jobId,
    putThirdPartyJobSuccessResult_clientToken,

    -- ** CreatePipeline
    createPipeline_tags,
    createPipeline_pipeline,
    createPipelineResponse_tags,
    createPipelineResponse_pipeline,
    createPipelineResponse_httpStatus,

    -- ** RetryStageExecution
    retryStageExecution_pipelineName,
    retryStageExecution_stageName,
    retryStageExecution_pipelineExecutionId,
    retryStageExecution_retryMode,
    retryStageExecutionResponse_pipelineExecutionId,
    retryStageExecutionResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_pipeline,
    updatePipelineResponse_pipeline,
    updatePipelineResponse_httpStatus,

    -- ** GetPipelineState
    getPipelineState_name,
    getPipelineStateResponse_stageStates,
    getPipelineStateResponse_created,
    getPipelineStateResponse_pipelineVersion,
    getPipelineStateResponse_updated,
    getPipelineStateResponse_pipelineName,
    getPipelineStateResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeletePipeline
    deletePipeline_name,

    -- ** StartPipelineExecution
    startPipelineExecution_clientRequestToken,
    startPipelineExecution_name,
    startPipelineExecutionResponse_pipelineExecutionId,
    startPipelineExecutionResponse_httpStatus,

    -- ** GetActionType
    getActionType_category,
    getActionType_owner,
    getActionType_provider,
    getActionType_version,
    getActionTypeResponse_actionType,
    getActionTypeResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** StopPipelineExecution
    stopPipelineExecution_reason,
    stopPipelineExecution_abandon,
    stopPipelineExecution_pipelineName,
    stopPipelineExecution_pipelineExecutionId,
    stopPipelineExecutionResponse_pipelineExecutionId,
    stopPipelineExecutionResponse_httpStatus,

    -- ** RegisterWebhookWithThirdParty
    registerWebhookWithThirdParty_webhookName,
    registerWebhookWithThirdPartyResponse_httpStatus,

    -- ** ListActionExecutions
    listActionExecutions_nextToken,
    listActionExecutions_maxResults,
    listActionExecutions_filter,
    listActionExecutions_pipelineName,
    listActionExecutionsResponse_nextToken,
    listActionExecutionsResponse_actionExecutionDetails,
    listActionExecutionsResponse_httpStatus,

    -- ** PollForThirdPartyJobs
    pollForThirdPartyJobs_maxBatchSize,
    pollForThirdPartyJobs_actionTypeId,
    pollForThirdPartyJobsResponse_jobs,
    pollForThirdPartyJobsResponse_httpStatus,

    -- ** EnableStageTransition
    enableStageTransition_pipelineName,
    enableStageTransition_stageName,
    enableStageTransition_transitionType,

    -- ** DeleteWebhook
    deleteWebhook_name,
    deleteWebhookResponse_httpStatus,

    -- ** AcknowledgeThirdPartyJob
    acknowledgeThirdPartyJob_jobId,
    acknowledgeThirdPartyJob_nonce,
    acknowledgeThirdPartyJob_clientToken,
    acknowledgeThirdPartyJobResponse_status,
    acknowledgeThirdPartyJobResponse_httpStatus,

    -- ** AcknowledgeJob
    acknowledgeJob_jobId,
    acknowledgeJob_nonce,
    acknowledgeJobResponse_status,
    acknowledgeJobResponse_httpStatus,

    -- ** DisableStageTransition
    disableStageTransition_pipelineName,
    disableStageTransition_stageName,
    disableStageTransition_transitionType,
    disableStageTransition_reason,

    -- ** UpdateActionType
    updateActionType_actionType,

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

    -- ** DeleteCustomActionType
    deleteCustomActionType_category,
    deleteCustomActionType_provider,
    deleteCustomActionType_version,

    -- ** GetPipeline
    getPipeline_version,
    getPipeline_name,
    getPipelineResponse_metadata,
    getPipelineResponse_pipeline,
    getPipelineResponse_httpStatus,

    -- ** CreateCustomActionType
    createCustomActionType_configurationProperties,
    createCustomActionType_tags,
    createCustomActionType_settings,
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

    -- ** GetThirdPartyJobDetails
    getThirdPartyJobDetails_jobId,
    getThirdPartyJobDetails_clientToken,
    getThirdPartyJobDetailsResponse_jobDetails,
    getThirdPartyJobDetailsResponse_httpStatus,

    -- ** GetPipelineExecution
    getPipelineExecution_pipelineName,
    getPipelineExecution_pipelineExecutionId,
    getPipelineExecutionResponse_pipelineExecution,
    getPipelineExecutionResponse_httpStatus,

    -- ** GetJobDetails
    getJobDetails_jobId,
    getJobDetailsResponse_jobDetails,
    getJobDetailsResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_nextToken,
    listPipelines_maxResults,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_httpStatus,

    -- ** PollForJobs
    pollForJobs_queryParam,
    pollForJobs_maxBatchSize,
    pollForJobs_actionTypeId,
    pollForJobsResponse_jobs,
    pollForJobsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutWebhook
    putWebhook_tags,
    putWebhook_webhook,
    putWebhookResponse_webhook,
    putWebhookResponse_httpStatus,

    -- ** PutThirdPartyJobFailureResult
    putThirdPartyJobFailureResult_jobId,
    putThirdPartyJobFailureResult_clientToken,
    putThirdPartyJobFailureResult_failureDetails,

    -- ** ListWebhooks
    listWebhooks_nextToken,
    listWebhooks_maxResults,
    listWebhooksResponse_nextToken,
    listWebhooksResponse_webhooks,
    listWebhooksResponse_httpStatus,

    -- * Types

    -- ** AWSSessionCredentials
    aWSSessionCredentials_accessKeyId,
    aWSSessionCredentials_secretAccessKey,
    aWSSessionCredentials_sessionToken,

    -- ** ActionConfiguration
    actionConfiguration_configuration,

    -- ** ActionConfigurationProperty
    actionConfigurationProperty_description,
    actionConfigurationProperty_type,
    actionConfigurationProperty_queryable,
    actionConfigurationProperty_name,
    actionConfigurationProperty_required,
    actionConfigurationProperty_key,
    actionConfigurationProperty_secret,

    -- ** ActionContext
    actionContext_actionExecutionId,
    actionContext_name,

    -- ** ActionDeclaration
    actionDeclaration_roleArn,
    actionDeclaration_configuration,
    actionDeclaration_runOrder,
    actionDeclaration_namespace,
    actionDeclaration_inputArtifacts,
    actionDeclaration_region,
    actionDeclaration_outputArtifacts,
    actionDeclaration_name,
    actionDeclaration_actionTypeId,

    -- ** ActionExecution
    actionExecution_status,
    actionExecution_actionExecutionId,
    actionExecution_lastStatusChange,
    actionExecution_percentComplete,
    actionExecution_externalExecutionId,
    actionExecution_externalExecutionUrl,
    actionExecution_lastUpdatedBy,
    actionExecution_summary,
    actionExecution_token,
    actionExecution_errorDetails,

    -- ** ActionExecutionDetail
    actionExecutionDetail_status,
    actionExecutionDetail_actionName,
    actionExecutionDetail_actionExecutionId,
    actionExecutionDetail_input,
    actionExecutionDetail_lastUpdateTime,
    actionExecutionDetail_stageName,
    actionExecutionDetail_startTime,
    actionExecutionDetail_output,
    actionExecutionDetail_pipelineVersion,
    actionExecutionDetail_pipelineExecutionId,

    -- ** ActionExecutionFilter
    actionExecutionFilter_pipelineExecutionId,

    -- ** ActionExecutionInput
    actionExecutionInput_roleArn,
    actionExecutionInput_configuration,
    actionExecutionInput_resolvedConfiguration,
    actionExecutionInput_namespace,
    actionExecutionInput_actionTypeId,
    actionExecutionInput_inputArtifacts,
    actionExecutionInput_region,

    -- ** ActionExecutionOutput
    actionExecutionOutput_executionResult,
    actionExecutionOutput_outputVariables,
    actionExecutionOutput_outputArtifacts,

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
    actionState_latestExecution,
    actionState_currentRevision,
    actionState_entityUrl,
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
    actionTypeDeclaration_permissions,
    actionTypeDeclaration_urls,
    actionTypeDeclaration_properties,
    actionTypeDeclaration_description,
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
    actionTypeProperty_description,
    actionTypeProperty_queryable,
    actionTypeProperty_name,
    actionTypeProperty_optional,
    actionTypeProperty_key,
    actionTypeProperty_noEcho,

    -- ** ActionTypeSettings
    actionTypeSettings_executionUrlTemplate,
    actionTypeSettings_entityUrlTemplate,
    actionTypeSettings_revisionUrlTemplate,
    actionTypeSettings_thirdPartyConfigurationUrl,

    -- ** ActionTypeUrls
    actionTypeUrls_executionUrlTemplate,
    actionTypeUrls_entityUrlTemplate,
    actionTypeUrls_revisionUrlTemplate,
    actionTypeUrls_configurationUrl,

    -- ** ApprovalResult
    approvalResult_summary,
    approvalResult_status,

    -- ** Artifact
    artifact_name,
    artifact_revision,
    artifact_location,

    -- ** ArtifactDetail
    artifactDetail_s3location,
    artifactDetail_name,

    -- ** ArtifactDetails
    artifactDetails_minimumCount,
    artifactDetails_maximumCount,

    -- ** ArtifactLocation
    artifactLocation_s3Location,
    artifactLocation_type,

    -- ** ArtifactRevision
    artifactRevision_revisionId,
    artifactRevision_revisionChangeIdentifier,
    artifactRevision_name,
    artifactRevision_revisionSummary,
    artifactRevision_created,
    artifactRevision_revisionUrl,

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
    errorDetails_message,
    errorDetails_code,

    -- ** ExecutionDetails
    executionDetails_percentComplete,
    executionDetails_externalExecutionId,
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
    job_nonce,
    job_data,
    job_id,

    -- ** JobData
    jobData_artifactCredentials,
    jobData_encryptionKey,
    jobData_actionConfiguration,
    jobData_actionTypeId,
    jobData_inputArtifacts,
    jobData_pipelineContext,
    jobData_continuationToken,
    jobData_outputArtifacts,

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
    listWebhookItem_lastTriggered,
    listWebhookItem_arn,
    listWebhookItem_tags,
    listWebhookItem_errorMessage,
    listWebhookItem_errorCode,
    listWebhookItem_definition,
    listWebhookItem_url,

    -- ** OutputArtifact
    outputArtifact_name,

    -- ** PipelineContext
    pipelineContext_pipelineArn,
    pipelineContext_stage,
    pipelineContext_action,
    pipelineContext_pipelineName,
    pipelineContext_pipelineExecutionId,

    -- ** PipelineDeclaration
    pipelineDeclaration_version,
    pipelineDeclaration_artifactStores,
    pipelineDeclaration_artifactStore,
    pipelineDeclaration_name,
    pipelineDeclaration_roleArn,
    pipelineDeclaration_stages,

    -- ** PipelineExecution
    pipelineExecution_status,
    pipelineExecution_artifactRevisions,
    pipelineExecution_pipelineVersion,
    pipelineExecution_statusSummary,
    pipelineExecution_pipelineName,
    pipelineExecution_pipelineExecutionId,

    -- ** PipelineExecutionSummary
    pipelineExecutionSummary_status,
    pipelineExecutionSummary_lastUpdateTime,
    pipelineExecutionSummary_trigger,
    pipelineExecutionSummary_startTime,
    pipelineExecutionSummary_stopTrigger,
    pipelineExecutionSummary_sourceRevisions,
    pipelineExecutionSummary_pipelineExecutionId,

    -- ** PipelineMetadata
    pipelineMetadata_pipelineArn,
    pipelineMetadata_created,
    pipelineMetadata_updated,

    -- ** PipelineSummary
    pipelineSummary_version,
    pipelineSummary_name,
    pipelineSummary_created,
    pipelineSummary_updated,

    -- ** S3ArtifactLocation
    s3ArtifactLocation_bucketName,
    s3ArtifactLocation_objectKey,

    -- ** S3Location
    s3Location_key,
    s3Location_bucket,

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
    stageState_inboundExecution,
    stageState_latestExecution,
    stageState_stageName,
    stageState_inboundTransitionState,
    stageState_actionStates,

    -- ** StopExecutionTrigger
    stopExecutionTrigger_reason,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ThirdPartyJob
    thirdPartyJob_clientId,
    thirdPartyJob_jobId,

    -- ** ThirdPartyJobData
    thirdPartyJobData_artifactCredentials,
    thirdPartyJobData_encryptionKey,
    thirdPartyJobData_actionConfiguration,
    thirdPartyJobData_actionTypeId,
    thirdPartyJobData_inputArtifacts,
    thirdPartyJobData_pipelineContext,
    thirdPartyJobData_continuationToken,
    thirdPartyJobData_outputArtifacts,

    -- ** ThirdPartyJobDetails
    thirdPartyJobDetails_nonce,
    thirdPartyJobDetails_data,
    thirdPartyJobDetails_id,

    -- ** TransitionState
    transitionState_lastChangedBy,
    transitionState_enabled,
    transitionState_disabledReason,
    transitionState_lastChangedAt,

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
import Network.AWS.CodePipeline.Types.AWSSessionCredentials
import Network.AWS.CodePipeline.Types.ActionConfiguration
import Network.AWS.CodePipeline.Types.ActionConfigurationProperty
import Network.AWS.CodePipeline.Types.ActionContext
import Network.AWS.CodePipeline.Types.ActionDeclaration
import Network.AWS.CodePipeline.Types.ActionExecution
import Network.AWS.CodePipeline.Types.ActionExecutionDetail
import Network.AWS.CodePipeline.Types.ActionExecutionFilter
import Network.AWS.CodePipeline.Types.ActionExecutionInput
import Network.AWS.CodePipeline.Types.ActionExecutionOutput
import Network.AWS.CodePipeline.Types.ActionExecutionResult
import Network.AWS.CodePipeline.Types.ActionRevision
import Network.AWS.CodePipeline.Types.ActionState
import Network.AWS.CodePipeline.Types.ActionType
import Network.AWS.CodePipeline.Types.ActionTypeArtifactDetails
import Network.AWS.CodePipeline.Types.ActionTypeDeclaration
import Network.AWS.CodePipeline.Types.ActionTypeExecutor
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.ActionTypeIdentifier
import Network.AWS.CodePipeline.Types.ActionTypePermissions
import Network.AWS.CodePipeline.Types.ActionTypeProperty
import Network.AWS.CodePipeline.Types.ActionTypeSettings
import Network.AWS.CodePipeline.Types.ActionTypeUrls
import Network.AWS.CodePipeline.Types.ApprovalResult
import Network.AWS.CodePipeline.Types.Artifact
import Network.AWS.CodePipeline.Types.ArtifactDetail
import Network.AWS.CodePipeline.Types.ArtifactDetails
import Network.AWS.CodePipeline.Types.ArtifactLocation
import Network.AWS.CodePipeline.Types.ArtifactRevision
import Network.AWS.CodePipeline.Types.ArtifactStore
import Network.AWS.CodePipeline.Types.BlockerDeclaration
import Network.AWS.CodePipeline.Types.CurrentRevision
import Network.AWS.CodePipeline.Types.EncryptionKey
import Network.AWS.CodePipeline.Types.ErrorDetails
import Network.AWS.CodePipeline.Types.ExecutionDetails
import Network.AWS.CodePipeline.Types.ExecutionTrigger
import Network.AWS.CodePipeline.Types.ExecutorConfiguration
import Network.AWS.CodePipeline.Types.FailureDetails
import Network.AWS.CodePipeline.Types.InputArtifact
import Network.AWS.CodePipeline.Types.Job
import Network.AWS.CodePipeline.Types.JobData
import Network.AWS.CodePipeline.Types.JobDetails
import Network.AWS.CodePipeline.Types.JobWorkerExecutorConfiguration
import Network.AWS.CodePipeline.Types.LambdaExecutorConfiguration
import Network.AWS.CodePipeline.Types.ListWebhookItem
import Network.AWS.CodePipeline.Types.OutputArtifact
import Network.AWS.CodePipeline.Types.PipelineContext
import Network.AWS.CodePipeline.Types.PipelineDeclaration
import Network.AWS.CodePipeline.Types.PipelineExecution
import Network.AWS.CodePipeline.Types.PipelineExecutionSummary
import Network.AWS.CodePipeline.Types.PipelineMetadata
import Network.AWS.CodePipeline.Types.PipelineSummary
import Network.AWS.CodePipeline.Types.S3ArtifactLocation
import Network.AWS.CodePipeline.Types.S3Location
import Network.AWS.CodePipeline.Types.SourceRevision
import Network.AWS.CodePipeline.Types.StageContext
import Network.AWS.CodePipeline.Types.StageDeclaration
import Network.AWS.CodePipeline.Types.StageExecution
import Network.AWS.CodePipeline.Types.StageState
import Network.AWS.CodePipeline.Types.StopExecutionTrigger
import Network.AWS.CodePipeline.Types.Tag
import Network.AWS.CodePipeline.Types.ThirdPartyJob
import Network.AWS.CodePipeline.Types.ThirdPartyJobData
import Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
import Network.AWS.CodePipeline.Types.TransitionState
import Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
import Network.AWS.CodePipeline.Types.WebhookDefinition
import Network.AWS.CodePipeline.Types.WebhookFilterRule
import Network.AWS.CodePipeline.UntagResource
import Network.AWS.CodePipeline.UpdateActionType
import Network.AWS.CodePipeline.UpdatePipeline
