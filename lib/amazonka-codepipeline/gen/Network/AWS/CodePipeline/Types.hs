{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types
  ( -- * Service Configuration
    codePipeline,

    -- * Errors

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
    AWSSessionCredentials,
    awsSessionCredentials,
    ascAccessKeyId,
    ascSecretAccessKey,
    ascSessionToken,

    -- * ActionConfiguration
    ActionConfiguration,
    actionConfiguration,
    acConfiguration,

    -- * ActionConfigurationProperty
    ActionConfigurationProperty,
    actionConfigurationProperty,
    acpQueryable,
    acpType,
    acpDescription,
    acpName,
    acpRequired,
    acpKey,
    acpSecret,

    -- * ActionContext
    ActionContext,
    actionContext,
    acName,
    acActionExecutionId,

    -- * ActionDeclaration
    ActionDeclaration,
    actionDeclaration,
    adOutputArtifacts,
    adNamespace,
    adRunOrder,
    adRegion,
    adConfiguration,
    adInputArtifacts,
    adRoleARN,
    adName,
    adActionTypeId,

    -- * ActionExecution
    ActionExecution,
    actionExecution,
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

    -- * ActionExecutionDetail
    ActionExecutionDetail,
    actionExecutionDetail,
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

    -- * ActionExecutionFilter
    ActionExecutionFilter,
    actionExecutionFilter,
    aefPipelineExecutionId,

    -- * ActionExecutionInput
    ActionExecutionInput,
    actionExecutionInput,
    aeiNamespace,
    aeiResolvedConfiguration,
    aeiRegion,
    aeiConfiguration,
    aeiActionTypeId,
    aeiInputArtifacts,
    aeiRoleARN,

    -- * ActionExecutionOutput
    ActionExecutionOutput,
    actionExecutionOutput,
    aeoOutputVariables,
    aeoOutputArtifacts,
    aeoExecutionResult,

    -- * ActionExecutionResult
    ActionExecutionResult,
    actionExecutionResult,
    aerExternalExecutionURL,
    aerExternalExecutionId,
    aerExternalExecutionSummary,

    -- * ActionRevision
    ActionRevision,
    actionRevision,
    aRevisionId,
    aRevisionChangeId,
    aCreated,

    -- * ActionState
    ActionState,
    actionState,
    asRevisionURL,
    asEntityURL,
    asActionName,
    asCurrentRevision,
    asLatestExecution,

    -- * ActionType
    ActionType,
    actionType,
    atSettings,
    atActionConfigurationProperties,
    atId,
    atInputArtifactDetails,
    atOutputArtifactDetails,

    -- * ActionTypeId
    ActionTypeId,
    actionTypeId,
    atiCategory,
    atiOwner,
    atiProvider,
    atiVersion,

    -- * ActionTypeSettings
    ActionTypeSettings,
    actionTypeSettings,
    atsThirdPartyConfigurationURL,
    atsExecutionURLTemplate,
    atsRevisionURLTemplate,
    atsEntityURLTemplate,

    -- * ApprovalResult
    ApprovalResult,
    approvalResult,
    arSummary,
    arStatus,

    -- * Artifact
    Artifact,
    artifact,
    artLocation,
    artName,
    artRevision,

    -- * ArtifactDetail
    ArtifactDetail,
    artifactDetail,
    aName,
    aS3location,

    -- * ArtifactDetails
    ArtifactDetails,
    artifactDetails,
    adMinimumCount,
    adMaximumCount,

    -- * ArtifactLocation
    ArtifactLocation,
    artifactLocation,
    alS3Location,
    alType,

    -- * ArtifactRevision
    ArtifactRevision,
    artifactRevision,
    arRevisionSummary,
    arRevisionURL,
    arCreated,
    arName,
    arRevisionId,
    arRevisionChangeIdentifier,

    -- * ArtifactStore
    ArtifactStore,
    artifactStore,
    asEncryptionKey,
    asType,
    asLocation,

    -- * BlockerDeclaration
    BlockerDeclaration,
    blockerDeclaration,
    bdName,
    bdType,

    -- * CurrentRevision
    CurrentRevision,
    currentRevision,
    crRevisionSummary,
    crCreated,
    crRevision,
    crChangeIdentifier,

    -- * EncryptionKey
    EncryptionKey,
    encryptionKey,
    ekId,
    ekType,

    -- * ErrorDetails
    ErrorDetails,
    errorDetails,
    edCode,
    edMessage,

    -- * ExecutionDetails
    ExecutionDetails,
    executionDetails,
    edSummary,
    edExternalExecutionId,
    edPercentComplete,

    -- * ExecutionTrigger
    ExecutionTrigger,
    executionTrigger,
    etTriggerType,
    etTriggerDetail,

    -- * FailureDetails
    FailureDetails,
    failureDetails,
    fdExternalExecutionId,
    fdType,
    fdMessage,

    -- * InputArtifact
    InputArtifact,
    inputArtifact,
    iaName,

    -- * Job
    Job,
    job,
    jData,
    jAccountId,
    jId,
    jNonce,

    -- * JobData
    JobData,
    jobData,
    jdContinuationToken,
    jdOutputArtifacts,
    jdArtifactCredentials,
    jdPipelineContext,
    jdEncryptionKey,
    jdActionTypeId,
    jdInputArtifacts,
    jdActionConfiguration,

    -- * JobDetails
    JobDetails,
    jobDetails,
    jdData,
    jdAccountId,
    jdId,

    -- * ListWebhookItem
    ListWebhookItem,
    listWebhookItem,
    lwiArn,
    lwiErrorCode,
    lwiLastTriggered,
    lwiErrorMessage,
    lwiTags,
    lwiDefinition,
    lwiUrl,

    -- * OutputArtifact
    OutputArtifact,
    outputArtifact,
    oaName,

    -- * PipelineContext
    PipelineContext,
    pipelineContext,
    pcStage,
    pcPipelineName,
    pcAction,
    pcPipelineARN,
    pcPipelineExecutionId,

    -- * PipelineDeclaration
    PipelineDeclaration,
    pipelineDeclaration,
    pdArtifactStores,
    pdArtifactStore,
    pdVersion,
    pdName,
    pdRoleARN,
    pdStages,

    -- * PipelineExecution
    PipelineExecution,
    pipelineExecution,
    peStatus,
    pePipelineName,
    pePipelineVersion,
    pePipelineExecutionId,
    peArtifactRevisions,

    -- * PipelineExecutionSummary
    PipelineExecutionSummary,
    pipelineExecutionSummary,
    pesStatus,
    pesStartTime,
    pesStopTrigger,
    pesPipelineExecutionId,
    pesSourceRevisions,
    pesTrigger,
    pesLastUpdateTime,

    -- * PipelineMetadata
    PipelineMetadata,
    pipelineMetadata,
    pmCreated,
    pmPipelineARN,
    pmUpdated,

    -- * PipelineSummary
    PipelineSummary,
    pipelineSummary,
    psCreated,
    psName,
    psVersion,
    psUpdated,

    -- * S3ArtifactLocation
    S3ArtifactLocation,
    s3ArtifactLocation,
    salBucketName,
    salObjectKey,

    -- * S3Location
    S3Location,
    s3Location,
    slBucket,
    slKey,

    -- * SourceRevision
    SourceRevision,
    sourceRevision,
    srRevisionSummary,
    srRevisionURL,
    srRevisionId,
    srActionName,

    -- * StageContext
    StageContext,
    stageContext,
    scName,

    -- * StageDeclaration
    StageDeclaration,
    stageDeclaration,
    sdBlockers,
    sdName,
    sdActions,

    -- * StageExecution
    StageExecution,
    stageExecution,
    sePipelineExecutionId,
    seStatus,

    -- * StageState
    StageState,
    stageState,
    ssInboundExecution,
    ssInboundTransitionState,
    ssActionStates,
    ssStageName,
    ssLatestExecution,

    -- * StopExecutionTrigger
    StopExecutionTrigger,
    stopExecutionTrigger,
    setReason,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * ThirdPartyJob
    ThirdPartyJob,
    thirdPartyJob,
    tpjClientId,
    tpjJobId,

    -- * ThirdPartyJobData
    ThirdPartyJobData,
    thirdPartyJobData,
    tpjdContinuationToken,
    tpjdOutputArtifacts,
    tpjdArtifactCredentials,
    tpjdPipelineContext,
    tpjdEncryptionKey,
    tpjdActionTypeId,
    tpjdInputArtifacts,
    tpjdActionConfiguration,

    -- * ThirdPartyJobDetails
    ThirdPartyJobDetails,
    thirdPartyJobDetails,
    tpjdData,
    tpjdId,
    tpjdNonce,

    -- * TransitionState
    TransitionState,
    transitionState,
    tsEnabled,
    tsDisabledReason,
    tsLastChangedAt,
    tsLastChangedBy,

    -- * WebhookAuthConfiguration
    WebhookAuthConfiguration,
    webhookAuthConfiguration,
    wacAllowedIPRange,
    wacSecretToken,

    -- * WebhookDefinition
    WebhookDefinition,
    webhookDefinition,
    wdName,
    wdTargetPipeline,
    wdTargetAction,
    wdFilters,
    wdAuthentication,
    wdAuthenticationConfiguration,

    -- * WebhookFilterRule
    WebhookFilterRule,
    webhookFilterRule,
    wfrMatchEquals,
    wfrJsonPath,
  )
where

import Network.AWS.CodePipeline.Types.AWSSessionCredentials
import Network.AWS.CodePipeline.Types.ActionCategory
import Network.AWS.CodePipeline.Types.ActionConfiguration
import Network.AWS.CodePipeline.Types.ActionConfigurationProperty
import Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
import Network.AWS.CodePipeline.Types.ActionContext
import Network.AWS.CodePipeline.Types.ActionDeclaration
import Network.AWS.CodePipeline.Types.ActionExecution
import Network.AWS.CodePipeline.Types.ActionExecutionDetail
import Network.AWS.CodePipeline.Types.ActionExecutionFilter
import Network.AWS.CodePipeline.Types.ActionExecutionInput
import Network.AWS.CodePipeline.Types.ActionExecutionOutput
import Network.AWS.CodePipeline.Types.ActionExecutionResult
import Network.AWS.CodePipeline.Types.ActionExecutionStatus
import Network.AWS.CodePipeline.Types.ActionOwner
import Network.AWS.CodePipeline.Types.ActionRevision
import Network.AWS.CodePipeline.Types.ActionState
import Network.AWS.CodePipeline.Types.ActionType
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.ActionTypeSettings
import Network.AWS.CodePipeline.Types.ApprovalResult
import Network.AWS.CodePipeline.Types.ApprovalStatus
import Network.AWS.CodePipeline.Types.Artifact
import Network.AWS.CodePipeline.Types.ArtifactDetail
import Network.AWS.CodePipeline.Types.ArtifactDetails
import Network.AWS.CodePipeline.Types.ArtifactLocation
import Network.AWS.CodePipeline.Types.ArtifactLocationType
import Network.AWS.CodePipeline.Types.ArtifactRevision
import Network.AWS.CodePipeline.Types.ArtifactStore
import Network.AWS.CodePipeline.Types.ArtifactStoreType
import Network.AWS.CodePipeline.Types.BlockerDeclaration
import Network.AWS.CodePipeline.Types.BlockerType
import Network.AWS.CodePipeline.Types.CurrentRevision
import Network.AWS.CodePipeline.Types.EncryptionKey
import Network.AWS.CodePipeline.Types.EncryptionKeyType
import Network.AWS.CodePipeline.Types.ErrorDetails
import Network.AWS.CodePipeline.Types.ExecutionDetails
import Network.AWS.CodePipeline.Types.ExecutionTrigger
import Network.AWS.CodePipeline.Types.FailureDetails
import Network.AWS.CodePipeline.Types.FailureType
import Network.AWS.CodePipeline.Types.InputArtifact
import Network.AWS.CodePipeline.Types.Job
import Network.AWS.CodePipeline.Types.JobData
import Network.AWS.CodePipeline.Types.JobDetails
import Network.AWS.CodePipeline.Types.JobStatus
import Network.AWS.CodePipeline.Types.ListWebhookItem
import Network.AWS.CodePipeline.Types.OutputArtifact
import Network.AWS.CodePipeline.Types.PipelineContext
import Network.AWS.CodePipeline.Types.PipelineDeclaration
import Network.AWS.CodePipeline.Types.PipelineExecution
import Network.AWS.CodePipeline.Types.PipelineExecutionStatus
import Network.AWS.CodePipeline.Types.PipelineExecutionSummary
import Network.AWS.CodePipeline.Types.PipelineMetadata
import Network.AWS.CodePipeline.Types.PipelineSummary
import Network.AWS.CodePipeline.Types.S3ArtifactLocation
import Network.AWS.CodePipeline.Types.S3Location
import Network.AWS.CodePipeline.Types.SourceRevision
import Network.AWS.CodePipeline.Types.StageContext
import Network.AWS.CodePipeline.Types.StageDeclaration
import Network.AWS.CodePipeline.Types.StageExecution
import Network.AWS.CodePipeline.Types.StageExecutionStatus
import Network.AWS.CodePipeline.Types.StageRetryMode
import Network.AWS.CodePipeline.Types.StageState
import Network.AWS.CodePipeline.Types.StageTransitionType
import Network.AWS.CodePipeline.Types.StopExecutionTrigger
import Network.AWS.CodePipeline.Types.Tag
import Network.AWS.CodePipeline.Types.ThirdPartyJob
import Network.AWS.CodePipeline.Types.ThirdPartyJobData
import Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
import Network.AWS.CodePipeline.Types.TransitionState
import Network.AWS.CodePipeline.Types.TriggerType
import Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
import Network.AWS.CodePipeline.Types.WebhookAuthenticationType
import Network.AWS.CodePipeline.Types.WebhookDefinition
import Network.AWS.CodePipeline.Types.WebhookFilterRule
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-07-09@ of the Amazon CodePipeline SDK configuration.
codePipeline :: Service
codePipeline =
  Service
    { _svcAbbrev = "CodePipeline",
      _svcSigner = v4,
      _svcPrefix = "codepipeline",
      _svcVersion = "2015-07-09",
      _svcEndpoint = defaultEndpoint codePipeline,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CodePipeline",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
