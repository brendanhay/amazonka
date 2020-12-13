-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types
  ( -- * Service configuration
    codePipelineService,

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
    AWSSessionCredentials (..),
    mkAWSSessionCredentials,
    ascSecretAccessKey,
    ascSessionToken,
    ascAccessKeyId,

    -- * ActionConfiguration
    ActionConfiguration (..),
    mkActionConfiguration,
    acConfiguration,

    -- * ActionConfigurationProperty
    ActionConfigurationProperty (..),
    mkActionConfigurationProperty,
    acpQueryable,
    acpSecret,
    acpRequired,
    acpKey,
    acpName,
    acpType,
    acpDescription,

    -- * ActionContext
    ActionContext (..),
    mkActionContext,
    acName,
    acActionExecutionId,

    -- * ActionDeclaration
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

    -- * ActionExecution
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

    -- * ActionExecutionDetail
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

    -- * ActionExecutionFilter
    ActionExecutionFilter (..),
    mkActionExecutionFilter,
    aefPipelineExecutionId,

    -- * ActionExecutionInput
    ActionExecutionInput (..),
    mkActionExecutionInput,
    aeiNamespace,
    aeiResolvedConfiguration,
    aeiRegion,
    aeiConfiguration,
    aeiActionTypeId,
    aeiInputArtifacts,
    aeiRoleARN,

    -- * ActionExecutionOutput
    ActionExecutionOutput (..),
    mkActionExecutionOutput,
    aeoOutputVariables,
    aeoOutputArtifacts,
    aeoExecutionResult,

    -- * ActionExecutionResult
    ActionExecutionResult (..),
    mkActionExecutionResult,
    aerExternalExecutionURL,
    aerExternalExecutionId,
    aerExternalExecutionSummary,

    -- * ActionRevision
    ActionRevision (..),
    mkActionRevision,
    aCreated,
    aRevisionChangeId,
    aRevisionId,

    -- * ActionState
    ActionState (..),
    mkActionState,
    asRevisionURL,
    asEntityURL,
    asActionName,
    asCurrentRevision,
    asLatestExecution,

    -- * ActionType
    ActionType (..),
    mkActionType,
    atSettings,
    atOutputArtifactDetails,
    atActionConfigurationProperties,
    atInputArtifactDetails,
    atId,

    -- * ActionTypeId
    ActionTypeId (..),
    mkActionTypeId,
    atiCategory,
    atiOwner,
    atiVersion,
    atiProvider,

    -- * ActionTypeSettings
    ActionTypeSettings (..),
    mkActionTypeSettings,
    atsThirdPartyConfigurationURL,
    atsExecutionURLTemplate,
    atsRevisionURLTemplate,
    atsEntityURLTemplate,

    -- * ApprovalResult
    ApprovalResult (..),
    mkApprovalResult,
    arSummary,
    arStatus,

    -- * Artifact
    Artifact (..),
    mkArtifact,
    afLocation,
    afName,
    afRevision,

    -- * ArtifactDetail
    ArtifactDetail (..),
    mkArtifactDetail,
    aName,
    aS3location,

    -- * ArtifactDetails
    ArtifactDetails (..),
    mkArtifactDetails,
    adMaximumCount,
    adMinimumCount,

    -- * ArtifactLocation
    ArtifactLocation (..),
    mkArtifactLocation,
    alS3Location,
    alType,

    -- * ArtifactRevision
    ArtifactRevision (..),
    mkArtifactRevision,
    arRevisionSummary,
    arRevisionURL,
    arCreated,
    arName,
    arRevisionId,
    arRevisionChangeIdentifier,

    -- * ArtifactStore
    ArtifactStore (..),
    mkArtifactStore,
    asLocation,
    asEncryptionKey,
    asType,

    -- * BlockerDeclaration
    BlockerDeclaration (..),
    mkBlockerDeclaration,
    bdName,
    bdType,

    -- * CurrentRevision
    CurrentRevision (..),
    mkCurrentRevision,
    crRevisionSummary,
    crCreated,
    crChangeIdentifier,
    crRevision,

    -- * EncryptionKey
    EncryptionKey (..),
    mkEncryptionKey,
    ekId,
    ekType,

    -- * ErrorDetails
    ErrorDetails (..),
    mkErrorDetails,
    edCode,
    edMessage,

    -- * ExecutionDetails
    ExecutionDetails (..),
    mkExecutionDetails,
    edSummary,
    edExternalExecutionId,
    edPercentComplete,

    -- * ExecutionTrigger
    ExecutionTrigger (..),
    mkExecutionTrigger,
    etTriggerType,
    etTriggerDetail,

    -- * FailureDetails
    FailureDetails (..),
    mkFailureDetails,
    fdExternalExecutionId,
    fdType,
    fdMessage,

    -- * InputArtifact
    InputArtifact (..),
    mkInputArtifact,
    iaName,

    -- * Job
    Job (..),
    mkJob,
    jData,
    jAccountId,
    jId,
    jNonce,

    -- * JobData
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

    -- * JobDetails
    JobDetails (..),
    mkJobDetails,
    jdData,
    jdAccountId,
    jdId,

    -- * ListWebhookItem
    ListWebhookItem (..),
    mkListWebhookItem,
    lwiArn,
    lwiDefinition,
    lwiUrl,
    lwiErrorCode,
    lwiLastTriggered,
    lwiErrorMessage,
    lwiTags,

    -- * OutputArtifact
    OutputArtifact (..),
    mkOutputArtifact,
    oaName,

    -- * PipelineContext
    PipelineContext (..),
    mkPipelineContext,
    pcStage,
    pcPipelineName,
    pcAction,
    pcPipelineARN,
    pcPipelineExecutionId,

    -- * PipelineDeclaration
    PipelineDeclaration (..),
    mkPipelineDeclaration,
    pdArtifactStores,
    pdArtifactStore,
    pdName,
    pdStages,
    pdVersion,
    pdRoleARN,

    -- * PipelineExecution
    PipelineExecution (..),
    mkPipelineExecution,
    peStatus,
    pePipelineName,
    pePipelineVersion,
    pePipelineExecutionId,
    peArtifactRevisions,

    -- * PipelineExecutionSummary
    PipelineExecutionSummary (..),
    mkPipelineExecutionSummary,
    pesStatus,
    pesStartTime,
    pesStopTrigger,
    pesPipelineExecutionId,
    pesSourceRevisions,
    pesTrigger,
    pesLastUpdateTime,

    -- * PipelineMetadata
    PipelineMetadata (..),
    mkPipelineMetadata,
    pmCreated,
    pmPipelineARN,
    pmUpdated,

    -- * PipelineSummary
    PipelineSummary (..),
    mkPipelineSummary,
    psCreated,
    psName,
    psVersion,
    psUpdated,

    -- * S3ArtifactLocation
    S3ArtifactLocation (..),
    mkS3ArtifactLocation,
    salBucketName,
    salObjectKey,

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,

    -- * SourceRevision
    SourceRevision (..),
    mkSourceRevision,
    srRevisionSummary,
    srRevisionURL,
    srActionName,
    srRevisionId,

    -- * StageContext
    StageContext (..),
    mkStageContext,
    scName,

    -- * StageDeclaration
    StageDeclaration (..),
    mkStageDeclaration,
    sdActions,
    sdBlockers,
    sdName,

    -- * StageExecution
    StageExecution (..),
    mkStageExecution,
    seStatus,
    sePipelineExecutionId,

    -- * StageState
    StageState (..),
    mkStageState,
    ssInboundExecution,
    ssInboundTransitionState,
    ssActionStates,
    ssStageName,
    ssLatestExecution,

    -- * StopExecutionTrigger
    StopExecutionTrigger (..),
    mkStopExecutionTrigger,
    setReason,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * ThirdPartyJob
    ThirdPartyJob (..),
    mkThirdPartyJob,
    tpjClientId,
    tpjJobId,

    -- * ThirdPartyJobData
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

    -- * ThirdPartyJobDetails
    ThirdPartyJobDetails (..),
    mkThirdPartyJobDetails,
    tpjdData,
    tpjdId,
    tpjdNonce,

    -- * TransitionState
    TransitionState (..),
    mkTransitionState,
    tsEnabled,
    tsDisabledReason,
    tsLastChangedAt,
    tsLastChangedBy,

    -- * WebhookAuthConfiguration
    WebhookAuthConfiguration (..),
    mkWebhookAuthConfiguration,
    wacAllowedIPRange,
    wacSecretToken,

    -- * WebhookDefinition
    WebhookDefinition (..),
    mkWebhookDefinition,
    wdTargetAction,
    wdAuthentication,
    wdFilters,
    wdTargetPipeline,
    wdName,
    wdAuthenticationConfiguration,

    -- * WebhookFilterRule
    WebhookFilterRule (..),
    mkWebhookFilterRule,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-07-09@ of the Amazon CodePipeline SDK configuration.
codePipelineService :: Lude.Service
codePipelineService =
  Lude.Service
    { Lude._svcAbbrev = "CodePipeline",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "codepipeline",
      Lude._svcVersion = "2015-07-09",
      Lude._svcEndpoint = Lude.defaultEndpoint codePipelineService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CodePipeline",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
