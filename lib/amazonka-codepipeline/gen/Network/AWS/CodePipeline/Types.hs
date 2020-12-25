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
    mkServiceConfig,

    -- * Errors
    _InvalidClientTokenException,
    _ValidationException,
    _InvalidNonceException,
    _ActionNotFoundException,
    _InvalidApprovalTokenException,
    _PipelineExecutionNotStoppableException,
    _InvalidBlockerDeclarationException,
    _OutputVariablesSizeExceededException,
    _InvalidJobStateException,
    _TooManyTagsException,
    _ConflictException,
    _InvalidJobException,
    _PipelineVersionNotFoundException,
    _StageNotRetryableException,
    _PipelineExecutionNotFoundException,
    _InvalidWebhookAuthenticationParametersException,
    _WebhookNotFoundException,
    _InvalidTagsException,
    _ActionTypeNotFoundException,
    _ConcurrentModificationException,
    _InvalidNextTokenException,
    _InvalidStageDeclarationException,
    _DuplicatedStopRequestException,
    _InvalidWebhookFilterPatternException,
    _InvalidActionDeclarationException,
    _StageNotFoundException,
    _InvalidStructureException,
    _JobNotFoundException,
    _ApprovalAlreadyCompletedException,
    _InvalidArnException,
    _PipelineNameInUseException,
    _PipelineNotFoundException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _NotLatestPipelineExecutionException,

    -- * PipelineExecutionStatus
    PipelineExecutionStatus (..),

    -- * LastUpdatedBy
    LastUpdatedBy (..),

    -- * ListWebhookItem
    ListWebhookItem (..),
    mkListWebhookItem,
    lwiDefinition,
    lwiUrl,
    lwiArn,
    lwiErrorCode,
    lwiErrorMessage,
    lwiLastTriggered,
    lwiTags,

    -- * ApprovalToken
    ApprovalToken (..),

    -- * ActionExecution
    ActionExecution (..),
    mkActionExecution,
    aeActionExecutionId,
    aeErrorDetails,
    aeExternalExecutionId,
    aeExternalExecutionUrl,
    aeLastStatusChange,
    aeLastUpdatedBy,
    aePercentComplete,
    aeStatus,
    aeSummary,
    aeToken,

    -- * WebhookName
    WebhookName (..),

    -- * StopPipelineExecutionReason
    StopPipelineExecutionReason (..),

    -- * ArtifactDetails
    ArtifactDetails (..),
    mkArtifactDetails,
    adMinimumCount,
    adMaximumCount,

    -- * MatchEquals
    MatchEquals (..),

    -- * PipelineExecutionSummary
    PipelineExecutionSummary (..),
    mkPipelineExecutionSummary,
    pesLastUpdateTime,
    pesPipelineExecutionId,
    pesSourceRevisions,
    pesStartTime,
    pesStatus,
    pesStopTrigger,
    pesTrigger,

    -- * ClientId
    ClientId (..),

    -- * StageDeclaration
    StageDeclaration (..),
    mkStageDeclaration,
    sdName,
    sdActions,
    sdBlockers,

    -- * ArtifactStoreLocation
    ArtifactStoreLocation (..),

    -- * ArtifactDetail
    ArtifactDetail (..),
    mkArtifactDetail,
    aName,
    aS3location,

    -- * ActionProvider
    ActionProvider (..),

    -- * StageRetryMode
    StageRetryMode (..),

    -- * RevisionSummary
    RevisionSummary (..),

    -- * ExecutionId
    ExecutionId (..),

    -- * PipelineMetadata
    PipelineMetadata (..),
    mkPipelineMetadata,
    pmCreated,
    pmPipelineArn,
    pmUpdated,

    -- * S3Key
    S3Key (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ThirdPartyJobData
    ThirdPartyJobData (..),
    mkThirdPartyJobData,
    tpjdActionConfiguration,
    tpjdActionTypeId,
    tpjdArtifactCredentials,
    tpjdContinuationToken,
    tpjdEncryptionKey,
    tpjdInputArtifacts,
    tpjdOutputArtifacts,
    tpjdPipelineContext,

    -- * ExecutionTrigger
    ExecutionTrigger (..),
    mkExecutionTrigger,
    etTriggerDetail,
    etTriggerType,

    -- * S3ObjectKey
    S3ObjectKey (..),

    -- * ThirdPartyJob
    ThirdPartyJob (..),
    mkThirdPartyJob,
    tpjClientId,
    tpjJobId,

    -- * FailureDetails
    FailureDetails (..),
    mkFailureDetails,
    fdType,
    fdMessage,
    fdExternalExecutionId,

    -- * ClientToken
    ClientToken (..),

    -- * JobId
    JobId (..),

    -- * ContinuationToken
    ContinuationToken (..),

    -- * AWSRegionName
    AWSRegionName (..),

    -- * ActionRevision
    ActionRevision (..),
    mkActionRevision,
    aRevisionId,
    aRevisionChangeId,
    aCreated,

    -- * PipelineName
    PipelineName (..),

    -- * ActionConfigurationValue
    ActionConfigurationValue (..),

    -- * SecretAccessKey
    SecretAccessKey (..),

    -- * String
    String (..),

    -- * ActionConfigurationQueryableValue
    ActionConfigurationQueryableValue (..),

    -- * SessionToken
    SessionToken (..),

    -- * EncryptionKeyId
    EncryptionKeyId (..),

    -- * ArtifactRevision
    ArtifactRevision (..),
    mkArtifactRevision,
    arCreated,
    arName,
    arRevisionChangeIdentifier,
    arRevisionId,
    arRevisionSummary,
    arRevisionUrl,

    -- * ActionExecutionResult
    ActionExecutionResult (..),
    mkActionExecutionResult,
    aerExternalExecutionId,
    aerExternalExecutionSummary,
    aerExternalExecutionUrl,

    -- * JsonPath
    JsonPath (..),

    -- * WebhookAuthConfigurationAllowedIPRange
    WebhookAuthConfigurationAllowedIPRange (..),

    -- * FailureType
    FailureType (..),

    -- * ActionExecutionStatus
    ActionExecutionStatus (..),

    -- * PipelineExecution
    PipelineExecution (..),
    mkPipelineExecution,
    peArtifactRevisions,
    pePipelineExecutionId,
    pePipelineName,
    pePipelineVersion,
    peStatus,

    -- * WebhookUrl
    WebhookUrl (..),

    -- * EncryptionKeyType
    EncryptionKeyType (..),

    -- * ExecutionDetails
    ExecutionDetails (..),
    mkExecutionDetails,
    edExternalExecutionId,
    edPercentComplete,
    edSummary,

    -- * InputArtifact
    InputArtifact (..),
    mkInputArtifact,
    iaName,

    -- * TriggerType
    TriggerType (..),

    -- * S3ArtifactLocation
    S3ArtifactLocation (..),
    mkS3ArtifactLocation,
    salBucketName,
    salObjectKey,

    -- * WebhookAuthenticationType
    WebhookAuthenticationType (..),

    -- * ActionTypeSettings
    ActionTypeSettings (..),
    mkActionTypeSettings,
    atsEntityUrlTemplate,
    atsExecutionUrlTemplate,
    atsRevisionUrlTemplate,
    atsThirdPartyConfigurationUrl,

    -- * ApprovalSummary
    ApprovalSummary (..),

    -- * StageState
    StageState (..),
    mkStageState,
    ssActionStates,
    ssInboundExecution,
    ssInboundTransitionState,
    ssLatestExecution,
    ssStageName,

    -- * Url
    Url (..),

    -- * StageContext
    StageContext (..),
    mkStageContext,
    scName,

    -- * ArtifactLocationType
    ArtifactLocationType (..),

    -- * AWSSessionCredentials
    AWSSessionCredentials (..),
    mkAWSSessionCredentials,
    awsscAccessKeyId,
    awsscSecretAccessKey,
    awsscSessionToken,

    -- * ApprovalResult
    ApprovalResult (..),
    mkApprovalResult,
    arSummary,
    arStatus,

    -- * BlockerDeclaration
    BlockerDeclaration (..),
    mkBlockerDeclaration,
    bdName,
    bdType,

    -- * StageExecution
    StageExecution (..),
    mkStageExecution,
    sePipelineExecutionId,
    seStatus,

    -- * ActionCategory
    ActionCategory (..),

    -- * ActionDeclaration
    ActionDeclaration (..),
    mkActionDeclaration,
    adName,
    adActionTypeId,
    adConfiguration,
    adInputArtifacts,
    adNamespace,
    adOutputArtifacts,
    adRegion,
    adRoleArn,
    adRunOrder,

    -- * PipelineSummary
    PipelineSummary (..),
    mkPipelineSummary,
    psCreated,
    psName,
    psUpdated,
    psVersion,

    -- * Artifact
    Artifact (..),
    mkArtifact,
    afLocation,
    afName,
    afRevision,

    -- * WebhookDefinition
    WebhookDefinition (..),
    mkWebhookDefinition,
    wdName,
    wdTargetPipeline,
    wdTargetAction,
    wdFilters,
    wdAuthentication,
    wdAuthenticationConfiguration,

    -- * WebhookAuthConfigurationSecretToken
    WebhookAuthConfigurationSecretToken (..),

    -- * ArtifactStore
    ArtifactStore (..),
    mkArtifactStore,
    asType,
    asLocation,
    asEncryptionKey,

    -- * ActionConfigurationProperty
    ActionConfigurationProperty (..),
    mkActionConfigurationProperty,
    acpName,
    acpRequired,
    acpKey,
    acpSecret,
    acpDescription,
    acpQueryable,
    acpType,

    -- * ArtifactName
    ArtifactName (..),

    -- * TriggerDetail
    TriggerDetail (..),

    -- * OutputVariablesValue
    OutputVariablesValue (..),

    -- * OutputArtifact
    OutputArtifact (..),
    mkOutputArtifact,
    oaName,

    -- * ActionName
    ActionName (..),

    -- * AccountId
    AccountId (..),

    -- * NextToken
    NextToken (..),

    -- * DisabledReason
    DisabledReason (..),

    -- * ActionExecutionFilter
    ActionExecutionFilter (..),
    mkActionExecutionFilter,
    aefPipelineExecutionId,

    -- * JobData
    JobData (..),
    mkJobData,
    jdActionConfiguration,
    jdActionTypeId,
    jdArtifactCredentials,
    jdContinuationToken,
    jdEncryptionKey,
    jdInputArtifacts,
    jdOutputArtifacts,
    jdPipelineContext,

    -- * Job
    Job (..),
    mkJob,
    jAccountId,
    jData,
    jId,
    jNonce,

    -- * ResourceArn
    ResourceArn (..),

    -- * ActionOwner
    ActionOwner (..),

    -- * ThirdPartyJobId
    ThirdPartyJobId (..),

    -- * PipelineArn
    PipelineArn (..),

    -- * ExternalExecutionId
    ExternalExecutionId (..),

    -- * CurrentRevision
    CurrentRevision (..),
    mkCurrentRevision,
    crRevision,
    crChangeIdentifier,
    crCreated,
    crRevisionSummary,

    -- * StopExecutionTrigger
    StopExecutionTrigger (..),
    mkStopExecutionTrigger,
    setReason,

    -- * Version
    Version (..),

    -- * PipelineDeclaration
    PipelineDeclaration (..),
    mkPipelineDeclaration,
    pdName,
    pdRoleArn,
    pdStages,
    pdArtifactStore,
    pdArtifactStores,
    pdVersion,

    -- * ActionExecutionInput
    ActionExecutionInput (..),
    mkActionExecutionInput,
    aeiActionTypeId,
    aeiConfiguration,
    aeiInputArtifacts,
    aeiNamespace,
    aeiRegion,
    aeiResolvedConfiguration,
    aeiRoleArn,

    -- * ErrorDetails
    ErrorDetails (..),
    mkErrorDetails,
    edCode,
    edMessage,

    -- * StageExecutionStatus
    StageExecutionStatus (..),

    -- * OutputVariablesKey
    OutputVariablesKey (..),

    -- * PipelineContext
    PipelineContext (..),
    mkPipelineContext,
    pcAction,
    pcPipelineArn,
    pcPipelineExecutionId,
    pcPipelineName,
    pcStage,

    -- * WebhookFilterRule
    WebhookFilterRule (..),
    mkWebhookFilterRule,
    wfrJsonPath,
    wfrMatchEquals,

    -- * JobDetails
    JobDetails (..),
    mkJobDetails,
    jdAccountId,
    jdData,
    jdId,

    -- * ActionExecutionDetail
    ActionExecutionDetail (..),
    mkActionExecutionDetail,
    aedActionExecutionId,
    aedActionName,
    aedInput,
    aedLastUpdateTime,
    aedOutput,
    aedPipelineExecutionId,
    aedPipelineVersion,
    aedStageName,
    aedStartTime,
    aedStatus,

    -- * PipelineExecutionId
    PipelineExecutionId (..),

    -- * Code
    Code (..),

    -- * TagKey
    TagKey (..),

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,

    -- * TransitionState
    TransitionState (..),
    mkTransitionState,
    tsDisabledReason,
    tsEnabled,
    tsLastChangedAt,
    tsLastChangedBy,

    -- * EncryptionKey
    EncryptionKey (..),
    mkEncryptionKey,
    ekId,
    ekType,

    -- * ThirdPartyJobDetails
    ThirdPartyJobDetails (..),
    mkThirdPartyJobDetails,
    tpjdData,
    tpjdId,
    tpjdNonce,

    -- * JobStatus
    JobStatus (..),

    -- * StageName
    StageName (..),

    -- * StageTransitionType
    StageTransitionType (..),

    -- * LastChangedBy
    LastChangedBy (..),

    -- * ExternalExecutionSummary
    ExternalExecutionSummary (..),

    -- * WebhookAuthConfiguration
    WebhookAuthConfiguration (..),
    mkWebhookAuthConfiguration,
    wacAllowedIPRange,
    wacSecretToken,

    -- * Revision
    Revision (..),

    -- * ArtifactLocation
    ArtifactLocation (..),
    mkArtifactLocation,
    alS3Location,
    alType,

    -- * ApprovalStatus
    ApprovalStatus (..),

    -- * Message
    Message (..),

    -- * ActionTypeId
    ActionTypeId (..),
    mkActionTypeId,
    atiCategory,
    atiOwner,
    atiProvider,
    atiVersion,

    -- * ClientRequestToken
    ClientRequestToken (..),

    -- * ActionExecutionOutput
    ActionExecutionOutput (..),
    mkActionExecutionOutput,
    aeoExecutionResult,
    aeoOutputArtifacts,
    aeoOutputVariables,

    -- * Description
    Description (..),

    -- * ActionType
    ActionType (..),
    mkActionType,
    atId,
    atInputArtifactDetails,
    atOutputArtifactDetails,
    atActionConfigurationProperties,
    atSettings,

    -- * BlockerType
    BlockerType (..),

    -- * ActionConfiguration
    ActionConfiguration (..),
    mkActionConfiguration,
    acConfiguration,

    -- * RevisionChangeIdentifier
    RevisionChangeIdentifier (..),

    -- * AccessKeyId
    AccessKeyId (..),

    -- * SourceRevision
    SourceRevision (..),
    mkSourceRevision,
    srActionName,
    srRevisionId,
    srRevisionSummary,
    srRevisionUrl,

    -- * Nonce
    Nonce (..),

    -- * ArtifactStoreType
    ArtifactStoreType (..),

    -- * ActionConfigurationPropertyType
    ActionConfigurationPropertyType (..),

    -- * ActionState
    ActionState (..),
    mkActionState,
    asActionName,
    asCurrentRevision,
    asEntityUrl,
    asLatestExecution,
    asRevisionUrl,

    -- * ActionConfigurationKey
    ActionConfigurationKey (..),

    -- * ActionContext
    ActionContext (..),
    mkActionContext,
    acActionExecutionId,
    acName,

    -- * ActionExecutionId
    ActionExecutionId (..),

    -- * RoleArn
    RoleArn (..),

    -- * Arn
    Arn (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * ErrorMessage
    ErrorMessage (..),

    -- * ExternalExecutionUrl
    ExternalExecutionUrl (..),

    -- * Summary
    Summary (..),

    -- * Token
    Token (..),

    -- * Name
    Name (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * RevisionId
    RevisionId (..),

    -- * RevisionChangeId
    RevisionChangeId (..),

    -- * RevisionUrl
    RevisionUrl (..),

    -- * BucketName
    BucketName (..),

    -- * EntityUrlTemplate
    EntityUrlTemplate (..),

    -- * ExecutionUrlTemplate
    ExecutionUrlTemplate (..),

    -- * RevisionUrlTemplate
    RevisionUrlTemplate (..),

    -- * ThirdPartyConfigurationUrl
    ThirdPartyConfigurationUrl (..),

    -- * Namespace
    Namespace (..),

    -- * TargetAction
    TargetAction (..),

    -- * ChangeIdentifier
    ChangeIdentifier (..),

    -- * Bucket
    Bucket (..),
  )
where

import Network.AWS.CodePipeline.Types.AWSRegionName
import Network.AWS.CodePipeline.Types.AWSSessionCredentials
import Network.AWS.CodePipeline.Types.AccessKeyId
import Network.AWS.CodePipeline.Types.AccountId
import Network.AWS.CodePipeline.Types.ActionCategory
import Network.AWS.CodePipeline.Types.ActionConfiguration
import Network.AWS.CodePipeline.Types.ActionConfigurationKey
import Network.AWS.CodePipeline.Types.ActionConfigurationProperty
import Network.AWS.CodePipeline.Types.ActionConfigurationPropertyType
import Network.AWS.CodePipeline.Types.ActionConfigurationQueryableValue
import Network.AWS.CodePipeline.Types.ActionConfigurationValue
import Network.AWS.CodePipeline.Types.ActionContext
import Network.AWS.CodePipeline.Types.ActionDeclaration
import Network.AWS.CodePipeline.Types.ActionExecution
import Network.AWS.CodePipeline.Types.ActionExecutionDetail
import Network.AWS.CodePipeline.Types.ActionExecutionFilter
import Network.AWS.CodePipeline.Types.ActionExecutionId
import Network.AWS.CodePipeline.Types.ActionExecutionInput
import Network.AWS.CodePipeline.Types.ActionExecutionOutput
import Network.AWS.CodePipeline.Types.ActionExecutionResult
import Network.AWS.CodePipeline.Types.ActionExecutionStatus
import Network.AWS.CodePipeline.Types.ActionName
import Network.AWS.CodePipeline.Types.ActionOwner
import Network.AWS.CodePipeline.Types.ActionProvider
import Network.AWS.CodePipeline.Types.ActionRevision
import Network.AWS.CodePipeline.Types.ActionState
import Network.AWS.CodePipeline.Types.ActionType
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.ActionTypeSettings
import Network.AWS.CodePipeline.Types.ApprovalResult
import Network.AWS.CodePipeline.Types.ApprovalStatus
import Network.AWS.CodePipeline.Types.ApprovalSummary
import Network.AWS.CodePipeline.Types.ApprovalToken
import Network.AWS.CodePipeline.Types.Arn
import Network.AWS.CodePipeline.Types.Artifact
import Network.AWS.CodePipeline.Types.ArtifactDetail
import Network.AWS.CodePipeline.Types.ArtifactDetails
import Network.AWS.CodePipeline.Types.ArtifactLocation
import Network.AWS.CodePipeline.Types.ArtifactLocationType
import Network.AWS.CodePipeline.Types.ArtifactName
import Network.AWS.CodePipeline.Types.ArtifactRevision
import Network.AWS.CodePipeline.Types.ArtifactStore
import Network.AWS.CodePipeline.Types.ArtifactStoreLocation
import Network.AWS.CodePipeline.Types.ArtifactStoreType
import Network.AWS.CodePipeline.Types.BlockerDeclaration
import Network.AWS.CodePipeline.Types.BlockerType
import Network.AWS.CodePipeline.Types.Bucket
import Network.AWS.CodePipeline.Types.BucketName
import Network.AWS.CodePipeline.Types.ChangeIdentifier
import Network.AWS.CodePipeline.Types.ClientId
import Network.AWS.CodePipeline.Types.ClientRequestToken
import Network.AWS.CodePipeline.Types.ClientToken
import Network.AWS.CodePipeline.Types.Code
import Network.AWS.CodePipeline.Types.ContinuationToken
import Network.AWS.CodePipeline.Types.CurrentRevision
import Network.AWS.CodePipeline.Types.Description
import Network.AWS.CodePipeline.Types.DisabledReason
import Network.AWS.CodePipeline.Types.EncryptionKey
import Network.AWS.CodePipeline.Types.EncryptionKeyId
import Network.AWS.CodePipeline.Types.EncryptionKeyType
import Network.AWS.CodePipeline.Types.EntityUrlTemplate
import Network.AWS.CodePipeline.Types.ErrorCode
import Network.AWS.CodePipeline.Types.ErrorDetails
import Network.AWS.CodePipeline.Types.ErrorMessage
import Network.AWS.CodePipeline.Types.ExecutionDetails
import Network.AWS.CodePipeline.Types.ExecutionId
import Network.AWS.CodePipeline.Types.ExecutionTrigger
import Network.AWS.CodePipeline.Types.ExecutionUrlTemplate
import Network.AWS.CodePipeline.Types.ExternalExecutionId
import Network.AWS.CodePipeline.Types.ExternalExecutionSummary
import Network.AWS.CodePipeline.Types.ExternalExecutionUrl
import Network.AWS.CodePipeline.Types.FailureDetails
import Network.AWS.CodePipeline.Types.FailureType
import Network.AWS.CodePipeline.Types.InputArtifact
import Network.AWS.CodePipeline.Types.Job
import Network.AWS.CodePipeline.Types.JobData
import Network.AWS.CodePipeline.Types.JobDetails
import Network.AWS.CodePipeline.Types.JobId
import Network.AWS.CodePipeline.Types.JobStatus
import Network.AWS.CodePipeline.Types.JsonPath
import Network.AWS.CodePipeline.Types.Key
import Network.AWS.CodePipeline.Types.LastChangedBy
import Network.AWS.CodePipeline.Types.LastUpdatedBy
import Network.AWS.CodePipeline.Types.ListWebhookItem
import Network.AWS.CodePipeline.Types.MatchEquals
import Network.AWS.CodePipeline.Types.Message
import Network.AWS.CodePipeline.Types.Name
import Network.AWS.CodePipeline.Types.Namespace
import Network.AWS.CodePipeline.Types.NextToken
import Network.AWS.CodePipeline.Types.Nonce
import Network.AWS.CodePipeline.Types.OutputArtifact
import Network.AWS.CodePipeline.Types.OutputVariablesKey
import Network.AWS.CodePipeline.Types.OutputVariablesValue
import Network.AWS.CodePipeline.Types.PipelineArn
import Network.AWS.CodePipeline.Types.PipelineContext
import Network.AWS.CodePipeline.Types.PipelineDeclaration
import Network.AWS.CodePipeline.Types.PipelineExecution
import Network.AWS.CodePipeline.Types.PipelineExecutionId
import Network.AWS.CodePipeline.Types.PipelineExecutionStatus
import Network.AWS.CodePipeline.Types.PipelineExecutionSummary
import Network.AWS.CodePipeline.Types.PipelineMetadata
import Network.AWS.CodePipeline.Types.PipelineName
import Network.AWS.CodePipeline.Types.PipelineSummary
import Network.AWS.CodePipeline.Types.ResourceArn
import Network.AWS.CodePipeline.Types.Revision
import Network.AWS.CodePipeline.Types.RevisionChangeId
import Network.AWS.CodePipeline.Types.RevisionChangeIdentifier
import Network.AWS.CodePipeline.Types.RevisionId
import Network.AWS.CodePipeline.Types.RevisionSummary
import Network.AWS.CodePipeline.Types.RevisionUrl
import Network.AWS.CodePipeline.Types.RevisionUrlTemplate
import Network.AWS.CodePipeline.Types.RoleArn
import Network.AWS.CodePipeline.Types.S3ArtifactLocation
import Network.AWS.CodePipeline.Types.S3Key
import Network.AWS.CodePipeline.Types.S3Location
import Network.AWS.CodePipeline.Types.S3ObjectKey
import Network.AWS.CodePipeline.Types.SecretAccessKey
import Network.AWS.CodePipeline.Types.SessionToken
import Network.AWS.CodePipeline.Types.SourceRevision
import Network.AWS.CodePipeline.Types.StageContext
import Network.AWS.CodePipeline.Types.StageDeclaration
import Network.AWS.CodePipeline.Types.StageExecution
import Network.AWS.CodePipeline.Types.StageExecutionStatus
import Network.AWS.CodePipeline.Types.StageName
import Network.AWS.CodePipeline.Types.StageRetryMode
import Network.AWS.CodePipeline.Types.StageState
import Network.AWS.CodePipeline.Types.StageTransitionType
import Network.AWS.CodePipeline.Types.StopExecutionTrigger
import Network.AWS.CodePipeline.Types.StopPipelineExecutionReason
import Network.AWS.CodePipeline.Types.String
import Network.AWS.CodePipeline.Types.Summary
import Network.AWS.CodePipeline.Types.Tag
import Network.AWS.CodePipeline.Types.TagKey
import Network.AWS.CodePipeline.Types.TargetAction
import Network.AWS.CodePipeline.Types.ThirdPartyConfigurationUrl
import Network.AWS.CodePipeline.Types.ThirdPartyJob
import Network.AWS.CodePipeline.Types.ThirdPartyJobData
import Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
import Network.AWS.CodePipeline.Types.ThirdPartyJobId
import Network.AWS.CodePipeline.Types.Token
import Network.AWS.CodePipeline.Types.TransitionState
import Network.AWS.CodePipeline.Types.TriggerDetail
import Network.AWS.CodePipeline.Types.TriggerType
import Network.AWS.CodePipeline.Types.Url
import Network.AWS.CodePipeline.Types.Value
import Network.AWS.CodePipeline.Types.Version
import Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
import Network.AWS.CodePipeline.Types.WebhookAuthConfigurationAllowedIPRange
import Network.AWS.CodePipeline.Types.WebhookAuthConfigurationSecretToken
import Network.AWS.CodePipeline.Types.WebhookAuthenticationType
import Network.AWS.CodePipeline.Types.WebhookDefinition
import Network.AWS.CodePipeline.Types.WebhookFilterRule
import Network.AWS.CodePipeline.Types.WebhookName
import Network.AWS.CodePipeline.Types.WebhookUrl
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-07-09@ of the Amazon CodePipeline SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CodePipeline",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "codepipeline",
      Core._svcVersion = "2015-07-09",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CodePipeline",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The client token was specified in an invalid format
_InvalidClientTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClientTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidClientTokenException"
{-# DEPRECATED _InvalidClientTokenException "Use generic-lens or generic-optics instead." #-}

-- | The validation was specified in an invalid format.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError mkServiceConfig "ValidationException"
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead." #-}

-- | The nonce was specified in an invalid format.
_InvalidNonceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNonceException =
  Core._MatchServiceError mkServiceConfig "InvalidNonceException"
{-# DEPRECATED _InvalidNonceException "Use generic-lens or generic-optics instead." #-}

-- | The specified action cannot be found.
_ActionNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ActionNotFoundException =
  Core._MatchServiceError mkServiceConfig "ActionNotFoundException"
{-# DEPRECATED _ActionNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The approval request already received a response or has expired.
_InvalidApprovalTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidApprovalTokenException"
{-# DEPRECATED _InvalidApprovalTokenException "Use generic-lens or generic-optics instead." #-}

-- | Unable to stop the pipeline execution. The execution might already be in a @Stopped@ state, or it might no longer be in progress.
_PipelineExecutionNotStoppableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PipelineExecutionNotStoppableException =
  Core._MatchServiceError
    mkServiceConfig
    "PipelineExecutionNotStoppableException"
{-# DEPRECATED _PipelineExecutionNotStoppableException "Use generic-lens or generic-optics instead." #-}

-- | Reserved for future use.
_InvalidBlockerDeclarationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidBlockerDeclarationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidBlockerDeclarationException"
{-# DEPRECATED _InvalidBlockerDeclarationException "Use generic-lens or generic-optics instead." #-}

-- | Exceeded the total size limit for all variables in the pipeline.
_OutputVariablesSizeExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OutputVariablesSizeExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "OutputVariablesSizeExceededException"
{-# DEPRECATED _OutputVariablesSizeExceededException "Use generic-lens or generic-optics instead." #-}

-- | The job state was specified in an invalid format.
_InvalidJobStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidJobStateException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidJobStateException"
{-# DEPRECATED _InvalidJobStateException "Use generic-lens or generic-optics instead." #-}

-- | The tags limit for a resource has been exceeded.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead." #-}

-- | Your request cannot be handled because the pipeline is busy handling ongoing activities. Try again later.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError mkServiceConfig "ConflictException"
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead." #-}

-- | The job was specified in an invalid format or cannot be found.
_InvalidJobException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidJobException =
  Core._MatchServiceError mkServiceConfig "InvalidJobException"
{-# DEPRECATED _InvalidJobException "Use generic-lens or generic-optics instead." #-}

-- | The pipeline version was specified in an invalid format or cannot be found.
_PipelineVersionNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PipelineVersionNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "PipelineVersionNotFoundException"
{-# DEPRECATED _PipelineVersionNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Unable to retry. The pipeline structure or stage state might have changed while actions awaited retry, or the stage contains no failed actions.
_StageNotRetryableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StageNotRetryableException =
  Core._MatchServiceError
    mkServiceConfig
    "StageNotRetryableException"
{-# DEPRECATED _StageNotRetryableException "Use generic-lens or generic-optics instead." #-}

-- | The pipeline execution was specified in an invalid format or cannot be found, or an execution ID does not belong to the specified pipeline.
_PipelineExecutionNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PipelineExecutionNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "PipelineExecutionNotFoundException"
{-# DEPRECATED _PipelineExecutionNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified authentication type is in an invalid format.
_InvalidWebhookAuthenticationParametersException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidWebhookAuthenticationParametersException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidWebhookAuthenticationParametersException"
{-# DEPRECATED _InvalidWebhookAuthenticationParametersException "Use generic-lens or generic-optics instead." #-}

-- | The specified webhook was entered in an invalid format or cannot be found.
_WebhookNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_WebhookNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "WebhookNotFoundException"
{-# DEPRECATED _WebhookNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource tags are invalid.
_InvalidTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagsException =
  Core._MatchServiceError mkServiceConfig "InvalidTagsException"
{-# DEPRECATED _InvalidTagsException "Use generic-lens or generic-optics instead." #-}

-- | The specified action type cannot be found.
_ActionTypeNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ActionTypeNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ActionTypeNotFoundException"
{-# DEPRECATED _ActionTypeNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Unable to modify the tag due to a simultaneous update request.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentModificationException"
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | The next token was specified in an invalid format. Make sure that the next token you provide is the token returned by a previous call.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidNextTokenException"
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | The stage declaration was specified in an invalid format.
_InvalidStageDeclarationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStageDeclarationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidStageDeclarationException"
{-# DEPRECATED _InvalidStageDeclarationException "Use generic-lens or generic-optics instead." #-}

-- | The pipeline execution is already in a @Stopping@ state. If you already chose to stop and wait, you cannot make that request again. You can choose to stop and abandon now, but be aware that this option can lead to failed tasks or out of sequence tasks. If you already chose to stop and abandon, you cannot make that request again.
_DuplicatedStopRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicatedStopRequestException =
  Core._MatchServiceError
    mkServiceConfig
    "DuplicatedStopRequestException"
{-# DEPRECATED _DuplicatedStopRequestException "Use generic-lens or generic-optics instead." #-}

-- | The specified event filter rule is in an invalid format.
_InvalidWebhookFilterPatternException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidWebhookFilterPatternException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidWebhookFilterPatternException"
{-# DEPRECATED _InvalidWebhookFilterPatternException "Use generic-lens or generic-optics instead." #-}

-- | The action declaration was specified in an invalid format.
_InvalidActionDeclarationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidActionDeclarationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidActionDeclarationException"
{-# DEPRECATED _InvalidActionDeclarationException "Use generic-lens or generic-optics instead." #-}

-- | The stage was specified in an invalid format or cannot be found.
_StageNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StageNotFoundException =
  Core._MatchServiceError mkServiceConfig "StageNotFoundException"
{-# DEPRECATED _StageNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The structure was specified in an invalid format.
_InvalidStructureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStructureException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidStructureException"
{-# DEPRECATED _InvalidStructureException "Use generic-lens or generic-optics instead." #-}

-- | The job was specified in an invalid format or cannot be found.
_JobNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_JobNotFoundException =
  Core._MatchServiceError mkServiceConfig "JobNotFoundException"
{-# DEPRECATED _JobNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The approval action has already been approved or rejected.
_ApprovalAlreadyCompletedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalAlreadyCompletedException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalAlreadyCompletedException"
{-# DEPRECATED _ApprovalAlreadyCompletedException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource ARN is invalid.
_InvalidArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError mkServiceConfig "InvalidArnException"
{-# DEPRECATED _InvalidArnException "Use generic-lens or generic-optics instead." #-}

-- | The specified pipeline name is already in use.
_PipelineNameInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PipelineNameInUseException =
  Core._MatchServiceError
    mkServiceConfig
    "PipelineNameInUseException"
{-# DEPRECATED _PipelineNameInUseException "Use generic-lens or generic-optics instead." #-}

-- | The pipeline was specified in an invalid format or cannot be found.
_PipelineNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PipelineNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "PipelineNotFoundException"
{-# DEPRECATED _PipelineNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The resource was specified in an invalid format.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The number of pipelines associated with the AWS account has exceeded the limit allowed for the account.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The stage has failed in a later run of the pipeline and the pipelineExecutionId associated with the request is out of date.
_NotLatestPipelineExecutionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotLatestPipelineExecutionException =
  Core._MatchServiceError
    mkServiceConfig
    "NotLatestPipelineExecutionException"
{-# DEPRECATED _NotLatestPipelineExecutionException "Use generic-lens or generic-optics instead." #-}
