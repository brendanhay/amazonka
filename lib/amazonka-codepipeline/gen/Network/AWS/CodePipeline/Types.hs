{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types
    (
    -- * Service Configuration
      codePipeline

    -- * Errors
    , _InvalidClientTokenException
    , _ValidationException
    , _InvalidNonceException
    , _ActionNotFoundException
    , _InvalidApprovalTokenException
    , _InvalidBlockerDeclarationException
    , _InvalidJobStateException
    , _InvalidJobException
    , _PipelineVersionNotFoundException
    , _StageNotRetryableException
    , _PipelineExecutionNotFoundException
    , _InvalidWebhookAuthenticationParametersException
    , _WebhookNotFoundException
    , _ActionTypeNotFoundException
    , _InvalidNextTokenException
    , _InvalidStageDeclarationException
    , _InvalidWebhookFilterPatternException
    , _InvalidActionDeclarationException
    , _StageNotFoundException
    , _InvalidStructureException
    , _JobNotFoundException
    , _ApprovalAlreadyCompletedException
    , _PipelineNameInUseException
    , _PipelineNotFoundException
    , _LimitExceededException
    , _NotLatestPipelineExecutionException

    -- * ActionCategory
    , ActionCategory (..)

    -- * ActionConfigurationPropertyType
    , ActionConfigurationPropertyType (..)

    -- * ActionExecutionStatus
    , ActionExecutionStatus (..)

    -- * ActionOwner
    , ActionOwner (..)

    -- * ApprovalStatus
    , ApprovalStatus (..)

    -- * ArtifactLocationType
    , ArtifactLocationType (..)

    -- * ArtifactStoreType
    , ArtifactStoreType (..)

    -- * BlockerType
    , BlockerType (..)

    -- * EncryptionKeyType
    , EncryptionKeyType (..)

    -- * FailureType
    , FailureType (..)

    -- * JobStatus
    , JobStatus (..)

    -- * PipelineExecutionStatus
    , PipelineExecutionStatus (..)

    -- * StageExecutionStatus
    , StageExecutionStatus (..)

    -- * StageRetryMode
    , StageRetryMode (..)

    -- * StageTransitionType
    , StageTransitionType (..)

    -- * WebhookAuthenticationType
    , WebhookAuthenticationType (..)

    -- * AWSSessionCredentials
    , AWSSessionCredentials
    , awsSessionCredentials
    , ascAccessKeyId
    , ascSecretAccessKey
    , ascSessionToken

    -- * ActionConfiguration
    , ActionConfiguration
    , actionConfiguration
    , acConfiguration

    -- * ActionConfigurationProperty
    , ActionConfigurationProperty
    , actionConfigurationProperty
    , acpQueryable
    , acpType
    , acpDescription
    , acpName
    , acpRequired
    , acpKey
    , acpSecret

    -- * ActionContext
    , ActionContext
    , actionContext
    , acName

    -- * ActionDeclaration
    , ActionDeclaration
    , actionDeclaration
    , adOutputArtifacts
    , adRunOrder
    , adConfiguration
    , adInputArtifacts
    , adRoleARN
    , adName
    , adActionTypeId

    -- * ActionExecution
    , ActionExecution
    , actionExecution
    , aeLastUpdatedBy
    , aeSummary
    , aeStatus
    , aeLastStatusChange
    , aeToken
    , aeExternalExecutionURL
    , aeExternalExecutionId
    , aeErrorDetails
    , aePercentComplete

    -- * ActionRevision
    , ActionRevision
    , actionRevision
    , aRevisionId
    , aRevisionChangeId
    , aCreated

    -- * ActionState
    , ActionState
    , actionState
    , asRevisionURL
    , asEntityURL
    , asActionName
    , asCurrentRevision
    , asLatestExecution

    -- * ActionType
    , ActionType
    , actionType
    , atSettings
    , atActionConfigurationProperties
    , atId
    , atInputArtifactDetails
    , atOutputArtifactDetails

    -- * ActionTypeId
    , ActionTypeId
    , actionTypeId
    , atiCategory
    , atiOwner
    , atiProvider
    , atiVersion

    -- * ActionTypeSettings
    , ActionTypeSettings
    , actionTypeSettings
    , atsThirdPartyConfigurationURL
    , atsExecutionURLTemplate
    , atsRevisionURLTemplate
    , atsEntityURLTemplate

    -- * ApprovalResult
    , ApprovalResult
    , approvalResult
    , arSummary
    , arStatus

    -- * Artifact
    , Artifact
    , artifact
    , aLocation
    , aName
    , aRevision

    -- * ArtifactDetails
    , ArtifactDetails
    , artifactDetails
    , adMinimumCount
    , adMaximumCount

    -- * ArtifactLocation
    , ArtifactLocation
    , artifactLocation
    , alS3Location
    , alType

    -- * ArtifactRevision
    , ArtifactRevision
    , artifactRevision
    , arRevisionSummary
    , arRevisionURL
    , arCreated
    , arName
    , arRevisionId
    , arRevisionChangeIdentifier

    -- * ArtifactStore
    , ArtifactStore
    , artifactStore
    , asEncryptionKey
    , asType
    , asLocation

    -- * BlockerDeclaration
    , BlockerDeclaration
    , blockerDeclaration
    , bdName
    , bdType

    -- * CurrentRevision
    , CurrentRevision
    , currentRevision
    , crRevisionSummary
    , crCreated
    , crRevision
    , crChangeIdentifier

    -- * EncryptionKey
    , EncryptionKey
    , encryptionKey
    , ekId
    , ekType

    -- * ErrorDetails
    , ErrorDetails
    , errorDetails
    , edCode
    , edMessage

    -- * ExecutionDetails
    , ExecutionDetails
    , executionDetails
    , edSummary
    , edExternalExecutionId
    , edPercentComplete

    -- * FailureDetails
    , FailureDetails
    , failureDetails
    , fdExternalExecutionId
    , fdType
    , fdMessage

    -- * InputArtifact
    , InputArtifact
    , inputArtifact
    , iaName

    -- * Job
    , Job
    , job
    , jData
    , jAccountId
    , jId
    , jNonce

    -- * JobData
    , JobData
    , jobData
    , jdContinuationToken
    , jdOutputArtifacts
    , jdArtifactCredentials
    , jdPipelineContext
    , jdEncryptionKey
    , jdActionTypeId
    , jdInputArtifacts
    , jdActionConfiguration

    -- * JobDetails
    , JobDetails
    , jobDetails
    , jdData
    , jdAccountId
    , jdId

    -- * ListWebhookItem
    , ListWebhookItem
    , listWebhookItem
    , lwiArn
    , lwiErrorCode
    , lwiLastTriggered
    , lwiErrorMessage
    , lwiDefinition
    , lwiUrl

    -- * OutputArtifact
    , OutputArtifact
    , outputArtifact
    , oaName

    -- * PipelineContext
    , PipelineContext
    , pipelineContext
    , pcStage
    , pcPipelineName
    , pcAction

    -- * PipelineDeclaration
    , PipelineDeclaration
    , pipelineDeclaration
    , pdVersion
    , pdName
    , pdRoleARN
    , pdArtifactStore
    , pdStages

    -- * PipelineExecution
    , PipelineExecution
    , pipelineExecution
    , peStatus
    , pePipelineName
    , pePipelineVersion
    , pePipelineExecutionId
    , peArtifactRevisions

    -- * PipelineExecutionSummary
    , PipelineExecutionSummary
    , pipelineExecutionSummary
    , pesStatus
    , pesStartTime
    , pesPipelineExecutionId
    , pesSourceRevisions
    , pesLastUpdateTime

    -- * PipelineMetadata
    , PipelineMetadata
    , pipelineMetadata
    , pmCreated
    , pmPipelineARN
    , pmUpdated

    -- * PipelineSummary
    , PipelineSummary
    , pipelineSummary
    , psCreated
    , psName
    , psVersion
    , psUpdated

    -- * S3ArtifactLocation
    , S3ArtifactLocation
    , s3ArtifactLocation
    , salBucketName
    , salObjectKey

    -- * SourceRevision
    , SourceRevision
    , sourceRevision
    , srRevisionSummary
    , srRevisionURL
    , srRevisionId
    , srActionName

    -- * StageContext
    , StageContext
    , stageContext
    , scName

    -- * StageDeclaration
    , StageDeclaration
    , stageDeclaration
    , sdBlockers
    , sdName
    , sdActions

    -- * StageExecution
    , StageExecution
    , stageExecution
    , sePipelineExecutionId
    , seStatus

    -- * StageState
    , StageState
    , stageState
    , ssInboundTransitionState
    , ssActionStates
    , ssStageName
    , ssLatestExecution

    -- * ThirdPartyJob
    , ThirdPartyJob
    , thirdPartyJob
    , tpjClientId
    , tpjJobId

    -- * ThirdPartyJobData
    , ThirdPartyJobData
    , thirdPartyJobData
    , tpjdContinuationToken
    , tpjdOutputArtifacts
    , tpjdArtifactCredentials
    , tpjdPipelineContext
    , tpjdEncryptionKey
    , tpjdActionTypeId
    , tpjdInputArtifacts
    , tpjdActionConfiguration

    -- * ThirdPartyJobDetails
    , ThirdPartyJobDetails
    , thirdPartyJobDetails
    , tpjdData
    , tpjdId
    , tpjdNonce

    -- * TransitionState
    , TransitionState
    , transitionState
    , tsEnabled
    , tsDisabledReason
    , tsLastChangedAt
    , tsLastChangedBy

    -- * WebhookAuthConfiguration
    , WebhookAuthConfiguration
    , webhookAuthConfiguration
    , wacAllowedIPRange
    , wacSecretToken

    -- * WebhookDefinition
    , WebhookDefinition
    , webhookDefinition
    , wdName
    , wdTargetPipeline
    , wdTargetAction
    , wdFilters
    , wdAuthentication
    , wdAuthenticationConfiguration

    -- * WebhookFilterRule
    , WebhookFilterRule
    , webhookFilterRule
    , wfrMatchEquals
    , wfrJsonPath
    ) where

import Network.AWS.CodePipeline.Types.Product
import Network.AWS.CodePipeline.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-07-09@ of the Amazon CodePipeline SDK configuration.
codePipeline :: Service
codePipeline =
  Service
    { _svcAbbrev = "CodePipeline"
    , _svcSigner = v4
    , _svcPrefix = "codepipeline"
    , _svcVersion = "2015-07-09"
    , _svcEndpoint = defaultEndpoint codePipeline
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CodePipeline"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The client token was specified in an invalid format
--
--
_InvalidClientTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClientTokenException =
  _MatchServiceError codePipeline "InvalidClientTokenException"


-- | The validation was specified in an invalid format.
--
--
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException = _MatchServiceError codePipeline "ValidationException"


-- | The specified nonce was specified in an invalid format.
--
--
_InvalidNonceException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNonceException = _MatchServiceError codePipeline "InvalidNonceException"


-- | The specified action cannot be found.
--
--
_ActionNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ActionNotFoundException =
  _MatchServiceError codePipeline "ActionNotFoundException"


-- | The approval request already received a response or has expired.
--
--
_InvalidApprovalTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidApprovalTokenException =
  _MatchServiceError codePipeline "InvalidApprovalTokenException"


-- | Reserved for future use.
--
--
_InvalidBlockerDeclarationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBlockerDeclarationException =
  _MatchServiceError codePipeline "InvalidBlockerDeclarationException"


-- | The specified job state was specified in an invalid format.
--
--
_InvalidJobStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidJobStateException =
  _MatchServiceError codePipeline "InvalidJobStateException"


-- | The specified job was specified in an invalid format or cannot be found.
--
--
_InvalidJobException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidJobException = _MatchServiceError codePipeline "InvalidJobException"


-- | The specified pipeline version was specified in an invalid format or cannot be found.
--
--
_PipelineVersionNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PipelineVersionNotFoundException =
  _MatchServiceError codePipeline "PipelineVersionNotFoundException"


-- | The specified stage can't be retried because the pipeline structure or stage state changed after the stage was not completed; the stage contains no failed actions; one or more actions are still in progress; or another retry attempt is already in progress.
--
--
_StageNotRetryableException :: AsError a => Getting (First ServiceError) a ServiceError
_StageNotRetryableException =
  _MatchServiceError codePipeline "StageNotRetryableException"


-- | The pipeline execution was specified in an invalid format or cannot be found, or an execution ID does not belong to the specified pipeline.
--
--
_PipelineExecutionNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PipelineExecutionNotFoundException =
  _MatchServiceError codePipeline "PipelineExecutionNotFoundException"


-- | The specified authentication type is in an invalid format.
--
--
_InvalidWebhookAuthenticationParametersException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidWebhookAuthenticationParametersException =
  _MatchServiceError
    codePipeline
    "InvalidWebhookAuthenticationParametersException"


-- | The specified webhook was entered in an invalid format or cannot be found.
--
--
_WebhookNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_WebhookNotFoundException =
  _MatchServiceError codePipeline "WebhookNotFoundException"


-- | The specified action type cannot be found.
--
--
_ActionTypeNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ActionTypeNotFoundException =
  _MatchServiceError codePipeline "ActionTypeNotFoundException"


-- | The next token was specified in an invalid format. Make sure that the next token you provided is the token returned by a previous call.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError codePipeline "InvalidNextTokenException"


-- | The specified stage declaration was specified in an invalid format.
--
--
_InvalidStageDeclarationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStageDeclarationException =
  _MatchServiceError codePipeline "InvalidStageDeclarationException"


-- | The specified event filter rule is in an invalid format.
--
--
_InvalidWebhookFilterPatternException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidWebhookFilterPatternException =
  _MatchServiceError codePipeline "InvalidWebhookFilterPatternException"


-- | The specified action declaration was specified in an invalid format.
--
--
_InvalidActionDeclarationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidActionDeclarationException =
  _MatchServiceError codePipeline "InvalidActionDeclarationException"


-- | The specified stage was specified in an invalid format or cannot be found.
--
--
_StageNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_StageNotFoundException =
  _MatchServiceError codePipeline "StageNotFoundException"


-- | The specified structure was specified in an invalid format.
--
--
_InvalidStructureException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStructureException =
  _MatchServiceError codePipeline "InvalidStructureException"


-- | The specified job was specified in an invalid format or cannot be found.
--
--
_JobNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_JobNotFoundException = _MatchServiceError codePipeline "JobNotFoundException"


-- | The approval action has already been approved or rejected.
--
--
_ApprovalAlreadyCompletedException :: AsError a => Getting (First ServiceError) a ServiceError
_ApprovalAlreadyCompletedException =
  _MatchServiceError codePipeline "ApprovalAlreadyCompletedException"


-- | The specified pipeline name is already in use.
--
--
_PipelineNameInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_PipelineNameInUseException =
  _MatchServiceError codePipeline "PipelineNameInUseException"


-- | The specified pipeline was specified in an invalid format or cannot be found.
--
--
_PipelineNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PipelineNotFoundException =
  _MatchServiceError codePipeline "PipelineNotFoundException"


-- | The number of pipelines associated with the AWS account has exceeded the limit allowed for the account.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError codePipeline "LimitExceededException"


-- | The stage has failed in a later run of the pipeline and the pipelineExecutionId associated with the request is out of date.
--
--
_NotLatestPipelineExecutionException :: AsError a => Getting (First ServiceError) a ServiceError
_NotLatestPipelineExecutionException =
  _MatchServiceError codePipeline "NotLatestPipelineExecutionException"

