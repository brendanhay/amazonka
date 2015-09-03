{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , _InvalidBlockerDeclarationException
    , _InvalidJobStateException
    , _InvalidJobException
    , _PipelineVersionNotFoundException
    , _ActionTypeNotFoundException
    , _InvalidNextTokenException
    , _InvalidStageDeclarationException
    , _InvalidActionDeclarationException
    , _StageNotFoundException
    , _InvalidStructureException
    , _JobNotFoundException
    , _PipelineNameInUseException
    , _PipelineNotFoundException
    , _LimitExceededException

    -- * ActionCategory
    , ActionCategory (..)

    -- * ActionConfigurationPropertyType
    , ActionConfigurationPropertyType (..)

    -- * ActionExecutionStatus
    , ActionExecutionStatus (..)

    -- * ActionOwner
    , ActionOwner (..)

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

    -- * StageTransitionType
    , StageTransitionType (..)

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
    , aeSummary
    , aeStatus
    , aeLastStatusChange
    , aeExternalExecutionURL
    , aeExternalExecutionId
    , aeErrorDetails
    , aePercentComplete

    -- * ActionRevision
    , ActionRevision
    , actionRevision
    , arRevisionChangeId
    , arRevisionId
    , arCreated

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

    -- * StageState
    , StageState
    , stageState
    , ssInboundTransitionState
    , ssActionStates
    , ssStageName

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
    ) where

import           Network.AWS.CodePipeline.Types.Product
import           Network.AWS.CodePipeline.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-07-09' of the Amazon CodePipeline SDK configuration.
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
    , _svcError = parseJSONError
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
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The client token was specified in an invalid format
_InvalidClientTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClientTokenException =
    _ServiceError . hasCode "InvalidClientTokenException"

-- | The validation was specified in an invalid format.
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException = _ServiceError . hasCode "ValidationException"

-- | The specified nonce was specified in an invalid format.
_InvalidNonceException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNonceException = _ServiceError . hasCode "InvalidNonceException"

-- | The specified action cannot be found.
_ActionNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ActionNotFoundException = _ServiceError . hasCode "ActionNotFoundException"

-- | The specified gate declaration was specified in an invalid format.
_InvalidBlockerDeclarationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBlockerDeclarationException =
    _ServiceError . hasCode "InvalidBlockerDeclarationException"

-- | The specified job state was specified in an invalid format.
_InvalidJobStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidJobStateException = _ServiceError . hasCode "InvalidJobStateException"

-- | The specified job was specified in an invalid format or cannot be found.
_InvalidJobException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidJobException = _ServiceError . hasCode "InvalidJobException"

-- | The specified pipeline version was specified in an invalid format or
-- cannot be found.
_PipelineVersionNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PipelineVersionNotFoundException =
    _ServiceError . hasCode "PipelineVersionNotFoundException"

-- | The specified action type cannot be found.
_ActionTypeNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ActionTypeNotFoundException =
    _ServiceError . hasCode "ActionTypeNotFoundException"

-- | The next token was specified in an invalid format. Make sure that the
-- next token you provided is the token returned by a previous call.
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
    _ServiceError . hasCode "InvalidNextTokenException"

-- | The specified stage declaration was specified in an invalid format.
_InvalidStageDeclarationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStageDeclarationException =
    _ServiceError . hasCode "InvalidStageDeclarationException"

-- | The specified action declaration was specified in an invalid format.
_InvalidActionDeclarationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidActionDeclarationException =
    _ServiceError . hasCode "InvalidActionDeclarationException"

-- | The specified stage was specified in an invalid format or cannot be
-- found.
_StageNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_StageNotFoundException = _ServiceError . hasCode "StageNotFoundException"

-- | The specified structure was specified in an invalid format.
_InvalidStructureException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStructureException =
    _ServiceError . hasCode "InvalidStructureException"

-- | The specified job was specified in an invalid format or cannot be found.
_JobNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_JobNotFoundException = _ServiceError . hasCode "JobNotFoundException"

-- | The specified pipeline name is already in use.
_PipelineNameInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_PipelineNameInUseException =
    _ServiceError . hasCode "PipelineNameInUseException"

-- | The specified pipeline was specified in an invalid format or cannot be
-- found.
_PipelineNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PipelineNotFoundException =
    _ServiceError . hasCode "PipelineNotFoundException"

-- | The number of pipelines associated with the AWS account has exceeded the
-- limit allowed for the account.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
