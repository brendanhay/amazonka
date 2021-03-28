{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodePipeline__ 
--
-- __Overview__ 
-- This is the AWS CodePipeline API Reference. This guide provides descriptions of the actions and data types for AWS CodePipeline. Some functionality for your pipeline can only be configured through the API. For more information, see the <https://docs.aws.amazon.com/codepipeline/latest/userguide/welcome.html AWS CodePipeline User Guide> .
-- You can use the AWS CodePipeline API to work with pipelines, stages, actions, and transitions.
-- /Pipelines/ are models of automated release processes. Each pipeline is uniquely named, and consists of stages, actions, and transitions. 
-- You can work with pipelines by calling:
--
--     * 'CreatePipeline' , which creates a uniquely named pipeline.
--
--
--     * 'DeletePipeline' , which deletes the specified pipeline.
--
--
--     * 'GetPipeline' , which returns information about the pipeline structure and pipeline metadata, including the pipeline Amazon Resource Name (ARN).
--
--
--     * 'GetPipelineExecution' , which returns information about a specific execution of a pipeline.
--
--
--     * 'GetPipelineState' , which returns information about the current state of the stages and actions of a pipeline.
--
--
--     * 'ListActionExecutions' , which returns action-level details for past executions. The details include full stage and action-level details, including individual action duration, status, any errors that occurred during the execution, and input and output artifact location details.
--
--
--     * 'ListPipelines' , which gets a summary of all of the pipelines associated with your account.
--
--
--     * 'ListPipelineExecutions' , which gets a summary of the most recent executions for a pipeline.
--
--
--     * 'StartPipelineExecution' , which runs the most recent revision of an artifact through the pipeline.
--
--
--     * 'StopPipelineExecution' , which stops the specified pipeline execution from continuing through the pipeline.
--
--
--     * 'UpdatePipeline' , which updates a pipeline with edits or changes to the structure of the pipeline.
--
--
-- Pipelines include /stages/ . Each stage contains one or more actions that must complete before the next stage begins. A stage results in success or failure. If a stage fails, the pipeline stops at that stage and remains stopped until either a new version of an artifact appears in the source location, or a user takes action to rerun the most recent artifact through the pipeline. You can call 'GetPipelineState' , which displays the status of a pipeline, including the status of stages in the pipeline, or 'GetPipeline' , which returns the entire structure of the pipeline, including the stages of that pipeline. For more information about the structure of stages and actions, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/pipeline-structure.html AWS CodePipeline Pipeline Structure Reference> .
-- Pipeline stages include /actions/ that are categorized into categories such as source or build actions performed in a stage of a pipeline. For example, you can use a source action to import artifacts into a pipeline from a source such as Amazon S3. Like stages, you do not work with actions directly in most cases, but you do define and interact with actions when working with pipeline operations such as 'CreatePipeline' and 'GetPipelineState' . Valid action categories are:
--
--     * Source
--
--
--     * Build
--
--
--     * Test
--
--
--     * Deploy
--
--
--     * Approval
--
--
--     * Invoke
--
--
-- Pipelines also include /transitions/ , which allow the transition of artifacts from one stage to the next in a pipeline after the actions in one stage complete.
-- You can work with transitions by calling:
--
--     * 'DisableStageTransition' , which prevents artifacts from transitioning to the next stage in a pipeline.
--
--
--     * 'EnableStageTransition' , which enables transition of artifacts between stages in a pipeline. 
--
--
-- __Using the API to integrate with AWS CodePipeline__ 
-- For third-party integrators or developers who want to create their own integrations with AWS CodePipeline, the expected sequence varies from the standard API user. To integrate with AWS CodePipeline, developers need to work with the following items:
-- __Jobs__ , which are instances of an action. For example, a job for a source action might import a revision of an artifact from a source. 
-- You can work with jobs by calling:
--
--     * 'AcknowledgeJob' , which confirms whether a job worker has received the specified job.
--
--
--     * 'GetJobDetails' , which returns the details of a job.
--
--
--     * 'PollForJobs' , which determines whether there are any jobs to act on.
--
--
--     * 'PutJobFailureResult' , which provides details of a job failure. 
--
--
--     * 'PutJobSuccessResult' , which provides details of a job success.
--
--
-- __Third party jobs__ , which are instances of an action created by a partner action and integrated into AWS CodePipeline. Partner actions are created by members of the AWS Partner Network.
-- You can work with third party jobs by calling:
--
--     * 'AcknowledgeThirdPartyJob' , which confirms whether a job worker has received the specified job.
--
--
--     * 'GetThirdPartyJobDetails' , which requests the details of a job for a partner action.
--
--
--     * 'PollForThirdPartyJobs' , which determines whether there are any jobs to act on. 
--
--
--     * 'PutThirdPartyJobFailureResult' , which provides details of a job failure.
--
--
--     * 'PutThirdPartyJobSuccessResult' , which provides details of a job success.
--
--
module Network.AWS.CodePipeline
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidClientTokenException
    , _InvalidClientTokenException

    -- ** ValidationException
    , _ValidationException

    -- ** InvalidNonceException
    , _InvalidNonceException

    -- ** ActionNotFoundException
    , _ActionNotFoundException

    -- ** InvalidApprovalTokenException
    , _InvalidApprovalTokenException

    -- ** PipelineExecutionNotStoppableException
    , _PipelineExecutionNotStoppableException

    -- ** InvalidBlockerDeclarationException
    , _InvalidBlockerDeclarationException

    -- ** OutputVariablesSizeExceededException
    , _OutputVariablesSizeExceededException

    -- ** InvalidJobStateException
    , _InvalidJobStateException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** ConflictException
    , _ConflictException

    -- ** InvalidJobException
    , _InvalidJobException

    -- ** PipelineVersionNotFoundException
    , _PipelineVersionNotFoundException

    -- ** StageNotRetryableException
    , _StageNotRetryableException

    -- ** PipelineExecutionNotFoundException
    , _PipelineExecutionNotFoundException

    -- ** InvalidWebhookAuthenticationParametersException
    , _InvalidWebhookAuthenticationParametersException

    -- ** WebhookNotFoundException
    , _WebhookNotFoundException

    -- ** InvalidTagsException
    , _InvalidTagsException

    -- ** ActionTypeNotFoundException
    , _ActionTypeNotFoundException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** InvalidStageDeclarationException
    , _InvalidStageDeclarationException

    -- ** DuplicatedStopRequestException
    , _DuplicatedStopRequestException

    -- ** InvalidWebhookFilterPatternException
    , _InvalidWebhookFilterPatternException

    -- ** InvalidActionDeclarationException
    , _InvalidActionDeclarationException

    -- ** StageNotFoundException
    , _StageNotFoundException

    -- ** InvalidStructureException
    , _InvalidStructureException

    -- ** JobNotFoundException
    , _JobNotFoundException

    -- ** ApprovalAlreadyCompletedException
    , _ApprovalAlreadyCompletedException

    -- ** InvalidArnException
    , _InvalidArnException

    -- ** PipelineNameInUseException
    , _PipelineNameInUseException

    -- ** PipelineNotFoundException
    , _PipelineNotFoundException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** NotLatestPipelineExecutionException
    , _NotLatestPipelineExecutionException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetPipeline 
    , module Network.AWS.CodePipeline.GetPipeline

    -- ** PutJobFailureResult 
    , module Network.AWS.CodePipeline.PutJobFailureResult

    -- ** PutApprovalResult 
    , module Network.AWS.CodePipeline.PutApprovalResult

    -- ** AcknowledgeThirdPartyJob 
    , module Network.AWS.CodePipeline.AcknowledgeThirdPartyJob

    -- ** PutThirdPartyJobFailureResult 
    , module Network.AWS.CodePipeline.PutThirdPartyJobFailureResult

    -- ** ListTagsForResource (Paginated)
    , module Network.AWS.CodePipeline.ListTagsForResource

    -- ** RegisterWebhookWithThirdParty 
    , module Network.AWS.CodePipeline.RegisterWebhookWithThirdParty

    -- ** PollForThirdPartyJobs 
    , module Network.AWS.CodePipeline.PollForThirdPartyJobs

    -- ** PollForJobs 
    , module Network.AWS.CodePipeline.PollForJobs

    -- ** StartPipelineExecution 
    , module Network.AWS.CodePipeline.StartPipelineExecution

    -- ** UpdatePipeline 
    , module Network.AWS.CodePipeline.UpdatePipeline

    -- ** DeletePipeline 
    , module Network.AWS.CodePipeline.DeletePipeline

    -- ** GetPipelineState 
    , module Network.AWS.CodePipeline.GetPipelineState

    -- ** GetJobDetails 
    , module Network.AWS.CodePipeline.GetJobDetails

    -- ** ListPipelines (Paginated)
    , module Network.AWS.CodePipeline.ListPipelines

    -- ** RetryStageExecution 
    , module Network.AWS.CodePipeline.RetryStageExecution

    -- ** GetPipelineExecution 
    , module Network.AWS.CodePipeline.GetPipelineExecution

    -- ** PutJobSuccessResult 
    , module Network.AWS.CodePipeline.PutJobSuccessResult

    -- ** DeregisterWebhookWithThirdParty 
    , module Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty

    -- ** DeleteCustomActionType 
    , module Network.AWS.CodePipeline.DeleteCustomActionType

    -- ** PutActionRevision 
    , module Network.AWS.CodePipeline.PutActionRevision

    -- ** DisableStageTransition 
    , module Network.AWS.CodePipeline.DisableStageTransition

    -- ** ListActionTypes (Paginated)
    , module Network.AWS.CodePipeline.ListActionTypes

    -- ** AcknowledgeJob 
    , module Network.AWS.CodePipeline.AcknowledgeJob

    -- ** EnableStageTransition 
    , module Network.AWS.CodePipeline.EnableStageTransition

    -- ** DeleteWebhook 
    , module Network.AWS.CodePipeline.DeleteWebhook

    -- ** PutWebhook 
    , module Network.AWS.CodePipeline.PutWebhook

    -- ** ListWebhooks (Paginated)
    , module Network.AWS.CodePipeline.ListWebhooks

    -- ** ListActionExecutions (Paginated)
    , module Network.AWS.CodePipeline.ListActionExecutions

    -- ** StopPipelineExecution 
    , module Network.AWS.CodePipeline.StopPipelineExecution

    -- ** TagResource 
    , module Network.AWS.CodePipeline.TagResource

    -- ** UntagResource 
    , module Network.AWS.CodePipeline.UntagResource

    -- ** CreatePipeline 
    , module Network.AWS.CodePipeline.CreatePipeline

    -- ** GetThirdPartyJobDetails 
    , module Network.AWS.CodePipeline.GetThirdPartyJobDetails

    -- ** PutThirdPartyJobSuccessResult 
    , module Network.AWS.CodePipeline.PutThirdPartyJobSuccessResult

    -- ** CreateCustomActionType 
    , module Network.AWS.CodePipeline.CreateCustomActionType

    -- ** ListPipelineExecutions (Paginated)
    , module Network.AWS.CodePipeline.ListPipelineExecutions

    -- * Types

    -- ** PipelineExecutionStatus
    , PipelineExecutionStatus (..)

    -- ** LastUpdatedBy
    , LastUpdatedBy (..)

    -- ** ListWebhookItem
    , ListWebhookItem (..)
    , mkListWebhookItem
    , lwiDefinition
    , lwiUrl
    , lwiArn
    , lwiErrorCode
    , lwiErrorMessage
    , lwiLastTriggered
    , lwiTags

    -- ** ApprovalToken
    , ApprovalToken (..)

    -- ** ActionExecution
    , ActionExecution (..)
    , mkActionExecution
    , aeActionExecutionId
    , aeErrorDetails
    , aeExternalExecutionId
    , aeExternalExecutionUrl
    , aeLastStatusChange
    , aeLastUpdatedBy
    , aePercentComplete
    , aeStatus
    , aeSummary
    , aeToken

    -- ** WebhookName
    , WebhookName (..)

    -- ** StopPipelineExecutionReason
    , StopPipelineExecutionReason (..)

    -- ** ArtifactDetails
    , ArtifactDetails (..)
    , mkArtifactDetails
    , adMinimumCount
    , adMaximumCount

    -- ** MatchEquals
    , MatchEquals (..)

    -- ** PipelineExecutionSummary
    , PipelineExecutionSummary (..)
    , mkPipelineExecutionSummary
    , pesLastUpdateTime
    , pesPipelineExecutionId
    , pesSourceRevisions
    , pesStartTime
    , pesStatus
    , pesStopTrigger
    , pesTrigger

    -- ** ClientId
    , ClientId (..)

    -- ** StageDeclaration
    , StageDeclaration (..)
    , mkStageDeclaration
    , sdName
    , sdActions
    , sdBlockers

    -- ** ArtifactStoreLocation
    , ArtifactStoreLocation (..)

    -- ** ArtifactDetail
    , ArtifactDetail (..)
    , mkArtifactDetail
    , aName
    , aS3location

    -- ** ActionProvider
    , ActionProvider (..)

    -- ** StageRetryMode
    , StageRetryMode (..)

    -- ** RevisionSummary
    , RevisionSummary (..)

    -- ** ExecutionId
    , ExecutionId (..)

    -- ** PipelineMetadata
    , PipelineMetadata (..)
    , mkPipelineMetadata
    , pmCreated
    , pmPipelineArn
    , pmUpdated

    -- ** S3Key
    , S3Key (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** ThirdPartyJobData
    , ThirdPartyJobData (..)
    , mkThirdPartyJobData
    , tpjdActionConfiguration
    , tpjdActionTypeId
    , tpjdArtifactCredentials
    , tpjdContinuationToken
    , tpjdEncryptionKey
    , tpjdInputArtifacts
    , tpjdOutputArtifacts
    , tpjdPipelineContext

    -- ** ExecutionTrigger
    , ExecutionTrigger (..)
    , mkExecutionTrigger
    , etTriggerDetail
    , etTriggerType

    -- ** S3ObjectKey
    , S3ObjectKey (..)

    -- ** ThirdPartyJob
    , ThirdPartyJob (..)
    , mkThirdPartyJob
    , tpjClientId
    , tpjJobId

    -- ** FailureDetails
    , FailureDetails (..)
    , mkFailureDetails
    , fdType
    , fdMessage
    , fdExternalExecutionId

    -- ** ClientToken
    , ClientToken (..)

    -- ** JobId
    , JobId (..)

    -- ** ContinuationToken
    , ContinuationToken (..)

    -- ** AWSRegionName
    , AWSRegionName (..)

    -- ** ActionRevision
    , ActionRevision (..)
    , mkActionRevision
    , aRevisionId
    , aRevisionChangeId
    , aCreated

    -- ** PipelineName
    , PipelineName (..)

    -- ** ActionConfigurationValue
    , ActionConfigurationValue (..)

    -- ** SecretAccessKey
    , SecretAccessKey (..)

    -- ** ActionConfigurationQueryableValue
    , ActionConfigurationQueryableValue (..)

    -- ** SessionToken
    , SessionToken (..)

    -- ** EncryptionKeyId
    , EncryptionKeyId (..)

    -- ** ArtifactRevision
    , ArtifactRevision (..)
    , mkArtifactRevision
    , arCreated
    , arName
    , arRevisionChangeIdentifier
    , arRevisionId
    , arRevisionSummary
    , arRevisionUrl

    -- ** ActionExecutionResult
    , ActionExecutionResult (..)
    , mkActionExecutionResult
    , aerExternalExecutionId
    , aerExternalExecutionSummary
    , aerExternalExecutionUrl

    -- ** JsonPath
    , JsonPath (..)

    -- ** WebhookAuthConfigurationAllowedIPRange
    , WebhookAuthConfigurationAllowedIPRange (..)

    -- ** FailureType
    , FailureType (..)

    -- ** ActionExecutionStatus
    , ActionExecutionStatus (..)

    -- ** PipelineExecution
    , PipelineExecution (..)
    , mkPipelineExecution
    , peArtifactRevisions
    , pePipelineExecutionId
    , pePipelineName
    , pePipelineVersion
    , peStatus

    -- ** WebhookUrl
    , WebhookUrl (..)

    -- ** EncryptionKeyType
    , EncryptionKeyType (..)

    -- ** ExecutionDetails
    , ExecutionDetails (..)
    , mkExecutionDetails
    , edExternalExecutionId
    , edPercentComplete
    , edSummary

    -- ** InputArtifact
    , InputArtifact (..)
    , mkInputArtifact
    , iaName

    -- ** TriggerType
    , TriggerType (..)

    -- ** S3ArtifactLocation
    , S3ArtifactLocation (..)
    , mkS3ArtifactLocation
    , salBucketName
    , salObjectKey

    -- ** WebhookAuthenticationType
    , WebhookAuthenticationType (..)

    -- ** ActionTypeSettings
    , ActionTypeSettings (..)
    , mkActionTypeSettings
    , atsEntityUrlTemplate
    , atsExecutionUrlTemplate
    , atsRevisionUrlTemplate
    , atsThirdPartyConfigurationUrl

    -- ** ApprovalSummary
    , ApprovalSummary (..)

    -- ** StageState
    , StageState (..)
    , mkStageState
    , ssActionStates
    , ssInboundExecution
    , ssInboundTransitionState
    , ssLatestExecution
    , ssStageName

    -- ** Url
    , Url (..)

    -- ** StageContext
    , StageContext (..)
    , mkStageContext
    , scName

    -- ** ArtifactLocationType
    , ArtifactLocationType (..)

    -- ** AWSSessionCredentials
    , AWSSessionCredentials (..)
    , mkAWSSessionCredentials
    , awsscAccessKeyId
    , awsscSecretAccessKey
    , awsscSessionToken

    -- ** ApprovalResult
    , ApprovalResult (..)
    , mkApprovalResult
    , arSummary
    , arStatus

    -- ** BlockerDeclaration
    , BlockerDeclaration (..)
    , mkBlockerDeclaration
    , bdName
    , bdType

    -- ** StageExecution
    , StageExecution (..)
    , mkStageExecution
    , sePipelineExecutionId
    , seStatus

    -- ** ActionCategory
    , ActionCategory (..)

    -- ** ActionDeclaration
    , ActionDeclaration (..)
    , mkActionDeclaration
    , adName
    , adActionTypeId
    , adConfiguration
    , adInputArtifacts
    , adNamespace
    , adOutputArtifacts
    , adRegion
    , adRoleArn
    , adRunOrder

    -- ** PipelineSummary
    , PipelineSummary (..)
    , mkPipelineSummary
    , psCreated
    , psName
    , psUpdated
    , psVersion

    -- ** Artifact
    , Artifact (..)
    , mkArtifact
    , afLocation
    , afName
    , afRevision

    -- ** WebhookDefinition
    , WebhookDefinition (..)
    , mkWebhookDefinition
    , wdName
    , wdTargetPipeline
    , wdTargetAction
    , wdFilters
    , wdAuthentication
    , wdAuthenticationConfiguration

    -- ** WebhookAuthConfigurationSecretToken
    , WebhookAuthConfigurationSecretToken (..)

    -- ** ArtifactStore
    , ArtifactStore (..)
    , mkArtifactStore
    , asType
    , asLocation
    , asEncryptionKey

    -- ** ActionConfigurationProperty
    , ActionConfigurationProperty (..)
    , mkActionConfigurationProperty
    , acpName
    , acpRequired
    , acpKey
    , acpSecret
    , acpDescription
    , acpQueryable
    , acpType

    -- ** ArtifactName
    , ArtifactName (..)

    -- ** TriggerDetail
    , TriggerDetail (..)

    -- ** OutputVariablesValue
    , OutputVariablesValue (..)

    -- ** OutputArtifact
    , OutputArtifact (..)
    , mkOutputArtifact
    , oaName

    -- ** ActionName
    , ActionName (..)

    -- ** AccountId
    , AccountId (..)

    -- ** NextToken
    , NextToken (..)

    -- ** DisabledReason
    , DisabledReason (..)

    -- ** ActionExecutionFilter
    , ActionExecutionFilter (..)
    , mkActionExecutionFilter
    , aefPipelineExecutionId

    -- ** JobData
    , JobData (..)
    , mkJobData
    , jdActionConfiguration
    , jdActionTypeId
    , jdArtifactCredentials
    , jdContinuationToken
    , jdEncryptionKey
    , jdInputArtifacts
    , jdOutputArtifacts
    , jdPipelineContext

    -- ** Job
    , Job (..)
    , mkJob
    , jAccountId
    , jData
    , jId
    , jNonce

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** ActionOwner
    , ActionOwner (..)

    -- ** ThirdPartyJobId
    , ThirdPartyJobId (..)

    -- ** PipelineArn
    , PipelineArn (..)

    -- ** ExternalExecutionId
    , ExternalExecutionId (..)

    -- ** CurrentRevision
    , CurrentRevision (..)
    , mkCurrentRevision
    , crRevision
    , crChangeIdentifier
    , crCreated
    , crRevisionSummary

    -- ** StopExecutionTrigger
    , StopExecutionTrigger (..)
    , mkStopExecutionTrigger
    , setReason

    -- ** Version
    , Version (..)

    -- ** PipelineDeclaration
    , PipelineDeclaration (..)
    , mkPipelineDeclaration
    , pdName
    , pdRoleArn
    , pdStages
    , pdArtifactStore
    , pdArtifactStores
    , pdVersion

    -- ** ActionExecutionInput
    , ActionExecutionInput (..)
    , mkActionExecutionInput
    , aeiActionTypeId
    , aeiConfiguration
    , aeiInputArtifacts
    , aeiNamespace
    , aeiRegion
    , aeiResolvedConfiguration
    , aeiRoleArn

    -- ** ErrorDetails
    , ErrorDetails (..)
    , mkErrorDetails
    , edCode
    , edMessage

    -- ** StageExecutionStatus
    , StageExecutionStatus (..)

    -- ** OutputVariablesKey
    , OutputVariablesKey (..)

    -- ** PipelineContext
    , PipelineContext (..)
    , mkPipelineContext
    , pcAction
    , pcPipelineArn
    , pcPipelineExecutionId
    , pcPipelineName
    , pcStage

    -- ** WebhookFilterRule
    , WebhookFilterRule (..)
    , mkWebhookFilterRule
    , wfrJsonPath
    , wfrMatchEquals

    -- ** JobDetails
    , JobDetails (..)
    , mkJobDetails
    , jdAccountId
    , jdData
    , jdId

    -- ** ActionExecutionDetail
    , ActionExecutionDetail (..)
    , mkActionExecutionDetail
    , aedActionExecutionId
    , aedActionName
    , aedInput
    , aedLastUpdateTime
    , aedOutput
    , aedPipelineExecutionId
    , aedPipelineVersion
    , aedStageName
    , aedStartTime
    , aedStatus

    -- ** PipelineExecutionId
    , PipelineExecutionId (..)

    -- ** Code
    , Code (..)

    -- ** TagKey
    , TagKey (..)

    -- ** S3Location
    , S3Location (..)
    , mkS3Location
    , slBucket
    , slKey

    -- ** TransitionState
    , TransitionState (..)
    , mkTransitionState
    , tsDisabledReason
    , tsEnabled
    , tsLastChangedAt
    , tsLastChangedBy

    -- ** EncryptionKey
    , EncryptionKey (..)
    , mkEncryptionKey
    , ekId
    , ekType

    -- ** ThirdPartyJobDetails
    , ThirdPartyJobDetails (..)
    , mkThirdPartyJobDetails
    , tpjdData
    , tpjdId
    , tpjdNonce

    -- ** JobStatus
    , JobStatus (..)

    -- ** StageName
    , StageName (..)

    -- ** StageTransitionType
    , StageTransitionType (..)

    -- ** LastChangedBy
    , LastChangedBy (..)

    -- ** ExternalExecutionSummary
    , ExternalExecutionSummary (..)

    -- ** WebhookAuthConfiguration
    , WebhookAuthConfiguration (..)
    , mkWebhookAuthConfiguration
    , wacAllowedIPRange
    , wacSecretToken

    -- ** Revision
    , Revision (..)

    -- ** ArtifactLocation
    , ArtifactLocation (..)
    , mkArtifactLocation
    , alS3Location
    , alType

    -- ** ApprovalStatus
    , ApprovalStatus (..)

    -- ** Message
    , Message (..)

    -- ** ActionTypeId
    , ActionTypeId (..)
    , mkActionTypeId
    , atiCategory
    , atiOwner
    , atiProvider
    , atiVersion

    -- ** ClientRequestToken
    , ClientRequestToken (..)

    -- ** ActionExecutionOutput
    , ActionExecutionOutput (..)
    , mkActionExecutionOutput
    , aeoExecutionResult
    , aeoOutputArtifacts
    , aeoOutputVariables

    -- ** Description
    , Description (..)

    -- ** ActionType
    , ActionType (..)
    , mkActionType
    , atId
    , atInputArtifactDetails
    , atOutputArtifactDetails
    , atActionConfigurationProperties
    , atSettings

    -- ** BlockerType
    , BlockerType (..)

    -- ** ActionConfiguration
    , ActionConfiguration (..)
    , mkActionConfiguration
    , acConfiguration

    -- ** RevisionChangeIdentifier
    , RevisionChangeIdentifier (..)

    -- ** AccessKeyId
    , AccessKeyId (..)

    -- ** SourceRevision
    , SourceRevision (..)
    , mkSourceRevision
    , srActionName
    , srRevisionId
    , srRevisionSummary
    , srRevisionUrl

    -- ** Nonce
    , Nonce (..)

    -- ** ArtifactStoreType
    , ArtifactStoreType (..)

    -- ** ActionConfigurationPropertyType
    , ActionConfigurationPropertyType (..)

    -- ** ActionState
    , ActionState (..)
    , mkActionState
    , asActionName
    , asCurrentRevision
    , asEntityUrl
    , asLatestExecution
    , asRevisionUrl

    -- ** ActionConfigurationKey
    , ActionConfigurationKey (..)

    -- ** ActionContext
    , ActionContext (..)
    , mkActionContext
    , acActionExecutionId
    , acName

    -- ** ActionExecutionId
    , ActionExecutionId (..)

    -- ** RoleArn
    , RoleArn (..)

    -- ** Arn
    , Arn (..)

    -- ** ErrorCode
    , ErrorCode (..)

    -- ** ErrorMessage
    , ErrorMessage (..)

    -- ** ExternalExecutionUrl
    , ExternalExecutionUrl (..)

    -- ** Summary
    , Summary (..)

    -- ** Token
    , Token (..)

    -- ** Name
    , Name (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** RevisionId
    , RevisionId (..)

    -- ** RevisionChangeId
    , RevisionChangeId (..)

    -- ** RevisionUrl
    , RevisionUrl (..)

    -- ** BucketName
    , BucketName (..)

    -- ** EntityUrlTemplate
    , EntityUrlTemplate (..)

    -- ** ExecutionUrlTemplate
    , ExecutionUrlTemplate (..)

    -- ** RevisionUrlTemplate
    , RevisionUrlTemplate (..)

    -- ** ThirdPartyConfigurationUrl
    , ThirdPartyConfigurationUrl (..)

    -- ** Namespace
    , Namespace (..)

    -- ** TargetAction
    , TargetAction (..)

    -- ** ChangeIdentifier
    , ChangeIdentifier (..)

    -- ** Bucket
    , Bucket (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Waiters
import Network.AWS.CodePipeline.GetPipeline
import Network.AWS.CodePipeline.PutJobFailureResult
import Network.AWS.CodePipeline.PutApprovalResult
import Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
import Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
import Network.AWS.CodePipeline.ListTagsForResource
import Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
import Network.AWS.CodePipeline.PollForThirdPartyJobs
import Network.AWS.CodePipeline.PollForJobs
import Network.AWS.CodePipeline.StartPipelineExecution
import Network.AWS.CodePipeline.UpdatePipeline
import Network.AWS.CodePipeline.DeletePipeline
import Network.AWS.CodePipeline.GetPipelineState
import Network.AWS.CodePipeline.GetJobDetails
import Network.AWS.CodePipeline.ListPipelines
import Network.AWS.CodePipeline.RetryStageExecution
import Network.AWS.CodePipeline.GetPipelineExecution
import Network.AWS.CodePipeline.PutJobSuccessResult
import Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty
import Network.AWS.CodePipeline.DeleteCustomActionType
import Network.AWS.CodePipeline.PutActionRevision
import Network.AWS.CodePipeline.DisableStageTransition
import Network.AWS.CodePipeline.ListActionTypes
import Network.AWS.CodePipeline.AcknowledgeJob
import Network.AWS.CodePipeline.EnableStageTransition
import Network.AWS.CodePipeline.DeleteWebhook
import Network.AWS.CodePipeline.PutWebhook
import Network.AWS.CodePipeline.ListWebhooks
import Network.AWS.CodePipeline.ListActionExecutions
import Network.AWS.CodePipeline.StopPipelineExecution
import Network.AWS.CodePipeline.TagResource
import Network.AWS.CodePipeline.UntagResource
import Network.AWS.CodePipeline.CreatePipeline
import Network.AWS.CodePipeline.GetThirdPartyJobDetails
import Network.AWS.CodePipeline.PutThirdPartyJobSuccessResult
import Network.AWS.CodePipeline.CreateCustomActionType
import Network.AWS.CodePipeline.ListPipelineExecutions
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CodePipeline'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
