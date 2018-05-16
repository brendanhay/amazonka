{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types
    (
    -- * Service Configuration
      ioT

    -- * Errors
    , _CertificateConflictException
    , _SqlParseException
    , _IndexNotReadyException
    , _InvalidRequestException
    , _TransferConflictException
    , _CertificateStateException
    , _InvalidResponseException
    , _RegistrationCodeValidationException
    , _MalformedPolicyException
    , _DeleteConflictException
    , _ResourceAlreadyExistsException
    , _NotConfiguredException
    , _CertificateValidationException
    , _ResourceRegistrationFailureException
    , _InvalidQueryException
    , _TransferAlreadyCompletedException
    , _ThrottlingException
    , _ConflictingResourceUpdateException
    , _InternalFailureException
    , _VersionsLimitExceededException
    , _ServiceUnavailableException
    , _InternalException
    , _VersionConflictException
    , _UnauthorizedException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * ActionType
    , ActionType (..)

    -- * AuthDecision
    , AuthDecision (..)

    -- * AuthorizerStatus
    , AuthorizerStatus (..)

    -- * AutoRegistrationStatus
    , AutoRegistrationStatus (..)

    -- * CACertificateStatus
    , CACertificateStatus (..)

    -- * CannedAccessControlList
    , CannedAccessControlList (..)

    -- * CertificateStatus
    , CertificateStatus (..)

    -- * DynamoKeyType
    , DynamoKeyType (..)

    -- * EventType
    , EventType (..)

    -- * IndexStatus
    , IndexStatus (..)

    -- * JobExecutionStatus
    , JobExecutionStatus (..)

    -- * JobStatus
    , JobStatus (..)

    -- * LogLevel
    , LogLevel (..)

    -- * LogTargetType
    , LogTargetType (..)

    -- * MessageFormat
    , MessageFormat (..)

    -- * OTAUpdateStatus
    , OTAUpdateStatus (..)

    -- * ReportType
    , ReportType (..)

    -- * TargetSelection
    , TargetSelection (..)

    -- * TaskStatus
    , TaskStatus (..)

    -- * ThingIndexingMode
    , ThingIndexingMode (..)

    -- * Action
    , Action
    , action
    , aCloudwatchMetric
    , aDynamoDBv2
    , aCloudwatchAlarm
    , aSns
    , aDynamoDB
    , aFirehose
    , aIotAnalytics
    , aLambda
    , aSalesforce
    , aKinesis
    , aS3
    , aElasticsearch
    , aRepublish
    , aSqs

    -- * Allowed
    , Allowed
    , allowed
    , aPolicies

    -- * AttributePayload
    , AttributePayload
    , attributePayload
    , apAttributes
    , apMerge

    -- * AuthInfo
    , AuthInfo
    , authInfo
    , aiResources
    , aiActionType

    -- * AuthResult
    , AuthResult
    , authResult
    , arDenied
    , arAuthDecision
    , arAllowed
    , arMissingContextValues
    , arAuthInfo

    -- * AuthorizerDescription
    , AuthorizerDescription
    , authorizerDescription
    , adStatus
    , adLastModifiedDate
    , adAuthorizerName
    , adAuthorizerFunctionARN
    , adAuthorizerARN
    , adCreationDate
    , adTokenSigningPublicKeys
    , adTokenKeyName

    -- * AuthorizerSummary
    , AuthorizerSummary
    , authorizerSummary
    , asAuthorizerName
    , asAuthorizerARN

    -- * CACertificate
    , CACertificate
    , cACertificate
    , cacStatus
    , cacCertificateARN
    , cacCertificateId
    , cacCreationDate

    -- * CACertificateDescription
    , CACertificateDescription
    , cACertificateDescription
    , cacdStatus
    , cacdOwnedBy
    , cacdLastModifiedDate
    , cacdCertificatePem
    , cacdCertificateARN
    , cacdCertificateId
    , cacdAutoRegistrationStatus
    , cacdCreationDate
    , cacdGenerationId
    , cacdCustomerVersion

    -- * Certificate
    , Certificate
    , certificate
    , cStatus
    , cCertificateARN
    , cCertificateId
    , cCreationDate

    -- * CertificateDescription
    , CertificateDescription
    , certificateDescription
    , cdStatus
    , cdOwnedBy
    , cdLastModifiedDate
    , cdCaCertificateId
    , cdPreviousOwnedBy
    , cdCertificatePem
    , cdCertificateARN
    , cdCertificateId
    , cdCreationDate
    , cdGenerationId
    , cdTransferData
    , cdCustomerVersion

    -- * CloudwatchAlarmAction
    , CloudwatchAlarmAction
    , cloudwatchAlarmAction
    , caaRoleARN
    , caaAlarmName
    , caaStateReason
    , caaStateValue

    -- * CloudwatchMetricAction
    , CloudwatchMetricAction
    , cloudwatchMetricAction
    , cmaMetricTimestamp
    , cmaRoleARN
    , cmaMetricNamespace
    , cmaMetricName
    , cmaMetricValue
    , cmaMetricUnit

    -- * CodeSigning
    , CodeSigning
    , codeSigning
    , csCustomCodeSigning
    , csAwsSignerJobId

    -- * CodeSigningCertificateChain
    , CodeSigningCertificateChain
    , codeSigningCertificateChain
    , csccStream
    , csccCertificateName
    , csccInlineDocument

    -- * CodeSigningSignature
    , CodeSigningSignature
    , codeSigningSignature
    , cssStream
    , cssInlineDocument

    -- * Configuration
    , Configuration
    , configuration
    , cEnabled

    -- * CustomCodeSigning
    , CustomCodeSigning
    , customCodeSigning
    , ccsSignature
    , ccsHashAlgorithm
    , ccsCertificateChain
    , ccsSignatureAlgorithm

    -- * Denied
    , Denied
    , denied
    , dImplicitDeny
    , dExplicitDeny

    -- * DynamoDBAction
    , DynamoDBAction
    , dynamoDBAction
    , ddbaHashKeyType
    , ddbaOperation
    , ddbaRangeKeyType
    , ddbaPayloadField
    , ddbaRangeKeyField
    , ddbaRangeKeyValue
    , ddbaTableName
    , ddbaRoleARN
    , ddbaHashKeyField
    , ddbaHashKeyValue

    -- * DynamoDBv2Action
    , DynamoDBv2Action
    , dynamoDBv2Action
    , ddaPutItem
    , ddaRoleARN

    -- * EffectivePolicy
    , EffectivePolicy
    , effectivePolicy
    , epPolicyName
    , epPolicyDocument
    , epPolicyARN

    -- * ElasticsearchAction
    , ElasticsearchAction
    , elasticsearchAction
    , eaRoleARN
    , eaEndpoint
    , eaIndex
    , eaType
    , eaId

    -- * ErrorInfo
    , ErrorInfo
    , errorInfo
    , eiCode
    , eiMessage

    -- * ExplicitDeny
    , ExplicitDeny
    , explicitDeny
    , edPolicies

    -- * FirehoseAction
    , FirehoseAction
    , firehoseAction
    , faSeparator
    , faRoleARN
    , faDeliveryStreamName

    -- * GroupNameAndARN
    , GroupNameAndARN
    , groupNameAndARN
    , gnaaGroupARN
    , gnaaGroupName

    -- * ImplicitDeny
    , ImplicitDeny
    , implicitDeny
    , idPolicies

    -- * IotAnalyticsAction
    , IotAnalyticsAction
    , iotAnalyticsAction
    , iaaChannelARN
    , iaaChannelName
    , iaaRoleARN

    -- * Job
    , Job
    , job
    , jobStatus
    , jobJobExecutionsRolloutConfig
    , jobJobId
    , jobLastUpdatedAt
    , jobJobARN
    , jobCreatedAt
    , jobDocumentParameters
    , jobJobProcessDetails
    , jobPresignedURLConfig
    , jobTargets
    , jobCompletedAt
    , jobComment
    , jobDescription
    , jobTargetSelection

    -- * JobExecution
    , JobExecution
    , jobExecution
    , jeStatus
    , jeJobId
    , jeLastUpdatedAt
    , jeQueuedAt
    , jeStatusDetails
    , jeThingARN
    , jeExecutionNumber
    , jeStartedAt

    -- * JobExecutionStatusDetails
    , JobExecutionStatusDetails
    , jobExecutionStatusDetails
    , jesdDetailsMap

    -- * JobExecutionSummary
    , JobExecutionSummary
    , jobExecutionSummary
    , jesStatus
    , jesLastUpdatedAt
    , jesQueuedAt
    , jesExecutionNumber
    , jesStartedAt

    -- * JobExecutionSummaryForJob
    , JobExecutionSummaryForJob
    , jobExecutionSummaryForJob
    , jesfjJobExecutionSummary
    , jesfjThingARN

    -- * JobExecutionSummaryForThing
    , JobExecutionSummaryForThing
    , jobExecutionSummaryForThing
    , jesftJobId
    , jesftJobExecutionSummary

    -- * JobExecutionsRolloutConfig
    , JobExecutionsRolloutConfig
    , jobExecutionsRolloutConfig
    , jercMaximumPerMinute

    -- * JobProcessDetails
    , JobProcessDetails
    , jobProcessDetails
    , jpdNumberOfRemovedThings
    , jpdNumberOfQueuedThings
    , jpdNumberOfFailedThings
    , jpdNumberOfSucceededThings
    , jpdNumberOfInProgressThings
    , jpdNumberOfCanceledThings
    , jpdNumberOfRejectedThings
    , jpdProcessingTargets

    -- * JobSummary
    , JobSummary
    , jobSummary
    , jsStatus
    , jsJobId
    , jsLastUpdatedAt
    , jsJobARN
    , jsCreatedAt
    , jsThingGroupId
    , jsCompletedAt
    , jsTargetSelection

    -- * KeyPair
    , KeyPair
    , keyPair
    , kpPrivateKey
    , kpPublicKey

    -- * KinesisAction
    , KinesisAction
    , kinesisAction
    , kaPartitionKey
    , kaRoleARN
    , kaStreamName

    -- * LambdaAction
    , LambdaAction
    , lambdaAction
    , laFunctionARN

    -- * LogTarget
    , LogTarget
    , logTarget
    , ltTargetName
    , ltTargetType

    -- * LogTargetConfiguration
    , LogTargetConfiguration
    , logTargetConfiguration
    , ltcLogLevel
    , ltcLogTarget

    -- * LoggingOptionsPayload
    , LoggingOptionsPayload
    , loggingOptionsPayload
    , lopLogLevel
    , lopRoleARN

    -- * OTAUpdateFile
    , OTAUpdateFile
    , oTAUpdateFile
    , otaufFileVersion
    , otaufAttributes
    , otaufFileSource
    , otaufCodeSigning
    , otaufFileName

    -- * OTAUpdateInfo
    , OTAUpdateInfo
    , oTAUpdateInfo
    , otauiLastModifiedDate
    , otauiAwsIotJobId
    , otauiOtaUpdateFiles
    , otauiOtaUpdateStatus
    , otauiTargets
    , otauiAwsIotJobARN
    , otauiCreationDate
    , otauiAdditionalParameters
    , otauiOtaUpdateId
    , otauiErrorInfo
    , otauiOtaUpdateARN
    , otauiDescription
    , otauiTargetSelection

    -- * OTAUpdateSummary
    , OTAUpdateSummary
    , oTAUpdateSummary
    , otausCreationDate
    , otausOtaUpdateId
    , otausOtaUpdateARN

    -- * OutgoingCertificate
    , OutgoingCertificate
    , outgoingCertificate
    , ocTransferDate
    , ocCertificateARN
    , ocCertificateId
    , ocTransferredTo
    , ocCreationDate
    , ocTransferMessage

    -- * Policy
    , Policy
    , policy
    , pPolicyName
    , pPolicyARN

    -- * PolicyVersion
    , PolicyVersion
    , policyVersion
    , pvVersionId
    , pvCreateDate
    , pvIsDefaultVersion

    -- * PresignedURLConfig
    , PresignedURLConfig
    , presignedURLConfig
    , pucExpiresInSec
    , pucRoleARN

    -- * PutItemInput
    , PutItemInput
    , putItemInput
    , piiTableName

    -- * RegistrationConfig
    , RegistrationConfig
    , registrationConfig
    , rcTemplateBody
    , rcRoleARN

    -- * RepublishAction
    , RepublishAction
    , republishAction
    , raRoleARN
    , raTopic

    -- * RoleAliasDescription
    , RoleAliasDescription
    , roleAliasDescription
    , radRoleAliasARN
    , radLastModifiedDate
    , radRoleAlias
    , radOwner
    , radCreationDate
    , radCredentialDurationSeconds
    , radRoleARN

    -- * S3Action
    , S3Action
    , s3Action
    , sCannedACL
    , sRoleARN
    , sBucketName
    , sKey

    -- * S3Location
    , S3Location
    , s3Location
    , slVersion
    , slBucket
    , slKey

    -- * SNSAction
    , SNSAction
    , snsAction
    , snsaMessageFormat
    , snsaTargetARN
    , snsaRoleARN

    -- * SalesforceAction
    , SalesforceAction
    , salesforceAction
    , saToken
    , saUrl

    -- * SqsAction
    , SqsAction
    , sqsAction
    , saUseBase64
    , saRoleARN
    , saQueueURL

    -- * Stream
    , Stream
    , stream
    , sFileId
    , sStreamId

    -- * StreamFile
    , StreamFile
    , streamFile
    , sfS3Location
    , sfFileId

    -- * StreamInfo
    , StreamInfo
    , streamInfo
    , siLastUpdatedAt
    , siCreatedAt
    , siStreamVersion
    , siStreamARN
    , siFiles
    , siDescription
    , siStreamId
    , siRoleARN

    -- * StreamSummary
    , StreamSummary
    , streamSummary
    , ssStreamVersion
    , ssStreamARN
    , ssDescription
    , ssStreamId

    -- * ThingAttribute
    , ThingAttribute
    , thingAttribute
    , taThingTypeName
    , taThingARN
    , taAttributes
    , taVersion
    , taThingName

    -- * ThingDocument
    , ThingDocument
    , thingDocument
    , tdThingGroupNames
    , tdThingTypeName
    , tdShadow
    , tdAttributes
    , tdThingName
    , tdThingId

    -- * ThingGroupMetadata
    , ThingGroupMetadata
    , thingGroupMetadata
    , tgmRootToParentThingGroups
    , tgmParentGroupName
    , tgmCreationDate

    -- * ThingGroupProperties
    , ThingGroupProperties
    , thingGroupProperties
    , tgpAttributePayload
    , tgpThingGroupDescription

    -- * ThingIndexingConfiguration
    , ThingIndexingConfiguration
    , thingIndexingConfiguration
    , ticThingIndexingMode

    -- * ThingTypeDefinition
    , ThingTypeDefinition
    , thingTypeDefinition
    , ttdThingTypeProperties
    , ttdThingTypeName
    , ttdThingTypeMetadata
    , ttdThingTypeARN

    -- * ThingTypeMetadata
    , ThingTypeMetadata
    , thingTypeMetadata
    , ttmDeprecationDate
    , ttmCreationDate
    , ttmDeprecated

    -- * ThingTypeProperties
    , ThingTypeProperties
    , thingTypeProperties
    , ttpSearchableAttributes
    , ttpThingTypeDescription

    -- * TopicRule
    , TopicRule
    , topicRule
    , trCreatedAt
    , trActions
    , trAwsIotSqlVersion
    , trErrorAction
    , trRuleDisabled
    , trRuleName
    , trSql
    , trDescription

    -- * TopicRuleListItem
    , TopicRuleListItem
    , topicRuleListItem
    , trliCreatedAt
    , trliRuleDisabled
    , trliRuleName
    , trliRuleARN
    , trliTopicPattern

    -- * TopicRulePayload
    , TopicRulePayload
    , topicRulePayload
    , trpAwsIotSqlVersion
    , trpErrorAction
    , trpRuleDisabled
    , trpDescription
    , trpSql
    , trpActions

    -- * TransferData
    , TransferData
    , transferData
    , tdTransferDate
    , tdAcceptDate
    , tdTransferMessage
    , tdRejectDate
    , tdRejectReason
    ) where

import Network.AWS.IoT.Types.Product
import Network.AWS.IoT.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-05-28@ of the Amazon IoT SDK configuration.
ioT :: Service
ioT =
  Service
    { _svcAbbrev = "IoT"
    , _svcSigner = v4
    , _svcPrefix = "iot"
    , _svcVersion = "2015-05-28"
    , _svcEndpoint = defaultEndpoint ioT
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "IoT"
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


-- | Unable to verify the CA certificate used to sign the device certificate you are attempting to register. This is happens when you have registered more than one CA certificate that has the same subject field and public key.
--
--
_CertificateConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateConflictException =
  _MatchServiceError ioT "CertificateConflictException" . hasStatus 409


-- | The Rule-SQL expression can't be parsed correctly.
--
--
_SqlParseException :: AsError a => Getting (First ServiceError) a ServiceError
_SqlParseException = _MatchServiceError ioT "SqlParseException" . hasStatus 400


-- | The index is not ready.
--
--
_IndexNotReadyException :: AsError a => Getting (First ServiceError) a ServiceError
_IndexNotReadyException =
  _MatchServiceError ioT "IndexNotReadyException" . hasStatus 400


-- | The request is not valid.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError ioT "InvalidRequestException" . hasStatus 400


-- | You can't transfer the certificate because authorization policies are still attached.
--
--
_TransferConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_TransferConflictException =
  _MatchServiceError ioT "TransferConflictException" . hasStatus 409


-- | The certificate operation is not allowed.
--
--
_CertificateStateException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateStateException =
  _MatchServiceError ioT "CertificateStateException" . hasStatus 406


-- | The response is invalid.
--
--
_InvalidResponseException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResponseException =
  _MatchServiceError ioT "InvalidResponseException" . hasStatus 400


-- | The registration code is invalid.
--
--
_RegistrationCodeValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_RegistrationCodeValidationException =
  _MatchServiceError ioT "RegistrationCodeValidationException" . hasStatus 400


-- | The policy documentation is not valid.
--
--
_MalformedPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyException =
  _MatchServiceError ioT "MalformedPolicyException" . hasStatus 400


-- | You can't delete the resource because it is attached to one or more resources.
--
--
_DeleteConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_DeleteConflictException =
  _MatchServiceError ioT "DeleteConflictException" . hasStatus 409


-- | The resource already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
  _MatchServiceError ioT "ResourceAlreadyExistsException" . hasStatus 409


-- | The resource is not configured.
--
--
_NotConfiguredException :: AsError a => Getting (First ServiceError) a ServiceError
_NotConfiguredException =
  _MatchServiceError ioT "NotConfiguredException" . hasStatus 404


-- | The certificate is invalid.
--
--
_CertificateValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateValidationException =
  _MatchServiceError ioT "CertificateValidationException" . hasStatus 400


-- | The resource registration failed.
--
--
_ResourceRegistrationFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceRegistrationFailureException =
  _MatchServiceError ioT "ResourceRegistrationFailureException" . hasStatus 400


-- | The query is invalid.
--
--
_InvalidQueryException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidQueryException =
  _MatchServiceError ioT "InvalidQueryException" . hasStatus 400


-- | You can't revert the certificate transfer because the transfer is already complete.
--
--
_TransferAlreadyCompletedException :: AsError a => Getting (First ServiceError) a ServiceError
_TransferAlreadyCompletedException =
  _MatchServiceError ioT "TransferAlreadyCompletedException" . hasStatus 410


-- | The rate exceeds the limit.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
  _MatchServiceError ioT "ThrottlingException" . hasStatus 429


-- | A conflicting resource update exception. This exception is thrown when two pending updates cause a conflict.
--
--
_ConflictingResourceUpdateException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictingResourceUpdateException =
  _MatchServiceError ioT "ConflictingResourceUpdateException" . hasStatus 409


-- | An unexpected error has occurred.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
  _MatchServiceError ioT "InternalFailureException" . hasStatus 500


-- | The number of policy versions exceeds the limit.
--
--
_VersionsLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_VersionsLimitExceededException =
  _MatchServiceError ioT "VersionsLimitExceededException" . hasStatus 409


-- | The service is temporarily unavailable.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError ioT "ServiceUnavailableException" . hasStatus 503


-- | An unexpected error has occurred.
--
--
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException = _MatchServiceError ioT "InternalException" . hasStatus 500


-- | An exception thrown when the version of a thing passed to a command is different than the version specified with the --version parameter.
--
--
_VersionConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_VersionConflictException =
  _MatchServiceError ioT "VersionConflictException" . hasStatus 409


-- | You are not authorized to perform this operation.
--
--
_UnauthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedException =
  _MatchServiceError ioT "UnauthorizedException" . hasStatus 401


-- | The specified resource does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError ioT "ResourceNotFoundException" . hasStatus 404


-- | The number of attached entities exceeds the limit.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError ioT "LimitExceededException" . hasStatus 410

