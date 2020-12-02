{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS IoT__
--
-- AWS IoT provides secure, bi-directional communication between Internet-connected devices (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. You can discover your custom IoT-Data endpoint to communicate with, configure rules for data processing and integration with other services, organize resources associated with each device (Registry), configure logging, and create and manage policies and credentials to authenticate devices.
--
-- For more information about how AWS IoT works, see the <http://docs.aws.amazon.com/iot/latest/developerguide/aws-iot-how-it-works.html Developer Guide> .
--
module Network.AWS.IoT
    (
    -- * Service Configuration
      ioT

    -- * Errors
    -- $errors

    -- ** CertificateConflictException
    , _CertificateConflictException

    -- ** SqlParseException
    , _SqlParseException

    -- ** IndexNotReadyException
    , _IndexNotReadyException

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** TransferConflictException
    , _TransferConflictException

    -- ** CertificateStateException
    , _CertificateStateException

    -- ** InvalidResponseException
    , _InvalidResponseException

    -- ** RegistrationCodeValidationException
    , _RegistrationCodeValidationException

    -- ** MalformedPolicyException
    , _MalformedPolicyException

    -- ** DeleteConflictException
    , _DeleteConflictException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** NotConfiguredException
    , _NotConfiguredException

    -- ** CertificateValidationException
    , _CertificateValidationException

    -- ** ResourceRegistrationFailureException
    , _ResourceRegistrationFailureException

    -- ** InvalidQueryException
    , _InvalidQueryException

    -- ** TransferAlreadyCompletedException
    , _TransferAlreadyCompletedException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** ConflictingResourceUpdateException
    , _ConflictingResourceUpdateException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** VersionsLimitExceededException
    , _VersionsLimitExceededException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** InternalException
    , _InternalException

    -- ** VersionConflictException
    , _VersionConflictException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListPolicies (Paginated)
    , module Network.AWS.IoT.ListPolicies

    -- ** CreatePolicy
    , module Network.AWS.IoT.CreatePolicy

    -- ** RegisterCertificate
    , module Network.AWS.IoT.RegisterCertificate

    -- ** ListThingPrincipals
    , module Network.AWS.IoT.ListThingPrincipals

    -- ** DescribeRoleAlias
    , module Network.AWS.IoT.DescribeRoleAlias

    -- ** CreateOTAUpdate
    , module Network.AWS.IoT.CreateOTAUpdate

    -- ** DescribeDefaultAuthorizer
    , module Network.AWS.IoT.DescribeDefaultAuthorizer

    -- ** ListThingRegistrationTaskReports
    , module Network.AWS.IoT.ListThingRegistrationTaskReports

    -- ** ListPrincipalThings (Paginated)
    , module Network.AWS.IoT.ListPrincipalThings

    -- ** RemoveThingFromThingGroup
    , module Network.AWS.IoT.RemoveThingFromThingGroup

    -- ** DescribeEventConfigurations
    , module Network.AWS.IoT.DescribeEventConfigurations

    -- ** ListThingGroups
    , module Network.AWS.IoT.ListThingGroups

    -- ** DescribeThingRegistrationTask
    , module Network.AWS.IoT.DescribeThingRegistrationTask

    -- ** GetLoggingOptions
    , module Network.AWS.IoT.GetLoggingOptions

    -- ** GetOTAUpdate
    , module Network.AWS.IoT.GetOTAUpdate

    -- ** GetEffectivePolicies
    , module Network.AWS.IoT.GetEffectivePolicies

    -- ** ListThingTypes (Paginated)
    , module Network.AWS.IoT.ListThingTypes

    -- ** SetV2LoggingOptions
    , module Network.AWS.IoT.SetV2LoggingOptions

    -- ** ListThingGroupsForThing
    , module Network.AWS.IoT.ListThingGroupsForThing

    -- ** CreateCertificateFromCSR
    , module Network.AWS.IoT.CreateCertificateFromCSR

    -- ** DeleteThing
    , module Network.AWS.IoT.DeleteThing

    -- ** UpdateThing
    , module Network.AWS.IoT.UpdateThing

    -- ** StartThingRegistrationTask
    , module Network.AWS.IoT.StartThingRegistrationTask

    -- ** ListAuthorizers
    , module Network.AWS.IoT.ListAuthorizers

    -- ** ListJobExecutionsForJob
    , module Network.AWS.IoT.ListJobExecutionsForJob

    -- ** SearchIndex
    , module Network.AWS.IoT.SearchIndex

    -- ** CreateThingType
    , module Network.AWS.IoT.CreateThingType

    -- ** DeleteV2LoggingLevel
    , module Network.AWS.IoT.DeleteV2LoggingLevel

    -- ** SetDefaultAuthorizer
    , module Network.AWS.IoT.SetDefaultAuthorizer

    -- ** DescribeJobExecution
    , module Network.AWS.IoT.DescribeJobExecution

    -- ** CancelCertificateTransfer
    , module Network.AWS.IoT.CancelCertificateTransfer

    -- ** GetIndexingConfiguration
    , module Network.AWS.IoT.GetIndexingConfiguration

    -- ** DeleteRoleAlias
    , module Network.AWS.IoT.DeleteRoleAlias

    -- ** UpdateRoleAlias
    , module Network.AWS.IoT.UpdateRoleAlias

    -- ** DeletePolicyVersion
    , module Network.AWS.IoT.DeletePolicyVersion

    -- ** DisableTopicRule
    , module Network.AWS.IoT.DisableTopicRule

    -- ** CreateTopicRule
    , module Network.AWS.IoT.CreateTopicRule

    -- ** CreateJob
    , module Network.AWS.IoT.CreateJob

    -- ** DescribeIndex
    , module Network.AWS.IoT.DescribeIndex

    -- ** AssociateTargetsWithJob
    , module Network.AWS.IoT.AssociateTargetsWithJob

    -- ** ListAttachedPolicies
    , module Network.AWS.IoT.ListAttachedPolicies

    -- ** CreatePolicyVersion
    , module Network.AWS.IoT.CreatePolicyVersion

    -- ** ListCACertificates (Paginated)
    , module Network.AWS.IoT.ListCACertificates

    -- ** DeleteTopicRule
    , module Network.AWS.IoT.DeleteTopicRule

    -- ** GetJobDocument
    , module Network.AWS.IoT.GetJobDocument

    -- ** CreateRoleAlias
    , module Network.AWS.IoT.CreateRoleAlias

    -- ** DeleteCACertificate
    , module Network.AWS.IoT.DeleteCACertificate

    -- ** UpdateCACertificate
    , module Network.AWS.IoT.UpdateCACertificate

    -- ** ListTopicRules (Paginated)
    , module Network.AWS.IoT.ListTopicRules

    -- ** TransferCertificate
    , module Network.AWS.IoT.TransferCertificate

    -- ** ListJobs
    , module Network.AWS.IoT.ListJobs

    -- ** ListRoleAliases
    , module Network.AWS.IoT.ListRoleAliases

    -- ** DescribeThingGroup
    , module Network.AWS.IoT.DescribeThingGroup

    -- ** GetTopicRule
    , module Network.AWS.IoT.GetTopicRule

    -- ** DescribeThing
    , module Network.AWS.IoT.DescribeThing

    -- ** DeletePolicy
    , module Network.AWS.IoT.DeletePolicy

    -- ** ListThingsInThingGroup
    , module Network.AWS.IoT.ListThingsInThingGroup

    -- ** ListCertificates (Paginated)
    , module Network.AWS.IoT.ListCertificates

    -- ** DescribeAuthorizer
    , module Network.AWS.IoT.DescribeAuthorizer

    -- ** GetPolicyVersion
    , module Network.AWS.IoT.GetPolicyVersion

    -- ** DeleteCertificate
    , module Network.AWS.IoT.DeleteCertificate

    -- ** UpdateCertificate
    , module Network.AWS.IoT.UpdateCertificate

    -- ** UpdateIndexingConfiguration
    , module Network.AWS.IoT.UpdateIndexingConfiguration

    -- ** TestInvokeAuthorizer
    , module Network.AWS.IoT.TestInvokeAuthorizer

    -- ** CreateThingGroup
    , module Network.AWS.IoT.CreateThingGroup

    -- ** DetachPolicy
    , module Network.AWS.IoT.DetachPolicy

    -- ** DescribeJob
    , module Network.AWS.IoT.DescribeJob

    -- ** DeleteThingGroup
    , module Network.AWS.IoT.DeleteThingGroup

    -- ** UpdateThingGroup
    , module Network.AWS.IoT.UpdateThingGroup

    -- ** ListOTAUpdates
    , module Network.AWS.IoT.ListOTAUpdates

    -- ** DeleteOTAUpdate
    , module Network.AWS.IoT.DeleteOTAUpdate

    -- ** ListOutgoingCertificates (Paginated)
    , module Network.AWS.IoT.ListOutgoingCertificates

    -- ** DescribeCACertificate
    , module Network.AWS.IoT.DescribeCACertificate

    -- ** GetRegistrationCode
    , module Network.AWS.IoT.GetRegistrationCode

    -- ** DeleteThingType
    , module Network.AWS.IoT.DeleteThingType

    -- ** AddThingToThingGroup
    , module Network.AWS.IoT.AddThingToThingGroup

    -- ** ListCertificatesByCA (Paginated)
    , module Network.AWS.IoT.ListCertificatesByCA

    -- ** AttachThingPrincipal
    , module Network.AWS.IoT.AttachThingPrincipal

    -- ** ListThings (Paginated)
    , module Network.AWS.IoT.ListThings

    -- ** RegisterThing
    , module Network.AWS.IoT.RegisterThing

    -- ** DeleteRegistrationCode
    , module Network.AWS.IoT.DeleteRegistrationCode

    -- ** UpdateStream
    , module Network.AWS.IoT.UpdateStream

    -- ** DeleteStream
    , module Network.AWS.IoT.DeleteStream

    -- ** ListStreams
    , module Network.AWS.IoT.ListStreams

    -- ** CreateAuthorizer
    , module Network.AWS.IoT.CreateAuthorizer

    -- ** TestAuthorization
    , module Network.AWS.IoT.TestAuthorization

    -- ** ListIndices
    , module Network.AWS.IoT.ListIndices

    -- ** UpdateAuthorizer
    , module Network.AWS.IoT.UpdateAuthorizer

    -- ** DeleteAuthorizer
    , module Network.AWS.IoT.DeleteAuthorizer

    -- ** CreateThing
    , module Network.AWS.IoT.CreateThing

    -- ** CreateStream
    , module Network.AWS.IoT.CreateStream

    -- ** ListV2LoggingLevels
    , module Network.AWS.IoT.ListV2LoggingLevels

    -- ** StopThingRegistrationTask
    , module Network.AWS.IoT.StopThingRegistrationTask

    -- ** DescribeCertificate
    , module Network.AWS.IoT.DescribeCertificate

    -- ** ListTargetsForPolicy
    , module Network.AWS.IoT.ListTargetsForPolicy

    -- ** ClearDefaultAuthorizer
    , module Network.AWS.IoT.ClearDefaultAuthorizer

    -- ** ReplaceTopicRule
    , module Network.AWS.IoT.ReplaceTopicRule

    -- ** SetDefaultPolicyVersion
    , module Network.AWS.IoT.SetDefaultPolicyVersion

    -- ** ListPolicyVersions
    , module Network.AWS.IoT.ListPolicyVersions

    -- ** SetV2LoggingLevel
    , module Network.AWS.IoT.SetV2LoggingLevel

    -- ** ListJobExecutionsForThing
    , module Network.AWS.IoT.ListJobExecutionsForThing

    -- ** AttachPolicy
    , module Network.AWS.IoT.AttachPolicy

    -- ** CreateKeysAndCertificate
    , module Network.AWS.IoT.CreateKeysAndCertificate

    -- ** UpdateThingGroupsForThing
    , module Network.AWS.IoT.UpdateThingGroupsForThing

    -- ** EnableTopicRule
    , module Network.AWS.IoT.EnableTopicRule

    -- ** AcceptCertificateTransfer
    , module Network.AWS.IoT.AcceptCertificateTransfer

    -- ** GetPolicy
    , module Network.AWS.IoT.GetPolicy

    -- ** DescribeEndpoint
    , module Network.AWS.IoT.DescribeEndpoint

    -- ** UpdateEventConfigurations
    , module Network.AWS.IoT.UpdateEventConfigurations

    -- ** RegisterCACertificate
    , module Network.AWS.IoT.RegisterCACertificate

    -- ** SetLoggingOptions
    , module Network.AWS.IoT.SetLoggingOptions

    -- ** DescribeThingType
    , module Network.AWS.IoT.DescribeThingType

    -- ** GetV2LoggingOptions
    , module Network.AWS.IoT.GetV2LoggingOptions

    -- ** ListThingRegistrationTasks
    , module Network.AWS.IoT.ListThingRegistrationTasks

    -- ** RejectCertificateTransfer
    , module Network.AWS.IoT.RejectCertificateTransfer

    -- ** DescribeStream
    , module Network.AWS.IoT.DescribeStream

    -- ** DetachThingPrincipal
    , module Network.AWS.IoT.DetachThingPrincipal

    -- ** CancelJob
    , module Network.AWS.IoT.CancelJob

    -- ** DeprecateThingType
    , module Network.AWS.IoT.DeprecateThingType

    -- * Types

    -- ** ActionType
    , ActionType (..)

    -- ** AuthDecision
    , AuthDecision (..)

    -- ** AuthorizerStatus
    , AuthorizerStatus (..)

    -- ** AutoRegistrationStatus
    , AutoRegistrationStatus (..)

    -- ** CACertificateStatus
    , CACertificateStatus (..)

    -- ** CannedAccessControlList
    , CannedAccessControlList (..)

    -- ** CertificateStatus
    , CertificateStatus (..)

    -- ** DynamoKeyType
    , DynamoKeyType (..)

    -- ** EventType
    , EventType (..)

    -- ** IndexStatus
    , IndexStatus (..)

    -- ** JobExecutionStatus
    , JobExecutionStatus (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** LogLevel
    , LogLevel (..)

    -- ** LogTargetType
    , LogTargetType (..)

    -- ** MessageFormat
    , MessageFormat (..)

    -- ** OTAUpdateStatus
    , OTAUpdateStatus (..)

    -- ** ReportType
    , ReportType (..)

    -- ** TargetSelection
    , TargetSelection (..)

    -- ** TaskStatus
    , TaskStatus (..)

    -- ** ThingIndexingMode
    , ThingIndexingMode (..)

    -- ** Action
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

    -- ** Allowed
    , Allowed
    , allowed
    , aPolicies

    -- ** AttributePayload
    , AttributePayload
    , attributePayload
    , apAttributes
    , apMerge

    -- ** AuthInfo
    , AuthInfo
    , authInfo
    , aiResources
    , aiActionType

    -- ** AuthResult
    , AuthResult
    , authResult
    , arDenied
    , arAuthDecision
    , arAllowed
    , arMissingContextValues
    , arAuthInfo

    -- ** AuthorizerDescription
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

    -- ** AuthorizerSummary
    , AuthorizerSummary
    , authorizerSummary
    , asAuthorizerName
    , asAuthorizerARN

    -- ** CACertificate
    , CACertificate
    , cACertificate
    , cacStatus
    , cacCertificateARN
    , cacCertificateId
    , cacCreationDate

    -- ** CACertificateDescription
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

    -- ** Certificate
    , Certificate
    , certificate
    , cStatus
    , cCertificateARN
    , cCertificateId
    , cCreationDate

    -- ** CertificateDescription
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

    -- ** CloudwatchAlarmAction
    , CloudwatchAlarmAction
    , cloudwatchAlarmAction
    , caaRoleARN
    , caaAlarmName
    , caaStateReason
    , caaStateValue

    -- ** CloudwatchMetricAction
    , CloudwatchMetricAction
    , cloudwatchMetricAction
    , cmaMetricTimestamp
    , cmaRoleARN
    , cmaMetricNamespace
    , cmaMetricName
    , cmaMetricValue
    , cmaMetricUnit

    -- ** CodeSigning
    , CodeSigning
    , codeSigning
    , csCustomCodeSigning
    , csAwsSignerJobId

    -- ** CodeSigningCertificateChain
    , CodeSigningCertificateChain
    , codeSigningCertificateChain
    , csccStream
    , csccCertificateName
    , csccInlineDocument

    -- ** CodeSigningSignature
    , CodeSigningSignature
    , codeSigningSignature
    , cssStream
    , cssInlineDocument

    -- ** Configuration
    , Configuration
    , configuration
    , cEnabled

    -- ** CustomCodeSigning
    , CustomCodeSigning
    , customCodeSigning
    , ccsSignature
    , ccsHashAlgorithm
    , ccsCertificateChain
    , ccsSignatureAlgorithm

    -- ** Denied
    , Denied
    , denied
    , dImplicitDeny
    , dExplicitDeny

    -- ** DynamoDBAction
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

    -- ** DynamoDBv2Action
    , DynamoDBv2Action
    , dynamoDBv2Action
    , ddaPutItem
    , ddaRoleARN

    -- ** EffectivePolicy
    , EffectivePolicy
    , effectivePolicy
    , epPolicyName
    , epPolicyDocument
    , epPolicyARN

    -- ** ElasticsearchAction
    , ElasticsearchAction
    , elasticsearchAction
    , eaRoleARN
    , eaEndpoint
    , eaIndex
    , eaType
    , eaId

    -- ** ErrorInfo
    , ErrorInfo
    , errorInfo
    , eiCode
    , eiMessage

    -- ** ExplicitDeny
    , ExplicitDeny
    , explicitDeny
    , edPolicies

    -- ** FirehoseAction
    , FirehoseAction
    , firehoseAction
    , faSeparator
    , faRoleARN
    , faDeliveryStreamName

    -- ** GroupNameAndARN
    , GroupNameAndARN
    , groupNameAndARN
    , gnaaGroupARN
    , gnaaGroupName

    -- ** ImplicitDeny
    , ImplicitDeny
    , implicitDeny
    , idPolicies

    -- ** IotAnalyticsAction
    , IotAnalyticsAction
    , iotAnalyticsAction
    , iaaChannelARN
    , iaaChannelName
    , iaaRoleARN

    -- ** Job
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

    -- ** JobExecution
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

    -- ** JobExecutionStatusDetails
    , JobExecutionStatusDetails
    , jobExecutionStatusDetails
    , jesdDetailsMap

    -- ** JobExecutionSummary
    , JobExecutionSummary
    , jobExecutionSummary
    , jesStatus
    , jesLastUpdatedAt
    , jesQueuedAt
    , jesExecutionNumber
    , jesStartedAt

    -- ** JobExecutionSummaryForJob
    , JobExecutionSummaryForJob
    , jobExecutionSummaryForJob
    , jesfjJobExecutionSummary
    , jesfjThingARN

    -- ** JobExecutionSummaryForThing
    , JobExecutionSummaryForThing
    , jobExecutionSummaryForThing
    , jesftJobId
    , jesftJobExecutionSummary

    -- ** JobExecutionsRolloutConfig
    , JobExecutionsRolloutConfig
    , jobExecutionsRolloutConfig
    , jercMaximumPerMinute

    -- ** JobProcessDetails
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

    -- ** JobSummary
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

    -- ** KeyPair
    , KeyPair
    , keyPair
    , kpPrivateKey
    , kpPublicKey

    -- ** KinesisAction
    , KinesisAction
    , kinesisAction
    , kaPartitionKey
    , kaRoleARN
    , kaStreamName

    -- ** LambdaAction
    , LambdaAction
    , lambdaAction
    , laFunctionARN

    -- ** LogTarget
    , LogTarget
    , logTarget
    , ltTargetName
    , ltTargetType

    -- ** LogTargetConfiguration
    , LogTargetConfiguration
    , logTargetConfiguration
    , ltcLogLevel
    , ltcLogTarget

    -- ** LoggingOptionsPayload
    , LoggingOptionsPayload
    , loggingOptionsPayload
    , lopLogLevel
    , lopRoleARN

    -- ** OTAUpdateFile
    , OTAUpdateFile
    , oTAUpdateFile
    , otaufFileVersion
    , otaufAttributes
    , otaufFileSource
    , otaufCodeSigning
    , otaufFileName

    -- ** OTAUpdateInfo
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

    -- ** OTAUpdateSummary
    , OTAUpdateSummary
    , oTAUpdateSummary
    , otausCreationDate
    , otausOtaUpdateId
    , otausOtaUpdateARN

    -- ** OutgoingCertificate
    , OutgoingCertificate
    , outgoingCertificate
    , ocTransferDate
    , ocCertificateARN
    , ocCertificateId
    , ocTransferredTo
    , ocCreationDate
    , ocTransferMessage

    -- ** Policy
    , Policy
    , policy
    , pPolicyName
    , pPolicyARN

    -- ** PolicyVersion
    , PolicyVersion
    , policyVersion
    , pvVersionId
    , pvCreateDate
    , pvIsDefaultVersion

    -- ** PresignedURLConfig
    , PresignedURLConfig
    , presignedURLConfig
    , pucExpiresInSec
    , pucRoleARN

    -- ** PutItemInput
    , PutItemInput
    , putItemInput
    , piiTableName

    -- ** RegistrationConfig
    , RegistrationConfig
    , registrationConfig
    , rcTemplateBody
    , rcRoleARN

    -- ** RepublishAction
    , RepublishAction
    , republishAction
    , raRoleARN
    , raTopic

    -- ** RoleAliasDescription
    , RoleAliasDescription
    , roleAliasDescription
    , radRoleAliasARN
    , radLastModifiedDate
    , radRoleAlias
    , radOwner
    , radCreationDate
    , radCredentialDurationSeconds
    , radRoleARN

    -- ** S3Action
    , S3Action
    , s3Action
    , sCannedACL
    , sRoleARN
    , sBucketName
    , sKey

    -- ** S3Location
    , S3Location
    , s3Location
    , slVersion
    , slBucket
    , slKey

    -- ** SNSAction
    , SNSAction
    , snsAction
    , snsaMessageFormat
    , snsaTargetARN
    , snsaRoleARN

    -- ** SalesforceAction
    , SalesforceAction
    , salesforceAction
    , saToken
    , saUrl

    -- ** SqsAction
    , SqsAction
    , sqsAction
    , saUseBase64
    , saRoleARN
    , saQueueURL

    -- ** Stream
    , Stream
    , stream
    , sFileId
    , sStreamId

    -- ** StreamFile
    , StreamFile
    , streamFile
    , sfS3Location
    , sfFileId

    -- ** StreamInfo
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

    -- ** StreamSummary
    , StreamSummary
    , streamSummary
    , ssStreamVersion
    , ssStreamARN
    , ssDescription
    , ssStreamId

    -- ** ThingAttribute
    , ThingAttribute
    , thingAttribute
    , taThingTypeName
    , taThingARN
    , taAttributes
    , taVersion
    , taThingName

    -- ** ThingDocument
    , ThingDocument
    , thingDocument
    , tdThingGroupNames
    , tdThingTypeName
    , tdShadow
    , tdAttributes
    , tdThingName
    , tdThingId

    -- ** ThingGroupMetadata
    , ThingGroupMetadata
    , thingGroupMetadata
    , tgmRootToParentThingGroups
    , tgmParentGroupName
    , tgmCreationDate

    -- ** ThingGroupProperties
    , ThingGroupProperties
    , thingGroupProperties
    , tgpAttributePayload
    , tgpThingGroupDescription

    -- ** ThingIndexingConfiguration
    , ThingIndexingConfiguration
    , thingIndexingConfiguration
    , ticThingIndexingMode

    -- ** ThingTypeDefinition
    , ThingTypeDefinition
    , thingTypeDefinition
    , ttdThingTypeProperties
    , ttdThingTypeName
    , ttdThingTypeMetadata
    , ttdThingTypeARN

    -- ** ThingTypeMetadata
    , ThingTypeMetadata
    , thingTypeMetadata
    , ttmDeprecationDate
    , ttmCreationDate
    , ttmDeprecated

    -- ** ThingTypeProperties
    , ThingTypeProperties
    , thingTypeProperties
    , ttpSearchableAttributes
    , ttpThingTypeDescription

    -- ** TopicRule
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

    -- ** TopicRuleListItem
    , TopicRuleListItem
    , topicRuleListItem
    , trliCreatedAt
    , trliRuleDisabled
    , trliRuleName
    , trliRuleARN
    , trliTopicPattern

    -- ** TopicRulePayload
    , TopicRulePayload
    , topicRulePayload
    , trpAwsIotSqlVersion
    , trpErrorAction
    , trpRuleDisabled
    , trpDescription
    , trpSql
    , trpActions

    -- ** TransferData
    , TransferData
    , transferData
    , tdTransferDate
    , tdAcceptDate
    , tdTransferMessage
    , tdRejectDate
    , tdRejectReason
    ) where

import Network.AWS.IoT.AcceptCertificateTransfer
import Network.AWS.IoT.AddThingToThingGroup
import Network.AWS.IoT.AssociateTargetsWithJob
import Network.AWS.IoT.AttachPolicy
import Network.AWS.IoT.AttachThingPrincipal
import Network.AWS.IoT.CancelCertificateTransfer
import Network.AWS.IoT.CancelJob
import Network.AWS.IoT.ClearDefaultAuthorizer
import Network.AWS.IoT.CreateAuthorizer
import Network.AWS.IoT.CreateCertificateFromCSR
import Network.AWS.IoT.CreateJob
import Network.AWS.IoT.CreateKeysAndCertificate
import Network.AWS.IoT.CreateOTAUpdate
import Network.AWS.IoT.CreatePolicy
import Network.AWS.IoT.CreatePolicyVersion
import Network.AWS.IoT.CreateRoleAlias
import Network.AWS.IoT.CreateStream
import Network.AWS.IoT.CreateThing
import Network.AWS.IoT.CreateThingGroup
import Network.AWS.IoT.CreateThingType
import Network.AWS.IoT.CreateTopicRule
import Network.AWS.IoT.DeleteAuthorizer
import Network.AWS.IoT.DeleteCACertificate
import Network.AWS.IoT.DeleteCertificate
import Network.AWS.IoT.DeleteOTAUpdate
import Network.AWS.IoT.DeletePolicy
import Network.AWS.IoT.DeletePolicyVersion
import Network.AWS.IoT.DeleteRegistrationCode
import Network.AWS.IoT.DeleteRoleAlias
import Network.AWS.IoT.DeleteStream
import Network.AWS.IoT.DeleteThing
import Network.AWS.IoT.DeleteThingGroup
import Network.AWS.IoT.DeleteThingType
import Network.AWS.IoT.DeleteTopicRule
import Network.AWS.IoT.DeleteV2LoggingLevel
import Network.AWS.IoT.DeprecateThingType
import Network.AWS.IoT.DescribeAuthorizer
import Network.AWS.IoT.DescribeCACertificate
import Network.AWS.IoT.DescribeCertificate
import Network.AWS.IoT.DescribeDefaultAuthorizer
import Network.AWS.IoT.DescribeEndpoint
import Network.AWS.IoT.DescribeEventConfigurations
import Network.AWS.IoT.DescribeIndex
import Network.AWS.IoT.DescribeJob
import Network.AWS.IoT.DescribeJobExecution
import Network.AWS.IoT.DescribeRoleAlias
import Network.AWS.IoT.DescribeStream
import Network.AWS.IoT.DescribeThing
import Network.AWS.IoT.DescribeThingGroup
import Network.AWS.IoT.DescribeThingRegistrationTask
import Network.AWS.IoT.DescribeThingType
import Network.AWS.IoT.DetachPolicy
import Network.AWS.IoT.DetachThingPrincipal
import Network.AWS.IoT.DisableTopicRule
import Network.AWS.IoT.EnableTopicRule
import Network.AWS.IoT.GetEffectivePolicies
import Network.AWS.IoT.GetIndexingConfiguration
import Network.AWS.IoT.GetJobDocument
import Network.AWS.IoT.GetLoggingOptions
import Network.AWS.IoT.GetOTAUpdate
import Network.AWS.IoT.GetPolicy
import Network.AWS.IoT.GetPolicyVersion
import Network.AWS.IoT.GetRegistrationCode
import Network.AWS.IoT.GetTopicRule
import Network.AWS.IoT.GetV2LoggingOptions
import Network.AWS.IoT.ListAttachedPolicies
import Network.AWS.IoT.ListAuthorizers
import Network.AWS.IoT.ListCACertificates
import Network.AWS.IoT.ListCertificates
import Network.AWS.IoT.ListCertificatesByCA
import Network.AWS.IoT.ListIndices
import Network.AWS.IoT.ListJobExecutionsForJob
import Network.AWS.IoT.ListJobExecutionsForThing
import Network.AWS.IoT.ListJobs
import Network.AWS.IoT.ListOTAUpdates
import Network.AWS.IoT.ListOutgoingCertificates
import Network.AWS.IoT.ListPolicies
import Network.AWS.IoT.ListPolicyVersions
import Network.AWS.IoT.ListPrincipalThings
import Network.AWS.IoT.ListRoleAliases
import Network.AWS.IoT.ListStreams
import Network.AWS.IoT.ListTargetsForPolicy
import Network.AWS.IoT.ListThingGroups
import Network.AWS.IoT.ListThingGroupsForThing
import Network.AWS.IoT.ListThingPrincipals
import Network.AWS.IoT.ListThingRegistrationTaskReports
import Network.AWS.IoT.ListThingRegistrationTasks
import Network.AWS.IoT.ListThings
import Network.AWS.IoT.ListThingsInThingGroup
import Network.AWS.IoT.ListThingTypes
import Network.AWS.IoT.ListTopicRules
import Network.AWS.IoT.ListV2LoggingLevels
import Network.AWS.IoT.RegisterCACertificate
import Network.AWS.IoT.RegisterCertificate
import Network.AWS.IoT.RegisterThing
import Network.AWS.IoT.RejectCertificateTransfer
import Network.AWS.IoT.RemoveThingFromThingGroup
import Network.AWS.IoT.ReplaceTopicRule
import Network.AWS.IoT.SearchIndex
import Network.AWS.IoT.SetDefaultAuthorizer
import Network.AWS.IoT.SetDefaultPolicyVersion
import Network.AWS.IoT.SetLoggingOptions
import Network.AWS.IoT.SetV2LoggingLevel
import Network.AWS.IoT.SetV2LoggingOptions
import Network.AWS.IoT.StartThingRegistrationTask
import Network.AWS.IoT.StopThingRegistrationTask
import Network.AWS.IoT.TestAuthorization
import Network.AWS.IoT.TestInvokeAuthorizer
import Network.AWS.IoT.TransferCertificate
import Network.AWS.IoT.Types
import Network.AWS.IoT.UpdateAuthorizer
import Network.AWS.IoT.UpdateCACertificate
import Network.AWS.IoT.UpdateCertificate
import Network.AWS.IoT.UpdateEventConfigurations
import Network.AWS.IoT.UpdateIndexingConfiguration
import Network.AWS.IoT.UpdateRoleAlias
import Network.AWS.IoT.UpdateStream
import Network.AWS.IoT.UpdateThing
import Network.AWS.IoT.UpdateThingGroup
import Network.AWS.IoT.UpdateThingGroupsForThing
import Network.AWS.IoT.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'IoT'.
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
