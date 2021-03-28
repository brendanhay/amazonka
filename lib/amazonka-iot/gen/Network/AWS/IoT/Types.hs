-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _TaskAlreadyExistsException
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
    , _InvalidAggregationException
    , _ConflictingResourceUpdateException
    , _InternalFailureException
    , _VersionsLimitExceededException
    , _ServiceUnavailableException
    , _InternalException
    , _VersionConflictException
    , _UnauthorizedException
    , _InvalidStateTransitionException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * AlarmName
    , AlarmName (..)

    -- * PayloadVersion
    , PayloadVersion (..)

    -- * TimestreamTimestampValue
    , TimestreamTimestampValue (..)

    -- * RegistrationCode
    , RegistrationCode (..)

    -- * TaskStatus
    , TaskStatus (..)

    -- * AwsJobExponentialRolloutRate
    , AwsJobExponentialRolloutRate (..)
    , mkAwsJobExponentialRolloutRate
    , ajerrBaseRatePerMinute
    , ajerrIncrementFactor
    , ajerrRateIncreaseCriteria

    -- * ConfirmationToken
    , ConfirmationToken (..)

    -- * RegistryS3BucketName
    , RegistryS3BucketName (..)

    -- * AuditSuppression
    , AuditSuppression (..)
    , mkAuditSuppression
    , asCheckName
    , asResourceIdentifier
    , asDescription
    , asExpirationDate
    , asSuppressIndefinitely

    -- * RoleAliasDescription
    , RoleAliasDescription (..)
    , mkRoleAliasDescription
    , radCreationDate
    , radCredentialDurationSeconds
    , radLastModifiedDate
    , radOwner
    , radRoleAlias
    , radRoleAliasArn
    , radRoleArn

    -- * DeviceDefenderThingName
    , DeviceDefenderThingName (..)

    -- * TargetArn
    , TargetArn (..)

    -- * Stream
    , Stream (..)
    , mkStream
    , sFileId
    , sStreamId

    -- * EnableIoTLoggingParams
    , EnableIoTLoggingParams (..)
    , mkEnableIoTLoggingParams
    , eitlpRoleArnForLogging
    , eitlpLogLevel

    -- * Destination
    , Destination (..)
    , mkDestination
    , dS3Destination

    -- * KeyValue
    , KeyValue (..)

    -- * ClientId
    , ClientId (..)

    -- * ResourceLogicalId
    , ResourceLogicalId (..)

    -- * HeaderValue
    , HeaderValue (..)

    -- * HttpUrlDestinationConfiguration
    , HttpUrlDestinationConfiguration (..)
    , mkHttpUrlDestinationConfiguration
    , hudcConfirmationUrl

    -- * AuditMitigationActionsTaskTarget
    , AuditMitigationActionsTaskTarget (..)
    , mkAuditMitigationActionsTaskTarget
    , amattAuditCheckToReasonCodeFilter
    , amattAuditTaskId
    , amattFindingIds

    -- * OTAUpdateSummary
    , OTAUpdateSummary (..)
    , mkOTAUpdateSummary
    , otausCreationDate
    , otausOtaUpdateArn
    , otausOtaUpdateId

    -- * HttpQueryString
    , HttpQueryString (..)

    -- * TopicRuleDestinationStatus
    , TopicRuleDestinationStatus (..)

    -- * AcmCertificateArn
    , AcmCertificateArn (..)

    -- * PolicyDocument
    , PolicyDocument (..)

    -- * CertificateValidity
    , CertificateValidity (..)
    , mkCertificateValidity
    , cvNotAfter
    , cvNotBefore

    -- * TemplateName
    , TemplateName (..)

    -- * DomainConfigurationName
    , DomainConfigurationName (..)

    -- * AlertTarget
    , AlertTarget (..)
    , mkAlertTarget
    , atAlertTargetArn
    , atRoleArn

    -- * BehaviorCriteria
    , BehaviorCriteria (..)
    , mkBehaviorCriteria
    , bcComparisonOperator
    , bcConsecutiveDatapointsToAlarm
    , bcConsecutiveDatapointsToClear
    , bcDurationSeconds
    , bcStatisticalThreshold
    , bcValue

    -- * Platform
    , Platform (..)

    -- * SigningProfileName
    , SigningProfileName (..)

    -- * IndexSchema
    , IndexSchema (..)

    -- * ProvisioningTemplateVersionSummary
    , ProvisioningTemplateVersionSummary (..)
    , mkProvisioningTemplateVersionSummary
    , ptvsCreationDate
    , ptvsIsDefaultVersion
    , ptvsVersionId

    -- * HashKeyField
    , HashKeyField (..)

    -- * RoleAliasArn
    , RoleAliasArn (..)

    -- * PrincipalId
    , PrincipalId (..)

    -- * AttributeValue
    , AttributeValue (..)

    -- * PolicyName
    , PolicyName (..)

    -- * SqsAction
    , SqsAction (..)
    , mkSqsAction
    , saRoleArn
    , saQueueUrl
    , saUseBase64

    -- * S3Key
    , S3Key (..)

    -- * JobExecutionsRolloutConfig
    , JobExecutionsRolloutConfig (..)
    , mkJobExecutionsRolloutConfig
    , jercExponentialRate
    , jercMaximumPerMinute

    -- * FunctionArn
    , FunctionArn (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * PrivateKey
    , PrivateKey (..)

    -- * AwsJobExecutionsRolloutConfig
    , AwsJobExecutionsRolloutConfig (..)
    , mkAwsJobExecutionsRolloutConfig
    , ajercExponentialRate
    , ajercMaximumPerMinute

    -- * Field
    , Field (..)
    , mkField
    , fName
    , fType

    -- * AssetPropertyIntegerValue
    , AssetPropertyIntegerValue (..)

    -- * StreamDescription
    , StreamDescription (..)

    -- * AddThingsToThingGroupParams
    , AddThingsToThingGroupParams (..)
    , mkAddThingsToThingGroupParams
    , atttgpThingGroupNames
    , atttgpOverrideDynamicGroups

    -- * TimestreamDimensionValue
    , TimestreamDimensionValue (..)

    -- * AwsJobAbortConfig
    , AwsJobAbortConfig (..)
    , mkAwsJobAbortConfig
    , ajacAbortCriteriaList

    -- * CertificateName
    , CertificateName (..)

    -- * SnsTopicArn
    , SnsTopicArn (..)

    -- * S3Version
    , S3Version (..)

    -- * PolicyVersion
    , PolicyVersion (..)
    , mkPolicyVersion
    , pvCreateDate
    , pvIsDefaultVersion
    , pvVersionId

    -- * StreamInfo
    , StreamInfo (..)
    , mkStreamInfo
    , siCreatedAt
    , siDescription
    , siFiles
    , siLastUpdatedAt
    , siRoleArn
    , siStreamArn
    , siStreamId
    , siStreamVersion

    -- * MqttUsername
    , MqttUsername (..)

    -- * ServerName
    , ServerName (..)

    -- * IndexStatus
    , IndexStatus (..)

    -- * CannedAccessControlList
    , CannedAccessControlList (..)

    -- * IotAnalyticsAction
    , IotAnalyticsAction (..)
    , mkIotAnalyticsAction
    , iaaBatchMode
    , iaaChannelArn
    , iaaChannelName
    , iaaRoleArn

    -- * LogLevel
    , LogLevel (..)

    -- * LogTargetName
    , LogTargetName (..)

    -- * ResourceType
    , ResourceType (..)

    -- * StreamFile
    , StreamFile (..)
    , mkStreamFile
    , sfFileId
    , sfS3Location

    -- * CodeSigningSignature
    , CodeSigningSignature (..)
    , mkCodeSigningSignature
    , cssInlineDocument

    -- * MitigationActionName
    , MitigationActionName (..)

    -- * JobId
    , JobId (..)

    -- * UpdateCACertificateParams
    , UpdateCACertificateParams (..)
    , mkUpdateCACertificateParams
    , ucacpAction

    -- * JobArn
    , JobArn (..)

    -- * ProvisioningHook
    , ProvisioningHook (..)
    , mkProvisioningHook
    , phTargetArn
    , phPayloadVersion

    -- * RoleAlias
    , RoleAlias (..)

    -- * AssetPropertyQuality
    , AssetPropertyQuality (..)

    -- * BillingGroupDescription
    , BillingGroupDescription (..)

    -- * EffectivePolicy
    , EffectivePolicy (..)
    , mkEffectivePolicy
    , epPolicyArn
    , epPolicyDocument
    , epPolicyName

    -- * StateMachineName
    , StateMachineName (..)

    -- * OTAUpdateDescription
    , OTAUpdateDescription (..)

    -- * LogTargetType
    , LogTargetType (..)

    -- * AuthorizerStatus
    , AuthorizerStatus (..)

    -- * DetailsKey
    , DetailsKey (..)

    -- * PartitionKey
    , PartitionKey (..)

    -- * AuditTaskId
    , AuditTaskId (..)

    -- * ProvisioningTemplateSummary
    , ProvisioningTemplateSummary (..)
    , mkProvisioningTemplateSummary
    , ptsCreationDate
    , ptsDescription
    , ptsEnabled
    , ptsLastModifiedDate
    , ptsTemplateArn
    , ptsTemplateName

    -- * ThingTypeProperties
    , ThingTypeProperties (..)
    , mkThingTypeProperties
    , ttpSearchableAttributes
    , ttpThingTypeDescription

    -- * ThingGroupIndexingConfiguration
    , ThingGroupIndexingConfiguration (..)
    , mkThingGroupIndexingConfiguration
    , tgicThingGroupIndexingMode
    , tgicCustomFields
    , tgicManagedFields

    -- * AwsIotJobId
    , AwsIotJobId (..)

    -- * ElasticsearchAction
    , ElasticsearchAction (..)
    , mkElasticsearchAction
    , eaRoleArn
    , eaEndpoint
    , eaIndex
    , eaType
    , eaId

    -- * JobExecutionFailureType
    , JobExecutionFailureType (..)

    -- * KeyPair
    , KeyPair (..)
    , mkKeyPair
    , kpPrivateKey
    , kpPublicKey

    -- * CertificatePem
    , CertificatePem (..)

    -- * ServerCertificateStatusDetail
    , ServerCertificateStatusDetail (..)

    -- * MitigationActionType
    , MitigationActionType (..)

    -- * DimensionArn
    , DimensionArn (..)

    -- * AuditTaskMetadata
    , AuditTaskMetadata (..)
    , mkAuditTaskMetadata
    , atmTaskId
    , atmTaskStatus
    , atmTaskType

    -- * ViolationEventType
    , ViolationEventType (..)

    -- * ExecutionNamePrefix
    , ExecutionNamePrefix (..)

    -- * AuditMitigationActionsExecutionStatus
    , AuditMitigationActionsExecutionStatus (..)

    -- * CertificateId
    , CertificateId (..)

    -- * CertificateArn
    , CertificateArn (..)

    -- * SnsAction
    , SnsAction (..)
    , mkSnsAction
    , safTargetArn
    , safRoleArn
    , safMessageFormat

    -- * TaskStatistics
    , TaskStatistics (..)
    , mkTaskStatistics
    , tsCanceledChecks
    , tsCompliantChecks
    , tsFailedChecks
    , tsInProgressChecks
    , tsNonCompliantChecks
    , tsTotalChecks
    , tsWaitingForDataCollectionChecks

    -- * MissingContextValue
    , MissingContextValue (..)

    -- * SecurityProfileTargetMapping
    , SecurityProfileTargetMapping (..)
    , mkSecurityProfileTargetMapping
    , sptmSecurityProfileIdentifier
    , sptmTarget

    -- * TopicRuleListItem
    , TopicRuleListItem (..)
    , mkTopicRuleListItem
    , trliCreatedAt
    , trliRuleArn
    , trliRuleDisabled
    , trliRuleName
    , trliTopicPattern

    -- * AssetPropertyOffsetInNanos
    , AssetPropertyOffsetInNanos (..)

    -- * AuditTaskType
    , AuditTaskType (..)

    -- * QueryVersion
    , QueryVersion (..)

    -- * JobDocument
    , JobDocument (..)

    -- * ThingAttribute
    , ThingAttribute (..)
    , mkThingAttribute
    , taAttributes
    , taThingArn
    , taThingName
    , taThingTypeName
    , taVersion

    -- * UpdateDeviceCertificateParams
    , UpdateDeviceCertificateParams (..)
    , mkUpdateDeviceCertificateParams
    , udcpAction

    -- * TaskId
    , TaskId (..)

    -- * KeyName
    , KeyName (..)

    -- * DynamoDBAction
    , DynamoDBAction (..)
    , mkDynamoDBAction
    , ddbaTableName
    , ddbaRoleArn
    , ddbaHashKeyField
    , ddbaHashKeyValue
    , ddbaHashKeyType
    , ddbaOperation
    , ddbaPayloadField
    , ddbaRangeKeyField
    , ddbaRangeKeyType
    , ddbaRangeKeyValue

    -- * EvaluationStatistic
    , EvaluationStatistic (..)

    -- * MitigationActionId
    , MitigationActionId (..)

    -- * MitigationActionArn
    , MitigationActionArn (..)

    -- * OTAUpdateErrorMessage
    , OTAUpdateErrorMessage (..)

    -- * Prefix
    , Prefix (..)

    -- * TimestreamTableName
    , TimestreamTableName (..)

    -- * ReasonForNonComplianceCode
    , ReasonForNonComplianceCode (..)

    -- * BehaviorMetric
    , BehaviorMetric (..)

    -- * AbortConfig
    , AbortConfig (..)
    , mkAbortConfig
    , acCriteriaList

    -- * StreamSummary
    , StreamSummary (..)
    , mkStreamSummary
    , ssDescription
    , ssStreamArn
    , ssStreamId
    , ssStreamVersion

    -- * Token
    , Token (..)

    -- * TopicRuleDestinationConfiguration
    , TopicRuleDestinationConfiguration (..)
    , mkTopicRuleDestinationConfiguration
    , trdcHttpUrlConfiguration

    -- * ThingTypeName
    , ThingTypeName (..)

    -- * AuditMitigationActionsTaskId
    , AuditMitigationActionsTaskId (..)

    -- * SalesforceAction
    , SalesforceAction (..)
    , mkSalesforceAction
    , saToken
    , saUrl

    -- * AuthorizerConfig
    , AuthorizerConfig (..)
    , mkAuthorizerConfig
    , acAllowAuthorizerOverride
    , acDefaultAuthorizerName

    -- * IotEventsAction
    , IotEventsAction (..)
    , mkIotEventsAction
    , ieaInputName
    , ieaRoleArn
    , ieaBatchMode
    , ieaMessageId

    -- * AuditCheckConfiguration
    , AuditCheckConfiguration (..)
    , mkAuditCheckConfiguration
    , accEnabled

    -- * JobSummary
    , JobSummary (..)
    , mkJobSummary
    , jsCompletedAt
    , jsCreatedAt
    , jsJobArn
    , jsJobId
    , jsLastUpdatedAt
    , jsStatus
    , jsTargetSelection
    , jsThingGroupId

    -- * BehaviorName
    , BehaviorName (..)

    -- * ScheduledAuditName
    , ScheduledAuditName (..)

    -- * AwsIotSqlVersion
    , AwsIotSqlVersion (..)

    -- * Url
    , Url (..)

    -- * AuditMitigationActionsTaskMetadata
    , AuditMitigationActionsTaskMetadata (..)
    , mkAuditMitigationActionsTaskMetadata
    , amatmStartTime
    , amatmTaskId
    , amatmTaskStatus

    -- * TopicRuleDestination
    , TopicRuleDestination (..)
    , mkTopicRuleDestination
    , trdArn
    , trdHttpUrlProperties
    , trdStatus
    , trdStatusReason

    -- * CertificateDescription
    , CertificateDescription (..)
    , mkCertificateDescription
    , cdCaCertificateId
    , cdCertificateArn
    , cdCertificateId
    , cdCertificateMode
    , cdCertificatePem
    , cdCreationDate
    , cdCustomerVersion
    , cdGenerationId
    , cdLastModifiedDate
    , cdOwnedBy
    , cdPreviousOwnedBy
    , cdStatus
    , cdTransferData
    , cdValidity

    -- * Value
    , Value (..)

    -- * ValidationError
    , ValidationError (..)
    , mkValidationError
    , veErrorMessage

    -- * FileLocation
    , FileLocation (..)
    , mkFileLocation
    , flS3Location
    , flStream

    -- * AwsJobAbortCriteriaFailureType
    , AwsJobAbortCriteriaFailureType (..)

    -- * EndpointType
    , EndpointType (..)

    -- * DimensionValueOperator
    , DimensionValueOperator (..)

    -- * AuthorizerFunctionArn
    , AuthorizerFunctionArn (..)

    -- * NamespaceId
    , NamespaceId (..)

    -- * CertificateMode
    , CertificateMode (..)

    -- * Action
    , Action (..)
    , mkAction
    , aCloudwatchAlarm
    , aCloudwatchLogs
    , aCloudwatchMetric
    , aDynamoDB
    , aDynamoDBv2
    , aElasticsearch
    , aFirehose
    , aHttp
    , aIotAnalytics
    , aIotEvents
    , aIotSiteWise
    , aKinesis
    , aLambda
    , aRepublish
    , aS3
    , aSalesforce
    , aSns
    , aSqs
    , aStepFunctions
    , aTimestream

    -- * InlineDocument
    , InlineDocument (..)

    -- * CodeSigningCertificateChain
    , CodeSigningCertificateChain (..)
    , mkCodeSigningCertificateChain
    , csccCertificateName
    , csccInlineDocument

    -- * PublicKey
    , PublicKey (..)

    -- * AssetPropertyTimeInSeconds
    , AssetPropertyTimeInSeconds (..)

    -- * Protocol
    , Protocol (..)

    -- * HttpUrlDestinationProperties
    , HttpUrlDestinationProperties (..)
    , mkHttpUrlDestinationProperties
    , hudpConfirmationUrl

    -- * JobProcessDetails
    , JobProcessDetails (..)
    , mkJobProcessDetails
    , jpdNumberOfCanceledThings
    , jpdNumberOfFailedThings
    , jpdNumberOfInProgressThings
    , jpdNumberOfQueuedThings
    , jpdNumberOfRejectedThings
    , jpdNumberOfRemovedThings
    , jpdNumberOfSucceededThings
    , jpdNumberOfTimedOutThings
    , jpdProcessingTargets

    -- * ThingGroupMetadata
    , ThingGroupMetadata (..)
    , mkThingGroupMetadata
    , tgmCreationDate
    , tgmParentGroupName
    , tgmRootToParentThingGroups

    -- * ThingGroupArn
    , ThingGroupArn (..)

    -- * AwsJobPresignedUrlConfig
    , AwsJobPresignedUrlConfig (..)
    , mkAwsJobPresignedUrlConfig
    , ajpucExpiresInSec

    -- * AssetPropertyEntryId
    , AssetPropertyEntryId (..)

    -- * DynamicGroupStatus
    , DynamicGroupStatus (..)

    -- * TemplateDescription
    , TemplateDescription (..)

    -- * DimensionStringValue
    , DimensionStringValue (..)

    -- * AuditCheckRunStatus
    , AuditCheckRunStatus (..)

    -- * ThingGroupId
    , ThingGroupId (..)

    -- * MitigationActionIdentifier
    , MitigationActionIdentifier (..)
    , mkMitigationActionIdentifier
    , maiActionArn
    , maiActionName
    , maiCreationDate

    -- * AuthorizerName
    , AuthorizerName (..)

    -- * AggregationField
    , AggregationField (..)

    -- * LambdaAction
    , LambdaAction (..)
    , mkLambdaAction
    , laFunctionArn

    -- * CACertificateDescription
    , CACertificateDescription (..)
    , mkCACertificateDescription
    , cacdAutoRegistrationStatus
    , cacdCertificateArn
    , cacdCertificateId
    , cacdCertificatePem
    , cacdCreationDate
    , cacdCustomerVersion
    , cacdGenerationId
    , cacdLastModifiedDate
    , cacdOwnedBy
    , cacdStatus
    , cacdValidity

    -- * ImplicitDeny
    , ImplicitDeny (..)
    , mkImplicitDeny
    , idPolicies

    -- * AssetPropertyAlias
    , AssetPropertyAlias (..)

    -- * ThingArn
    , ThingArn (..)

    -- * AwsAccountId
    , AwsAccountId (..)

    -- * StreamArn
    , StreamArn (..)

    -- * AssetPropertyStringValue
    , AssetPropertyStringValue (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * RuleName
    , RuleName (..)

    -- * AssetPropertyDoubleValue
    , AssetPropertyDoubleValue (..)

    -- * CertificatePathOnDevice
    , CertificatePathOnDevice (..)

    -- * ThingTypeId
    , ThingTypeId (..)

    -- * JobDescription
    , JobDescription (..)

    -- * PutAssetPropertyValueEntry
    , PutAssetPropertyValueEntry (..)
    , mkPutAssetPropertyValueEntry
    , papvePropertyValues
    , papveAssetId
    , papveEntryId
    , papvePropertyAlias
    , papvePropertyId

    -- * JobExecutionSummary
    , JobExecutionSummary (..)
    , mkJobExecutionSummary
    , jesExecutionNumber
    , jesLastUpdatedAt
    , jesQueuedAt
    , jesStartedAt
    , jesStatus

    -- * ThingTypeMetadata
    , ThingTypeMetadata (..)
    , mkThingTypeMetadata
    , ttmCreationDate
    , ttmDeprecated
    , ttmDeprecationDate

    -- * ThingIndexingMode
    , ThingIndexingMode (..)

    -- * BillingGroupArn
    , BillingGroupArn (..)

    -- * InputName
    , InputName (..)

    -- * ReplaceDefaultPolicyVersionParams
    , ReplaceDefaultPolicyVersionParams (..)
    , mkReplaceDefaultPolicyVersionParams
    , rdpvpTemplateName

    -- * MitigationActionParams
    , MitigationActionParams (..)
    , mkMitigationActionParams
    , mapAddThingsToThingGroupParams
    , mapEnableIoTLoggingParams
    , mapPublishFindingToSnsParams
    , mapReplaceDefaultPolicyVersionParams
    , mapUpdateCACertificateParams
    , mapUpdateDeviceCertificateParams

    -- * ReasonCode
    , ReasonCode (..)

    -- * SigningProfileParameter
    , SigningProfileParameter (..)
    , mkSigningProfileParameter
    , sppCertificateArn
    , sppCertificatePathOnDevice
    , sppPlatform

    -- * AuditCheckDetails
    , AuditCheckDetails (..)
    , mkAuditCheckDetails
    , acdCheckCompliant
    , acdCheckRunStatus
    , acdErrorCode
    , acdMessage
    , acdNonCompliantResourcesCount
    , acdSuppressedNonCompliantResourcesCount
    , acdTotalResourcesCount

    -- * PolicyVersionIdentifier
    , PolicyVersionIdentifier (..)
    , mkPolicyVersionIdentifier
    , pviPolicyName
    , pviPolicyVersionId

    -- * Topic
    , Topic (..)

    -- * AuthResult
    , AuthResult (..)
    , mkAuthResult
    , arAllowed
    , arAuthDecision
    , arAuthInfo
    , arDenied
    , arMissingContextValues

    -- * BucketName
    , BucketName (..)

    -- * ScheduledAuditMetadata
    , ScheduledAuditMetadata (..)
    , mkScheduledAuditMetadata
    , samDayOfMonth
    , samDayOfWeek
    , samFrequency
    , samScheduledAuditArn
    , samScheduledAuditName

    -- * PolicyTarget
    , PolicyTarget (..)

    -- * LogGroupName
    , LogGroupName (..)

    -- * ThingConnectivityIndexingMode
    , ThingConnectivityIndexingMode (..)

    -- * HttpHeaderValue
    , HttpHeaderValue (..)

    -- * MetricDimension
    , MetricDimension (..)
    , mkMetricDimension
    , mdDimensionName
    , mdOperator

    -- * PresignedUrlConfig
    , PresignedUrlConfig (..)
    , mkPresignedUrlConfig
    , pucExpiresInSec
    , pucRoleArn

    -- * AuditCheckName
    , AuditCheckName (..)

    -- * Behavior
    , Behavior (..)
    , mkBehavior
    , bName
    , bCriteria
    , bMetric
    , bMetricDimension

    -- * AuthorizerArn
    , AuthorizerArn (..)

    -- * AbortAction
    , AbortAction (..)

    -- * ServerCertificateStatus
    , ServerCertificateStatus (..)

    -- * NextToken
    , NextToken (..)

    -- * ViolationId
    , ViolationId (..)

    -- * ChannelName
    , ChannelName (..)

    -- * Cidr
    , Cidr (..)

    -- * S3Destination
    , S3Destination (..)
    , mkS3Destination
    , sdBucket
    , sdPrefix

    -- * DynamoDBv2Action
    , DynamoDBv2Action (..)
    , mkDynamoDBv2Action
    , dRoleArn
    , dPutItem

    -- * AuditTaskStatus
    , AuditTaskStatus (..)

    -- * StatisticalThreshold
    , StatisticalThreshold (..)
    , mkStatisticalThreshold
    , stStatistic

    -- * ThingGroupDocument
    , ThingGroupDocument (..)
    , mkThingGroupDocument
    , tgdAttributes
    , tgdParentGroupNames
    , tgdThingGroupDescription
    , tgdThingGroupId
    , tgdThingGroupName

    -- * ThingGroupName
    , ThingGroupName (..)

    -- * ServerCertificateSummary
    , ServerCertificateSummary (..)
    , mkServerCertificateSummary
    , scsServerCertificateArn
    , scsServerCertificateStatus
    , scsServerCertificateStatusDetail

    -- * FirehoseSeparator
    , FirehoseSeparator (..)

    -- * EventType
    , EventType (..)

    -- * SigV4Authorization
    , SigV4Authorization (..)
    , mkSigV4Authorization
    , svaSigningRegion
    , svaServiceName
    , svaRoleArn

    -- * PayloadField
    , PayloadField (..)

    -- * DomainConfigurationStatus
    , DomainConfigurationStatus (..)

    -- * SecurityProfileTarget
    , SecurityProfileTarget (..)
    , mkSecurityProfileTarget
    , sptArn

    -- * AssetPropertyValue
    , AssetPropertyValue (..)
    , mkAssetPropertyValue
    , apvValue
    , apvTimestamp
    , apvQuality

    -- * TlsContext
    , TlsContext (..)
    , mkTlsContext
    , tcServerName

    -- * StateValue
    , StateValue (..)

    -- * DomainConfigurationSummary
    , DomainConfigurationSummary (..)
    , mkDomainConfigurationSummary
    , dcsDomainConfigurationArn
    , dcsDomainConfigurationName
    , dcsServiceType

    -- * FirehoseAction
    , FirehoseAction (..)
    , mkFirehoseAction
    , faRoleArn
    , faDeliveryStreamName
    , faBatchMode
    , faSeparator

    -- * TimestreamTimestamp
    , TimestreamTimestamp (..)
    , mkTimestreamTimestamp
    , ttValue
    , ttUnit

    -- * JsonDocument
    , JsonDocument (..)

    -- * OTAUpdateStatus
    , OTAUpdateStatus (..)

    -- * Job
    , Job (..)
    , mkJob
    , jAbortConfig
    , jComment
    , jCompletedAt
    , jCreatedAt
    , jDescription
    , jForceCanceled
    , jJobArn
    , jJobExecutionsRolloutConfig
    , jJobId
    , jJobProcessDetails
    , jLastUpdatedAt
    , jNamespaceId
    , jPresignedUrlConfig
    , jReasonCode
    , jStatus
    , jTargetSelection
    , jTargets
    , jTimeoutConfig

    -- * PrincipalArn
    , PrincipalArn (..)

    -- * TopicRule
    , TopicRule (..)
    , mkTopicRule
    , trActions
    , trAwsIotSqlVersion
    , trCreatedAt
    , trDescription
    , trErrorAction
    , trRuleDisabled
    , trRuleName
    , trSql

    -- * Denied
    , Denied (..)
    , mkDenied
    , dExplicitDeny
    , dImplicitDeny

    -- * OTAUpdateFileVersion
    , OTAUpdateFileVersion (..)

    -- * ResourceArn
    , ResourceArn (..)

    -- * DimensionName
    , DimensionName (..)

    -- * DynamoKeyType
    , DynamoKeyType (..)

    -- * Key
    , Key (..)

    -- * PolicyVersionId
    , PolicyVersionId (..)

    -- * DomainName
    , DomainName (..)

    -- * AuthorizerDescription
    , AuthorizerDescription (..)
    , mkAuthorizerDescription
    , adAuthorizerArn
    , adAuthorizerFunctionArn
    , adAuthorizerName
    , adCreationDate
    , adLastModifiedDate
    , adSigningDisabled
    , adStatus
    , adTokenKeyName
    , adTokenSigningPublicKeys

    -- * AwsJobAbortCriteriaAbortAction
    , AwsJobAbortCriteriaAbortAction (..)

    -- * AuditMitigationActionExecutionMetadata
    , AuditMitigationActionExecutionMetadata (..)
    , mkAuditMitigationActionExecutionMetadata
    , amaemActionId
    , amaemActionName
    , amaemEndTime
    , amaemErrorCode
    , amaemFindingId
    , amaemMessage
    , amaemStartTime
    , amaemStatus
    , amaemTaskId

    -- * TopicRuleDestinationSummary
    , TopicRuleDestinationSummary (..)
    , mkTopicRuleDestinationSummary
    , trdsArn
    , trdsHttpUrlSummary
    , trdsStatus
    , trdsStatusReason

    -- * MetricValue
    , MetricValue (..)
    , mkMetricValue
    , mvCidrs
    , mvCount
    , mvPorts

    -- * AwsJobRateIncreaseCriteria
    , AwsJobRateIncreaseCriteria (..)
    , mkAwsJobRateIncreaseCriteria
    , ajricNumberOfNotifiedThings
    , ajricNumberOfSucceededThings

    -- * CertificateSigningRequest
    , CertificateSigningRequest (..)

    -- * PolicyTemplateName
    , PolicyTemplateName (..)

    -- * HttpContext
    , HttpContext (..)
    , mkHttpContext
    , hcHeaders
    , hcQueryString

    -- * TimestreamAction
    , TimestreamAction (..)
    , mkTimestreamAction
    , taRoleArn
    , taDatabaseName
    , taTableName
    , taDimensions
    , taTimestamp

    -- * QueryString
    , QueryString (..)

    -- * CACertificate
    , CACertificate (..)
    , mkCACertificate
    , cacCertificateArn
    , cacCertificateId
    , cacCreationDate
    , cacStatus

    -- * AlertTargetType
    , AlertTargetType (..)

    -- * IotSiteWiseAction
    , IotSiteWiseAction (..)
    , mkIotSiteWiseAction
    , iswaPutAssetPropertyValueEntries
    , iswaRoleArn

    -- * DeliveryStreamName
    , DeliveryStreamName (..)

    -- * TokenSignature
    , TokenSignature (..)

    -- * Principal
    , Principal (..)

    -- * DeviceCertificateUpdateAction
    , DeviceCertificateUpdateAction (..)

    -- * AttributePayload
    , AttributePayload (..)
    , mkAttributePayload
    , apAttributes
    , apMerge

    -- * CloudwatchMetricAction
    , CloudwatchMetricAction (..)
    , mkCloudwatchMetricAction
    , cmaRoleArn
    , cmaMetricNamespace
    , cmaMetricName
    , cmaMetricValue
    , cmaMetricUnit
    , cmaMetricTimestamp

    -- * RangeKeyField
    , RangeKeyField (..)

    -- * ThingTypeDescription
    , ThingTypeDescription (..)

    -- * FieldType
    , FieldType (..)

    -- * AlertTargetArn
    , AlertTargetArn (..)

    -- * ThingGroupProperties
    , ThingGroupProperties (..)
    , mkThingGroupProperties
    , tgpAttributePayload
    , tgpThingGroupDescription

    -- * NonCompliantResource
    , NonCompliantResource (..)
    , mkNonCompliantResource
    , ncrAdditionalInfo
    , ncrResourceIdentifier
    , ncrResourceType

    -- * AuditMitigationActionsTaskStatus
    , AuditMitigationActionsTaskStatus (..)

    -- * AutoRegistrationStatus
    , AutoRegistrationStatus (..)

    -- * Marker
    , Marker (..)

    -- * ServiceName
    , ServiceName (..)

    -- * ThingIndexingConfiguration
    , ThingIndexingConfiguration (..)
    , mkThingIndexingConfiguration
    , ticThingIndexingMode
    , ticCustomFields
    , ticManagedFields
    , ticThingConnectivityIndexingMode

    -- * MetricToRetain
    , MetricToRetain (..)
    , mkMetricToRetain
    , mtrMetric
    , mtrMetricDimension

    -- * SecurityProfileName
    , SecurityProfileName (..)

    -- * Resource
    , Resource (..)

    -- * PutItemInput
    , PutItemInput (..)
    , mkPutItemInput
    , piiTableName

    -- * QueueUrl
    , QueueUrl (..)

    -- * AwsIotJobArn
    , AwsIotJobArn (..)

    -- * LoggingOptionsPayload
    , LoggingOptionsPayload (..)
    , mkLoggingOptionsPayload
    , lopRoleArn
    , lopLogLevel

    -- * DayOfMonth
    , DayOfMonth (..)

    -- * ErrorCode
    , ErrorCode (..)

    -- * ServiceType
    , ServiceType (..)

    -- * Certificate
    , Certificate (..)
    , mkCertificate
    , cCertificateArn
    , cCertificateId
    , cCertificateMode
    , cCreationDate
    , cStatus

    -- * BillingGroupProperties
    , BillingGroupProperties (..)
    , mkBillingGroupProperties
    , bgpBillingGroupDescription

    -- * LogTargetConfiguration
    , LogTargetConfiguration (..)
    , mkLogTargetConfiguration
    , ltcLogLevel
    , ltcLogTarget

    -- * EndpointAddress
    , EndpointAddress (..)

    -- * JobExecutionSummaryForJob
    , JobExecutionSummaryForJob (..)
    , mkJobExecutionSummaryForJob
    , jesfjJobExecutionSummary
    , jesfjThingArn

    -- * RateIncreaseCriteria
    , RateIncreaseCriteria (..)
    , mkRateIncreaseCriteria
    , ricNumberOfNotifiedThings
    , ricNumberOfSucceededThings

    -- * LogTarget
    , LogTarget (..)
    , mkLogTarget
    , ltTargetType
    , ltTargetName

    -- * ThingGroupDescription
    , ThingGroupDescription (..)

    -- * OTAUpdateFile
    , OTAUpdateFile (..)
    , mkOTAUpdateFile
    , otaufAttributes
    , otaufCodeSigning
    , otaufFileLocation
    , otaufFileName
    , otaufFileType
    , otaufFileVersion

    -- * JobDocumentSource
    , JobDocumentSource (..)

    -- * MitigationAction
    , MitigationAction (..)
    , mkMitigationAction
    , maActionParams
    , maId
    , maName
    , maRoleArn

    -- * PublishFindingToSnsParams
    , PublishFindingToSnsParams (..)
    , mkPublishFindingToSnsParams
    , pftspTopicArn

    -- * StepFunctionsAction
    , StepFunctionsAction (..)
    , mkStepFunctionsAction
    , sfaStateMachineName
    , sfaRoleArn
    , sfaExecutionNamePrefix

    -- * RegistryS3KeyName
    , RegistryS3KeyName (..)

    -- * RelatedResource
    , RelatedResource (..)
    , mkRelatedResource
    , rrAdditionalInfo
    , rrResourceIdentifier
    , rrResourceType

    -- * S3FileUrl
    , S3FileUrl (..)

    -- * TemplateArn
    , TemplateArn (..)

    -- * CustomCodeSigning
    , CustomCodeSigning (..)
    , mkCustomCodeSigning
    , ccsCertificateChain
    , ccsHashAlgorithm
    , ccsSignature
    , ccsSignatureAlgorithm

    -- * AssetId
    , AssetId (..)

    -- * Statistics
    , Statistics (..)
    , mkStatistics
    , sAverage
    , sCount
    , sMaximum
    , sMinimum
    , sStdDeviation
    , sSum
    , sSumOfSquares
    , sVariance

    -- * ActiveViolation
    , ActiveViolation (..)
    , mkActiveViolation
    , avBehavior
    , avLastViolationTime
    , avLastViolationValue
    , avSecurityProfileName
    , avThingName
    , avViolationId
    , avViolationStartTime

    -- * ProcessingTargetName
    , ProcessingTargetName (..)

    -- * OTAUpdateInfo
    , OTAUpdateInfo (..)
    , mkOTAUpdateInfo
    , otauiAdditionalParameters
    , otauiAwsIotJobArn
    , otauiAwsIotJobId
    , otauiAwsJobExecutionsRolloutConfig
    , otauiAwsJobPresignedUrlConfig
    , otauiCreationDate
    , otauiDescription
    , otauiErrorInfo
    , otauiLastModifiedDate
    , otauiOtaUpdateArn
    , otauiOtaUpdateFiles
    , otauiOtaUpdateId
    , otauiOtaUpdateStatus
    , otauiProtocols
    , otauiTargetSelection
    , otauiTargets

    -- * StartSigningJobParameter
    , StartSigningJobParameter (..)
    , mkStartSigningJobParameter
    , ssjpDestination
    , ssjpSigningProfileName
    , ssjpSigningProfileParameter

    -- * CloudwatchAlarmAction
    , CloudwatchAlarmAction (..)
    , mkCloudwatchAlarmAction
    , caaRoleArn
    , caaAlarmName
    , caaStateReason
    , caaStateValue

    -- * PolicyArn
    , PolicyArn (..)

    -- * ViolationEvent
    , ViolationEvent (..)
    , mkViolationEvent
    , veBehavior
    , veMetricValue
    , veSecurityProfileName
    , veThingName
    , veViolationEventTime
    , veViolationEventType
    , veViolationId

    -- * ReasonForNonCompliance
    , ReasonForNonCompliance (..)

    -- * JobExecution
    , JobExecution (..)
    , mkJobExecution
    , jeApproximateSecondsBeforeTimedOut
    , jeExecutionNumber
    , jeForceCanceled
    , jeJobId
    , jeLastUpdatedAt
    , jeQueuedAt
    , jeStartedAt
    , jeStatus
    , jeStatusDetails
    , jeThingArn
    , jeVersionNumber

    -- * Code
    , Code (..)

    -- * AuthorizerSummary
    , AuthorizerSummary (..)
    , mkAuthorizerSummary
    , asAuthorizerArn
    , asAuthorizerName

    -- * TagKey
    , TagKey (..)

    -- * S3Location
    , S3Location (..)
    , mkS3Location
    , slBucket
    , slKey
    , slVersion

    -- * DomainConfigurationArn
    , DomainConfigurationArn (..)

    -- * HashAlgorithm
    , HashAlgorithm (..)

    -- * DomainType
    , DomainType (..)

    -- * MqttClientId
    , MqttClientId (..)

    -- * MessageFormat
    , MessageFormat (..)

    -- * AwsArn
    , AwsArn (..)

    -- * HeaderKey
    , HeaderKey (..)

    -- * FindingId
    , FindingId (..)

    -- * Configuration
    , Configuration (..)
    , mkConfiguration
    , cEnabled

    -- * TaskStatisticsForAuditCheck
    , TaskStatisticsForAuditCheck (..)
    , mkTaskStatisticsForAuditCheck
    , tsfacCanceledFindingsCount
    , tsfacFailedFindingsCount
    , tsfacSkippedFindingsCount
    , tsfacSucceededFindingsCount
    , tsfacTotalFindingsCount

    -- * Policy
    , Policy (..)
    , mkPolicy
    , pPolicyArn
    , pPolicyName

    -- * AssetPropertyVariant
    , AssetPropertyVariant (..)
    , mkAssetPropertyVariant
    , apvBooleanValue
    , apvDoubleValue
    , apvIntegerValue
    , apvStringValue

    -- * AuditFinding
    , AuditFinding (..)
    , mkAuditFinding
    , afCheckName
    , afFindingId
    , afFindingTime
    , afIsSuppressed
    , afNonCompliantResource
    , afReasonForNonCompliance
    , afReasonForNonComplianceCode
    , afRelatedResources
    , afSeverity
    , afTaskId
    , afTaskStartTime

    -- * TemplateBody
    , TemplateBody (..)

    -- * ExplicitDeny
    , ExplicitDeny (..)
    , mkExplicitDeny
    , edPolicies

    -- * HttpHeaderName
    , HttpHeaderName (..)

    -- * RepublishAction
    , RepublishAction (..)
    , mkRepublishAction
    , raRoleArn
    , raTopic
    , raQos

    -- * AuditNotificationType
    , AuditNotificationType (..)

    -- * CognitoIdentityPoolId
    , CognitoIdentityPoolId (..)

    -- * CACertificateUpdateAction
    , CACertificateUpdateAction (..)

    -- * AuthDecision
    , AuthDecision (..)

    -- * DimensionType
    , DimensionType (..)

    -- * Allowed
    , Allowed (..)
    , mkAllowed
    , aPolicies

    -- * SecurityProfileArn
    , SecurityProfileArn (..)

    -- * JobStatus
    , JobStatus (..)

    -- * AuditFindingSeverity
    , AuditFindingSeverity (..)

    -- * BillingGroupName
    , BillingGroupName (..)

    -- * RegistrationConfig
    , RegistrationConfig (..)
    , mkRegistrationConfig
    , rcRoleArn
    , rcTemplateBody

    -- * GenerationId
    , GenerationId (..)

    -- * JobExecutionStatusDetails
    , JobExecutionStatusDetails (..)
    , mkJobExecutionStatusDetails
    , jesdDetailsMap

    -- * AttributeKey
    , AttributeKey (..)

    -- * HttpAuthorization
    , HttpAuthorization (..)
    , mkHttpAuthorization
    , haSigv4

    -- * SigningRegion
    , SigningRegion (..)

    -- * RangeKeyValue
    , RangeKeyValue (..)

    -- * ThingDocument
    , ThingDocument (..)
    , mkThingDocument
    , tdAttributes
    , tdConnectivity
    , tdShadow
    , tdThingGroupNames
    , tdThingId
    , tdThingName
    , tdThingTypeName

    -- * TransferData
    , TransferData (..)
    , mkTransferData
    , tdAcceptDate
    , tdRejectDate
    , tdRejectReason
    , tdTransferDate
    , tdTransferMessage

    -- * TokenKeyName
    , TokenKeyName (..)

    -- * ThingName
    , ThingName (..)

    -- * ErrorMessage
    , ErrorMessage (..)

    -- * CACertificateStatus
    , CACertificateStatus (..)

    -- * StateReason
    , StateReason (..)

    -- * HttpUrlDestinationSummary
    , HttpUrlDestinationSummary (..)
    , mkHttpUrlDestinationSummary
    , hudsConfirmationUrl

    -- * Message
    , Message (..)

    -- * SignatureAlgorithm
    , SignatureAlgorithm (..)

    -- * OutgoingCertificate
    , OutgoingCertificate (..)
    , mkOutgoingCertificate
    , ocCertificateArn
    , ocCertificateId
    , ocCreationDate
    , ocTransferDate
    , ocTransferMessage
    , ocTransferredTo

    -- * SecurityProfileDescription
    , SecurityProfileDescription (..)

    -- * RuleArn
    , RuleArn (..)

    -- * AttributeName
    , AttributeName (..)

    -- * MqttContext
    , MqttContext (..)
    , mkMqttContext
    , mcClientId
    , mcPassword
    , mcUsername

    -- * AwsJobAbortCriteria
    , AwsJobAbortCriteria (..)
    , mkAwsJobAbortCriteria
    , ajacFailureType
    , ajacAction
    , ajacThresholdPercentage
    , ajacMinNumberOfExecutedThings

    -- * StreamName
    , StreamName (..)

    -- * ClientRequestToken
    , ClientRequestToken (..)

    -- * AwsJobTimeoutConfig
    , AwsJobTimeoutConfig (..)
    , mkAwsJobTimeoutConfig
    , ajtcInProgressTimeoutInMinutes

    -- * Comment
    , Comment (..)

    -- * HttpAction
    , HttpAction (..)
    , mkHttpAction
    , haUrl
    , haAuth
    , haConfirmationUrl
    , haHeaders

    -- * ThingGroupIndexingMode
    , ThingGroupIndexingMode (..)

    -- * Description
    , Description (..)

    -- * SalesforceEndpoint
    , SalesforceEndpoint (..)

    -- * JobExecutionSummaryForThing
    , JobExecutionSummaryForThing (..)
    , mkJobExecutionSummaryForThing
    , jesftJobExecutionSummary
    , jesftJobId

    -- * AuthInfo
    , AuthInfo (..)
    , mkAuthInfo
    , aiResources
    , aiActionType

    -- * ErrorInfo
    , ErrorInfo (..)
    , mkErrorInfo
    , eiCode
    , eiMessage

    -- * ReportType
    , ReportType (..)

    -- * CodeSigning
    , CodeSigning (..)
    , mkCodeSigning
    , csAwsSignerJobId
    , csCustomCodeSigning
    , csStartSigningJobParameter

    -- * ExponentialRolloutRate
    , ExponentialRolloutRate (..)
    , mkExponentialRolloutRate
    , errBaseRatePerMinute
    , errIncrementFactor
    , errRateIncreaseCriteria

    -- * GroupNameAndArn
    , GroupNameAndArn (..)
    , mkGroupNameAndArn
    , gnaaGroupArn
    , gnaaGroupName

    -- * ActionType
    , ActionType (..)

    -- * SQL
    , SQL (..)

    -- * S3Action
    , S3Action (..)
    , mkS3Action
    , sRoleArn
    , sBucketName
    , sKey
    , sCannedAcl

    -- * SecurityProfileIdentifier
    , SecurityProfileIdentifier (..)
    , mkSecurityProfileIdentifier
    , spiName
    , spiArn

    -- * KinesisAction
    , KinesisAction (..)
    , mkKinesisAction
    , kaRoleArn
    , kaStreamName
    , kaPartitionKey

    -- * MessageId
    , MessageId (..)

    -- * TimestreamDimensionName
    , TimestreamDimensionName (..)

    -- * HashKeyValue
    , HashKeyValue (..)

    -- * ResourceIdentifier
    , ResourceIdentifier (..)
    , mkResourceIdentifier
    , riAccount
    , riCaCertificateId
    , riClientId
    , riCognitoIdentityPoolId
    , riDeviceCertificateId
    , riIamRoleArn
    , riPolicyVersionIdentifier
    , riRoleAliasArn

    -- * AbortCriteria
    , AbortCriteria (..)
    , mkAbortCriteria
    , acFailureType
    , acAction
    , acThresholdPercentage
    , acMinNumberOfExecutedThings

    -- * CertificateStatus
    , CertificateStatus (..)

    -- * CloudwatchLogsAction
    , CloudwatchLogsAction (..)
    , mkCloudwatchLogsAction
    , claRoleArn
    , claLogGroupName

    -- * PercentPair
    , PercentPair (..)
    , mkPercentPair
    , ppPercent
    , ppValue

    -- * TableName
    , TableName (..)

    -- * DetailsValue
    , DetailsValue (..)

    -- * AssetPropertyTimestamp
    , AssetPropertyTimestamp (..)
    , mkAssetPropertyTimestamp
    , aptTimeInSeconds
    , aptOffsetInNanos

    -- * Parameter
    , Parameter (..)

    -- * AuditNotificationTarget
    , AuditNotificationTarget (..)
    , mkAuditNotificationTarget
    , antEnabled
    , antRoleArn
    , antTargetArn

    -- * AuditFrequency
    , AuditFrequency (..)

    -- * TimestreamDimension
    , TimestreamDimension (..)
    , mkTimestreamDimension
    , tdName
    , tdValue

    -- * StreamId
    , StreamId (..)

    -- * ThingConnectivity
    , ThingConnectivity (..)
    , mkThingConnectivity
    , tcConnected
    , tcTimestamp

    -- * FileName
    , FileName (..)

    -- * SecurityProfileTargetArn
    , SecurityProfileTargetArn (..)

    -- * ThingTypeArn
    , ThingTypeArn (..)

    -- * BillingGroupId
    , BillingGroupId (..)

    -- * DayOfWeek
    , DayOfWeek (..)

    -- * ThingTypeDefinition
    , ThingTypeDefinition (..)
    , mkThingTypeDefinition
    , ttdThingTypeArn
    , ttdThingTypeMetadata
    , ttdThingTypeName
    , ttdThingTypeProperties

    -- * JobExecutionStatus
    , JobExecutionStatus (..)

    -- * TargetSelection
    , TargetSelection (..)

    -- * ThingId
    , ThingId (..)

    -- * BillingGroupMetadata
    , BillingGroupMetadata (..)
    , mkBillingGroupMetadata
    , bgmCreationDate

    -- * Target
    , Target (..)

    -- * TimeoutConfig
    , TimeoutConfig (..)
    , mkTimeoutConfig
    , tcInProgressTimeoutInMinutes

    -- * HttpActionHeader
    , HttpActionHeader (..)
    , mkHttpActionHeader
    , hahKey
    , hahValue

    -- * TopicPattern
    , TopicPattern (..)

    -- * TopicRulePayload
    , TopicRulePayload (..)
    , mkTopicRulePayload
    , trpSql
    , trpActions
    , trpAwsIotSqlVersion
    , trpDescription
    , trpErrorAction
    , trpRuleDisabled

    -- * RoleArn
    , RoleArn (..)

    -- * IndexName
    , IndexName (..)

    -- * ScheduledAuditArn
    , ScheduledAuditArn (..)

    -- * OtaUpdateId
    , OtaUpdateId (..)

    -- * Name
    , Name (..)

    -- * CheckName
    , CheckName (..)

    -- * Owner
    , Owner (..)

    -- * ProvisioningRoleArn
    , ProvisioningRoleArn (..)

    -- * RoleArnForLogging
    , RoleArnForLogging (..)

    -- * OtaUpdateArn
    , OtaUpdateArn (..)

    -- * ConfirmationUrl
    , ConfirmationUrl (..)

    -- * ActionArn
    , ActionArn (..)

    -- * ActionId
    , ActionId (..)

    -- * NextMarker
    , NextMarker (..)

    -- * TargetAwsAccount
    , TargetAwsAccount (..)

    -- * TransferMessage
    , TransferMessage (..)

    -- * VersionId
    , VersionId (..)

    -- * Arn
    , Arn (..)

    -- * ChannelArn
    , ChannelArn (..)

    -- * Document
    , Document (..)

    -- * DocumentSource
    , DocumentSource (..)

    -- * Endpoint
    , Endpoint (..)

    -- * Index
    , Index (..)

    -- * Type
    , Type (..)

    -- * Id
    , Id (..)

    -- * DefaultVersionId
    , DefaultVersionId (..)

    -- * Operation
    , Operation (..)

    -- * DefaultAuthorizerName
    , DefaultAuthorizerName (..)

    -- * OwnedBy
    , OwnedBy (..)

    -- * PreviousOwnedBy
    , PreviousOwnedBy (..)

    -- * ParentGroupName
    , ParentGroupName (..)

    -- * PropertyId
    , PropertyId (..)

    -- * NamePrefixFilter
    , NamePrefixFilter (..)

    -- * Bucket
    , Bucket (..)

    -- * Unit
    , Unit (..)

    -- * Sql
    , Sql (..)

    -- * DatabaseName
    , DatabaseName (..)

    -- * BooleanValue
    , BooleanValue (..)

    -- * RejectReason
    , RejectReason (..)

    -- * AwsSignerJobId
    , AwsSignerJobId (..)

    -- * IamRoleArn
    , IamRoleArn (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.IoT.Types.AlarmName
  
import Network.AWS.IoT.Types.PayloadVersion
  
import Network.AWS.IoT.Types.TimestreamTimestampValue
  
  
import Network.AWS.IoT.Types.RegistrationCode
  
import Network.AWS.IoT.Types.TaskStatus
  
import Network.AWS.IoT.Types.AwsJobExponentialRolloutRate
  
import Network.AWS.IoT.Types.ConfirmationToken
  
import Network.AWS.IoT.Types.RegistryS3BucketName
  
  
import Network.AWS.IoT.Types.AuditSuppression
  
import Network.AWS.IoT.Types.RoleAliasDescription
  
import Network.AWS.IoT.Types.DeviceDefenderThingName
  
import Network.AWS.IoT.Types.TargetArn
  
import Network.AWS.IoT.Types.Stream
  
import Network.AWS.IoT.Types.EnableIoTLoggingParams
  
import Network.AWS.IoT.Types.Destination
  
  
import Network.AWS.IoT.Types.KeyValue
  
import Network.AWS.IoT.Types.ClientId
  
import Network.AWS.IoT.Types.ResourceLogicalId
  
import Network.AWS.IoT.Types.HeaderValue
  
import Network.AWS.IoT.Types.HttpUrlDestinationConfiguration
  
import Network.AWS.IoT.Types.AuditMitigationActionsTaskTarget
  
import Network.AWS.IoT.Types.OTAUpdateSummary
  
import Network.AWS.IoT.Types.HttpQueryString
  
import Network.AWS.IoT.Types.TopicRuleDestinationStatus
  
import Network.AWS.IoT.Types.AcmCertificateArn
  
import Network.AWS.IoT.Types.PolicyDocument
  
import Network.AWS.IoT.Types.CertificateValidity
  
import Network.AWS.IoT.Types.TemplateName
  
import Network.AWS.IoT.Types.DomainConfigurationName
  
  
import Network.AWS.IoT.Types.AlertTarget
  
import Network.AWS.IoT.Types.BehaviorCriteria
  
import Network.AWS.IoT.Types.Platform
  
import Network.AWS.IoT.Types.SigningProfileName
  
import Network.AWS.IoT.Types.IndexSchema
  
import Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
  
import Network.AWS.IoT.Types.HashKeyField
  
import Network.AWS.IoT.Types.RoleAliasArn
  
import Network.AWS.IoT.Types.PrincipalId
  
  
import Network.AWS.IoT.Types.AttributeValue
  
import Network.AWS.IoT.Types.PolicyName
  
import Network.AWS.IoT.Types.SqsAction
  
import Network.AWS.IoT.Types.S3Key
  
import Network.AWS.IoT.Types.JobExecutionsRolloutConfig
  
import Network.AWS.IoT.Types.FunctionArn
  
import Network.AWS.IoT.Types.Tag
  
import Network.AWS.IoT.Types.PrivateKey
  
import Network.AWS.IoT.Types.AwsJobExecutionsRolloutConfig
  
import Network.AWS.IoT.Types.Field
  
import Network.AWS.IoT.Types.AssetPropertyIntegerValue
  
import Network.AWS.IoT.Types.StreamDescription
  
import Network.AWS.IoT.Types.AddThingsToThingGroupParams
  
import Network.AWS.IoT.Types.TimestreamDimensionValue
  
import Network.AWS.IoT.Types.AwsJobAbortConfig
  
import Network.AWS.IoT.Types.CertificateName
  
import Network.AWS.IoT.Types.SnsTopicArn
  
import Network.AWS.IoT.Types.S3Version
  
import Network.AWS.IoT.Types.PolicyVersion
  
import Network.AWS.IoT.Types.StreamInfo
  
import Network.AWS.IoT.Types.MqttUsername
  
import Network.AWS.IoT.Types.ServerName
  
import Network.AWS.IoT.Types.IndexStatus
  
  
import Network.AWS.IoT.Types.CannedAccessControlList
  
import Network.AWS.IoT.Types.IotAnalyticsAction
  
import Network.AWS.IoT.Types.LogLevel
  
import Network.AWS.IoT.Types.LogTargetName
  
import Network.AWS.IoT.Types.ResourceType
  
import Network.AWS.IoT.Types.StreamFile
  
import Network.AWS.IoT.Types.CodeSigningSignature
  
import Network.AWS.IoT.Types.MitigationActionName
  
import Network.AWS.IoT.Types.JobId
  
import Network.AWS.IoT.Types.UpdateCACertificateParams
  
import Network.AWS.IoT.Types.JobArn
  
import Network.AWS.IoT.Types.ProvisioningHook
  
import Network.AWS.IoT.Types.RoleAlias
  
import Network.AWS.IoT.Types.AssetPropertyQuality
  
import Network.AWS.IoT.Types.BillingGroupDescription
  
import Network.AWS.IoT.Types.EffectivePolicy
  
import Network.AWS.IoT.Types.StateMachineName
  
  
import Network.AWS.IoT.Types.OTAUpdateDescription
  
import Network.AWS.IoT.Types.LogTargetType
  
import Network.AWS.IoT.Types.AuthorizerStatus
  
import Network.AWS.IoT.Types.DetailsKey
  
import Network.AWS.IoT.Types.PartitionKey
  
import Network.AWS.IoT.Types.AuditTaskId
  
import Network.AWS.IoT.Types.ProvisioningTemplateSummary
  
  
import Network.AWS.IoT.Types.ThingTypeProperties
  
import Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
  
import Network.AWS.IoT.Types.AwsIotJobId
  
import Network.AWS.IoT.Types.ElasticsearchAction
  
import Network.AWS.IoT.Types.JobExecutionFailureType
  
import Network.AWS.IoT.Types.KeyPair
  
import Network.AWS.IoT.Types.CertificatePem
  
import Network.AWS.IoT.Types.ServerCertificateStatusDetail
  
import Network.AWS.IoT.Types.MitigationActionType
  
import Network.AWS.IoT.Types.DimensionArn
  
import Network.AWS.IoT.Types.AuditTaskMetadata
  
import Network.AWS.IoT.Types.ViolationEventType
  
import Network.AWS.IoT.Types.ExecutionNamePrefix
  
import Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
  
import Network.AWS.IoT.Types.CertificateId
  
import Network.AWS.IoT.Types.CertificateArn
  
import Network.AWS.IoT.Types.SnsAction
  
  
import Network.AWS.IoT.Types.TaskStatistics
  
import Network.AWS.IoT.Types.MissingContextValue
  
import Network.AWS.IoT.Types.SecurityProfileTargetMapping
  
import Network.AWS.IoT.Types.TopicRuleListItem
  
import Network.AWS.IoT.Types.AssetPropertyOffsetInNanos
  
  
import Network.AWS.IoT.Types.AuditTaskType
  
import Network.AWS.IoT.Types.QueryVersion
  
import Network.AWS.IoT.Types.JobDocument
  
import Network.AWS.IoT.Types.ThingAttribute
  
import Network.AWS.IoT.Types.UpdateDeviceCertificateParams
  
import Network.AWS.IoT.Types.TaskId
  
import Network.AWS.IoT.Types.KeyName
  
import Network.AWS.IoT.Types.DynamoDBAction
  
import Network.AWS.IoT.Types.EvaluationStatistic
  
import Network.AWS.IoT.Types.MitigationActionId
  
  
import Network.AWS.IoT.Types.MitigationActionArn
  
import Network.AWS.IoT.Types.OTAUpdateErrorMessage
  
import Network.AWS.IoT.Types.Prefix
  
import Network.AWS.IoT.Types.TimestreamTableName
  
import Network.AWS.IoT.Types.ReasonForNonComplianceCode
  
import Network.AWS.IoT.Types.BehaviorMetric
  
import Network.AWS.IoT.Types.AbortConfig
  
import Network.AWS.IoT.Types.StreamSummary
  
import Network.AWS.IoT.Types.Token
  
import Network.AWS.IoT.Types.TopicRuleDestinationConfiguration
  
import Network.AWS.IoT.Types.ThingTypeName
  
import Network.AWS.IoT.Types.AuditMitigationActionsTaskId
  
import Network.AWS.IoT.Types.SalesforceAction
  
  
import Network.AWS.IoT.Types.AuthorizerConfig
  
import Network.AWS.IoT.Types.IotEventsAction
  
import Network.AWS.IoT.Types.AuditCheckConfiguration
  
import Network.AWS.IoT.Types.JobSummary
  
import Network.AWS.IoT.Types.BehaviorName
  
import Network.AWS.IoT.Types.ScheduledAuditName
  
import Network.AWS.IoT.Types.AwsIotSqlVersion
  
import Network.AWS.IoT.Types.Url
  
import Network.AWS.IoT.Types.AuditMitigationActionsTaskMetadata
  
import Network.AWS.IoT.Types.TopicRuleDestination
  
import Network.AWS.IoT.Types.CertificateDescription
  
import Network.AWS.IoT.Types.Value
  
import Network.AWS.IoT.Types.ValidationError
  
import Network.AWS.IoT.Types.FileLocation
  
import Network.AWS.IoT.Types.AwsJobAbortCriteriaFailureType
  
import Network.AWS.IoT.Types.EndpointType
  
import Network.AWS.IoT.Types.DimensionValueOperator
  
import Network.AWS.IoT.Types.AuthorizerFunctionArn
  
import Network.AWS.IoT.Types.NamespaceId
  
import Network.AWS.IoT.Types.CertificateMode
  
import Network.AWS.IoT.Types.Action
  
import Network.AWS.IoT.Types.InlineDocument
  
import Network.AWS.IoT.Types.CodeSigningCertificateChain
  
import Network.AWS.IoT.Types.PublicKey
  
import Network.AWS.IoT.Types.AssetPropertyTimeInSeconds
  
import Network.AWS.IoT.Types.Protocol
  
import Network.AWS.IoT.Types.HttpUrlDestinationProperties
  
import Network.AWS.IoT.Types.JobProcessDetails
  
import Network.AWS.IoT.Types.ThingGroupMetadata
  
import Network.AWS.IoT.Types.ThingGroupArn
  
import Network.AWS.IoT.Types.AwsJobPresignedUrlConfig
  
  
import Network.AWS.IoT.Types.AssetPropertyEntryId
  
import Network.AWS.IoT.Types.DynamicGroupStatus
  
import Network.AWS.IoT.Types.TemplateDescription
  
import Network.AWS.IoT.Types.DimensionStringValue
  
import Network.AWS.IoT.Types.AuditCheckRunStatus
  
import Network.AWS.IoT.Types.ThingGroupId
  
import Network.AWS.IoT.Types.MitigationActionIdentifier
  
import Network.AWS.IoT.Types.AuthorizerName
  
import Network.AWS.IoT.Types.AggregationField
  
import Network.AWS.IoT.Types.LambdaAction
  
import Network.AWS.IoT.Types.CACertificateDescription
  
import Network.AWS.IoT.Types.ImplicitDeny
  
import Network.AWS.IoT.Types.AssetPropertyAlias
  
  
import Network.AWS.IoT.Types.ThingArn
  
import Network.AWS.IoT.Types.AwsAccountId
  
import Network.AWS.IoT.Types.StreamArn
  
import Network.AWS.IoT.Types.AssetPropertyStringValue
  
import Network.AWS.IoT.Types.ComparisonOperator
  
import Network.AWS.IoT.Types.RuleName
  
import Network.AWS.IoT.Types.AssetPropertyDoubleValue
  
import Network.AWS.IoT.Types.CertificatePathOnDevice
  
import Network.AWS.IoT.Types.ThingTypeId
  
import Network.AWS.IoT.Types.JobDescription
  
import Network.AWS.IoT.Types.PutAssetPropertyValueEntry
  
import Network.AWS.IoT.Types.JobExecutionSummary
  
import Network.AWS.IoT.Types.ThingTypeMetadata
  
import Network.AWS.IoT.Types.ThingIndexingMode
  
import Network.AWS.IoT.Types.BillingGroupArn
  
import Network.AWS.IoT.Types.InputName
  
import Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
  
import Network.AWS.IoT.Types.MitigationActionParams
  
import Network.AWS.IoT.Types.ReasonCode
  
import Network.AWS.IoT.Types.SigningProfileParameter
  
import Network.AWS.IoT.Types.AuditCheckDetails
  
import Network.AWS.IoT.Types.PolicyVersionIdentifier
  
import Network.AWS.IoT.Types.Topic
  
import Network.AWS.IoT.Types.AuthResult
  
import Network.AWS.IoT.Types.BucketName
  
import Network.AWS.IoT.Types.ScheduledAuditMetadata
  
import Network.AWS.IoT.Types.PolicyTarget
  
import Network.AWS.IoT.Types.LogGroupName
  
import Network.AWS.IoT.Types.ThingConnectivityIndexingMode
  
import Network.AWS.IoT.Types.HttpHeaderValue
  
import Network.AWS.IoT.Types.MetricDimension
  
import Network.AWS.IoT.Types.PresignedUrlConfig
  
import Network.AWS.IoT.Types.AuditCheckName
  
import Network.AWS.IoT.Types.Behavior
  
import Network.AWS.IoT.Types.AuthorizerArn
  
import Network.AWS.IoT.Types.AbortAction
  
import Network.AWS.IoT.Types.ServerCertificateStatus
  
import Network.AWS.IoT.Types.NextToken
  
  
import Network.AWS.IoT.Types.ViolationId
  
import Network.AWS.IoT.Types.ChannelName
  
import Network.AWS.IoT.Types.Cidr
  
import Network.AWS.IoT.Types.S3Destination
  
import Network.AWS.IoT.Types.DynamoDBv2Action
  
import Network.AWS.IoT.Types.AuditTaskStatus
  
import Network.AWS.IoT.Types.StatisticalThreshold
  
import Network.AWS.IoT.Types.ThingGroupDocument
  
import Network.AWS.IoT.Types.ThingGroupName
  
import Network.AWS.IoT.Types.ServerCertificateSummary
  
import Network.AWS.IoT.Types.FirehoseSeparator
  
import Network.AWS.IoT.Types.EventType
  
import Network.AWS.IoT.Types.SigV4Authorization
  
import Network.AWS.IoT.Types.PayloadField
  
import Network.AWS.IoT.Types.DomainConfigurationStatus
  
import Network.AWS.IoT.Types.SecurityProfileTarget
  
import Network.AWS.IoT.Types.AssetPropertyValue
  
import Network.AWS.IoT.Types.TlsContext
  
import Network.AWS.IoT.Types.StateValue
  
import Network.AWS.IoT.Types.DomainConfigurationSummary
  
  
import Network.AWS.IoT.Types.FirehoseAction
  
import Network.AWS.IoT.Types.TimestreamTimestamp
  
import Network.AWS.IoT.Types.JsonDocument
  
import Network.AWS.IoT.Types.OTAUpdateStatus
  
import Network.AWS.IoT.Types.Job
  
import Network.AWS.IoT.Types.PrincipalArn
  
import Network.AWS.IoT.Types.TopicRule
  
import Network.AWS.IoT.Types.Denied
  
import Network.AWS.IoT.Types.OTAUpdateFileVersion
  
import Network.AWS.IoT.Types.ResourceArn
  
import Network.AWS.IoT.Types.DimensionName
  
import Network.AWS.IoT.Types.DynamoKeyType
  
import Network.AWS.IoT.Types.Key
  
import Network.AWS.IoT.Types.PolicyVersionId
  
import Network.AWS.IoT.Types.DomainName
  
import Network.AWS.IoT.Types.AuthorizerDescription
  
import Network.AWS.IoT.Types.AwsJobAbortCriteriaAbortAction
  
import Network.AWS.IoT.Types.AuditMitigationActionExecutionMetadata
  
  
import Network.AWS.IoT.Types.TopicRuleDestinationSummary
  
import Network.AWS.IoT.Types.MetricValue
  
import Network.AWS.IoT.Types.AwsJobRateIncreaseCriteria
  
import Network.AWS.IoT.Types.CertificateSigningRequest
  
import Network.AWS.IoT.Types.PolicyTemplateName
  
import Network.AWS.IoT.Types.HttpContext
  
import Network.AWS.IoT.Types.TimestreamAction
  
import Network.AWS.IoT.Types.QueryString
  
import Network.AWS.IoT.Types.CACertificate
  
import Network.AWS.IoT.Types.AlertTargetType
  
import Network.AWS.IoT.Types.IotSiteWiseAction
  
import Network.AWS.IoT.Types.DeliveryStreamName
  
import Network.AWS.IoT.Types.TokenSignature
  
import Network.AWS.IoT.Types.Principal
  
import Network.AWS.IoT.Types.DeviceCertificateUpdateAction
  
import Network.AWS.IoT.Types.AttributePayload
  
import Network.AWS.IoT.Types.CloudwatchMetricAction
  
import Network.AWS.IoT.Types.RangeKeyField
  
import Network.AWS.IoT.Types.ThingTypeDescription
  
import Network.AWS.IoT.Types.FieldType
  
import Network.AWS.IoT.Types.AlertTargetArn
  
  
import Network.AWS.IoT.Types.ThingGroupProperties
  
import Network.AWS.IoT.Types.NonCompliantResource
  
import Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
  
import Network.AWS.IoT.Types.AutoRegistrationStatus
  
import Network.AWS.IoT.Types.Marker
  
import Network.AWS.IoT.Types.ServiceName
  
import Network.AWS.IoT.Types.ThingIndexingConfiguration
  
import Network.AWS.IoT.Types.MetricToRetain
  
import Network.AWS.IoT.Types.SecurityProfileName
  
import Network.AWS.IoT.Types.Resource
  
import Network.AWS.IoT.Types.PutItemInput
  
  
  
import Network.AWS.IoT.Types.QueueUrl
  
import Network.AWS.IoT.Types.AwsIotJobArn
  
import Network.AWS.IoT.Types.LoggingOptionsPayload
  
import Network.AWS.IoT.Types.DayOfMonth
  
import Network.AWS.IoT.Types.ErrorCode
  
import Network.AWS.IoT.Types.ServiceType
  
import Network.AWS.IoT.Types.Certificate
  
import Network.AWS.IoT.Types.BillingGroupProperties
  
import Network.AWS.IoT.Types.LogTargetConfiguration
  
import Network.AWS.IoT.Types.EndpointAddress
  
import Network.AWS.IoT.Types.JobExecutionSummaryForJob
  
import Network.AWS.IoT.Types.RateIncreaseCriteria
  
import Network.AWS.IoT.Types.LogTarget
  
import Network.AWS.IoT.Types.ThingGroupDescription
  
import Network.AWS.IoT.Types.OTAUpdateFile
  
import Network.AWS.IoT.Types.JobDocumentSource
  
import Network.AWS.IoT.Types.MitigationAction
  
import Network.AWS.IoT.Types.PublishFindingToSnsParams
  
import Network.AWS.IoT.Types.StepFunctionsAction
  
import Network.AWS.IoT.Types.RegistryS3KeyName
  
import Network.AWS.IoT.Types.RelatedResource
  
import Network.AWS.IoT.Types.S3FileUrl
  
import Network.AWS.IoT.Types.TemplateArn
  
import Network.AWS.IoT.Types.CustomCodeSigning
  
import Network.AWS.IoT.Types.AssetId
  
import Network.AWS.IoT.Types.Statistics
  
import Network.AWS.IoT.Types.ActiveViolation
  
import Network.AWS.IoT.Types.ProcessingTargetName
  
import Network.AWS.IoT.Types.OTAUpdateInfo
  
import Network.AWS.IoT.Types.StartSigningJobParameter
  
import Network.AWS.IoT.Types.CloudwatchAlarmAction
  
import Network.AWS.IoT.Types.PolicyArn
  
import Network.AWS.IoT.Types.ViolationEvent
  
import Network.AWS.IoT.Types.ReasonForNonCompliance
  
import Network.AWS.IoT.Types.JobExecution
  
import Network.AWS.IoT.Types.Code
  
import Network.AWS.IoT.Types.AuthorizerSummary
  
import Network.AWS.IoT.Types.TagKey
  
import Network.AWS.IoT.Types.S3Location
  
import Network.AWS.IoT.Types.DomainConfigurationArn
  
import Network.AWS.IoT.Types.HashAlgorithm
  
  
import Network.AWS.IoT.Types.DomainType
  
import Network.AWS.IoT.Types.MqttClientId
  
  
import Network.AWS.IoT.Types.MessageFormat
  
import Network.AWS.IoT.Types.AwsArn
  
import Network.AWS.IoT.Types.HeaderKey
  
import Network.AWS.IoT.Types.FindingId
  
import Network.AWS.IoT.Types.Configuration
  
import Network.AWS.IoT.Types.TaskStatisticsForAuditCheck
  
import Network.AWS.IoT.Types.Policy
  
import Network.AWS.IoT.Types.AssetPropertyVariant
  
  
import Network.AWS.IoT.Types.AuditFinding
  
import Network.AWS.IoT.Types.TemplateBody
  
import Network.AWS.IoT.Types.ExplicitDeny
  
import Network.AWS.IoT.Types.HttpHeaderName
  
import Network.AWS.IoT.Types.RepublishAction
  
import Network.AWS.IoT.Types.AuditNotificationType
  
  
import Network.AWS.IoT.Types.CognitoIdentityPoolId
  
import Network.AWS.IoT.Types.CACertificateUpdateAction
  
import Network.AWS.IoT.Types.AuthDecision
  
import Network.AWS.IoT.Types.DimensionType
  
import Network.AWS.IoT.Types.Allowed
  
import Network.AWS.IoT.Types.SecurityProfileArn
  
import Network.AWS.IoT.Types.JobStatus
  
import Network.AWS.IoT.Types.AuditFindingSeverity
  
import Network.AWS.IoT.Types.BillingGroupName
  
import Network.AWS.IoT.Types.RegistrationConfig
  
import Network.AWS.IoT.Types.GenerationId
  
import Network.AWS.IoT.Types.JobExecutionStatusDetails
  
import Network.AWS.IoT.Types.AttributeKey
  
import Network.AWS.IoT.Types.HttpAuthorization
  
import Network.AWS.IoT.Types.SigningRegion
  
import Network.AWS.IoT.Types.RangeKeyValue
  
import Network.AWS.IoT.Types.ThingDocument
  
import Network.AWS.IoT.Types.TransferData
  
import Network.AWS.IoT.Types.TokenKeyName
  
import Network.AWS.IoT.Types.ThingName
  
import Network.AWS.IoT.Types.ErrorMessage
  
import Network.AWS.IoT.Types.CACertificateStatus
  
  
import Network.AWS.IoT.Types.StateReason
  
import Network.AWS.IoT.Types.HttpUrlDestinationSummary
  
import Network.AWS.IoT.Types.Message
  
import Network.AWS.IoT.Types.SignatureAlgorithm
  
import Network.AWS.IoT.Types.OutgoingCertificate
  
import Network.AWS.IoT.Types.SecurityProfileDescription
  
import Network.AWS.IoT.Types.RuleArn
  
import Network.AWS.IoT.Types.AttributeName
  
  
import Network.AWS.IoT.Types.MqttContext
  
import Network.AWS.IoT.Types.AwsJobAbortCriteria
  
import Network.AWS.IoT.Types.StreamName
  
import Network.AWS.IoT.Types.ClientRequestToken
  
import Network.AWS.IoT.Types.AwsJobTimeoutConfig
  
import Network.AWS.IoT.Types.Comment
  
import Network.AWS.IoT.Types.HttpAction
  
import Network.AWS.IoT.Types.ThingGroupIndexingMode
  
  
import Network.AWS.IoT.Types.Description
  
import Network.AWS.IoT.Types.SalesforceEndpoint
  
import Network.AWS.IoT.Types.JobExecutionSummaryForThing
  
import Network.AWS.IoT.Types.AuthInfo
  
import Network.AWS.IoT.Types.ErrorInfo
  
import Network.AWS.IoT.Types.ReportType
  
import Network.AWS.IoT.Types.CodeSigning
  
import Network.AWS.IoT.Types.ExponentialRolloutRate
  
import Network.AWS.IoT.Types.GroupNameAndArn
  
import Network.AWS.IoT.Types.ActionType
  
import Network.AWS.IoT.Types.SQL
  
import Network.AWS.IoT.Types.S3Action
  
import Network.AWS.IoT.Types.SecurityProfileIdentifier
  
import Network.AWS.IoT.Types.KinesisAction
  
import Network.AWS.IoT.Types.MessageId
  
import Network.AWS.IoT.Types.TimestreamDimensionName
  
import Network.AWS.IoT.Types.HashKeyValue
  
  
import Network.AWS.IoT.Types.ResourceIdentifier
  
import Network.AWS.IoT.Types.AbortCriteria
  
import Network.AWS.IoT.Types.CertificateStatus
  
import Network.AWS.IoT.Types.CloudwatchLogsAction
  
import Network.AWS.IoT.Types.PercentPair
  
import Network.AWS.IoT.Types.TableName
  
import Network.AWS.IoT.Types.DetailsValue
  
import Network.AWS.IoT.Types.AssetPropertyTimestamp
  
import Network.AWS.IoT.Types.Parameter
  
import Network.AWS.IoT.Types.AuditNotificationTarget
  
import Network.AWS.IoT.Types.AuditFrequency
  
import Network.AWS.IoT.Types.TimestreamDimension
  
import Network.AWS.IoT.Types.StreamId
  
import Network.AWS.IoT.Types.ThingConnectivity
  
import Network.AWS.IoT.Types.FileName
  
import Network.AWS.IoT.Types.SecurityProfileTargetArn
  
import Network.AWS.IoT.Types.ThingTypeArn
  
import Network.AWS.IoT.Types.BillingGroupId
  
import Network.AWS.IoT.Types.DayOfWeek
  
import Network.AWS.IoT.Types.ThingTypeDefinition
  
import Network.AWS.IoT.Types.JobExecutionStatus
  
import Network.AWS.IoT.Types.TargetSelection
  
import Network.AWS.IoT.Types.ThingId
  
import Network.AWS.IoT.Types.BillingGroupMetadata
  
  
import Network.AWS.IoT.Types.Target
  
import Network.AWS.IoT.Types.TimeoutConfig
  
import Network.AWS.IoT.Types.HttpActionHeader
  
import Network.AWS.IoT.Types.TopicPattern
  
import Network.AWS.IoT.Types.TopicRulePayload
  
import Network.AWS.IoT.Types.RoleArn
  
import Network.AWS.IoT.Types.IndexName
  
import Network.AWS.IoT.Types.ScheduledAuditArn
  
import Network.AWS.IoT.Types.OtaUpdateId
  
import Network.AWS.IoT.Types.Name
  
import Network.AWS.IoT.Types.CheckName
  
import Network.AWS.IoT.Types.Owner
  
import Network.AWS.IoT.Types.ProvisioningRoleArn
  
import Network.AWS.IoT.Types.RoleArnForLogging
  
import Network.AWS.IoT.Types.OtaUpdateArn
  
import Network.AWS.IoT.Types.ConfirmationUrl
  
import Network.AWS.IoT.Types.ActionArn
  
import Network.AWS.IoT.Types.ActionId
  
import Network.AWS.IoT.Types.NextMarker
  
import Network.AWS.IoT.Types.TargetAwsAccount
  
import Network.AWS.IoT.Types.TransferMessage
  
import Network.AWS.IoT.Types.VersionId
  
import Network.AWS.IoT.Types.Arn
  
import Network.AWS.IoT.Types.ChannelArn
  
import Network.AWS.IoT.Types.Document
  
import Network.AWS.IoT.Types.DocumentSource
  
import Network.AWS.IoT.Types.Endpoint
  
import Network.AWS.IoT.Types.Index
  
import Network.AWS.IoT.Types.Type
  
import Network.AWS.IoT.Types.Id
  
import Network.AWS.IoT.Types.DefaultVersionId
  
import Network.AWS.IoT.Types.Operation
  
import Network.AWS.IoT.Types.DefaultAuthorizerName
  
import Network.AWS.IoT.Types.OwnedBy
  
import Network.AWS.IoT.Types.PreviousOwnedBy
  
import Network.AWS.IoT.Types.ParentGroupName
  
import Network.AWS.IoT.Types.PropertyId
  
import Network.AWS.IoT.Types.NamePrefixFilter
  
import Network.AWS.IoT.Types.Bucket
  
import Network.AWS.IoT.Types.Unit
  
import Network.AWS.IoT.Types.Sql
  
import Network.AWS.IoT.Types.DatabaseName
  
import Network.AWS.IoT.Types.BooleanValue
  
import Network.AWS.IoT.Types.RejectReason
  
import Network.AWS.IoT.Types.AwsSignerJobId
  
import Network.AWS.IoT.Types.IamRoleArn
  

-- | API version @2015-05-28@ of the Amazon IoT SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "IoT", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "iot", Core._svcVersion = "2015-05-28",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "IoT",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | This exception occurs if you attempt to start a task with the same task-id as an existing task but with a different clientRequestToken.
_TaskAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TaskAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "TaskAlreadyExistsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TaskAlreadyExistsException #-}
{-# DEPRECATED _TaskAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | Unable to verify the CA certificate used to sign the device certificate you are attempting to register. This is happens when you have registered more than one CA certificate that has the same subject field and public key.
_CertificateConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateConflictException
  = Core._MatchServiceError mkServiceConfig
      "CertificateConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _CertificateConflictException #-}
{-# DEPRECATED _CertificateConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The Rule-SQL expression can't be parsed correctly.
_SqlParseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SqlParseException
  = Core._MatchServiceError mkServiceConfig "SqlParseException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SqlParseException #-}
{-# DEPRECATED _SqlParseException "Use generic-lens or generic-optics instead"  #-}

-- | The index is not ready.
_IndexNotReadyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IndexNotReadyException
  = Core._MatchServiceError mkServiceConfig "IndexNotReadyException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _IndexNotReadyException #-}
{-# DEPRECATED _IndexNotReadyException "Use generic-lens or generic-optics instead"  #-}

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException
  = Core._MatchServiceError mkServiceConfig "InvalidRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRequestException #-}
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead"  #-}

-- | You can't transfer the certificate because authorization policies are still attached.
_TransferConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TransferConflictException
  = Core._MatchServiceError mkServiceConfig
      "TransferConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _TransferConflictException #-}
{-# DEPRECATED _TransferConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate operation is not allowed.
_CertificateStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateStateException
  = Core._MatchServiceError mkServiceConfig
      "CertificateStateException"
      Core.. Core.hasStatues 406
{-# INLINEABLE _CertificateStateException #-}
{-# DEPRECATED _CertificateStateException "Use generic-lens or generic-optics instead"  #-}

-- | The response is invalid.
_InvalidResponseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResponseException
  = Core._MatchServiceError mkServiceConfig
      "InvalidResponseException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidResponseException #-}
{-# DEPRECATED _InvalidResponseException "Use generic-lens or generic-optics instead"  #-}

-- | The registration code is invalid.
_RegistrationCodeValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RegistrationCodeValidationException
  = Core._MatchServiceError mkServiceConfig
      "RegistrationCodeValidationException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _RegistrationCodeValidationException #-}
{-# DEPRECATED _RegistrationCodeValidationException "Use generic-lens or generic-optics instead"  #-}

-- | The policy documentation is not valid.
_MalformedPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyException
  = Core._MatchServiceError mkServiceConfig
      "MalformedPolicyException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _MalformedPolicyException #-}
{-# DEPRECATED _MalformedPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | You can't delete the resource because it is attached to one or more resources.
_DeleteConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeleteConflictException
  = Core._MatchServiceError mkServiceConfig "DeleteConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _DeleteConflictException #-}
{-# DEPRECATED _DeleteConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "ResourceAlreadyExistsException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ResourceAlreadyExistsException #-}
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The resource is not configured.
_NotConfiguredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotConfiguredException
  = Core._MatchServiceError mkServiceConfig "NotConfiguredException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NotConfiguredException #-}
{-# DEPRECATED _NotConfiguredException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate is invalid.
_CertificateValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateValidationException
  = Core._MatchServiceError mkServiceConfig
      "CertificateValidationException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CertificateValidationException #-}
{-# DEPRECATED _CertificateValidationException "Use generic-lens or generic-optics instead"  #-}

-- | The resource registration failed.
_ResourceRegistrationFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceRegistrationFailureException
  = Core._MatchServiceError mkServiceConfig
      "ResourceRegistrationFailureException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ResourceRegistrationFailureException #-}
{-# DEPRECATED _ResourceRegistrationFailureException "Use generic-lens or generic-optics instead"  #-}

-- | The query is invalid.
_InvalidQueryException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidQueryException
  = Core._MatchServiceError mkServiceConfig "InvalidQueryException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidQueryException #-}
{-# DEPRECATED _InvalidQueryException "Use generic-lens or generic-optics instead"  #-}

-- | You can't revert the certificate transfer because the transfer is already complete.
_TransferAlreadyCompletedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TransferAlreadyCompletedException
  = Core._MatchServiceError mkServiceConfig
      "TransferAlreadyCompletedException"
      Core.. Core.hasStatues 410
{-# INLINEABLE _TransferAlreadyCompletedException #-}
{-# DEPRECATED _TransferAlreadyCompletedException "Use generic-lens or generic-optics instead"  #-}

-- | The rate exceeds the limit.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException
  = Core._MatchServiceError mkServiceConfig "ThrottlingException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ThrottlingException #-}
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead"  #-}

-- | The aggregation is invalid.
_InvalidAggregationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAggregationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidAggregationException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidAggregationException #-}
{-# DEPRECATED _InvalidAggregationException "Use generic-lens or generic-optics instead"  #-}

-- | A conflicting resource update exception. This exception is thrown when two pending updates cause a conflict.
_ConflictingResourceUpdateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictingResourceUpdateException
  = Core._MatchServiceError mkServiceConfig
      "ConflictingResourceUpdateException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConflictingResourceUpdateException #-}
{-# DEPRECATED _ConflictingResourceUpdateException "Use generic-lens or generic-optics instead"  #-}

-- | An unexpected error has occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException
  = Core._MatchServiceError mkServiceConfig
      "InternalFailureException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalFailureException #-}
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead"  #-}

-- | The number of policy versions exceeds the limit.
_VersionsLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_VersionsLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "VersionsLimitExceededException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _VersionsLimitExceededException #-}
{-# DEPRECATED _VersionsLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "ServiceUnavailableException"
      Core.. Core.hasStatues 503
{-# INLINEABLE _ServiceUnavailableException #-}
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | An unexpected error has occurred.
_InternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalException
  = Core._MatchServiceError mkServiceConfig "InternalException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalException #-}
{-# DEPRECATED _InternalException "Use generic-lens or generic-optics instead"  #-}

-- | An exception thrown when the version of an entity specified with the @expectedVersion@ parameter does not match the latest version in the system.
_VersionConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_VersionConflictException
  = Core._MatchServiceError mkServiceConfig
      "VersionConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _VersionConflictException #-}
{-# DEPRECATED _VersionConflictException "Use generic-lens or generic-optics instead"  #-}

-- | You are not authorized to perform this operation.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException
  = Core._MatchServiceError mkServiceConfig "UnauthorizedException"
      Core.. Core.hasStatues 401
{-# INLINEABLE _UnauthorizedException #-}
{-# DEPRECATED _UnauthorizedException "Use generic-lens or generic-optics instead"  #-}

-- | An attempt was made to change to an invalid state, for example by deleting a job or a job execution which is "IN_PROGRESS" without setting the @force@ parameter.
_InvalidStateTransitionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateTransitionException
  = Core._MatchServiceError mkServiceConfig
      "InvalidStateTransitionException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _InvalidStateTransitionException #-}
{-# DEPRECATED _InvalidStateTransitionException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | A limit has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 410
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
