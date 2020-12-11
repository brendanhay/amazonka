-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types
  ( -- * Service configuration
    ioTService,

    -- * Errors

    -- * AWSJobAbortCriteriaAbortAction
    AWSJobAbortCriteriaAbortAction (..),

    -- * AWSJobAbortCriteriaFailureType
    AWSJobAbortCriteriaFailureType (..),

    -- * AbortAction
    AbortAction (..),

    -- * ActionType
    ActionType (..),

    -- * AlertTargetType
    AlertTargetType (..),

    -- * AuditCheckRunStatus
    AuditCheckRunStatus (..),

    -- * AuditFindingSeverity
    AuditFindingSeverity (..),

    -- * AuditFrequency
    AuditFrequency (..),

    -- * AuditMitigationActionsExecutionStatus
    AuditMitigationActionsExecutionStatus (..),

    -- * AuditMitigationActionsTaskStatus
    AuditMitigationActionsTaskStatus (..),

    -- * AuditNotificationType
    AuditNotificationType (..),

    -- * AuditTaskStatus
    AuditTaskStatus (..),

    -- * AuditTaskType
    AuditTaskType (..),

    -- * AuthDecision
    AuthDecision (..),

    -- * AuthorizerStatus
    AuthorizerStatus (..),

    -- * AutoRegistrationStatus
    AutoRegistrationStatus (..),

    -- * CACertificateStatus
    CACertificateStatus (..),

    -- * CACertificateUpdateAction
    CACertificateUpdateAction (..),

    -- * CannedAccessControlList
    CannedAccessControlList (..),

    -- * CertificateMode
    CertificateMode (..),

    -- * CertificateStatus
    CertificateStatus (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * DayOfWeek
    DayOfWeek (..),

    -- * DeviceCertificateUpdateAction
    DeviceCertificateUpdateAction (..),

    -- * DimensionType
    DimensionType (..),

    -- * DimensionValueOperator
    DimensionValueOperator (..),

    -- * DomainConfigurationStatus
    DomainConfigurationStatus (..),

    -- * DomainType
    DomainType (..),

    -- * DynamicGroupStatus
    DynamicGroupStatus (..),

    -- * DynamoKeyType
    DynamoKeyType (..),

    -- * EventType
    EventType (..),

    -- * FieldType
    FieldType (..),

    -- * IndexStatus
    IndexStatus (..),

    -- * JobExecutionFailureType
    JobExecutionFailureType (..),

    -- * JobExecutionStatus
    JobExecutionStatus (..),

    -- * JobStatus
    JobStatus (..),

    -- * LogLevel
    LogLevel (..),

    -- * LogTargetType
    LogTargetType (..),

    -- * MessageFormat
    MessageFormat (..),

    -- * MitigationActionType
    MitigationActionType (..),

    -- * OTAUpdateStatus
    OTAUpdateStatus (..),

    -- * PolicyTemplateName
    PolicyTemplateName (..),

    -- * Protocol
    Protocol (..),

    -- * ReportType
    ReportType (..),

    -- * ResourceType
    ResourceType (..),

    -- * ServerCertificateStatus
    ServerCertificateStatus (..),

    -- * ServiceType
    ServiceType (..),

    -- * TargetSelection
    TargetSelection (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * ThingConnectivityIndexingMode
    ThingConnectivityIndexingMode (..),

    -- * ThingGroupIndexingMode
    ThingGroupIndexingMode (..),

    -- * ThingIndexingMode
    ThingIndexingMode (..),

    -- * TopicRuleDestinationStatus
    TopicRuleDestinationStatus (..),

    -- * ViolationEventType
    ViolationEventType (..),

    -- * AWSJobAbortConfig
    AWSJobAbortConfig (..),
    mkAWSJobAbortConfig,
    ajacAbortCriteriaList,

    -- * AWSJobAbortCriteria
    AWSJobAbortCriteria (..),
    mkAWSJobAbortCriteria,
    ajacFailureType,
    ajacAction,
    ajacThresholdPercentage,
    ajacMinNumberOfExecutedThings,

    -- * AWSJobExecutionsRolloutConfig
    AWSJobExecutionsRolloutConfig (..),
    mkAWSJobExecutionsRolloutConfig,
    ajercExponentialRate,
    ajercMaximumPerMinute,

    -- * AWSJobExponentialRolloutRate
    AWSJobExponentialRolloutRate (..),
    mkAWSJobExponentialRolloutRate,
    ajerrBaseRatePerMinute,
    ajerrIncrementFactor,
    ajerrRateIncreaseCriteria,

    -- * AWSJobPresignedURLConfig
    AWSJobPresignedURLConfig (..),
    mkAWSJobPresignedURLConfig,
    ajpucExpiresInSec,

    -- * AWSJobRateIncreaseCriteria
    AWSJobRateIncreaseCriteria (..),
    mkAWSJobRateIncreaseCriteria,
    ajricNumberOfNotifiedThings,
    ajricNumberOfSucceededThings,

    -- * AWSJobTimeoutConfig
    AWSJobTimeoutConfig (..),
    mkAWSJobTimeoutConfig,
    ajtcInProgressTimeoutInMinutes,

    -- * AbortConfig
    AbortConfig (..),
    mkAbortConfig,
    acCriteriaList,

    -- * AbortCriteria
    AbortCriteria (..),
    mkAbortCriteria,
    acFailureType,
    acAction,
    acThresholdPercentage,
    acMinNumberOfExecutedThings,

    -- * Action
    Action (..),
    mkAction,
    aCloudwatchMetric,
    aCloudwatchLogs,
    aDynamoDBv2,
    aStepFunctions,
    aCloudwatchAlarm,
    aSns,
    aDynamoDB,
    aFirehose,
    aTimestream,
    aIotSiteWise,
    aIotAnalytics,
    aLambda,
    aIotEvents,
    aSalesforce,
    aKinesis,
    aS3,
    aHttp,
    aElasticsearch,
    aRepublish,
    aSqs,

    -- * ActiveViolation
    ActiveViolation (..),
    mkActiveViolation,
    avLastViolationValue,
    avLastViolationTime,
    avViolationStartTime,
    avViolationId,
    avBehavior,
    avSecurityProfileName,
    avThingName,

    -- * AddThingsToThingGroupParams
    AddThingsToThingGroupParams (..),
    mkAddThingsToThingGroupParams,
    atttgpOverrideDynamicGroups,
    atttgpThingGroupNames,

    -- * AlertTarget
    AlertTarget (..),
    mkAlertTarget,
    atAlertTargetARN,
    atRoleARN,

    -- * Allowed
    Allowed (..),
    mkAllowed,
    aPolicies,

    -- * AssetPropertyTimestamp
    AssetPropertyTimestamp (..),
    mkAssetPropertyTimestamp,
    aptOffsetInNanos,
    aptTimeInSeconds,

    -- * AssetPropertyValue
    AssetPropertyValue (..),
    mkAssetPropertyValue,
    apvQuality,
    apvValue,
    apvTimestamp,

    -- * AssetPropertyVariant
    AssetPropertyVariant (..),
    mkAssetPropertyVariant,
    apvIntegerValue,
    apvDoubleValue,
    apvStringValue,
    apvBooleanValue,

    -- * AttributePayload
    AttributePayload (..),
    mkAttributePayload,
    apAttributes,
    apMerge,

    -- * AuditCheckConfiguration
    AuditCheckConfiguration (..),
    mkAuditCheckConfiguration,
    accEnabled,

    -- * AuditCheckDetails
    AuditCheckDetails (..),
    mkAuditCheckDetails,
    acdSuppressedNonCompliantResourcesCount,
    acdTotalResourcesCount,
    acdCheckCompliant,
    acdNonCompliantResourcesCount,
    acdErrorCode,
    acdMessage,
    acdCheckRunStatus,

    -- * AuditFinding
    AuditFinding (..),
    mkAuditFinding,
    afIsSuppressed,
    afTaskId,
    afFindingTime,
    afTaskStartTime,
    afReasonForNonComplianceCode,
    afSeverity,
    afRelatedResources,
    afCheckName,
    afNonCompliantResource,
    afReasonForNonCompliance,
    afFindingId,

    -- * AuditMitigationActionExecutionMetadata
    AuditMitigationActionExecutionMetadata (..),
    mkAuditMitigationActionExecutionMetadata,
    amaemStatus,
    amaemStartTime,
    amaemTaskId,
    amaemActionId,
    amaemActionName,
    amaemEndTime,
    amaemErrorCode,
    amaemFindingId,
    amaemMessage,

    -- * AuditMitigationActionsTaskMetadata
    AuditMitigationActionsTaskMetadata (..),
    mkAuditMitigationActionsTaskMetadata,
    amatmStartTime,
    amatmTaskId,
    amatmTaskStatus,

    -- * AuditMitigationActionsTaskTarget
    AuditMitigationActionsTaskTarget (..),
    mkAuditMitigationActionsTaskTarget,
    amattAuditTaskId,
    amattFindingIds,
    amattAuditCheckToReasonCodeFilter,

    -- * AuditNotificationTarget
    AuditNotificationTarget (..),
    mkAuditNotificationTarget,
    antTargetARN,
    antEnabled,
    antRoleARN,

    -- * AuditSuppression
    AuditSuppression (..),
    mkAuditSuppression,
    asExpirationDate,
    asSuppressIndefinitely,
    asDescription,
    asCheckName,
    asResourceIdentifier,

    -- * AuditTaskMetadata
    AuditTaskMetadata (..),
    mkAuditTaskMetadata,
    atmTaskType,
    atmTaskId,
    atmTaskStatus,

    -- * AuthInfo
    AuthInfo (..),
    mkAuthInfo,
    aiActionType,
    aiResources,

    -- * AuthResult
    AuthResult (..),
    mkAuthResult,
    arDenied,
    arAuthDecision,
    arAllowed,
    arMissingContextValues,
    arAuthInfo,

    -- * AuthorizerConfig
    AuthorizerConfig (..),
    mkAuthorizerConfig,
    acAllowAuthorizerOverride,
    acDefaultAuthorizerName,

    -- * AuthorizerDescription
    AuthorizerDescription (..),
    mkAuthorizerDescription,
    adStatus,
    adLastModifiedDate,
    adSigningDisabled,
    adAuthorizerName,
    adAuthorizerFunctionARN,
    adAuthorizerARN,
    adCreationDate,
    adTokenSigningPublicKeys,
    adTokenKeyName,

    -- * AuthorizerSummary
    AuthorizerSummary (..),
    mkAuthorizerSummary,
    asAuthorizerName,
    asAuthorizerARN,

    -- * Behavior
    Behavior (..),
    mkBehavior,
    bMetricDimension,
    bMetric,
    bCriteria,
    bName,

    -- * BehaviorCriteria
    BehaviorCriteria (..),
    mkBehaviorCriteria,
    bcValue,
    bcConsecutiveDatapointsToAlarm,
    bcComparisonOperator,
    bcStatisticalThreshold,
    bcDurationSeconds,
    bcConsecutiveDatapointsToClear,

    -- * BillingGroupMetadata
    BillingGroupMetadata (..),
    mkBillingGroupMetadata,
    bgmCreationDate,

    -- * BillingGroupProperties
    BillingGroupProperties (..),
    mkBillingGroupProperties,
    bgpBillingGroupDescription,

    -- * CACertificate
    CACertificate (..),
    mkCACertificate,
    cacStatus,
    cacCertificateARN,
    cacCertificateId,
    cacCreationDate,

    -- * CACertificateDescription
    CACertificateDescription (..),
    mkCACertificateDescription,
    cacdStatus,
    cacdOwnedBy,
    cacdLastModifiedDate,
    cacdCertificatePem,
    cacdCertificateARN,
    cacdCertificateId,
    cacdValidity,
    cacdAutoRegistrationStatus,
    cacdCreationDate,
    cacdGenerationId,
    cacdCustomerVersion,

    -- * Certificate
    Certificate (..),
    mkCertificate,
    cStatus,
    cCertificateARN,
    cCertificateId,
    cCertificateMode,
    cCreationDate,

    -- * CertificateDescription
    CertificateDescription (..),
    mkCertificateDescription,
    cdStatus,
    cdOwnedBy,
    cdLastModifiedDate,
    cdCaCertificateId,
    cdPreviousOwnedBy,
    cdCertificatePem,
    cdCertificateARN,
    cdCertificateId,
    cdCertificateMode,
    cdValidity,
    cdCreationDate,
    cdGenerationId,
    cdTransferData,
    cdCustomerVersion,

    -- * CertificateValidity
    CertificateValidity (..),
    mkCertificateValidity,
    cvNotBefore,
    cvNotAfter,

    -- * CloudwatchAlarmAction
    CloudwatchAlarmAction (..),
    mkCloudwatchAlarmAction,
    caaRoleARN,
    caaAlarmName,
    caaStateReason,
    caaStateValue,

    -- * CloudwatchLogsAction
    CloudwatchLogsAction (..),
    mkCloudwatchLogsAction,
    claRoleARN,
    claLogGroupName,

    -- * CloudwatchMetricAction
    CloudwatchMetricAction (..),
    mkCloudwatchMetricAction,
    cmaMetricTimestamp,
    cmaRoleARN,
    cmaMetricNamespace,
    cmaMetricName,
    cmaMetricValue,
    cmaMetricUnit,

    -- * CodeSigning
    CodeSigning (..),
    mkCodeSigning,
    csCustomCodeSigning,
    csStartSigningJobParameter,
    csAwsSignerJobId,

    -- * CodeSigningCertificateChain
    CodeSigningCertificateChain (..),
    mkCodeSigningCertificateChain,
    csccCertificateName,
    csccInlineDocument,

    -- * CodeSigningSignature
    CodeSigningSignature (..),
    mkCodeSigningSignature,
    cssInlineDocument,

    -- * Configuration
    Configuration (..),
    mkConfiguration,
    cEnabled,

    -- * CustomCodeSigning
    CustomCodeSigning (..),
    mkCustomCodeSigning,
    ccsSignature,
    ccsHashAlgorithm,
    ccsCertificateChain,
    ccsSignatureAlgorithm,

    -- * Denied
    Denied (..),
    mkDenied,
    dImplicitDeny,
    dExplicitDeny,

    -- * Destination
    Destination (..),
    mkDestination,
    dS3Destination,

    -- * DomainConfigurationSummary
    DomainConfigurationSummary (..),
    mkDomainConfigurationSummary,
    dcsDomainConfigurationName,
    dcsDomainConfigurationARN,
    dcsServiceType,

    -- * DynamoDBAction
    DynamoDBAction (..),
    mkDynamoDBAction,
    ddbaHashKeyType,
    ddbaOperation,
    ddbaRangeKeyType,
    ddbaPayloadField,
    ddbaRangeKeyField,
    ddbaRangeKeyValue,
    ddbaTableName,
    ddbaRoleARN,
    ddbaHashKeyField,
    ddbaHashKeyValue,

    -- * DynamoDBv2Action
    DynamoDBv2Action (..),
    mkDynamoDBv2Action,
    ddaRoleARN,
    ddaPutItem,

    -- * EffectivePolicy
    EffectivePolicy (..),
    mkEffectivePolicy,
    epPolicyName,
    epPolicyDocument,
    epPolicyARN,

    -- * ElasticsearchAction
    ElasticsearchAction (..),
    mkElasticsearchAction,
    eaRoleARN,
    eaEndpoint,
    eaIndex,
    eaType,
    eaId,

    -- * EnableIOTLoggingParams
    EnableIOTLoggingParams (..),
    mkEnableIOTLoggingParams,
    eiotlpRoleARNForLogging,
    eiotlpLogLevel,

    -- * ErrorInfo
    ErrorInfo (..),
    mkErrorInfo,
    eiCode,
    eiMessage,

    -- * ExplicitDeny
    ExplicitDeny (..),
    mkExplicitDeny,
    edPolicies,

    -- * ExponentialRolloutRate
    ExponentialRolloutRate (..),
    mkExponentialRolloutRate,
    errBaseRatePerMinute,
    errIncrementFactor,
    errRateIncreaseCriteria,

    -- * Field
    Field (..),
    mkField,
    fName,
    fType,

    -- * FileLocation
    FileLocation (..),
    mkFileLocation,
    flStream,
    flS3Location,

    -- * FirehoseAction
    FirehoseAction (..),
    mkFirehoseAction,
    faBatchMode,
    faSeparator,
    faRoleARN,
    faDeliveryStreamName,

    -- * GroupNameAndARN
    GroupNameAndARN (..),
    mkGroupNameAndARN,
    gnaaGroupARN,
    gnaaGroupName,

    -- * HTTPAction
    HTTPAction (..),
    mkHTTPAction,
    httpaConfirmationURL,
    httpaAuth,
    httpaHeaders,
    httpaUrl,

    -- * HTTPActionHeader
    HTTPActionHeader (..),
    mkHTTPActionHeader,
    httpahKey,
    httpahValue,

    -- * HTTPAuthorization
    HTTPAuthorization (..),
    mkHTTPAuthorization,
    httpaSigv4,

    -- * HTTPContext
    HTTPContext (..),
    mkHTTPContext,
    httpcHeaders,
    httpcQueryString,

    -- * HTTPURLDestinationConfiguration
    HTTPURLDestinationConfiguration (..),
    mkHTTPURLDestinationConfiguration,
    httpudcConfirmationURL,

    -- * HTTPURLDestinationProperties
    HTTPURLDestinationProperties (..),
    mkHTTPURLDestinationProperties,
    httpudpConfirmationURL,

    -- * HTTPURLDestinationSummary
    HTTPURLDestinationSummary (..),
    mkHTTPURLDestinationSummary,
    httpudsConfirmationURL,

    -- * ImplicitDeny
    ImplicitDeny (..),
    mkImplicitDeny,
    idPolicies,

    -- * IotAnalyticsAction
    IotAnalyticsAction (..),
    mkIotAnalyticsAction,
    iaaBatchMode,
    iaaChannelARN,
    iaaChannelName,
    iaaRoleARN,

    -- * IotEventsAction
    IotEventsAction (..),
    mkIotEventsAction,
    ieaBatchMode,
    ieaMessageId,
    ieaInputName,
    ieaRoleARN,

    -- * IotSiteWiseAction
    IotSiteWiseAction (..),
    mkIotSiteWiseAction,
    iswaPutAssetPropertyValueEntries,
    iswaRoleARN,

    -- * Job
    Job (..),
    mkJob,
    jStatus,
    jJobExecutionsRolloutConfig,
    jJobId,
    jLastUpdatedAt,
    jJobARN,
    jCreatedAt,
    jAbortConfig,
    jJobProcessDetails,
    jNamespaceId,
    jReasonCode,
    jPresignedURLConfig,
    jForceCanceled,
    jTargets,
    jCompletedAt,
    jComment,
    jDescription,
    jTargetSelection,
    jTimeoutConfig,

    -- * JobExecution
    JobExecution (..),
    mkJobExecution,
    jeStatus,
    jeJobId,
    jeLastUpdatedAt,
    jeApproximateSecondsBeforeTimedOut,
    jeQueuedAt,
    jeStatusDetails,
    jeThingARN,
    jeExecutionNumber,
    jeVersionNumber,
    jeStartedAt,
    jeForceCanceled,

    -- * JobExecutionStatusDetails
    JobExecutionStatusDetails (..),
    mkJobExecutionStatusDetails,
    jesdDetailsMap,

    -- * JobExecutionSummary
    JobExecutionSummary (..),
    mkJobExecutionSummary,
    jesStatus,
    jesLastUpdatedAt,
    jesQueuedAt,
    jesExecutionNumber,
    jesStartedAt,

    -- * JobExecutionSummaryForJob
    JobExecutionSummaryForJob (..),
    mkJobExecutionSummaryForJob,
    jesfjJobExecutionSummary,
    jesfjThingARN,

    -- * JobExecutionSummaryForThing
    JobExecutionSummaryForThing (..),
    mkJobExecutionSummaryForThing,
    jesftJobId,
    jesftJobExecutionSummary,

    -- * JobExecutionsRolloutConfig
    JobExecutionsRolloutConfig (..),
    mkJobExecutionsRolloutConfig,
    jercExponentialRate,
    jercMaximumPerMinute,

    -- * JobProcessDetails
    JobProcessDetails (..),
    mkJobProcessDetails,
    jpdNumberOfRemovedThings,
    jpdNumberOfQueuedThings,
    jpdNumberOfFailedThings,
    jpdNumberOfSucceededThings,
    jpdNumberOfInProgressThings,
    jpdNumberOfCanceledThings,
    jpdNumberOfTimedOutThings,
    jpdNumberOfRejectedThings,
    jpdProcessingTargets,

    -- * JobSummary
    JobSummary (..),
    mkJobSummary,
    jsStatus,
    jsJobId,
    jsLastUpdatedAt,
    jsJobARN,
    jsCreatedAt,
    jsThingGroupId,
    jsCompletedAt,
    jsTargetSelection,

    -- * KeyPair
    KeyPair (..),
    mkKeyPair,
    kpPrivateKey,
    kpPublicKey,

    -- * KinesisAction
    KinesisAction (..),
    mkKinesisAction,
    kaPartitionKey,
    kaRoleARN,
    kaStreamName,

    -- * LambdaAction
    LambdaAction (..),
    mkLambdaAction,
    laFunctionARN,

    -- * LogTarget
    LogTarget (..),
    mkLogTarget,
    ltTargetName,
    ltTargetType,

    -- * LogTargetConfiguration
    LogTargetConfiguration (..),
    mkLogTargetConfiguration,
    ltcLogLevel,
    ltcLogTarget,

    -- * LoggingOptionsPayload
    LoggingOptionsPayload (..),
    mkLoggingOptionsPayload,
    lopLogLevel,
    lopRoleARN,

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdOperator,
    mdDimensionName,

    -- * MetricToRetain
    MetricToRetain (..),
    mkMetricToRetain,
    mtrMetricDimension,
    mtrMetric,

    -- * MetricValue
    MetricValue (..),
    mkMetricValue,
    mvCidrs,
    mvCount,
    mvPorts,

    -- * MitigationAction
    MitigationAction (..),
    mkMitigationAction,
    maActionParams,
    maName,
    maId,
    maRoleARN,

    -- * MitigationActionIdentifier
    MitigationActionIdentifier (..),
    mkMitigationActionIdentifier,
    maiActionName,
    maiCreationDate,
    maiActionARN,

    -- * MitigationActionParams
    MitigationActionParams (..),
    mkMitigationActionParams,
    mapEnableIOTLoggingParams,
    mapAddThingsToThingGroupParams,
    mapUpdateCACertificateParams,
    mapUpdateDeviceCertificateParams,
    mapReplaceDefaultPolicyVersionParams,
    mapPublishFindingToSNSParams,

    -- * MqttContext
    MqttContext (..),
    mkMqttContext,
    mcClientId,
    mcUsername,
    mcPassword,

    -- * NonCompliantResource
    NonCompliantResource (..),
    mkNonCompliantResource,
    ncrAdditionalInfo,
    ncrResourceType,
    ncrResourceIdentifier,

    -- * OTAUpdateFile
    OTAUpdateFile (..),
    mkOTAUpdateFile,
    otaufFileLocation,
    otaufFileType,
    otaufFileVersion,
    otaufAttributes,
    otaufCodeSigning,
    otaufFileName,

    -- * OTAUpdateInfo
    OTAUpdateInfo (..),
    mkOTAUpdateInfo,
    otauiLastModifiedDate,
    otauiAwsJobExecutionsRolloutConfig,
    otauiAwsIotJobId,
    otauiProtocols,
    otauiAwsJobPresignedURLConfig,
    otauiOtaUpdateFiles,
    otauiOtaUpdateStatus,
    otauiTargets,
    otauiAwsIotJobARN,
    otauiCreationDate,
    otauiAdditionalParameters,
    otauiOtaUpdateId,
    otauiErrorInfo,
    otauiOtaUpdateARN,
    otauiDescription,
    otauiTargetSelection,

    -- * OTAUpdateSummary
    OTAUpdateSummary (..),
    mkOTAUpdateSummary,
    otausCreationDate,
    otausOtaUpdateId,
    otausOtaUpdateARN,

    -- * OutgoingCertificate
    OutgoingCertificate (..),
    mkOutgoingCertificate,
    ocTransferDate,
    ocCertificateARN,
    ocCertificateId,
    ocTransferredTo,
    ocCreationDate,
    ocTransferMessage,

    -- * PercentPair
    PercentPair (..),
    mkPercentPair,
    ppValue,
    ppPercent,

    -- * Policy
    Policy (..),
    mkPolicy,
    pPolicyName,
    pPolicyARN,

    -- * PolicyVersion
    PolicyVersion (..),
    mkPolicyVersion,
    pvVersionId,
    pvCreateDate,
    pvIsDefaultVersion,

    -- * PolicyVersionIdentifier
    PolicyVersionIdentifier (..),
    mkPolicyVersionIdentifier,
    pviPolicyName,
    pviPolicyVersionId,

    -- * PresignedURLConfig
    PresignedURLConfig (..),
    mkPresignedURLConfig,
    pucExpiresInSec,
    pucRoleARN,

    -- * ProvisioningHook
    ProvisioningHook (..),
    mkProvisioningHook,
    phPayloadVersion,
    phTargetARN,

    -- * ProvisioningTemplateSummary
    ProvisioningTemplateSummary (..),
    mkProvisioningTemplateSummary,
    ptsLastModifiedDate,
    ptsTemplateName,
    ptsEnabled,
    ptsCreationDate,
    ptsTemplateARN,
    ptsDescription,

    -- * ProvisioningTemplateVersionSummary
    ProvisioningTemplateVersionSummary (..),
    mkProvisioningTemplateVersionSummary,
    ptvsVersionId,
    ptvsCreationDate,
    ptvsIsDefaultVersion,

    -- * PublishFindingToSNSParams
    PublishFindingToSNSParams (..),
    mkPublishFindingToSNSParams,
    pftspTopicARN,

    -- * PutAssetPropertyValueEntry
    PutAssetPropertyValueEntry (..),
    mkPutAssetPropertyValueEntry,
    papveEntryId,
    papvePropertyAlias,
    papvePropertyId,
    papveAssetId,
    papvePropertyValues,

    -- * PutItemInput
    PutItemInput (..),
    mkPutItemInput,
    piiTableName,

    -- * RateIncreaseCriteria
    RateIncreaseCriteria (..),
    mkRateIncreaseCriteria,
    ricNumberOfNotifiedThings,
    ricNumberOfSucceededThings,

    -- * RegistrationConfig
    RegistrationConfig (..),
    mkRegistrationConfig,
    rcTemplateBody,
    rcRoleARN,

    -- * RelatedResource
    RelatedResource (..),
    mkRelatedResource,
    rrAdditionalInfo,
    rrResourceType,
    rrResourceIdentifier,

    -- * ReplaceDefaultPolicyVersionParams
    ReplaceDefaultPolicyVersionParams (..),
    mkReplaceDefaultPolicyVersionParams,
    rdpvpTemplateName,

    -- * RepublishAction
    RepublishAction (..),
    mkRepublishAction,
    raQos,
    raRoleARN,
    raTopic,

    -- * ResourceIdentifier
    ResourceIdentifier (..),
    mkResourceIdentifier,
    riIamRoleARN,
    riClientId,
    riRoleAliasARN,
    riCaCertificateId,
    riDeviceCertificateId,
    riAccount,
    riPolicyVersionIdentifier,
    riCognitoIdentityPoolId,

    -- * RoleAliasDescription
    RoleAliasDescription (..),
    mkRoleAliasDescription,
    radRoleAliasARN,
    radLastModifiedDate,
    radRoleAlias,
    radOwner,
    radCreationDate,
    radCredentialDurationSeconds,
    radRoleARN,

    -- * S3Action
    S3Action (..),
    mkS3Action,
    sCannedACL,
    sRoleARN,
    sBucketName,
    sKey,

    -- * S3Destination
    S3Destination (..),
    mkS3Destination,
    sdPrefix,
    sdBucket,

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,
    slVersion,

    -- * SNSAction
    SNSAction (..),
    mkSNSAction,
    snsaMessageFormat,
    snsaTargetARN,
    snsaRoleARN,

    -- * SalesforceAction
    SalesforceAction (..),
    mkSalesforceAction,
    saToken,
    saUrl,

    -- * ScheduledAuditMetadata
    ScheduledAuditMetadata (..),
    mkScheduledAuditMetadata,
    samFrequency,
    samScheduledAuditName,
    samDayOfMonth,
    samDayOfWeek,
    samScheduledAuditARN,

    -- * SecurityProfileIdentifier
    SecurityProfileIdentifier (..),
    mkSecurityProfileIdentifier,
    spiName,
    spiArn,

    -- * SecurityProfileTarget
    SecurityProfileTarget (..),
    mkSecurityProfileTarget,
    sptArn,

    -- * SecurityProfileTargetMapping
    SecurityProfileTargetMapping (..),
    mkSecurityProfileTargetMapping,
    sptmSecurityProfileIdentifier,
    sptmTarget,

    -- * ServerCertificateSummary
    ServerCertificateSummary (..),
    mkServerCertificateSummary,
    scsServerCertificateStatusDetail,
    scsServerCertificateStatus,
    scsServerCertificateARN,

    -- * SigV4Authorization
    SigV4Authorization (..),
    mkSigV4Authorization,
    svaSigningRegion,
    svaServiceName,
    svaRoleARN,

    -- * SigningProfileParameter
    SigningProfileParameter (..),
    mkSigningProfileParameter,
    sppPlatform,
    sppCertificateARN,
    sppCertificatePathOnDevice,

    -- * SqsAction
    SqsAction (..),
    mkSqsAction,
    saUseBase64,
    saRoleARN,
    saQueueURL,

    -- * StartSigningJobParameter
    StartSigningJobParameter (..),
    mkStartSigningJobParameter,
    ssjpDestination,
    ssjpSigningProfileName,
    ssjpSigningProfileParameter,

    -- * StatisticalThreshold
    StatisticalThreshold (..),
    mkStatisticalThreshold,
    stStatistic,

    -- * Statistics
    Statistics (..),
    mkStatistics,
    sStdDeviation,
    sMaximum,
    sAverage,
    sCount,
    sMinimum,
    sVariance,
    sSumOfSquares,
    sSum,

    -- * StepFunctionsAction
    StepFunctionsAction (..),
    mkStepFunctionsAction,
    sfaExecutionNamePrefix,
    sfaStateMachineName,
    sfaRoleARN,

    -- * Stream
    Stream (..),
    mkStream,
    sFileId,
    sStreamId,

    -- * StreamFile
    StreamFile (..),
    mkStreamFile,
    sfS3Location,
    sfFileId,

    -- * StreamInfo
    StreamInfo (..),
    mkStreamInfo,
    siLastUpdatedAt,
    siCreatedAt,
    siStreamVersion,
    siStreamARN,
    siFiles,
    siDescription,
    siStreamId,
    siRoleARN,

    -- * StreamSummary
    StreamSummary (..),
    mkStreamSummary,
    ssStreamVersion,
    ssStreamARN,
    ssDescription,
    ssStreamId,

    -- * TLSContext
    TLSContext (..),
    mkTLSContext,
    tcServerName,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TaskStatistics
    TaskStatistics (..),
    mkTaskStatistics,
    tsNonCompliantChecks,
    tsWaitingForDataCollectionChecks,
    tsFailedChecks,
    tsTotalChecks,
    tsInProgressChecks,
    tsCompliantChecks,
    tsCanceledChecks,

    -- * TaskStatisticsForAuditCheck
    TaskStatisticsForAuditCheck (..),
    mkTaskStatisticsForAuditCheck,
    tsfacCanceledFindingsCount,
    tsfacSkippedFindingsCount,
    tsfacTotalFindingsCount,
    tsfacFailedFindingsCount,
    tsfacSucceededFindingsCount,

    -- * ThingAttribute
    ThingAttribute (..),
    mkThingAttribute,
    taThingTypeName,
    taThingARN,
    taAttributes,
    taVersion,
    taThingName,

    -- * ThingConnectivity
    ThingConnectivity (..),
    mkThingConnectivity,
    tcConnected,
    tcTimestamp,

    -- * ThingDocument
    ThingDocument (..),
    mkThingDocument,
    tdThingGroupNames,
    tdThingTypeName,
    tdShadow,
    tdAttributes,
    tdConnectivity,
    tdThingName,
    tdThingId,

    -- * ThingGroupDocument
    ThingGroupDocument (..),
    mkThingGroupDocument,
    tgdParentGroupNames,
    tgdThingGroupId,
    tgdThingGroupName,
    tgdAttributes,
    tgdThingGroupDescription,

    -- * ThingGroupIndexingConfiguration
    ThingGroupIndexingConfiguration (..),
    mkThingGroupIndexingConfiguration,
    tgicManagedFields,
    tgicCustomFields,
    tgicThingGroupIndexingMode,

    -- * ThingGroupMetadata
    ThingGroupMetadata (..),
    mkThingGroupMetadata,
    tgmRootToParentThingGroups,
    tgmParentGroupName,
    tgmCreationDate,

    -- * ThingGroupProperties
    ThingGroupProperties (..),
    mkThingGroupProperties,
    tgpAttributePayload,
    tgpThingGroupDescription,

    -- * ThingIndexingConfiguration
    ThingIndexingConfiguration (..),
    mkThingIndexingConfiguration,
    ticManagedFields,
    ticThingConnectivityIndexingMode,
    ticCustomFields,
    ticThingIndexingMode,

    -- * ThingTypeDefinition
    ThingTypeDefinition (..),
    mkThingTypeDefinition,
    ttdThingTypeProperties,
    ttdThingTypeName,
    ttdThingTypeMetadata,
    ttdThingTypeARN,

    -- * ThingTypeMetadata
    ThingTypeMetadata (..),
    mkThingTypeMetadata,
    ttmDeprecationDate,
    ttmCreationDate,
    ttmDeprecated,

    -- * ThingTypeProperties
    ThingTypeProperties (..),
    mkThingTypeProperties,
    ttpSearchableAttributes,
    ttpThingTypeDescription,

    -- * TimeoutConfig
    TimeoutConfig (..),
    mkTimeoutConfig,
    tcInProgressTimeoutInMinutes,

    -- * TimestreamAction
    TimestreamAction (..),
    mkTimestreamAction,
    taTimestamp,
    taRoleARN,
    taDatabaseName,
    taTableName,
    taDimensions,

    -- * TimestreamDimension
    TimestreamDimension (..),
    mkTimestreamDimension,
    tdName,
    tdValue,

    -- * TimestreamTimestamp
    TimestreamTimestamp (..),
    mkTimestreamTimestamp,
    ttValue,
    ttUnit,

    -- * TopicRule
    TopicRule (..),
    mkTopicRule,
    trCreatedAt,
    trActions,
    trAwsIotSqlVersion,
    trErrorAction,
    trRuleDisabled,
    trRuleName,
    trSql,
    trDescription,

    -- * TopicRuleDestination
    TopicRuleDestination (..),
    mkTopicRuleDestination,
    trdStatus,
    trdHttpURLProperties,
    trdArn,
    trdStatusReason,

    -- * TopicRuleDestinationConfiguration
    TopicRuleDestinationConfiguration (..),
    mkTopicRuleDestinationConfiguration,
    trdcHttpURLConfiguration,

    -- * TopicRuleDestinationSummary
    TopicRuleDestinationSummary (..),
    mkTopicRuleDestinationSummary,
    trdsStatus,
    trdsHttpURLSummary,
    trdsArn,
    trdsStatusReason,

    -- * TopicRuleListItem
    TopicRuleListItem (..),
    mkTopicRuleListItem,
    trliCreatedAt,
    trliRuleDisabled,
    trliRuleName,
    trliRuleARN,
    trliTopicPattern,

    -- * TopicRulePayload
    TopicRulePayload (..),
    mkTopicRulePayload,
    trpAwsIotSqlVersion,
    trpErrorAction,
    trpRuleDisabled,
    trpDescription,
    trpSql,
    trpActions,

    -- * TransferData
    TransferData (..),
    mkTransferData,
    tdTransferDate,
    tdAcceptDate,
    tdTransferMessage,
    tdRejectDate,
    tdRejectReason,

    -- * UpdateCACertificateParams
    UpdateCACertificateParams (..),
    mkUpdateCACertificateParams,
    ucacpAction,

    -- * UpdateDeviceCertificateParams
    UpdateDeviceCertificateParams (..),
    mkUpdateDeviceCertificateParams,
    udcpAction,

    -- * ValidationError
    ValidationError (..),
    mkValidationError,
    veErrorMessage,

    -- * ViolationEvent
    ViolationEvent (..),
    mkViolationEvent,
    veViolationEventType,
    veViolationId,
    veBehavior,
    veMetricValue,
    veSecurityProfileName,
    veViolationEventTime,
    veThingName,
  )
where

import Network.AWS.IoT.Types.AWSJobAbortConfig
import Network.AWS.IoT.Types.AWSJobAbortCriteria
import Network.AWS.IoT.Types.AWSJobAbortCriteriaAbortAction
import Network.AWS.IoT.Types.AWSJobAbortCriteriaFailureType
import Network.AWS.IoT.Types.AWSJobExecutionsRolloutConfig
import Network.AWS.IoT.Types.AWSJobExponentialRolloutRate
import Network.AWS.IoT.Types.AWSJobPresignedURLConfig
import Network.AWS.IoT.Types.AWSJobRateIncreaseCriteria
import Network.AWS.IoT.Types.AWSJobTimeoutConfig
import Network.AWS.IoT.Types.AbortAction
import Network.AWS.IoT.Types.AbortConfig
import Network.AWS.IoT.Types.AbortCriteria
import Network.AWS.IoT.Types.Action
import Network.AWS.IoT.Types.ActionType
import Network.AWS.IoT.Types.ActiveViolation
import Network.AWS.IoT.Types.AddThingsToThingGroupParams
import Network.AWS.IoT.Types.AlertTarget
import Network.AWS.IoT.Types.AlertTargetType
import Network.AWS.IoT.Types.Allowed
import Network.AWS.IoT.Types.AssetPropertyTimestamp
import Network.AWS.IoT.Types.AssetPropertyValue
import Network.AWS.IoT.Types.AssetPropertyVariant
import Network.AWS.IoT.Types.AttributePayload
import Network.AWS.IoT.Types.AuditCheckConfiguration
import Network.AWS.IoT.Types.AuditCheckDetails
import Network.AWS.IoT.Types.AuditCheckRunStatus
import Network.AWS.IoT.Types.AuditFinding
import Network.AWS.IoT.Types.AuditFindingSeverity
import Network.AWS.IoT.Types.AuditFrequency
import Network.AWS.IoT.Types.AuditMitigationActionExecutionMetadata
import Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
import Network.AWS.IoT.Types.AuditMitigationActionsTaskMetadata
import Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
import Network.AWS.IoT.Types.AuditMitigationActionsTaskTarget
import Network.AWS.IoT.Types.AuditNotificationTarget
import Network.AWS.IoT.Types.AuditNotificationType
import Network.AWS.IoT.Types.AuditSuppression
import Network.AWS.IoT.Types.AuditTaskMetadata
import Network.AWS.IoT.Types.AuditTaskStatus
import Network.AWS.IoT.Types.AuditTaskType
import Network.AWS.IoT.Types.AuthDecision
import Network.AWS.IoT.Types.AuthInfo
import Network.AWS.IoT.Types.AuthResult
import Network.AWS.IoT.Types.AuthorizerConfig
import Network.AWS.IoT.Types.AuthorizerDescription
import Network.AWS.IoT.Types.AuthorizerStatus
import Network.AWS.IoT.Types.AuthorizerSummary
import Network.AWS.IoT.Types.AutoRegistrationStatus
import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.BehaviorCriteria
import Network.AWS.IoT.Types.BillingGroupMetadata
import Network.AWS.IoT.Types.BillingGroupProperties
import Network.AWS.IoT.Types.CACertificate
import Network.AWS.IoT.Types.CACertificateDescription
import Network.AWS.IoT.Types.CACertificateStatus
import Network.AWS.IoT.Types.CACertificateUpdateAction
import Network.AWS.IoT.Types.CannedAccessControlList
import Network.AWS.IoT.Types.Certificate
import Network.AWS.IoT.Types.CertificateDescription
import Network.AWS.IoT.Types.CertificateMode
import Network.AWS.IoT.Types.CertificateStatus
import Network.AWS.IoT.Types.CertificateValidity
import Network.AWS.IoT.Types.CloudwatchAlarmAction
import Network.AWS.IoT.Types.CloudwatchLogsAction
import Network.AWS.IoT.Types.CloudwatchMetricAction
import Network.AWS.IoT.Types.CodeSigning
import Network.AWS.IoT.Types.CodeSigningCertificateChain
import Network.AWS.IoT.Types.CodeSigningSignature
import Network.AWS.IoT.Types.ComparisonOperator
import Network.AWS.IoT.Types.Configuration
import Network.AWS.IoT.Types.CustomCodeSigning
import Network.AWS.IoT.Types.DayOfWeek
import Network.AWS.IoT.Types.Denied
import Network.AWS.IoT.Types.Destination
import Network.AWS.IoT.Types.DeviceCertificateUpdateAction
import Network.AWS.IoT.Types.DimensionType
import Network.AWS.IoT.Types.DimensionValueOperator
import Network.AWS.IoT.Types.DomainConfigurationStatus
import Network.AWS.IoT.Types.DomainConfigurationSummary
import Network.AWS.IoT.Types.DomainType
import Network.AWS.IoT.Types.DynamicGroupStatus
import Network.AWS.IoT.Types.DynamoDBAction
import Network.AWS.IoT.Types.DynamoDBv2Action
import Network.AWS.IoT.Types.DynamoKeyType
import Network.AWS.IoT.Types.EffectivePolicy
import Network.AWS.IoT.Types.ElasticsearchAction
import Network.AWS.IoT.Types.EnableIOTLoggingParams
import Network.AWS.IoT.Types.ErrorInfo
import Network.AWS.IoT.Types.EventType
import Network.AWS.IoT.Types.ExplicitDeny
import Network.AWS.IoT.Types.ExponentialRolloutRate
import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.FieldType
import Network.AWS.IoT.Types.FileLocation
import Network.AWS.IoT.Types.FirehoseAction
import Network.AWS.IoT.Types.GroupNameAndARN
import Network.AWS.IoT.Types.HTTPAction
import Network.AWS.IoT.Types.HTTPActionHeader
import Network.AWS.IoT.Types.HTTPAuthorization
import Network.AWS.IoT.Types.HTTPContext
import Network.AWS.IoT.Types.HTTPURLDestinationConfiguration
import Network.AWS.IoT.Types.HTTPURLDestinationProperties
import Network.AWS.IoT.Types.HTTPURLDestinationSummary
import Network.AWS.IoT.Types.ImplicitDeny
import Network.AWS.IoT.Types.IndexStatus
import Network.AWS.IoT.Types.IotAnalyticsAction
import Network.AWS.IoT.Types.IotEventsAction
import Network.AWS.IoT.Types.IotSiteWiseAction
import Network.AWS.IoT.Types.Job
import Network.AWS.IoT.Types.JobExecution
import Network.AWS.IoT.Types.JobExecutionFailureType
import Network.AWS.IoT.Types.JobExecutionStatus
import Network.AWS.IoT.Types.JobExecutionStatusDetails
import Network.AWS.IoT.Types.JobExecutionSummary
import Network.AWS.IoT.Types.JobExecutionSummaryForJob
import Network.AWS.IoT.Types.JobExecutionSummaryForThing
import Network.AWS.IoT.Types.JobExecutionsRolloutConfig
import Network.AWS.IoT.Types.JobProcessDetails
import Network.AWS.IoT.Types.JobStatus
import Network.AWS.IoT.Types.JobSummary
import Network.AWS.IoT.Types.KeyPair
import Network.AWS.IoT.Types.KinesisAction
import Network.AWS.IoT.Types.LambdaAction
import Network.AWS.IoT.Types.LogLevel
import Network.AWS.IoT.Types.LogTarget
import Network.AWS.IoT.Types.LogTargetConfiguration
import Network.AWS.IoT.Types.LogTargetType
import Network.AWS.IoT.Types.LoggingOptionsPayload
import Network.AWS.IoT.Types.MessageFormat
import Network.AWS.IoT.Types.MetricDimension
import Network.AWS.IoT.Types.MetricToRetain
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.MitigationAction
import Network.AWS.IoT.Types.MitigationActionIdentifier
import Network.AWS.IoT.Types.MitigationActionParams
import Network.AWS.IoT.Types.MitigationActionType
import Network.AWS.IoT.Types.MqttContext
import Network.AWS.IoT.Types.NonCompliantResource
import Network.AWS.IoT.Types.OTAUpdateFile
import Network.AWS.IoT.Types.OTAUpdateInfo
import Network.AWS.IoT.Types.OTAUpdateStatus
import Network.AWS.IoT.Types.OTAUpdateSummary
import Network.AWS.IoT.Types.OutgoingCertificate
import Network.AWS.IoT.Types.PercentPair
import Network.AWS.IoT.Types.Policy
import Network.AWS.IoT.Types.PolicyTemplateName
import Network.AWS.IoT.Types.PolicyVersion
import Network.AWS.IoT.Types.PolicyVersionIdentifier
import Network.AWS.IoT.Types.PresignedURLConfig
import Network.AWS.IoT.Types.Protocol
import Network.AWS.IoT.Types.ProvisioningHook
import Network.AWS.IoT.Types.ProvisioningTemplateSummary
import Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
import Network.AWS.IoT.Types.PublishFindingToSNSParams
import Network.AWS.IoT.Types.PutAssetPropertyValueEntry
import Network.AWS.IoT.Types.PutItemInput
import Network.AWS.IoT.Types.RateIncreaseCriteria
import Network.AWS.IoT.Types.RegistrationConfig
import Network.AWS.IoT.Types.RelatedResource
import Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
import Network.AWS.IoT.Types.ReportType
import Network.AWS.IoT.Types.RepublishAction
import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.IoT.Types.ResourceType
import Network.AWS.IoT.Types.RoleAliasDescription
import Network.AWS.IoT.Types.S3Action
import Network.AWS.IoT.Types.S3Destination
import Network.AWS.IoT.Types.S3Location
import Network.AWS.IoT.Types.SNSAction
import Network.AWS.IoT.Types.SalesforceAction
import Network.AWS.IoT.Types.ScheduledAuditMetadata
import Network.AWS.IoT.Types.SecurityProfileIdentifier
import Network.AWS.IoT.Types.SecurityProfileTarget
import Network.AWS.IoT.Types.SecurityProfileTargetMapping
import Network.AWS.IoT.Types.ServerCertificateStatus
import Network.AWS.IoT.Types.ServerCertificateSummary
import Network.AWS.IoT.Types.ServiceType
import Network.AWS.IoT.Types.SigV4Authorization
import Network.AWS.IoT.Types.SigningProfileParameter
import Network.AWS.IoT.Types.SqsAction
import Network.AWS.IoT.Types.StartSigningJobParameter
import Network.AWS.IoT.Types.StatisticalThreshold
import Network.AWS.IoT.Types.Statistics
import Network.AWS.IoT.Types.StepFunctionsAction
import Network.AWS.IoT.Types.Stream
import Network.AWS.IoT.Types.StreamFile
import Network.AWS.IoT.Types.StreamInfo
import Network.AWS.IoT.Types.StreamSummary
import Network.AWS.IoT.Types.TLSContext
import Network.AWS.IoT.Types.Tag
import Network.AWS.IoT.Types.TargetSelection
import Network.AWS.IoT.Types.TaskStatistics
import Network.AWS.IoT.Types.TaskStatisticsForAuditCheck
import Network.AWS.IoT.Types.TaskStatus
import Network.AWS.IoT.Types.ThingAttribute
import Network.AWS.IoT.Types.ThingConnectivity
import Network.AWS.IoT.Types.ThingConnectivityIndexingMode
import Network.AWS.IoT.Types.ThingDocument
import Network.AWS.IoT.Types.ThingGroupDocument
import Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
import Network.AWS.IoT.Types.ThingGroupIndexingMode
import Network.AWS.IoT.Types.ThingGroupMetadata
import Network.AWS.IoT.Types.ThingGroupProperties
import Network.AWS.IoT.Types.ThingIndexingConfiguration
import Network.AWS.IoT.Types.ThingIndexingMode
import Network.AWS.IoT.Types.ThingTypeDefinition
import Network.AWS.IoT.Types.ThingTypeMetadata
import Network.AWS.IoT.Types.ThingTypeProperties
import Network.AWS.IoT.Types.TimeoutConfig
import Network.AWS.IoT.Types.TimestreamAction
import Network.AWS.IoT.Types.TimestreamDimension
import Network.AWS.IoT.Types.TimestreamTimestamp
import Network.AWS.IoT.Types.TopicRule
import Network.AWS.IoT.Types.TopicRuleDestination
import Network.AWS.IoT.Types.TopicRuleDestinationConfiguration
import Network.AWS.IoT.Types.TopicRuleDestinationStatus
import Network.AWS.IoT.Types.TopicRuleDestinationSummary
import Network.AWS.IoT.Types.TopicRuleListItem
import Network.AWS.IoT.Types.TopicRulePayload
import Network.AWS.IoT.Types.TransferData
import Network.AWS.IoT.Types.UpdateCACertificateParams
import Network.AWS.IoT.Types.UpdateDeviceCertificateParams
import Network.AWS.IoT.Types.ValidationError
import Network.AWS.IoT.Types.ViolationEvent
import Network.AWS.IoT.Types.ViolationEventType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-05-28@ of the Amazon IoT SDK configuration.
ioTService :: Lude.Service
ioTService =
  Lude.Service
    { Lude._svcAbbrev = "IoT",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "iot",
      Lude._svcVersion = "2015-05-28",
      Lude._svcEndpoint = Lude.defaultEndpoint ioTService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "IoT",
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
