{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types
  ( -- * Service Configuration
    ioT,

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
    AWSJobAbortConfig,
    awsJobAbortConfig,
    ajacAbortCriteriaList,

    -- * AWSJobAbortCriteria
    AWSJobAbortCriteria,
    awsJobAbortCriteria,
    ajacFailureType,
    ajacAction,
    ajacThresholdPercentage,
    ajacMinNumberOfExecutedThings,

    -- * AWSJobExecutionsRolloutConfig
    AWSJobExecutionsRolloutConfig,
    awsJobExecutionsRolloutConfig,
    ajercExponentialRate,
    ajercMaximumPerMinute,

    -- * AWSJobExponentialRolloutRate
    AWSJobExponentialRolloutRate,
    awsJobExponentialRolloutRate,
    ajerrBaseRatePerMinute,
    ajerrIncrementFactor,
    ajerrRateIncreaseCriteria,

    -- * AWSJobPresignedURLConfig
    AWSJobPresignedURLConfig,
    awsJobPresignedURLConfig,
    ajpucExpiresInSec,

    -- * AWSJobRateIncreaseCriteria
    AWSJobRateIncreaseCriteria,
    awsJobRateIncreaseCriteria,
    ajricNumberOfNotifiedThings,
    ajricNumberOfSucceededThings,

    -- * AWSJobTimeoutConfig
    AWSJobTimeoutConfig,
    awsJobTimeoutConfig,
    ajtcInProgressTimeoutInMinutes,

    -- * AbortConfig
    AbortConfig,
    abortConfig,
    acCriteriaList,

    -- * AbortCriteria
    AbortCriteria,
    abortCriteria,
    acFailureType,
    acAction,
    acThresholdPercentage,
    acMinNumberOfExecutedThings,

    -- * Action
    Action,
    action,
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
    ActiveViolation,
    activeViolation,
    avLastViolationValue,
    avLastViolationTime,
    avViolationStartTime,
    avViolationId,
    avBehavior,
    avSecurityProfileName,
    avThingName,

    -- * AddThingsToThingGroupParams
    AddThingsToThingGroupParams,
    addThingsToThingGroupParams,
    atttgpOverrideDynamicGroups,
    atttgpThingGroupNames,

    -- * AlertTarget
    AlertTarget,
    alertTarget,
    atAlertTargetARN,
    atRoleARN,

    -- * Allowed
    Allowed,
    allowed,
    aPolicies,

    -- * AssetPropertyTimestamp
    AssetPropertyTimestamp,
    assetPropertyTimestamp,
    aptOffsetInNanos,
    aptTimeInSeconds,

    -- * AssetPropertyValue
    AssetPropertyValue,
    assetPropertyValue,
    apvQuality,
    apvValue,
    apvTimestamp,

    -- * AssetPropertyVariant
    AssetPropertyVariant,
    assetPropertyVariant,
    apvIntegerValue,
    apvDoubleValue,
    apvStringValue,
    apvBooleanValue,

    -- * AttributePayload
    AttributePayload,
    attributePayload,
    apAttributes,
    apMerge,

    -- * AuditCheckConfiguration
    AuditCheckConfiguration,
    auditCheckConfiguration,
    accEnabled,

    -- * AuditCheckDetails
    AuditCheckDetails,
    auditCheckDetails,
    acdSuppressedNonCompliantResourcesCount,
    acdTotalResourcesCount,
    acdCheckCompliant,
    acdNonCompliantResourcesCount,
    acdErrorCode,
    acdMessage,
    acdCheckRunStatus,

    -- * AuditFinding
    AuditFinding,
    auditFinding,
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
    AuditMitigationActionExecutionMetadata,
    auditMitigationActionExecutionMetadata,
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
    AuditMitigationActionsTaskMetadata,
    auditMitigationActionsTaskMetadata,
    amatmStartTime,
    amatmTaskId,
    amatmTaskStatus,

    -- * AuditMitigationActionsTaskTarget
    AuditMitigationActionsTaskTarget,
    auditMitigationActionsTaskTarget,
    amattAuditTaskId,
    amattFindingIds,
    amattAuditCheckToReasonCodeFilter,

    -- * AuditNotificationTarget
    AuditNotificationTarget,
    auditNotificationTarget,
    antTargetARN,
    antEnabled,
    antRoleARN,

    -- * AuditSuppression
    AuditSuppression,
    auditSuppression,
    asExpirationDate,
    asSuppressIndefinitely,
    asDescription,
    asCheckName,
    asResourceIdentifier,

    -- * AuditTaskMetadata
    AuditTaskMetadata,
    auditTaskMetadata,
    atmTaskType,
    atmTaskId,
    atmTaskStatus,

    -- * AuthInfo
    AuthInfo,
    authInfo,
    aiActionType,
    aiResources,

    -- * AuthResult
    AuthResult,
    authResult,
    arDenied,
    arAuthDecision,
    arAllowed,
    arMissingContextValues,
    arAuthInfo,

    -- * AuthorizerConfig
    AuthorizerConfig,
    authorizerConfig,
    acAllowAuthorizerOverride,
    acDefaultAuthorizerName,

    -- * AuthorizerDescription
    AuthorizerDescription,
    authorizerDescription,
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
    AuthorizerSummary,
    authorizerSummary,
    asAuthorizerName,
    asAuthorizerARN,

    -- * Behavior
    Behavior,
    behavior,
    bMetricDimension,
    bMetric,
    bCriteria,
    bName,

    -- * BehaviorCriteria
    BehaviorCriteria,
    behaviorCriteria,
    bcValue,
    bcConsecutiveDatapointsToAlarm,
    bcComparisonOperator,
    bcStatisticalThreshold,
    bcDurationSeconds,
    bcConsecutiveDatapointsToClear,

    -- * BillingGroupMetadata
    BillingGroupMetadata,
    billingGroupMetadata,
    bgmCreationDate,

    -- * BillingGroupProperties
    BillingGroupProperties,
    billingGroupProperties,
    bgpBillingGroupDescription,

    -- * CACertificate
    CACertificate,
    cACertificate,
    cacStatus,
    cacCertificateARN,
    cacCertificateId,
    cacCreationDate,

    -- * CACertificateDescription
    CACertificateDescription,
    cACertificateDescription,
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
    Certificate,
    certificate,
    cStatus,
    cCertificateARN,
    cCertificateId,
    cCertificateMode,
    cCreationDate,

    -- * CertificateDescription
    CertificateDescription,
    certificateDescription,
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
    CertificateValidity,
    certificateValidity,
    cvNotBefore,
    cvNotAfter,

    -- * CloudwatchAlarmAction
    CloudwatchAlarmAction,
    cloudwatchAlarmAction,
    caaRoleARN,
    caaAlarmName,
    caaStateReason,
    caaStateValue,

    -- * CloudwatchLogsAction
    CloudwatchLogsAction,
    cloudwatchLogsAction,
    claRoleARN,
    claLogGroupName,

    -- * CloudwatchMetricAction
    CloudwatchMetricAction,
    cloudwatchMetricAction,
    cmaMetricTimestamp,
    cmaRoleARN,
    cmaMetricNamespace,
    cmaMetricName,
    cmaMetricValue,
    cmaMetricUnit,

    -- * CodeSigning
    CodeSigning,
    codeSigning,
    csCustomCodeSigning,
    csStartSigningJobParameter,
    csAwsSignerJobId,

    -- * CodeSigningCertificateChain
    CodeSigningCertificateChain,
    codeSigningCertificateChain,
    csccCertificateName,
    csccInlineDocument,

    -- * CodeSigningSignature
    CodeSigningSignature,
    codeSigningSignature,
    cssInlineDocument,

    -- * Configuration
    Configuration,
    configuration,
    cEnabled,

    -- * CustomCodeSigning
    CustomCodeSigning,
    customCodeSigning,
    ccsSignature,
    ccsHashAlgorithm,
    ccsCertificateChain,
    ccsSignatureAlgorithm,

    -- * Denied
    Denied,
    denied,
    dImplicitDeny,
    dExplicitDeny,

    -- * Destination
    Destination,
    destination,
    dS3Destination,

    -- * DomainConfigurationSummary
    DomainConfigurationSummary,
    domainConfigurationSummary,
    dcsDomainConfigurationName,
    dcsDomainConfigurationARN,
    dcsServiceType,

    -- * DynamoDBAction
    DynamoDBAction,
    dynamoDBAction,
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
    DynamoDBv2Action,
    dynamoDBv2Action,
    ddaRoleARN,
    ddaPutItem,

    -- * EffectivePolicy
    EffectivePolicy,
    effectivePolicy,
    epPolicyName,
    epPolicyDocument,
    epPolicyARN,

    -- * ElasticsearchAction
    ElasticsearchAction,
    elasticsearchAction,
    eaRoleARN,
    eaEndpoint,
    eaIndex,
    eaType,
    eaId,

    -- * EnableIOTLoggingParams
    EnableIOTLoggingParams,
    enableIOTLoggingParams,
    eiotlpRoleARNForLogging,
    eiotlpLogLevel,

    -- * ErrorInfo
    ErrorInfo,
    errorInfo,
    eiCode,
    eiMessage,

    -- * ExplicitDeny
    ExplicitDeny,
    explicitDeny,
    edPolicies,

    -- * ExponentialRolloutRate
    ExponentialRolloutRate,
    exponentialRolloutRate,
    errBaseRatePerMinute,
    errIncrementFactor,
    errRateIncreaseCriteria,

    -- * Field
    Field,
    field,
    fName,
    fType,

    -- * FileLocation
    FileLocation,
    fileLocation,
    flStream,
    flS3Location,

    -- * FirehoseAction
    FirehoseAction,
    firehoseAction,
    faBatchMode,
    faSeparator,
    faRoleARN,
    faDeliveryStreamName,

    -- * GroupNameAndARN
    GroupNameAndARN,
    groupNameAndARN,
    gnaaGroupARN,
    gnaaGroupName,

    -- * HTTPAction
    HTTPAction,
    hTTPAction,
    httpaConfirmationURL,
    httpaAuth,
    httpaHeaders,
    httpaUrl,

    -- * HTTPActionHeader
    HTTPActionHeader,
    hTTPActionHeader,
    httpahKey,
    httpahValue,

    -- * HTTPAuthorization
    HTTPAuthorization,
    hTTPAuthorization,
    httpaSigv4,

    -- * HTTPContext
    HTTPContext,
    hTTPContext,
    httpcHeaders,
    httpcQueryString,

    -- * HTTPURLDestinationConfiguration
    HTTPURLDestinationConfiguration,
    hTTPURLDestinationConfiguration,
    httpudcConfirmationURL,

    -- * HTTPURLDestinationProperties
    HTTPURLDestinationProperties,
    hTTPURLDestinationProperties,
    httpudpConfirmationURL,

    -- * HTTPURLDestinationSummary
    HTTPURLDestinationSummary,
    hTTPURLDestinationSummary,
    httpudsConfirmationURL,

    -- * ImplicitDeny
    ImplicitDeny,
    implicitDeny,
    idPolicies,

    -- * IotAnalyticsAction
    IotAnalyticsAction,
    iotAnalyticsAction,
    iaaBatchMode,
    iaaChannelARN,
    iaaChannelName,
    iaaRoleARN,

    -- * IotEventsAction
    IotEventsAction,
    iotEventsAction,
    ieaBatchMode,
    ieaMessageId,
    ieaInputName,
    ieaRoleARN,

    -- * IotSiteWiseAction
    IotSiteWiseAction,
    iotSiteWiseAction,
    iswaPutAssetPropertyValueEntries,
    iswaRoleARN,

    -- * Job
    Job,
    job,
    jobStatus,
    jobJobExecutionsRolloutConfig,
    jobJobId,
    jobLastUpdatedAt,
    jobJobARN,
    jobCreatedAt,
    jobAbortConfig,
    jobJobProcessDetails,
    jobNamespaceId,
    jobReasonCode,
    jobPresignedURLConfig,
    jobForceCanceled,
    jobTargets,
    jobCompletedAt,
    jobComment,
    jobDescription,
    jobTargetSelection,
    jobTimeoutConfig,

    -- * JobExecution
    JobExecution,
    jobExecution,
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
    JobExecutionStatusDetails,
    jobExecutionStatusDetails,
    jesdDetailsMap,

    -- * JobExecutionSummary
    JobExecutionSummary,
    jobExecutionSummary,
    jesStatus,
    jesLastUpdatedAt,
    jesQueuedAt,
    jesExecutionNumber,
    jesStartedAt,

    -- * JobExecutionSummaryForJob
    JobExecutionSummaryForJob,
    jobExecutionSummaryForJob,
    jesfjJobExecutionSummary,
    jesfjThingARN,

    -- * JobExecutionSummaryForThing
    JobExecutionSummaryForThing,
    jobExecutionSummaryForThing,
    jesftJobId,
    jesftJobExecutionSummary,

    -- * JobExecutionsRolloutConfig
    JobExecutionsRolloutConfig,
    jobExecutionsRolloutConfig,
    jercExponentialRate,
    jercMaximumPerMinute,

    -- * JobProcessDetails
    JobProcessDetails,
    jobProcessDetails,
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
    JobSummary,
    jobSummary,
    jsStatus,
    jsJobId,
    jsLastUpdatedAt,
    jsJobARN,
    jsCreatedAt,
    jsThingGroupId,
    jsCompletedAt,
    jsTargetSelection,

    -- * KeyPair
    KeyPair,
    keyPair,
    kpPrivateKey,
    kpPublicKey,

    -- * KinesisAction
    KinesisAction,
    kinesisAction,
    kaPartitionKey,
    kaRoleARN,
    kaStreamName,

    -- * LambdaAction
    LambdaAction,
    lambdaAction,
    laFunctionARN,

    -- * LogTarget
    LogTarget,
    logTarget,
    ltTargetName,
    ltTargetType,

    -- * LogTargetConfiguration
    LogTargetConfiguration,
    logTargetConfiguration,
    ltcLogLevel,
    ltcLogTarget,

    -- * LoggingOptionsPayload
    LoggingOptionsPayload,
    loggingOptionsPayload,
    lopLogLevel,
    lopRoleARN,

    -- * MetricDimension
    MetricDimension,
    metricDimension,
    mdOperator,
    mdDimensionName,

    -- * MetricToRetain
    MetricToRetain,
    metricToRetain,
    mtrMetricDimension,
    mtrMetric,

    -- * MetricValue
    MetricValue,
    metricValue,
    mvCidrs,
    mvCount,
    mvPorts,

    -- * MitigationAction
    MitigationAction,
    mitigationAction,
    maActionParams,
    maName,
    maId,
    maRoleARN,

    -- * MitigationActionIdentifier
    MitigationActionIdentifier,
    mitigationActionIdentifier,
    maiActionName,
    maiCreationDate,
    maiActionARN,

    -- * MitigationActionParams
    MitigationActionParams,
    mitigationActionParams,
    mapEnableIOTLoggingParams,
    mapAddThingsToThingGroupParams,
    mapUpdateCACertificateParams,
    mapUpdateDeviceCertificateParams,
    mapReplaceDefaultPolicyVersionParams,
    mapPublishFindingToSNSParams,

    -- * MqttContext
    MqttContext,
    mqttContext,
    mcClientId,
    mcUsername,
    mcPassword,

    -- * NonCompliantResource
    NonCompliantResource,
    nonCompliantResource,
    ncrAdditionalInfo,
    ncrResourceType,
    ncrResourceIdentifier,

    -- * OTAUpdateFile
    OTAUpdateFile,
    oTAUpdateFile,
    otaufFileLocation,
    otaufFileType,
    otaufFileVersion,
    otaufAttributes,
    otaufCodeSigning,
    otaufFileName,

    -- * OTAUpdateInfo
    OTAUpdateInfo,
    oTAUpdateInfo,
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
    OTAUpdateSummary,
    oTAUpdateSummary,
    otausCreationDate,
    otausOtaUpdateId,
    otausOtaUpdateARN,

    -- * OutgoingCertificate
    OutgoingCertificate,
    outgoingCertificate,
    ocTransferDate,
    ocCertificateARN,
    ocCertificateId,
    ocTransferredTo,
    ocCreationDate,
    ocTransferMessage,

    -- * PercentPair
    PercentPair,
    percentPair,
    ppValue,
    ppPercent,

    -- * Policy
    Policy,
    policy,
    pPolicyName,
    pPolicyARN,

    -- * PolicyVersion
    PolicyVersion,
    policyVersion,
    pvVersionId,
    pvCreateDate,
    pvIsDefaultVersion,

    -- * PolicyVersionIdentifier
    PolicyVersionIdentifier,
    policyVersionIdentifier,
    pviPolicyName,
    pviPolicyVersionId,

    -- * PresignedURLConfig
    PresignedURLConfig,
    presignedURLConfig,
    pucExpiresInSec,
    pucRoleARN,

    -- * ProvisioningHook
    ProvisioningHook,
    provisioningHook,
    phPayloadVersion,
    phTargetARN,

    -- * ProvisioningTemplateSummary
    ProvisioningTemplateSummary,
    provisioningTemplateSummary,
    ptsLastModifiedDate,
    ptsTemplateName,
    ptsEnabled,
    ptsCreationDate,
    ptsTemplateARN,
    ptsDescription,

    -- * ProvisioningTemplateVersionSummary
    ProvisioningTemplateVersionSummary,
    provisioningTemplateVersionSummary,
    ptvsVersionId,
    ptvsCreationDate,
    ptvsIsDefaultVersion,

    -- * PublishFindingToSNSParams
    PublishFindingToSNSParams,
    publishFindingToSNSParams,
    pftspTopicARN,

    -- * PutAssetPropertyValueEntry
    PutAssetPropertyValueEntry,
    putAssetPropertyValueEntry,
    papveEntryId,
    papvePropertyAlias,
    papvePropertyId,
    papveAssetId,
    papvePropertyValues,

    -- * PutItemInput
    PutItemInput,
    putItemInput,
    piiTableName,

    -- * RateIncreaseCriteria
    RateIncreaseCriteria,
    rateIncreaseCriteria,
    ricNumberOfNotifiedThings,
    ricNumberOfSucceededThings,

    -- * RegistrationConfig
    RegistrationConfig,
    registrationConfig,
    rcTemplateBody,
    rcRoleARN,

    -- * RelatedResource
    RelatedResource,
    relatedResource,
    rrAdditionalInfo,
    rrResourceType,
    rrResourceIdentifier,

    -- * ReplaceDefaultPolicyVersionParams
    ReplaceDefaultPolicyVersionParams,
    replaceDefaultPolicyVersionParams,
    rdpvpTemplateName,

    -- * RepublishAction
    RepublishAction,
    republishAction,
    raQos,
    raRoleARN,
    raTopic,

    -- * ResourceIdentifier
    ResourceIdentifier,
    resourceIdentifier,
    riIamRoleARN,
    riClientId,
    riRoleAliasARN,
    riCaCertificateId,
    riDeviceCertificateId,
    riAccount,
    riPolicyVersionIdentifier,
    riCognitoIdentityPoolId,

    -- * RoleAliasDescription
    RoleAliasDescription,
    roleAliasDescription,
    radRoleAliasARN,
    radLastModifiedDate,
    radRoleAlias,
    radOwner,
    radCreationDate,
    radCredentialDurationSeconds,
    radRoleARN,

    -- * S3Action
    S3Action,
    s3Action,
    sCannedACL,
    sRoleARN,
    sBucketName,
    sKey,

    -- * S3Destination
    S3Destination,
    s3Destination,
    sdPrefix,
    sdBucket,

    -- * S3Location
    S3Location,
    s3Location,
    slBucket,
    slKey,
    slVersion,

    -- * SNSAction
    SNSAction,
    snsAction,
    snsaMessageFormat,
    snsaTargetARN,
    snsaRoleARN,

    -- * SalesforceAction
    SalesforceAction,
    salesforceAction,
    saToken,
    saUrl,

    -- * ScheduledAuditMetadata
    ScheduledAuditMetadata,
    scheduledAuditMetadata,
    samFrequency,
    samScheduledAuditName,
    samDayOfMonth,
    samDayOfWeek,
    samScheduledAuditARN,

    -- * SecurityProfileIdentifier
    SecurityProfileIdentifier,
    securityProfileIdentifier,
    spiName,
    spiArn,

    -- * SecurityProfileTarget
    SecurityProfileTarget,
    securityProfileTarget,
    sptArn,

    -- * SecurityProfileTargetMapping
    SecurityProfileTargetMapping,
    securityProfileTargetMapping,
    sptmSecurityProfileIdentifier,
    sptmTarget,

    -- * ServerCertificateSummary
    ServerCertificateSummary,
    serverCertificateSummary,
    scsServerCertificateStatusDetail,
    scsServerCertificateStatus,
    scsServerCertificateARN,

    -- * SigV4Authorization
    SigV4Authorization,
    sigV4Authorization,
    svaSigningRegion,
    svaServiceName,
    svaRoleARN,

    -- * SigningProfileParameter
    SigningProfileParameter,
    signingProfileParameter,
    sppPlatform,
    sppCertificateARN,
    sppCertificatePathOnDevice,

    -- * SqsAction
    SqsAction,
    sqsAction,
    saUseBase64,
    saRoleARN,
    saQueueURL,

    -- * StartSigningJobParameter
    StartSigningJobParameter,
    startSigningJobParameter,
    ssjpDestination,
    ssjpSigningProfileName,
    ssjpSigningProfileParameter,

    -- * StatisticalThreshold
    StatisticalThreshold,
    statisticalThreshold,
    stStatistic,

    -- * Statistics
    Statistics,
    statistics,
    sStdDeviation,
    sMaximum,
    sAverage,
    sCount,
    sMinimum,
    sVariance,
    sSumOfSquares,
    sSum,

    -- * StepFunctionsAction
    StepFunctionsAction,
    stepFunctionsAction,
    sfaExecutionNamePrefix,
    sfaStateMachineName,
    sfaRoleARN,

    -- * Stream
    Stream,
    stream,
    sFileId,
    sStreamId,

    -- * StreamFile
    StreamFile,
    streamFile,
    sfS3Location,
    sfFileId,

    -- * StreamInfo
    StreamInfo,
    streamInfo,
    siLastUpdatedAt,
    siCreatedAt,
    siStreamVersion,
    siStreamARN,
    siFiles,
    siDescription,
    siStreamId,
    siRoleARN,

    -- * StreamSummary
    StreamSummary,
    streamSummary,
    ssStreamVersion,
    ssStreamARN,
    ssDescription,
    ssStreamId,

    -- * TLSContext
    TLSContext,
    tlsContext,
    tcServerName,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TaskStatistics
    TaskStatistics,
    taskStatistics,
    tsNonCompliantChecks,
    tsWaitingForDataCollectionChecks,
    tsFailedChecks,
    tsTotalChecks,
    tsInProgressChecks,
    tsCompliantChecks,
    tsCanceledChecks,

    -- * TaskStatisticsForAuditCheck
    TaskStatisticsForAuditCheck,
    taskStatisticsForAuditCheck,
    tsfacCanceledFindingsCount,
    tsfacSkippedFindingsCount,
    tsfacTotalFindingsCount,
    tsfacFailedFindingsCount,
    tsfacSucceededFindingsCount,

    -- * ThingAttribute
    ThingAttribute,
    thingAttribute,
    taThingTypeName,
    taThingARN,
    taAttributes,
    taVersion,
    taThingName,

    -- * ThingConnectivity
    ThingConnectivity,
    thingConnectivity,
    tcConnected,
    tcTimestamp,

    -- * ThingDocument
    ThingDocument,
    thingDocument,
    tdThingGroupNames,
    tdThingTypeName,
    tdShadow,
    tdAttributes,
    tdConnectivity,
    tdThingName,
    tdThingId,

    -- * ThingGroupDocument
    ThingGroupDocument,
    thingGroupDocument,
    tgdParentGroupNames,
    tgdThingGroupId,
    tgdThingGroupName,
    tgdAttributes,
    tgdThingGroupDescription,

    -- * ThingGroupIndexingConfiguration
    ThingGroupIndexingConfiguration,
    thingGroupIndexingConfiguration,
    tgicManagedFields,
    tgicCustomFields,
    tgicThingGroupIndexingMode,

    -- * ThingGroupMetadata
    ThingGroupMetadata,
    thingGroupMetadata,
    tgmRootToParentThingGroups,
    tgmParentGroupName,
    tgmCreationDate,

    -- * ThingGroupProperties
    ThingGroupProperties,
    thingGroupProperties,
    tgpAttributePayload,
    tgpThingGroupDescription,

    -- * ThingIndexingConfiguration
    ThingIndexingConfiguration,
    thingIndexingConfiguration,
    ticManagedFields,
    ticThingConnectivityIndexingMode,
    ticCustomFields,
    ticThingIndexingMode,

    -- * ThingTypeDefinition
    ThingTypeDefinition,
    thingTypeDefinition,
    ttdThingTypeProperties,
    ttdThingTypeName,
    ttdThingTypeMetadata,
    ttdThingTypeARN,

    -- * ThingTypeMetadata
    ThingTypeMetadata,
    thingTypeMetadata,
    ttmDeprecationDate,
    ttmCreationDate,
    ttmDeprecated,

    -- * ThingTypeProperties
    ThingTypeProperties,
    thingTypeProperties,
    ttpSearchableAttributes,
    ttpThingTypeDescription,

    -- * TimeoutConfig
    TimeoutConfig,
    timeoutConfig,
    tcInProgressTimeoutInMinutes,

    -- * TimestreamAction
    TimestreamAction,
    timestreamAction,
    taTimestamp,
    taRoleARN,
    taDatabaseName,
    taTableName,
    taDimensions,

    -- * TimestreamDimension
    TimestreamDimension,
    timestreamDimension,
    tdName,
    tdValue,

    -- * TimestreamTimestamp
    TimestreamTimestamp,
    timestreamTimestamp,
    ttValue,
    ttUnit,

    -- * TopicRule
    TopicRule,
    topicRule,
    trCreatedAt,
    trActions,
    trAwsIotSqlVersion,
    trErrorAction,
    trRuleDisabled,
    trRuleName,
    trSql,
    trDescription,

    -- * TopicRuleDestination
    TopicRuleDestination,
    topicRuleDestination,
    trdStatus,
    trdHttpURLProperties,
    trdArn,
    trdStatusReason,

    -- * TopicRuleDestinationConfiguration
    TopicRuleDestinationConfiguration,
    topicRuleDestinationConfiguration,
    trdcHttpURLConfiguration,

    -- * TopicRuleDestinationSummary
    TopicRuleDestinationSummary,
    topicRuleDestinationSummary,
    trdsStatus,
    trdsHttpURLSummary,
    trdsArn,
    trdsStatusReason,

    -- * TopicRuleListItem
    TopicRuleListItem,
    topicRuleListItem,
    trliCreatedAt,
    trliRuleDisabled,
    trliRuleName,
    trliRuleARN,
    trliTopicPattern,

    -- * TopicRulePayload
    TopicRulePayload,
    topicRulePayload,
    trpAwsIotSqlVersion,
    trpErrorAction,
    trpRuleDisabled,
    trpDescription,
    trpSql,
    trpActions,

    -- * TransferData
    TransferData,
    transferData,
    tdTransferDate,
    tdAcceptDate,
    tdTransferMessage,
    tdRejectDate,
    tdRejectReason,

    -- * UpdateCACertificateParams
    UpdateCACertificateParams,
    updateCACertificateParams,
    ucacpAction,

    -- * UpdateDeviceCertificateParams
    UpdateDeviceCertificateParams,
    updateDeviceCertificateParams,
    udcpAction,

    -- * ValidationError
    ValidationError,
    validationError,
    veErrorMessage,

    -- * ViolationEvent
    ViolationEvent,
    violationEvent,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-05-28@ of the Amazon IoT SDK configuration.
ioT :: Service
ioT =
  Service
    { _svcAbbrev = "IoT",
      _svcSigner = v4,
      _svcPrefix = "iot",
      _svcVersion = "2015-05-28",
      _svcEndpoint = defaultEndpoint ioT,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "IoT",
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
