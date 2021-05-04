{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _IndexNotReadyException,
    _TransferAlreadyCompletedException,
    _InvalidQueryException,
    _CertificateConflictException,
    _TaskAlreadyExistsException,
    _CertificateValidationException,
    _UnauthorizedException,
    _ResourceAlreadyExistsException,
    _InternalException,
    _MalformedPolicyException,
    _ServiceUnavailableException,
    _CertificateStateException,
    _InvalidAggregationException,
    _ThrottlingException,
    _InvalidRequestException,
    _ResourceRegistrationFailureException,
    _SqlParseException,
    _LimitExceededException,
    _InvalidStateTransitionException,
    _NotConfiguredException,
    _ResourceNotFoundException,
    _VersionConflictException,
    _RegistrationCodeValidationException,
    _VersionsLimitExceededException,
    _InternalFailureException,
    _DeleteConflictException,
    _InvalidResponseException,
    _TransferConflictException,
    _ConflictingResourceUpdateException,

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

    -- * AwsJobAbortCriteriaAbortAction
    AwsJobAbortCriteriaAbortAction (..),

    -- * AwsJobAbortCriteriaFailureType
    AwsJobAbortCriteriaFailureType (..),

    -- * BehaviorCriteriaType
    BehaviorCriteriaType (..),

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

    -- * ConfidenceLevel
    ConfidenceLevel (..),

    -- * CustomMetricType
    CustomMetricType (..),

    -- * DayOfWeek
    DayOfWeek (..),

    -- * DetectMitigationActionExecutionStatus
    DetectMitigationActionExecutionStatus (..),

    -- * DetectMitigationActionsTaskStatus
    DetectMitigationActionsTaskStatus (..),

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

    -- * ModelStatus
    ModelStatus (..),

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

    -- * AbortConfig
    AbortConfig (..),
    newAbortConfig,
    abortConfig_criteriaList,

    -- * AbortCriteria
    AbortCriteria (..),
    newAbortCriteria,
    abortCriteria_failureType,
    abortCriteria_action,
    abortCriteria_thresholdPercentage,
    abortCriteria_minNumberOfExecutedThings,

    -- * Action
    Action (..),
    newAction,
    action_cloudwatchLogs,
    action_cloudwatchMetric,
    action_sqs,
    action_timestream,
    action_firehose,
    action_sns,
    action_elasticsearch,
    action_kinesis,
    action_salesforce,
    action_dynamoDBv2,
    action_lambda,
    action_iotAnalytics,
    action_iotSiteWise,
    action_republish,
    action_kafka,
    action_dynamoDB,
    action_stepFunctions,
    action_cloudwatchAlarm,
    action_http,
    action_s3,
    action_iotEvents,

    -- * ActiveViolation
    ActiveViolation (..),
    newActiveViolation,
    activeViolation_violationId,
    activeViolation_lastViolationTime,
    activeViolation_thingName,
    activeViolation_lastViolationValue,
    activeViolation_securityProfileName,
    activeViolation_behavior,
    activeViolation_violationStartTime,
    activeViolation_violationEventAdditionalInfo,

    -- * AddThingsToThingGroupParams
    AddThingsToThingGroupParams (..),
    newAddThingsToThingGroupParams,
    addThingsToThingGroupParams_overrideDynamicGroups,
    addThingsToThingGroupParams_thingGroupNames,

    -- * AlertTarget
    AlertTarget (..),
    newAlertTarget,
    alertTarget_alertTargetArn,
    alertTarget_roleArn,

    -- * Allowed
    Allowed (..),
    newAllowed,
    allowed_policies,

    -- * AssetPropertyTimestamp
    AssetPropertyTimestamp (..),
    newAssetPropertyTimestamp,
    assetPropertyTimestamp_offsetInNanos,
    assetPropertyTimestamp_timeInSeconds,

    -- * AssetPropertyValue
    AssetPropertyValue (..),
    newAssetPropertyValue,
    assetPropertyValue_quality,
    assetPropertyValue_value,
    assetPropertyValue_timestamp,

    -- * AssetPropertyVariant
    AssetPropertyVariant (..),
    newAssetPropertyVariant,
    assetPropertyVariant_doubleValue,
    assetPropertyVariant_stringValue,
    assetPropertyVariant_booleanValue,
    assetPropertyVariant_integerValue,

    -- * AttributePayload
    AttributePayload (..),
    newAttributePayload,
    attributePayload_merge,
    attributePayload_attributes,

    -- * AuditCheckConfiguration
    AuditCheckConfiguration (..),
    newAuditCheckConfiguration,
    auditCheckConfiguration_enabled,

    -- * AuditCheckDetails
    AuditCheckDetails (..),
    newAuditCheckDetails,
    auditCheckDetails_checkCompliant,
    auditCheckDetails_message,
    auditCheckDetails_suppressedNonCompliantResourcesCount,
    auditCheckDetails_checkRunStatus,
    auditCheckDetails_totalResourcesCount,
    auditCheckDetails_errorCode,
    auditCheckDetails_nonCompliantResourcesCount,

    -- * AuditFinding
    AuditFinding (..),
    newAuditFinding,
    auditFinding_severity,
    auditFinding_findingId,
    auditFinding_taskId,
    auditFinding_reasonForNonComplianceCode,
    auditFinding_reasonForNonCompliance,
    auditFinding_isSuppressed,
    auditFinding_checkName,
    auditFinding_relatedResources,
    auditFinding_findingTime,
    auditFinding_taskStartTime,
    auditFinding_nonCompliantResource,

    -- * AuditMitigationActionExecutionMetadata
    AuditMitigationActionExecutionMetadata (..),
    newAuditMitigationActionExecutionMetadata,
    auditMitigationActionExecutionMetadata_status,
    auditMitigationActionExecutionMetadata_actionName,
    auditMitigationActionExecutionMetadata_message,
    auditMitigationActionExecutionMetadata_actionId,
    auditMitigationActionExecutionMetadata_findingId,
    auditMitigationActionExecutionMetadata_taskId,
    auditMitigationActionExecutionMetadata_startTime,
    auditMitigationActionExecutionMetadata_endTime,
    auditMitigationActionExecutionMetadata_errorCode,

    -- * AuditMitigationActionsTaskMetadata
    AuditMitigationActionsTaskMetadata (..),
    newAuditMitigationActionsTaskMetadata,
    auditMitigationActionsTaskMetadata_taskId,
    auditMitigationActionsTaskMetadata_startTime,
    auditMitigationActionsTaskMetadata_taskStatus,

    -- * AuditMitigationActionsTaskTarget
    AuditMitigationActionsTaskTarget (..),
    newAuditMitigationActionsTaskTarget,
    auditMitigationActionsTaskTarget_findingIds,
    auditMitigationActionsTaskTarget_auditTaskId,
    auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter,

    -- * AuditNotificationTarget
    AuditNotificationTarget (..),
    newAuditNotificationTarget,
    auditNotificationTarget_roleArn,
    auditNotificationTarget_enabled,
    auditNotificationTarget_targetArn,

    -- * AuditSuppression
    AuditSuppression (..),
    newAuditSuppression,
    auditSuppression_expirationDate,
    auditSuppression_description,
    auditSuppression_suppressIndefinitely,
    auditSuppression_checkName,
    auditSuppression_resourceIdentifier,

    -- * AuditTaskMetadata
    AuditTaskMetadata (..),
    newAuditTaskMetadata,
    auditTaskMetadata_taskId,
    auditTaskMetadata_taskStatus,
    auditTaskMetadata_taskType,

    -- * AuthInfo
    AuthInfo (..),
    newAuthInfo,
    authInfo_actionType,
    authInfo_resources,

    -- * AuthResult
    AuthResult (..),
    newAuthResult,
    authResult_authInfo,
    authResult_allowed,
    authResult_denied,
    authResult_missingContextValues,
    authResult_authDecision,

    -- * AuthorizerConfig
    AuthorizerConfig (..),
    newAuthorizerConfig,
    authorizerConfig_allowAuthorizerOverride,
    authorizerConfig_defaultAuthorizerName,

    -- * AuthorizerDescription
    AuthorizerDescription (..),
    newAuthorizerDescription,
    authorizerDescription_lastModifiedDate,
    authorizerDescription_status,
    authorizerDescription_authorizerArn,
    authorizerDescription_authorizerFunctionArn,
    authorizerDescription_creationDate,
    authorizerDescription_tokenSigningPublicKeys,
    authorizerDescription_authorizerName,
    authorizerDescription_signingDisabled,
    authorizerDescription_tokenKeyName,

    -- * AuthorizerSummary
    AuthorizerSummary (..),
    newAuthorizerSummary,
    authorizerSummary_authorizerArn,
    authorizerSummary_authorizerName,

    -- * AwsJobAbortConfig
    AwsJobAbortConfig (..),
    newAwsJobAbortConfig,
    awsJobAbortConfig_abortCriteriaList,

    -- * AwsJobAbortCriteria
    AwsJobAbortCriteria (..),
    newAwsJobAbortCriteria,
    awsJobAbortCriteria_failureType,
    awsJobAbortCriteria_action,
    awsJobAbortCriteria_thresholdPercentage,
    awsJobAbortCriteria_minNumberOfExecutedThings,

    -- * AwsJobExecutionsRolloutConfig
    AwsJobExecutionsRolloutConfig (..),
    newAwsJobExecutionsRolloutConfig,
    awsJobExecutionsRolloutConfig_exponentialRate,
    awsJobExecutionsRolloutConfig_maximumPerMinute,

    -- * AwsJobExponentialRolloutRate
    AwsJobExponentialRolloutRate (..),
    newAwsJobExponentialRolloutRate,
    awsJobExponentialRolloutRate_baseRatePerMinute,
    awsJobExponentialRolloutRate_incrementFactor,
    awsJobExponentialRolloutRate_rateIncreaseCriteria,

    -- * AwsJobPresignedUrlConfig
    AwsJobPresignedUrlConfig (..),
    newAwsJobPresignedUrlConfig,
    awsJobPresignedUrlConfig_expiresInSec,

    -- * AwsJobRateIncreaseCriteria
    AwsJobRateIncreaseCriteria (..),
    newAwsJobRateIncreaseCriteria,
    awsJobRateIncreaseCriteria_numberOfNotifiedThings,
    awsJobRateIncreaseCriteria_numberOfSucceededThings,

    -- * AwsJobTimeoutConfig
    AwsJobTimeoutConfig (..),
    newAwsJobTimeoutConfig,
    awsJobTimeoutConfig_inProgressTimeoutInMinutes,

    -- * Behavior
    Behavior (..),
    newBehavior,
    behavior_metricDimension,
    behavior_suppressAlerts,
    behavior_metric,
    behavior_criteria,
    behavior_name,

    -- * BehaviorCriteria
    BehaviorCriteria (..),
    newBehaviorCriteria,
    behaviorCriteria_comparisonOperator,
    behaviorCriteria_consecutiveDatapointsToAlarm,
    behaviorCriteria_statisticalThreshold,
    behaviorCriteria_mlDetectionConfig,
    behaviorCriteria_consecutiveDatapointsToClear,
    behaviorCriteria_value,
    behaviorCriteria_durationSeconds,

    -- * BehaviorModelTrainingSummary
    BehaviorModelTrainingSummary (..),
    newBehaviorModelTrainingSummary,
    behaviorModelTrainingSummary_lastModelRefreshDate,
    behaviorModelTrainingSummary_datapointsCollectionPercentage,
    behaviorModelTrainingSummary_modelStatus,
    behaviorModelTrainingSummary_behaviorName,
    behaviorModelTrainingSummary_trainingDataCollectionStartDate,
    behaviorModelTrainingSummary_securityProfileName,

    -- * BillingGroupMetadata
    BillingGroupMetadata (..),
    newBillingGroupMetadata,
    billingGroupMetadata_creationDate,

    -- * BillingGroupProperties
    BillingGroupProperties (..),
    newBillingGroupProperties,
    billingGroupProperties_billingGroupDescription,

    -- * CACertificate
    CACertificate (..),
    newCACertificate,
    cACertificate_status,
    cACertificate_certificateArn,
    cACertificate_creationDate,
    cACertificate_certificateId,

    -- * CACertificateDescription
    CACertificateDescription (..),
    newCACertificateDescription,
    cACertificateDescription_lastModifiedDate,
    cACertificateDescription_status,
    cACertificateDescription_certificateArn,
    cACertificateDescription_creationDate,
    cACertificateDescription_ownedBy,
    cACertificateDescription_customerVersion,
    cACertificateDescription_generationId,
    cACertificateDescription_certificateId,
    cACertificateDescription_certificatePem,
    cACertificateDescription_validity,
    cACertificateDescription_autoRegistrationStatus,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_status,
    certificate_certificateMode,
    certificate_certificateArn,
    certificate_creationDate,
    certificate_certificateId,

    -- * CertificateDescription
    CertificateDescription (..),
    newCertificateDescription,
    certificateDescription_lastModifiedDate,
    certificateDescription_status,
    certificateDescription_certificateMode,
    certificateDescription_certificateArn,
    certificateDescription_previousOwnedBy,
    certificateDescription_creationDate,
    certificateDescription_ownedBy,
    certificateDescription_customerVersion,
    certificateDescription_generationId,
    certificateDescription_transferData,
    certificateDescription_certificateId,
    certificateDescription_certificatePem,
    certificateDescription_validity,
    certificateDescription_caCertificateId,

    -- * CertificateValidity
    CertificateValidity (..),
    newCertificateValidity,
    certificateValidity_notBefore,
    certificateValidity_notAfter,

    -- * CloudwatchAlarmAction
    CloudwatchAlarmAction (..),
    newCloudwatchAlarmAction,
    cloudwatchAlarmAction_roleArn,
    cloudwatchAlarmAction_alarmName,
    cloudwatchAlarmAction_stateReason,
    cloudwatchAlarmAction_stateValue,

    -- * CloudwatchLogsAction
    CloudwatchLogsAction (..),
    newCloudwatchLogsAction,
    cloudwatchLogsAction_roleArn,
    cloudwatchLogsAction_logGroupName,

    -- * CloudwatchMetricAction
    CloudwatchMetricAction (..),
    newCloudwatchMetricAction,
    cloudwatchMetricAction_metricTimestamp,
    cloudwatchMetricAction_roleArn,
    cloudwatchMetricAction_metricNamespace,
    cloudwatchMetricAction_metricName,
    cloudwatchMetricAction_metricValue,
    cloudwatchMetricAction_metricUnit,

    -- * CodeSigning
    CodeSigning (..),
    newCodeSigning,
    codeSigning_startSigningJobParameter,
    codeSigning_awsSignerJobId,
    codeSigning_customCodeSigning,

    -- * CodeSigningCertificateChain
    CodeSigningCertificateChain (..),
    newCodeSigningCertificateChain,
    codeSigningCertificateChain_inlineDocument,
    codeSigningCertificateChain_certificateName,

    -- * CodeSigningSignature
    CodeSigningSignature (..),
    newCodeSigningSignature,
    codeSigningSignature_inlineDocument,

    -- * Configuration
    Configuration (..),
    newConfiguration,
    configuration_enabled,

    -- * CustomCodeSigning
    CustomCodeSigning (..),
    newCustomCodeSigning,
    customCodeSigning_signature,
    customCodeSigning_signatureAlgorithm,
    customCodeSigning_certificateChain,
    customCodeSigning_hashAlgorithm,

    -- * Denied
    Denied (..),
    newDenied,
    denied_implicitDeny,
    denied_explicitDeny,

    -- * Destination
    Destination (..),
    newDestination,
    destination_s3Destination,

    -- * DetectMitigationActionExecution
    DetectMitigationActionExecution (..),
    newDetectMitigationActionExecution,
    detectMitigationActionExecution_violationId,
    detectMitigationActionExecution_status,
    detectMitigationActionExecution_actionName,
    detectMitigationActionExecution_executionStartDate,
    detectMitigationActionExecution_message,
    detectMitigationActionExecution_thingName,
    detectMitigationActionExecution_taskId,
    detectMitigationActionExecution_executionEndDate,
    detectMitigationActionExecution_errorCode,

    -- * DetectMitigationActionsTaskStatistics
    DetectMitigationActionsTaskStatistics (..),
    newDetectMitigationActionsTaskStatistics,
    detectMitigationActionsTaskStatistics_actionsFailed,
    detectMitigationActionsTaskStatistics_actionsSkipped,
    detectMitigationActionsTaskStatistics_actionsExecuted,

    -- * DetectMitigationActionsTaskSummary
    DetectMitigationActionsTaskSummary (..),
    newDetectMitigationActionsTaskSummary,
    detectMitigationActionsTaskSummary_taskEndTime,
    detectMitigationActionsTaskSummary_taskStatistics,
    detectMitigationActionsTaskSummary_taskId,
    detectMitigationActionsTaskSummary_violationEventOccurrenceRange,
    detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded,
    detectMitigationActionsTaskSummary_target,
    detectMitigationActionsTaskSummary_taskStatus,
    detectMitigationActionsTaskSummary_actionsDefinition,
    detectMitigationActionsTaskSummary_taskStartTime,
    detectMitigationActionsTaskSummary_suppressedAlertsIncluded,

    -- * DetectMitigationActionsTaskTarget
    DetectMitigationActionsTaskTarget (..),
    newDetectMitigationActionsTaskTarget,
    detectMitigationActionsTaskTarget_violationIds,
    detectMitigationActionsTaskTarget_behaviorName,
    detectMitigationActionsTaskTarget_securityProfileName,

    -- * DomainConfigurationSummary
    DomainConfigurationSummary (..),
    newDomainConfigurationSummary,
    domainConfigurationSummary_domainConfigurationArn,
    domainConfigurationSummary_domainConfigurationName,
    domainConfigurationSummary_serviceType,

    -- * DynamoDBAction
    DynamoDBAction (..),
    newDynamoDBAction,
    dynamoDBAction_rangeKeyValue,
    dynamoDBAction_rangeKeyType,
    dynamoDBAction_operation,
    dynamoDBAction_hashKeyType,
    dynamoDBAction_rangeKeyField,
    dynamoDBAction_payloadField,
    dynamoDBAction_tableName,
    dynamoDBAction_roleArn,
    dynamoDBAction_hashKeyField,
    dynamoDBAction_hashKeyValue,

    -- * DynamoDBv2Action
    DynamoDBv2Action (..),
    newDynamoDBv2Action,
    dynamoDBv2Action_roleArn,
    dynamoDBv2Action_putItem,

    -- * EffectivePolicy
    EffectivePolicy (..),
    newEffectivePolicy,
    effectivePolicy_policyName,
    effectivePolicy_policyDocument,
    effectivePolicy_policyArn,

    -- * ElasticsearchAction
    ElasticsearchAction (..),
    newElasticsearchAction,
    elasticsearchAction_roleArn,
    elasticsearchAction_endpoint,
    elasticsearchAction_index,
    elasticsearchAction_type,
    elasticsearchAction_id,

    -- * EnableIoTLoggingParams
    EnableIoTLoggingParams (..),
    newEnableIoTLoggingParams,
    enableIoTLoggingParams_roleArnForLogging,
    enableIoTLoggingParams_logLevel,

    -- * ErrorInfo
    ErrorInfo (..),
    newErrorInfo,
    errorInfo_message,
    errorInfo_code,

    -- * ExplicitDeny
    ExplicitDeny (..),
    newExplicitDeny,
    explicitDeny_policies,

    -- * ExponentialRolloutRate
    ExponentialRolloutRate (..),
    newExponentialRolloutRate,
    exponentialRolloutRate_baseRatePerMinute,
    exponentialRolloutRate_incrementFactor,
    exponentialRolloutRate_rateIncreaseCriteria,

    -- * Field
    Field (..),
    newField,
    field_name,
    field_type,

    -- * FileLocation
    FileLocation (..),
    newFileLocation,
    fileLocation_stream,
    fileLocation_s3Location,

    -- * FirehoseAction
    FirehoseAction (..),
    newFirehoseAction,
    firehoseAction_separator,
    firehoseAction_batchMode,
    firehoseAction_roleArn,
    firehoseAction_deliveryStreamName,

    -- * GroupNameAndArn
    GroupNameAndArn (..),
    newGroupNameAndArn,
    groupNameAndArn_groupName,
    groupNameAndArn_groupArn,

    -- * HttpAction
    HttpAction (..),
    newHttpAction,
    httpAction_headers,
    httpAction_auth,
    httpAction_confirmationUrl,
    httpAction_url,

    -- * HttpActionHeader
    HttpActionHeader (..),
    newHttpActionHeader,
    httpActionHeader_key,
    httpActionHeader_value,

    -- * HttpAuthorization
    HttpAuthorization (..),
    newHttpAuthorization,
    httpAuthorization_sigv4,

    -- * HttpContext
    HttpContext (..),
    newHttpContext,
    httpContext_queryString,
    httpContext_headers,

    -- * HttpUrlDestinationConfiguration
    HttpUrlDestinationConfiguration (..),
    newHttpUrlDestinationConfiguration,
    httpUrlDestinationConfiguration_confirmationUrl,

    -- * HttpUrlDestinationProperties
    HttpUrlDestinationProperties (..),
    newHttpUrlDestinationProperties,
    httpUrlDestinationProperties_confirmationUrl,

    -- * HttpUrlDestinationSummary
    HttpUrlDestinationSummary (..),
    newHttpUrlDestinationSummary,
    httpUrlDestinationSummary_confirmationUrl,

    -- * ImplicitDeny
    ImplicitDeny (..),
    newImplicitDeny,
    implicitDeny_policies,

    -- * IotAnalyticsAction
    IotAnalyticsAction (..),
    newIotAnalyticsAction,
    iotAnalyticsAction_channelName,
    iotAnalyticsAction_roleArn,
    iotAnalyticsAction_batchMode,
    iotAnalyticsAction_channelArn,

    -- * IotEventsAction
    IotEventsAction (..),
    newIotEventsAction,
    iotEventsAction_batchMode,
    iotEventsAction_messageId,
    iotEventsAction_inputName,
    iotEventsAction_roleArn,

    -- * IotSiteWiseAction
    IotSiteWiseAction (..),
    newIotSiteWiseAction,
    iotSiteWiseAction_putAssetPropertyValueEntries,
    iotSiteWiseAction_roleArn,

    -- * Job
    Job (..),
    newJob,
    job_jobExecutionsRolloutConfig,
    job_status,
    job_targetSelection,
    job_timeoutConfig,
    job_reasonCode,
    job_namespaceId,
    job_jobProcessDetails,
    job_comment,
    job_completedAt,
    job_createdAt,
    job_jobArn,
    job_targets,
    job_forceCanceled,
    job_presignedUrlConfig,
    job_description,
    job_abortConfig,
    job_lastUpdatedAt,
    job_jobId,

    -- * JobExecution
    JobExecution (..),
    newJobExecution,
    jobExecution_startedAt,
    jobExecution_status,
    jobExecution_thingArn,
    jobExecution_statusDetails,
    jobExecution_queuedAt,
    jobExecution_forceCanceled,
    jobExecution_versionNumber,
    jobExecution_executionNumber,
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_lastUpdatedAt,
    jobExecution_jobId,

    -- * JobExecutionStatusDetails
    JobExecutionStatusDetails (..),
    newJobExecutionStatusDetails,
    jobExecutionStatusDetails_detailsMap,

    -- * JobExecutionSummary
    JobExecutionSummary (..),
    newJobExecutionSummary,
    jobExecutionSummary_startedAt,
    jobExecutionSummary_status,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_lastUpdatedAt,

    -- * JobExecutionSummaryForJob
    JobExecutionSummaryForJob (..),
    newJobExecutionSummaryForJob,
    jobExecutionSummaryForJob_thingArn,
    jobExecutionSummaryForJob_jobExecutionSummary,

    -- * JobExecutionSummaryForThing
    JobExecutionSummaryForThing (..),
    newJobExecutionSummaryForThing,
    jobExecutionSummaryForThing_jobExecutionSummary,
    jobExecutionSummaryForThing_jobId,

    -- * JobExecutionsRolloutConfig
    JobExecutionsRolloutConfig (..),
    newJobExecutionsRolloutConfig,
    jobExecutionsRolloutConfig_exponentialRate,
    jobExecutionsRolloutConfig_maximumPerMinute,

    -- * JobProcessDetails
    JobProcessDetails (..),
    newJobProcessDetails,
    jobProcessDetails_processingTargets,
    jobProcessDetails_numberOfSucceededThings,
    jobProcessDetails_numberOfQueuedThings,
    jobProcessDetails_numberOfRemovedThings,
    jobProcessDetails_numberOfInProgressThings,
    jobProcessDetails_numberOfFailedThings,
    jobProcessDetails_numberOfRejectedThings,
    jobProcessDetails_numberOfCanceledThings,
    jobProcessDetails_numberOfTimedOutThings,

    -- * JobSummary
    JobSummary (..),
    newJobSummary,
    jobSummary_status,
    jobSummary_targetSelection,
    jobSummary_completedAt,
    jobSummary_createdAt,
    jobSummary_jobArn,
    jobSummary_thingGroupId,
    jobSummary_lastUpdatedAt,
    jobSummary_jobId,

    -- * KafkaAction
    KafkaAction (..),
    newKafkaAction,
    kafkaAction_key,
    kafkaAction_partition,
    kafkaAction_destinationArn,
    kafkaAction_topic,
    kafkaAction_clientProperties,

    -- * KeyPair
    KeyPair (..),
    newKeyPair,
    keyPair_publicKey,
    keyPair_privateKey,

    -- * KinesisAction
    KinesisAction (..),
    newKinesisAction,
    kinesisAction_partitionKey,
    kinesisAction_roleArn,
    kinesisAction_streamName,

    -- * LambdaAction
    LambdaAction (..),
    newLambdaAction,
    lambdaAction_functionArn,

    -- * LogTarget
    LogTarget (..),
    newLogTarget,
    logTarget_targetName,
    logTarget_targetType,

    -- * LogTargetConfiguration
    LogTargetConfiguration (..),
    newLogTargetConfiguration,
    logTargetConfiguration_logLevel,
    logTargetConfiguration_logTarget,

    -- * LoggingOptionsPayload
    LoggingOptionsPayload (..),
    newLoggingOptionsPayload,
    loggingOptionsPayload_logLevel,
    loggingOptionsPayload_roleArn,

    -- * MachineLearningDetectionConfig
    MachineLearningDetectionConfig (..),
    newMachineLearningDetectionConfig,
    machineLearningDetectionConfig_confidenceLevel,

    -- * MetricDimension
    MetricDimension (..),
    newMetricDimension,
    metricDimension_operator,
    metricDimension_dimensionName,

    -- * MetricToRetain
    MetricToRetain (..),
    newMetricToRetain,
    metricToRetain_metricDimension,
    metricToRetain_metric,

    -- * MetricValue
    MetricValue (..),
    newMetricValue,
    metricValue_numbers,
    metricValue_ports,
    metricValue_cidrs,
    metricValue_strings,
    metricValue_count,
    metricValue_number,

    -- * MitigationAction
    MitigationAction (..),
    newMitigationAction,
    mitigationAction_roleArn,
    mitigationAction_id,
    mitigationAction_actionParams,
    mitigationAction_name,

    -- * MitigationActionIdentifier
    MitigationActionIdentifier (..),
    newMitigationActionIdentifier,
    mitigationActionIdentifier_actionName,
    mitigationActionIdentifier_actionArn,
    mitigationActionIdentifier_creationDate,

    -- * MitigationActionParams
    MitigationActionParams (..),
    newMitigationActionParams,
    mitigationActionParams_enableIoTLoggingParams,
    mitigationActionParams_replaceDefaultPolicyVersionParams,
    mitigationActionParams_updateDeviceCertificateParams,
    mitigationActionParams_publishFindingToSnsParams,
    mitigationActionParams_addThingsToThingGroupParams,
    mitigationActionParams_updateCACertificateParams,

    -- * MqttContext
    MqttContext (..),
    newMqttContext,
    mqttContext_clientId,
    mqttContext_password,
    mqttContext_username,

    -- * NonCompliantResource
    NonCompliantResource (..),
    newNonCompliantResource,
    nonCompliantResource_additionalInfo,
    nonCompliantResource_resourceType,
    nonCompliantResource_resourceIdentifier,

    -- * OTAUpdateFile
    OTAUpdateFile (..),
    newOTAUpdateFile,
    oTAUpdateFile_fileVersion,
    oTAUpdateFile_fileLocation,
    oTAUpdateFile_attributes,
    oTAUpdateFile_fileName,
    oTAUpdateFile_fileType,
    oTAUpdateFile_codeSigning,

    -- * OTAUpdateInfo
    OTAUpdateInfo (..),
    newOTAUpdateInfo,
    oTAUpdateInfo_otaUpdateStatus,
    oTAUpdateInfo_lastModifiedDate,
    oTAUpdateInfo_targetSelection,
    oTAUpdateInfo_otaUpdateArn,
    oTAUpdateInfo_awsIotJobId,
    oTAUpdateInfo_creationDate,
    oTAUpdateInfo_awsIotJobArn,
    oTAUpdateInfo_protocols,
    oTAUpdateInfo_targets,
    oTAUpdateInfo_awsJobPresignedUrlConfig,
    oTAUpdateInfo_errorInfo,
    oTAUpdateInfo_description,
    oTAUpdateInfo_otaUpdateFiles,
    oTAUpdateInfo_otaUpdateId,
    oTAUpdateInfo_additionalParameters,
    oTAUpdateInfo_awsJobExecutionsRolloutConfig,

    -- * OTAUpdateSummary
    OTAUpdateSummary (..),
    newOTAUpdateSummary,
    oTAUpdateSummary_otaUpdateArn,
    oTAUpdateSummary_creationDate,
    oTAUpdateSummary_otaUpdateId,

    -- * OutgoingCertificate
    OutgoingCertificate (..),
    newOutgoingCertificate,
    outgoingCertificate_transferDate,
    outgoingCertificate_certificateArn,
    outgoingCertificate_transferMessage,
    outgoingCertificate_creationDate,
    outgoingCertificate_transferredTo,
    outgoingCertificate_certificateId,

    -- * PercentPair
    PercentPair (..),
    newPercentPair,
    percentPair_percent,
    percentPair_value,

    -- * Policy
    Policy (..),
    newPolicy,
    policy_policyName,
    policy_policyArn,

    -- * PolicyVersion
    PolicyVersion (..),
    newPolicyVersion,
    policyVersion_createDate,
    policyVersion_versionId,
    policyVersion_isDefaultVersion,

    -- * PolicyVersionIdentifier
    PolicyVersionIdentifier (..),
    newPolicyVersionIdentifier,
    policyVersionIdentifier_policyVersionId,
    policyVersionIdentifier_policyName,

    -- * PresignedUrlConfig
    PresignedUrlConfig (..),
    newPresignedUrlConfig,
    presignedUrlConfig_roleArn,
    presignedUrlConfig_expiresInSec,

    -- * ProvisioningHook
    ProvisioningHook (..),
    newProvisioningHook,
    provisioningHook_payloadVersion,
    provisioningHook_targetArn,

    -- * ProvisioningTemplateSummary
    ProvisioningTemplateSummary (..),
    newProvisioningTemplateSummary,
    provisioningTemplateSummary_templateName,
    provisioningTemplateSummary_lastModifiedDate,
    provisioningTemplateSummary_enabled,
    provisioningTemplateSummary_creationDate,
    provisioningTemplateSummary_description,
    provisioningTemplateSummary_templateArn,

    -- * ProvisioningTemplateVersionSummary
    ProvisioningTemplateVersionSummary (..),
    newProvisioningTemplateVersionSummary,
    provisioningTemplateVersionSummary_creationDate,
    provisioningTemplateVersionSummary_versionId,
    provisioningTemplateVersionSummary_isDefaultVersion,

    -- * PublishFindingToSnsParams
    PublishFindingToSnsParams (..),
    newPublishFindingToSnsParams,
    publishFindingToSnsParams_topicArn,

    -- * PutAssetPropertyValueEntry
    PutAssetPropertyValueEntry (..),
    newPutAssetPropertyValueEntry,
    putAssetPropertyValueEntry_entryId,
    putAssetPropertyValueEntry_propertyAlias,
    putAssetPropertyValueEntry_assetId,
    putAssetPropertyValueEntry_propertyId,
    putAssetPropertyValueEntry_propertyValues,

    -- * PutItemInput
    PutItemInput (..),
    newPutItemInput,
    putItemInput_tableName,

    -- * RateIncreaseCriteria
    RateIncreaseCriteria (..),
    newRateIncreaseCriteria,
    rateIncreaseCriteria_numberOfNotifiedThings,
    rateIncreaseCriteria_numberOfSucceededThings,

    -- * RegistrationConfig
    RegistrationConfig (..),
    newRegistrationConfig,
    registrationConfig_roleArn,
    registrationConfig_templateBody,

    -- * RelatedResource
    RelatedResource (..),
    newRelatedResource,
    relatedResource_additionalInfo,
    relatedResource_resourceType,
    relatedResource_resourceIdentifier,

    -- * ReplaceDefaultPolicyVersionParams
    ReplaceDefaultPolicyVersionParams (..),
    newReplaceDefaultPolicyVersionParams,
    replaceDefaultPolicyVersionParams_templateName,

    -- * RepublishAction
    RepublishAction (..),
    newRepublishAction,
    republishAction_qos,
    republishAction_roleArn,
    republishAction_topic,

    -- * ResourceIdentifier
    ResourceIdentifier (..),
    newResourceIdentifier,
    resourceIdentifier_roleAliasArn,
    resourceIdentifier_clientId,
    resourceIdentifier_iamRoleArn,
    resourceIdentifier_cognitoIdentityPoolId,
    resourceIdentifier_account,
    resourceIdentifier_policyVersionIdentifier,
    resourceIdentifier_deviceCertificateId,
    resourceIdentifier_caCertificateId,

    -- * RoleAliasDescription
    RoleAliasDescription (..),
    newRoleAliasDescription,
    roleAliasDescription_roleAliasArn,
    roleAliasDescription_lastModifiedDate,
    roleAliasDescription_roleArn,
    roleAliasDescription_creationDate,
    roleAliasDescription_owner,
    roleAliasDescription_credentialDurationSeconds,
    roleAliasDescription_roleAlias,

    -- * S3Action
    S3Action (..),
    newS3Action,
    s3Action_cannedAcl,
    s3Action_roleArn,
    s3Action_bucketName,
    s3Action_key,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_prefix,
    s3Destination_bucket,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_key,
    s3Location_version,
    s3Location_bucket,

    -- * SalesforceAction
    SalesforceAction (..),
    newSalesforceAction,
    salesforceAction_token,
    salesforceAction_url,

    -- * ScheduledAuditMetadata
    ScheduledAuditMetadata (..),
    newScheduledAuditMetadata,
    scheduledAuditMetadata_dayOfWeek,
    scheduledAuditMetadata_scheduledAuditArn,
    scheduledAuditMetadata_scheduledAuditName,
    scheduledAuditMetadata_dayOfMonth,
    scheduledAuditMetadata_frequency,

    -- * SecurityProfileIdentifier
    SecurityProfileIdentifier (..),
    newSecurityProfileIdentifier,
    securityProfileIdentifier_name,
    securityProfileIdentifier_arn,

    -- * SecurityProfileTarget
    SecurityProfileTarget (..),
    newSecurityProfileTarget,
    securityProfileTarget_arn,

    -- * SecurityProfileTargetMapping
    SecurityProfileTargetMapping (..),
    newSecurityProfileTargetMapping,
    securityProfileTargetMapping_target,
    securityProfileTargetMapping_securityProfileIdentifier,

    -- * ServerCertificateSummary
    ServerCertificateSummary (..),
    newServerCertificateSummary,
    serverCertificateSummary_serverCertificateStatus,
    serverCertificateSummary_serverCertificateArn,
    serverCertificateSummary_serverCertificateStatusDetail,

    -- * SigV4Authorization
    SigV4Authorization (..),
    newSigV4Authorization,
    sigV4Authorization_signingRegion,
    sigV4Authorization_serviceName,
    sigV4Authorization_roleArn,

    -- * SigningProfileParameter
    SigningProfileParameter (..),
    newSigningProfileParameter,
    signingProfileParameter_platform,
    signingProfileParameter_certificateArn,
    signingProfileParameter_certificatePathOnDevice,

    -- * SnsAction
    SnsAction (..),
    newSnsAction,
    snsAction_messageFormat,
    snsAction_targetArn,
    snsAction_roleArn,

    -- * SqsAction
    SqsAction (..),
    newSqsAction,
    sqsAction_useBase64,
    sqsAction_roleArn,
    sqsAction_queueUrl,

    -- * StartSigningJobParameter
    StartSigningJobParameter (..),
    newStartSigningJobParameter,
    startSigningJobParameter_signingProfileName,
    startSigningJobParameter_destination,
    startSigningJobParameter_signingProfileParameter,

    -- * StatisticalThreshold
    StatisticalThreshold (..),
    newStatisticalThreshold,
    statisticalThreshold_statistic,

    -- * Statistics
    Statistics (..),
    newStatistics,
    statistics_minimum,
    statistics_sum,
    statistics_stdDeviation,
    statistics_variance,
    statistics_average,
    statistics_count,
    statistics_maximum,
    statistics_sumOfSquares,

    -- * StepFunctionsAction
    StepFunctionsAction (..),
    newStepFunctionsAction,
    stepFunctionsAction_executionNamePrefix,
    stepFunctionsAction_stateMachineName,
    stepFunctionsAction_roleArn,

    -- * Stream
    Stream (..),
    newStream,
    stream_streamId,
    stream_fileId,

    -- * StreamFile
    StreamFile (..),
    newStreamFile,
    streamFile_s3Location,
    streamFile_fileId,

    -- * StreamInfo
    StreamInfo (..),
    newStreamInfo,
    streamInfo_roleArn,
    streamInfo_streamVersion,
    streamInfo_createdAt,
    streamInfo_streamId,
    streamInfo_streamArn,
    streamInfo_description,
    streamInfo_files,
    streamInfo_lastUpdatedAt,

    -- * StreamSummary
    StreamSummary (..),
    newStreamSummary,
    streamSummary_streamVersion,
    streamSummary_streamId,
    streamSummary_streamArn,
    streamSummary_description,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TaskStatistics
    TaskStatistics (..),
    newTaskStatistics,
    taskStatistics_totalChecks,
    taskStatistics_waitingForDataCollectionChecks,
    taskStatistics_compliantChecks,
    taskStatistics_inProgressChecks,
    taskStatistics_failedChecks,
    taskStatistics_nonCompliantChecks,
    taskStatistics_canceledChecks,

    -- * TaskStatisticsForAuditCheck
    TaskStatisticsForAuditCheck (..),
    newTaskStatisticsForAuditCheck,
    taskStatisticsForAuditCheck_succeededFindingsCount,
    taskStatisticsForAuditCheck_totalFindingsCount,
    taskStatisticsForAuditCheck_failedFindingsCount,
    taskStatisticsForAuditCheck_skippedFindingsCount,
    taskStatisticsForAuditCheck_canceledFindingsCount,

    -- * ThingAttribute
    ThingAttribute (..),
    newThingAttribute,
    thingAttribute_thingArn,
    thingAttribute_thingName,
    thingAttribute_version,
    thingAttribute_attributes,
    thingAttribute_thingTypeName,

    -- * ThingConnectivity
    ThingConnectivity (..),
    newThingConnectivity,
    thingConnectivity_connected,
    thingConnectivity_timestamp,

    -- * ThingDocument
    ThingDocument (..),
    newThingDocument,
    thingDocument_thingId,
    thingDocument_thingName,
    thingDocument_connectivity,
    thingDocument_attributes,
    thingDocument_thingGroupNames,
    thingDocument_shadow,
    thingDocument_thingTypeName,

    -- * ThingGroupDocument
    ThingGroupDocument (..),
    newThingGroupDocument,
    thingGroupDocument_parentGroupNames,
    thingGroupDocument_attributes,
    thingGroupDocument_thingGroupName,
    thingGroupDocument_thingGroupId,
    thingGroupDocument_thingGroupDescription,

    -- * ThingGroupIndexingConfiguration
    ThingGroupIndexingConfiguration (..),
    newThingGroupIndexingConfiguration,
    thingGroupIndexingConfiguration_managedFields,
    thingGroupIndexingConfiguration_customFields,
    thingGroupIndexingConfiguration_thingGroupIndexingMode,

    -- * ThingGroupMetadata
    ThingGroupMetadata (..),
    newThingGroupMetadata,
    thingGroupMetadata_parentGroupName,
    thingGroupMetadata_creationDate,
    thingGroupMetadata_rootToParentThingGroups,

    -- * ThingGroupProperties
    ThingGroupProperties (..),
    newThingGroupProperties,
    thingGroupProperties_thingGroupDescription,
    thingGroupProperties_attributePayload,

    -- * ThingIndexingConfiguration
    ThingIndexingConfiguration (..),
    newThingIndexingConfiguration,
    thingIndexingConfiguration_managedFields,
    thingIndexingConfiguration_thingConnectivityIndexingMode,
    thingIndexingConfiguration_customFields,
    thingIndexingConfiguration_thingIndexingMode,

    -- * ThingTypeDefinition
    ThingTypeDefinition (..),
    newThingTypeDefinition,
    thingTypeDefinition_thingTypeProperties,
    thingTypeDefinition_thingTypeMetadata,
    thingTypeDefinition_thingTypeArn,
    thingTypeDefinition_thingTypeName,

    -- * ThingTypeMetadata
    ThingTypeMetadata (..),
    newThingTypeMetadata,
    thingTypeMetadata_deprecationDate,
    thingTypeMetadata_creationDate,
    thingTypeMetadata_deprecated,

    -- * ThingTypeProperties
    ThingTypeProperties (..),
    newThingTypeProperties,
    thingTypeProperties_searchableAttributes,
    thingTypeProperties_thingTypeDescription,

    -- * TimeoutConfig
    TimeoutConfig (..),
    newTimeoutConfig,
    timeoutConfig_inProgressTimeoutInMinutes,

    -- * TimestreamAction
    TimestreamAction (..),
    newTimestreamAction,
    timestreamAction_timestamp,
    timestreamAction_roleArn,
    timestreamAction_databaseName,
    timestreamAction_tableName,
    timestreamAction_dimensions,

    -- * TimestreamDimension
    TimestreamDimension (..),
    newTimestreamDimension,
    timestreamDimension_name,
    timestreamDimension_value,

    -- * TimestreamTimestamp
    TimestreamTimestamp (..),
    newTimestreamTimestamp,
    timestreamTimestamp_value,
    timestreamTimestamp_unit,

    -- * TlsContext
    TlsContext (..),
    newTlsContext,
    tlsContext_serverName,

    -- * TopicRule
    TopicRule (..),
    newTopicRule,
    topicRule_ruleName,
    topicRule_errorAction,
    topicRule_awsIotSqlVersion,
    topicRule_createdAt,
    topicRule_actions,
    topicRule_ruleDisabled,
    topicRule_description,
    topicRule_sql,

    -- * TopicRuleDestination
    TopicRuleDestination (..),
    newTopicRuleDestination,
    topicRuleDestination_status,
    topicRuleDestination_createdAt,
    topicRuleDestination_arn,
    topicRuleDestination_vpcProperties,
    topicRuleDestination_statusReason,
    topicRuleDestination_httpUrlProperties,
    topicRuleDestination_lastUpdatedAt,

    -- * TopicRuleDestinationConfiguration
    TopicRuleDestinationConfiguration (..),
    newTopicRuleDestinationConfiguration,
    topicRuleDestinationConfiguration_vpcConfiguration,
    topicRuleDestinationConfiguration_httpUrlConfiguration,

    -- * TopicRuleDestinationSummary
    TopicRuleDestinationSummary (..),
    newTopicRuleDestinationSummary,
    topicRuleDestinationSummary_httpUrlSummary,
    topicRuleDestinationSummary_status,
    topicRuleDestinationSummary_createdAt,
    topicRuleDestinationSummary_arn,
    topicRuleDestinationSummary_statusReason,
    topicRuleDestinationSummary_vpcDestinationSummary,
    topicRuleDestinationSummary_lastUpdatedAt,

    -- * TopicRuleListItem
    TopicRuleListItem (..),
    newTopicRuleListItem,
    topicRuleListItem_ruleName,
    topicRuleListItem_ruleArn,
    topicRuleListItem_createdAt,
    topicRuleListItem_topicPattern,
    topicRuleListItem_ruleDisabled,

    -- * TopicRulePayload
    TopicRulePayload (..),
    newTopicRulePayload,
    topicRulePayload_errorAction,
    topicRulePayload_awsIotSqlVersion,
    topicRulePayload_ruleDisabled,
    topicRulePayload_description,
    topicRulePayload_sql,
    topicRulePayload_actions,

    -- * TransferData
    TransferData (..),
    newTransferData,
    transferData_transferDate,
    transferData_transferMessage,
    transferData_acceptDate,
    transferData_rejectDate,
    transferData_rejectReason,

    -- * UpdateCACertificateParams
    UpdateCACertificateParams (..),
    newUpdateCACertificateParams,
    updateCACertificateParams_action,

    -- * UpdateDeviceCertificateParams
    UpdateDeviceCertificateParams (..),
    newUpdateDeviceCertificateParams,
    updateDeviceCertificateParams_action,

    -- * ValidationError
    ValidationError (..),
    newValidationError,
    validationError_errorMessage,

    -- * ViolationEvent
    ViolationEvent (..),
    newViolationEvent,
    violationEvent_metricValue,
    violationEvent_violationId,
    violationEvent_thingName,
    violationEvent_securityProfileName,
    violationEvent_behavior,
    violationEvent_violationEventTime,
    violationEvent_violationEventType,
    violationEvent_violationEventAdditionalInfo,

    -- * ViolationEventAdditionalInfo
    ViolationEventAdditionalInfo (..),
    newViolationEventAdditionalInfo,
    violationEventAdditionalInfo_confidenceLevel,

    -- * ViolationEventOccurrenceRange
    ViolationEventOccurrenceRange (..),
    newViolationEventOccurrenceRange,
    violationEventOccurrenceRange_startTime,
    violationEventOccurrenceRange_endTime,

    -- * VpcDestinationConfiguration
    VpcDestinationConfiguration (..),
    newVpcDestinationConfiguration,
    vpcDestinationConfiguration_securityGroups,
    vpcDestinationConfiguration_subnetIds,
    vpcDestinationConfiguration_vpcId,
    vpcDestinationConfiguration_roleArn,

    -- * VpcDestinationProperties
    VpcDestinationProperties (..),
    newVpcDestinationProperties,
    vpcDestinationProperties_roleArn,
    vpcDestinationProperties_subnetIds,
    vpcDestinationProperties_securityGroups,
    vpcDestinationProperties_vpcId,

    -- * VpcDestinationSummary
    VpcDestinationSummary (..),
    newVpcDestinationSummary,
    vpcDestinationSummary_roleArn,
    vpcDestinationSummary_subnetIds,
    vpcDestinationSummary_securityGroups,
    vpcDestinationSummary_vpcId,
  )
where

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
import Network.AWS.IoT.Types.AwsJobAbortConfig
import Network.AWS.IoT.Types.AwsJobAbortCriteria
import Network.AWS.IoT.Types.AwsJobAbortCriteriaAbortAction
import Network.AWS.IoT.Types.AwsJobAbortCriteriaFailureType
import Network.AWS.IoT.Types.AwsJobExecutionsRolloutConfig
import Network.AWS.IoT.Types.AwsJobExponentialRolloutRate
import Network.AWS.IoT.Types.AwsJobPresignedUrlConfig
import Network.AWS.IoT.Types.AwsJobRateIncreaseCriteria
import Network.AWS.IoT.Types.AwsJobTimeoutConfig
import Network.AWS.IoT.Types.Behavior
import Network.AWS.IoT.Types.BehaviorCriteria
import Network.AWS.IoT.Types.BehaviorCriteriaType
import Network.AWS.IoT.Types.BehaviorModelTrainingSummary
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
import Network.AWS.IoT.Types.ConfidenceLevel
import Network.AWS.IoT.Types.Configuration
import Network.AWS.IoT.Types.CustomCodeSigning
import Network.AWS.IoT.Types.CustomMetricType
import Network.AWS.IoT.Types.DayOfWeek
import Network.AWS.IoT.Types.Denied
import Network.AWS.IoT.Types.Destination
import Network.AWS.IoT.Types.DetectMitigationActionExecution
import Network.AWS.IoT.Types.DetectMitigationActionExecutionStatus
import Network.AWS.IoT.Types.DetectMitigationActionsTaskStatistics
import Network.AWS.IoT.Types.DetectMitigationActionsTaskStatus
import Network.AWS.IoT.Types.DetectMitigationActionsTaskSummary
import Network.AWS.IoT.Types.DetectMitigationActionsTaskTarget
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
import Network.AWS.IoT.Types.EnableIoTLoggingParams
import Network.AWS.IoT.Types.ErrorInfo
import Network.AWS.IoT.Types.EventType
import Network.AWS.IoT.Types.ExplicitDeny
import Network.AWS.IoT.Types.ExponentialRolloutRate
import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.FieldType
import Network.AWS.IoT.Types.FileLocation
import Network.AWS.IoT.Types.FirehoseAction
import Network.AWS.IoT.Types.GroupNameAndArn
import Network.AWS.IoT.Types.HttpAction
import Network.AWS.IoT.Types.HttpActionHeader
import Network.AWS.IoT.Types.HttpAuthorization
import Network.AWS.IoT.Types.HttpContext
import Network.AWS.IoT.Types.HttpUrlDestinationConfiguration
import Network.AWS.IoT.Types.HttpUrlDestinationProperties
import Network.AWS.IoT.Types.HttpUrlDestinationSummary
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
import Network.AWS.IoT.Types.KafkaAction
import Network.AWS.IoT.Types.KeyPair
import Network.AWS.IoT.Types.KinesisAction
import Network.AWS.IoT.Types.LambdaAction
import Network.AWS.IoT.Types.LogLevel
import Network.AWS.IoT.Types.LogTarget
import Network.AWS.IoT.Types.LogTargetConfiguration
import Network.AWS.IoT.Types.LogTargetType
import Network.AWS.IoT.Types.LoggingOptionsPayload
import Network.AWS.IoT.Types.MachineLearningDetectionConfig
import Network.AWS.IoT.Types.MessageFormat
import Network.AWS.IoT.Types.MetricDimension
import Network.AWS.IoT.Types.MetricToRetain
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.MitigationAction
import Network.AWS.IoT.Types.MitigationActionIdentifier
import Network.AWS.IoT.Types.MitigationActionParams
import Network.AWS.IoT.Types.MitigationActionType
import Network.AWS.IoT.Types.ModelStatus
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
import Network.AWS.IoT.Types.PresignedUrlConfig
import Network.AWS.IoT.Types.Protocol
import Network.AWS.IoT.Types.ProvisioningHook
import Network.AWS.IoT.Types.ProvisioningTemplateSummary
import Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
import Network.AWS.IoT.Types.PublishFindingToSnsParams
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
import Network.AWS.IoT.Types.SnsAction
import Network.AWS.IoT.Types.SqsAction
import Network.AWS.IoT.Types.StartSigningJobParameter
import Network.AWS.IoT.Types.StatisticalThreshold
import Network.AWS.IoT.Types.Statistics
import Network.AWS.IoT.Types.StepFunctionsAction
import Network.AWS.IoT.Types.Stream
import Network.AWS.IoT.Types.StreamFile
import Network.AWS.IoT.Types.StreamInfo
import Network.AWS.IoT.Types.StreamSummary
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
import Network.AWS.IoT.Types.TlsContext
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
import Network.AWS.IoT.Types.ViolationEventAdditionalInfo
import Network.AWS.IoT.Types.ViolationEventOccurrenceRange
import Network.AWS.IoT.Types.ViolationEventType
import Network.AWS.IoT.Types.VpcDestinationConfiguration
import Network.AWS.IoT.Types.VpcDestinationProperties
import Network.AWS.IoT.Types.VpcDestinationSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-05-28@ of the Amazon IoT SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "IoT",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "iot",
      Prelude._svcSigningName = "execute-api",
      Prelude._svcVersion = "2015-05-28",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "IoT",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The index is not ready.
_IndexNotReadyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IndexNotReadyException =
  Prelude._MatchServiceError
    defaultService
    "IndexNotReadyException"
    Prelude.. Prelude.hasStatus 400

-- | You can\'t revert the certificate transfer because the transfer is
-- already complete.
_TransferAlreadyCompletedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TransferAlreadyCompletedException =
  Prelude._MatchServiceError
    defaultService
    "TransferAlreadyCompletedException"
    Prelude.. Prelude.hasStatus 410

-- | The query is invalid.
_InvalidQueryException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidQueryException =
  Prelude._MatchServiceError
    defaultService
    "InvalidQueryException"
    Prelude.. Prelude.hasStatus 400

-- | Unable to verify the CA certificate used to sign the device certificate
-- you are attempting to register. This is happens when you have registered
-- more than one CA certificate that has the same subject field and public
-- key.
_CertificateConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CertificateConflictException =
  Prelude._MatchServiceError
    defaultService
    "CertificateConflictException"
    Prelude.. Prelude.hasStatus 409

-- | This exception occurs if you attempt to start a task with the same
-- task-id as an existing task but with a different clientRequestToken.
_TaskAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TaskAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "TaskAlreadyExistsException"
    Prelude.. Prelude.hasStatus 400

-- | The certificate is invalid.
_CertificateValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CertificateValidationException =
  Prelude._MatchServiceError
    defaultService
    "CertificateValidationException"
    Prelude.. Prelude.hasStatus 400

-- | You are not authorized to perform this operation.
_UnauthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnauthorizedException =
  Prelude._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Prelude.hasStatus 401

-- | The resource already exists.
_ResourceAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Prelude.hasStatus 409

-- | An unexpected error has occurred.
_InternalException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalException =
  Prelude._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Prelude.hasStatus 500

-- | The policy documentation is not valid.
_MalformedPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MalformedPolicyException =
  Prelude._MatchServiceError
    defaultService
    "MalformedPolicyException"
    Prelude.. Prelude.hasStatus 400

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Prelude.hasStatus 503

-- | The certificate operation is not allowed.
_CertificateStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CertificateStateException =
  Prelude._MatchServiceError
    defaultService
    "CertificateStateException"
    Prelude.. Prelude.hasStatus 406

-- | The aggregation is invalid.
_InvalidAggregationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAggregationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAggregationException"
    Prelude.. Prelude.hasStatus 400

-- | The rate exceeds the limit.
_ThrottlingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ThrottlingException =
  Prelude._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Prelude.hasStatus 400

-- | The request is not valid.
_InvalidRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Prelude.hasStatus 400

-- | The resource registration failed.
_ResourceRegistrationFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceRegistrationFailureException =
  Prelude._MatchServiceError
    defaultService
    "ResourceRegistrationFailureException"
    Prelude.. Prelude.hasStatus 400

-- | The Rule-SQL expression can\'t be parsed correctly.
_SqlParseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SqlParseException =
  Prelude._MatchServiceError
    defaultService
    "SqlParseException"
    Prelude.. Prelude.hasStatus 400

-- | A limit has been exceeded.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Prelude.hasStatus 410

-- | An attempt was made to change to an invalid state, for example by
-- deleting a job or a job execution which is \"IN_PROGRESS\" without
-- setting the @force@ parameter.
_InvalidStateTransitionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidStateTransitionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidStateTransitionException"
    Prelude.. Prelude.hasStatus 409

-- | The resource is not configured.
_NotConfiguredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotConfiguredException =
  Prelude._MatchServiceError
    defaultService
    "NotConfiguredException"
    Prelude.. Prelude.hasStatus 404

-- | The specified resource does not exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | An exception thrown when the version of an entity specified with the
-- @expectedVersion@ parameter does not match the latest version in the
-- system.
_VersionConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_VersionConflictException =
  Prelude._MatchServiceError
    defaultService
    "VersionConflictException"
    Prelude.. Prelude.hasStatus 409

-- | The registration code is invalid.
_RegistrationCodeValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RegistrationCodeValidationException =
  Prelude._MatchServiceError
    defaultService
    "RegistrationCodeValidationException"
    Prelude.. Prelude.hasStatus 400

-- | The number of policy versions exceeds the limit.
_VersionsLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_VersionsLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "VersionsLimitExceededException"
    Prelude.. Prelude.hasStatus 409

-- | An unexpected error has occurred.
_InternalFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalFailureException =
  Prelude._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Prelude.hasStatus 500

-- | You can\'t delete the resource because it is attached to one or more
-- resources.
_DeleteConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeleteConflictException =
  Prelude._MatchServiceError
    defaultService
    "DeleteConflictException"
    Prelude.. Prelude.hasStatus 409

-- | The response is invalid.
_InvalidResponseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidResponseException =
  Prelude._MatchServiceError
    defaultService
    "InvalidResponseException"
    Prelude.. Prelude.hasStatus 400

-- | You can\'t transfer the certificate because authorization policies are
-- still attached.
_TransferConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TransferConflictException =
  Prelude._MatchServiceError
    defaultService
    "TransferConflictException"
    Prelude.. Prelude.hasStatus 409

-- | A conflicting resource update exception. This exception is thrown when
-- two pending updates cause a conflict.
_ConflictingResourceUpdateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConflictingResourceUpdateException =
  Prelude._MatchServiceError
    defaultService
    "ConflictingResourceUpdateException"
    Prelude.. Prelude.hasStatus 409
