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
    _TaskAlreadyExistsException,
    _CertificateConflictException,
    _SqlParseException,
    _IndexNotReadyException,
    _InvalidRequestException,
    _TransferConflictException,
    _CertificateStateException,
    _InvalidResponseException,
    _RegistrationCodeValidationException,
    _MalformedPolicyException,
    _DeleteConflictException,
    _ResourceAlreadyExistsException,
    _NotConfiguredException,
    _CertificateValidationException,
    _ConflictException,
    _ResourceRegistrationFailureException,
    _InvalidQueryException,
    _TransferAlreadyCompletedException,
    _ThrottlingException,
    _InvalidAggregationException,
    _ConflictingResourceUpdateException,
    _InternalFailureException,
    _VersionsLimitExceededException,
    _ServiceUnavailableException,
    _InternalException,
    _VersionConflictException,
    _UnauthorizedException,
    _InvalidStateTransitionException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * AbortAction
    AbortAction (..),

    -- * ActionType
    ActionType (..),

    -- * AggregationTypeName
    AggregationTypeName (..),

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

    -- * FleetMetricUnit
    FleetMetricUnit (..),

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

    -- * VerificationState
    VerificationState (..),

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
    action_cloudwatchMetric,
    action_cloudwatchLogs,
    action_dynamoDBv2,
    action_stepFunctions,
    action_cloudwatchAlarm,
    action_sns,
    action_dynamoDB,
    action_firehose,
    action_timestream,
    action_iotSiteWise,
    action_iotAnalytics,
    action_lambda,
    action_openSearch,
    action_iotEvents,
    action_salesforce,
    action_kinesis,
    action_s3,
    action_http,
    action_elasticsearch,
    action_kafka,
    action_republish,
    action_sqs,

    -- * ActiveViolation
    ActiveViolation (..),
    newActiveViolation,
    activeViolation_verificationStateDescription,
    activeViolation_lastViolationValue,
    activeViolation_lastViolationTime,
    activeViolation_violationStartTime,
    activeViolation_violationId,
    activeViolation_behavior,
    activeViolation_securityProfileName,
    activeViolation_violationEventAdditionalInfo,
    activeViolation_thingName,
    activeViolation_verificationState,

    -- * AddThingsToThingGroupParams
    AddThingsToThingGroupParams (..),
    newAddThingsToThingGroupParams,
    addThingsToThingGroupParams_overrideDynamicGroups,
    addThingsToThingGroupParams_thingGroupNames,

    -- * AggregationType
    AggregationType (..),
    newAggregationType,
    aggregationType_values,
    aggregationType_name,

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
    assetPropertyVariant_integerValue,
    assetPropertyVariant_doubleValue,
    assetPropertyVariant_stringValue,
    assetPropertyVariant_booleanValue,

    -- * AttributePayload
    AttributePayload (..),
    newAttributePayload,
    attributePayload_attributes,
    attributePayload_merge,

    -- * AuditCheckConfiguration
    AuditCheckConfiguration (..),
    newAuditCheckConfiguration,
    auditCheckConfiguration_enabled,

    -- * AuditCheckDetails
    AuditCheckDetails (..),
    newAuditCheckDetails,
    auditCheckDetails_suppressedNonCompliantResourcesCount,
    auditCheckDetails_totalResourcesCount,
    auditCheckDetails_checkCompliant,
    auditCheckDetails_nonCompliantResourcesCount,
    auditCheckDetails_errorCode,
    auditCheckDetails_message,
    auditCheckDetails_checkRunStatus,

    -- * AuditFinding
    AuditFinding (..),
    newAuditFinding,
    auditFinding_isSuppressed,
    auditFinding_taskId,
    auditFinding_findingTime,
    auditFinding_taskStartTime,
    auditFinding_reasonForNonComplianceCode,
    auditFinding_severity,
    auditFinding_relatedResources,
    auditFinding_checkName,
    auditFinding_nonCompliantResource,
    auditFinding_reasonForNonCompliance,
    auditFinding_findingId,

    -- * AuditMitigationActionExecutionMetadata
    AuditMitigationActionExecutionMetadata (..),
    newAuditMitigationActionExecutionMetadata,
    auditMitigationActionExecutionMetadata_status,
    auditMitigationActionExecutionMetadata_startTime,
    auditMitigationActionExecutionMetadata_taskId,
    auditMitigationActionExecutionMetadata_actionId,
    auditMitigationActionExecutionMetadata_actionName,
    auditMitigationActionExecutionMetadata_endTime,
    auditMitigationActionExecutionMetadata_errorCode,
    auditMitigationActionExecutionMetadata_findingId,
    auditMitigationActionExecutionMetadata_message,

    -- * AuditMitigationActionsTaskMetadata
    AuditMitigationActionsTaskMetadata (..),
    newAuditMitigationActionsTaskMetadata,
    auditMitigationActionsTaskMetadata_startTime,
    auditMitigationActionsTaskMetadata_taskId,
    auditMitigationActionsTaskMetadata_taskStatus,

    -- * AuditMitigationActionsTaskTarget
    AuditMitigationActionsTaskTarget (..),
    newAuditMitigationActionsTaskTarget,
    auditMitigationActionsTaskTarget_auditTaskId,
    auditMitigationActionsTaskTarget_findingIds,
    auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter,

    -- * AuditNotificationTarget
    AuditNotificationTarget (..),
    newAuditNotificationTarget,
    auditNotificationTarget_targetArn,
    auditNotificationTarget_enabled,
    auditNotificationTarget_roleArn,

    -- * AuditSuppression
    AuditSuppression (..),
    newAuditSuppression,
    auditSuppression_expirationDate,
    auditSuppression_suppressIndefinitely,
    auditSuppression_description,
    auditSuppression_checkName,
    auditSuppression_resourceIdentifier,

    -- * AuditTaskMetadata
    AuditTaskMetadata (..),
    newAuditTaskMetadata,
    auditTaskMetadata_taskType,
    auditTaskMetadata_taskId,
    auditTaskMetadata_taskStatus,

    -- * AuthInfo
    AuthInfo (..),
    newAuthInfo,
    authInfo_actionType,
    authInfo_resources,

    -- * AuthResult
    AuthResult (..),
    newAuthResult,
    authResult_denied,
    authResult_authDecision,
    authResult_allowed,
    authResult_missingContextValues,
    authResult_authInfo,

    -- * AuthorizerConfig
    AuthorizerConfig (..),
    newAuthorizerConfig,
    authorizerConfig_allowAuthorizerOverride,
    authorizerConfig_defaultAuthorizerName,

    -- * AuthorizerDescription
    AuthorizerDescription (..),
    newAuthorizerDescription,
    authorizerDescription_status,
    authorizerDescription_lastModifiedDate,
    authorizerDescription_signingDisabled,
    authorizerDescription_authorizerName,
    authorizerDescription_authorizerFunctionArn,
    authorizerDescription_authorizerArn,
    authorizerDescription_creationDate,
    authorizerDescription_tokenSigningPublicKeys,
    authorizerDescription_tokenKeyName,

    -- * AuthorizerSummary
    AuthorizerSummary (..),
    newAuthorizerSummary,
    authorizerSummary_authorizerName,
    authorizerSummary_authorizerArn,

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
    behavior_suppressAlerts,
    behavior_metricDimension,
    behavior_metric,
    behavior_criteria,
    behavior_name,

    -- * BehaviorCriteria
    BehaviorCriteria (..),
    newBehaviorCriteria,
    behaviorCriteria_mlDetectionConfig,
    behaviorCriteria_value,
    behaviorCriteria_consecutiveDatapointsToAlarm,
    behaviorCriteria_comparisonOperator,
    behaviorCriteria_statisticalThreshold,
    behaviorCriteria_durationSeconds,
    behaviorCriteria_consecutiveDatapointsToClear,

    -- * BehaviorModelTrainingSummary
    BehaviorModelTrainingSummary (..),
    newBehaviorModelTrainingSummary,
    behaviorModelTrainingSummary_lastModelRefreshDate,
    behaviorModelTrainingSummary_behaviorName,
    behaviorModelTrainingSummary_datapointsCollectionPercentage,
    behaviorModelTrainingSummary_securityProfileName,
    behaviorModelTrainingSummary_trainingDataCollectionStartDate,
    behaviorModelTrainingSummary_modelStatus,

    -- * BillingGroupMetadata
    BillingGroupMetadata (..),
    newBillingGroupMetadata,
    billingGroupMetadata_creationDate,

    -- * BillingGroupProperties
    BillingGroupProperties (..),
    newBillingGroupProperties,
    billingGroupProperties_billingGroupDescription,

    -- * Bucket
    Bucket (..),
    newBucket,
    bucket_keyValue,
    bucket_count,

    -- * BucketsAggregationType
    BucketsAggregationType (..),
    newBucketsAggregationType,
    bucketsAggregationType_termsAggregation,

    -- * CACertificate
    CACertificate (..),
    newCACertificate,
    cACertificate_status,
    cACertificate_certificateArn,
    cACertificate_certificateId,
    cACertificate_creationDate,

    -- * CACertificateDescription
    CACertificateDescription (..),
    newCACertificateDescription,
    cACertificateDescription_status,
    cACertificateDescription_ownedBy,
    cACertificateDescription_lastModifiedDate,
    cACertificateDescription_certificatePem,
    cACertificateDescription_certificateArn,
    cACertificateDescription_certificateId,
    cACertificateDescription_validity,
    cACertificateDescription_autoRegistrationStatus,
    cACertificateDescription_creationDate,
    cACertificateDescription_generationId,
    cACertificateDescription_customerVersion,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_status,
    certificate_certificateArn,
    certificate_certificateId,
    certificate_certificateMode,
    certificate_creationDate,

    -- * CertificateDescription
    CertificateDescription (..),
    newCertificateDescription,
    certificateDescription_status,
    certificateDescription_ownedBy,
    certificateDescription_lastModifiedDate,
    certificateDescription_caCertificateId,
    certificateDescription_previousOwnedBy,
    certificateDescription_certificatePem,
    certificateDescription_certificateArn,
    certificateDescription_certificateId,
    certificateDescription_certificateMode,
    certificateDescription_validity,
    certificateDescription_creationDate,
    certificateDescription_generationId,
    certificateDescription_transferData,
    certificateDescription_customerVersion,

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
    codeSigning_customCodeSigning,
    codeSigning_startSigningJobParameter,
    codeSigning_awsSignerJobId,

    -- * CodeSigningCertificateChain
    CodeSigningCertificateChain (..),
    newCodeSigningCertificateChain,
    codeSigningCertificateChain_certificateName,
    codeSigningCertificateChain_inlineDocument,

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
    customCodeSigning_hashAlgorithm,
    customCodeSigning_certificateChain,
    customCodeSigning_signatureAlgorithm,

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
    detectMitigationActionExecution_status,
    detectMitigationActionExecution_taskId,
    detectMitigationActionExecution_actionName,
    detectMitigationActionExecution_violationId,
    detectMitigationActionExecution_errorCode,
    detectMitigationActionExecution_message,
    detectMitigationActionExecution_thingName,
    detectMitigationActionExecution_executionStartDate,
    detectMitigationActionExecution_executionEndDate,

    -- * DetectMitigationActionsTaskStatistics
    DetectMitigationActionsTaskStatistics (..),
    newDetectMitigationActionsTaskStatistics,
    detectMitigationActionsTaskStatistics_actionsFailed,
    detectMitigationActionsTaskStatistics_actionsSkipped,
    detectMitigationActionsTaskStatistics_actionsExecuted,

    -- * DetectMitigationActionsTaskSummary
    DetectMitigationActionsTaskSummary (..),
    newDetectMitigationActionsTaskSummary,
    detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded,
    detectMitigationActionsTaskSummary_suppressedAlertsIncluded,
    detectMitigationActionsTaskSummary_violationEventOccurrenceRange,
    detectMitigationActionsTaskSummary_taskId,
    detectMitigationActionsTaskSummary_taskStartTime,
    detectMitigationActionsTaskSummary_taskStatistics,
    detectMitigationActionsTaskSummary_actionsDefinition,
    detectMitigationActionsTaskSummary_taskEndTime,
    detectMitigationActionsTaskSummary_target,
    detectMitigationActionsTaskSummary_taskStatus,

    -- * DetectMitigationActionsTaskTarget
    DetectMitigationActionsTaskTarget (..),
    newDetectMitigationActionsTaskTarget,
    detectMitigationActionsTaskTarget_violationIds,
    detectMitigationActionsTaskTarget_behaviorName,
    detectMitigationActionsTaskTarget_securityProfileName,

    -- * DomainConfigurationSummary
    DomainConfigurationSummary (..),
    newDomainConfigurationSummary,
    domainConfigurationSummary_domainConfigurationName,
    domainConfigurationSummary_domainConfigurationArn,
    domainConfigurationSummary_serviceType,

    -- * DynamoDBAction
    DynamoDBAction (..),
    newDynamoDBAction,
    dynamoDBAction_hashKeyType,
    dynamoDBAction_operation,
    dynamoDBAction_rangeKeyType,
    dynamoDBAction_payloadField,
    dynamoDBAction_rangeKeyField,
    dynamoDBAction_rangeKeyValue,
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
    errorInfo_code,
    errorInfo_message,

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
    firehoseAction_batchMode,
    firehoseAction_separator,
    firehoseAction_roleArn,
    firehoseAction_deliveryStreamName,

    -- * FleetMetricNameAndArn
    FleetMetricNameAndArn (..),
    newFleetMetricNameAndArn,
    fleetMetricNameAndArn_metricName,
    fleetMetricNameAndArn_metricArn,

    -- * GroupNameAndArn
    GroupNameAndArn (..),
    newGroupNameAndArn,
    groupNameAndArn_groupArn,
    groupNameAndArn_groupName,

    -- * HttpAction
    HttpAction (..),
    newHttpAction,
    httpAction_confirmationUrl,
    httpAction_auth,
    httpAction_headers,
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
    httpContext_headers,
    httpContext_queryString,

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
    iotAnalyticsAction_batchMode,
    iotAnalyticsAction_channelArn,
    iotAnalyticsAction_channelName,
    iotAnalyticsAction_roleArn,

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
    job_status,
    job_jobExecutionsRolloutConfig,
    job_jobId,
    job_lastUpdatedAt,
    job_jobArn,
    job_createdAt,
    job_abortConfig,
    job_jobProcessDetails,
    job_namespaceId,
    job_reasonCode,
    job_presignedUrlConfig,
    job_forceCanceled,
    job_jobTemplateArn,
    job_targets,
    job_completedAt,
    job_comment,
    job_description,
    job_targetSelection,
    job_timeoutConfig,

    -- * JobExecution
    JobExecution (..),
    newJobExecution,
    jobExecution_status,
    jobExecution_jobId,
    jobExecution_lastUpdatedAt,
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_queuedAt,
    jobExecution_statusDetails,
    jobExecution_thingArn,
    jobExecution_executionNumber,
    jobExecution_versionNumber,
    jobExecution_startedAt,
    jobExecution_forceCanceled,

    -- * JobExecutionStatusDetails
    JobExecutionStatusDetails (..),
    newJobExecutionStatusDetails,
    jobExecutionStatusDetails_detailsMap,

    -- * JobExecutionSummary
    JobExecutionSummary (..),
    newJobExecutionSummary,
    jobExecutionSummary_status,
    jobExecutionSummary_lastUpdatedAt,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_startedAt,

    -- * JobExecutionSummaryForJob
    JobExecutionSummaryForJob (..),
    newJobExecutionSummaryForJob,
    jobExecutionSummaryForJob_jobExecutionSummary,
    jobExecutionSummaryForJob_thingArn,

    -- * JobExecutionSummaryForThing
    JobExecutionSummaryForThing (..),
    newJobExecutionSummaryForThing,
    jobExecutionSummaryForThing_jobId,
    jobExecutionSummaryForThing_jobExecutionSummary,

    -- * JobExecutionsRolloutConfig
    JobExecutionsRolloutConfig (..),
    newJobExecutionsRolloutConfig,
    jobExecutionsRolloutConfig_exponentialRate,
    jobExecutionsRolloutConfig_maximumPerMinute,

    -- * JobProcessDetails
    JobProcessDetails (..),
    newJobProcessDetails,
    jobProcessDetails_numberOfRemovedThings,
    jobProcessDetails_numberOfQueuedThings,
    jobProcessDetails_numberOfFailedThings,
    jobProcessDetails_numberOfSucceededThings,
    jobProcessDetails_numberOfInProgressThings,
    jobProcessDetails_numberOfCanceledThings,
    jobProcessDetails_numberOfTimedOutThings,
    jobProcessDetails_numberOfRejectedThings,
    jobProcessDetails_processingTargets,

    -- * JobSummary
    JobSummary (..),
    newJobSummary,
    jobSummary_status,
    jobSummary_jobId,
    jobSummary_lastUpdatedAt,
    jobSummary_jobArn,
    jobSummary_createdAt,
    jobSummary_thingGroupId,
    jobSummary_completedAt,
    jobSummary_targetSelection,

    -- * JobTemplateSummary
    JobTemplateSummary (..),
    newJobTemplateSummary,
    jobTemplateSummary_createdAt,
    jobTemplateSummary_jobTemplateId,
    jobTemplateSummary_jobTemplateArn,
    jobTemplateSummary_description,

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
    keyPair_privateKey,
    keyPair_publicKey,

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
    metricValue_cidrs,
    metricValue_count,
    metricValue_ports,
    metricValue_numbers,
    metricValue_number,
    metricValue_strings,

    -- * MitigationAction
    MitigationAction (..),
    newMitigationAction,
    mitigationAction_actionParams,
    mitigationAction_name,
    mitigationAction_id,
    mitigationAction_roleArn,

    -- * MitigationActionIdentifier
    MitigationActionIdentifier (..),
    newMitigationActionIdentifier,
    mitigationActionIdentifier_actionName,
    mitigationActionIdentifier_creationDate,
    mitigationActionIdentifier_actionArn,

    -- * MitigationActionParams
    MitigationActionParams (..),
    newMitigationActionParams,
    mitigationActionParams_enableIoTLoggingParams,
    mitigationActionParams_addThingsToThingGroupParams,
    mitigationActionParams_updateCACertificateParams,
    mitigationActionParams_updateDeviceCertificateParams,
    mitigationActionParams_replaceDefaultPolicyVersionParams,
    mitigationActionParams_publishFindingToSnsParams,

    -- * MqttContext
    MqttContext (..),
    newMqttContext,
    mqttContext_clientId,
    mqttContext_username,
    mqttContext_password,

    -- * NonCompliantResource
    NonCompliantResource (..),
    newNonCompliantResource,
    nonCompliantResource_additionalInfo,
    nonCompliantResource_resourceType,
    nonCompliantResource_resourceIdentifier,

    -- * OTAUpdateFile
    OTAUpdateFile (..),
    newOTAUpdateFile,
    oTAUpdateFile_fileLocation,
    oTAUpdateFile_fileType,
    oTAUpdateFile_fileVersion,
    oTAUpdateFile_attributes,
    oTAUpdateFile_codeSigning,
    oTAUpdateFile_fileName,

    -- * OTAUpdateInfo
    OTAUpdateInfo (..),
    newOTAUpdateInfo,
    oTAUpdateInfo_lastModifiedDate,
    oTAUpdateInfo_awsJobExecutionsRolloutConfig,
    oTAUpdateInfo_awsIotJobId,
    oTAUpdateInfo_protocols,
    oTAUpdateInfo_awsJobPresignedUrlConfig,
    oTAUpdateInfo_otaUpdateFiles,
    oTAUpdateInfo_otaUpdateStatus,
    oTAUpdateInfo_targets,
    oTAUpdateInfo_awsIotJobArn,
    oTAUpdateInfo_creationDate,
    oTAUpdateInfo_additionalParameters,
    oTAUpdateInfo_otaUpdateId,
    oTAUpdateInfo_errorInfo,
    oTAUpdateInfo_otaUpdateArn,
    oTAUpdateInfo_description,
    oTAUpdateInfo_targetSelection,

    -- * OTAUpdateSummary
    OTAUpdateSummary (..),
    newOTAUpdateSummary,
    oTAUpdateSummary_creationDate,
    oTAUpdateSummary_otaUpdateId,
    oTAUpdateSummary_otaUpdateArn,

    -- * OpenSearchAction
    OpenSearchAction (..),
    newOpenSearchAction,
    openSearchAction_roleArn,
    openSearchAction_endpoint,
    openSearchAction_index,
    openSearchAction_type,
    openSearchAction_id,

    -- * OutgoingCertificate
    OutgoingCertificate (..),
    newOutgoingCertificate,
    outgoingCertificate_transferDate,
    outgoingCertificate_certificateArn,
    outgoingCertificate_certificateId,
    outgoingCertificate_transferredTo,
    outgoingCertificate_creationDate,
    outgoingCertificate_transferMessage,

    -- * PercentPair
    PercentPair (..),
    newPercentPair,
    percentPair_value,
    percentPair_percent,

    -- * Policy
    Policy (..),
    newPolicy,
    policy_policyName,
    policy_policyArn,

    -- * PolicyVersion
    PolicyVersion (..),
    newPolicyVersion,
    policyVersion_versionId,
    policyVersion_createDate,
    policyVersion_isDefaultVersion,

    -- * PolicyVersionIdentifier
    PolicyVersionIdentifier (..),
    newPolicyVersionIdentifier,
    policyVersionIdentifier_policyName,
    policyVersionIdentifier_policyVersionId,

    -- * PresignedUrlConfig
    PresignedUrlConfig (..),
    newPresignedUrlConfig,
    presignedUrlConfig_expiresInSec,
    presignedUrlConfig_roleArn,

    -- * ProvisioningHook
    ProvisioningHook (..),
    newProvisioningHook,
    provisioningHook_payloadVersion,
    provisioningHook_targetArn,

    -- * ProvisioningTemplateSummary
    ProvisioningTemplateSummary (..),
    newProvisioningTemplateSummary,
    provisioningTemplateSummary_lastModifiedDate,
    provisioningTemplateSummary_templateName,
    provisioningTemplateSummary_enabled,
    provisioningTemplateSummary_creationDate,
    provisioningTemplateSummary_templateArn,
    provisioningTemplateSummary_description,

    -- * ProvisioningTemplateVersionSummary
    ProvisioningTemplateVersionSummary (..),
    newProvisioningTemplateVersionSummary,
    provisioningTemplateVersionSummary_versionId,
    provisioningTemplateVersionSummary_creationDate,
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
    putAssetPropertyValueEntry_propertyId,
    putAssetPropertyValueEntry_assetId,
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
    registrationConfig_templateBody,
    registrationConfig_roleArn,

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
    resourceIdentifier_iamRoleArn,
    resourceIdentifier_clientId,
    resourceIdentifier_roleAliasArn,
    resourceIdentifier_caCertificateId,
    resourceIdentifier_deviceCertificateId,
    resourceIdentifier_account,
    resourceIdentifier_policyVersionIdentifier,
    resourceIdentifier_cognitoIdentityPoolId,

    -- * RoleAliasDescription
    RoleAliasDescription (..),
    newRoleAliasDescription,
    roleAliasDescription_roleAliasArn,
    roleAliasDescription_lastModifiedDate,
    roleAliasDescription_roleAlias,
    roleAliasDescription_owner,
    roleAliasDescription_creationDate,
    roleAliasDescription_credentialDurationSeconds,
    roleAliasDescription_roleArn,

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
    s3Location_bucket,
    s3Location_key,
    s3Location_version,

    -- * SalesforceAction
    SalesforceAction (..),
    newSalesforceAction,
    salesforceAction_token,
    salesforceAction_url,

    -- * ScheduledAuditMetadata
    ScheduledAuditMetadata (..),
    newScheduledAuditMetadata,
    scheduledAuditMetadata_frequency,
    scheduledAuditMetadata_scheduledAuditName,
    scheduledAuditMetadata_dayOfMonth,
    scheduledAuditMetadata_dayOfWeek,
    scheduledAuditMetadata_scheduledAuditArn,

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
    securityProfileTargetMapping_securityProfileIdentifier,
    securityProfileTargetMapping_target,

    -- * ServerCertificateSummary
    ServerCertificateSummary (..),
    newServerCertificateSummary,
    serverCertificateSummary_serverCertificateStatusDetail,
    serverCertificateSummary_serverCertificateStatus,
    serverCertificateSummary_serverCertificateArn,

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
    startSigningJobParameter_destination,
    startSigningJobParameter_signingProfileName,
    startSigningJobParameter_signingProfileParameter,

    -- * StatisticalThreshold
    StatisticalThreshold (..),
    newStatisticalThreshold,
    statisticalThreshold_statistic,

    -- * Statistics
    Statistics (..),
    newStatistics,
    statistics_stdDeviation,
    statistics_maximum,
    statistics_average,
    statistics_count,
    statistics_minimum,
    statistics_variance,
    statistics_sumOfSquares,
    statistics_sum,

    -- * StepFunctionsAction
    StepFunctionsAction (..),
    newStepFunctionsAction,
    stepFunctionsAction_executionNamePrefix,
    stepFunctionsAction_stateMachineName,
    stepFunctionsAction_roleArn,

    -- * Stream
    Stream (..),
    newStream,
    stream_fileId,
    stream_streamId,

    -- * StreamFile
    StreamFile (..),
    newStreamFile,
    streamFile_s3Location,
    streamFile_fileId,

    -- * StreamInfo
    StreamInfo (..),
    newStreamInfo,
    streamInfo_lastUpdatedAt,
    streamInfo_createdAt,
    streamInfo_streamVersion,
    streamInfo_streamArn,
    streamInfo_files,
    streamInfo_description,
    streamInfo_streamId,
    streamInfo_roleArn,

    -- * StreamSummary
    StreamSummary (..),
    newStreamSummary,
    streamSummary_streamVersion,
    streamSummary_streamArn,
    streamSummary_description,
    streamSummary_streamId,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TaskStatistics
    TaskStatistics (..),
    newTaskStatistics,
    taskStatistics_nonCompliantChecks,
    taskStatistics_waitingForDataCollectionChecks,
    taskStatistics_failedChecks,
    taskStatistics_totalChecks,
    taskStatistics_inProgressChecks,
    taskStatistics_compliantChecks,
    taskStatistics_canceledChecks,

    -- * TaskStatisticsForAuditCheck
    TaskStatisticsForAuditCheck (..),
    newTaskStatisticsForAuditCheck,
    taskStatisticsForAuditCheck_canceledFindingsCount,
    taskStatisticsForAuditCheck_skippedFindingsCount,
    taskStatisticsForAuditCheck_totalFindingsCount,
    taskStatisticsForAuditCheck_failedFindingsCount,
    taskStatisticsForAuditCheck_succeededFindingsCount,

    -- * TermsAggregation
    TermsAggregation (..),
    newTermsAggregation,
    termsAggregation_maxBuckets,

    -- * ThingAttribute
    ThingAttribute (..),
    newThingAttribute,
    thingAttribute_thingTypeName,
    thingAttribute_thingArn,
    thingAttribute_attributes,
    thingAttribute_version,
    thingAttribute_thingName,

    -- * ThingConnectivity
    ThingConnectivity (..),
    newThingConnectivity,
    thingConnectivity_disconnectReason,
    thingConnectivity_connected,
    thingConnectivity_timestamp,

    -- * ThingDocument
    ThingDocument (..),
    newThingDocument,
    thingDocument_thingGroupNames,
    thingDocument_thingTypeName,
    thingDocument_shadow,
    thingDocument_attributes,
    thingDocument_connectivity,
    thingDocument_thingName,
    thingDocument_thingId,

    -- * ThingGroupDocument
    ThingGroupDocument (..),
    newThingGroupDocument,
    thingGroupDocument_parentGroupNames,
    thingGroupDocument_thingGroupId,
    thingGroupDocument_thingGroupName,
    thingGroupDocument_attributes,
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
    thingGroupMetadata_rootToParentThingGroups,
    thingGroupMetadata_parentGroupName,
    thingGroupMetadata_creationDate,

    -- * ThingGroupProperties
    ThingGroupProperties (..),
    newThingGroupProperties,
    thingGroupProperties_attributePayload,
    thingGroupProperties_thingGroupDescription,

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
    thingTypeDefinition_thingTypeName,
    thingTypeDefinition_thingTypeMetadata,
    thingTypeDefinition_thingTypeArn,

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
    topicRule_createdAt,
    topicRule_actions,
    topicRule_awsIotSqlVersion,
    topicRule_errorAction,
    topicRule_ruleDisabled,
    topicRule_ruleName,
    topicRule_sql,
    topicRule_description,

    -- * TopicRuleDestination
    TopicRuleDestination (..),
    newTopicRuleDestination,
    topicRuleDestination_vpcProperties,
    topicRuleDestination_status,
    topicRuleDestination_lastUpdatedAt,
    topicRuleDestination_httpUrlProperties,
    topicRuleDestination_arn,
    topicRuleDestination_createdAt,
    topicRuleDestination_statusReason,

    -- * TopicRuleDestinationConfiguration
    TopicRuleDestinationConfiguration (..),
    newTopicRuleDestinationConfiguration,
    topicRuleDestinationConfiguration_vpcConfiguration,
    topicRuleDestinationConfiguration_httpUrlConfiguration,

    -- * TopicRuleDestinationSummary
    TopicRuleDestinationSummary (..),
    newTopicRuleDestinationSummary,
    topicRuleDestinationSummary_status,
    topicRuleDestinationSummary_httpUrlSummary,
    topicRuleDestinationSummary_lastUpdatedAt,
    topicRuleDestinationSummary_arn,
    topicRuleDestinationSummary_createdAt,
    topicRuleDestinationSummary_vpcDestinationSummary,
    topicRuleDestinationSummary_statusReason,

    -- * TopicRuleListItem
    TopicRuleListItem (..),
    newTopicRuleListItem,
    topicRuleListItem_createdAt,
    topicRuleListItem_ruleDisabled,
    topicRuleListItem_ruleName,
    topicRuleListItem_ruleArn,
    topicRuleListItem_topicPattern,

    -- * TopicRulePayload
    TopicRulePayload (..),
    newTopicRulePayload,
    topicRulePayload_awsIotSqlVersion,
    topicRulePayload_errorAction,
    topicRulePayload_ruleDisabled,
    topicRulePayload_description,
    topicRulePayload_sql,
    topicRulePayload_actions,

    -- * TransferData
    TransferData (..),
    newTransferData,
    transferData_transferDate,
    transferData_acceptDate,
    transferData_transferMessage,
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
    violationEvent_violationEventType,
    violationEvent_verificationStateDescription,
    violationEvent_violationId,
    violationEvent_behavior,
    violationEvent_metricValue,
    violationEvent_securityProfileName,
    violationEvent_violationEventAdditionalInfo,
    violationEvent_violationEventTime,
    violationEvent_thingName,
    violationEvent_verificationState,

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
    vpcDestinationProperties_securityGroups,
    vpcDestinationProperties_subnetIds,
    vpcDestinationProperties_vpcId,
    vpcDestinationProperties_roleArn,

    -- * VpcDestinationSummary
    VpcDestinationSummary (..),
    newVpcDestinationSummary,
    vpcDestinationSummary_securityGroups,
    vpcDestinationSummary_subnetIds,
    vpcDestinationSummary_vpcId,
    vpcDestinationSummary_roleArn,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AbortAction
import Network.AWS.IoT.Types.AbortConfig
import Network.AWS.IoT.Types.AbortCriteria
import Network.AWS.IoT.Types.Action
import Network.AWS.IoT.Types.ActionType
import Network.AWS.IoT.Types.ActiveViolation
import Network.AWS.IoT.Types.AddThingsToThingGroupParams
import Network.AWS.IoT.Types.AggregationType
import Network.AWS.IoT.Types.AggregationTypeName
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
import Network.AWS.IoT.Types.Bucket
import Network.AWS.IoT.Types.BucketsAggregationType
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
import Network.AWS.IoT.Types.FleetMetricNameAndArn
import Network.AWS.IoT.Types.FleetMetricUnit
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
import Network.AWS.IoT.Types.JobTemplateSummary
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
import Network.AWS.IoT.Types.OpenSearchAction
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
import Network.AWS.IoT.Types.TermsAggregation
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
import Network.AWS.IoT.Types.VerificationState
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
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "IoT",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "iot",
      Core._serviceSigningName = "execute-api",
      Core._serviceVersion = "2015-05-28",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "IoT",
      Core._serviceRetry = retry
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
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | This exception occurs if you attempt to start a task with the same
-- task-id as an existing task but with a different clientRequestToken.
_TaskAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TaskAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "TaskAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | Unable to verify the CA certificate used to sign the device certificate
-- you are attempting to register. This is happens when you have registered
-- more than one CA certificate that has the same subject field and public
-- key.
_CertificateConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateConflictException =
  Core._MatchServiceError
    defaultService
    "CertificateConflictException"
    Prelude.. Core.hasStatus 409

-- | The Rule-SQL expression can\'t be parsed correctly.
_SqlParseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SqlParseException =
  Core._MatchServiceError
    defaultService
    "SqlParseException"
    Prelude.. Core.hasStatus 400

-- | The index is not ready.
_IndexNotReadyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IndexNotReadyException =
  Core._MatchServiceError
    defaultService
    "IndexNotReadyException"
    Prelude.. Core.hasStatus 400

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | You can\'t transfer the certificate because authorization policies are
-- still attached.
_TransferConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TransferConflictException =
  Core._MatchServiceError
    defaultService
    "TransferConflictException"
    Prelude.. Core.hasStatus 409

-- | The certificate operation is not allowed.
_CertificateStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateStateException =
  Core._MatchServiceError
    defaultService
    "CertificateStateException"
    Prelude.. Core.hasStatus 406

-- | The response is invalid.
_InvalidResponseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResponseException =
  Core._MatchServiceError
    defaultService
    "InvalidResponseException"
    Prelude.. Core.hasStatus 400

-- | The registration code is invalid.
_RegistrationCodeValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RegistrationCodeValidationException =
  Core._MatchServiceError
    defaultService
    "RegistrationCodeValidationException"
    Prelude.. Core.hasStatus 400

-- | The policy documentation is not valid.
_MalformedPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyException"
    Prelude.. Core.hasStatus 400

-- | You can\'t delete the resource because it is attached to one or more
-- resources.
_DeleteConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeleteConflictException =
  Core._MatchServiceError
    defaultService
    "DeleteConflictException"
    Prelude.. Core.hasStatus 409

-- | The resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The resource is not configured.
_NotConfiguredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotConfiguredException =
  Core._MatchServiceError
    defaultService
    "NotConfiguredException"
    Prelude.. Core.hasStatus 404

-- | The certificate is invalid.
_CertificateValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateValidationException =
  Core._MatchServiceError
    defaultService
    "CertificateValidationException"
    Prelude.. Core.hasStatus 400

-- | A resource with the same name already exists.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The resource registration failed.
_ResourceRegistrationFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceRegistrationFailureException =
  Core._MatchServiceError
    defaultService
    "ResourceRegistrationFailureException"
    Prelude.. Core.hasStatus 400

-- | The query is invalid.
_InvalidQueryException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidQueryException =
  Core._MatchServiceError
    defaultService
    "InvalidQueryException"
    Prelude.. Core.hasStatus 400

-- | You can\'t revert the certificate transfer because the transfer is
-- already complete.
_TransferAlreadyCompletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TransferAlreadyCompletedException =
  Core._MatchServiceError
    defaultService
    "TransferAlreadyCompletedException"
    Prelude.. Core.hasStatus 410

-- | The rate exceeds the limit.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 400

-- | The aggregation is invalid.
_InvalidAggregationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAggregationException =
  Core._MatchServiceError
    defaultService
    "InvalidAggregationException"
    Prelude.. Core.hasStatus 400

-- | A conflicting resource update exception. This exception is thrown when
-- two pending updates cause a conflict.
_ConflictingResourceUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictingResourceUpdateException =
  Core._MatchServiceError
    defaultService
    "ConflictingResourceUpdateException"
    Prelude.. Core.hasStatus 409

-- | An unexpected error has occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | The number of policy versions exceeds the limit.
_VersionsLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_VersionsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "VersionsLimitExceededException"
    Prelude.. Core.hasStatus 409

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | An unexpected error has occurred.
_InternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | An exception thrown when the version of an entity specified with the
-- @expectedVersion@ parameter does not match the latest version in the
-- system.
_VersionConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_VersionConflictException =
  Core._MatchServiceError
    defaultService
    "VersionConflictException"
    Prelude.. Core.hasStatus 409

-- | You are not authorized to perform this operation.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | An attempt was made to change to an invalid state, for example by
-- deleting a job or a job execution which is \"IN_PROGRESS\" without
-- setting the @force@ parameter.
_InvalidStateTransitionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateTransitionException =
  Core._MatchServiceError
    defaultService
    "InvalidStateTransitionException"
    Prelude.. Core.hasStatus 409

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A limit has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 410
