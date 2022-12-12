{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CertificateConflictException,
    _CertificateStateException,
    _CertificateValidationException,
    _ConflictException,
    _ConflictingResourceUpdateException,
    _DeleteConflictException,
    _IndexNotReadyException,
    _InternalException,
    _InternalFailureException,
    _InternalServerException,
    _InvalidAggregationException,
    _InvalidQueryException,
    _InvalidRequestException,
    _InvalidResponseException,
    _InvalidStateTransitionException,
    _LimitExceededException,
    _MalformedPolicyException,
    _NotConfiguredException,
    _RegistrationCodeValidationException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _ResourceRegistrationFailureException,
    _ServiceUnavailableException,
    _SqlParseException,
    _TaskAlreadyExistsException,
    _ThrottlingException,
    _TransferAlreadyCompletedException,
    _TransferConflictException,
    _UnauthorizedException,
    _VersionConflictException,
    _VersionsLimitExceededException,

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

    -- * DeviceDefenderIndexingMode
    DeviceDefenderIndexingMode (..),

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

    -- * JobEndBehavior
    JobEndBehavior (..),

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

    -- * NamedShadowIndexingMode
    NamedShadowIndexingMode (..),

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

    -- * RetryableFailureType
    RetryableFailureType (..),

    -- * ServerCertificateStatus
    ServerCertificateStatus (..),

    -- * ServiceType
    ServiceType (..),

    -- * TargetSelection
    TargetSelection (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * TemplateType
    TemplateType (..),

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
    action_cloudwatchAlarm,
    action_cloudwatchLogs,
    action_cloudwatchMetric,
    action_dynamoDB,
    action_dynamoDBv2,
    action_elasticsearch,
    action_firehose,
    action_http,
    action_iotAnalytics,
    action_iotEvents,
    action_iotSiteWise,
    action_kafka,
    action_kinesis,
    action_lambda,
    action_location,
    action_openSearch,
    action_republish,
    action_s3,
    action_salesforce,
    action_sns,
    action_sqs,
    action_stepFunctions,
    action_timestream,

    -- * ActiveViolation
    ActiveViolation (..),
    newActiveViolation,
    activeViolation_behavior,
    activeViolation_lastViolationTime,
    activeViolation_lastViolationValue,
    activeViolation_securityProfileName,
    activeViolation_thingName,
    activeViolation_verificationState,
    activeViolation_verificationStateDescription,
    activeViolation_violationEventAdditionalInfo,
    activeViolation_violationId,
    activeViolation_violationStartTime,

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
    assetPropertyVariant_booleanValue,
    assetPropertyVariant_doubleValue,
    assetPropertyVariant_integerValue,
    assetPropertyVariant_stringValue,

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
    auditCheckDetails_checkCompliant,
    auditCheckDetails_checkRunStatus,
    auditCheckDetails_errorCode,
    auditCheckDetails_message,
    auditCheckDetails_nonCompliantResourcesCount,
    auditCheckDetails_suppressedNonCompliantResourcesCount,
    auditCheckDetails_totalResourcesCount,

    -- * AuditFinding
    AuditFinding (..),
    newAuditFinding,
    auditFinding_checkName,
    auditFinding_findingId,
    auditFinding_findingTime,
    auditFinding_isSuppressed,
    auditFinding_nonCompliantResource,
    auditFinding_reasonForNonCompliance,
    auditFinding_reasonForNonComplianceCode,
    auditFinding_relatedResources,
    auditFinding_severity,
    auditFinding_taskId,
    auditFinding_taskStartTime,

    -- * AuditMitigationActionExecutionMetadata
    AuditMitigationActionExecutionMetadata (..),
    newAuditMitigationActionExecutionMetadata,
    auditMitigationActionExecutionMetadata_actionId,
    auditMitigationActionExecutionMetadata_actionName,
    auditMitigationActionExecutionMetadata_endTime,
    auditMitigationActionExecutionMetadata_errorCode,
    auditMitigationActionExecutionMetadata_findingId,
    auditMitigationActionExecutionMetadata_message,
    auditMitigationActionExecutionMetadata_startTime,
    auditMitigationActionExecutionMetadata_status,
    auditMitigationActionExecutionMetadata_taskId,

    -- * AuditMitigationActionsTaskMetadata
    AuditMitigationActionsTaskMetadata (..),
    newAuditMitigationActionsTaskMetadata,
    auditMitigationActionsTaskMetadata_startTime,
    auditMitigationActionsTaskMetadata_taskId,
    auditMitigationActionsTaskMetadata_taskStatus,

    -- * AuditMitigationActionsTaskTarget
    AuditMitigationActionsTaskTarget (..),
    newAuditMitigationActionsTaskTarget,
    auditMitigationActionsTaskTarget_auditCheckToReasonCodeFilter,
    auditMitigationActionsTaskTarget_auditTaskId,
    auditMitigationActionsTaskTarget_findingIds,

    -- * AuditNotificationTarget
    AuditNotificationTarget (..),
    newAuditNotificationTarget,
    auditNotificationTarget_enabled,
    auditNotificationTarget_roleArn,
    auditNotificationTarget_targetArn,

    -- * AuditSuppression
    AuditSuppression (..),
    newAuditSuppression,
    auditSuppression_description,
    auditSuppression_expirationDate,
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
    authResult_allowed,
    authResult_authDecision,
    authResult_authInfo,
    authResult_denied,
    authResult_missingContextValues,

    -- * AuthorizerConfig
    AuthorizerConfig (..),
    newAuthorizerConfig,
    authorizerConfig_allowAuthorizerOverride,
    authorizerConfig_defaultAuthorizerName,

    -- * AuthorizerDescription
    AuthorizerDescription (..),
    newAuthorizerDescription,
    authorizerDescription_authorizerArn,
    authorizerDescription_authorizerFunctionArn,
    authorizerDescription_authorizerName,
    authorizerDescription_creationDate,
    authorizerDescription_enableCachingForHttp,
    authorizerDescription_lastModifiedDate,
    authorizerDescription_signingDisabled,
    authorizerDescription_status,
    authorizerDescription_tokenKeyName,
    authorizerDescription_tokenSigningPublicKeys,

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
    behavior_criteria,
    behavior_metric,
    behavior_metricDimension,
    behavior_suppressAlerts,
    behavior_name,

    -- * BehaviorCriteria
    BehaviorCriteria (..),
    newBehaviorCriteria,
    behaviorCriteria_comparisonOperator,
    behaviorCriteria_consecutiveDatapointsToAlarm,
    behaviorCriteria_consecutiveDatapointsToClear,
    behaviorCriteria_durationSeconds,
    behaviorCriteria_mlDetectionConfig,
    behaviorCriteria_statisticalThreshold,
    behaviorCriteria_value,

    -- * BehaviorModelTrainingSummary
    BehaviorModelTrainingSummary (..),
    newBehaviorModelTrainingSummary,
    behaviorModelTrainingSummary_behaviorName,
    behaviorModelTrainingSummary_datapointsCollectionPercentage,
    behaviorModelTrainingSummary_lastModelRefreshDate,
    behaviorModelTrainingSummary_modelStatus,
    behaviorModelTrainingSummary_securityProfileName,
    behaviorModelTrainingSummary_trainingDataCollectionStartDate,

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
    bucket_count,
    bucket_keyValue,

    -- * BucketsAggregationType
    BucketsAggregationType (..),
    newBucketsAggregationType,
    bucketsAggregationType_termsAggregation,

    -- * CACertificate
    CACertificate (..),
    newCACertificate,
    cACertificate_certificateArn,
    cACertificate_certificateId,
    cACertificate_creationDate,
    cACertificate_status,

    -- * CACertificateDescription
    CACertificateDescription (..),
    newCACertificateDescription,
    cACertificateDescription_autoRegistrationStatus,
    cACertificateDescription_certificateArn,
    cACertificateDescription_certificateId,
    cACertificateDescription_certificateMode,
    cACertificateDescription_certificatePem,
    cACertificateDescription_creationDate,
    cACertificateDescription_customerVersion,
    cACertificateDescription_generationId,
    cACertificateDescription_lastModifiedDate,
    cACertificateDescription_ownedBy,
    cACertificateDescription_status,
    cACertificateDescription_validity,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateArn,
    certificate_certificateId,
    certificate_certificateMode,
    certificate_creationDate,
    certificate_status,

    -- * CertificateDescription
    CertificateDescription (..),
    newCertificateDescription,
    certificateDescription_caCertificateId,
    certificateDescription_certificateArn,
    certificateDescription_certificateId,
    certificateDescription_certificateMode,
    certificateDescription_certificatePem,
    certificateDescription_creationDate,
    certificateDescription_customerVersion,
    certificateDescription_generationId,
    certificateDescription_lastModifiedDate,
    certificateDescription_ownedBy,
    certificateDescription_previousOwnedBy,
    certificateDescription_status,
    certificateDescription_transferData,
    certificateDescription_validity,

    -- * CertificateValidity
    CertificateValidity (..),
    newCertificateValidity,
    certificateValidity_notAfter,
    certificateValidity_notBefore,

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
    codeSigning_awsSignerJobId,
    codeSigning_customCodeSigning,
    codeSigning_startSigningJobParameter,

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
    customCodeSigning_certificateChain,
    customCodeSigning_hashAlgorithm,
    customCodeSigning_signature,
    customCodeSigning_signatureAlgorithm,

    -- * Denied
    Denied (..),
    newDenied,
    denied_explicitDeny,
    denied_implicitDeny,

    -- * Destination
    Destination (..),
    newDestination,
    destination_s3Destination,

    -- * DetectMitigationActionExecution
    DetectMitigationActionExecution (..),
    newDetectMitigationActionExecution,
    detectMitigationActionExecution_actionName,
    detectMitigationActionExecution_errorCode,
    detectMitigationActionExecution_executionEndDate,
    detectMitigationActionExecution_executionStartDate,
    detectMitigationActionExecution_message,
    detectMitigationActionExecution_status,
    detectMitigationActionExecution_taskId,
    detectMitigationActionExecution_thingName,
    detectMitigationActionExecution_violationId,

    -- * DetectMitigationActionsTaskStatistics
    DetectMitigationActionsTaskStatistics (..),
    newDetectMitigationActionsTaskStatistics,
    detectMitigationActionsTaskStatistics_actionsExecuted,
    detectMitigationActionsTaskStatistics_actionsFailed,
    detectMitigationActionsTaskStatistics_actionsSkipped,

    -- * DetectMitigationActionsTaskSummary
    DetectMitigationActionsTaskSummary (..),
    newDetectMitigationActionsTaskSummary,
    detectMitigationActionsTaskSummary_actionsDefinition,
    detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded,
    detectMitigationActionsTaskSummary_suppressedAlertsIncluded,
    detectMitigationActionsTaskSummary_target,
    detectMitigationActionsTaskSummary_taskEndTime,
    detectMitigationActionsTaskSummary_taskId,
    detectMitigationActionsTaskSummary_taskStartTime,
    detectMitigationActionsTaskSummary_taskStatistics,
    detectMitigationActionsTaskSummary_taskStatus,
    detectMitigationActionsTaskSummary_violationEventOccurrenceRange,

    -- * DetectMitigationActionsTaskTarget
    DetectMitigationActionsTaskTarget (..),
    newDetectMitigationActionsTaskTarget,
    detectMitigationActionsTaskTarget_behaviorName,
    detectMitigationActionsTaskTarget_securityProfileName,
    detectMitigationActionsTaskTarget_violationIds,

    -- * DocumentParameter
    DocumentParameter (..),
    newDocumentParameter,
    documentParameter_description,
    documentParameter_example,
    documentParameter_key,
    documentParameter_optional,
    documentParameter_regex,

    -- * DomainConfigurationSummary
    DomainConfigurationSummary (..),
    newDomainConfigurationSummary,
    domainConfigurationSummary_domainConfigurationArn,
    domainConfigurationSummary_domainConfigurationName,
    domainConfigurationSummary_serviceType,

    -- * DynamoDBAction
    DynamoDBAction (..),
    newDynamoDBAction,
    dynamoDBAction_hashKeyType,
    dynamoDBAction_operation,
    dynamoDBAction_payloadField,
    dynamoDBAction_rangeKeyField,
    dynamoDBAction_rangeKeyType,
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
    effectivePolicy_policyArn,
    effectivePolicy_policyDocument,
    effectivePolicy_policyName,

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
    fileLocation_s3Location,
    fileLocation_stream,

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
    fleetMetricNameAndArn_metricArn,
    fleetMetricNameAndArn_metricName,

    -- * GroupNameAndArn
    GroupNameAndArn (..),
    newGroupNameAndArn,
    groupNameAndArn_groupArn,
    groupNameAndArn_groupName,

    -- * HttpAction
    HttpAction (..),
    newHttpAction,
    httpAction_auth,
    httpAction_confirmationUrl,
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

    -- * IndexingFilter
    IndexingFilter (..),
    newIndexingFilter,
    indexingFilter_namedShadowNames,

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

    -- * IssuerCertificateIdentifier
    IssuerCertificateIdentifier (..),
    newIssuerCertificateIdentifier,
    issuerCertificateIdentifier_issuerCertificateSerialNumber,
    issuerCertificateIdentifier_issuerCertificateSubject,
    issuerCertificateIdentifier_issuerId,

    -- * Job
    Job (..),
    newJob,
    job_abortConfig,
    job_comment,
    job_completedAt,
    job_createdAt,
    job_description,
    job_documentParameters,
    job_forceCanceled,
    job_isConcurrent,
    job_jobArn,
    job_jobExecutionsRetryConfig,
    job_jobExecutionsRolloutConfig,
    job_jobId,
    job_jobProcessDetails,
    job_jobTemplateArn,
    job_lastUpdatedAt,
    job_namespaceId,
    job_presignedUrlConfig,
    job_reasonCode,
    job_schedulingConfig,
    job_status,
    job_targetSelection,
    job_targets,
    job_timeoutConfig,

    -- * JobExecution
    JobExecution (..),
    newJobExecution,
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_executionNumber,
    jobExecution_forceCanceled,
    jobExecution_jobId,
    jobExecution_lastUpdatedAt,
    jobExecution_queuedAt,
    jobExecution_startedAt,
    jobExecution_status,
    jobExecution_statusDetails,
    jobExecution_thingArn,
    jobExecution_versionNumber,

    -- * JobExecutionStatusDetails
    JobExecutionStatusDetails (..),
    newJobExecutionStatusDetails,
    jobExecutionStatusDetails_detailsMap,

    -- * JobExecutionSummary
    JobExecutionSummary (..),
    newJobExecutionSummary,
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_lastUpdatedAt,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_retryAttempt,
    jobExecutionSummary_startedAt,
    jobExecutionSummary_status,

    -- * JobExecutionSummaryForJob
    JobExecutionSummaryForJob (..),
    newJobExecutionSummaryForJob,
    jobExecutionSummaryForJob_jobExecutionSummary,
    jobExecutionSummaryForJob_thingArn,

    -- * JobExecutionSummaryForThing
    JobExecutionSummaryForThing (..),
    newJobExecutionSummaryForThing,
    jobExecutionSummaryForThing_jobExecutionSummary,
    jobExecutionSummaryForThing_jobId,

    -- * JobExecutionsRetryConfig
    JobExecutionsRetryConfig (..),
    newJobExecutionsRetryConfig,
    jobExecutionsRetryConfig_criteriaList,

    -- * JobExecutionsRolloutConfig
    JobExecutionsRolloutConfig (..),
    newJobExecutionsRolloutConfig,
    jobExecutionsRolloutConfig_exponentialRate,
    jobExecutionsRolloutConfig_maximumPerMinute,

    -- * JobProcessDetails
    JobProcessDetails (..),
    newJobProcessDetails,
    jobProcessDetails_numberOfCanceledThings,
    jobProcessDetails_numberOfFailedThings,
    jobProcessDetails_numberOfInProgressThings,
    jobProcessDetails_numberOfQueuedThings,
    jobProcessDetails_numberOfRejectedThings,
    jobProcessDetails_numberOfRemovedThings,
    jobProcessDetails_numberOfSucceededThings,
    jobProcessDetails_numberOfTimedOutThings,
    jobProcessDetails_processingTargets,

    -- * JobSummary
    JobSummary (..),
    newJobSummary,
    jobSummary_completedAt,
    jobSummary_createdAt,
    jobSummary_isConcurrent,
    jobSummary_jobArn,
    jobSummary_jobId,
    jobSummary_lastUpdatedAt,
    jobSummary_status,
    jobSummary_targetSelection,
    jobSummary_thingGroupId,

    -- * JobTemplateSummary
    JobTemplateSummary (..),
    newJobTemplateSummary,
    jobTemplateSummary_createdAt,
    jobTemplateSummary_description,
    jobTemplateSummary_jobTemplateArn,
    jobTemplateSummary_jobTemplateId,

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

    -- * LocationAction
    LocationAction (..),
    newLocationAction,
    locationAction_timestamp,
    locationAction_roleArn,
    locationAction_trackerName,
    locationAction_deviceId,
    locationAction_latitude,
    locationAction_longitude,

    -- * LocationTimestamp
    LocationTimestamp (..),
    newLocationTimestamp,
    locationTimestamp_unit,
    locationTimestamp_value,

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

    -- * ManagedJobTemplateSummary
    ManagedJobTemplateSummary (..),
    newManagedJobTemplateSummary,
    managedJobTemplateSummary_description,
    managedJobTemplateSummary_environments,
    managedJobTemplateSummary_templateArn,
    managedJobTemplateSummary_templateName,
    managedJobTemplateSummary_templateVersion,

    -- * MetricDatum
    MetricDatum (..),
    newMetricDatum,
    metricDatum_timestamp,
    metricDatum_value,

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
    metricValue_number,
    metricValue_numbers,
    metricValue_ports,
    metricValue_strings,

    -- * MitigationAction
    MitigationAction (..),
    newMitigationAction,
    mitigationAction_actionParams,
    mitigationAction_id,
    mitigationAction_name,
    mitigationAction_roleArn,

    -- * MitigationActionIdentifier
    MitigationActionIdentifier (..),
    newMitigationActionIdentifier,
    mitigationActionIdentifier_actionArn,
    mitigationActionIdentifier_actionName,
    mitigationActionIdentifier_creationDate,

    -- * MitigationActionParams
    MitigationActionParams (..),
    newMitigationActionParams,
    mitigationActionParams_addThingsToThingGroupParams,
    mitigationActionParams_enableIoTLoggingParams,
    mitigationActionParams_publishFindingToSnsParams,
    mitigationActionParams_replaceDefaultPolicyVersionParams,
    mitigationActionParams_updateCACertificateParams,
    mitigationActionParams_updateDeviceCertificateParams,

    -- * MqttContext
    MqttContext (..),
    newMqttContext,
    mqttContext_clientId,
    mqttContext_password,
    mqttContext_username,

    -- * MqttHeaders
    MqttHeaders (..),
    newMqttHeaders,
    mqttHeaders_contentType,
    mqttHeaders_correlationData,
    mqttHeaders_messageExpiry,
    mqttHeaders_payloadFormatIndicator,
    mqttHeaders_responseTopic,
    mqttHeaders_userProperties,

    -- * NonCompliantResource
    NonCompliantResource (..),
    newNonCompliantResource,
    nonCompliantResource_additionalInfo,
    nonCompliantResource_resourceIdentifier,
    nonCompliantResource_resourceType,

    -- * OTAUpdateFile
    OTAUpdateFile (..),
    newOTAUpdateFile,
    oTAUpdateFile_attributes,
    oTAUpdateFile_codeSigning,
    oTAUpdateFile_fileLocation,
    oTAUpdateFile_fileName,
    oTAUpdateFile_fileType,
    oTAUpdateFile_fileVersion,

    -- * OTAUpdateInfo
    OTAUpdateInfo (..),
    newOTAUpdateInfo,
    oTAUpdateInfo_additionalParameters,
    oTAUpdateInfo_awsIotJobArn,
    oTAUpdateInfo_awsIotJobId,
    oTAUpdateInfo_awsJobExecutionsRolloutConfig,
    oTAUpdateInfo_awsJobPresignedUrlConfig,
    oTAUpdateInfo_creationDate,
    oTAUpdateInfo_description,
    oTAUpdateInfo_errorInfo,
    oTAUpdateInfo_lastModifiedDate,
    oTAUpdateInfo_otaUpdateArn,
    oTAUpdateInfo_otaUpdateFiles,
    oTAUpdateInfo_otaUpdateId,
    oTAUpdateInfo_otaUpdateStatus,
    oTAUpdateInfo_protocols,
    oTAUpdateInfo_targetSelection,
    oTAUpdateInfo_targets,

    -- * OTAUpdateSummary
    OTAUpdateSummary (..),
    newOTAUpdateSummary,
    oTAUpdateSummary_creationDate,
    oTAUpdateSummary_otaUpdateArn,
    oTAUpdateSummary_otaUpdateId,

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
    outgoingCertificate_certificateArn,
    outgoingCertificate_certificateId,
    outgoingCertificate_creationDate,
    outgoingCertificate_transferDate,
    outgoingCertificate_transferMessage,
    outgoingCertificate_transferredTo,

    -- * PercentPair
    PercentPair (..),
    newPercentPair,
    percentPair_percent,
    percentPair_value,

    -- * Policy
    Policy (..),
    newPolicy,
    policy_policyArn,
    policy_policyName,

    -- * PolicyVersion
    PolicyVersion (..),
    newPolicyVersion,
    policyVersion_createDate,
    policyVersion_isDefaultVersion,
    policyVersion_versionId,

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
    provisioningTemplateSummary_creationDate,
    provisioningTemplateSummary_description,
    provisioningTemplateSummary_enabled,
    provisioningTemplateSummary_lastModifiedDate,
    provisioningTemplateSummary_templateArn,
    provisioningTemplateSummary_templateName,
    provisioningTemplateSummary_type,

    -- * ProvisioningTemplateVersionSummary
    ProvisioningTemplateVersionSummary (..),
    newProvisioningTemplateVersionSummary,
    provisioningTemplateVersionSummary_creationDate,
    provisioningTemplateVersionSummary_isDefaultVersion,
    provisioningTemplateVersionSummary_versionId,

    -- * PublishFindingToSnsParams
    PublishFindingToSnsParams (..),
    newPublishFindingToSnsParams,
    publishFindingToSnsParams_topicArn,

    -- * PutAssetPropertyValueEntry
    PutAssetPropertyValueEntry (..),
    newPutAssetPropertyValueEntry,
    putAssetPropertyValueEntry_assetId,
    putAssetPropertyValueEntry_entryId,
    putAssetPropertyValueEntry_propertyAlias,
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
    registrationConfig_templateName,

    -- * RelatedResource
    RelatedResource (..),
    newRelatedResource,
    relatedResource_additionalInfo,
    relatedResource_resourceIdentifier,
    relatedResource_resourceType,

    -- * ReplaceDefaultPolicyVersionParams
    ReplaceDefaultPolicyVersionParams (..),
    newReplaceDefaultPolicyVersionParams,
    replaceDefaultPolicyVersionParams_templateName,

    -- * RepublishAction
    RepublishAction (..),
    newRepublishAction,
    republishAction_headers,
    republishAction_qos,
    republishAction_roleArn,
    republishAction_topic,

    -- * ResourceIdentifier
    ResourceIdentifier (..),
    newResourceIdentifier,
    resourceIdentifier_account,
    resourceIdentifier_caCertificateId,
    resourceIdentifier_clientId,
    resourceIdentifier_cognitoIdentityPoolId,
    resourceIdentifier_deviceCertificateArn,
    resourceIdentifier_deviceCertificateId,
    resourceIdentifier_iamRoleArn,
    resourceIdentifier_issuerCertificateIdentifier,
    resourceIdentifier_policyVersionIdentifier,
    resourceIdentifier_roleAliasArn,

    -- * RetryCriteria
    RetryCriteria (..),
    newRetryCriteria,
    retryCriteria_failureType,
    retryCriteria_numberOfRetries,

    -- * RoleAliasDescription
    RoleAliasDescription (..),
    newRoleAliasDescription,
    roleAliasDescription_creationDate,
    roleAliasDescription_credentialDurationSeconds,
    roleAliasDescription_lastModifiedDate,
    roleAliasDescription_owner,
    roleAliasDescription_roleAlias,
    roleAliasDescription_roleAliasArn,
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
    s3Destination_bucket,
    s3Destination_prefix,

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
    scheduledAuditMetadata_dayOfMonth,
    scheduledAuditMetadata_dayOfWeek,
    scheduledAuditMetadata_frequency,
    scheduledAuditMetadata_scheduledAuditArn,
    scheduledAuditMetadata_scheduledAuditName,

    -- * SchedulingConfig
    SchedulingConfig (..),
    newSchedulingConfig,
    schedulingConfig_endBehavior,
    schedulingConfig_endTime,
    schedulingConfig_startTime,

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
    serverCertificateSummary_serverCertificateArn,
    serverCertificateSummary_serverCertificateStatus,
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
    signingProfileParameter_certificateArn,
    signingProfileParameter_certificatePathOnDevice,
    signingProfileParameter_platform,

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
    statistics_average,
    statistics_count,
    statistics_maximum,
    statistics_minimum,
    statistics_stdDeviation,
    statistics_sum,
    statistics_sumOfSquares,
    statistics_variance,

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
    streamFile_fileId,
    streamFile_s3Location,

    -- * StreamInfo
    StreamInfo (..),
    newStreamInfo,
    streamInfo_createdAt,
    streamInfo_description,
    streamInfo_files,
    streamInfo_lastUpdatedAt,
    streamInfo_roleArn,
    streamInfo_streamArn,
    streamInfo_streamId,
    streamInfo_streamVersion,

    -- * StreamSummary
    StreamSummary (..),
    newStreamSummary,
    streamSummary_description,
    streamSummary_streamArn,
    streamSummary_streamId,
    streamSummary_streamVersion,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TaskStatistics
    TaskStatistics (..),
    newTaskStatistics,
    taskStatistics_canceledChecks,
    taskStatistics_compliantChecks,
    taskStatistics_failedChecks,
    taskStatistics_inProgressChecks,
    taskStatistics_nonCompliantChecks,
    taskStatistics_totalChecks,
    taskStatistics_waitingForDataCollectionChecks,

    -- * TaskStatisticsForAuditCheck
    TaskStatisticsForAuditCheck (..),
    newTaskStatisticsForAuditCheck,
    taskStatisticsForAuditCheck_canceledFindingsCount,
    taskStatisticsForAuditCheck_failedFindingsCount,
    taskStatisticsForAuditCheck_skippedFindingsCount,
    taskStatisticsForAuditCheck_succeededFindingsCount,
    taskStatisticsForAuditCheck_totalFindingsCount,

    -- * TermsAggregation
    TermsAggregation (..),
    newTermsAggregation,
    termsAggregation_maxBuckets,

    -- * ThingAttribute
    ThingAttribute (..),
    newThingAttribute,
    thingAttribute_attributes,
    thingAttribute_thingArn,
    thingAttribute_thingName,
    thingAttribute_thingTypeName,
    thingAttribute_version,

    -- * ThingConnectivity
    ThingConnectivity (..),
    newThingConnectivity,
    thingConnectivity_connected,
    thingConnectivity_disconnectReason,
    thingConnectivity_timestamp,

    -- * ThingDocument
    ThingDocument (..),
    newThingDocument,
    thingDocument_attributes,
    thingDocument_connectivity,
    thingDocument_deviceDefender,
    thingDocument_shadow,
    thingDocument_thingGroupNames,
    thingDocument_thingId,
    thingDocument_thingName,
    thingDocument_thingTypeName,

    -- * ThingGroupDocument
    ThingGroupDocument (..),
    newThingGroupDocument,
    thingGroupDocument_attributes,
    thingGroupDocument_parentGroupNames,
    thingGroupDocument_thingGroupDescription,
    thingGroupDocument_thingGroupId,
    thingGroupDocument_thingGroupName,

    -- * ThingGroupIndexingConfiguration
    ThingGroupIndexingConfiguration (..),
    newThingGroupIndexingConfiguration,
    thingGroupIndexingConfiguration_customFields,
    thingGroupIndexingConfiguration_managedFields,
    thingGroupIndexingConfiguration_thingGroupIndexingMode,

    -- * ThingGroupMetadata
    ThingGroupMetadata (..),
    newThingGroupMetadata,
    thingGroupMetadata_creationDate,
    thingGroupMetadata_parentGroupName,
    thingGroupMetadata_rootToParentThingGroups,

    -- * ThingGroupProperties
    ThingGroupProperties (..),
    newThingGroupProperties,
    thingGroupProperties_attributePayload,
    thingGroupProperties_thingGroupDescription,

    -- * ThingIndexingConfiguration
    ThingIndexingConfiguration (..),
    newThingIndexingConfiguration,
    thingIndexingConfiguration_customFields,
    thingIndexingConfiguration_deviceDefenderIndexingMode,
    thingIndexingConfiguration_filter,
    thingIndexingConfiguration_managedFields,
    thingIndexingConfiguration_namedShadowIndexingMode,
    thingIndexingConfiguration_thingConnectivityIndexingMode,
    thingIndexingConfiguration_thingIndexingMode,

    -- * ThingTypeDefinition
    ThingTypeDefinition (..),
    newThingTypeDefinition,
    thingTypeDefinition_thingTypeArn,
    thingTypeDefinition_thingTypeMetadata,
    thingTypeDefinition_thingTypeName,
    thingTypeDefinition_thingTypeProperties,

    -- * ThingTypeMetadata
    ThingTypeMetadata (..),
    newThingTypeMetadata,
    thingTypeMetadata_creationDate,
    thingTypeMetadata_deprecated,
    thingTypeMetadata_deprecationDate,

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
    topicRule_actions,
    topicRule_awsIotSqlVersion,
    topicRule_createdAt,
    topicRule_description,
    topicRule_errorAction,
    topicRule_ruleDisabled,
    topicRule_ruleName,
    topicRule_sql,

    -- * TopicRuleDestination
    TopicRuleDestination (..),
    newTopicRuleDestination,
    topicRuleDestination_arn,
    topicRuleDestination_createdAt,
    topicRuleDestination_httpUrlProperties,
    topicRuleDestination_lastUpdatedAt,
    topicRuleDestination_status,
    topicRuleDestination_statusReason,
    topicRuleDestination_vpcProperties,

    -- * TopicRuleDestinationConfiguration
    TopicRuleDestinationConfiguration (..),
    newTopicRuleDestinationConfiguration,
    topicRuleDestinationConfiguration_httpUrlConfiguration,
    topicRuleDestinationConfiguration_vpcConfiguration,

    -- * TopicRuleDestinationSummary
    TopicRuleDestinationSummary (..),
    newTopicRuleDestinationSummary,
    topicRuleDestinationSummary_arn,
    topicRuleDestinationSummary_createdAt,
    topicRuleDestinationSummary_httpUrlSummary,
    topicRuleDestinationSummary_lastUpdatedAt,
    topicRuleDestinationSummary_status,
    topicRuleDestinationSummary_statusReason,
    topicRuleDestinationSummary_vpcDestinationSummary,

    -- * TopicRuleListItem
    TopicRuleListItem (..),
    newTopicRuleListItem,
    topicRuleListItem_createdAt,
    topicRuleListItem_ruleArn,
    topicRuleListItem_ruleDisabled,
    topicRuleListItem_ruleName,
    topicRuleListItem_topicPattern,

    -- * TopicRulePayload
    TopicRulePayload (..),
    newTopicRulePayload,
    topicRulePayload_awsIotSqlVersion,
    topicRulePayload_description,
    topicRulePayload_errorAction,
    topicRulePayload_ruleDisabled,
    topicRulePayload_sql,
    topicRulePayload_actions,

    -- * TransferData
    TransferData (..),
    newTransferData,
    transferData_acceptDate,
    transferData_rejectDate,
    transferData_rejectReason,
    transferData_transferDate,
    transferData_transferMessage,

    -- * UpdateCACertificateParams
    UpdateCACertificateParams (..),
    newUpdateCACertificateParams,
    updateCACertificateParams_action,

    -- * UpdateDeviceCertificateParams
    UpdateDeviceCertificateParams (..),
    newUpdateDeviceCertificateParams,
    updateDeviceCertificateParams_action,

    -- * UserProperty
    UserProperty (..),
    newUserProperty,
    userProperty_key,
    userProperty_value,

    -- * ValidationError
    ValidationError (..),
    newValidationError,
    validationError_errorMessage,

    -- * ViolationEvent
    ViolationEvent (..),
    newViolationEvent,
    violationEvent_behavior,
    violationEvent_metricValue,
    violationEvent_securityProfileName,
    violationEvent_thingName,
    violationEvent_verificationState,
    violationEvent_verificationStateDescription,
    violationEvent_violationEventAdditionalInfo,
    violationEvent_violationEventTime,
    violationEvent_violationEventType,
    violationEvent_violationId,

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
    vpcDestinationProperties_securityGroups,
    vpcDestinationProperties_subnetIds,
    vpcDestinationProperties_vpcId,

    -- * VpcDestinationSummary
    VpcDestinationSummary (..),
    newVpcDestinationSummary,
    vpcDestinationSummary_roleArn,
    vpcDestinationSummary_securityGroups,
    vpcDestinationSummary_subnetIds,
    vpcDestinationSummary_vpcId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.AbortAction
import Amazonka.IoT.Types.AbortConfig
import Amazonka.IoT.Types.AbortCriteria
import Amazonka.IoT.Types.Action
import Amazonka.IoT.Types.ActionType
import Amazonka.IoT.Types.ActiveViolation
import Amazonka.IoT.Types.AddThingsToThingGroupParams
import Amazonka.IoT.Types.AggregationType
import Amazonka.IoT.Types.AggregationTypeName
import Amazonka.IoT.Types.AlertTarget
import Amazonka.IoT.Types.AlertTargetType
import Amazonka.IoT.Types.Allowed
import Amazonka.IoT.Types.AssetPropertyTimestamp
import Amazonka.IoT.Types.AssetPropertyValue
import Amazonka.IoT.Types.AssetPropertyVariant
import Amazonka.IoT.Types.AttributePayload
import Amazonka.IoT.Types.AuditCheckConfiguration
import Amazonka.IoT.Types.AuditCheckDetails
import Amazonka.IoT.Types.AuditCheckRunStatus
import Amazonka.IoT.Types.AuditFinding
import Amazonka.IoT.Types.AuditFindingSeverity
import Amazonka.IoT.Types.AuditFrequency
import Amazonka.IoT.Types.AuditMitigationActionExecutionMetadata
import Amazonka.IoT.Types.AuditMitigationActionsExecutionStatus
import Amazonka.IoT.Types.AuditMitigationActionsTaskMetadata
import Amazonka.IoT.Types.AuditMitigationActionsTaskStatus
import Amazonka.IoT.Types.AuditMitigationActionsTaskTarget
import Amazonka.IoT.Types.AuditNotificationTarget
import Amazonka.IoT.Types.AuditNotificationType
import Amazonka.IoT.Types.AuditSuppression
import Amazonka.IoT.Types.AuditTaskMetadata
import Amazonka.IoT.Types.AuditTaskStatus
import Amazonka.IoT.Types.AuditTaskType
import Amazonka.IoT.Types.AuthDecision
import Amazonka.IoT.Types.AuthInfo
import Amazonka.IoT.Types.AuthResult
import Amazonka.IoT.Types.AuthorizerConfig
import Amazonka.IoT.Types.AuthorizerDescription
import Amazonka.IoT.Types.AuthorizerStatus
import Amazonka.IoT.Types.AuthorizerSummary
import Amazonka.IoT.Types.AutoRegistrationStatus
import Amazonka.IoT.Types.AwsJobAbortConfig
import Amazonka.IoT.Types.AwsJobAbortCriteria
import Amazonka.IoT.Types.AwsJobAbortCriteriaAbortAction
import Amazonka.IoT.Types.AwsJobAbortCriteriaFailureType
import Amazonka.IoT.Types.AwsJobExecutionsRolloutConfig
import Amazonka.IoT.Types.AwsJobExponentialRolloutRate
import Amazonka.IoT.Types.AwsJobPresignedUrlConfig
import Amazonka.IoT.Types.AwsJobRateIncreaseCriteria
import Amazonka.IoT.Types.AwsJobTimeoutConfig
import Amazonka.IoT.Types.Behavior
import Amazonka.IoT.Types.BehaviorCriteria
import Amazonka.IoT.Types.BehaviorCriteriaType
import Amazonka.IoT.Types.BehaviorModelTrainingSummary
import Amazonka.IoT.Types.BillingGroupMetadata
import Amazonka.IoT.Types.BillingGroupProperties
import Amazonka.IoT.Types.Bucket
import Amazonka.IoT.Types.BucketsAggregationType
import Amazonka.IoT.Types.CACertificate
import Amazonka.IoT.Types.CACertificateDescription
import Amazonka.IoT.Types.CACertificateStatus
import Amazonka.IoT.Types.CACertificateUpdateAction
import Amazonka.IoT.Types.CannedAccessControlList
import Amazonka.IoT.Types.Certificate
import Amazonka.IoT.Types.CertificateDescription
import Amazonka.IoT.Types.CertificateMode
import Amazonka.IoT.Types.CertificateStatus
import Amazonka.IoT.Types.CertificateValidity
import Amazonka.IoT.Types.CloudwatchAlarmAction
import Amazonka.IoT.Types.CloudwatchLogsAction
import Amazonka.IoT.Types.CloudwatchMetricAction
import Amazonka.IoT.Types.CodeSigning
import Amazonka.IoT.Types.CodeSigningCertificateChain
import Amazonka.IoT.Types.CodeSigningSignature
import Amazonka.IoT.Types.ComparisonOperator
import Amazonka.IoT.Types.ConfidenceLevel
import Amazonka.IoT.Types.Configuration
import Amazonka.IoT.Types.CustomCodeSigning
import Amazonka.IoT.Types.CustomMetricType
import Amazonka.IoT.Types.DayOfWeek
import Amazonka.IoT.Types.Denied
import Amazonka.IoT.Types.Destination
import Amazonka.IoT.Types.DetectMitigationActionExecution
import Amazonka.IoT.Types.DetectMitigationActionExecutionStatus
import Amazonka.IoT.Types.DetectMitigationActionsTaskStatistics
import Amazonka.IoT.Types.DetectMitigationActionsTaskStatus
import Amazonka.IoT.Types.DetectMitigationActionsTaskSummary
import Amazonka.IoT.Types.DetectMitigationActionsTaskTarget
import Amazonka.IoT.Types.DeviceCertificateUpdateAction
import Amazonka.IoT.Types.DeviceDefenderIndexingMode
import Amazonka.IoT.Types.DimensionType
import Amazonka.IoT.Types.DimensionValueOperator
import Amazonka.IoT.Types.DocumentParameter
import Amazonka.IoT.Types.DomainConfigurationStatus
import Amazonka.IoT.Types.DomainConfigurationSummary
import Amazonka.IoT.Types.DomainType
import Amazonka.IoT.Types.DynamicGroupStatus
import Amazonka.IoT.Types.DynamoDBAction
import Amazonka.IoT.Types.DynamoDBv2Action
import Amazonka.IoT.Types.DynamoKeyType
import Amazonka.IoT.Types.EffectivePolicy
import Amazonka.IoT.Types.ElasticsearchAction
import Amazonka.IoT.Types.EnableIoTLoggingParams
import Amazonka.IoT.Types.ErrorInfo
import Amazonka.IoT.Types.EventType
import Amazonka.IoT.Types.ExplicitDeny
import Amazonka.IoT.Types.ExponentialRolloutRate
import Amazonka.IoT.Types.Field
import Amazonka.IoT.Types.FieldType
import Amazonka.IoT.Types.FileLocation
import Amazonka.IoT.Types.FirehoseAction
import Amazonka.IoT.Types.FleetMetricNameAndArn
import Amazonka.IoT.Types.FleetMetricUnit
import Amazonka.IoT.Types.GroupNameAndArn
import Amazonka.IoT.Types.HttpAction
import Amazonka.IoT.Types.HttpActionHeader
import Amazonka.IoT.Types.HttpAuthorization
import Amazonka.IoT.Types.HttpContext
import Amazonka.IoT.Types.HttpUrlDestinationConfiguration
import Amazonka.IoT.Types.HttpUrlDestinationProperties
import Amazonka.IoT.Types.HttpUrlDestinationSummary
import Amazonka.IoT.Types.ImplicitDeny
import Amazonka.IoT.Types.IndexStatus
import Amazonka.IoT.Types.IndexingFilter
import Amazonka.IoT.Types.IotAnalyticsAction
import Amazonka.IoT.Types.IotEventsAction
import Amazonka.IoT.Types.IotSiteWiseAction
import Amazonka.IoT.Types.IssuerCertificateIdentifier
import Amazonka.IoT.Types.Job
import Amazonka.IoT.Types.JobEndBehavior
import Amazonka.IoT.Types.JobExecution
import Amazonka.IoT.Types.JobExecutionFailureType
import Amazonka.IoT.Types.JobExecutionStatus
import Amazonka.IoT.Types.JobExecutionStatusDetails
import Amazonka.IoT.Types.JobExecutionSummary
import Amazonka.IoT.Types.JobExecutionSummaryForJob
import Amazonka.IoT.Types.JobExecutionSummaryForThing
import Amazonka.IoT.Types.JobExecutionsRetryConfig
import Amazonka.IoT.Types.JobExecutionsRolloutConfig
import Amazonka.IoT.Types.JobProcessDetails
import Amazonka.IoT.Types.JobStatus
import Amazonka.IoT.Types.JobSummary
import Amazonka.IoT.Types.JobTemplateSummary
import Amazonka.IoT.Types.KafkaAction
import Amazonka.IoT.Types.KeyPair
import Amazonka.IoT.Types.KinesisAction
import Amazonka.IoT.Types.LambdaAction
import Amazonka.IoT.Types.LocationAction
import Amazonka.IoT.Types.LocationTimestamp
import Amazonka.IoT.Types.LogLevel
import Amazonka.IoT.Types.LogTarget
import Amazonka.IoT.Types.LogTargetConfiguration
import Amazonka.IoT.Types.LogTargetType
import Amazonka.IoT.Types.LoggingOptionsPayload
import Amazonka.IoT.Types.MachineLearningDetectionConfig
import Amazonka.IoT.Types.ManagedJobTemplateSummary
import Amazonka.IoT.Types.MessageFormat
import Amazonka.IoT.Types.MetricDatum
import Amazonka.IoT.Types.MetricDimension
import Amazonka.IoT.Types.MetricToRetain
import Amazonka.IoT.Types.MetricValue
import Amazonka.IoT.Types.MitigationAction
import Amazonka.IoT.Types.MitigationActionIdentifier
import Amazonka.IoT.Types.MitigationActionParams
import Amazonka.IoT.Types.MitigationActionType
import Amazonka.IoT.Types.ModelStatus
import Amazonka.IoT.Types.MqttContext
import Amazonka.IoT.Types.MqttHeaders
import Amazonka.IoT.Types.NamedShadowIndexingMode
import Amazonka.IoT.Types.NonCompliantResource
import Amazonka.IoT.Types.OTAUpdateFile
import Amazonka.IoT.Types.OTAUpdateInfo
import Amazonka.IoT.Types.OTAUpdateStatus
import Amazonka.IoT.Types.OTAUpdateSummary
import Amazonka.IoT.Types.OpenSearchAction
import Amazonka.IoT.Types.OutgoingCertificate
import Amazonka.IoT.Types.PercentPair
import Amazonka.IoT.Types.Policy
import Amazonka.IoT.Types.PolicyTemplateName
import Amazonka.IoT.Types.PolicyVersion
import Amazonka.IoT.Types.PolicyVersionIdentifier
import Amazonka.IoT.Types.PresignedUrlConfig
import Amazonka.IoT.Types.Protocol
import Amazonka.IoT.Types.ProvisioningHook
import Amazonka.IoT.Types.ProvisioningTemplateSummary
import Amazonka.IoT.Types.ProvisioningTemplateVersionSummary
import Amazonka.IoT.Types.PublishFindingToSnsParams
import Amazonka.IoT.Types.PutAssetPropertyValueEntry
import Amazonka.IoT.Types.PutItemInput
import Amazonka.IoT.Types.RateIncreaseCriteria
import Amazonka.IoT.Types.RegistrationConfig
import Amazonka.IoT.Types.RelatedResource
import Amazonka.IoT.Types.ReplaceDefaultPolicyVersionParams
import Amazonka.IoT.Types.ReportType
import Amazonka.IoT.Types.RepublishAction
import Amazonka.IoT.Types.ResourceIdentifier
import Amazonka.IoT.Types.ResourceType
import Amazonka.IoT.Types.RetryCriteria
import Amazonka.IoT.Types.RetryableFailureType
import Amazonka.IoT.Types.RoleAliasDescription
import Amazonka.IoT.Types.S3Action
import Amazonka.IoT.Types.S3Destination
import Amazonka.IoT.Types.S3Location
import Amazonka.IoT.Types.SalesforceAction
import Amazonka.IoT.Types.ScheduledAuditMetadata
import Amazonka.IoT.Types.SchedulingConfig
import Amazonka.IoT.Types.SecurityProfileIdentifier
import Amazonka.IoT.Types.SecurityProfileTarget
import Amazonka.IoT.Types.SecurityProfileTargetMapping
import Amazonka.IoT.Types.ServerCertificateStatus
import Amazonka.IoT.Types.ServerCertificateSummary
import Amazonka.IoT.Types.ServiceType
import Amazonka.IoT.Types.SigV4Authorization
import Amazonka.IoT.Types.SigningProfileParameter
import Amazonka.IoT.Types.SnsAction
import Amazonka.IoT.Types.SqsAction
import Amazonka.IoT.Types.StartSigningJobParameter
import Amazonka.IoT.Types.StatisticalThreshold
import Amazonka.IoT.Types.Statistics
import Amazonka.IoT.Types.StepFunctionsAction
import Amazonka.IoT.Types.Stream
import Amazonka.IoT.Types.StreamFile
import Amazonka.IoT.Types.StreamInfo
import Amazonka.IoT.Types.StreamSummary
import Amazonka.IoT.Types.Tag
import Amazonka.IoT.Types.TargetSelection
import Amazonka.IoT.Types.TaskStatistics
import Amazonka.IoT.Types.TaskStatisticsForAuditCheck
import Amazonka.IoT.Types.TaskStatus
import Amazonka.IoT.Types.TemplateType
import Amazonka.IoT.Types.TermsAggregation
import Amazonka.IoT.Types.ThingAttribute
import Amazonka.IoT.Types.ThingConnectivity
import Amazonka.IoT.Types.ThingConnectivityIndexingMode
import Amazonka.IoT.Types.ThingDocument
import Amazonka.IoT.Types.ThingGroupDocument
import Amazonka.IoT.Types.ThingGroupIndexingConfiguration
import Amazonka.IoT.Types.ThingGroupIndexingMode
import Amazonka.IoT.Types.ThingGroupMetadata
import Amazonka.IoT.Types.ThingGroupProperties
import Amazonka.IoT.Types.ThingIndexingConfiguration
import Amazonka.IoT.Types.ThingIndexingMode
import Amazonka.IoT.Types.ThingTypeDefinition
import Amazonka.IoT.Types.ThingTypeMetadata
import Amazonka.IoT.Types.ThingTypeProperties
import Amazonka.IoT.Types.TimeoutConfig
import Amazonka.IoT.Types.TimestreamAction
import Amazonka.IoT.Types.TimestreamDimension
import Amazonka.IoT.Types.TimestreamTimestamp
import Amazonka.IoT.Types.TlsContext
import Amazonka.IoT.Types.TopicRule
import Amazonka.IoT.Types.TopicRuleDestination
import Amazonka.IoT.Types.TopicRuleDestinationConfiguration
import Amazonka.IoT.Types.TopicRuleDestinationStatus
import Amazonka.IoT.Types.TopicRuleDestinationSummary
import Amazonka.IoT.Types.TopicRuleListItem
import Amazonka.IoT.Types.TopicRulePayload
import Amazonka.IoT.Types.TransferData
import Amazonka.IoT.Types.UpdateCACertificateParams
import Amazonka.IoT.Types.UpdateDeviceCertificateParams
import Amazonka.IoT.Types.UserProperty
import Amazonka.IoT.Types.ValidationError
import Amazonka.IoT.Types.VerificationState
import Amazonka.IoT.Types.ViolationEvent
import Amazonka.IoT.Types.ViolationEventAdditionalInfo
import Amazonka.IoT.Types.ViolationEventOccurrenceRange
import Amazonka.IoT.Types.ViolationEventType
import Amazonka.IoT.Types.VpcDestinationConfiguration
import Amazonka.IoT.Types.VpcDestinationProperties
import Amazonka.IoT.Types.VpcDestinationSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-05-28@ of the Amazon IoT SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoT",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "iot",
      Core.signingName = "execute-api",
      Core.version = "2015-05-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoT",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

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

-- | The certificate operation is not allowed.
_CertificateStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateStateException =
  Core._MatchServiceError
    defaultService
    "CertificateStateException"
    Prelude.. Core.hasStatus 406

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

-- | A conflicting resource update exception. This exception is thrown when
-- two pending updates cause a conflict.
_ConflictingResourceUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictingResourceUpdateException =
  Core._MatchServiceError
    defaultService
    "ConflictingResourceUpdateException"
    Prelude.. Core.hasStatus 409

-- | You can\'t delete the resource because it is attached to one or more
-- resources.
_DeleteConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeleteConflictException =
  Core._MatchServiceError
    defaultService
    "DeleteConflictException"
    Prelude.. Core.hasStatus 409

-- | The index is not ready.
_IndexNotReadyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IndexNotReadyException =
  Core._MatchServiceError
    defaultService
    "IndexNotReadyException"
    Prelude.. Core.hasStatus 400

-- | An unexpected error has occurred.
_InternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | An unexpected error has occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | Internal error from the service that indicates an unexpected error or
-- that the service is unavailable.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The aggregation is invalid.
_InvalidAggregationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAggregationException =
  Core._MatchServiceError
    defaultService
    "InvalidAggregationException"
    Prelude.. Core.hasStatus 400

-- | The query is invalid.
_InvalidQueryException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidQueryException =
  Core._MatchServiceError
    defaultService
    "InvalidQueryException"
    Prelude.. Core.hasStatus 400

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The response is invalid.
_InvalidResponseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResponseException =
  Core._MatchServiceError
    defaultService
    "InvalidResponseException"
    Prelude.. Core.hasStatus 400

-- | An attempt was made to change to an invalid state, for example by
-- deleting a job or a job execution which is \"IN_PROGRESS\" without
-- setting the @force@ parameter.
_InvalidStateTransitionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateTransitionException =
  Core._MatchServiceError
    defaultService
    "InvalidStateTransitionException"
    Prelude.. Core.hasStatus 409

-- | A limit has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 410

-- | The policy documentation is not valid.
_MalformedPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyException"
    Prelude.. Core.hasStatus 400

-- | The resource is not configured.
_NotConfiguredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotConfiguredException =
  Core._MatchServiceError
    defaultService
    "NotConfiguredException"
    Prelude.. Core.hasStatus 404

-- | The registration code is invalid.
_RegistrationCodeValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RegistrationCodeValidationException =
  Core._MatchServiceError
    defaultService
    "RegistrationCodeValidationException"
    Prelude.. Core.hasStatus 400

-- | The resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The resource registration failed.
_ResourceRegistrationFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceRegistrationFailureException =
  Core._MatchServiceError
    defaultService
    "ResourceRegistrationFailureException"
    Prelude.. Core.hasStatus 400

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The Rule-SQL expression can\'t be parsed correctly.
_SqlParseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SqlParseException =
  Core._MatchServiceError
    defaultService
    "SqlParseException"
    Prelude.. Core.hasStatus 400

-- | This exception occurs if you attempt to start a task with the same
-- task-id as an existing task but with a different clientRequestToken.
_TaskAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TaskAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "TaskAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | The rate exceeds the limit.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 400

-- | You can\'t revert the certificate transfer because the transfer is
-- already complete.
_TransferAlreadyCompletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TransferAlreadyCompletedException =
  Core._MatchServiceError
    defaultService
    "TransferAlreadyCompletedException"
    Prelude.. Core.hasStatus 410

-- | You can\'t transfer the certificate because authorization policies are
-- still attached.
_TransferConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TransferConflictException =
  Core._MatchServiceError
    defaultService
    "TransferConflictException"
    Prelude.. Core.hasStatus 409

-- | You are not authorized to perform this operation.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | An exception thrown when the version of an entity specified with the
-- @expectedVersion@ parameter does not match the latest version in the
-- system.
_VersionConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_VersionConflictException =
  Core._MatchServiceError
    defaultService
    "VersionConflictException"
    Prelude.. Core.hasStatus 409

-- | The number of policy versions exceeds the limit.
_VersionsLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_VersionsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "VersionsLimitExceededException"
    Prelude.. Core.hasStatus 409
