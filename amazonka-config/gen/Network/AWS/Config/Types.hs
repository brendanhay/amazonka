{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NoSuchRetentionConfigurationException,
    _NoSuchConfigRuleException,
    _NoAvailableOrganizationException,
    _NoSuchOrganizationConformancePackException,
    _NoSuchOrganizationConfigRuleException,
    _InvalidResultTokenException,
    _InvalidRecordingGroupException,
    _InvalidExpressionException,
    _TooManyTagsException,
    _InvalidS3KeyPrefixException,
    _MaxNumberOfDeliveryChannelsExceededException,
    _InvalidDeliveryChannelNameException,
    _InvalidConfigurationRecorderNameException,
    _NoRunningConfigurationRecorderException,
    _NoAvailableDeliveryChannelException,
    _LastDeliveryChannelDeleteFailedException,
    _ConformancePackTemplateValidationException,
    _OversizedConfigurationItemException,
    _MaxNumberOfConformancePacksExceededException,
    _InvalidRoleException,
    _InvalidNextTokenException,
    _NoSuchConfigurationAggregatorException,
    _NoSuchConformancePackException,
    _OrganizationAccessDeniedException,
    _InvalidParameterValueException,
    _NoSuchRemediationConfigurationException,
    _InvalidTimeRangeException,
    _NoSuchConfigRuleInConformancePackException,
    _ValidationException,
    _InvalidSNSTopicARNException,
    _NoSuchDeliveryChannelException,
    _ResourceInUseException,
    _LimitExceededException,
    _InvalidS3KmsKeyArnException,
    _NoSuchConfigurationRecorderException,
    _InsufficientDeliveryPolicyException,
    _MaxNumberOfOrganizationConfigRulesExceededException,
    _MaxNumberOfOrganizationConformancePacksExceededException,
    _ResourceNotFoundException,
    _InvalidLimitException,
    _OrganizationConformancePackTemplateValidationException,
    _MaxNumberOfConfigurationRecordersExceededException,
    _NoSuchRemediationExceptionException,
    _ResourceConcurrentModificationException,
    _MaxNumberOfConfigRulesExceededException,
    _NoAvailableConfigurationRecorderException,
    _NoSuchBucketException,
    _MaxActiveResourcesExceededException,
    _MaxNumberOfRetentionConfigurationsExceededException,
    _OrganizationAllFeaturesNotEnabledException,
    _InsufficientPermissionsException,
    _RemediationInProgressException,
    _ResourceNotDiscoveredException,

    -- * AggregatedSourceStatusType
    AggregatedSourceStatusType (..),

    -- * AggregatedSourceType
    AggregatedSourceType (..),

    -- * ChronologicalOrder
    ChronologicalOrder (..),

    -- * ComplianceType
    ComplianceType (..),

    -- * ConfigRuleComplianceSummaryGroupKey
    ConfigRuleComplianceSummaryGroupKey (..),

    -- * ConfigRuleState
    ConfigRuleState (..),

    -- * ConfigurationItemStatus
    ConfigurationItemStatus (..),

    -- * ConformancePackComplianceType
    ConformancePackComplianceType (..),

    -- * ConformancePackState
    ConformancePackState (..),

    -- * DeliveryStatus
    DeliveryStatus (..),

    -- * EventSource
    EventSource (..),

    -- * MaximumExecutionFrequency
    MaximumExecutionFrequency (..),

    -- * MemberAccountRuleStatus
    MemberAccountRuleStatus (..),

    -- * MessageType
    MessageType (..),

    -- * OrganizationConfigRuleTriggerType
    OrganizationConfigRuleTriggerType (..),

    -- * OrganizationResourceDetailedStatus
    OrganizationResourceDetailedStatus (..),

    -- * OrganizationResourceStatus
    OrganizationResourceStatus (..),

    -- * OrganizationRuleStatus
    OrganizationRuleStatus (..),

    -- * Owner
    Owner (..),

    -- * RecorderStatus
    RecorderStatus (..),

    -- * RemediationExecutionState
    RemediationExecutionState (..),

    -- * RemediationExecutionStepState
    RemediationExecutionStepState (..),

    -- * RemediationTargetType
    RemediationTargetType (..),

    -- * ResourceCountGroupKey
    ResourceCountGroupKey (..),

    -- * ResourceType
    ResourceType (..),

    -- * ResourceValueType
    ResourceValueType (..),

    -- * AccountAggregationSource
    AccountAggregationSource (..),
    newAccountAggregationSource,
    accountAggregationSource_allAwsRegions,
    accountAggregationSource_awsRegions,
    accountAggregationSource_accountIds,

    -- * AggregateComplianceByConfigRule
    AggregateComplianceByConfigRule (..),
    newAggregateComplianceByConfigRule,
    aggregateComplianceByConfigRule_accountId,
    aggregateComplianceByConfigRule_configRuleName,
    aggregateComplianceByConfigRule_compliance,
    aggregateComplianceByConfigRule_awsRegion,

    -- * AggregateComplianceCount
    AggregateComplianceCount (..),
    newAggregateComplianceCount,
    aggregateComplianceCount_complianceSummary,
    aggregateComplianceCount_groupName,

    -- * AggregateEvaluationResult
    AggregateEvaluationResult (..),
    newAggregateEvaluationResult,
    aggregateEvaluationResult_annotation,
    aggregateEvaluationResult_evaluationResultIdentifier,
    aggregateEvaluationResult_accountId,
    aggregateEvaluationResult_resultRecordedTime,
    aggregateEvaluationResult_complianceType,
    aggregateEvaluationResult_configRuleInvokedTime,
    aggregateEvaluationResult_awsRegion,

    -- * AggregateResourceIdentifier
    AggregateResourceIdentifier (..),
    newAggregateResourceIdentifier,
    aggregateResourceIdentifier_resourceName,
    aggregateResourceIdentifier_sourceAccountId,
    aggregateResourceIdentifier_sourceRegion,
    aggregateResourceIdentifier_resourceId,
    aggregateResourceIdentifier_resourceType,

    -- * AggregatedSourceStatus
    AggregatedSourceStatus (..),
    newAggregatedSourceStatus,
    aggregatedSourceStatus_lastUpdateStatus,
    aggregatedSourceStatus_lastErrorMessage,
    aggregatedSourceStatus_lastUpdateTime,
    aggregatedSourceStatus_sourceId,
    aggregatedSourceStatus_lastErrorCode,
    aggregatedSourceStatus_awsRegion,
    aggregatedSourceStatus_sourceType,

    -- * AggregationAuthorization
    AggregationAuthorization (..),
    newAggregationAuthorization,
    aggregationAuthorization_creationTime,
    aggregationAuthorization_authorizedAccountId,
    aggregationAuthorization_authorizedAwsRegion,
    aggregationAuthorization_aggregationAuthorizationArn,

    -- * BaseConfigurationItem
    BaseConfigurationItem (..),
    newBaseConfigurationItem,
    baseConfigurationItem_resourceId,
    baseConfigurationItem_accountId,
    baseConfigurationItem_configuration,
    baseConfigurationItem_arn,
    baseConfigurationItem_version,
    baseConfigurationItem_configurationStateId,
    baseConfigurationItem_resourceType,
    baseConfigurationItem_supplementaryConfiguration,
    baseConfigurationItem_availabilityZone,
    baseConfigurationItem_configurationItemCaptureTime,
    baseConfigurationItem_configurationItemStatus,
    baseConfigurationItem_resourceCreationTime,
    baseConfigurationItem_awsRegion,
    baseConfigurationItem_resourceName,

    -- * Compliance
    Compliance (..),
    newCompliance,
    compliance_complianceType,
    compliance_complianceContributorCount,

    -- * ComplianceByConfigRule
    ComplianceByConfigRule (..),
    newComplianceByConfigRule,
    complianceByConfigRule_configRuleName,
    complianceByConfigRule_compliance,

    -- * ComplianceByResource
    ComplianceByResource (..),
    newComplianceByResource,
    complianceByResource_resourceId,
    complianceByResource_resourceType,
    complianceByResource_compliance,

    -- * ComplianceContributorCount
    ComplianceContributorCount (..),
    newComplianceContributorCount,
    complianceContributorCount_capExceeded,
    complianceContributorCount_cappedCount,

    -- * ComplianceSummary
    ComplianceSummary (..),
    newComplianceSummary,
    complianceSummary_complianceSummaryTimestamp,
    complianceSummary_nonCompliantResourceCount,
    complianceSummary_compliantResourceCount,

    -- * ComplianceSummaryByResourceType
    ComplianceSummaryByResourceType (..),
    newComplianceSummaryByResourceType,
    complianceSummaryByResourceType_complianceSummary,
    complianceSummaryByResourceType_resourceType,

    -- * ConfigExportDeliveryInfo
    ConfigExportDeliveryInfo (..),
    newConfigExportDeliveryInfo,
    configExportDeliveryInfo_lastErrorMessage,
    configExportDeliveryInfo_nextDeliveryTime,
    configExportDeliveryInfo_lastSuccessfulTime,
    configExportDeliveryInfo_lastErrorCode,
    configExportDeliveryInfo_lastStatus,
    configExportDeliveryInfo_lastAttemptTime,

    -- * ConfigRule
    ConfigRule (..),
    newConfigRule,
    configRule_configRuleId,
    configRule_configRuleArn,
    configRule_maximumExecutionFrequency,
    configRule_configRuleName,
    configRule_configRuleState,
    configRule_scope,
    configRule_inputParameters,
    configRule_description,
    configRule_createdBy,
    configRule_source,

    -- * ConfigRuleComplianceFilters
    ConfigRuleComplianceFilters (..),
    newConfigRuleComplianceFilters,
    configRuleComplianceFilters_accountId,
    configRuleComplianceFilters_configRuleName,
    configRuleComplianceFilters_complianceType,
    configRuleComplianceFilters_awsRegion,

    -- * ConfigRuleComplianceSummaryFilters
    ConfigRuleComplianceSummaryFilters (..),
    newConfigRuleComplianceSummaryFilters,
    configRuleComplianceSummaryFilters_accountId,
    configRuleComplianceSummaryFilters_awsRegion,

    -- * ConfigRuleEvaluationStatus
    ConfigRuleEvaluationStatus (..),
    newConfigRuleEvaluationStatus,
    configRuleEvaluationStatus_lastErrorMessage,
    configRuleEvaluationStatus_configRuleId,
    configRuleEvaluationStatus_configRuleArn,
    configRuleEvaluationStatus_configRuleName,
    configRuleEvaluationStatus_firstEvaluationStarted,
    configRuleEvaluationStatus_lastFailedEvaluationTime,
    configRuleEvaluationStatus_firstActivatedTime,
    configRuleEvaluationStatus_lastErrorCode,
    configRuleEvaluationStatus_lastFailedInvocationTime,
    configRuleEvaluationStatus_lastSuccessfulInvocationTime,
    configRuleEvaluationStatus_lastDeactivatedTime,
    configRuleEvaluationStatus_lastSuccessfulEvaluationTime,

    -- * ConfigSnapshotDeliveryProperties
    ConfigSnapshotDeliveryProperties (..),
    newConfigSnapshotDeliveryProperties,
    configSnapshotDeliveryProperties_deliveryFrequency,

    -- * ConfigStreamDeliveryInfo
    ConfigStreamDeliveryInfo (..),
    newConfigStreamDeliveryInfo,
    configStreamDeliveryInfo_lastErrorMessage,
    configStreamDeliveryInfo_lastErrorCode,
    configStreamDeliveryInfo_lastStatus,
    configStreamDeliveryInfo_lastStatusChangeTime,

    -- * ConfigurationAggregator
    ConfigurationAggregator (..),
    newConfigurationAggregator,
    configurationAggregator_creationTime,
    configurationAggregator_configurationAggregatorArn,
    configurationAggregator_configurationAggregatorName,
    configurationAggregator_accountAggregationSources,
    configurationAggregator_createdBy,
    configurationAggregator_lastUpdatedTime,
    configurationAggregator_organizationAggregationSource,

    -- * ConfigurationItem
    ConfigurationItem (..),
    newConfigurationItem,
    configurationItem_relationships,
    configurationItem_resourceId,
    configurationItem_accountId,
    configurationItem_relatedEvents,
    configurationItem_configuration,
    configurationItem_arn,
    configurationItem_version,
    configurationItem_configurationStateId,
    configurationItem_resourceType,
    configurationItem_supplementaryConfiguration,
    configurationItem_availabilityZone,
    configurationItem_configurationItemCaptureTime,
    configurationItem_configurationItemStatus,
    configurationItem_tags,
    configurationItem_resourceCreationTime,
    configurationItem_configurationItemMD5Hash,
    configurationItem_awsRegion,
    configurationItem_resourceName,

    -- * ConfigurationRecorder
    ConfigurationRecorder (..),
    newConfigurationRecorder,
    configurationRecorder_roleARN,
    configurationRecorder_name,
    configurationRecorder_recordingGroup,

    -- * ConfigurationRecorderStatus
    ConfigurationRecorderStatus (..),
    newConfigurationRecorderStatus,
    configurationRecorderStatus_lastStopTime,
    configurationRecorderStatus_lastStartTime,
    configurationRecorderStatus_lastErrorMessage,
    configurationRecorderStatus_recording,
    configurationRecorderStatus_name,
    configurationRecorderStatus_lastErrorCode,
    configurationRecorderStatus_lastStatus,
    configurationRecorderStatus_lastStatusChangeTime,

    -- * ConformancePackComplianceFilters
    ConformancePackComplianceFilters (..),
    newConformancePackComplianceFilters,
    conformancePackComplianceFilters_complianceType,
    conformancePackComplianceFilters_configRuleNames,

    -- * ConformancePackComplianceSummary
    ConformancePackComplianceSummary (..),
    newConformancePackComplianceSummary,
    conformancePackComplianceSummary_conformancePackName,
    conformancePackComplianceSummary_conformancePackComplianceStatus,

    -- * ConformancePackDetail
    ConformancePackDetail (..),
    newConformancePackDetail,
    conformancePackDetail_lastUpdateRequestedTime,
    conformancePackDetail_deliveryS3Bucket,
    conformancePackDetail_deliveryS3KeyPrefix,
    conformancePackDetail_createdBy,
    conformancePackDetail_conformancePackInputParameters,
    conformancePackDetail_conformancePackName,
    conformancePackDetail_conformancePackArn,
    conformancePackDetail_conformancePackId,

    -- * ConformancePackEvaluationFilters
    ConformancePackEvaluationFilters (..),
    newConformancePackEvaluationFilters,
    conformancePackEvaluationFilters_complianceType,
    conformancePackEvaluationFilters_resourceType,
    conformancePackEvaluationFilters_resourceIds,
    conformancePackEvaluationFilters_configRuleNames,

    -- * ConformancePackEvaluationResult
    ConformancePackEvaluationResult (..),
    newConformancePackEvaluationResult,
    conformancePackEvaluationResult_annotation,
    conformancePackEvaluationResult_complianceType,
    conformancePackEvaluationResult_evaluationResultIdentifier,
    conformancePackEvaluationResult_configRuleInvokedTime,
    conformancePackEvaluationResult_resultRecordedTime,

    -- * ConformancePackInputParameter
    ConformancePackInputParameter (..),
    newConformancePackInputParameter,
    conformancePackInputParameter_parameterName,
    conformancePackInputParameter_parameterValue,

    -- * ConformancePackRuleCompliance
    ConformancePackRuleCompliance (..),
    newConformancePackRuleCompliance,
    conformancePackRuleCompliance_configRuleName,
    conformancePackRuleCompliance_complianceType,

    -- * ConformancePackStatusDetail
    ConformancePackStatusDetail (..),
    newConformancePackStatusDetail,
    conformancePackStatusDetail_conformancePackStatusReason,
    conformancePackStatusDetail_lastUpdateCompletedTime,
    conformancePackStatusDetail_conformancePackName,
    conformancePackStatusDetail_conformancePackId,
    conformancePackStatusDetail_conformancePackArn,
    conformancePackStatusDetail_conformancePackState,
    conformancePackStatusDetail_stackArn,
    conformancePackStatusDetail_lastUpdateRequestedTime,

    -- * DeliveryChannel
    DeliveryChannel (..),
    newDeliveryChannel,
    deliveryChannel_s3KmsKeyArn,
    deliveryChannel_name,
    deliveryChannel_s3KeyPrefix,
    deliveryChannel_s3BucketName,
    deliveryChannel_configSnapshotDeliveryProperties,
    deliveryChannel_snsTopicARN,

    -- * DeliveryChannelStatus
    DeliveryChannelStatus (..),
    newDeliveryChannelStatus,
    deliveryChannelStatus_configSnapshotDeliveryInfo,
    deliveryChannelStatus_configStreamDeliveryInfo,
    deliveryChannelStatus_name,
    deliveryChannelStatus_configHistoryDeliveryInfo,

    -- * Evaluation
    Evaluation (..),
    newEvaluation,
    evaluation_annotation,
    evaluation_complianceResourceType,
    evaluation_complianceResourceId,
    evaluation_complianceType,
    evaluation_orderingTimestamp,

    -- * EvaluationResult
    EvaluationResult (..),
    newEvaluationResult,
    evaluationResult_annotation,
    evaluationResult_evaluationResultIdentifier,
    evaluationResult_resultRecordedTime,
    evaluationResult_complianceType,
    evaluationResult_configRuleInvokedTime,
    evaluationResult_resultToken,

    -- * EvaluationResultIdentifier
    EvaluationResultIdentifier (..),
    newEvaluationResultIdentifier,
    evaluationResultIdentifier_evaluationResultQualifier,
    evaluationResultIdentifier_orderingTimestamp,

    -- * EvaluationResultQualifier
    EvaluationResultQualifier (..),
    newEvaluationResultQualifier,
    evaluationResultQualifier_resourceId,
    evaluationResultQualifier_configRuleName,
    evaluationResultQualifier_resourceType,

    -- * ExecutionControls
    ExecutionControls (..),
    newExecutionControls,
    executionControls_ssmControls,

    -- * ExternalEvaluation
    ExternalEvaluation (..),
    newExternalEvaluation,
    externalEvaluation_annotation,
    externalEvaluation_complianceResourceType,
    externalEvaluation_complianceResourceId,
    externalEvaluation_complianceType,
    externalEvaluation_orderingTimestamp,

    -- * FailedDeleteRemediationExceptionsBatch
    FailedDeleteRemediationExceptionsBatch (..),
    newFailedDeleteRemediationExceptionsBatch,
    failedDeleteRemediationExceptionsBatch_failureMessage,
    failedDeleteRemediationExceptionsBatch_failedItems,

    -- * FailedRemediationBatch
    FailedRemediationBatch (..),
    newFailedRemediationBatch,
    failedRemediationBatch_failureMessage,
    failedRemediationBatch_failedItems,

    -- * FailedRemediationExceptionBatch
    FailedRemediationExceptionBatch (..),
    newFailedRemediationExceptionBatch,
    failedRemediationExceptionBatch_failureMessage,
    failedRemediationExceptionBatch_failedItems,

    -- * FieldInfo
    FieldInfo (..),
    newFieldInfo,
    fieldInfo_name,

    -- * GroupedResourceCount
    GroupedResourceCount (..),
    newGroupedResourceCount,
    groupedResourceCount_groupName,
    groupedResourceCount_resourceCount,

    -- * MemberAccountStatus
    MemberAccountStatus (..),
    newMemberAccountStatus,
    memberAccountStatus_lastUpdateTime,
    memberAccountStatus_errorMessage,
    memberAccountStatus_errorCode,
    memberAccountStatus_accountId,
    memberAccountStatus_configRuleName,
    memberAccountStatus_memberAccountRuleStatus,

    -- * OrganizationAggregationSource
    OrganizationAggregationSource (..),
    newOrganizationAggregationSource,
    organizationAggregationSource_allAwsRegions,
    organizationAggregationSource_awsRegions,
    organizationAggregationSource_roleArn,

    -- * OrganizationConfigRule
    OrganizationConfigRule (..),
    newOrganizationConfigRule,
    organizationConfigRule_lastUpdateTime,
    organizationConfigRule_organizationManagedRuleMetadata,
    organizationConfigRule_organizationCustomRuleMetadata,
    organizationConfigRule_excludedAccounts,
    organizationConfigRule_organizationConfigRuleName,
    organizationConfigRule_organizationConfigRuleArn,

    -- * OrganizationConfigRuleStatus
    OrganizationConfigRuleStatus (..),
    newOrganizationConfigRuleStatus,
    organizationConfigRuleStatus_lastUpdateTime,
    organizationConfigRuleStatus_errorMessage,
    organizationConfigRuleStatus_errorCode,
    organizationConfigRuleStatus_organizationConfigRuleName,
    organizationConfigRuleStatus_organizationRuleStatus,

    -- * OrganizationConformancePack
    OrganizationConformancePack (..),
    newOrganizationConformancePack,
    organizationConformancePack_deliveryS3Bucket,
    organizationConformancePack_deliveryS3KeyPrefix,
    organizationConformancePack_excludedAccounts,
    organizationConformancePack_conformancePackInputParameters,
    organizationConformancePack_organizationConformancePackName,
    organizationConformancePack_organizationConformancePackArn,
    organizationConformancePack_lastUpdateTime,

    -- * OrganizationConformancePackDetailedStatus
    OrganizationConformancePackDetailedStatus (..),
    newOrganizationConformancePackDetailedStatus,
    organizationConformancePackDetailedStatus_lastUpdateTime,
    organizationConformancePackDetailedStatus_errorMessage,
    organizationConformancePackDetailedStatus_errorCode,
    organizationConformancePackDetailedStatus_accountId,
    organizationConformancePackDetailedStatus_conformancePackName,
    organizationConformancePackDetailedStatus_status,

    -- * OrganizationConformancePackStatus
    OrganizationConformancePackStatus (..),
    newOrganizationConformancePackStatus,
    organizationConformancePackStatus_lastUpdateTime,
    organizationConformancePackStatus_errorMessage,
    organizationConformancePackStatus_errorCode,
    organizationConformancePackStatus_organizationConformancePackName,
    organizationConformancePackStatus_status,

    -- * OrganizationCustomRuleMetadata
    OrganizationCustomRuleMetadata (..),
    newOrganizationCustomRuleMetadata,
    organizationCustomRuleMetadata_tagKeyScope,
    organizationCustomRuleMetadata_maximumExecutionFrequency,
    organizationCustomRuleMetadata_resourceIdScope,
    organizationCustomRuleMetadata_inputParameters,
    organizationCustomRuleMetadata_description,
    organizationCustomRuleMetadata_resourceTypesScope,
    organizationCustomRuleMetadata_tagValueScope,
    organizationCustomRuleMetadata_lambdaFunctionArn,
    organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes,

    -- * OrganizationManagedRuleMetadata
    OrganizationManagedRuleMetadata (..),
    newOrganizationManagedRuleMetadata,
    organizationManagedRuleMetadata_tagKeyScope,
    organizationManagedRuleMetadata_maximumExecutionFrequency,
    organizationManagedRuleMetadata_resourceIdScope,
    organizationManagedRuleMetadata_inputParameters,
    organizationManagedRuleMetadata_description,
    organizationManagedRuleMetadata_resourceTypesScope,
    organizationManagedRuleMetadata_tagValueScope,
    organizationManagedRuleMetadata_ruleIdentifier,

    -- * OrganizationResourceDetailedStatusFilters
    OrganizationResourceDetailedStatusFilters (..),
    newOrganizationResourceDetailedStatusFilters,
    organizationResourceDetailedStatusFilters_status,
    organizationResourceDetailedStatusFilters_accountId,

    -- * PendingAggregationRequest
    PendingAggregationRequest (..),
    newPendingAggregationRequest,
    pendingAggregationRequest_requesterAwsRegion,
    pendingAggregationRequest_requesterAccountId,

    -- * QueryInfo
    QueryInfo (..),
    newQueryInfo,
    queryInfo_selectFields,

    -- * RecordingGroup
    RecordingGroup (..),
    newRecordingGroup,
    recordingGroup_allSupported,
    recordingGroup_resourceTypes,
    recordingGroup_includeGlobalResourceTypes,

    -- * Relationship
    Relationship (..),
    newRelationship,
    relationship_resourceId,
    relationship_resourceType,
    relationship_relationshipName,
    relationship_resourceName,

    -- * RemediationConfiguration
    RemediationConfiguration (..),
    newRemediationConfiguration,
    remediationConfiguration_retryAttemptSeconds,
    remediationConfiguration_executionControls,
    remediationConfiguration_targetVersion,
    remediationConfiguration_arn,
    remediationConfiguration_automatic,
    remediationConfiguration_resourceType,
    remediationConfiguration_createdByService,
    remediationConfiguration_maximumAutomaticAttempts,
    remediationConfiguration_parameters,
    remediationConfiguration_configRuleName,
    remediationConfiguration_targetType,
    remediationConfiguration_targetId,

    -- * RemediationException
    RemediationException (..),
    newRemediationException,
    remediationException_expirationTime,
    remediationException_message,
    remediationException_configRuleName,
    remediationException_resourceType,
    remediationException_resourceId,

    -- * RemediationExceptionResourceKey
    RemediationExceptionResourceKey (..),
    newRemediationExceptionResourceKey,
    remediationExceptionResourceKey_resourceId,
    remediationExceptionResourceKey_resourceType,

    -- * RemediationExecutionStatus
    RemediationExecutionStatus (..),
    newRemediationExecutionStatus,
    remediationExecutionStatus_invocationTime,
    remediationExecutionStatus_resourceKey,
    remediationExecutionStatus_state,
    remediationExecutionStatus_stepDetails,
    remediationExecutionStatus_lastUpdatedTime,

    -- * RemediationExecutionStep
    RemediationExecutionStep (..),
    newRemediationExecutionStep,
    remediationExecutionStep_startTime,
    remediationExecutionStep_stopTime,
    remediationExecutionStep_state,
    remediationExecutionStep_name,
    remediationExecutionStep_errorMessage,

    -- * RemediationParameterValue
    RemediationParameterValue (..),
    newRemediationParameterValue,
    remediationParameterValue_resourceValue,
    remediationParameterValue_staticValue,

    -- * ResourceCount
    ResourceCount (..),
    newResourceCount,
    resourceCount_resourceType,
    resourceCount_count,

    -- * ResourceCountFilters
    ResourceCountFilters (..),
    newResourceCountFilters,
    resourceCountFilters_accountId,
    resourceCountFilters_resourceType,
    resourceCountFilters_region,

    -- * ResourceFilters
    ResourceFilters (..),
    newResourceFilters,
    resourceFilters_resourceId,
    resourceFilters_accountId,
    resourceFilters_region,
    resourceFilters_resourceName,

    -- * ResourceIdentifier
    ResourceIdentifier (..),
    newResourceIdentifier,
    resourceIdentifier_resourceId,
    resourceIdentifier_resourceType,
    resourceIdentifier_resourceDeletionTime,
    resourceIdentifier_resourceName,

    -- * ResourceKey
    ResourceKey (..),
    newResourceKey,
    resourceKey_resourceType,
    resourceKey_resourceId,

    -- * ResourceValue
    ResourceValue (..),
    newResourceValue,
    resourceValue_value,

    -- * RetentionConfiguration
    RetentionConfiguration (..),
    newRetentionConfiguration,
    retentionConfiguration_name,
    retentionConfiguration_retentionPeriodInDays,

    -- * Scope
    Scope (..),
    newScope,
    scope_tagValue,
    scope_tagKey,
    scope_complianceResourceId,
    scope_complianceResourceTypes,

    -- * Source
    Source (..),
    newSource,
    source_sourceDetails,
    source_owner,
    source_sourceIdentifier,

    -- * SourceDetail
    SourceDetail (..),
    newSourceDetail,
    sourceDetail_eventSource,
    sourceDetail_maximumExecutionFrequency,
    sourceDetail_messageType,

    -- * SsmControls
    SsmControls (..),
    newSsmControls,
    ssmControls_errorPercentage,
    ssmControls_concurrentExecutionRatePercentage,

    -- * StaticValue
    StaticValue (..),
    newStaticValue,
    staticValue_values,

    -- * StatusDetailFilters
    StatusDetailFilters (..),
    newStatusDetailFilters,
    statusDetailFilters_accountId,
    statusDetailFilters_memberAccountRuleStatus,

    -- * StoredQuery
    StoredQuery (..),
    newStoredQuery,
    storedQuery_queryArn,
    storedQuery_queryId,
    storedQuery_description,
    storedQuery_expression,
    storedQuery_queryName,

    -- * StoredQueryMetadata
    StoredQueryMetadata (..),
    newStoredQueryMetadata,
    storedQueryMetadata_description,
    storedQueryMetadata_queryId,
    storedQueryMetadata_queryArn,
    storedQueryMetadata_queryName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Network.AWS.Config.Types.AccountAggregationSource
import Network.AWS.Config.Types.AggregateComplianceByConfigRule
import Network.AWS.Config.Types.AggregateComplianceCount
import Network.AWS.Config.Types.AggregateEvaluationResult
import Network.AWS.Config.Types.AggregateResourceIdentifier
import Network.AWS.Config.Types.AggregatedSourceStatus
import Network.AWS.Config.Types.AggregatedSourceStatusType
import Network.AWS.Config.Types.AggregatedSourceType
import Network.AWS.Config.Types.AggregationAuthorization
import Network.AWS.Config.Types.BaseConfigurationItem
import Network.AWS.Config.Types.ChronologicalOrder
import Network.AWS.Config.Types.Compliance
import Network.AWS.Config.Types.ComplianceByConfigRule
import Network.AWS.Config.Types.ComplianceByResource
import Network.AWS.Config.Types.ComplianceContributorCount
import Network.AWS.Config.Types.ComplianceSummary
import Network.AWS.Config.Types.ComplianceSummaryByResourceType
import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Config.Types.ConfigExportDeliveryInfo
import Network.AWS.Config.Types.ConfigRule
import Network.AWS.Config.Types.ConfigRuleComplianceFilters
import Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
import Network.AWS.Config.Types.ConfigRuleComplianceSummaryGroupKey
import Network.AWS.Config.Types.ConfigRuleEvaluationStatus
import Network.AWS.Config.Types.ConfigRuleState
import Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties
import Network.AWS.Config.Types.ConfigStreamDeliveryInfo
import Network.AWS.Config.Types.ConfigurationAggregator
import Network.AWS.Config.Types.ConfigurationItem
import Network.AWS.Config.Types.ConfigurationItemStatus
import Network.AWS.Config.Types.ConfigurationRecorder
import Network.AWS.Config.Types.ConfigurationRecorderStatus
import Network.AWS.Config.Types.ConformancePackComplianceFilters
import Network.AWS.Config.Types.ConformancePackComplianceSummary
import Network.AWS.Config.Types.ConformancePackComplianceType
import Network.AWS.Config.Types.ConformancePackDetail
import Network.AWS.Config.Types.ConformancePackEvaluationFilters
import Network.AWS.Config.Types.ConformancePackEvaluationResult
import Network.AWS.Config.Types.ConformancePackInputParameter
import Network.AWS.Config.Types.ConformancePackRuleCompliance
import Network.AWS.Config.Types.ConformancePackState
import Network.AWS.Config.Types.ConformancePackStatusDetail
import Network.AWS.Config.Types.DeliveryChannel
import Network.AWS.Config.Types.DeliveryChannelStatus
import Network.AWS.Config.Types.DeliveryStatus
import Network.AWS.Config.Types.Evaluation
import Network.AWS.Config.Types.EvaluationResult
import Network.AWS.Config.Types.EvaluationResultIdentifier
import Network.AWS.Config.Types.EvaluationResultQualifier
import Network.AWS.Config.Types.EventSource
import Network.AWS.Config.Types.ExecutionControls
import Network.AWS.Config.Types.ExternalEvaluation
import Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
import Network.AWS.Config.Types.FailedRemediationBatch
import Network.AWS.Config.Types.FailedRemediationExceptionBatch
import Network.AWS.Config.Types.FieldInfo
import Network.AWS.Config.Types.GroupedResourceCount
import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.MemberAccountRuleStatus
import Network.AWS.Config.Types.MemberAccountStatus
import Network.AWS.Config.Types.MessageType
import Network.AWS.Config.Types.OrganizationAggregationSource
import Network.AWS.Config.Types.OrganizationConfigRule
import Network.AWS.Config.Types.OrganizationConfigRuleStatus
import Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
import Network.AWS.Config.Types.OrganizationConformancePack
import Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus
import Network.AWS.Config.Types.OrganizationConformancePackStatus
import Network.AWS.Config.Types.OrganizationCustomRuleMetadata
import Network.AWS.Config.Types.OrganizationManagedRuleMetadata
import Network.AWS.Config.Types.OrganizationResourceDetailedStatus
import Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters
import Network.AWS.Config.Types.OrganizationResourceStatus
import Network.AWS.Config.Types.OrganizationRuleStatus
import Network.AWS.Config.Types.Owner
import Network.AWS.Config.Types.PendingAggregationRequest
import Network.AWS.Config.Types.QueryInfo
import Network.AWS.Config.Types.RecorderStatus
import Network.AWS.Config.Types.RecordingGroup
import Network.AWS.Config.Types.Relationship
import Network.AWS.Config.Types.RemediationConfiguration
import Network.AWS.Config.Types.RemediationException
import Network.AWS.Config.Types.RemediationExceptionResourceKey
import Network.AWS.Config.Types.RemediationExecutionState
import Network.AWS.Config.Types.RemediationExecutionStatus
import Network.AWS.Config.Types.RemediationExecutionStep
import Network.AWS.Config.Types.RemediationExecutionStepState
import Network.AWS.Config.Types.RemediationParameterValue
import Network.AWS.Config.Types.RemediationTargetType
import Network.AWS.Config.Types.ResourceCount
import Network.AWS.Config.Types.ResourceCountFilters
import Network.AWS.Config.Types.ResourceCountGroupKey
import Network.AWS.Config.Types.ResourceFilters
import Network.AWS.Config.Types.ResourceIdentifier
import Network.AWS.Config.Types.ResourceKey
import Network.AWS.Config.Types.ResourceType
import Network.AWS.Config.Types.ResourceValue
import Network.AWS.Config.Types.ResourceValueType
import Network.AWS.Config.Types.RetentionConfiguration
import Network.AWS.Config.Types.Scope
import Network.AWS.Config.Types.Source
import Network.AWS.Config.Types.SourceDetail
import Network.AWS.Config.Types.SsmControls
import Network.AWS.Config.Types.StaticValue
import Network.AWS.Config.Types.StatusDetailFilters
import Network.AWS.Config.Types.StoredQuery
import Network.AWS.Config.Types.StoredQueryMetadata
import Network.AWS.Config.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-11-12@ of the Amazon Config SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Config",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "config",
      Core._serviceSigningName = "config",
      Core._serviceVersion = "2014-11-12",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Config",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | You have specified a retention configuration that does not exist.
_NoSuchRetentionConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchRetentionConfigurationException =
  Core._MatchServiceError
    defaultService
    "NoSuchRetentionConfigurationException"

-- | One or more AWS Config rules in the request are invalid. Verify that the
-- rule names are correct and try again.
_NoSuchConfigRuleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConfigRuleException =
  Core._MatchServiceError
    defaultService
    "NoSuchConfigRuleException"

-- | Organization is no longer available.
_NoAvailableOrganizationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoAvailableOrganizationException =
  Core._MatchServiceError
    defaultService
    "NoAvailableOrganizationException"

-- | AWS Config organization conformance pack that you passed in the filter
-- does not exist.
--
-- For DeleteOrganizationConformancePack, you tried to delete an
-- organization conformance pack that does not exist.
_NoSuchOrganizationConformancePackException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchOrganizationConformancePackException =
  Core._MatchServiceError
    defaultService
    "NoSuchOrganizationConformancePackException"

-- | You specified one or more organization config rules that do not exist.
_NoSuchOrganizationConfigRuleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchOrganizationConfigRuleException =
  Core._MatchServiceError
    defaultService
    "NoSuchOrganizationConfigRuleException"

-- | The specified @ResultToken@ is invalid.
_InvalidResultTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResultTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidResultTokenException"

-- | AWS Config throws an exception if the recording group does not contain a
-- valid list of resource types. Invalid values might also be incorrectly
-- formatted.
_InvalidRecordingGroupException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRecordingGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidRecordingGroupException"

-- | The syntax of the query is incorrect.
_InvalidExpressionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExpressionException =
  Core._MatchServiceError
    defaultService
    "InvalidExpressionException"

-- | You have reached the limit of the number of tags you can use. You have
-- more than 50 tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The specified Amazon S3 key prefix is not valid.
_InvalidS3KeyPrefixException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3KeyPrefixException =
  Core._MatchServiceError
    defaultService
    "InvalidS3KeyPrefixException"

-- | You have reached the limit of the number of delivery channels you can
-- create.
_MaxNumberOfDeliveryChannelsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfDeliveryChannelsExceededException =
  Core._MatchServiceError
    defaultService
    "MaxNumberOfDeliveryChannelsExceededException"

-- | The specified delivery channel name is not valid.
_InvalidDeliveryChannelNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeliveryChannelNameException =
  Core._MatchServiceError
    defaultService
    "InvalidDeliveryChannelNameException"

-- | You have provided a configuration recorder name that is not valid.
_InvalidConfigurationRecorderNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationRecorderNameException =
  Core._MatchServiceError
    defaultService
    "InvalidConfigurationRecorderNameException"

-- | There is no configuration recorder running.
_NoRunningConfigurationRecorderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoRunningConfigurationRecorderException =
  Core._MatchServiceError
    defaultService
    "NoRunningConfigurationRecorderException"

-- | There is no delivery channel available to record configurations.
_NoAvailableDeliveryChannelException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoAvailableDeliveryChannelException =
  Core._MatchServiceError
    defaultService
    "NoAvailableDeliveryChannelException"

-- | You cannot delete the delivery channel you specified because the
-- configuration recorder is running.
_LastDeliveryChannelDeleteFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LastDeliveryChannelDeleteFailedException =
  Core._MatchServiceError
    defaultService
    "LastDeliveryChannelDeleteFailedException"

-- | You have specified a template that is not valid or supported.
_ConformancePackTemplateValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConformancePackTemplateValidationException =
  Core._MatchServiceError
    defaultService
    "ConformancePackTemplateValidationException"

-- | The configuration item size is outside the allowable range.
_OversizedConfigurationItemException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OversizedConfigurationItemException =
  Core._MatchServiceError
    defaultService
    "OversizedConfigurationItemException"

-- | You have reached the limit (6) of the number of conformance packs in an
-- account (6 conformance pack with 25 AWS Config rules per pack).
_MaxNumberOfConformancePacksExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfConformancePacksExceededException =
  Core._MatchServiceError
    defaultService
    "MaxNumberOfConformancePacksExceededException"

-- | You have provided a null or empty role ARN.
_InvalidRoleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRoleException =
  Core._MatchServiceError
    defaultService
    "InvalidRoleException"

-- | The specified next token is invalid. Specify the @nextToken@ string that
-- was returned in the previous response to get the next page of results.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | You have specified a configuration aggregator that does not exist.
_NoSuchConfigurationAggregatorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConfigurationAggregatorException =
  Core._MatchServiceError
    defaultService
    "NoSuchConfigurationAggregatorException"

-- | You specified one or more conformance packs that do not exist.
_NoSuchConformancePackException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConformancePackException =
  Core._MatchServiceError
    defaultService
    "NoSuchConformancePackException"

-- | For @PutConfigurationAggregator@ API, you can see this exception for the
-- following reasons:
--
-- -   No permission to call @EnableAWSServiceAccess@ API
--
-- -   The configuration aggregator cannot be updated because your AWS
--     Organization management account or the delegated administrator role
--     changed. Delete this aggregator and create a new one with the
--     current AWS Organization.
--
-- -   The configuration aggregator is associated with a previous AWS
--     Organization and AWS Config cannot aggregate data with current AWS
--     Organization. Delete this aggregator and create a new one with the
--     current AWS Organization.
--
-- -   You are not a registered delegated administrator for AWS Config with
--     permissions to call @ListDelegatedAdministrators@ API. Ensure that
--     the management account registers delagated administrator for AWS
--     Config service principle name before the delegated administrator
--     creates an aggregator.
--
-- For all @OrganizationConfigRule@ and @OrganizationConformancePack@ APIs,
-- AWS Config throws an exception if APIs are called from member accounts.
-- All APIs must be called from organization master account.
_OrganizationAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "OrganizationAccessDeniedException"

-- | One or more of the specified parameters are invalid. Verify that your
-- parameters are valid and try again.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | You specified an AWS Config rule without a remediation configuration.
_NoSuchRemediationConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchRemediationConfigurationException =
  Core._MatchServiceError
    defaultService
    "NoSuchRemediationConfigurationException"

-- | The specified time range is not valid. The earlier time is not
-- chronologically before the later time.
_InvalidTimeRangeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTimeRangeException =
  Core._MatchServiceError
    defaultService
    "InvalidTimeRangeException"

-- | AWS Config rule that you passed in the filter does not exist.
_NoSuchConfigRuleInConformancePackException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConfigRuleInConformancePackException =
  Core._MatchServiceError
    defaultService
    "NoSuchConfigRuleInConformancePackException"

-- | The requested action is not valid.
--
-- For PutStoredQuery, you will see this exception if there are missing
-- required fields or if the input value fails the validation, or if you
-- are trying to create more than 300 queries.
--
-- For GetStoredQuery, ListStoredQuery, and DeleteStoredQuery you will see
-- this exception if there are missing required fields or if the input
-- value fails the validation.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The specified Amazon SNS topic does not exist.
_InvalidSNSTopicARNException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSNSTopicARNException =
  Core._MatchServiceError
    defaultService
    "InvalidSNSTopicARNException"

-- | You have specified a delivery channel that does not exist.
_NoSuchDeliveryChannelException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchDeliveryChannelException =
  Core._MatchServiceError
    defaultService
    "NoSuchDeliveryChannelException"

-- | You see this exception in the following cases:
--
-- -   For DeleteConfigRule, AWS Config is deleting this rule. Try your
--     request again later.
--
-- -   For DeleteConfigRule, the rule is deleting your evaluation results.
--     Try your request again later.
--
-- -   For DeleteConfigRule, a remediation action is associated with the
--     rule and AWS Config cannot delete this rule. Delete the remediation
--     action associated with the rule before deleting the rule and try
--     your request again later.
--
-- -   For PutConfigOrganizationRule, organization config rule deletion is
--     in progress. Try your request again later.
--
-- -   For DeleteOrganizationConfigRule, organization config rule creation
--     is in progress. Try your request again later.
--
-- -   For PutConformancePack and PutOrganizationConformancePack, a
--     conformance pack creation, update, and deletion is in progress. Try
--     your request again later.
--
-- -   For DeleteConformancePack, a conformance pack creation, update, and
--     deletion is in progress. Try your request again later.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | For @StartConfigRulesEvaluation@ API, this exception is thrown if an
-- evaluation is in progress or if you call the StartConfigRulesEvaluation
-- API more than once per minute.
--
-- For @PutConfigurationAggregator@ API, this exception is thrown if the
-- number of accounts and aggregators exceeds the limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified Amazon KMS Key ARN is not valid.
_InvalidS3KmsKeyArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3KmsKeyArnException =
  Core._MatchServiceError
    defaultService
    "InvalidS3KmsKeyArnException"

-- | You have specified a configuration recorder that does not exist.
_NoSuchConfigurationRecorderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConfigurationRecorderException =
  Core._MatchServiceError
    defaultService
    "NoSuchConfigurationRecorderException"

-- | Your Amazon S3 bucket policy does not permit AWS Config to write to it.
_InsufficientDeliveryPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientDeliveryPolicyException =
  Core._MatchServiceError
    defaultService
    "InsufficientDeliveryPolicyException"

-- | You have reached the limit of the number of organization config rules
-- you can create.
_MaxNumberOfOrganizationConfigRulesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfOrganizationConfigRulesExceededException =
  Core._MatchServiceError
    defaultService
    "MaxNumberOfOrganizationConfigRulesExceededException"

-- | You have reached the limit (6) of the number of organization conformance
-- packs in an account (6 conformance pack with 25 AWS Config rules per
-- pack per account).
_MaxNumberOfOrganizationConformancePacksExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfOrganizationConformancePacksExceededException =
  Core._MatchServiceError
    defaultService
    "MaxNumberOfOrganizationConformancePacksExceededException"

-- | You have specified a resource that does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified limit is outside the allowable range.
_InvalidLimitException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLimitException =
  Core._MatchServiceError
    defaultService
    "InvalidLimitException"

-- | You have specified a template that is not valid or supported.
_OrganizationConformancePackTemplateValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationConformancePackTemplateValidationException =
  Core._MatchServiceError
    defaultService
    "OrganizationConformancePackTemplateValidationException"

-- | You have reached the limit of the number of recorders you can create.
_MaxNumberOfConfigurationRecordersExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfConfigurationRecordersExceededException =
  Core._MatchServiceError
    defaultService
    "MaxNumberOfConfigurationRecordersExceededException"

-- | You tried to delete a remediation exception that does not exist.
_NoSuchRemediationExceptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchRemediationExceptionException =
  Core._MatchServiceError
    defaultService
    "NoSuchRemediationExceptionException"

-- | Two users are trying to modify the same query at the same time. Wait for
-- a moment and try again.
_ResourceConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ResourceConcurrentModificationException"

-- | Failed to add the AWS Config rule because the account already contains
-- the maximum number of 150 rules. Consider deleting any deactivated rules
-- before you add new rules.
_MaxNumberOfConfigRulesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfConfigRulesExceededException =
  Core._MatchServiceError
    defaultService
    "MaxNumberOfConfigRulesExceededException"

-- | There are no configuration recorders available to provide the role
-- needed to describe your resources. Create a configuration recorder.
_NoAvailableConfigurationRecorderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoAvailableConfigurationRecorderException =
  Core._MatchServiceError
    defaultService
    "NoAvailableConfigurationRecorderException"

-- | The specified Amazon S3 bucket does not exist.
_NoSuchBucketException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchBucketException =
  Core._MatchServiceError
    defaultService
    "NoSuchBucketException"

-- | You have reached the limit (100,000) of active custom resource types in
-- your account. Delete unused resources using @DeleteResourceConfig@.
_MaxActiveResourcesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxActiveResourcesExceededException =
  Core._MatchServiceError
    defaultService
    "MaxActiveResourcesExceededException"

-- | Failed to add the retention configuration because a retention
-- configuration with that name already exists.
_MaxNumberOfRetentionConfigurationsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfRetentionConfigurationsExceededException =
  Core._MatchServiceError
    defaultService
    "MaxNumberOfRetentionConfigurationsExceededException"

-- | AWS Config resource cannot be created because your organization does not
-- have all features enabled.
_OrganizationAllFeaturesNotEnabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationAllFeaturesNotEnabledException =
  Core._MatchServiceError
    defaultService
    "OrganizationAllFeaturesNotEnabledException"

-- | Indicates one of the following errors:
--
-- -   For PutConfigRule, the rule cannot be created because the IAM role
--     assigned to AWS Config lacks permissions to perform the config:Put*
--     action.
--
-- -   For PutConfigRule, the AWS Lambda function cannot be invoked. Check
--     the function ARN, and check the function\'s permissions.
--
-- -   For PutOrganizationConfigRule, organization config rule cannot be
--     created because you do not have permissions to call IAM @GetRole@
--     action or create a service linked role.
--
-- -   For PutConformancePack and PutOrganizationConformancePack, a
--     conformance pack cannot be created because you do not have
--     permissions:
--
--     -   To call IAM @GetRole@ action or create a service linked role.
--
--     -   To read Amazon S3 bucket.
_InsufficientPermissionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientPermissionsException =
  Core._MatchServiceError
    defaultService
    "InsufficientPermissionsException"

-- | Remediation action is in progress. You can either cancel execution in
-- AWS Systems Manager or wait and try again later.
_RemediationInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RemediationInProgressException =
  Core._MatchServiceError
    defaultService
    "RemediationInProgressException"

-- | You have specified a resource that is either unknown or has not been
-- discovered.
_ResourceNotDiscoveredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotDiscoveredException =
  Core._MatchServiceError
    defaultService
    "ResourceNotDiscoveredException"
