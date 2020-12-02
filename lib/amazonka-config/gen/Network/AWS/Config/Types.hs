{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types
  ( -- * Service Configuration
    config,

    -- * Errors

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
    AccountAggregationSource,
    accountAggregationSource,
    aasAWSRegions,
    aasAllAWSRegions,
    aasAccountIds,

    -- * AggregateComplianceByConfigRule
    AggregateComplianceByConfigRule,
    aggregateComplianceByConfigRule,
    acbcrCompliance,
    acbcrConfigRuleName,
    acbcrAccountId,
    acbcrAWSRegion,

    -- * AggregateComplianceCount
    AggregateComplianceCount,
    aggregateComplianceCount,
    accGroupName,
    accComplianceSummary,

    -- * AggregateEvaluationResult
    AggregateEvaluationResult,
    aggregateEvaluationResult,
    aerEvaluationResultIdentifier,
    aerAnnotation,
    aerConfigRuleInvokedTime,
    aerResultRecordedTime,
    aerAccountId,
    aerComplianceType,
    aerAWSRegion,

    -- * AggregateResourceIdentifier
    AggregateResourceIdentifier,
    aggregateResourceIdentifier,
    ariResourceName,
    ariSourceAccountId,
    ariSourceRegion,
    ariResourceId,
    ariResourceType,

    -- * AggregatedSourceStatus
    AggregatedSourceStatus,
    aggregatedSourceStatus,
    assLastErrorCode,
    assLastUpdateStatus,
    assSourceType,
    assSourceId,
    assLastErrorMessage,
    assAWSRegion,
    assLastUpdateTime,

    -- * AggregationAuthorization
    AggregationAuthorization,
    aggregationAuthorization,
    aaCreationTime,
    aaAuthorizedAWSRegion,
    aaAggregationAuthorizationARN,
    aaAuthorizedAccountId,

    -- * BaseConfigurationItem
    BaseConfigurationItem,
    baseConfigurationItem,
    bciResourceId,
    bciResourceType,
    bciConfigurationStateId,
    bciArn,
    bciResourceName,
    bciResourceCreationTime,
    bciConfigurationItemStatus,
    bciConfigurationItemCaptureTime,
    bciAccountId,
    bciSupplementaryConfiguration,
    bciAvailabilityZone,
    bciVersion,
    bciAwsRegion,
    bciConfiguration,

    -- * Compliance
    Compliance,
    compliance,
    cComplianceContributorCount,
    cComplianceType,

    -- * ComplianceByConfigRule
    ComplianceByConfigRule,
    complianceByConfigRule,
    cbcrCompliance,
    cbcrConfigRuleName,

    -- * ComplianceByResource
    ComplianceByResource,
    complianceByResource,
    cbrResourceId,
    cbrResourceType,
    cbrCompliance,

    -- * ComplianceContributorCount
    ComplianceContributorCount,
    complianceContributorCount,
    cccCappedCount,
    cccCapExceeded,

    -- * ComplianceSummary
    ComplianceSummary,
    complianceSummary,
    csComplianceSummaryTimestamp,
    csCompliantResourceCount,
    csNonCompliantResourceCount,

    -- * ComplianceSummaryByResourceType
    ComplianceSummaryByResourceType,
    complianceSummaryByResourceType,
    csbrtResourceType,
    csbrtComplianceSummary,

    -- * ConfigExportDeliveryInfo
    ConfigExportDeliveryInfo,
    configExportDeliveryInfo,
    cediLastErrorCode,
    cediLastAttemptTime,
    cediLastSuccessfulTime,
    cediLastStatus,
    cediLastErrorMessage,
    cediNextDeliveryTime,

    -- * ConfigRule
    ConfigRule,
    configRule,
    crInputParameters,
    crConfigRuleName,
    crCreatedBy,
    crMaximumExecutionFrequency,
    crConfigRuleId,
    crScope,
    crConfigRuleState,
    crDescription,
    crConfigRuleARN,
    crSource,

    -- * ConfigRuleComplianceFilters
    ConfigRuleComplianceFilters,
    configRuleComplianceFilters,
    crcfConfigRuleName,
    crcfAccountId,
    crcfComplianceType,
    crcfAWSRegion,

    -- * ConfigRuleComplianceSummaryFilters
    ConfigRuleComplianceSummaryFilters,
    configRuleComplianceSummaryFilters,
    crcsfAccountId,
    crcsfAWSRegion,

    -- * ConfigRuleEvaluationStatus
    ConfigRuleEvaluationStatus,
    configRuleEvaluationStatus,
    cresLastErrorCode,
    cresLastFailedEvaluationTime,
    cresFirstActivatedTime,
    cresLastSuccessfulEvaluationTime,
    cresLastDeactivatedTime,
    cresConfigRuleName,
    cresLastErrorMessage,
    cresConfigRuleId,
    cresLastFailedInvocationTime,
    cresFirstEvaluationStarted,
    cresLastSuccessfulInvocationTime,
    cresConfigRuleARN,

    -- * ConfigSnapshotDeliveryProperties
    ConfigSnapshotDeliveryProperties,
    configSnapshotDeliveryProperties,
    csdpDeliveryFrequency,

    -- * ConfigStreamDeliveryInfo
    ConfigStreamDeliveryInfo,
    configStreamDeliveryInfo,
    csdiLastErrorCode,
    csdiLastStatusChangeTime,
    csdiLastStatus,
    csdiLastErrorMessage,

    -- * ConfigurationAggregator
    ConfigurationAggregator,
    configurationAggregator,
    caConfigurationAggregatorARN,
    caCreationTime,
    caOrganizationAggregationSource,
    caLastUpdatedTime,
    caAccountAggregationSources,
    caCreatedBy,
    caConfigurationAggregatorName,

    -- * ConfigurationItem
    ConfigurationItem,
    configurationItem,
    ciResourceId,
    ciResourceType,
    ciConfigurationStateId,
    ciArn,
    ciResourceName,
    ciResourceCreationTime,
    ciConfigurationItemStatus,
    ciConfigurationItemCaptureTime,
    ciAccountId,
    ciSupplementaryConfiguration,
    ciAvailabilityZone,
    ciRelationships,
    ciVersion,
    ciAwsRegion,
    ciRelatedEvents,
    ciConfiguration,
    ciConfigurationItemMD5Hash,
    ciTags,

    -- * ConfigurationRecorder
    ConfigurationRecorder,
    configurationRecorder,
    crName,
    crRecordingGroup,
    crRoleARN,

    -- * ConfigurationRecorderStatus
    ConfigurationRecorderStatus,
    configurationRecorderStatus,
    crsLastErrorCode,
    crsLastStopTime,
    crsLastStatusChangeTime,
    crsRecording,
    crsLastStatus,
    crsLastErrorMessage,
    crsName,
    crsLastStartTime,

    -- * ConformancePackComplianceFilters
    ConformancePackComplianceFilters,
    conformancePackComplianceFilters,
    cpcfConfigRuleNames,
    cpcfComplianceType,

    -- * ConformancePackComplianceSummary
    ConformancePackComplianceSummary,
    conformancePackComplianceSummary,
    cpcsConformancePackName,
    cpcsConformancePackComplianceStatus,

    -- * ConformancePackDetail
    ConformancePackDetail,
    conformancePackDetail,
    cpdDeliveryS3Bucket,
    cpdDeliveryS3KeyPrefix,
    cpdCreatedBy,
    cpdLastUpdateRequestedTime,
    cpdConformancePackInputParameters,
    cpdConformancePackName,
    cpdConformancePackARN,
    cpdConformancePackId,

    -- * ConformancePackEvaluationFilters
    ConformancePackEvaluationFilters,
    conformancePackEvaluationFilters,
    cpefResourceIds,
    cpefResourceType,
    cpefConfigRuleNames,
    cpefComplianceType,

    -- * ConformancePackEvaluationResult
    ConformancePackEvaluationResult,
    conformancePackEvaluationResult,
    cperAnnotation,
    cperComplianceType,
    cperEvaluationResultIdentifier,
    cperConfigRuleInvokedTime,
    cperResultRecordedTime,

    -- * ConformancePackInputParameter
    ConformancePackInputParameter,
    conformancePackInputParameter,
    cpipParameterName,
    cpipParameterValue,

    -- * ConformancePackRuleCompliance
    ConformancePackRuleCompliance,
    conformancePackRuleCompliance,
    cprcConfigRuleName,
    cprcComplianceType,

    -- * ConformancePackStatusDetail
    ConformancePackStatusDetail,
    conformancePackStatusDetail,
    cpsdConformancePackStatusReason,
    cpsdLastUpdateCompletedTime,
    cpsdConformancePackName,
    cpsdConformancePackId,
    cpsdConformancePackARN,
    cpsdConformancePackState,
    cpsdStackARN,
    cpsdLastUpdateRequestedTime,

    -- * DeliveryChannel
    DeliveryChannel,
    deliveryChannel,
    dcS3KeyPrefix,
    dcSnsTopicARN,
    dcName,
    dcConfigSnapshotDeliveryProperties,
    dcS3BucketName,

    -- * DeliveryChannelStatus
    DeliveryChannelStatus,
    deliveryChannelStatus,
    dcsConfigSnapshotDeliveryInfo,
    dcsConfigStreamDeliveryInfo,
    dcsConfigHistoryDeliveryInfo,
    dcsName,

    -- * Evaluation
    Evaluation,
    evaluation,
    eAnnotation,
    eComplianceResourceType,
    eComplianceResourceId,
    eComplianceType,
    eOrderingTimestamp,

    -- * EvaluationResult
    EvaluationResult,
    evaluationResult,
    erEvaluationResultIdentifier,
    erAnnotation,
    erConfigRuleInvokedTime,
    erResultRecordedTime,
    erResultToken,
    erComplianceType,

    -- * EvaluationResultIdentifier
    EvaluationResultIdentifier,
    evaluationResultIdentifier,
    eriEvaluationResultQualifier,
    eriOrderingTimestamp,

    -- * EvaluationResultQualifier
    EvaluationResultQualifier,
    evaluationResultQualifier,
    erqResourceId,
    erqResourceType,
    erqConfigRuleName,

    -- * ExecutionControls
    ExecutionControls,
    executionControls,
    ecSsmControls,

    -- * FailedDeleteRemediationExceptionsBatch
    FailedDeleteRemediationExceptionsBatch,
    failedDeleteRemediationExceptionsBatch,
    fdrebFailureMessage,
    fdrebFailedItems,

    -- * FailedRemediationBatch
    FailedRemediationBatch,
    failedRemediationBatch,
    frbFailureMessage,
    frbFailedItems,

    -- * FailedRemediationExceptionBatch
    FailedRemediationExceptionBatch,
    failedRemediationExceptionBatch,
    frebFailureMessage,
    frebFailedItems,

    -- * FieldInfo
    FieldInfo,
    fieldInfo,
    fiName,

    -- * GroupedResourceCount
    GroupedResourceCount,
    groupedResourceCount,
    grcGroupName,
    grcResourceCount,

    -- * MemberAccountStatus
    MemberAccountStatus,
    memberAccountStatus,
    masErrorCode,
    masErrorMessage,
    masLastUpdateTime,
    masAccountId,
    masConfigRuleName,
    masMemberAccountRuleStatus,

    -- * OrganizationAggregationSource
    OrganizationAggregationSource,
    organizationAggregationSource,
    oasAWSRegions,
    oasAllAWSRegions,
    oasRoleARN,

    -- * OrganizationConfigRule
    OrganizationConfigRule,
    organizationConfigRule,
    ocrOrganizationManagedRuleMetadata,
    ocrExcludedAccounts,
    ocrOrganizationCustomRuleMetadata,
    ocrLastUpdateTime,
    ocrOrganizationConfigRuleName,
    ocrOrganizationConfigRuleARN,

    -- * OrganizationConfigRuleStatus
    OrganizationConfigRuleStatus,
    organizationConfigRuleStatus,
    ocrsErrorCode,
    ocrsErrorMessage,
    ocrsLastUpdateTime,
    ocrsOrganizationConfigRuleName,
    ocrsOrganizationRuleStatus,

    -- * OrganizationConformancePack
    OrganizationConformancePack,
    organizationConformancePack,
    ocpDeliveryS3Bucket,
    ocpDeliveryS3KeyPrefix,
    ocpConformancePackInputParameters,
    ocpExcludedAccounts,
    ocpOrganizationConformancePackName,
    ocpOrganizationConformancePackARN,
    ocpLastUpdateTime,

    -- * OrganizationConformancePackDetailedStatus
    OrganizationConformancePackDetailedStatus,
    organizationConformancePackDetailedStatus,
    ocpdsErrorCode,
    ocpdsErrorMessage,
    ocpdsLastUpdateTime,
    ocpdsAccountId,
    ocpdsConformancePackName,
    ocpdsStatus,

    -- * OrganizationConformancePackStatus
    OrganizationConformancePackStatus,
    organizationConformancePackStatus,
    ocpsErrorCode,
    ocpsErrorMessage,
    ocpsLastUpdateTime,
    ocpsOrganizationConformancePackName,
    ocpsStatus,

    -- * OrganizationCustomRuleMetadata
    OrganizationCustomRuleMetadata,
    organizationCustomRuleMetadata,
    ocrmInputParameters,
    ocrmResourceIdScope,
    ocrmTagValueScope,
    ocrmMaximumExecutionFrequency,
    ocrmTagKeyScope,
    ocrmResourceTypesScope,
    ocrmDescription,
    ocrmLambdaFunctionARN,
    ocrmOrganizationConfigRuleTriggerTypes,

    -- * OrganizationManagedRuleMetadata
    OrganizationManagedRuleMetadata,
    organizationManagedRuleMetadata,
    omrmInputParameters,
    omrmResourceIdScope,
    omrmTagValueScope,
    omrmMaximumExecutionFrequency,
    omrmTagKeyScope,
    omrmResourceTypesScope,
    omrmDescription,
    omrmRuleIdentifier,

    -- * OrganizationResourceDetailedStatusFilters
    OrganizationResourceDetailedStatusFilters,
    organizationResourceDetailedStatusFilters,
    ordsfStatus,
    ordsfAccountId,

    -- * PendingAggregationRequest
    PendingAggregationRequest,
    pendingAggregationRequest,
    parRequesterAccountId,
    parRequesterAWSRegion,

    -- * QueryInfo
    QueryInfo,
    queryInfo,
    qiSelectFields,

    -- * RecordingGroup
    RecordingGroup,
    recordingGroup,
    rgAllSupported,
    rgIncludeGlobalResourceTypes,
    rgResourceTypes,

    -- * Relationship
    Relationship,
    relationship,
    rResourceId,
    rResourceType,
    rResourceName,
    rRelationshipName,

    -- * RemediationConfiguration
    RemediationConfiguration,
    remediationConfiguration,
    rcResourceType,
    rcARN,
    rcAutomatic,
    rcCreatedByService,
    rcRetryAttemptSeconds,
    rcExecutionControls,
    rcParameters,
    rcMaximumAutomaticAttempts,
    rcTargetVersion,
    rcConfigRuleName,
    rcTargetType,
    rcTargetId,

    -- * RemediationException
    RemediationException,
    remediationException,
    reMessage,
    reExpirationTime,
    reConfigRuleName,
    reResourceType,
    reResourceId,

    -- * RemediationExceptionResourceKey
    RemediationExceptionResourceKey,
    remediationExceptionResourceKey,
    rerkResourceId,
    rerkResourceType,

    -- * RemediationExecutionStatus
    RemediationExecutionStatus,
    remediationExecutionStatus,
    rState,
    rLastUpdatedTime,
    rResourceKey,
    rStepDetails,
    rInvocationTime,

    -- * RemediationExecutionStep
    RemediationExecutionStep,
    remediationExecutionStep,
    resState,
    resStartTime,
    resName,
    resStopTime,
    resErrorMessage,

    -- * RemediationParameterValue
    RemediationParameterValue,
    remediationParameterValue,
    rpvStaticValue,
    rpvResourceValue,

    -- * ResourceCount
    ResourceCount,
    resourceCount,
    resResourceType,
    resCount,

    -- * ResourceCountFilters
    ResourceCountFilters,
    resourceCountFilters,
    rcfResourceType,
    rcfAccountId,
    rcfRegion,

    -- * ResourceFilters
    ResourceFilters,
    resourceFilters,
    rfResourceId,
    rfResourceName,
    rfAccountId,
    rfRegion,

    -- * ResourceIdentifier
    ResourceIdentifier,
    resourceIdentifier,
    riResourceId,
    riResourceType,
    riResourceName,
    riResourceDeletionTime,

    -- * ResourceKey
    ResourceKey,
    resourceKey,
    rkResourceType,
    rkResourceId,

    -- * ResourceValue
    ResourceValue,
    resourceValue,
    rvValue,

    -- * RetentionConfiguration
    RetentionConfiguration,
    retentionConfiguration,
    rcName,
    rcRetentionPeriodInDays,

    -- * Scope
    Scope,
    scope,
    sComplianceResourceTypes,
    sComplianceResourceId,
    sTagValue,
    sTagKey,

    -- * Source
    Source,
    source,
    sSourceDetails,
    sOwner,
    sSourceIdentifier,

    -- * SourceDetail
    SourceDetail,
    sourceDetail,
    sdMessageType,
    sdMaximumExecutionFrequency,
    sdEventSource,

    -- * SsmControls
    SsmControls,
    ssmControls,
    scConcurrentExecutionRatePercentage,
    scErrorPercentage,

    -- * StaticValue
    StaticValue,
    staticValue,
    svValues,

    -- * StatusDetailFilters
    StatusDetailFilters,
    statusDetailFilters,
    sdfMemberAccountRuleStatus,
    sdfAccountId,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,
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
import Network.AWS.Config.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2014-11-12@ of the Amazon Config SDK configuration.
config :: Service
config =
  Service
    { _svcAbbrev = "Config",
      _svcSigner = v4,
      _svcPrefix = "config",
      _svcVersion = "2014-11-12",
      _svcEndpoint = defaultEndpoint config,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Config",
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
