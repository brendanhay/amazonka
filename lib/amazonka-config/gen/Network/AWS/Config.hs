{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Config__
--
-- AWS Config provides a way to keep track of the configurations of all the AWS resources associated with your AWS account. You can use AWS Config to get the current and historical configurations of each AWS resource and also to get information about the relationship between the resources. An AWS resource can be an Amazon Compute Cloud (Amazon EC2) instance, an Elastic Block Store (EBS) volume, an elastic network Interface (ENI), or a security group. For a complete list of resources currently supported by AWS Config, see <https://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resources> .
-- You can access and manage AWS Config through the AWS Management Console, the AWS Command Line Interface (AWS CLI), the AWS Config API, or the AWS SDKs for AWS Config. This reference guide contains documentation for the AWS Config API and the AWS CLI commands that you can use to manage AWS Config. The AWS Config API uses the Signature Version 4 protocol for signing requests. For more information about how to sign a request with this protocol, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> . For detailed information about AWS Config features and their associated actions or commands, as well as how to work with AWS Management Console, see <https://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html What Is AWS Config> in the /AWS Config Developer Guide/ .
module Network.AWS.Config
  ( -- * Service configuration
    configService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribePendingAggregationRequests (Paginated)
    module Network.AWS.Config.DescribePendingAggregationRequests,

    -- ** DescribeRemediationExecutionStatus (Paginated)
    module Network.AWS.Config.DescribeRemediationExecutionStatus,

    -- ** GetResourceConfigHistory (Paginated)
    module Network.AWS.Config.GetResourceConfigHistory,

    -- ** GetAggregateResourceConfig
    module Network.AWS.Config.GetAggregateResourceConfig,

    -- ** DescribeConfigurationAggregators (Paginated)
    module Network.AWS.Config.DescribeConfigurationAggregators,

    -- ** DescribeComplianceByConfigRule (Paginated)
    module Network.AWS.Config.DescribeComplianceByConfigRule,

    -- ** DescribeRetentionConfigurations (Paginated)
    module Network.AWS.Config.DescribeRetentionConfigurations,

    -- ** StopConfigurationRecorder
    module Network.AWS.Config.StopConfigurationRecorder,

    -- ** GetAggregateConfigRuleComplianceSummary
    module Network.AWS.Config.GetAggregateConfigRuleComplianceSummary,

    -- ** ListTagsForResource
    module Network.AWS.Config.ListTagsForResource,

    -- ** BatchGetResourceConfig
    module Network.AWS.Config.BatchGetResourceConfig,

    -- ** DescribeConfigRules (Paginated)
    module Network.AWS.Config.DescribeConfigRules,

    -- ** PutRetentionConfiguration
    module Network.AWS.Config.PutRetentionConfiguration,

    -- ** GetOrganizationConformancePackDetailedStatus
    module Network.AWS.Config.GetOrganizationConformancePackDetailedStatus,

    -- ** DescribeAggregateComplianceByConfigRules (Paginated)
    module Network.AWS.Config.DescribeAggregateComplianceByConfigRules,

    -- ** DeleteEvaluationResults
    module Network.AWS.Config.DeleteEvaluationResults,

    -- ** PutConfigRule
    module Network.AWS.Config.PutConfigRule,

    -- ** GetConformancePackComplianceDetails
    module Network.AWS.Config.GetConformancePackComplianceDetails,

    -- ** DeleteConfigRule
    module Network.AWS.Config.DeleteConfigRule,

    -- ** DeleteRetentionConfiguration
    module Network.AWS.Config.DeleteRetentionConfiguration,

    -- ** SelectResourceConfig
    module Network.AWS.Config.SelectResourceConfig,

    -- ** ListAggregateDiscoveredResources (Paginated)
    module Network.AWS.Config.ListAggregateDiscoveredResources,

    -- ** DescribeOrganizationConfigRuleStatuses
    module Network.AWS.Config.DescribeOrganizationConfigRuleStatuses,

    -- ** DescribeOrganizationConformancePackStatuses
    module Network.AWS.Config.DescribeOrganizationConformancePackStatuses,

    -- ** GetComplianceDetailsByResource (Paginated)
    module Network.AWS.Config.GetComplianceDetailsByResource,

    -- ** DeletePendingAggregationRequest
    module Network.AWS.Config.DeletePendingAggregationRequest,

    -- ** DeliverConfigSnapshot
    module Network.AWS.Config.DeliverConfigSnapshot,

    -- ** BatchGetAggregateResourceConfig
    module Network.AWS.Config.BatchGetAggregateResourceConfig,

    -- ** DescribeConfigRuleEvaluationStatus (Paginated)
    module Network.AWS.Config.DescribeConfigRuleEvaluationStatus,

    -- ** GetDiscoveredResourceCounts
    module Network.AWS.Config.GetDiscoveredResourceCounts,

    -- ** DescribeRemediationExceptions
    module Network.AWS.Config.DescribeRemediationExceptions,

    -- ** DeleteOrganizationConformancePack
    module Network.AWS.Config.DeleteOrganizationConformancePack,

    -- ** PutOrganizationConfigRule
    module Network.AWS.Config.PutOrganizationConfigRule,

    -- ** PutOrganizationConformancePack
    module Network.AWS.Config.PutOrganizationConformancePack,

    -- ** DeleteOrganizationConfigRule
    module Network.AWS.Config.DeleteOrganizationConfigRule,

    -- ** PutResourceConfig
    module Network.AWS.Config.PutResourceConfig,

    -- ** StartConfigRulesEvaluation
    module Network.AWS.Config.StartConfigRulesEvaluation,

    -- ** DescribeOrganizationConfigRules
    module Network.AWS.Config.DescribeOrganizationConfigRules,

    -- ** SelectAggregateResourceConfig
    module Network.AWS.Config.SelectAggregateResourceConfig,

    -- ** DescribeComplianceByResource (Paginated)
    module Network.AWS.Config.DescribeComplianceByResource,

    -- ** DescribeOrganizationConformancePacks
    module Network.AWS.Config.DescribeOrganizationConformancePacks,

    -- ** DeleteResourceConfig
    module Network.AWS.Config.DeleteResourceConfig,

    -- ** PutEvaluations
    module Network.AWS.Config.PutEvaluations,

    -- ** DescribeConfigurationRecorders
    module Network.AWS.Config.DescribeConfigurationRecorders,

    -- ** DescribeConformancePackCompliance
    module Network.AWS.Config.DescribeConformancePackCompliance,

    -- ** GetAggregateComplianceDetailsByConfigRule (Paginated)
    module Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule,

    -- ** GetAggregateDiscoveredResourceCounts
    module Network.AWS.Config.GetAggregateDiscoveredResourceCounts,

    -- ** StartConfigurationRecorder
    module Network.AWS.Config.StartConfigurationRecorder,

    -- ** DescribeConformancePacks
    module Network.AWS.Config.DescribeConformancePacks,

    -- ** DeleteRemediationExceptions
    module Network.AWS.Config.DeleteRemediationExceptions,

    -- ** PutRemediationExceptions
    module Network.AWS.Config.PutRemediationExceptions,

    -- ** GetOrganizationConfigRuleDetailedStatus
    module Network.AWS.Config.GetOrganizationConfigRuleDetailedStatus,

    -- ** PutRemediationConfigurations
    module Network.AWS.Config.PutRemediationConfigurations,

    -- ** DeleteConformancePack
    module Network.AWS.Config.DeleteConformancePack,

    -- ** PutConformancePack
    module Network.AWS.Config.PutConformancePack,

    -- ** StartRemediationExecution
    module Network.AWS.Config.StartRemediationExecution,

    -- ** DescribeConformancePackStatus
    module Network.AWS.Config.DescribeConformancePackStatus,

    -- ** GetComplianceSummaryByConfigRule
    module Network.AWS.Config.GetComplianceSummaryByConfigRule,

    -- ** PutConfigurationAggregator
    module Network.AWS.Config.PutConfigurationAggregator,

    -- ** TagResource
    module Network.AWS.Config.TagResource,

    -- ** DeleteConfigurationAggregator
    module Network.AWS.Config.DeleteConfigurationAggregator,

    -- ** DescribeConfigurationRecorderStatus
    module Network.AWS.Config.DescribeConfigurationRecorderStatus,

    -- ** PutConfigurationRecorder
    module Network.AWS.Config.PutConfigurationRecorder,

    -- ** UntagResource
    module Network.AWS.Config.UntagResource,

    -- ** DeleteConfigurationRecorder
    module Network.AWS.Config.DeleteConfigurationRecorder,

    -- ** GetConformancePackComplianceSummary
    module Network.AWS.Config.GetConformancePackComplianceSummary,

    -- ** GetComplianceSummaryByResourceType
    module Network.AWS.Config.GetComplianceSummaryByResourceType,

    -- ** DescribeDeliveryChannelStatus
    module Network.AWS.Config.DescribeDeliveryChannelStatus,

    -- ** PutDeliveryChannel
    module Network.AWS.Config.PutDeliveryChannel,

    -- ** GetComplianceDetailsByConfigRule (Paginated)
    module Network.AWS.Config.GetComplianceDetailsByConfigRule,

    -- ** DeleteAggregationAuthorization
    module Network.AWS.Config.DeleteAggregationAuthorization,

    -- ** DeleteDeliveryChannel
    module Network.AWS.Config.DeleteDeliveryChannel,

    -- ** DeleteRemediationConfiguration
    module Network.AWS.Config.DeleteRemediationConfiguration,

    -- ** PutAggregationAuthorization
    module Network.AWS.Config.PutAggregationAuthorization,

    -- ** DescribeConfigurationAggregatorSourcesStatus (Paginated)
    module Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus,

    -- ** ListDiscoveredResources (Paginated)
    module Network.AWS.Config.ListDiscoveredResources,

    -- ** DescribeRemediationConfigurations
    module Network.AWS.Config.DescribeRemediationConfigurations,

    -- ** DescribeDeliveryChannels
    module Network.AWS.Config.DescribeDeliveryChannels,

    -- ** DescribeAggregationAuthorizations (Paginated)
    module Network.AWS.Config.DescribeAggregationAuthorizations,

    -- * Types

    -- ** AggregatedSourceStatusType
    AggregatedSourceStatusType (..),

    -- ** AggregatedSourceType
    AggregatedSourceType (..),

    -- ** ChronologicalOrder
    ChronologicalOrder (..),

    -- ** ComplianceType
    ComplianceType (..),

    -- ** ConfigRuleComplianceSummaryGroupKey
    ConfigRuleComplianceSummaryGroupKey (..),

    -- ** ConfigRuleState
    ConfigRuleState (..),

    -- ** ConfigurationItemStatus
    ConfigurationItemStatus (..),

    -- ** ConformancePackComplianceType
    ConformancePackComplianceType (..),

    -- ** ConformancePackState
    ConformancePackState (..),

    -- ** DeliveryStatus
    DeliveryStatus (..),

    -- ** EventSource
    EventSource (..),

    -- ** MaximumExecutionFrequency
    MaximumExecutionFrequency (..),

    -- ** MemberAccountRuleStatus
    MemberAccountRuleStatus (..),

    -- ** MessageType
    MessageType (..),

    -- ** OrganizationConfigRuleTriggerType
    OrganizationConfigRuleTriggerType (..),

    -- ** OrganizationResourceDetailedStatus
    OrganizationResourceDetailedStatus (..),

    -- ** OrganizationResourceStatus
    OrganizationResourceStatus (..),

    -- ** OrganizationRuleStatus
    OrganizationRuleStatus (..),

    -- ** Owner
    Owner (..),

    -- ** RecorderStatus
    RecorderStatus (..),

    -- ** RemediationExecutionState
    RemediationExecutionState (..),

    -- ** RemediationExecutionStepState
    RemediationExecutionStepState (..),

    -- ** RemediationTargetType
    RemediationTargetType (..),

    -- ** ResourceCountGroupKey
    ResourceCountGroupKey (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ResourceValueType
    ResourceValueType (..),

    -- ** AccountAggregationSource
    AccountAggregationSource (..),
    mkAccountAggregationSource,
    aasAWSRegions,
    aasAllAWSRegions,
    aasAccountIds,

    -- ** AggregateComplianceByConfigRule
    AggregateComplianceByConfigRule (..),
    mkAggregateComplianceByConfigRule,
    acbcrCompliance,
    acbcrConfigRuleName,
    acbcrAccountId,
    acbcrAWSRegion,

    -- ** AggregateComplianceCount
    AggregateComplianceCount (..),
    mkAggregateComplianceCount,
    accGroupName,
    accComplianceSummary,

    -- ** AggregateEvaluationResult
    AggregateEvaluationResult (..),
    mkAggregateEvaluationResult,
    aerEvaluationResultIdentifier,
    aerAnnotation,
    aerConfigRuleInvokedTime,
    aerResultRecordedTime,
    aerAccountId,
    aerComplianceType,
    aerAWSRegion,

    -- ** AggregateResourceIdentifier
    AggregateResourceIdentifier (..),
    mkAggregateResourceIdentifier,
    ariResourceName,
    ariSourceAccountId,
    ariSourceRegion,
    ariResourceId,
    ariResourceType,

    -- ** AggregatedSourceStatus
    AggregatedSourceStatus (..),
    mkAggregatedSourceStatus,
    assLastErrorCode,
    assLastUpdateStatus,
    assSourceType,
    assSourceId,
    assLastErrorMessage,
    assAWSRegion,
    assLastUpdateTime,

    -- ** AggregationAuthorization
    AggregationAuthorization (..),
    mkAggregationAuthorization,
    aaCreationTime,
    aaAuthorizedAWSRegion,
    aaAggregationAuthorizationARN,
    aaAuthorizedAccountId,

    -- ** BaseConfigurationItem
    BaseConfigurationItem (..),
    mkBaseConfigurationItem,
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

    -- ** Compliance
    Compliance (..),
    mkCompliance,
    cComplianceContributorCount,
    cComplianceType,

    -- ** ComplianceByConfigRule
    ComplianceByConfigRule (..),
    mkComplianceByConfigRule,
    cbcrCompliance,
    cbcrConfigRuleName,

    -- ** ComplianceByResource
    ComplianceByResource (..),
    mkComplianceByResource,
    cbrResourceId,
    cbrResourceType,
    cbrCompliance,

    -- ** ComplianceContributorCount
    ComplianceContributorCount (..),
    mkComplianceContributorCount,
    cccCappedCount,
    cccCapExceeded,

    -- ** ComplianceSummary
    ComplianceSummary (..),
    mkComplianceSummary,
    csComplianceSummaryTimestamp,
    csCompliantResourceCount,
    csNonCompliantResourceCount,

    -- ** ComplianceSummaryByResourceType
    ComplianceSummaryByResourceType (..),
    mkComplianceSummaryByResourceType,
    csbrtResourceType,
    csbrtComplianceSummary,

    -- ** ConfigExportDeliveryInfo
    ConfigExportDeliveryInfo (..),
    mkConfigExportDeliveryInfo,
    cediLastErrorCode,
    cediLastAttemptTime,
    cediLastSuccessfulTime,
    cediLastStatus,
    cediLastErrorMessage,
    cediNextDeliveryTime,

    -- ** ConfigRule
    ConfigRule (..),
    mkConfigRule,
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

    -- ** ConfigRuleComplianceFilters
    ConfigRuleComplianceFilters (..),
    mkConfigRuleComplianceFilters,
    crcfConfigRuleName,
    crcfAccountId,
    crcfComplianceType,
    crcfAWSRegion,

    -- ** ConfigRuleComplianceSummaryFilters
    ConfigRuleComplianceSummaryFilters (..),
    mkConfigRuleComplianceSummaryFilters,
    crcsfAccountId,
    crcsfAWSRegion,

    -- ** ConfigRuleEvaluationStatus
    ConfigRuleEvaluationStatus (..),
    mkConfigRuleEvaluationStatus,
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

    -- ** ConfigSnapshotDeliveryProperties
    ConfigSnapshotDeliveryProperties (..),
    mkConfigSnapshotDeliveryProperties,
    csdpDeliveryFrequency,

    -- ** ConfigStreamDeliveryInfo
    ConfigStreamDeliveryInfo (..),
    mkConfigStreamDeliveryInfo,
    csdiLastErrorCode,
    csdiLastStatusChangeTime,
    csdiLastStatus,
    csdiLastErrorMessage,

    -- ** ConfigurationAggregator
    ConfigurationAggregator (..),
    mkConfigurationAggregator,
    caConfigurationAggregatorARN,
    caCreationTime,
    caOrganizationAggregationSource,
    caLastUpdatedTime,
    caAccountAggregationSources,
    caCreatedBy,
    caConfigurationAggregatorName,

    -- ** ConfigurationItem
    ConfigurationItem (..),
    mkConfigurationItem,
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

    -- ** ConfigurationRecorder
    ConfigurationRecorder (..),
    mkConfigurationRecorder,
    crName,
    crRecordingGroup,
    crRoleARN,

    -- ** ConfigurationRecorderStatus
    ConfigurationRecorderStatus (..),
    mkConfigurationRecorderStatus,
    crsLastErrorCode,
    crsLastStopTime,
    crsLastStatusChangeTime,
    crsRecording,
    crsLastStatus,
    crsLastErrorMessage,
    crsName,
    crsLastStartTime,

    -- ** ConformancePackComplianceFilters
    ConformancePackComplianceFilters (..),
    mkConformancePackComplianceFilters,
    cpcfConfigRuleNames,
    cpcfComplianceType,

    -- ** ConformancePackComplianceSummary
    ConformancePackComplianceSummary (..),
    mkConformancePackComplianceSummary,
    cpcsConformancePackName,
    cpcsConformancePackComplianceStatus,

    -- ** ConformancePackDetail
    ConformancePackDetail (..),
    mkConformancePackDetail,
    cpdDeliveryS3Bucket,
    cpdDeliveryS3KeyPrefix,
    cpdCreatedBy,
    cpdLastUpdateRequestedTime,
    cpdConformancePackInputParameters,
    cpdConformancePackName,
    cpdConformancePackARN,
    cpdConformancePackId,

    -- ** ConformancePackEvaluationFilters
    ConformancePackEvaluationFilters (..),
    mkConformancePackEvaluationFilters,
    cpefResourceIds,
    cpefResourceType,
    cpefConfigRuleNames,
    cpefComplianceType,

    -- ** ConformancePackEvaluationResult
    ConformancePackEvaluationResult (..),
    mkConformancePackEvaluationResult,
    cperAnnotation,
    cperComplianceType,
    cperEvaluationResultIdentifier,
    cperConfigRuleInvokedTime,
    cperResultRecordedTime,

    -- ** ConformancePackInputParameter
    ConformancePackInputParameter (..),
    mkConformancePackInputParameter,
    cpipParameterName,
    cpipParameterValue,

    -- ** ConformancePackRuleCompliance
    ConformancePackRuleCompliance (..),
    mkConformancePackRuleCompliance,
    cprcConfigRuleName,
    cprcComplianceType,

    -- ** ConformancePackStatusDetail
    ConformancePackStatusDetail (..),
    mkConformancePackStatusDetail,
    cpsdConformancePackStatusReason,
    cpsdLastUpdateCompletedTime,
    cpsdConformancePackName,
    cpsdConformancePackId,
    cpsdConformancePackARN,
    cpsdConformancePackState,
    cpsdStackARN,
    cpsdLastUpdateRequestedTime,

    -- ** DeliveryChannel
    DeliveryChannel (..),
    mkDeliveryChannel,
    dcS3KeyPrefix,
    dcSnsTopicARN,
    dcName,
    dcConfigSnapshotDeliveryProperties,
    dcS3BucketName,

    -- ** DeliveryChannelStatus
    DeliveryChannelStatus (..),
    mkDeliveryChannelStatus,
    dcsConfigSnapshotDeliveryInfo,
    dcsConfigStreamDeliveryInfo,
    dcsConfigHistoryDeliveryInfo,
    dcsName,

    -- ** Evaluation
    Evaluation (..),
    mkEvaluation,
    eAnnotation,
    eComplianceResourceType,
    eComplianceResourceId,
    eComplianceType,
    eOrderingTimestamp,

    -- ** EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
    erEvaluationResultIdentifier,
    erAnnotation,
    erConfigRuleInvokedTime,
    erResultRecordedTime,
    erResultToken,
    erComplianceType,

    -- ** EvaluationResultIdentifier
    EvaluationResultIdentifier (..),
    mkEvaluationResultIdentifier,
    eriEvaluationResultQualifier,
    eriOrderingTimestamp,

    -- ** EvaluationResultQualifier
    EvaluationResultQualifier (..),
    mkEvaluationResultQualifier,
    erqResourceId,
    erqResourceType,
    erqConfigRuleName,

    -- ** ExecutionControls
    ExecutionControls (..),
    mkExecutionControls,
    ecSsmControls,

    -- ** FailedDeleteRemediationExceptionsBatch
    FailedDeleteRemediationExceptionsBatch (..),
    mkFailedDeleteRemediationExceptionsBatch,
    fdrebFailureMessage,
    fdrebFailedItems,

    -- ** FailedRemediationBatch
    FailedRemediationBatch (..),
    mkFailedRemediationBatch,
    frbFailureMessage,
    frbFailedItems,

    -- ** FailedRemediationExceptionBatch
    FailedRemediationExceptionBatch (..),
    mkFailedRemediationExceptionBatch,
    frebFailureMessage,
    frebFailedItems,

    -- ** FieldInfo
    FieldInfo (..),
    mkFieldInfo,
    fiName,

    -- ** GroupedResourceCount
    GroupedResourceCount (..),
    mkGroupedResourceCount,
    grcGroupName,
    grcResourceCount,

    -- ** MemberAccountStatus
    MemberAccountStatus (..),
    mkMemberAccountStatus,
    masErrorCode,
    masErrorMessage,
    masLastUpdateTime,
    masAccountId,
    masConfigRuleName,
    masMemberAccountRuleStatus,

    -- ** OrganizationAggregationSource
    OrganizationAggregationSource (..),
    mkOrganizationAggregationSource,
    oasAWSRegions,
    oasAllAWSRegions,
    oasRoleARN,

    -- ** OrganizationConfigRule
    OrganizationConfigRule (..),
    mkOrganizationConfigRule,
    ocrOrganizationManagedRuleMetadata,
    ocrExcludedAccounts,
    ocrOrganizationCustomRuleMetadata,
    ocrLastUpdateTime,
    ocrOrganizationConfigRuleName,
    ocrOrganizationConfigRuleARN,

    -- ** OrganizationConfigRuleStatus
    OrganizationConfigRuleStatus (..),
    mkOrganizationConfigRuleStatus,
    ocrsErrorCode,
    ocrsErrorMessage,
    ocrsLastUpdateTime,
    ocrsOrganizationConfigRuleName,
    ocrsOrganizationRuleStatus,

    -- ** OrganizationConformancePack
    OrganizationConformancePack (..),
    mkOrganizationConformancePack,
    ocpDeliveryS3Bucket,
    ocpDeliveryS3KeyPrefix,
    ocpConformancePackInputParameters,
    ocpExcludedAccounts,
    ocpOrganizationConformancePackName,
    ocpOrganizationConformancePackARN,
    ocpLastUpdateTime,

    -- ** OrganizationConformancePackDetailedStatus
    OrganizationConformancePackDetailedStatus (..),
    mkOrganizationConformancePackDetailedStatus,
    ocpdsErrorCode,
    ocpdsErrorMessage,
    ocpdsLastUpdateTime,
    ocpdsAccountId,
    ocpdsConformancePackName,
    ocpdsStatus,

    -- ** OrganizationConformancePackStatus
    OrganizationConformancePackStatus (..),
    mkOrganizationConformancePackStatus,
    ocpsErrorCode,
    ocpsErrorMessage,
    ocpsLastUpdateTime,
    ocpsOrganizationConformancePackName,
    ocpsStatus,

    -- ** OrganizationCustomRuleMetadata
    OrganizationCustomRuleMetadata (..),
    mkOrganizationCustomRuleMetadata,
    ocrmInputParameters,
    ocrmResourceIdScope,
    ocrmTagValueScope,
    ocrmMaximumExecutionFrequency,
    ocrmTagKeyScope,
    ocrmResourceTypesScope,
    ocrmDescription,
    ocrmLambdaFunctionARN,
    ocrmOrganizationConfigRuleTriggerTypes,

    -- ** OrganizationManagedRuleMetadata
    OrganizationManagedRuleMetadata (..),
    mkOrganizationManagedRuleMetadata,
    omrmInputParameters,
    omrmResourceIdScope,
    omrmTagValueScope,
    omrmMaximumExecutionFrequency,
    omrmTagKeyScope,
    omrmResourceTypesScope,
    omrmDescription,
    omrmRuleIdentifier,

    -- ** OrganizationResourceDetailedStatusFilters
    OrganizationResourceDetailedStatusFilters (..),
    mkOrganizationResourceDetailedStatusFilters,
    ordsfStatus,
    ordsfAccountId,

    -- ** PendingAggregationRequest
    PendingAggregationRequest (..),
    mkPendingAggregationRequest,
    parRequesterAccountId,
    parRequesterAWSRegion,

    -- ** QueryInfo
    QueryInfo (..),
    mkQueryInfo,
    qiSelectFields,

    -- ** RecordingGroup
    RecordingGroup (..),
    mkRecordingGroup,
    rgAllSupported,
    rgIncludeGlobalResourceTypes,
    rgResourceTypes,

    -- ** Relationship
    Relationship (..),
    mkRelationship,
    rResourceId,
    rResourceType,
    rResourceName,
    rRelationshipName,

    -- ** RemediationConfiguration
    RemediationConfiguration (..),
    mkRemediationConfiguration,
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

    -- ** RemediationException
    RemediationException (..),
    mkRemediationException,
    reMessage,
    reExpirationTime,
    reConfigRuleName,
    reResourceType,
    reResourceId,

    -- ** RemediationExceptionResourceKey
    RemediationExceptionResourceKey (..),
    mkRemediationExceptionResourceKey,
    rerkResourceId,
    rerkResourceType,

    -- ** RemediationExecutionStatus
    RemediationExecutionStatus (..),
    mkRemediationExecutionStatus,
    rState,
    rLastUpdatedTime,
    rResourceKey,
    rStepDetails,
    rInvocationTime,

    -- ** RemediationExecutionStep
    RemediationExecutionStep (..),
    mkRemediationExecutionStep,
    resState,
    resStartTime,
    resName,
    resStopTime,
    resErrorMessage,

    -- ** RemediationParameterValue
    RemediationParameterValue (..),
    mkRemediationParameterValue,
    rpvStaticValue,
    rpvResourceValue,

    -- ** ResourceCount
    ResourceCount (..),
    mkResourceCount,
    resResourceType,
    resCount,

    -- ** ResourceCountFilters
    ResourceCountFilters (..),
    mkResourceCountFilters,
    rcfResourceType,
    rcfAccountId,
    rcfRegion,

    -- ** ResourceFilters
    ResourceFilters (..),
    mkResourceFilters,
    rfResourceId,
    rfResourceName,
    rfAccountId,
    rfRegion,

    -- ** ResourceIdentifier
    ResourceIdentifier (..),
    mkResourceIdentifier,
    riResourceId,
    riResourceType,
    riResourceName,
    riResourceDeletionTime,

    -- ** ResourceKey
    ResourceKey (..),
    mkResourceKey,
    rkResourceType,
    rkResourceId,

    -- ** ResourceValue
    ResourceValue (..),
    mkResourceValue,
    rvValue,

    -- ** RetentionConfiguration
    RetentionConfiguration (..),
    mkRetentionConfiguration,
    rcName,
    rcRetentionPeriodInDays,

    -- ** Scope
    Scope (..),
    mkScope,
    sComplianceResourceTypes,
    sComplianceResourceId,
    sTagValue,
    sTagKey,

    -- ** Source
    Source (..),
    mkSource,
    sSourceDetails,
    sOwner,
    sSourceIdentifier,

    -- ** SourceDetail
    SourceDetail (..),
    mkSourceDetail,
    sdMessageType,
    sdMaximumExecutionFrequency,
    sdEventSource,

    -- ** SsmControls
    SsmControls (..),
    mkSsmControls,
    scConcurrentExecutionRatePercentage,
    scErrorPercentage,

    -- ** StaticValue
    StaticValue (..),
    mkStaticValue,
    svValues,

    -- ** StatusDetailFilters
    StatusDetailFilters (..),
    mkStatusDetailFilters,
    sdfMemberAccountRuleStatus,
    sdfAccountId,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.Config.BatchGetAggregateResourceConfig
import Network.AWS.Config.BatchGetResourceConfig
import Network.AWS.Config.DeleteAggregationAuthorization
import Network.AWS.Config.DeleteConfigRule
import Network.AWS.Config.DeleteConfigurationAggregator
import Network.AWS.Config.DeleteConfigurationRecorder
import Network.AWS.Config.DeleteConformancePack
import Network.AWS.Config.DeleteDeliveryChannel
import Network.AWS.Config.DeleteEvaluationResults
import Network.AWS.Config.DeleteOrganizationConfigRule
import Network.AWS.Config.DeleteOrganizationConformancePack
import Network.AWS.Config.DeletePendingAggregationRequest
import Network.AWS.Config.DeleteRemediationConfiguration
import Network.AWS.Config.DeleteRemediationExceptions
import Network.AWS.Config.DeleteResourceConfig
import Network.AWS.Config.DeleteRetentionConfiguration
import Network.AWS.Config.DeliverConfigSnapshot
import Network.AWS.Config.DescribeAggregateComplianceByConfigRules
import Network.AWS.Config.DescribeAggregationAuthorizations
import Network.AWS.Config.DescribeComplianceByConfigRule
import Network.AWS.Config.DescribeComplianceByResource
import Network.AWS.Config.DescribeConfigRuleEvaluationStatus
import Network.AWS.Config.DescribeConfigRules
import Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus
import Network.AWS.Config.DescribeConfigurationAggregators
import Network.AWS.Config.DescribeConfigurationRecorderStatus
import Network.AWS.Config.DescribeConfigurationRecorders
import Network.AWS.Config.DescribeConformancePackCompliance
import Network.AWS.Config.DescribeConformancePackStatus
import Network.AWS.Config.DescribeConformancePacks
import Network.AWS.Config.DescribeDeliveryChannelStatus
import Network.AWS.Config.DescribeDeliveryChannels
import Network.AWS.Config.DescribeOrganizationConfigRuleStatuses
import Network.AWS.Config.DescribeOrganizationConfigRules
import Network.AWS.Config.DescribeOrganizationConformancePackStatuses
import Network.AWS.Config.DescribeOrganizationConformancePacks
import Network.AWS.Config.DescribePendingAggregationRequests
import Network.AWS.Config.DescribeRemediationConfigurations
import Network.AWS.Config.DescribeRemediationExceptions
import Network.AWS.Config.DescribeRemediationExecutionStatus
import Network.AWS.Config.DescribeRetentionConfigurations
import Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule
import Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
import Network.AWS.Config.GetAggregateDiscoveredResourceCounts
import Network.AWS.Config.GetAggregateResourceConfig
import Network.AWS.Config.GetComplianceDetailsByConfigRule
import Network.AWS.Config.GetComplianceDetailsByResource
import Network.AWS.Config.GetComplianceSummaryByConfigRule
import Network.AWS.Config.GetComplianceSummaryByResourceType
import Network.AWS.Config.GetConformancePackComplianceDetails
import Network.AWS.Config.GetConformancePackComplianceSummary
import Network.AWS.Config.GetDiscoveredResourceCounts
import Network.AWS.Config.GetOrganizationConfigRuleDetailedStatus
import Network.AWS.Config.GetOrganizationConformancePackDetailedStatus
import Network.AWS.Config.GetResourceConfigHistory
import Network.AWS.Config.ListAggregateDiscoveredResources
import Network.AWS.Config.ListDiscoveredResources
import Network.AWS.Config.ListTagsForResource
import Network.AWS.Config.PutAggregationAuthorization
import Network.AWS.Config.PutConfigRule
import Network.AWS.Config.PutConfigurationAggregator
import Network.AWS.Config.PutConfigurationRecorder
import Network.AWS.Config.PutConformancePack
import Network.AWS.Config.PutDeliveryChannel
import Network.AWS.Config.PutEvaluations
import Network.AWS.Config.PutOrganizationConfigRule
import Network.AWS.Config.PutOrganizationConformancePack
import Network.AWS.Config.PutRemediationConfigurations
import Network.AWS.Config.PutRemediationExceptions
import Network.AWS.Config.PutResourceConfig
import Network.AWS.Config.PutRetentionConfiguration
import Network.AWS.Config.SelectAggregateResourceConfig
import Network.AWS.Config.SelectResourceConfig
import Network.AWS.Config.StartConfigRulesEvaluation
import Network.AWS.Config.StartConfigurationRecorder
import Network.AWS.Config.StartRemediationExecution
import Network.AWS.Config.StopConfigurationRecorder
import Network.AWS.Config.TagResource
import Network.AWS.Config.Types
import Network.AWS.Config.UntagResource
import Network.AWS.Config.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Config'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
