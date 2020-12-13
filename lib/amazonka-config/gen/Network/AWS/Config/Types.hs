-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types
  ( -- * Service configuration
    configService,

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
    AccountAggregationSource (..),
    mkAccountAggregationSource,
    aasAccountIds,
    aasAWSRegions,
    aasAllAWSRegions,

    -- * AggregateComplianceByConfigRule
    AggregateComplianceByConfigRule (..),
    mkAggregateComplianceByConfigRule,
    acbcrCompliance,
    acbcrConfigRuleName,
    acbcrAccountId,
    acbcrAWSRegion,

    -- * AggregateComplianceCount
    AggregateComplianceCount (..),
    mkAggregateComplianceCount,
    accGroupName,
    accComplianceSummary,

    -- * AggregateEvaluationResult
    AggregateEvaluationResult (..),
    mkAggregateEvaluationResult,
    aerEvaluationResultIdentifier,
    aerAnnotation,
    aerConfigRuleInvokedTime,
    aerResultRecordedTime,
    aerAccountId,
    aerComplianceType,
    aerAWSRegion,

    -- * AggregateResourceIdentifier
    AggregateResourceIdentifier (..),
    mkAggregateResourceIdentifier,
    ariResourceId,
    ariResourceType,
    ariSourceRegion,
    ariResourceName,
    ariSourceAccountId,

    -- * AggregatedSourceStatus
    AggregatedSourceStatus (..),
    mkAggregatedSourceStatus,
    assLastErrorCode,
    assLastUpdateStatus,
    assSourceType,
    assSourceId,
    assLastErrorMessage,
    assAWSRegion,
    assLastUpdateTime,

    -- * AggregationAuthorization
    AggregationAuthorization (..),
    mkAggregationAuthorization,
    aaCreationTime,
    aaAuthorizedAWSRegion,
    aaAggregationAuthorizationARN,
    aaAuthorizedAccountId,

    -- * BaseConfigurationItem
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

    -- * Compliance
    Compliance (..),
    mkCompliance,
    cComplianceContributorCount,
    cComplianceType,

    -- * ComplianceByConfigRule
    ComplianceByConfigRule (..),
    mkComplianceByConfigRule,
    cbcrCompliance,
    cbcrConfigRuleName,

    -- * ComplianceByResource
    ComplianceByResource (..),
    mkComplianceByResource,
    cbrResourceId,
    cbrResourceType,
    cbrCompliance,

    -- * ComplianceContributorCount
    ComplianceContributorCount (..),
    mkComplianceContributorCount,
    cccCappedCount,
    cccCapExceeded,

    -- * ComplianceSummary
    ComplianceSummary (..),
    mkComplianceSummary,
    csComplianceSummaryTimestamp,
    csCompliantResourceCount,
    csNonCompliantResourceCount,

    -- * ComplianceSummaryByResourceType
    ComplianceSummaryByResourceType (..),
    mkComplianceSummaryByResourceType,
    csbrtResourceType,
    csbrtComplianceSummary,

    -- * ConfigExportDeliveryInfo
    ConfigExportDeliveryInfo (..),
    mkConfigExportDeliveryInfo,
    cediLastErrorCode,
    cediLastAttemptTime,
    cediLastSuccessfulTime,
    cediLastStatus,
    cediLastErrorMessage,
    cediNextDeliveryTime,

    -- * ConfigRule
    ConfigRule (..),
    mkConfigRule,
    crInputParameters,
    crConfigRuleName,
    crCreatedBy,
    crMaximumExecutionFrequency,
    crConfigRuleId,
    crScope,
    crSource,
    crConfigRuleState,
    crDescription,
    crConfigRuleARN,

    -- * ConfigRuleComplianceFilters
    ConfigRuleComplianceFilters (..),
    mkConfigRuleComplianceFilters,
    crcfConfigRuleName,
    crcfAccountId,
    crcfComplianceType,
    crcfAWSRegion,

    -- * ConfigRuleComplianceSummaryFilters
    ConfigRuleComplianceSummaryFilters (..),
    mkConfigRuleComplianceSummaryFilters,
    crcsfAccountId,
    crcsfAWSRegion,

    -- * ConfigRuleEvaluationStatus
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

    -- * ConfigSnapshotDeliveryProperties
    ConfigSnapshotDeliveryProperties (..),
    mkConfigSnapshotDeliveryProperties,
    csdpDeliveryFrequency,

    -- * ConfigStreamDeliveryInfo
    ConfigStreamDeliveryInfo (..),
    mkConfigStreamDeliveryInfo,
    csdiLastErrorCode,
    csdiLastStatusChangeTime,
    csdiLastStatus,
    csdiLastErrorMessage,

    -- * ConfigurationAggregator
    ConfigurationAggregator (..),
    mkConfigurationAggregator,
    caConfigurationAggregatorARN,
    caCreationTime,
    caOrganizationAggregationSource,
    caLastUpdatedTime,
    caAccountAggregationSources,
    caCreatedBy,
    caConfigurationAggregatorName,

    -- * ConfigurationItem
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

    -- * ConfigurationRecorder
    ConfigurationRecorder (..),
    mkConfigurationRecorder,
    crName,
    crRecordingGroup,
    crRoleARN,

    -- * ConfigurationRecorderStatus
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

    -- * ConformancePackComplianceFilters
    ConformancePackComplianceFilters (..),
    mkConformancePackComplianceFilters,
    cpcfConfigRuleNames,
    cpcfComplianceType,

    -- * ConformancePackComplianceSummary
    ConformancePackComplianceSummary (..),
    mkConformancePackComplianceSummary,
    cpcsConformancePackName,
    cpcsConformancePackComplianceStatus,

    -- * ConformancePackDetail
    ConformancePackDetail (..),
    mkConformancePackDetail,
    cpdDeliveryS3Bucket,
    cpdConformancePackName,
    cpdDeliveryS3KeyPrefix,
    cpdCreatedBy,
    cpdLastUpdateRequestedTime,
    cpdConformancePackId,
    cpdConformancePackInputParameters,
    cpdConformancePackARN,

    -- * ConformancePackEvaluationFilters
    ConformancePackEvaluationFilters (..),
    mkConformancePackEvaluationFilters,
    cpefResourceIds,
    cpefResourceType,
    cpefConfigRuleNames,
    cpefComplianceType,

    -- * ConformancePackEvaluationResult
    ConformancePackEvaluationResult (..),
    mkConformancePackEvaluationResult,
    cperEvaluationResultIdentifier,
    cperAnnotation,
    cperConfigRuleInvokedTime,
    cperResultRecordedTime,
    cperComplianceType,

    -- * ConformancePackInputParameter
    ConformancePackInputParameter (..),
    mkConformancePackInputParameter,
    cpipParameterValue,
    cpipParameterName,

    -- * ConformancePackRuleCompliance
    ConformancePackRuleCompliance (..),
    mkConformancePackRuleCompliance,
    cprcConfigRuleName,
    cprcComplianceType,

    -- * ConformancePackStatusDetail
    ConformancePackStatusDetail (..),
    mkConformancePackStatusDetail,
    cpsdConformancePackStatusReason,
    cpsdStackARN,
    cpsdLastUpdateCompletedTime,
    cpsdConformancePackName,
    cpsdLastUpdateRequestedTime,
    cpsdConformancePackId,
    cpsdConformancePackState,
    cpsdConformancePackARN,

    -- * DeliveryChannel
    DeliveryChannel (..),
    mkDeliveryChannel,
    dcS3KeyPrefix,
    dcSnsTopicARN,
    dcName,
    dcConfigSnapshotDeliveryProperties,
    dcS3BucketName,

    -- * DeliveryChannelStatus
    DeliveryChannelStatus (..),
    mkDeliveryChannelStatus,
    dcsConfigSnapshotDeliveryInfo,
    dcsConfigStreamDeliveryInfo,
    dcsConfigHistoryDeliveryInfo,
    dcsName,

    -- * Evaluation
    Evaluation (..),
    mkEvaluation,
    eAnnotation,
    eComplianceResourceType,
    eComplianceResourceId,
    eOrderingTimestamp,
    eComplianceType,

    -- * EvaluationResult
    EvaluationResult (..),
    mkEvaluationResult,
    erEvaluationResultIdentifier,
    erAnnotation,
    erConfigRuleInvokedTime,
    erResultRecordedTime,
    erResultToken,
    erComplianceType,

    -- * EvaluationResultIdentifier
    EvaluationResultIdentifier (..),
    mkEvaluationResultIdentifier,
    eriEvaluationResultQualifier,
    eriOrderingTimestamp,

    -- * EvaluationResultQualifier
    EvaluationResultQualifier (..),
    mkEvaluationResultQualifier,
    erqResourceId,
    erqResourceType,
    erqConfigRuleName,

    -- * ExecutionControls
    ExecutionControls (..),
    mkExecutionControls,
    ecSsmControls,

    -- * FailedDeleteRemediationExceptionsBatch
    FailedDeleteRemediationExceptionsBatch (..),
    mkFailedDeleteRemediationExceptionsBatch,
    fdrebFailureMessage,
    fdrebFailedItems,

    -- * FailedRemediationBatch
    FailedRemediationBatch (..),
    mkFailedRemediationBatch,
    frbFailureMessage,
    frbFailedItems,

    -- * FailedRemediationExceptionBatch
    FailedRemediationExceptionBatch (..),
    mkFailedRemediationExceptionBatch,
    frebFailureMessage,
    frebFailedItems,

    -- * FieldInfo
    FieldInfo (..),
    mkFieldInfo,
    fiName,

    -- * GroupedResourceCount
    GroupedResourceCount (..),
    mkGroupedResourceCount,
    grcGroupName,
    grcResourceCount,

    -- * MemberAccountStatus
    MemberAccountStatus (..),
    mkMemberAccountStatus,
    masMemberAccountRuleStatus,
    masConfigRuleName,
    masAccountId,
    masErrorCode,
    masErrorMessage,
    masLastUpdateTime,

    -- * OrganizationAggregationSource
    OrganizationAggregationSource (..),
    mkOrganizationAggregationSource,
    oasAWSRegions,
    oasAllAWSRegions,
    oasRoleARN,

    -- * OrganizationConfigRule
    OrganizationConfigRule (..),
    mkOrganizationConfigRule,
    ocrOrganizationManagedRuleMetadata,
    ocrOrganizationConfigRuleARN,
    ocrOrganizationConfigRuleName,
    ocrExcludedAccounts,
    ocrOrganizationCustomRuleMetadata,
    ocrLastUpdateTime,

    -- * OrganizationConfigRuleStatus
    OrganizationConfigRuleStatus (..),
    mkOrganizationConfigRuleStatus,
    ocrsOrganizationRuleStatus,
    ocrsErrorCode,
    ocrsOrganizationConfigRuleName,
    ocrsErrorMessage,
    ocrsLastUpdateTime,

    -- * OrganizationConformancePack
    OrganizationConformancePack (..),
    mkOrganizationConformancePack,
    ocpOrganizationConformancePackARN,
    ocpDeliveryS3Bucket,
    ocpOrganizationConformancePackName,
    ocpDeliveryS3KeyPrefix,
    ocpConformancePackInputParameters,
    ocpExcludedAccounts,
    ocpLastUpdateTime,

    -- * OrganizationConformancePackDetailedStatus
    OrganizationConformancePackDetailedStatus (..),
    mkOrganizationConformancePackDetailedStatus,
    ocpdsStatus,
    ocpdsConformancePackName,
    ocpdsAccountId,
    ocpdsErrorCode,
    ocpdsErrorMessage,
    ocpdsLastUpdateTime,

    -- * OrganizationConformancePackStatus
    OrganizationConformancePackStatus (..),
    mkOrganizationConformancePackStatus,
    ocpsStatus,
    ocpsOrganizationConformancePackName,
    ocpsErrorCode,
    ocpsErrorMessage,
    ocpsLastUpdateTime,

    -- * OrganizationCustomRuleMetadata
    OrganizationCustomRuleMetadata (..),
    mkOrganizationCustomRuleMetadata,
    ocrmInputParameters,
    ocrmResourceIdScope,
    ocrmLambdaFunctionARN,
    ocrmTagValueScope,
    ocrmMaximumExecutionFrequency,
    ocrmOrganizationConfigRuleTriggerTypes,
    ocrmTagKeyScope,
    ocrmResourceTypesScope,
    ocrmDescription,

    -- * OrganizationManagedRuleMetadata
    OrganizationManagedRuleMetadata (..),
    mkOrganizationManagedRuleMetadata,
    omrmInputParameters,
    omrmResourceIdScope,
    omrmTagValueScope,
    omrmMaximumExecutionFrequency,
    omrmTagKeyScope,
    omrmRuleIdentifier,
    omrmResourceTypesScope,
    omrmDescription,

    -- * OrganizationResourceDetailedStatusFilters
    OrganizationResourceDetailedStatusFilters (..),
    mkOrganizationResourceDetailedStatusFilters,
    ordsfStatus,
    ordsfAccountId,

    -- * PendingAggregationRequest
    PendingAggregationRequest (..),
    mkPendingAggregationRequest,
    parRequesterAccountId,
    parRequesterAWSRegion,

    -- * QueryInfo
    QueryInfo (..),
    mkQueryInfo,
    qiSelectFields,

    -- * RecordingGroup
    RecordingGroup (..),
    mkRecordingGroup,
    rgAllSupported,
    rgIncludeGlobalResourceTypes,
    rgResourceTypes,

    -- * Relationship
    Relationship (..),
    mkRelationship,
    rResourceId,
    rResourceType,
    rResourceName,
    rRelationshipName,

    -- * RemediationConfiguration
    RemediationConfiguration (..),
    mkRemediationConfiguration,
    rcTargetId,
    rcResourceType,
    rcARN,
    rcAutomatic,
    rcConfigRuleName,
    rcCreatedByService,
    rcRetryAttemptSeconds,
    rcExecutionControls,
    rcTargetType,
    rcParameters,
    rcMaximumAutomaticAttempts,
    rcTargetVersion,

    -- * RemediationException
    RemediationException (..),
    mkRemediationException,
    reResourceId,
    reResourceType,
    reConfigRuleName,
    reMessage,
    reExpirationTime,

    -- * RemediationExceptionResourceKey
    RemediationExceptionResourceKey (..),
    mkRemediationExceptionResourceKey,
    rerkResourceId,
    rerkResourceType,

    -- * RemediationExecutionStatus
    RemediationExecutionStatus (..),
    mkRemediationExecutionStatus,
    rState,
    rLastUpdatedTime,
    rResourceKey,
    rStepDetails,
    rInvocationTime,

    -- * RemediationExecutionStep
    RemediationExecutionStep (..),
    mkRemediationExecutionStep,
    resState,
    resStartTime,
    resName,
    resStopTime,
    resErrorMessage,

    -- * RemediationParameterValue
    RemediationParameterValue (..),
    mkRemediationParameterValue,
    rpvStaticValue,
    rpvResourceValue,

    -- * ResourceCount
    ResourceCount (..),
    mkResourceCount,
    rcgResourceType,
    rcgCount,

    -- * ResourceCountFilters
    ResourceCountFilters (..),
    mkResourceCountFilters,
    rcfResourceType,
    rcfAccountId,
    rcfRegion,

    -- * ResourceFilters
    ResourceFilters (..),
    mkResourceFilters,
    rfResourceId,
    rfResourceName,
    rfAccountId,
    rfRegion,

    -- * ResourceIdentifier
    ResourceIdentifier (..),
    mkResourceIdentifier,
    riResourceId,
    riResourceType,
    riResourceName,
    riResourceDeletionTime,

    -- * ResourceKey
    ResourceKey (..),
    mkResourceKey,
    rkResourceId,
    rkResourceType,

    -- * ResourceValue
    ResourceValue (..),
    mkResourceValue,
    rvValue,

    -- * RetentionConfiguration
    RetentionConfiguration (..),
    mkRetentionConfiguration,
    rcName,
    rcRetentionPeriodInDays,

    -- * Scope
    Scope (..),
    mkScope,
    sComplianceResourceTypes,
    sComplianceResourceId,
    sTagValue,
    sTagKey,

    -- * Source
    Source (..),
    mkSource,
    sSourceIdentifier,
    sOwner,
    sSourceDetails,

    -- * SourceDetail
    SourceDetail (..),
    mkSourceDetail,
    sdMessageType,
    sdMaximumExecutionFrequency,
    sdEventSource,

    -- * SsmControls
    SsmControls (..),
    mkSsmControls,
    scConcurrentExecutionRatePercentage,
    scErrorPercentage,

    -- * StaticValue
    StaticValue (..),
    mkStaticValue,
    svValues,

    -- * StatusDetailFilters
    StatusDetailFilters (..),
    mkStatusDetailFilters,
    sdfMemberAccountRuleStatus,
    sdfAccountId,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-11-12@ of the Amazon Config SDK configuration.
configService :: Lude.Service
configService =
  Lude.Service
    { Lude._svcAbbrev = "Config",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "config",
      Lude._svcVersion = "2014-11-12",
      Lude._svcEndpoint = Lude.defaultEndpoint configService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Config",
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
