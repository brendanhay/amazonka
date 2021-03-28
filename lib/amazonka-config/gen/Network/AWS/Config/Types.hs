-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _NoSuchRemediationConfigurationException
    , _InvalidTimeRangeException
    , _NoSuchOrganizationConformancePackException
    , _InvalidSNSTopicARNException
    , _InvalidRecordingGroupException
    , _InvalidExpressionException
    , _NoAvailableOrganizationException
    , _ValidationException
    , _OrganizationAccessDeniedException
    , _NoSuchConfigurationAggregatorException
    , _InvalidRoleException
    , _OversizedConfigurationItemException
    , _LastDeliveryChannelDeleteFailedException
    , _ConformancePackTemplateValidationException
    , _NoSuchRemediationExceptionException
    , _InvalidLimitException
    , _MaxNumberOfOrganizationConformancePacksExceededException
    , _InvalidDeliveryChannelNameException
    , _TooManyTagsException
    , _InvalidParameterValueException
    , _InvalidResultTokenException
    , _NoSuchConfigRuleInConformancePackException
    , _NoSuchOrganizationConfigRuleException
    , _NoSuchDeliveryChannelException
    , _NoSuchConfigRuleException
    , _NoSuchConformancePackException
    , _NoSuchRetentionConfigurationException
    , _RemediationInProgressException
    , _OrganizationAllFeaturesNotEnabledException
    , _InsufficientPermissionsException
    , _ResourceNotDiscoveredException
    , _InvalidNextTokenException
    , _MaxNumberOfRetentionConfigurationsExceededException
    , _MaxNumberOfConformancePacksExceededException
    , _MaxNumberOfConfigRulesExceededException
    , _NoAvailableConfigurationRecorderException
    , _NoSuchBucketException
    , _MaxActiveResourcesExceededException
    , _NoAvailableDeliveryChannelException
    , _OrganizationConformancePackTemplateValidationException
    , _InvalidConfigurationRecorderNameException
    , _NoRunningConfigurationRecorderException
    , _MaxNumberOfConfigurationRecordersExceededException
    , _InsufficientDeliveryPolicyException
    , _MaxNumberOfDeliveryChannelsExceededException
    , _MaxNumberOfOrganizationConfigRulesExceededException
    , _ResourceNotFoundException
    , _NoSuchConfigurationRecorderException
    , _InvalidS3KeyPrefixException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ConfigurationAggregatorArn
    , ConfigurationAggregatorArn (..)

    -- * StringWithCharLimit2048
    , StringWithCharLimit2048 (..)

    -- * EvaluationResultIdentifier
    , EvaluationResultIdentifier (..)
    , mkEvaluationResultIdentifier
    , eriEvaluationResultQualifier
    , eriOrderingTimestamp

    -- * ConformancePackStatusReason
    , ConformancePackStatusReason (..)

    -- * SourceDetail
    , SourceDetail (..)
    , mkSourceDetail
    , sdEventSource
    , sdMaximumExecutionFrequency
    , sdMessageType

    -- * ConfigExportDeliveryInfo
    , ConfigExportDeliveryInfo (..)
    , mkConfigExportDeliveryInfo
    , cediLastAttemptTime
    , cediLastErrorCode
    , cediLastErrorMessage
    , cediLastStatus
    , cediLastSuccessfulTime
    , cediNextDeliveryTime

    -- * AggregatedSourceStatusType
    , AggregatedSourceStatusType (..)

    -- * SupplementaryConfigurationValue
    , SupplementaryConfigurationValue (..)

    -- * ConfigStreamDeliveryInfo
    , ConfigStreamDeliveryInfo (..)
    , mkConfigStreamDeliveryInfo
    , csdiLastErrorCode
    , csdiLastErrorMessage
    , csdiLastStatus
    , csdiLastStatusChangeTime

    -- * Relationship
    , Relationship (..)
    , mkRelationship
    , rRelationshipName
    , rResourceId
    , rResourceName
    , rResourceType

    -- * FieldInfo
    , FieldInfo (..)
    , mkFieldInfo
    , fiName

    -- * ConformancePackRuleCompliance
    , ConformancePackRuleCompliance (..)
    , mkConformancePackRuleCompliance
    , cprcComplianceType
    , cprcConfigRuleName

    -- * EvaluationResultQualifier
    , EvaluationResultQualifier (..)
    , mkEvaluationResultQualifier
    , erqConfigRuleName
    , erqResourceId
    , erqResourceType

    -- * Annotation
    , Annotation (..)

    -- * ResourceTypeString
    , ResourceTypeString (..)

    -- * FailedRemediationExceptionBatch
    , FailedRemediationExceptionBatch (..)
    , mkFailedRemediationExceptionBatch
    , frebFailedItems
    , frebFailureMessage

    -- * OrganizationConfigRule
    , OrganizationConfigRule (..)
    , mkOrganizationConfigRule
    , ocrOrganizationConfigRuleName
    , ocrOrganizationConfigRuleArn
    , ocrExcludedAccounts
    , ocrLastUpdateTime
    , ocrOrganizationCustomRuleMetadata
    , ocrOrganizationManagedRuleMetadata

    -- * RemediationExecutionStep
    , RemediationExecutionStep (..)
    , mkRemediationExecutionStep
    , resErrorMessage
    , resName
    , resStartTime
    , resState
    , resStopTime

    -- * EvaluationResult
    , EvaluationResult (..)
    , mkEvaluationResult
    , erAnnotation
    , erComplianceType
    , erConfigRuleInvokedTime
    , erEvaluationResultIdentifier
    , erResultRecordedTime
    , erResultToken

    -- * GroupedResourceCount
    , GroupedResourceCount (..)
    , mkGroupedResourceCount
    , grcGroupName
    , grcResourceCount

    -- * ResourceId
    , ResourceId (..)

    -- * AggregateComplianceCount
    , AggregateComplianceCount (..)
    , mkAggregateComplianceCount
    , accComplianceSummary
    , accGroupName

    -- * DeliveryChannel
    , DeliveryChannel (..)
    , mkDeliveryChannel
    , dcConfigSnapshotDeliveryProperties
    , dcName
    , dcS3BucketName
    , dcS3KeyPrefix
    , dcSnsTopicARN

    -- * PendingAggregationRequest
    , PendingAggregationRequest (..)
    , mkPendingAggregationRequest
    , parRequesterAccountId
    , parRequesterAwsRegion

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * OrganizationAggregationSource
    , OrganizationAggregationSource (..)
    , mkOrganizationAggregationSource
    , oasRoleArn
    , oasAllAwsRegions
    , oasAwsRegions

    -- * RemediationExecutionState
    , RemediationExecutionState (..)

    -- * ChronologicalOrder
    , ChronologicalOrder (..)

    -- * ConformancePackComplianceFilters
    , ConformancePackComplianceFilters (..)
    , mkConformancePackComplianceFilters
    , cpcfComplianceType
    , cpcfConfigRuleNames

    -- * ConfigRuleComplianceFilters
    , ConfigRuleComplianceFilters (..)
    , mkConfigRuleComplianceFilters
    , crcfAccountId
    , crcfAwsRegion
    , crcfComplianceType
    , crcfConfigRuleName

    -- * ResourceType
    , ResourceType (..)

    -- * ParameterValue
    , ParameterValue (..)

    -- * ResourceCountFilters
    , ResourceCountFilters (..)
    , mkResourceCountFilters
    , rcfAccountId
    , rcfRegion
    , rcfResourceType

    -- * DeliveryS3Bucket
    , DeliveryS3Bucket (..)

    -- * BaseResourceId
    , BaseResourceId (..)

    -- * ConfigurationStateId
    , ConfigurationStateId (..)

    -- * RemediationExceptionResourceKey
    , RemediationExceptionResourceKey (..)
    , mkRemediationExceptionResourceKey
    , rerkResourceId
    , rerkResourceType

    -- * ConformancePackDetail
    , ConformancePackDetail (..)
    , mkConformancePackDetail
    , cpdConformancePackName
    , cpdConformancePackArn
    , cpdConformancePackId
    , cpdConformancePackInputParameters
    , cpdCreatedBy
    , cpdDeliveryS3Bucket
    , cpdDeliveryS3KeyPrefix
    , cpdLastUpdateRequestedTime

    -- * ComplianceContributorCount
    , ComplianceContributorCount (..)
    , mkComplianceContributorCount
    , cccCapExceeded
    , cccCappedCount

    -- * FailedDeleteRemediationExceptionsBatch
    , FailedDeleteRemediationExceptionsBatch (..)
    , mkFailedDeleteRemediationExceptionsBatch
    , fdrebFailedItems
    , fdrebFailureMessage

    -- * ARN
    , ARN (..)

    -- * ResourceName
    , ResourceName (..)

    -- * ConformancePackComplianceSummary
    , ConformancePackComplianceSummary (..)
    , mkConformancePackComplianceSummary
    , cpcsConformancePackName
    , cpcsConformancePackComplianceStatus

    -- * ResourceKey
    , ResourceKey (..)
    , mkResourceKey
    , rkResourceType
    , rkResourceId

    -- * OrganizationResourceDetailedStatus
    , OrganizationResourceDetailedStatus (..)

    -- * StringWithCharLimit1024
    , StringWithCharLimit1024 (..)

    -- * Compliance
    , Compliance (..)
    , mkCompliance
    , cComplianceContributorCount
    , cComplianceType

    -- * ConfigRuleComplianceSummaryGroupKey
    , ConfigRuleComplianceSummaryGroupKey (..)

    -- * OrganizationManagedRuleMetadata
    , OrganizationManagedRuleMetadata (..)
    , mkOrganizationManagedRuleMetadata
    , omrmRuleIdentifier
    , omrmDescription
    , omrmInputParameters
    , omrmMaximumExecutionFrequency
    , omrmResourceIdScope
    , omrmResourceTypesScope
    , omrmTagKeyScope
    , omrmTagValueScope

    -- * ConfigurationItem
    , ConfigurationItem (..)
    , mkConfigurationItem
    , ciAccountId
    , ciArn
    , ciAvailabilityZone
    , ciAwsRegion
    , ciConfiguration
    , ciConfigurationItemCaptureTime
    , ciConfigurationItemMD5Hash
    , ciConfigurationItemStatus
    , ciConfigurationStateId
    , ciRelatedEvents
    , ciRelationships
    , ciResourceCreationTime
    , ciResourceId
    , ciResourceName
    , ciResourceType
    , ciSupplementaryConfiguration
    , ciTags
    , ciVersion

    -- * OrganizationResourceStatus
    , OrganizationResourceStatus (..)

    -- * StringWithCharLimit64
    , StringWithCharLimit64 (..)

    -- * OrganizationConformancePackName
    , OrganizationConformancePackName (..)

    -- * RemediationException
    , RemediationException (..)
    , mkRemediationException
    , reConfigRuleName
    , reResourceType
    , reResourceId
    , reExpirationTime
    , reMessage

    -- * AggregatedSourceType
    , AggregatedSourceType (..)

    -- * DeliveryStatus
    , DeliveryStatus (..)

    -- * SupplementaryConfigurationName
    , SupplementaryConfigurationName (..)

    -- * RetentionConfigurationName
    , RetentionConfigurationName (..)

    -- * MemberAccountRuleStatus
    , MemberAccountRuleStatus (..)

    -- * StringWithCharLimit768
    , StringWithCharLimit768 (..)

    -- * StaticValue
    , StaticValue (..)
    , mkStaticValue
    , svValues

    -- * StackArn
    , StackArn (..)

    -- * DeliveryChannelStatus
    , DeliveryChannelStatus (..)
    , mkDeliveryChannelStatus
    , dcsConfigHistoryDeliveryInfo
    , dcsConfigSnapshotDeliveryInfo
    , dcsConfigStreamDeliveryInfo
    , dcsName

    -- * MessageType
    , MessageType (..)

    -- * Value
    , Value (..)

    -- * ConfigRuleName
    , ConfigRuleName (..)

    -- * ConformancePackName
    , ConformancePackName (..)

    -- * StringWithCharLimit256
    , StringWithCharLimit256 (..)

    -- * DeliveryS3KeyPrefix
    , DeliveryS3KeyPrefix (..)

    -- * OrganizationConfigRuleStatus
    , OrganizationConfigRuleStatus (..)
    , mkOrganizationConfigRuleStatus
    , ocrsOrganizationConfigRuleName
    , ocrsOrganizationRuleStatus
    , ocrsErrorCode
    , ocrsErrorMessage
    , ocrsLastUpdateTime

    -- * RemediationTargetType
    , RemediationTargetType (..)

    -- * ConfigRuleComplianceSummaryFilters
    , ConfigRuleComplianceSummaryFilters (..)
    , mkConfigRuleComplianceSummaryFilters
    , crcsfAccountId
    , crcsfAwsRegion

    -- * OrganizationResourceDetailedStatusFilters
    , OrganizationResourceDetailedStatusFilters (..)
    , mkOrganizationResourceDetailedStatusFilters
    , ordsfAccountId
    , ordsfStatus

    -- * ConfigRuleEvaluationStatus
    , ConfigRuleEvaluationStatus (..)
    , mkConfigRuleEvaluationStatus
    , cresConfigRuleArn
    , cresConfigRuleId
    , cresConfigRuleName
    , cresFirstActivatedTime
    , cresFirstEvaluationStarted
    , cresLastDeactivatedTime
    , cresLastErrorCode
    , cresLastErrorMessage
    , cresLastFailedEvaluationTime
    , cresLastFailedInvocationTime
    , cresLastSuccessfulEvaluationTime
    , cresLastSuccessfulInvocationTime

    -- * ExecutionControls
    , ExecutionControls (..)
    , mkExecutionControls
    , ecSsmControls

    -- * SchemaVersionId
    , SchemaVersionId (..)

    -- * MaximumExecutionFrequency
    , MaximumExecutionFrequency (..)

    -- * TemplateS3Uri
    , TemplateS3Uri (..)

    -- * ConfigurationRecorderStatus
    , ConfigurationRecorderStatus (..)
    , mkConfigurationRecorderStatus
    , crsLastErrorCode
    , crsLastErrorMessage
    , crsLastStartTime
    , crsLastStatus
    , crsLastStatusChangeTime
    , crsLastStopTime
    , crsName
    , crsRecording

    -- * ConformancePackStatusDetail
    , ConformancePackStatusDetail (..)
    , mkConformancePackStatusDetail
    , cpsdConformancePackName
    , cpsdConformancePackId
    , cpsdConformancePackArn
    , cpsdConformancePackState
    , cpsdStackArn
    , cpsdLastUpdateRequestedTime
    , cpsdConformancePackStatusReason
    , cpsdLastUpdateCompletedTime

    -- * ResourceValueType
    , ResourceValueType (..)

    -- * ConformancePackId
    , ConformancePackId (..)

    -- * ConfigurationItemStatus
    , ConfigurationItemStatus (..)

    -- * Owner
    , Owner (..)

    -- * OrganizationRuleStatus
    , OrganizationRuleStatus (..)

    -- * ConformancePackComplianceType
    , ConformancePackComplianceType (..)

    -- * RetentionConfiguration
    , RetentionConfiguration (..)
    , mkRetentionConfiguration
    , rcName
    , rcRetentionPeriodInDays

    -- * QueryInfo
    , QueryInfo (..)
    , mkQueryInfo
    , qiSelectFields

    -- * AccountId
    , AccountId (..)

    -- * EmptiableStringWithCharLimit256
    , EmptiableStringWithCharLimit256 (..)

    -- * NextToken
    , NextToken (..)

    -- * ComplianceByResource
    , ComplianceByResource (..)
    , mkComplianceByResource
    , cbrCompliance
    , cbrResourceId
    , cbrResourceType

    -- * ChannelName
    , ChannelName (..)

    -- * AggregatedSourceStatus
    , AggregatedSourceStatus (..)
    , mkAggregatedSourceStatus
    , assAwsRegion
    , assLastErrorCode
    , assLastErrorMessage
    , assLastUpdateStatus
    , assLastUpdateTime
    , assSourceId
    , assSourceType

    -- * ConfigRule
    , ConfigRule (..)
    , mkConfigRule
    , crSource
    , crConfigRuleArn
    , crConfigRuleId
    , crConfigRuleName
    , crConfigRuleState
    , crCreatedBy
    , crDescription
    , crInputParameters
    , crMaximumExecutionFrequency
    , crScope

    -- * AggregationAuthorization
    , AggregationAuthorization (..)
    , mkAggregationAuthorization
    , aaAggregationAuthorizationArn
    , aaAuthorizedAccountId
    , aaAuthorizedAwsRegion
    , aaCreationTime

    -- * AvailabilityZone
    , AvailabilityZone (..)

    -- * OrganizationConfigRuleTriggerType
    , OrganizationConfigRuleTriggerType (..)

    -- * Name
    , Name (..)

    -- * RemediationConfiguration
    , RemediationConfiguration (..)
    , mkRemediationConfiguration
    , rcConfigRuleName
    , rcTargetType
    , rcTargetId
    , rcArn
    , rcAutomatic
    , rcCreatedByService
    , rcExecutionControls
    , rcMaximumAutomaticAttempts
    , rcParameters
    , rcResourceType
    , rcRetryAttemptSeconds
    , rcTargetVersion

    -- * OrganizationConformancePack
    , OrganizationConformancePack (..)
    , mkOrganizationConformancePack
    , ocpOrganizationConformancePackName
    , ocpOrganizationConformancePackArn
    , ocpLastUpdateTime
    , ocpConformancePackInputParameters
    , ocpDeliveryS3Bucket
    , ocpDeliveryS3KeyPrefix
    , ocpExcludedAccounts

    -- * RecordingGroup
    , RecordingGroup (..)
    , mkRecordingGroup
    , rgAllSupported
    , rgIncludeGlobalResourceTypes
    , rgResourceTypes

    -- * ComplianceByConfigRule
    , ComplianceByConfigRule (..)
    , mkComplianceByConfigRule
    , cbcrCompliance
    , cbcrConfigRuleName

    -- * Version
    , Version (..)

    -- * Scope
    , Scope (..)
    , mkScope
    , sComplianceResourceId
    , sComplianceResourceTypes
    , sTagKey
    , sTagValue

    -- * Expression
    , Expression (..)

    -- * ConfigurationRecorder
    , ConfigurationRecorder (..)
    , mkConfigurationRecorder
    , crName
    , crRecordingGroup
    , crRoleARN

    -- * Source
    , Source (..)
    , mkSource
    , sOwner
    , sSourceIdentifier
    , sSourceDetails

    -- * RecorderName
    , RecorderName (..)

    -- * RemediationParameterValue
    , RemediationParameterValue (..)
    , mkRemediationParameterValue
    , rpvResourceValue
    , rpvStaticValue

    -- * ComplianceType
    , ComplianceType (..)

    -- * AwsRegion
    , AwsRegion (..)

    -- * MemberAccountStatus
    , MemberAccountStatus (..)
    , mkMemberAccountStatus
    , masAccountId
    , masConfigRuleName
    , masMemberAccountRuleStatus
    , masErrorCode
    , masErrorMessage
    , masLastUpdateTime

    -- * TagKey
    , TagKey (..)

    -- * RemediationExecutionStepState
    , RemediationExecutionStepState (..)

    -- * BaseConfigurationItem
    , BaseConfigurationItem (..)
    , mkBaseConfigurationItem
    , bciAccountId
    , bciArn
    , bciAvailabilityZone
    , bciAwsRegion
    , bciConfiguration
    , bciConfigurationItemCaptureTime
    , bciConfigurationItemStatus
    , bciConfigurationStateId
    , bciResourceCreationTime
    , bciResourceId
    , bciResourceName
    , bciResourceType
    , bciSupplementaryConfiguration
    , bciVersion

    -- * OrganizationConfigRuleName
    , OrganizationConfigRuleName (..)

    -- * AggregateResourceIdentifier
    , AggregateResourceIdentifier (..)
    , mkAggregateResourceIdentifier
    , ariSourceAccountId
    , ariSourceRegion
    , ariResourceId
    , ariResourceType
    , ariResourceName

    -- * Configuration
    , Configuration (..)

    -- * AccountAggregationSource
    , AccountAggregationSource (..)
    , mkAccountAggregationSource
    , aasAccountIds
    , aasAllAwsRegions
    , aasAwsRegions

    -- * TemplateBody
    , TemplateBody (..)

    -- * ComplianceSummaryByResourceType
    , ComplianceSummaryByResourceType (..)
    , mkComplianceSummaryByResourceType
    , csbrtComplianceSummary
    , csbrtResourceType

    -- * FailedRemediationBatch
    , FailedRemediationBatch (..)
    , mkFailedRemediationBatch
    , frbFailedItems
    , frbFailureMessage

    -- * ConformancePackInputParameter
    , ConformancePackInputParameter (..)
    , mkConformancePackInputParameter
    , cpipParameterName
    , cpipParameterValue

    -- * ConfigurationItemMD5Hash
    , ConfigurationItemMD5Hash (..)

    -- * OrganizationCustomRuleMetadata
    , OrganizationCustomRuleMetadata (..)
    , mkOrganizationCustomRuleMetadata
    , ocrmLambdaFunctionArn
    , ocrmOrganizationConfigRuleTriggerTypes
    , ocrmDescription
    , ocrmInputParameters
    , ocrmMaximumExecutionFrequency
    , ocrmResourceIdScope
    , ocrmResourceTypesScope
    , ocrmTagKeyScope
    , ocrmTagValueScope

    -- * AggregateComplianceByConfigRule
    , AggregateComplianceByConfigRule (..)
    , mkAggregateComplianceByConfigRule
    , acbcrAccountId
    , acbcrAwsRegion
    , acbcrCompliance
    , acbcrConfigRuleName

    -- * ConfigSnapshotDeliveryProperties
    , ConfigSnapshotDeliveryProperties (..)
    , mkConfigSnapshotDeliveryProperties
    , csdpDeliveryFrequency

    -- * AmazonResourceName
    , AmazonResourceName (..)

    -- * ConformancePackEvaluationResult
    , ConformancePackEvaluationResult (..)
    , mkConformancePackEvaluationResult
    , cperComplianceType
    , cperEvaluationResultIdentifier
    , cperConfigRuleInvokedTime
    , cperResultRecordedTime
    , cperAnnotation

    -- * ConformancePackState
    , ConformancePackState (..)

    -- * ConfigRuleState
    , ConfigRuleState (..)

    -- * OrganizationConformancePackStatus
    , OrganizationConformancePackStatus (..)
    , mkOrganizationConformancePackStatus
    , ocpsOrganizationConformancePackName
    , ocpsStatus
    , ocpsErrorCode
    , ocpsErrorMessage
    , ocpsLastUpdateTime

    -- * RelationshipName
    , RelationshipName (..)

    -- * RelatedEvent
    , RelatedEvent (..)

    -- * OrganizationConformancePackDetailedStatus
    , OrganizationConformancePackDetailedStatus (..)
    , mkOrganizationConformancePackDetailedStatus
    , ocpdsAccountId
    , ocpdsConformancePackName
    , ocpdsStatus
    , ocpdsErrorCode
    , ocpdsErrorMessage
    , ocpdsLastUpdateTime

    -- * ParameterName
    , ParameterName (..)

    -- * ConfigurationAggregatorName
    , ConfigurationAggregatorName (..)

    -- * Evaluation
    , Evaluation (..)
    , mkEvaluation
    , eComplianceResourceType
    , eComplianceResourceId
    , eComplianceType
    , eOrderingTimestamp
    , eAnnotation

    -- * StatusDetailFilters
    , StatusDetailFilters (..)
    , mkStatusDetailFilters
    , sdfAccountId
    , sdfMemberAccountRuleStatus

    -- * AggregateEvaluationResult
    , AggregateEvaluationResult (..)
    , mkAggregateEvaluationResult
    , aerAccountId
    , aerAnnotation
    , aerAwsRegion
    , aerComplianceType
    , aerConfigRuleInvokedTime
    , aerEvaluationResultIdentifier
    , aerResultRecordedTime

    -- * RecorderStatus
    , RecorderStatus (..)

    -- * ResourceIdentifier
    , ResourceIdentifier (..)
    , mkResourceIdentifier
    , riResourceDeletionTime
    , riResourceId
    , riResourceName
    , riResourceType

    -- * EventSource
    , EventSource (..)

    -- * SsmControls
    , SsmControls (..)
    , mkSsmControls
    , scConcurrentExecutionRatePercentage
    , scErrorPercentage

    -- * ConfigurationAggregator
    , ConfigurationAggregator (..)
    , mkConfigurationAggregator
    , caAccountAggregationSources
    , caConfigurationAggregatorArn
    , caConfigurationAggregatorName
    , caCreatedBy
    , caCreationTime
    , caLastUpdatedTime
    , caOrganizationAggregationSource

    -- * ResourceFilters
    , ResourceFilters (..)
    , mkResourceFilters
    , rfAccountId
    , rfRegion
    , rfResourceId
    , rfResourceName

    -- * StringWithCharLimit128
    , StringWithCharLimit128 (..)

    -- * ResourceCount
    , ResourceCount (..)
    , mkResourceCount
    , rcgCount
    , rcgResourceType

    -- * ResourceValue
    , ResourceValue (..)
    , mkResourceValue
    , rvValue

    -- * ComplianceSummary
    , ComplianceSummary (..)
    , mkComplianceSummary
    , csComplianceSummaryTimestamp
    , csCompliantResourceCount
    , csNonCompliantResourceCount

    -- * ConformancePackEvaluationFilters
    , ConformancePackEvaluationFilters (..)
    , mkConformancePackEvaluationFilters
    , cpefComplianceType
    , cpefConfigRuleNames
    , cpefResourceIds
    , cpefResourceType

    -- * ResourceCountGroupKey
    , ResourceCountGroupKey (..)

    -- * ConformancePackArn
    , ConformancePackArn (..)

    -- * RemediationExecutionStatus
    , RemediationExecutionStatus (..)
    , mkRemediationExecutionStatus
    , rInvocationTime
    , rLastUpdatedTime
    , rResourceKey
    , rState
    , rStepDetails

    -- * OrganizationConfigRuleArn
    , OrganizationConfigRuleArn (..)

    -- * GroupName
    , GroupName (..)

    -- * RequesterAccountId
    , RequesterAccountId (..)

    -- * RequesterAwsRegion
    , RequesterAwsRegion (..)

    -- * Key
    , Key (..)

    -- * DeliveryChannelName
    , DeliveryChannelName (..)

    -- * Region
    , Region (..)

    -- * CreatedBy
    , CreatedBy (..)

    -- * RuleIdentifier
    , RuleIdentifier (..)

    -- * Description
    , Description (..)

    -- * ResourceIdScope
    , ResourceIdScope (..)

    -- * TagKeyScope
    , TagKeyScope (..)

    -- * TagValueScope
    , TagValueScope (..)

    -- * ConfigurationRecorderName
    , ConfigurationRecorderName (..)

    -- * ResourceArn
    , ResourceArn (..)

    -- * AuthorizedAwsRegion
    , AuthorizedAwsRegion (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.Config.Types.ConfigurationAggregatorArn
  
import Network.AWS.Config.Types.StringWithCharLimit2048
  
  
import Network.AWS.Config.Types.EvaluationResultIdentifier
  
import Network.AWS.Config.Types.ConformancePackStatusReason
  
  
  
import Network.AWS.Config.Types.SourceDetail
  
  
  
import Network.AWS.Config.Types.ConfigExportDeliveryInfo
  
import Network.AWS.Config.Types.AggregatedSourceStatusType
  
import Network.AWS.Config.Types.SupplementaryConfigurationValue
  
import Network.AWS.Config.Types.ConfigStreamDeliveryInfo
  
  
  
import Network.AWS.Config.Types.Relationship
  
import Network.AWS.Config.Types.FieldInfo
  
import Network.AWS.Config.Types.ConformancePackRuleCompliance
  
import Network.AWS.Config.Types.EvaluationResultQualifier
  
import Network.AWS.Config.Types.Annotation
  
import Network.AWS.Config.Types.ResourceTypeString
  
import Network.AWS.Config.Types.FailedRemediationExceptionBatch
  
import Network.AWS.Config.Types.OrganizationConfigRule
  
import Network.AWS.Config.Types.RemediationExecutionStep
  
import Network.AWS.Config.Types.EvaluationResult
  
import Network.AWS.Config.Types.GroupedResourceCount
  
import Network.AWS.Config.Types.ResourceId
  
  
import Network.AWS.Config.Types.AggregateComplianceCount
  
import Network.AWS.Config.Types.DeliveryChannel
  
  
import Network.AWS.Config.Types.PendingAggregationRequest
  
import Network.AWS.Config.Types.Tag
  
import Network.AWS.Config.Types.OrganizationAggregationSource
  
import Network.AWS.Config.Types.RemediationExecutionState
  
import Network.AWS.Config.Types.ChronologicalOrder
  
import Network.AWS.Config.Types.ConformancePackComplianceFilters
  
import Network.AWS.Config.Types.ConfigRuleComplianceFilters
  
  
import Network.AWS.Config.Types.ResourceType
  
import Network.AWS.Config.Types.ParameterValue
  
import Network.AWS.Config.Types.ResourceCountFilters
  
import Network.AWS.Config.Types.DeliveryS3Bucket
  
import Network.AWS.Config.Types.BaseResourceId
  
import Network.AWS.Config.Types.ConfigurationStateId
  
import Network.AWS.Config.Types.RemediationExceptionResourceKey
  
import Network.AWS.Config.Types.ConformancePackDetail
  
  
import Network.AWS.Config.Types.ComplianceContributorCount
  
import Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
  
import Network.AWS.Config.Types.ARN
  
import Network.AWS.Config.Types.ResourceName
  
import Network.AWS.Config.Types.ConformancePackComplianceSummary
  
import Network.AWS.Config.Types.ResourceKey
  
import Network.AWS.Config.Types.OrganizationResourceDetailedStatus
  
import Network.AWS.Config.Types.StringWithCharLimit1024
  
import Network.AWS.Config.Types.Compliance
  
import Network.AWS.Config.Types.ConfigRuleComplianceSummaryGroupKey
  
import Network.AWS.Config.Types.OrganizationManagedRuleMetadata
  
import Network.AWS.Config.Types.ConfigurationItem
  
import Network.AWS.Config.Types.OrganizationResourceStatus
  
import Network.AWS.Config.Types.StringWithCharLimit64
  
  
import Network.AWS.Config.Types.OrganizationConformancePackName
  
import Network.AWS.Config.Types.RemediationException
  
  
import Network.AWS.Config.Types.AggregatedSourceType
  
import Network.AWS.Config.Types.DeliveryStatus
  
import Network.AWS.Config.Types.SupplementaryConfigurationName
  
import Network.AWS.Config.Types.RetentionConfigurationName
  
  
import Network.AWS.Config.Types.MemberAccountRuleStatus
  
import Network.AWS.Config.Types.StringWithCharLimit768
  
import Network.AWS.Config.Types.StaticValue
  
import Network.AWS.Config.Types.StackArn
  
  
import Network.AWS.Config.Types.DeliveryChannelStatus
  
import Network.AWS.Config.Types.MessageType
  
import Network.AWS.Config.Types.Value
  
import Network.AWS.Config.Types.ConfigRuleName
  
import Network.AWS.Config.Types.ConformancePackName
  
import Network.AWS.Config.Types.StringWithCharLimit256
  
import Network.AWS.Config.Types.DeliveryS3KeyPrefix
  
import Network.AWS.Config.Types.OrganizationConfigRuleStatus
  
import Network.AWS.Config.Types.RemediationTargetType
  
  
import Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
  
  
import Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters
  
import Network.AWS.Config.Types.ConfigRuleEvaluationStatus
  
import Network.AWS.Config.Types.ExecutionControls
  
import Network.AWS.Config.Types.SchemaVersionId
  
import Network.AWS.Config.Types.MaximumExecutionFrequency
  
import Network.AWS.Config.Types.TemplateS3Uri
  
import Network.AWS.Config.Types.ConfigurationRecorderStatus
  
import Network.AWS.Config.Types.ConformancePackStatusDetail
  
import Network.AWS.Config.Types.ResourceValueType
  
  
import Network.AWS.Config.Types.ConformancePackId
  
import Network.AWS.Config.Types.ConfigurationItemStatus
  
import Network.AWS.Config.Types.Owner
  
import Network.AWS.Config.Types.OrganizationRuleStatus
  
import Network.AWS.Config.Types.ConformancePackComplianceType
  
import Network.AWS.Config.Types.RetentionConfiguration
  
import Network.AWS.Config.Types.QueryInfo
  
import Network.AWS.Config.Types.AccountId
  
import Network.AWS.Config.Types.EmptiableStringWithCharLimit256
  
  
import Network.AWS.Config.Types.NextToken
  
import Network.AWS.Config.Types.ComplianceByResource
  
import Network.AWS.Config.Types.ChannelName
  
  
import Network.AWS.Config.Types.AggregatedSourceStatus
  
  
  
import Network.AWS.Config.Types.ConfigRule
  
  
import Network.AWS.Config.Types.AggregationAuthorization
  
import Network.AWS.Config.Types.AvailabilityZone
  
import Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
  
  
  
import Network.AWS.Config.Types.Name
  
import Network.AWS.Config.Types.RemediationConfiguration
  
  
import Network.AWS.Config.Types.OrganizationConformancePack
  
  
import Network.AWS.Config.Types.RecordingGroup
  
import Network.AWS.Config.Types.ComplianceByConfigRule
  
  
import Network.AWS.Config.Types.Version
  
  
  
import Network.AWS.Config.Types.Scope
  
import Network.AWS.Config.Types.Expression
  
  
import Network.AWS.Config.Types.ConfigurationRecorder
  
  
import Network.AWS.Config.Types.Source
  
  
import Network.AWS.Config.Types.RecorderName
  
import Network.AWS.Config.Types.RemediationParameterValue
  
  
  
  
import Network.AWS.Config.Types.ComplianceType
  
import Network.AWS.Config.Types.AwsRegion
  
import Network.AWS.Config.Types.MemberAccountStatus
  
import Network.AWS.Config.Types.TagKey
  
  
import Network.AWS.Config.Types.RemediationExecutionStepState
  
import Network.AWS.Config.Types.BaseConfigurationItem
  
  
import Network.AWS.Config.Types.OrganizationConfigRuleName
  
import Network.AWS.Config.Types.AggregateResourceIdentifier
  
import Network.AWS.Config.Types.Configuration
  
import Network.AWS.Config.Types.AccountAggregationSource
  
import Network.AWS.Config.Types.TemplateBody
  
import Network.AWS.Config.Types.ComplianceSummaryByResourceType
  
import Network.AWS.Config.Types.FailedRemediationBatch
  
import Network.AWS.Config.Types.ConformancePackInputParameter
  
import Network.AWS.Config.Types.ConfigurationItemMD5Hash
  
import Network.AWS.Config.Types.OrganizationCustomRuleMetadata
  
import Network.AWS.Config.Types.AggregateComplianceByConfigRule
  
import Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties
  
  
import Network.AWS.Config.Types.AmazonResourceName
  
import Network.AWS.Config.Types.ConformancePackEvaluationResult
  
import Network.AWS.Config.Types.ConformancePackState
  
import Network.AWS.Config.Types.ConfigRuleState
  
  
import Network.AWS.Config.Types.OrganizationConformancePackStatus
  
import Network.AWS.Config.Types.RelationshipName
  
import Network.AWS.Config.Types.RelatedEvent
  
  
import Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus
  
  
import Network.AWS.Config.Types.ParameterName
  
import Network.AWS.Config.Types.ConfigurationAggregatorName
  
import Network.AWS.Config.Types.Evaluation
  
import Network.AWS.Config.Types.StatusDetailFilters
  
import Network.AWS.Config.Types.AggregateEvaluationResult
  
import Network.AWS.Config.Types.RecorderStatus
  
  
  
  
  
import Network.AWS.Config.Types.ResourceIdentifier
  
import Network.AWS.Config.Types.EventSource
  
  
import Network.AWS.Config.Types.SsmControls
  
import Network.AWS.Config.Types.ConfigurationAggregator
  
import Network.AWS.Config.Types.ResourceFilters
  
import Network.AWS.Config.Types.StringWithCharLimit128
  
import Network.AWS.Config.Types.ResourceCount
  
import Network.AWS.Config.Types.ResourceValue
  
import Network.AWS.Config.Types.ComplianceSummary
  
import Network.AWS.Config.Types.ConformancePackEvaluationFilters
  
import Network.AWS.Config.Types.ResourceCountGroupKey
  
  
import Network.AWS.Config.Types.ConformancePackArn
  
  
  
import Network.AWS.Config.Types.RemediationExecutionStatus
  
import Network.AWS.Config.Types.OrganizationConfigRuleArn
  
import Network.AWS.Config.Types.GroupName
  
import Network.AWS.Config.Types.RequesterAccountId
  
import Network.AWS.Config.Types.RequesterAwsRegion
  
import Network.AWS.Config.Types.Key
  
import Network.AWS.Config.Types.DeliveryChannelName
  
import Network.AWS.Config.Types.Region
  
import Network.AWS.Config.Types.CreatedBy
  
import Network.AWS.Config.Types.RuleIdentifier
  
import Network.AWS.Config.Types.Description
  
import Network.AWS.Config.Types.ResourceIdScope
  
import Network.AWS.Config.Types.TagKeyScope
  
import Network.AWS.Config.Types.TagValueScope
  
import Network.AWS.Config.Types.ConfigurationRecorderName
  
import Network.AWS.Config.Types.ResourceArn
  
import Network.AWS.Config.Types.AuthorizedAwsRegion
  

-- | API version @2014-11-12@ of the Amazon Config SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Config",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "config",
                 Core._svcVersion = "2014-11-12", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Config",
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

-- | You specified an AWS Config rule without a remediation configuration.
_NoSuchRemediationConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchRemediationConfigurationException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchRemediationConfigurationException"
{-# INLINEABLE _NoSuchRemediationConfigurationException #-}
{-# DEPRECATED _NoSuchRemediationConfigurationException "Use generic-lens or generic-optics instead"  #-}

-- | The specified time range is not valid. The earlier time is not chronologically before the later time.
_InvalidTimeRangeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTimeRangeException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTimeRangeException"
{-# INLINEABLE _InvalidTimeRangeException #-}
{-# DEPRECATED _InvalidTimeRangeException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Config organization conformance pack that you passed in the filter does not exist.
--
-- For DeleteOrganizationConformancePack, you tried to delete an organization conformance pack that does not exist.
_NoSuchOrganizationConformancePackException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchOrganizationConformancePackException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchOrganizationConformancePackException"
{-# INLINEABLE _NoSuchOrganizationConformancePackException #-}
{-# DEPRECATED _NoSuchOrganizationConformancePackException "Use generic-lens or generic-optics instead"  #-}

-- | The specified Amazon SNS topic does not exist.
_InvalidSNSTopicARNException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSNSTopicARNException
  = Core._MatchServiceError mkServiceConfig
      "InvalidSNSTopicARNException"
{-# INLINEABLE _InvalidSNSTopicARNException #-}
{-# DEPRECATED _InvalidSNSTopicARNException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Config throws an exception if the recording group does not contain a valid list of resource types. Invalid values might also be incorrectly formatted.
_InvalidRecordingGroupException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRecordingGroupException
  = Core._MatchServiceError mkServiceConfig
      "InvalidRecordingGroupException"
{-# INLINEABLE _InvalidRecordingGroupException #-}
{-# DEPRECATED _InvalidRecordingGroupException "Use generic-lens or generic-optics instead"  #-}

-- | The syntax of the query is incorrect.
_InvalidExpressionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExpressionException
  = Core._MatchServiceError mkServiceConfig
      "InvalidExpressionException"
{-# INLINEABLE _InvalidExpressionException #-}
{-# DEPRECATED _InvalidExpressionException "Use generic-lens or generic-optics instead"  #-}

-- | Organization is no longer available.
_NoAvailableOrganizationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoAvailableOrganizationException
  = Core._MatchServiceError mkServiceConfig
      "NoAvailableOrganizationException"
{-# INLINEABLE _NoAvailableOrganizationException #-}
{-# DEPRECATED _NoAvailableOrganizationException "Use generic-lens or generic-optics instead"  #-}

-- | The requested action is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException
  = Core._MatchServiceError mkServiceConfig "ValidationException"
{-# INLINEABLE _ValidationException #-}
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead"  #-}

-- | For PutConfigAggregator API, no permission to call EnableAWSServiceAccess API.
--
-- For all OrganizationConfigRule and OrganizationConformancePack APIs, AWS Config throws an exception if APIs are called from member accounts. All APIs must be called from organization master account.
_OrganizationAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationAccessDeniedException
  = Core._MatchServiceError mkServiceConfig
      "OrganizationAccessDeniedException"
{-# INLINEABLE _OrganizationAccessDeniedException #-}
{-# DEPRECATED _OrganizationAccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | You have specified a configuration aggregator that does not exist.
_NoSuchConfigurationAggregatorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConfigurationAggregatorException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchConfigurationAggregatorException"
{-# INLINEABLE _NoSuchConfigurationAggregatorException #-}
{-# DEPRECATED _NoSuchConfigurationAggregatorException "Use generic-lens or generic-optics instead"  #-}

-- | You have provided a null or empty role ARN.
_InvalidRoleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRoleException
  = Core._MatchServiceError mkServiceConfig "InvalidRoleException"
{-# INLINEABLE _InvalidRoleException #-}
{-# DEPRECATED _InvalidRoleException "Use generic-lens or generic-optics instead"  #-}

-- | The configuration item size is outside the allowable range.
_OversizedConfigurationItemException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OversizedConfigurationItemException
  = Core._MatchServiceError mkServiceConfig
      "OversizedConfigurationItemException"
{-# INLINEABLE _OversizedConfigurationItemException #-}
{-# DEPRECATED _OversizedConfigurationItemException "Use generic-lens or generic-optics instead"  #-}

-- | You cannot delete the delivery channel you specified because the configuration recorder is running.
_LastDeliveryChannelDeleteFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LastDeliveryChannelDeleteFailedException
  = Core._MatchServiceError mkServiceConfig
      "LastDeliveryChannelDeleteFailedException"
{-# INLINEABLE _LastDeliveryChannelDeleteFailedException #-}
{-# DEPRECATED _LastDeliveryChannelDeleteFailedException "Use generic-lens or generic-optics instead"  #-}

-- | You have specified a template that is not valid or supported.
_ConformancePackTemplateValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConformancePackTemplateValidationException
  = Core._MatchServiceError mkServiceConfig
      "ConformancePackTemplateValidationException"
{-# INLINEABLE _ConformancePackTemplateValidationException #-}
{-# DEPRECATED _ConformancePackTemplateValidationException "Use generic-lens or generic-optics instead"  #-}

-- | You tried to delete a remediation exception that does not exist.
_NoSuchRemediationExceptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchRemediationExceptionException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchRemediationExceptionException"
{-# INLINEABLE _NoSuchRemediationExceptionException #-}
{-# DEPRECATED _NoSuchRemediationExceptionException "Use generic-lens or generic-optics instead"  #-}

-- | The specified limit is outside the allowable range.
_InvalidLimitException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLimitException
  = Core._MatchServiceError mkServiceConfig "InvalidLimitException"
{-# INLINEABLE _InvalidLimitException #-}
{-# DEPRECATED _InvalidLimitException "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the limit (6) of the number of organization conformance packs in an account (6 conformance pack with 25 AWS Config rules per pack per account).
_MaxNumberOfOrganizationConformancePacksExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfOrganizationConformancePacksExceededException
  = Core._MatchServiceError mkServiceConfig
      "MaxNumberOfOrganizationConformancePacksExceededException"
{-# INLINEABLE _MaxNumberOfOrganizationConformancePacksExceededException #-}
{-# DEPRECATED _MaxNumberOfOrganizationConformancePacksExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified delivery channel name is not valid.
_InvalidDeliveryChannelNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeliveryChannelNameException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeliveryChannelNameException"
{-# INLINEABLE _InvalidDeliveryChannelNameException #-}
{-# DEPRECATED _InvalidDeliveryChannelNameException "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the limit of the number of tags you can use. You have more than 50 tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | One or more of the specified parameters are invalid. Verify that your parameters are valid and try again.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterValueException"
{-# INLINEABLE _InvalidParameterValueException #-}
{-# DEPRECATED _InvalidParameterValueException "Use generic-lens or generic-optics instead"  #-}

-- | The specified @ResultToken@ is invalid.
_InvalidResultTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResultTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidResultTokenException"
{-# INLINEABLE _InvalidResultTokenException #-}
{-# DEPRECATED _InvalidResultTokenException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Config rule that you passed in the filter does not exist.
_NoSuchConfigRuleInConformancePackException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConfigRuleInConformancePackException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchConfigRuleInConformancePackException"
{-# INLINEABLE _NoSuchConfigRuleInConformancePackException #-}
{-# DEPRECATED _NoSuchConfigRuleInConformancePackException "Use generic-lens or generic-optics instead"  #-}

-- | You specified one or more organization config rules that do not exist.
_NoSuchOrganizationConfigRuleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchOrganizationConfigRuleException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchOrganizationConfigRuleException"
{-# INLINEABLE _NoSuchOrganizationConfigRuleException #-}
{-# DEPRECATED _NoSuchOrganizationConfigRuleException "Use generic-lens or generic-optics instead"  #-}

-- | You have specified a delivery channel that does not exist.
_NoSuchDeliveryChannelException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchDeliveryChannelException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchDeliveryChannelException"
{-# INLINEABLE _NoSuchDeliveryChannelException #-}
{-# DEPRECATED _NoSuchDeliveryChannelException "Use generic-lens or generic-optics instead"  #-}

-- | One or more AWS Config rules in the request are invalid. Verify that the rule names are correct and try again.
_NoSuchConfigRuleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConfigRuleException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchConfigRuleException"
{-# INLINEABLE _NoSuchConfigRuleException #-}
{-# DEPRECATED _NoSuchConfigRuleException "Use generic-lens or generic-optics instead"  #-}

-- | You specified one or more conformance packs that do not exist.
_NoSuchConformancePackException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConformancePackException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchConformancePackException"
{-# INLINEABLE _NoSuchConformancePackException #-}
{-# DEPRECATED _NoSuchConformancePackException "Use generic-lens or generic-optics instead"  #-}

-- | You have specified a retention configuration that does not exist.
_NoSuchRetentionConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchRetentionConfigurationException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchRetentionConfigurationException"
{-# INLINEABLE _NoSuchRetentionConfigurationException #-}
{-# DEPRECATED _NoSuchRetentionConfigurationException "Use generic-lens or generic-optics instead"  #-}

-- | Remediation action is in progress. You can either cancel execution in AWS Systems Manager or wait and try again later. 
_RemediationInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RemediationInProgressException
  = Core._MatchServiceError mkServiceConfig
      "RemediationInProgressException"
{-# INLINEABLE _RemediationInProgressException #-}
{-# DEPRECATED _RemediationInProgressException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Config resource cannot be created because your organization does not have all features enabled.
_OrganizationAllFeaturesNotEnabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationAllFeaturesNotEnabledException
  = Core._MatchServiceError mkServiceConfig
      "OrganizationAllFeaturesNotEnabledException"
{-# INLINEABLE _OrganizationAllFeaturesNotEnabledException #-}
{-# DEPRECATED _OrganizationAllFeaturesNotEnabledException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates one of the following errors:
--
--
--     * For PutConfigRule, the rule cannot be created because the IAM role assigned to AWS Config lacks permissions to perform the config:Put* action.
--
--
--     * For PutConfigRule, the AWS Lambda function cannot be invoked. Check the function ARN, and check the function's permissions.
--
--
--     * For PutOrganizationConfigRule, organization config rule cannot be created because you do not have permissions to call IAM @GetRole@ action or create a service linked role.
--
--
--     * For PutConformancePack and PutOrganizationConformancePack, a conformance pack cannot be created because you do not have permissions: 
--
--     * To call IAM @GetRole@ action or create a service linked role.
--
--
--     * To read Amazon S3 bucket.
--
--
--
--
_InsufficientPermissionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientPermissionsException
  = Core._MatchServiceError mkServiceConfig
      "InsufficientPermissionsException"
{-# INLINEABLE _InsufficientPermissionsException #-}
{-# DEPRECATED _InsufficientPermissionsException "Use generic-lens or generic-optics instead"  #-}

-- | You have specified a resource that is either unknown or has not been discovered.
_ResourceNotDiscoveredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotDiscoveredException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotDiscoveredException"
{-# INLINEABLE _ResourceNotDiscoveredException #-}
{-# DEPRECATED _ResourceNotDiscoveredException "Use generic-lens or generic-optics instead"  #-}

-- | The specified next token is invalid. Specify the @nextToken@ string that was returned in the previous response to get the next page of results.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidNextTokenException"
{-# INLINEABLE _InvalidNextTokenException #-}
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead"  #-}

-- | Failed to add the retention configuration because a retention configuration with that name already exists.
_MaxNumberOfRetentionConfigurationsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfRetentionConfigurationsExceededException
  = Core._MatchServiceError mkServiceConfig
      "MaxNumberOfRetentionConfigurationsExceededException"
{-# INLINEABLE _MaxNumberOfRetentionConfigurationsExceededException #-}
{-# DEPRECATED _MaxNumberOfRetentionConfigurationsExceededException "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the limit (6) of the number of conformance packs in an account (6 conformance pack with 25 AWS Config rules per pack).
_MaxNumberOfConformancePacksExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfConformancePacksExceededException
  = Core._MatchServiceError mkServiceConfig
      "MaxNumberOfConformancePacksExceededException"
{-# INLINEABLE _MaxNumberOfConformancePacksExceededException #-}
{-# DEPRECATED _MaxNumberOfConformancePacksExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Failed to add the AWS Config rule because the account already contains the maximum number of 150 rules. Consider deleting any deactivated rules before you add new rules.
_MaxNumberOfConfigRulesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfConfigRulesExceededException
  = Core._MatchServiceError mkServiceConfig
      "MaxNumberOfConfigRulesExceededException"
{-# INLINEABLE _MaxNumberOfConfigRulesExceededException #-}
{-# DEPRECATED _MaxNumberOfConfigRulesExceededException "Use generic-lens or generic-optics instead"  #-}

-- | There are no configuration recorders available to provide the role needed to describe your resources. Create a configuration recorder.
_NoAvailableConfigurationRecorderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoAvailableConfigurationRecorderException
  = Core._MatchServiceError mkServiceConfig
      "NoAvailableConfigurationRecorderException"
{-# INLINEABLE _NoAvailableConfigurationRecorderException #-}
{-# DEPRECATED _NoAvailableConfigurationRecorderException "Use generic-lens or generic-optics instead"  #-}

-- | The specified Amazon S3 bucket does not exist.
_NoSuchBucketException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchBucketException
  = Core._MatchServiceError mkServiceConfig "NoSuchBucketException"
{-# INLINEABLE _NoSuchBucketException #-}
{-# DEPRECATED _NoSuchBucketException "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the limit (100,000) of active custom resource types in your account. Delete unused resources using @DeleteResourceConfig@ .
_MaxActiveResourcesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxActiveResourcesExceededException
  = Core._MatchServiceError mkServiceConfig
      "MaxActiveResourcesExceededException"
{-# INLINEABLE _MaxActiveResourcesExceededException #-}
{-# DEPRECATED _MaxActiveResourcesExceededException "Use generic-lens or generic-optics instead"  #-}

-- | There is no delivery channel available to record configurations.
_NoAvailableDeliveryChannelException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoAvailableDeliveryChannelException
  = Core._MatchServiceError mkServiceConfig
      "NoAvailableDeliveryChannelException"
{-# INLINEABLE _NoAvailableDeliveryChannelException #-}
{-# DEPRECATED _NoAvailableDeliveryChannelException "Use generic-lens or generic-optics instead"  #-}

-- | You have specified a template that is not valid or supported.
_OrganizationConformancePackTemplateValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationConformancePackTemplateValidationException
  = Core._MatchServiceError mkServiceConfig
      "OrganizationConformancePackTemplateValidationException"
{-# INLINEABLE _OrganizationConformancePackTemplateValidationException #-}
{-# DEPRECATED _OrganizationConformancePackTemplateValidationException "Use generic-lens or generic-optics instead"  #-}

-- | You have provided a configuration recorder name that is not valid.
_InvalidConfigurationRecorderNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationRecorderNameException
  = Core._MatchServiceError mkServiceConfig
      "InvalidConfigurationRecorderNameException"
{-# INLINEABLE _InvalidConfigurationRecorderNameException #-}
{-# DEPRECATED _InvalidConfigurationRecorderNameException "Use generic-lens or generic-optics instead"  #-}

-- | There is no configuration recorder running.
_NoRunningConfigurationRecorderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoRunningConfigurationRecorderException
  = Core._MatchServiceError mkServiceConfig
      "NoRunningConfigurationRecorderException"
{-# INLINEABLE _NoRunningConfigurationRecorderException #-}
{-# DEPRECATED _NoRunningConfigurationRecorderException "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the limit of the number of recorders you can create.
_MaxNumberOfConfigurationRecordersExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfConfigurationRecordersExceededException
  = Core._MatchServiceError mkServiceConfig
      "MaxNumberOfConfigurationRecordersExceededException"
{-# INLINEABLE _MaxNumberOfConfigurationRecordersExceededException #-}
{-# DEPRECATED _MaxNumberOfConfigurationRecordersExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Your Amazon S3 bucket policy does not permit AWS Config to write to it.
_InsufficientDeliveryPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientDeliveryPolicyException
  = Core._MatchServiceError mkServiceConfig
      "InsufficientDeliveryPolicyException"
{-# INLINEABLE _InsufficientDeliveryPolicyException #-}
{-# DEPRECATED _InsufficientDeliveryPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the limit of the number of delivery channels you can create.
_MaxNumberOfDeliveryChannelsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfDeliveryChannelsExceededException
  = Core._MatchServiceError mkServiceConfig
      "MaxNumberOfDeliveryChannelsExceededException"
{-# INLINEABLE _MaxNumberOfDeliveryChannelsExceededException #-}
{-# DEPRECATED _MaxNumberOfDeliveryChannelsExceededException "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the limit of the number of organization config rules you can create.
_MaxNumberOfOrganizationConfigRulesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxNumberOfOrganizationConfigRulesExceededException
  = Core._MatchServiceError mkServiceConfig
      "MaxNumberOfOrganizationConfigRulesExceededException"
{-# INLINEABLE _MaxNumberOfOrganizationConfigRulesExceededException #-}
{-# DEPRECATED _MaxNumberOfOrganizationConfigRulesExceededException "Use generic-lens or generic-optics instead"  #-}

-- | You have specified a resource that does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | You have specified a configuration recorder that does not exist.
_NoSuchConfigurationRecorderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchConfigurationRecorderException
  = Core._MatchServiceError mkServiceConfig
      "NoSuchConfigurationRecorderException"
{-# INLINEABLE _NoSuchConfigurationRecorderException #-}
{-# DEPRECATED _NoSuchConfigurationRecorderException "Use generic-lens or generic-optics instead"  #-}

-- | The specified Amazon S3 key prefix is not valid.
_InvalidS3KeyPrefixException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3KeyPrefixException
  = Core._MatchServiceError mkServiceConfig
      "InvalidS3KeyPrefixException"
{-# INLINEABLE _InvalidS3KeyPrefixException #-}
{-# DEPRECATED _InvalidS3KeyPrefixException "Use generic-lens or generic-optics instead"  #-}

-- | For @StartConfigRulesEvaluation@ API, this exception is thrown if an evaluation is in progress or if you call the 'StartConfigRulesEvaluation' API more than once per minute.
--
-- For @PutConfigurationAggregator@ API, this exception is thrown if the number of accounts and aggregators exceeds the limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | You see this exception in the following cases: 
--
--
--     * For DeleteConfigRule, AWS Config is deleting this rule. Try your request again later.
--
--
--     * For DeleteConfigRule, the rule is deleting your evaluation results. Try your request again later.
--
--
--     * For DeleteConfigRule, a remediation action is associated with the rule and AWS Config cannot delete this rule. Delete the remediation action associated with the rule before deleting the rule and try your request again later.
--
--
--     * For PutConfigOrganizationRule, organization config rule deletion is in progress. Try your request again later.
--
--
--     * For DeleteOrganizationConfigRule, organization config rule creation is in progress. Try your request again later.
--
--
--     * For PutConformancePack and PutOrganizationConformancePack, a conformance pack creation, update, and deletion is in progress. Try your request again later.
--
--
--     * For DeleteConformancePack, a conformance pack creation, update, and deletion is in progress. Try your request again later.
--
--
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
