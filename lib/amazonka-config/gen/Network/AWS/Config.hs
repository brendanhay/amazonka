{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Config__
--
-- AWS Config provides a way to keep track of the configurations of all the AWS resources associated with your AWS account. You can use AWS Config to get the current and historical configurations of each AWS resource and also to get information about the relationship between the resources. An AWS resource can be an Amazon Compute Cloud (Amazon EC2) instance, an Elastic Block Store (EBS) volume, an elastic network Interface (ENI), or a security group. For a complete list of resources currently supported by AWS Config, see <http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resources> .
--
-- You can access and manage AWS Config through the AWS Management Console, the AWS Command Line Interface (AWS CLI), the AWS Config API, or the AWS SDKs for AWS Config. This reference guide contains documentation for the AWS Config API and the AWS CLI commands that you can use to manage AWS Config. The AWS Config API uses the Signature Version 4 protocol for signing requests. For more information about how to sign a request with this protocol, see <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> . For detailed information about AWS Config features and their associated actions or commands, as well as how to work with AWS Management Console, see <http://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html What Is AWS Config> in the /AWS Config Developer Guide/ .
--
module Network.AWS.Config
    (
    -- * Service Configuration
      config

    -- * Errors
    -- $errors

    -- ** InvalidTimeRangeException
    , _InvalidTimeRangeException

    -- ** InvalidSNSTopicARNException
    , _InvalidSNSTopicARNException

    -- ** InvalidRecordingGroupException
    , _InvalidRecordingGroupException

    -- ** NoAvailableOrganizationException
    , _NoAvailableOrganizationException

    -- ** ValidationException
    , _ValidationException

    -- ** OrganizationAccessDeniedException
    , _OrganizationAccessDeniedException

    -- ** NoSuchConfigurationAggregatorException
    , _NoSuchConfigurationAggregatorException

    -- ** InvalidRoleException
    , _InvalidRoleException

    -- ** LastDeliveryChannelDeleteFailedException
    , _LastDeliveryChannelDeleteFailedException

    -- ** InvalidLimitException
    , _InvalidLimitException

    -- ** InvalidDeliveryChannelNameException
    , _InvalidDeliveryChannelNameException

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** InvalidResultTokenException
    , _InvalidResultTokenException

    -- ** NoSuchDeliveryChannelException
    , _NoSuchDeliveryChannelException

    -- ** NoSuchConfigRuleException
    , _NoSuchConfigRuleException

    -- ** OrganizationAllFeaturesNotEnabledException
    , _OrganizationAllFeaturesNotEnabledException

    -- ** InsufficientPermissionsException
    , _InsufficientPermissionsException

    -- ** ResourceNotDiscoveredException
    , _ResourceNotDiscoveredException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** MaxNumberOfConfigRulesExceededException
    , _MaxNumberOfConfigRulesExceededException

    -- ** NoAvailableConfigurationRecorderException
    , _NoAvailableConfigurationRecorderException

    -- ** NoSuchBucketException
    , _NoSuchBucketException

    -- ** NoAvailableDeliveryChannelException
    , _NoAvailableDeliveryChannelException

    -- ** InvalidConfigurationRecorderNameException
    , _InvalidConfigurationRecorderNameException

    -- ** NoRunningConfigurationRecorderException
    , _NoRunningConfigurationRecorderException

    -- ** MaxNumberOfConfigurationRecordersExceededException
    , _MaxNumberOfConfigurationRecordersExceededException

    -- ** InsufficientDeliveryPolicyException
    , _InsufficientDeliveryPolicyException

    -- ** MaxNumberOfDeliveryChannelsExceededException
    , _MaxNumberOfDeliveryChannelsExceededException

    -- ** NoSuchConfigurationRecorderException
    , _NoSuchConfigurationRecorderException

    -- ** InvalidS3KeyPrefixException
    , _InvalidS3KeyPrefixException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribePendingAggregationRequests
    , module Network.AWS.Config.DescribePendingAggregationRequests

    -- ** GetResourceConfigHistory (Paginated)
    , module Network.AWS.Config.GetResourceConfigHistory

    -- ** DescribeConfigurationAggregators
    , module Network.AWS.Config.DescribeConfigurationAggregators

    -- ** DescribeComplianceByConfigRule (Paginated)
    , module Network.AWS.Config.DescribeComplianceByConfigRule

    -- ** StopConfigurationRecorder
    , module Network.AWS.Config.StopConfigurationRecorder

    -- ** GetAggregateConfigRuleComplianceSummary
    , module Network.AWS.Config.GetAggregateConfigRuleComplianceSummary

    -- ** BatchGetResourceConfig
    , module Network.AWS.Config.BatchGetResourceConfig

    -- ** DescribeConfigRules (Paginated)
    , module Network.AWS.Config.DescribeConfigRules

    -- ** DescribeAggregateComplianceByConfigRules
    , module Network.AWS.Config.DescribeAggregateComplianceByConfigRules

    -- ** DeleteEvaluationResults
    , module Network.AWS.Config.DeleteEvaluationResults

    -- ** PutConfigRule
    , module Network.AWS.Config.PutConfigRule

    -- ** DeleteConfigRule
    , module Network.AWS.Config.DeleteConfigRule

    -- ** GetComplianceDetailsByResource (Paginated)
    , module Network.AWS.Config.GetComplianceDetailsByResource

    -- ** DeletePendingAggregationRequest
    , module Network.AWS.Config.DeletePendingAggregationRequest

    -- ** DeliverConfigSnapshot
    , module Network.AWS.Config.DeliverConfigSnapshot

    -- ** DescribeConfigRuleEvaluationStatus
    , module Network.AWS.Config.DescribeConfigRuleEvaluationStatus

    -- ** GetDiscoveredResourceCounts
    , module Network.AWS.Config.GetDiscoveredResourceCounts

    -- ** StartConfigRulesEvaluation
    , module Network.AWS.Config.StartConfigRulesEvaluation

    -- ** DescribeComplianceByResource (Paginated)
    , module Network.AWS.Config.DescribeComplianceByResource

    -- ** PutEvaluations
    , module Network.AWS.Config.PutEvaluations

    -- ** DescribeConfigurationRecorders
    , module Network.AWS.Config.DescribeConfigurationRecorders

    -- ** GetAggregateComplianceDetailsByConfigRule
    , module Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule

    -- ** StartConfigurationRecorder
    , module Network.AWS.Config.StartConfigurationRecorder

    -- ** GetComplianceSummaryByConfigRule
    , module Network.AWS.Config.GetComplianceSummaryByConfigRule

    -- ** PutConfigurationAggregator
    , module Network.AWS.Config.PutConfigurationAggregator

    -- ** DeleteConfigurationAggregator
    , module Network.AWS.Config.DeleteConfigurationAggregator

    -- ** DescribeConfigurationRecorderStatus
    , module Network.AWS.Config.DescribeConfigurationRecorderStatus

    -- ** PutConfigurationRecorder
    , module Network.AWS.Config.PutConfigurationRecorder

    -- ** DeleteConfigurationRecorder
    , module Network.AWS.Config.DeleteConfigurationRecorder

    -- ** GetComplianceSummaryByResourceType
    , module Network.AWS.Config.GetComplianceSummaryByResourceType

    -- ** DescribeDeliveryChannelStatus
    , module Network.AWS.Config.DescribeDeliveryChannelStatus

    -- ** PutDeliveryChannel
    , module Network.AWS.Config.PutDeliveryChannel

    -- ** GetComplianceDetailsByConfigRule (Paginated)
    , module Network.AWS.Config.GetComplianceDetailsByConfigRule

    -- ** DeleteAggregationAuthorization
    , module Network.AWS.Config.DeleteAggregationAuthorization

    -- ** DeleteDeliveryChannel
    , module Network.AWS.Config.DeleteDeliveryChannel

    -- ** PutAggregationAuthorization
    , module Network.AWS.Config.PutAggregationAuthorization

    -- ** DescribeConfigurationAggregatorSourcesStatus
    , module Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus

    -- ** ListDiscoveredResources (Paginated)
    , module Network.AWS.Config.ListDiscoveredResources

    -- ** DescribeDeliveryChannels
    , module Network.AWS.Config.DescribeDeliveryChannels

    -- ** DescribeAggregationAuthorizations
    , module Network.AWS.Config.DescribeAggregationAuthorizations

    -- * Types

    -- ** AggregatedSourceStatusType
    , AggregatedSourceStatusType (..)

    -- ** AggregatedSourceType
    , AggregatedSourceType (..)

    -- ** ChronologicalOrder
    , ChronologicalOrder (..)

    -- ** ComplianceType
    , ComplianceType (..)

    -- ** ConfigRuleComplianceSummaryGroupKey
    , ConfigRuleComplianceSummaryGroupKey (..)

    -- ** ConfigRuleState
    , ConfigRuleState (..)

    -- ** ConfigurationItemStatus
    , ConfigurationItemStatus (..)

    -- ** DeliveryStatus
    , DeliveryStatus (..)

    -- ** EventSource
    , EventSource (..)

    -- ** MaximumExecutionFrequency
    , MaximumExecutionFrequency (..)

    -- ** MessageType
    , MessageType (..)

    -- ** Owner
    , Owner (..)

    -- ** RecorderStatus
    , RecorderStatus (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** AccountAggregationSource
    , AccountAggregationSource
    , accountAggregationSource
    , aasAWSRegions
    , aasAllAWSRegions
    , aasAccountIds

    -- ** AggregateComplianceByConfigRule
    , AggregateComplianceByConfigRule
    , aggregateComplianceByConfigRule
    , acbcrCompliance
    , acbcrConfigRuleName
    , acbcrAccountId
    , acbcrAWSRegion

    -- ** AggregateComplianceCount
    , AggregateComplianceCount
    , aggregateComplianceCount
    , accGroupName
    , accComplianceSummary

    -- ** AggregateEvaluationResult
    , AggregateEvaluationResult
    , aggregateEvaluationResult
    , aerEvaluationResultIdentifier
    , aerAnnotation
    , aerConfigRuleInvokedTime
    , aerResultRecordedTime
    , aerAccountId
    , aerComplianceType
    , aerAWSRegion

    -- ** AggregatedSourceStatus
    , AggregatedSourceStatus
    , aggregatedSourceStatus
    , assLastErrorCode
    , assLastUpdateStatus
    , assSourceType
    , assSourceId
    , assLastErrorMessage
    , assAWSRegion
    , assLastUpdateTime

    -- ** AggregationAuthorization
    , AggregationAuthorization
    , aggregationAuthorization
    , aaCreationTime
    , aaAuthorizedAWSRegion
    , aaAggregationAuthorizationARN
    , aaAuthorizedAccountId

    -- ** BaseConfigurationItem
    , BaseConfigurationItem
    , baseConfigurationItem
    , bciResourceId
    , bciResourceType
    , bciConfigurationStateId
    , bciArn
    , bciResourceName
    , bciResourceCreationTime
    , bciConfigurationItemStatus
    , bciConfigurationItemCaptureTime
    , bciAccountId
    , bciSupplementaryConfiguration
    , bciAvailabilityZone
    , bciVersion
    , bciAwsRegion
    , bciConfiguration

    -- ** Compliance
    , Compliance
    , compliance
    , cComplianceContributorCount
    , cComplianceType

    -- ** ComplianceByConfigRule
    , ComplianceByConfigRule
    , complianceByConfigRule
    , cbcrCompliance
    , cbcrConfigRuleName

    -- ** ComplianceByResource
    , ComplianceByResource
    , complianceByResource
    , cbrResourceId
    , cbrResourceType
    , cbrCompliance

    -- ** ComplianceContributorCount
    , ComplianceContributorCount
    , complianceContributorCount
    , cccCappedCount
    , cccCapExceeded

    -- ** ComplianceSummary
    , ComplianceSummary
    , complianceSummary
    , csComplianceSummaryTimestamp
    , csCompliantResourceCount
    , csNonCompliantResourceCount

    -- ** ComplianceSummaryByResourceType
    , ComplianceSummaryByResourceType
    , complianceSummaryByResourceType
    , csbrtResourceType
    , csbrtComplianceSummary

    -- ** ConfigExportDeliveryInfo
    , ConfigExportDeliveryInfo
    , configExportDeliveryInfo
    , cediLastErrorCode
    , cediLastAttemptTime
    , cediLastSuccessfulTime
    , cediLastStatus
    , cediLastErrorMessage
    , cediNextDeliveryTime

    -- ** ConfigRule
    , ConfigRule
    , configRule
    , crInputParameters
    , crConfigRuleName
    , crMaximumExecutionFrequency
    , crConfigRuleId
    , crScope
    , crConfigRuleState
    , crDescription
    , crConfigRuleARN
    , crSource

    -- ** ConfigRuleComplianceFilters
    , ConfigRuleComplianceFilters
    , configRuleComplianceFilters
    , crcfConfigRuleName
    , crcfAccountId
    , crcfComplianceType
    , crcfAWSRegion

    -- ** ConfigRuleComplianceSummaryFilters
    , ConfigRuleComplianceSummaryFilters
    , configRuleComplianceSummaryFilters
    , crcsfAccountId
    , crcsfAWSRegion

    -- ** ConfigRuleEvaluationStatus
    , ConfigRuleEvaluationStatus
    , configRuleEvaluationStatus
    , cresLastErrorCode
    , cresLastFailedEvaluationTime
    , cresFirstActivatedTime
    , cresLastSuccessfulEvaluationTime
    , cresConfigRuleName
    , cresLastErrorMessage
    , cresConfigRuleId
    , cresLastFailedInvocationTime
    , cresFirstEvaluationStarted
    , cresLastSuccessfulInvocationTime
    , cresConfigRuleARN

    -- ** ConfigSnapshotDeliveryProperties
    , ConfigSnapshotDeliveryProperties
    , configSnapshotDeliveryProperties
    , csdpDeliveryFrequency

    -- ** ConfigStreamDeliveryInfo
    , ConfigStreamDeliveryInfo
    , configStreamDeliveryInfo
    , csdiLastErrorCode
    , csdiLastStatusChangeTime
    , csdiLastStatus
    , csdiLastErrorMessage

    -- ** ConfigurationAggregator
    , ConfigurationAggregator
    , configurationAggregator
    , caConfigurationAggregatorARN
    , caCreationTime
    , caOrganizationAggregationSource
    , caLastUpdatedTime
    , caAccountAggregationSources
    , caConfigurationAggregatorName

    -- ** ConfigurationItem
    , ConfigurationItem
    , configurationItem
    , ciResourceId
    , ciResourceType
    , ciConfigurationStateId
    , ciArn
    , ciResourceName
    , ciResourceCreationTime
    , ciConfigurationItemStatus
    , ciConfigurationItemCaptureTime
    , ciAccountId
    , ciSupplementaryConfiguration
    , ciAvailabilityZone
    , ciRelationships
    , ciVersion
    , ciAwsRegion
    , ciRelatedEvents
    , ciConfiguration
    , ciConfigurationItemMD5Hash
    , ciTags

    -- ** ConfigurationRecorder
    , ConfigurationRecorder
    , configurationRecorder
    , crName
    , crRecordingGroup
    , crRoleARN

    -- ** ConfigurationRecorderStatus
    , ConfigurationRecorderStatus
    , configurationRecorderStatus
    , crsLastErrorCode
    , crsLastStopTime
    , crsLastStatusChangeTime
    , crsRecording
    , crsLastStatus
    , crsLastErrorMessage
    , crsName
    , crsLastStartTime

    -- ** DeliveryChannel
    , DeliveryChannel
    , deliveryChannel
    , dcS3KeyPrefix
    , dcSnsTopicARN
    , dcName
    , dcConfigSnapshotDeliveryProperties
    , dcS3BucketName

    -- ** DeliveryChannelStatus
    , DeliveryChannelStatus
    , deliveryChannelStatus
    , dcsConfigSnapshotDeliveryInfo
    , dcsConfigStreamDeliveryInfo
    , dcsConfigHistoryDeliveryInfo
    , dcsName

    -- ** Evaluation
    , Evaluation
    , evaluation
    , eAnnotation
    , eComplianceResourceType
    , eComplianceResourceId
    , eComplianceType
    , eOrderingTimestamp

    -- ** EvaluationResult
    , EvaluationResult
    , evaluationResult
    , erEvaluationResultIdentifier
    , erAnnotation
    , erConfigRuleInvokedTime
    , erResultRecordedTime
    , erResultToken
    , erComplianceType

    -- ** EvaluationResultIdentifier
    , EvaluationResultIdentifier
    , evaluationResultIdentifier
    , eriEvaluationResultQualifier
    , eriOrderingTimestamp

    -- ** EvaluationResultQualifier
    , EvaluationResultQualifier
    , evaluationResultQualifier
    , erqResourceId
    , erqResourceType
    , erqConfigRuleName

    -- ** OrganizationAggregationSource
    , OrganizationAggregationSource
    , organizationAggregationSource
    , oasAWSRegions
    , oasAllAWSRegions
    , oasRoleARN

    -- ** PendingAggregationRequest
    , PendingAggregationRequest
    , pendingAggregationRequest
    , parRequesterAccountId
    , parRequesterAWSRegion

    -- ** RecordingGroup
    , RecordingGroup
    , recordingGroup
    , rgAllSupported
    , rgIncludeGlobalResourceTypes
    , rgResourceTypes

    -- ** Relationship
    , Relationship
    , relationship
    , rResourceId
    , rResourceType
    , rResourceName
    , rRelationshipName

    -- ** ResourceCount
    , ResourceCount
    , resourceCount
    , rcResourceType
    , rcCount

    -- ** ResourceIdentifier
    , ResourceIdentifier
    , resourceIdentifier
    , riResourceId
    , riResourceType
    , riResourceName
    , riResourceDeletionTime

    -- ** ResourceKey
    , ResourceKey
    , resourceKey
    , rkResourceType
    , rkResourceId

    -- ** Scope
    , Scope
    , scope
    , sComplianceResourceTypes
    , sComplianceResourceId
    , sTagValue
    , sTagKey

    -- ** Source
    , Source
    , source
    , sSourceDetails
    , sOwner
    , sSourceIdentifier

    -- ** SourceDetail
    , SourceDetail
    , sourceDetail
    , sdMessageType
    , sdMaximumExecutionFrequency
    , sdEventSource
    ) where

import Network.AWS.Config.BatchGetResourceConfig
import Network.AWS.Config.DeleteAggregationAuthorization
import Network.AWS.Config.DeleteConfigRule
import Network.AWS.Config.DeleteConfigurationAggregator
import Network.AWS.Config.DeleteConfigurationRecorder
import Network.AWS.Config.DeleteDeliveryChannel
import Network.AWS.Config.DeleteEvaluationResults
import Network.AWS.Config.DeletePendingAggregationRequest
import Network.AWS.Config.DeliverConfigSnapshot
import Network.AWS.Config.DescribeAggregateComplianceByConfigRules
import Network.AWS.Config.DescribeAggregationAuthorizations
import Network.AWS.Config.DescribeComplianceByConfigRule
import Network.AWS.Config.DescribeComplianceByResource
import Network.AWS.Config.DescribeConfigRuleEvaluationStatus
import Network.AWS.Config.DescribeConfigRules
import Network.AWS.Config.DescribeConfigurationAggregators
import Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus
import Network.AWS.Config.DescribeConfigurationRecorders
import Network.AWS.Config.DescribeConfigurationRecorderStatus
import Network.AWS.Config.DescribeDeliveryChannels
import Network.AWS.Config.DescribeDeliveryChannelStatus
import Network.AWS.Config.DescribePendingAggregationRequests
import Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule
import Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
import Network.AWS.Config.GetComplianceDetailsByConfigRule
import Network.AWS.Config.GetComplianceDetailsByResource
import Network.AWS.Config.GetComplianceSummaryByConfigRule
import Network.AWS.Config.GetComplianceSummaryByResourceType
import Network.AWS.Config.GetDiscoveredResourceCounts
import Network.AWS.Config.GetResourceConfigHistory
import Network.AWS.Config.ListDiscoveredResources
import Network.AWS.Config.PutAggregationAuthorization
import Network.AWS.Config.PutConfigRule
import Network.AWS.Config.PutConfigurationAggregator
import Network.AWS.Config.PutConfigurationRecorder
import Network.AWS.Config.PutDeliveryChannel
import Network.AWS.Config.PutEvaluations
import Network.AWS.Config.StartConfigRulesEvaluation
import Network.AWS.Config.StartConfigurationRecorder
import Network.AWS.Config.StopConfigurationRecorder
import Network.AWS.Config.Types
import Network.AWS.Config.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Config'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
