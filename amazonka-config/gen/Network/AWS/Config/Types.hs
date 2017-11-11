{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types
    (
    -- * Service Configuration
      config

    -- * Errors
    , _InvalidTimeRangeException
    , _InvalidSNSTopicARNException
    , _InvalidRecordingGroupException
    , _ValidationException
    , _InvalidRoleException
    , _LastDeliveryChannelDeleteFailedException
    , _InvalidLimitException
    , _InvalidDeliveryChannelNameException
    , _InvalidParameterValueException
    , _InvalidResultTokenException
    , _NoSuchDeliveryChannelException
    , _NoSuchConfigRuleException
    , _InsufficientPermissionsException
    , _ResourceNotDiscoveredException
    , _InvalidNextTokenException
    , _MaxNumberOfConfigRulesExceededException
    , _NoAvailableConfigurationRecorderException
    , _NoSuchBucketException
    , _NoAvailableDeliveryChannelException
    , _InvalidConfigurationRecorderNameException
    , _NoRunningConfigurationRecorderException
    , _MaxNumberOfConfigurationRecordersExceededException
    , _InsufficientDeliveryPolicyException
    , _MaxNumberOfDeliveryChannelsExceededException
    , _NoSuchConfigurationRecorderException
    , _InvalidS3KeyPrefixException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ChronologicalOrder
    , ChronologicalOrder (..)

    -- * ComplianceType
    , ComplianceType (..)

    -- * ConfigRuleState
    , ConfigRuleState (..)

    -- * ConfigurationItemStatus
    , ConfigurationItemStatus (..)

    -- * DeliveryStatus
    , DeliveryStatus (..)

    -- * EventSource
    , EventSource (..)

    -- * MaximumExecutionFrequency
    , MaximumExecutionFrequency (..)

    -- * MessageType
    , MessageType (..)

    -- * Owner
    , Owner (..)

    -- * RecorderStatus
    , RecorderStatus (..)

    -- * ResourceType
    , ResourceType (..)

    -- * Compliance
    , Compliance
    , compliance
    , cComplianceContributorCount
    , cComplianceType

    -- * ComplianceByConfigRule
    , ComplianceByConfigRule
    , complianceByConfigRule
    , cbcrCompliance
    , cbcrConfigRuleName

    -- * ComplianceByResource
    , ComplianceByResource
    , complianceByResource
    , cbrResourceId
    , cbrResourceType
    , cbrCompliance

    -- * ComplianceContributorCount
    , ComplianceContributorCount
    , complianceContributorCount
    , cccCappedCount
    , cccCapExceeded

    -- * ComplianceSummary
    , ComplianceSummary
    , complianceSummary
    , csComplianceSummaryTimestamp
    , csCompliantResourceCount
    , csNonCompliantResourceCount

    -- * ComplianceSummaryByResourceType
    , ComplianceSummaryByResourceType
    , complianceSummaryByResourceType
    , csbrtResourceType
    , csbrtComplianceSummary

    -- * ConfigExportDeliveryInfo
    , ConfigExportDeliveryInfo
    , configExportDeliveryInfo
    , cediLastErrorCode
    , cediLastAttemptTime
    , cediLastSuccessfulTime
    , cediLastStatus
    , cediLastErrorMessage
    , cediNextDeliveryTime

    -- * ConfigRule
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

    -- * ConfigRuleEvaluationStatus
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

    -- * ConfigSnapshotDeliveryProperties
    , ConfigSnapshotDeliveryProperties
    , configSnapshotDeliveryProperties
    , csdpDeliveryFrequency

    -- * ConfigStreamDeliveryInfo
    , ConfigStreamDeliveryInfo
    , configStreamDeliveryInfo
    , csdiLastErrorCode
    , csdiLastStatusChangeTime
    , csdiLastStatus
    , csdiLastErrorMessage

    -- * ConfigurationItem
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

    -- * ConfigurationRecorder
    , ConfigurationRecorder
    , configurationRecorder
    , crName
    , crRecordingGroup
    , crRoleARN

    -- * ConfigurationRecorderStatus
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

    -- * DeliveryChannel
    , DeliveryChannel
    , deliveryChannel
    , dcS3KeyPrefix
    , dcSnsTopicARN
    , dcName
    , dcConfigSnapshotDeliveryProperties
    , dcS3BucketName

    -- * DeliveryChannelStatus
    , DeliveryChannelStatus
    , deliveryChannelStatus
    , dcsConfigSnapshotDeliveryInfo
    , dcsConfigStreamDeliveryInfo
    , dcsConfigHistoryDeliveryInfo
    , dcsName

    -- * Evaluation
    , Evaluation
    , evaluation
    , eAnnotation
    , eComplianceResourceType
    , eComplianceResourceId
    , eComplianceType
    , eOrderingTimestamp

    -- * EvaluationResult
    , EvaluationResult
    , evaluationResult
    , erEvaluationResultIdentifier
    , erAnnotation
    , erConfigRuleInvokedTime
    , erResultRecordedTime
    , erResultToken
    , erComplianceType

    -- * EvaluationResultIdentifier
    , EvaluationResultIdentifier
    , evaluationResultIdentifier
    , eriEvaluationResultQualifier
    , eriOrderingTimestamp

    -- * EvaluationResultQualifier
    , EvaluationResultQualifier
    , evaluationResultQualifier
    , erqResourceId
    , erqResourceType
    , erqConfigRuleName

    -- * RecordingGroup
    , RecordingGroup
    , recordingGroup
    , rgAllSupported
    , rgIncludeGlobalResourceTypes
    , rgResourceTypes

    -- * Relationship
    , Relationship
    , relationship
    , rResourceId
    , rResourceType
    , rResourceName
    , rRelationshipName

    -- * ResourceCount
    , ResourceCount
    , resourceCount
    , rcResourceType
    , rcCount

    -- * ResourceIdentifier
    , ResourceIdentifier
    , resourceIdentifier
    , riResourceId
    , riResourceType
    , riResourceName
    , riResourceDeletionTime

    -- * Scope
    , Scope
    , scope
    , sComplianceResourceTypes
    , sComplianceResourceId
    , sTagValue
    , sTagKey

    -- * Source
    , Source
    , source
    , sSourceDetails
    , sOwner
    , sSourceIdentifier

    -- * SourceDetail
    , SourceDetail
    , sourceDetail
    , sdMessageType
    , sdMaximumExecutionFrequency
    , sdEventSource
    ) where

import Network.AWS.Config.Types.Product
import Network.AWS.Config.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2014-11-12@ of the Amazon Config SDK configuration.
config :: Service
config =
  Service
  { _svcAbbrev = "Config"
  , _svcSigner = v4
  , _svcPrefix = "config"
  , _svcVersion = "2014-11-12"
  , _svcEndpoint = defaultEndpoint config
  , _svcTimeout = Just 70
  , _svcCheck = statusSuccess
  , _svcError = parseJSONError "Config"
  , _svcRetry = retry
  }
  where
    retry =
      Exponential
      { _retryBase = 5.0e-2
      , _retryGrowth = 2
      , _retryAttempts = 5
      , _retryCheck = check
      }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The specified time range is not valid. The earlier time is not chronologically before the later time.
--
--
_InvalidTimeRangeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTimeRangeException =
  _MatchServiceError config "InvalidTimeRangeException"


-- | The specified Amazon SNS topic does not exist.
--
--
_InvalidSNSTopicARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicARNException =
  _MatchServiceError config "InvalidSNSTopicARNException"


-- | AWS Config throws an exception if the recording group does not contain a valid list of resource types. Invalid values could also be incorrectly formatted.
--
--
_InvalidRecordingGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRecordingGroupException =
  _MatchServiceError config "InvalidRecordingGroupException"


-- | The requested action is not valid.
--
--
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException = _MatchServiceError config "ValidationException"


-- | You have provided a null or empty role ARN.
--
--
_InvalidRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRoleException = _MatchServiceError config "InvalidRoleException"


-- | You cannot delete the delivery channel you specified because the configuration recorder is running.
--
--
_LastDeliveryChannelDeleteFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_LastDeliveryChannelDeleteFailedException =
  _MatchServiceError config "LastDeliveryChannelDeleteFailedException"


-- | The specified limit is outside the allowable range.
--
--
_InvalidLimitException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLimitException = _MatchServiceError config "InvalidLimitException"


-- | The specified delivery channel name is not valid.
--
--
_InvalidDeliveryChannelNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeliveryChannelNameException =
  _MatchServiceError config "InvalidDeliveryChannelNameException"


-- | One or more of the specified parameters are invalid. Verify that your parameters are valid and try again.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
  _MatchServiceError config "InvalidParameterValueException"


-- | The specified @ResultToken@ is invalid.
--
--
_InvalidResultTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResultTokenException =
  _MatchServiceError config "InvalidResultTokenException"


-- | You have specified a delivery channel that does not exist.
--
--
_NoSuchDeliveryChannelException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDeliveryChannelException =
  _MatchServiceError config "NoSuchDeliveryChannelException"


-- | One or more AWS Config rules in the request are invalid. Verify that the rule names are correct and try again.
--
--
_NoSuchConfigRuleException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchConfigRuleException =
  _MatchServiceError config "NoSuchConfigRuleException"


-- | Indicates one of the following errors:
--
--
--     * The rule cannot be created because the IAM role assigned to AWS Config lacks permissions to perform the config:Put* action.
--
--     * The AWS Lambda function cannot be invoked. Check the function ARN, and check the function's permissions.
--
--
--
_InsufficientPermissionsException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientPermissionsException =
  _MatchServiceError config "InsufficientPermissionsException"


-- | You have specified a resource that is either unknown or has not been discovered.
--
--
_ResourceNotDiscoveredException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotDiscoveredException =
  _MatchServiceError config "ResourceNotDiscoveredException"


-- | The specified next token is invalid. Specify the @NextToken@ string that was returned in the previous response to get the next page of results.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError config "InvalidNextTokenException"


-- | Failed to add the AWS Config rule because the account already contains the maximum number of 50 rules. Consider deleting any deactivated rules before adding new rules.
--
--
_MaxNumberOfConfigRulesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfConfigRulesExceededException =
  _MatchServiceError config "MaxNumberOfConfigRulesExceededException"


-- | There are no configuration recorders available to provide the role needed to describe your resources. Create a configuration recorder.
--
--
_NoAvailableConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableConfigurationRecorderException =
  _MatchServiceError config "NoAvailableConfigurationRecorderException"


-- | The specified Amazon S3 bucket does not exist.
--
--
_NoSuchBucketException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchBucketException = _MatchServiceError config "NoSuchBucketException"


-- | There is no delivery channel available to record configurations.
--
--
_NoAvailableDeliveryChannelException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableDeliveryChannelException =
  _MatchServiceError config "NoAvailableDeliveryChannelException"


-- | You have provided a configuration recorder name that is not valid.
--
--
_InvalidConfigurationRecorderNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationRecorderNameException =
  _MatchServiceError config "InvalidConfigurationRecorderNameException"


-- | There is no configuration recorder running.
--
--
_NoRunningConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoRunningConfigurationRecorderException =
  _MatchServiceError config "NoRunningConfigurationRecorderException"


-- | You have reached the limit on the number of recorders you can create.
--
--
_MaxNumberOfConfigurationRecordersExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfConfigurationRecordersExceededException =
  _MatchServiceError config "MaxNumberOfConfigurationRecordersExceededException"


-- | Your Amazon S3 bucket policy does not permit AWS Config to write to it.
--
--
_InsufficientDeliveryPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDeliveryPolicyException =
  _MatchServiceError config "InsufficientDeliveryPolicyException"


-- | You have reached the limit on the number of delivery channels you can create.
--
--
_MaxNumberOfDeliveryChannelsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfDeliveryChannelsExceededException =
  _MatchServiceError config "MaxNumberOfDeliveryChannelsExceededException"


-- | You have specified a configuration recorder that does not exist.
--
--
_NoSuchConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchConfigurationRecorderException =
  _MatchServiceError config "NoSuchConfigurationRecorderException"


-- | The specified Amazon S3 key prefix is not valid.
--
--
_InvalidS3KeyPrefixException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3KeyPrefixException =
  _MatchServiceError config "InvalidS3KeyPrefixException"


-- | This exception is thrown if an evaluation is in progress or if you call the 'StartConfigRulesEvaluation' API more than once per minute.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError config "LimitExceededException"


-- | The rule is currently being deleted or the rule is deleting your evaluation results. Try your request again later.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException = _MatchServiceError config "ResourceInUseException"

