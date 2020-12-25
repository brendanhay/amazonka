-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidTimeRangeException,
    _InsufficientS3BucketPolicyException,
    _MaximumNumberOfTrailsExceededException,
    _InsufficientDependencyServiceAccessPermissionException,
    _UnsupportedOperationException,
    _InvalidEventCategoryException,
    _KmsKeyDisabledException,
    _InsufficientEncryptionPolicyException,
    _InsufficientSnsTopicPolicyException,
    _InvalidCloudWatchLogsRoleArnException,
    _CloudTrailAccessNotEnabledException,
    _TagsLimitExceededException,
    _CloudTrailARNInvalidException,
    _InvalidLookupAttributesException,
    _InvalidTrailNameException,
    _InvalidSnsTopicNameException,
    _ResourceTypeNotSupportedException,
    _CloudWatchLogsDeliveryUnavailableException,
    _OrganizationsNotInUseException,
    _KmsKeyNotFoundException,
    _TrailNotFoundException,
    _InsightNotEnabledException,
    _NotOrganizationMasterAccountException,
    _InvalidEventSelectorsException,
    _TrailNotProvidedException,
    _InvalidS3BucketNameException,
    _InvalidCloudWatchLogsLogGroupArnException,
    _KmsException,
    _S3BucketDoesNotExistException,
    _InvalidNextTokenException,
    _InvalidTagParameterException,
    _OperationNotPermittedException,
    _InvalidTokenException,
    _InvalidMaxResultsException,
    _TrailAlreadyExistsException,
    _OrganizationNotInAllFeaturesModeException,
    _InvalidInsightSelectorsException,
    _InvalidS3PrefixException,
    _ResourceNotFoundException,
    _InvalidParameterCombinationException,
    _InvalidKmsKeyIdException,
    _InvalidHomeRegionException,

    -- * TrailInfo
    TrailInfo (..),
    mkTrailInfo,
    tiHomeRegion,
    tiName,
    tiTrailARN,

    -- * EventCategory
    EventCategory (..),

    -- * Event
    Event (..),
    mkEvent,
    eAccessKeyId,
    eCloudTrailEvent,
    eEventId,
    eEventName,
    eEventSource,
    eEventTime,
    eReadOnly,
    eResources,
    eUsername,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Trail
    Trail (..),
    mkTrail,
    tCloudWatchLogsLogGroupArn,
    tCloudWatchLogsRoleArn,
    tHasCustomEventSelectors,
    tHasInsightSelectors,
    tHomeRegion,
    tIncludeGlobalServiceEvents,
    tIsMultiRegionTrail,
    tIsOrganizationTrail,
    tKmsKeyId,
    tLogFileValidationEnabled,
    tName,
    tS3BucketName,
    tS3KeyPrefix,
    tSnsTopicARN,
    tSnsTopicName,
    tTrailARN,

    -- * String
    String (..),

    -- * OperatorValue
    OperatorValue (..),

    -- * LookupAttribute
    LookupAttribute (..),
    mkLookupAttribute,
    laAttributeKey,
    laAttributeValue,

    -- * PublicKey
    PublicKey (..),
    mkPublicKey,
    pkFingerprint,
    pkValidityEndTime,
    pkValidityStartTime,
    pkValue,

    -- * EventSelector
    EventSelector (..),
    mkEventSelector,
    esDataResources,
    esExcludeManagementEventSources,
    esIncludeManagementEvents,
    esReadWriteType,

    -- * LookupAttributeKey
    LookupAttributeKey (..),

    -- * NextToken
    NextToken (..),

    -- * ReadWriteType
    ReadWriteType (..),

    -- * SelectorField
    SelectorField (..),

    -- * AdvancedEventSelector
    AdvancedEventSelector (..),
    mkAdvancedEventSelector,
    aesName,
    aesFieldSelectors,

    -- * Resource
    Resource (..),
    mkResource,
    rResourceName,
    rResourceType,

    -- * DataResource
    DataResource (..),
    mkDataResource,
    drType,
    drValues,

    -- * ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rtResourceId,
    rtTagsList,

    -- * InsightSelector
    InsightSelector (..),
    mkInsightSelector,
    isInsightType,

    -- * InsightType
    InsightType (..),

    -- * AdvancedFieldSelector
    AdvancedFieldSelector (..),
    mkAdvancedFieldSelector,
    afsField,
    afsEndsWith,
    afsEquals,
    afsNotEndsWith,
    afsNotEquals,
    afsNotStartsWith,
    afsStartsWith,

    -- * HomeRegion
    HomeRegion (..),

    -- * Name
    Name (..),

    -- * TrailARN
    TrailARN (..),

    -- * AccessKeyId
    AccessKeyId (..),

    -- * CloudTrailEvent
    CloudTrailEvent (..),

    -- * EventId
    EventId (..),

    -- * EventName
    EventName (..),

    -- * EventSource
    EventSource (..),

    -- * ReadOnly
    ReadOnly (..),

    -- * Username
    Username (..),

    -- * S3BucketName
    S3BucketName (..),

    -- * CloudWatchLogsLogGroupArn
    CloudWatchLogsLogGroupArn (..),

    -- * CloudWatchLogsRoleArn
    CloudWatchLogsRoleArn (..),

    -- * KmsKeyId
    KmsKeyId (..),

    -- * S3KeyPrefix
    S3KeyPrefix (..),

    -- * SnsTopicName
    SnsTopicName (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * SnsTopicARN
    SnsTopicARN (..),

    -- * TrailName
    TrailName (..),
  )
where

import Network.AWS.CloudTrail.Types.AccessKeyId
import Network.AWS.CloudTrail.Types.AdvancedEventSelector
import Network.AWS.CloudTrail.Types.AdvancedFieldSelector
import Network.AWS.CloudTrail.Types.CloudTrailEvent
import Network.AWS.CloudTrail.Types.CloudWatchLogsLogGroupArn
import Network.AWS.CloudTrail.Types.CloudWatchLogsRoleArn
import Network.AWS.CloudTrail.Types.DataResource
import Network.AWS.CloudTrail.Types.Event
import Network.AWS.CloudTrail.Types.EventCategory
import Network.AWS.CloudTrail.Types.EventId
import Network.AWS.CloudTrail.Types.EventName
import Network.AWS.CloudTrail.Types.EventSelector
import Network.AWS.CloudTrail.Types.EventSource
import Network.AWS.CloudTrail.Types.HomeRegion
import Network.AWS.CloudTrail.Types.InsightSelector
import Network.AWS.CloudTrail.Types.InsightType
import Network.AWS.CloudTrail.Types.Key
import Network.AWS.CloudTrail.Types.KmsKeyId
import Network.AWS.CloudTrail.Types.LookupAttribute
import Network.AWS.CloudTrail.Types.LookupAttributeKey
import Network.AWS.CloudTrail.Types.Name
import Network.AWS.CloudTrail.Types.NextToken
import Network.AWS.CloudTrail.Types.OperatorValue
import Network.AWS.CloudTrail.Types.PublicKey
import Network.AWS.CloudTrail.Types.ReadOnly
import Network.AWS.CloudTrail.Types.ReadWriteType
import Network.AWS.CloudTrail.Types.Resource
import Network.AWS.CloudTrail.Types.ResourceTag
import Network.AWS.CloudTrail.Types.S3BucketName
import Network.AWS.CloudTrail.Types.S3KeyPrefix
import Network.AWS.CloudTrail.Types.SelectorField
import Network.AWS.CloudTrail.Types.SnsTopicARN
import Network.AWS.CloudTrail.Types.SnsTopicName
import Network.AWS.CloudTrail.Types.String
import Network.AWS.CloudTrail.Types.Tag
import Network.AWS.CloudTrail.Types.Trail
import Network.AWS.CloudTrail.Types.TrailARN
import Network.AWS.CloudTrail.Types.TrailInfo
import Network.AWS.CloudTrail.Types.TrailName
import Network.AWS.CloudTrail.Types.Username
import Network.AWS.CloudTrail.Types.Value
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-11-01@ of the Amazon CloudTrail SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CloudTrail",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "cloudtrail",
      Core._svcVersion = "2013-11-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CloudTrail",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | Occurs if the timestamp values are invalid. Either the start time occurs after the end time or the time range is outside the range of possible values.
_InvalidTimeRangeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTimeRangeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidTimeRangeException"
{-# DEPRECATED _InvalidTimeRangeException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the policy on the S3 bucket is not sufficient.
_InsufficientS3BucketPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientS3BucketPolicyException =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientS3BucketPolicyException"
{-# DEPRECATED _InsufficientS3BucketPolicyException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the maximum number of trails is reached.
_MaximumNumberOfTrailsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumNumberOfTrailsExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumNumberOfTrailsExceededException"
{-# DEPRECATED _MaximumNumberOfTrailsExceededException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the IAM user or role that is used to create the organization trail is lacking one or more required permissions for creating an organization trail in a required service. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization> .
_InsufficientDependencyServiceAccessPermissionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientDependencyServiceAccessPermissionException =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientDependencyServiceAccessPermissionException"
{-# DEPRECATED _InsufficientDependencyServiceAccessPermissionException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the requested operation is not supported.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedOperationException"
{-# DEPRECATED _UnsupportedOperationException "Use generic-lens or generic-optics instead." #-}

-- | Occurs if an event category that is not valid is specified as a value of @EventCategory@ .
_InvalidEventCategoryException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEventCategoryException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidEventCategoryException"
{-# DEPRECATED _InvalidEventCategoryException "Use generic-lens or generic-optics instead." #-}

-- | This exception is no longer in use.
_KmsKeyDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KmsKeyDisabledException =
  Core._MatchServiceError mkServiceConfig "KmsKeyDisabledException"
{-# DEPRECATED _KmsKeyDisabledException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the policy on the S3 bucket or KMS key is not sufficient.
_InsufficientEncryptionPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientEncryptionPolicyException =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientEncryptionPolicyException"
{-# DEPRECATED _InsufficientEncryptionPolicyException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the policy on the SNS topic is not sufficient.
_InsufficientSnsTopicPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientSnsTopicPolicyException =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientSnsTopicPolicyException"
{-# DEPRECATED _InsufficientSnsTopicPolicyException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the provided role is not valid.
_InvalidCloudWatchLogsRoleArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCloudWatchLogsRoleArnException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidCloudWatchLogsRoleArnException"
{-# DEPRECATED _InvalidCloudWatchLogsRoleArnException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when trusted access has not been enabled between AWS CloudTrail and AWS Organizations. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Enabling Trusted Access with Other AWS Services> and <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization> .
_CloudTrailAccessNotEnabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudTrailAccessNotEnabledException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudTrailAccessNotEnabledException"
{-# DEPRECATED _CloudTrailAccessNotEnabledException "Use generic-lens or generic-optics instead." #-}

-- | The number of tags per trail has exceeded the permitted amount. Currently, the limit is 50.
_TagsLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagsLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "TagsLimitExceededException"
{-# DEPRECATED _TagsLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when an operation is called with an invalid trail ARN. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
_CloudTrailARNInvalidException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudTrailARNInvalidException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudTrailARNInvalidException"
{-# DEPRECATED _CloudTrailARNInvalidException "Use generic-lens or generic-optics instead." #-}

-- | Occurs when an invalid lookup attribute is specified.
_InvalidLookupAttributesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLookupAttributesException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidLookupAttributesException"
{-# DEPRECATED _InvalidLookupAttributesException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the provided trail name is not valid. Trail names must meet the following requirements:
--
--
--     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
--
--
--     * Start with a letter or number, and end with a letter or number
--
--
--     * Be between 3 and 128 characters
--
--
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
--
--
--     * Not be in IP address format (for example, 192.168.5.4)
_InvalidTrailNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTrailNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidTrailNameException"
{-# DEPRECATED _InvalidTrailNameException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the provided SNS topic name is not valid.
_InvalidSnsTopicNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSnsTopicNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidSnsTopicNameException"
{-# DEPRECATED _InvalidSnsTopicNameException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the specified resource type is not supported by CloudTrail.
_ResourceTypeNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceTypeNotSupportedException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceTypeNotSupportedException"
{-# DEPRECATED _ResourceTypeNotSupportedException "Use generic-lens or generic-optics instead." #-}

-- | Cannot set a CloudWatch Logs delivery for this region.
_CloudWatchLogsDeliveryUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudWatchLogsDeliveryUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudWatchLogsDeliveryUnavailableException"
{-# DEPRECATED _CloudWatchLogsDeliveryUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the request is made from an AWS account that is not a member of an organization. To make this request, sign in using the credentials of an account that belongs to an organization.
_OrganizationsNotInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationsNotInUseException =
  Core._MatchServiceError
    mkServiceConfig
    "OrganizationsNotInUseException"
{-# DEPRECATED _OrganizationsNotInUseException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the KMS key does not exist, when the S3 bucket and the KMS key are not in the same region, or when the KMS key associated with the SNS topic either does not exist or is not in the same region.
_KmsKeyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KmsKeyNotFoundException =
  Core._MatchServiceError mkServiceConfig "KmsKeyNotFoundException"
{-# DEPRECATED _KmsKeyNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the trail with the given name is not found.
_TrailNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrailNotFoundException =
  Core._MatchServiceError mkServiceConfig "TrailNotFoundException"
{-# DEPRECATED _TrailNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | If you run @GetInsightSelectors@ on a trail that does not have Insights events enabled, the operation throws the exception @InsightNotEnabledException@ .
_InsightNotEnabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsightNotEnabledException =
  Core._MatchServiceError
    mkServiceConfig
    "InsightNotEnabledException"
{-# DEPRECATED _InsightNotEnabledException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the AWS account making the request to create or update an organization trail is not the master account for an organization in AWS Organizations. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization> .
_NotOrganizationMasterAccountException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotOrganizationMasterAccountException =
  Core._MatchServiceError
    mkServiceConfig
    "NotOrganizationMasterAccountException"
{-# DEPRECATED _NotOrganizationMasterAccountException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the @PutEventSelectors@ operation is called with a number of event selectors or data resources that is not valid. The combination of event selectors and data resources is not valid. A trail can have up to 5 event selectors. A trail is limited to 250 data resources. These data resources can be distributed across event selectors, but the overall total cannot exceed 250.
--
-- You can:
--
--     * Specify a valid number of event selectors (1 to 5) for a trail.
--
--
--     * Specify a valid number of data resources (1 to 250) for an event selector. The limit of number of resources on an individual event selector is configurable up to 250. However, this upper limit is allowed only if the total number of data resources does not exceed 250 across all event selectors for a trail.
--
--
--     * Specify a valid value for a parameter. For example, specifying the @ReadWriteType@ parameter with a value of @read-only@ is invalid.
_InvalidEventSelectorsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEventSelectorsException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidEventSelectorsException"
{-# DEPRECATED _InvalidEventSelectorsException "Use generic-lens or generic-optics instead." #-}

-- | This exception is no longer in use.
_TrailNotProvidedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrailNotProvidedException =
  Core._MatchServiceError
    mkServiceConfig
    "TrailNotProvidedException"
{-# DEPRECATED _TrailNotProvidedException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the provided S3 bucket name is not valid.
_InvalidS3BucketNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3BucketNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidS3BucketNameException"
{-# DEPRECATED _InvalidS3BucketNameException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the provided CloudWatch log group is not valid.
_InvalidCloudWatchLogsLogGroupArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCloudWatchLogsLogGroupArnException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidCloudWatchLogsLogGroupArnException"
{-# DEPRECATED _InvalidCloudWatchLogsLogGroupArnException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when there is an issue with the specified KMS key and the trail can’t be updated.
_KmsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KmsException =
  Core._MatchServiceError mkServiceConfig "KmsException"
{-# DEPRECATED _KmsException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the specified S3 bucket does not exist.
_S3BucketDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_S3BucketDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "S3BucketDoesNotExistException"
{-# DEPRECATED _S3BucketDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | Invalid token or token that was previously used in a request with different parameters. This exception is thrown if the token is invalid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidNextTokenException"
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the specified tag key or values are not valid. It can also occur if there are duplicate tags or too many tags on the resource.
_InvalidTagParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidTagParameterException"
{-# DEPRECATED _InvalidTagParameterException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the requested operation is not permitted.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationNotPermittedException"
{-# DEPRECATED _OperationNotPermittedException "Use generic-lens or generic-optics instead." #-}

-- | Reserved for future use.
_InvalidTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTokenException =
  Core._MatchServiceError mkServiceConfig "InvalidTokenException"
{-# DEPRECATED _InvalidTokenException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown if the limit specified is invalid.
_InvalidMaxResultsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMaxResultsException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidMaxResultsException"
{-# DEPRECATED _InvalidMaxResultsException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the specified trail already exists.
_TrailAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrailAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "TrailAlreadyExistsException"
{-# DEPRECATED _TrailAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when AWS Organizations is not configured to support all features. All features must be enabled in AWS Organization to support creating an organization trail. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization> .
_OrganizationNotInAllFeaturesModeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationNotInAllFeaturesModeException =
  Core._MatchServiceError
    mkServiceConfig
    "OrganizationNotInAllFeaturesModeException"
{-# DEPRECATED _OrganizationNotInAllFeaturesModeException "Use generic-lens or generic-optics instead." #-}

-- | The formatting or syntax of the @InsightSelectors@ JSON statement in your @PutInsightSelectors@ or @GetInsightSelectors@ request is not valid, or the specified insight type in the @InsightSelectors@ statement is not a valid insight type.
_InvalidInsightSelectorsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInsightSelectorsException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidInsightSelectorsException"
{-# DEPRECATED _InvalidInsightSelectorsException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the provided S3 prefix is not valid.
_InvalidS3PrefixException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3PrefixException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidS3PrefixException"
{-# DEPRECATED _InvalidS3PrefixException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the specified resource is not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the combination of parameters provided is not valid.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterCombinationException"
{-# DEPRECATED _InvalidParameterCombinationException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when the KMS key ARN is invalid.
_InvalidKmsKeyIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidKmsKeyIdException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidKmsKeyIdException"
{-# DEPRECATED _InvalidKmsKeyIdException "Use generic-lens or generic-optics instead." #-}

-- | This exception is thrown when an operation is called on a trail from a region other than the region in which the trail was created.
_InvalidHomeRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidHomeRegionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidHomeRegionException"
{-# DEPRECATED _InvalidHomeRegionException "Use generic-lens or generic-optics instead." #-}
