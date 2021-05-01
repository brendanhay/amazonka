{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InsufficientDependencyServiceAccessPermissionException,
    _InvalidCloudWatchLogsLogGroupArnException,
    _TrailNotProvidedException,
    _MaximumNumberOfTrailsExceededException,
    _NotOrganizationMasterAccountException,
    _InsightNotEnabledException,
    _KmsKeyNotFoundException,
    _ResourceTypeNotSupportedException,
    _InvalidS3PrefixException,
    _InvalidParameterCombinationException,
    _InvalidInsightSelectorsException,
    _InvalidLookupAttributesException,
    _OrganizationNotInAllFeaturesModeException,
    _TrailAlreadyExistsException,
    _TagsLimitExceededException,
    _KmsKeyDisabledException,
    _OperationNotPermittedException,
    _InvalidTagParameterException,
    _InsufficientEncryptionPolicyException,
    _InsufficientSnsTopicPolicyException,
    _S3BucketDoesNotExistException,
    _InvalidNextTokenException,
    _CloudTrailInvalidClientTokenIdException,
    _InvalidS3BucketNameException,
    _UnsupportedOperationException,
    _InvalidTimeRangeException,
    _InsufficientS3BucketPolicyException,
    _InvalidEventSelectorsException,
    _ConflictException,
    _InvalidKmsKeyIdException,
    _InvalidHomeRegionException,
    _TrailNotFoundException,
    _CloudWatchLogsDeliveryUnavailableException,
    _OrganizationsNotInUseException,
    _InvalidSnsTopicNameException,
    _ResourceNotFoundException,
    _InvalidTrailNameException,
    _InvalidMaxResultsException,
    _CloudTrailAccessNotEnabledException,
    _InvalidCloudWatchLogsRoleArnException,
    _CloudTrailARNInvalidException,
    _InvalidTokenException,
    _InvalidEventCategoryException,
    _KmsException,

    -- * EventCategory
    EventCategory (..),

    -- * InsightType
    InsightType (..),

    -- * LookupAttributeKey
    LookupAttributeKey (..),

    -- * ReadWriteType
    ReadWriteType (..),

    -- * AdvancedEventSelector
    AdvancedEventSelector (..),
    newAdvancedEventSelector,
    advancedEventSelector_name,
    advancedEventSelector_fieldSelectors,

    -- * AdvancedFieldSelector
    AdvancedFieldSelector (..),
    newAdvancedFieldSelector,
    advancedFieldSelector_notStartsWith,
    advancedFieldSelector_notEndsWith,
    advancedFieldSelector_notEquals,
    advancedFieldSelector_equals,
    advancedFieldSelector_startsWith,
    advancedFieldSelector_endsWith,
    advancedFieldSelector_field,

    -- * DataResource
    DataResource (..),
    newDataResource,
    dataResource_values,
    dataResource_type,

    -- * Event
    Event (..),
    newEvent,
    event_cloudTrailEvent,
    event_eventId,
    event_eventSource,
    event_readOnly,
    event_eventName,
    event_resources,
    event_eventTime,
    event_accessKeyId,
    event_username,

    -- * EventSelector
    EventSelector (..),
    newEventSelector,
    eventSelector_readWriteType,
    eventSelector_excludeManagementEventSources,
    eventSelector_includeManagementEvents,
    eventSelector_dataResources,

    -- * InsightSelector
    InsightSelector (..),
    newInsightSelector,
    insightSelector_insightType,

    -- * LookupAttribute
    LookupAttribute (..),
    newLookupAttribute,
    lookupAttribute_attributeKey,
    lookupAttribute_attributeValue,

    -- * PublicKey
    PublicKey (..),
    newPublicKey,
    publicKey_validityStartTime,
    publicKey_value,
    publicKey_validityEndTime,
    publicKey_fingerprint,

    -- * Resource
    Resource (..),
    newResource,
    resource_resourceType,
    resource_resourceName,

    -- * ResourceTag
    ResourceTag (..),
    newResourceTag,
    resourceTag_resourceId,
    resourceTag_tagsList,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * Trail
    Trail (..),
    newTrail,
    trail_trailARN,
    trail_logFileValidationEnabled,
    trail_isOrganizationTrail,
    trail_hasCustomEventSelectors,
    trail_snsTopicName,
    trail_includeGlobalServiceEvents,
    trail_homeRegion,
    trail_kmsKeyId,
    trail_s3KeyPrefix,
    trail_name,
    trail_cloudWatchLogsLogGroupArn,
    trail_isMultiRegionTrail,
    trail_s3BucketName,
    trail_cloudWatchLogsRoleArn,
    trail_snsTopicARN,
    trail_hasInsightSelectors,

    -- * TrailInfo
    TrailInfo (..),
    newTrailInfo,
    trailInfo_trailARN,
    trailInfo_homeRegion,
    trailInfo_name,
  )
where

import Network.AWS.CloudTrail.Types.AdvancedEventSelector
import Network.AWS.CloudTrail.Types.AdvancedFieldSelector
import Network.AWS.CloudTrail.Types.DataResource
import Network.AWS.CloudTrail.Types.Event
import Network.AWS.CloudTrail.Types.EventCategory
import Network.AWS.CloudTrail.Types.EventSelector
import Network.AWS.CloudTrail.Types.InsightSelector
import Network.AWS.CloudTrail.Types.InsightType
import Network.AWS.CloudTrail.Types.LookupAttribute
import Network.AWS.CloudTrail.Types.LookupAttributeKey
import Network.AWS.CloudTrail.Types.PublicKey
import Network.AWS.CloudTrail.Types.ReadWriteType
import Network.AWS.CloudTrail.Types.Resource
import Network.AWS.CloudTrail.Types.ResourceTag
import Network.AWS.CloudTrail.Types.Tag
import Network.AWS.CloudTrail.Types.Trail
import Network.AWS.CloudTrail.Types.TrailInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-11-01@ of the Amazon CloudTrail SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "CloudTrail",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "cloudtrail",
      Prelude._svcVersion = "2013-11-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CloudTrail",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | This exception is thrown when the IAM user or role that is used to
-- create the organization trail is lacking one or more required
-- permissions for creating an organization trail in a required service.
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization>.
_InsufficientDependencyServiceAccessPermissionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientDependencyServiceAccessPermissionException =
  Prelude._MatchServiceError
    defaultService
    "InsufficientDependencyServiceAccessPermissionException"

-- | This exception is thrown when the provided CloudWatch log group is not
-- valid.
_InvalidCloudWatchLogsLogGroupArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCloudWatchLogsLogGroupArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCloudWatchLogsLogGroupArnException"

-- | This exception is no longer in use.
_TrailNotProvidedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TrailNotProvidedException =
  Prelude._MatchServiceError
    defaultService
    "TrailNotProvidedException"

-- | This exception is thrown when the maximum number of trails is reached.
_MaximumNumberOfTrailsExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumNumberOfTrailsExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumNumberOfTrailsExceededException"

-- | This exception is thrown when the AWS account making the request to
-- create or update an organization trail is not the master account for an
-- organization in AWS Organizations. For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization>.
_NotOrganizationMasterAccountException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotOrganizationMasterAccountException =
  Prelude._MatchServiceError
    defaultService
    "NotOrganizationMasterAccountException"

-- | If you run @GetInsightSelectors@ on a trail that does not have Insights
-- events enabled, the operation throws the exception
-- @InsightNotEnabledException@.
_InsightNotEnabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsightNotEnabledException =
  Prelude._MatchServiceError
    defaultService
    "InsightNotEnabledException"

-- | This exception is thrown when the KMS key does not exist, when the S3
-- bucket and the KMS key are not in the same region, or when the KMS key
-- associated with the SNS topic either does not exist or is not in the
-- same region.
_KmsKeyNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KmsKeyNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "KmsKeyNotFoundException"

-- | This exception is thrown when the specified resource type is not
-- supported by CloudTrail.
_ResourceTypeNotSupportedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceTypeNotSupportedException =
  Prelude._MatchServiceError
    defaultService
    "ResourceTypeNotSupportedException"

-- | This exception is thrown when the provided S3 prefix is not valid.
_InvalidS3PrefixException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidS3PrefixException =
  Prelude._MatchServiceError
    defaultService
    "InvalidS3PrefixException"

-- | This exception is thrown when the combination of parameters provided is
-- not valid.
_InvalidParameterCombinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterCombinationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

-- | The formatting or syntax of the @InsightSelectors@ JSON statement in
-- your @PutInsightSelectors@ or @GetInsightSelectors@ request is not
-- valid, or the specified insight type in the @InsightSelectors@ statement
-- is not a valid insight type.
_InvalidInsightSelectorsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInsightSelectorsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInsightSelectorsException"

-- | Occurs when an invalid lookup attribute is specified.
_InvalidLookupAttributesException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLookupAttributesException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLookupAttributesException"

-- | This exception is thrown when AWS Organizations is not configured to
-- support all features. All features must be enabled in AWS Organization
-- to support creating an organization trail. For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization>.
_OrganizationNotInAllFeaturesModeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OrganizationNotInAllFeaturesModeException =
  Prelude._MatchServiceError
    defaultService
    "OrganizationNotInAllFeaturesModeException"

-- | This exception is thrown when the specified trail already exists.
_TrailAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TrailAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "TrailAlreadyExistsException"

-- | The number of tags per trail has exceeded the permitted amount.
-- Currently, the limit is 50.
_TagsLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagsLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "TagsLimitExceededException"

-- | This exception is no longer in use.
_KmsKeyDisabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KmsKeyDisabledException =
  Prelude._MatchServiceError
    defaultService
    "KmsKeyDisabledException"

-- | This exception is thrown when the requested operation is not permitted.
_OperationNotPermittedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OperationNotPermittedException =
  Prelude._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | This exception is thrown when the specified tag key or values are not
-- valid. It can also occur if there are duplicate tags or too many tags on
-- the resource.
_InvalidTagParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagParameterException"

-- | This exception is thrown when the policy on the S3 bucket or KMS key is
-- not sufficient.
_InsufficientEncryptionPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientEncryptionPolicyException =
  Prelude._MatchServiceError
    defaultService
    "InsufficientEncryptionPolicyException"

-- | This exception is thrown when the policy on the SNS topic is not
-- sufficient.
_InsufficientSnsTopicPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientSnsTopicPolicyException =
  Prelude._MatchServiceError
    defaultService
    "InsufficientSnsTopicPolicyException"

-- | This exception is thrown when the specified S3 bucket does not exist.
_S3BucketDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_S3BucketDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "S3BucketDoesNotExistException"

-- | Invalid token or token that was previously used in a request with
-- different parameters. This exception is thrown if the token is invalid.
_InvalidNextTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | This exception is thrown when a call results in the
-- @InvalidClientTokenId@ error code. This can occur when you are creating
-- or updating a trail to send notifications to an Amazon SNS topic that is
-- in a suspended AWS account.
_CloudTrailInvalidClientTokenIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudTrailInvalidClientTokenIdException =
  Prelude._MatchServiceError
    defaultService
    "CloudTrailInvalidClientTokenIdException"

-- | This exception is thrown when the provided S3 bucket name is not valid.
_InvalidS3BucketNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidS3BucketNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidS3BucketNameException"

-- | This exception is thrown when the requested operation is not supported.
_UnsupportedOperationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedOperationException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | Occurs if the timestamp values are invalid. Either the start time occurs
-- after the end time or the time range is outside the range of possible
-- values.
_InvalidTimeRangeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTimeRangeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTimeRangeException"

-- | This exception is thrown when the policy on the S3 bucket is not
-- sufficient.
_InsufficientS3BucketPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientS3BucketPolicyException =
  Prelude._MatchServiceError
    defaultService
    "InsufficientS3BucketPolicyException"

-- | This exception is thrown when the @PutEventSelectors@ operation is
-- called with a number of event selectors, advanced event selectors, or
-- data resources that is not valid. The combination of event selectors or
-- advanced event selectors and data resources is not valid. A trail can
-- have up to 5 event selectors. If a trail uses advanced event selectors,
-- a maximum of 500 total values for all conditions in all advanced event
-- selectors is allowed. A trail is limited to 250 data resources. These
-- data resources can be distributed across event selectors, but the
-- overall total cannot exceed 250.
--
-- You can:
--
-- -   Specify a valid number of event selectors (1 to 5) for a trail.
--
-- -   Specify a valid number of data resources (1 to 250) for an event
--     selector. The limit of number of resources on an individual event
--     selector is configurable up to 250. However, this upper limit is
--     allowed only if the total number of data resources does not exceed
--     250 across all event selectors for a trail.
--
-- -   Specify up to 500 values for all conditions in all advanced event
--     selectors for a trail.
--
-- -   Specify a valid value for a parameter. For example, specifying the
--     @ReadWriteType@ parameter with a value of @read-only@ is invalid.
_InvalidEventSelectorsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidEventSelectorsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidEventSelectorsException"

-- | This exception is thrown when the specified resource is not ready for an
-- operation. This can occur when you try to run an operation on a trail
-- before CloudTrail has time to fully load the trail. If this exception
-- occurs, wait a few minutes, and then try the operation again.
_ConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConflictException =
  Prelude._MatchServiceError
    defaultService
    "ConflictException"

-- | This exception is thrown when the KMS key ARN is invalid.
_InvalidKmsKeyIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidKmsKeyIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidKmsKeyIdException"

-- | This exception is thrown when an operation is called on a trail from a
-- region other than the region in which the trail was created.
_InvalidHomeRegionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidHomeRegionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidHomeRegionException"

-- | This exception is thrown when the trail with the given name is not
-- found.
_TrailNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TrailNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "TrailNotFoundException"

-- | Cannot set a CloudWatch Logs delivery for this region.
_CloudWatchLogsDeliveryUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudWatchLogsDeliveryUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "CloudWatchLogsDeliveryUnavailableException"

-- | This exception is thrown when the request is made from an AWS account
-- that is not a member of an organization. To make this request, sign in
-- using the credentials of an account that belongs to an organization.
_OrganizationsNotInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OrganizationsNotInUseException =
  Prelude._MatchServiceError
    defaultService
    "OrganizationsNotInUseException"

-- | This exception is thrown when the provided SNS topic name is not valid.
_InvalidSnsTopicNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSnsTopicNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSnsTopicNameException"

-- | This exception is thrown when the specified resource is not found.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | This exception is thrown when the provided trail name is not valid.
-- Trail names must meet the following requirements:
--
-- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
--     underscores (_), or dashes (-)
--
-- -   Start with a letter or number, and end with a letter or number
--
-- -   Be between 3 and 128 characters
--
-- -   Have no adjacent periods, underscores or dashes. Names like
--     @my-_namespace@ and @my--namespace@ are invalid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
_InvalidTrailNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTrailNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTrailNameException"

-- | This exception is thrown if the limit specified is invalid.
_InvalidMaxResultsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMaxResultsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidMaxResultsException"

-- | This exception is thrown when trusted access has not been enabled
-- between AWS CloudTrail and AWS Organizations. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Enabling Trusted Access with Other AWS Services>
-- and
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/creating-an-organizational-trail-prepare.html Prepare For Creating a Trail For Your Organization>.
_CloudTrailAccessNotEnabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudTrailAccessNotEnabledException =
  Prelude._MatchServiceError
    defaultService
    "CloudTrailAccessNotEnabledException"

-- | This exception is thrown when the provided role is not valid.
_InvalidCloudWatchLogsRoleArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCloudWatchLogsRoleArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCloudWatchLogsRoleArnException"

-- | This exception is thrown when an operation is called with an invalid
-- trail ARN. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
_CloudTrailARNInvalidException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudTrailARNInvalidException =
  Prelude._MatchServiceError
    defaultService
    "CloudTrailARNInvalidException"

-- | Reserved for future use.
_InvalidTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTokenException"

-- | Occurs if an event category that is not valid is specified as a value of
-- @EventCategory@.
_InvalidEventCategoryException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidEventCategoryException =
  Prelude._MatchServiceError
    defaultService
    "InvalidEventCategoryException"

-- | This exception is thrown when there is an issue with the specified KMS
-- key and the trail can’t be updated.
_KmsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KmsException =
  Prelude._MatchServiceError
    defaultService
    "KmsException"
