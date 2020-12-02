{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types
    (
    -- * Service Configuration
      cloudTrail

    -- * Errors
    , _InvalidTimeRangeException
    , _InsufficientS3BucketPolicyException
    , _MaximumNumberOfTrailsExceededException
    , _UnsupportedOperationException
    , _KMSKeyDisabledException
    , _InsufficientEncryptionPolicyException
    , _InsufficientSNSTopicPolicyException
    , _InvalidCloudWatchLogsRoleARNException
    , _TagsLimitExceededException
    , _CloudTrailARNInvalidException
    , _InvalidLookupAttributesException
    , _InvalidTrailNameException
    , _InvalidSNSTopicNameException
    , _ResourceTypeNotSupportedException
    , _CloudWatchLogsDeliveryUnavailableException
    , _KMSKeyNotFoundException
    , _TrailNotFoundException
    , _InvalidEventSelectorsException
    , _TrailNotProvidedException
    , _InvalidS3BucketNameException
    , _InvalidCloudWatchLogsLogGroupARNException
    , _KMSException
    , _S3BucketDoesNotExistException
    , _InvalidNextTokenException
    , _InvalidTagParameterException
    , _OperationNotPermittedException
    , _InvalidTokenException
    , _InvalidMaxResultsException
    , _TrailAlreadyExistsException
    , _InvalidS3PrefixException
    , _ResourceNotFoundException
    , _InvalidParameterCombinationException
    , _InvalidKMSKeyIdException
    , _InvalidHomeRegionException

    -- * LookupAttributeKey
    , LookupAttributeKey (..)

    -- * ReadWriteType
    , ReadWriteType (..)

    -- * DataResource
    , DataResource
    , dataResource
    , drValues
    , drType

    -- * Event
    , Event
    , event
    , eUsername
    , eResources
    , eEventTime
    , eCloudTrailEvent
    , eEventName
    , eEventSource
    , eEventId

    -- * EventSelector
    , EventSelector
    , eventSelector
    , esDataResources
    , esReadWriteType
    , esIncludeManagementEvents

    -- * LookupAttribute
    , LookupAttribute
    , lookupAttribute
    , laAttributeKey
    , laAttributeValue

    -- * PublicKey
    , PublicKey
    , publicKey
    , pkFingerprint
    , pkValidityEndTime
    , pkValue
    , pkValidityStartTime

    -- * Resource
    , Resource
    , resource
    , rResourceType
    , rResourceName

    -- * ResourceTag
    , ResourceTag
    , resourceTag
    , rResourceId
    , rTagsList

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * Trail
    , Trail
    , trail
    , tLogFileValidationEnabled
    , tTrailARN
    , tS3KeyPrefix
    , tSNSTopicARN
    , tSNSTopicName
    , tCloudWatchLogsLogGroupARN
    , tKMSKeyId
    , tHomeRegion
    , tName
    , tIncludeGlobalServiceEvents
    , tHasCustomEventSelectors
    , tCloudWatchLogsRoleARN
    , tS3BucketName
    , tIsMultiRegionTrail
    ) where

import Network.AWS.CloudTrail.Types.Product
import Network.AWS.CloudTrail.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2013-11-01@ of the Amazon CloudTrail SDK configuration.
cloudTrail :: Service
cloudTrail =
  Service
    { _svcAbbrev = "CloudTrail"
    , _svcSigner = v4
    , _svcPrefix = "cloudtrail"
    , _svcVersion = "2013-11-01"
    , _svcEndpoint = defaultEndpoint cloudTrail
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CloudTrail"
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
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | Occurs if the timestamp values are invalid. Either the start time occurs after the end time or the time range is outside the range of possible values.
--
--
_InvalidTimeRangeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTimeRangeException =
  _MatchServiceError cloudTrail "InvalidTimeRangeException"


-- | This exception is thrown when the policy on the S3 bucket is not sufficient.
--
--
_InsufficientS3BucketPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientS3BucketPolicyException =
  _MatchServiceError cloudTrail "InsufficientS3BucketPolicyException"


-- | This exception is thrown when the maximum number of trails is reached.
--
--
_MaximumNumberOfTrailsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumNumberOfTrailsExceededException =
  _MatchServiceError cloudTrail "MaximumNumberOfTrailsExceededException"


-- | This exception is thrown when the requested operation is not supported.
--
--
_UnsupportedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException =
  _MatchServiceError cloudTrail "UnsupportedOperationException"


-- | This exception is deprecated.
--
--
_KMSKeyDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSKeyDisabledException =
  _MatchServiceError cloudTrail "KmsKeyDisabledException"


-- | This exception is thrown when the policy on the S3 bucket or KMS key is not sufficient.
--
--
_InsufficientEncryptionPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientEncryptionPolicyException =
  _MatchServiceError cloudTrail "InsufficientEncryptionPolicyException"


-- | This exception is thrown when the policy on the SNS topic is not sufficient.
--
--
_InsufficientSNSTopicPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientSNSTopicPolicyException =
  _MatchServiceError cloudTrail "InsufficientSnsTopicPolicyException"


-- | This exception is thrown when the provided role is not valid.
--
--
_InvalidCloudWatchLogsRoleARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCloudWatchLogsRoleARNException =
  _MatchServiceError cloudTrail "InvalidCloudWatchLogsRoleArnException"


-- | The number of tags per trail has exceeded the permitted amount. Currently, the limit is 50.
--
--
_TagsLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TagsLimitExceededException =
  _MatchServiceError cloudTrail "TagsLimitExceededException"


-- | This exception is thrown when an operation is called with an invalid trail ARN. The format of a trail ARN is:
--
--
-- @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
--
_CloudTrailARNInvalidException :: AsError a => Getting (First ServiceError) a ServiceError
_CloudTrailARNInvalidException =
  _MatchServiceError cloudTrail "CloudTrailARNInvalidException"


-- | Occurs when an invalid lookup attribute is specified.
--
--
_InvalidLookupAttributesException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLookupAttributesException =
  _MatchServiceError cloudTrail "InvalidLookupAttributesException"


-- | This exception is thrown when the provided trail name is not valid. Trail names must meet the following requirements:
--
--
--     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
--
--     * Start with a letter or number, and end with a letter or number
--
--     * Be between 3 and 128 characters
--
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
--
--     * Not be in IP address format (for example, 192.168.5.4)
--
--
--
_InvalidTrailNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTrailNameException =
  _MatchServiceError cloudTrail "InvalidTrailNameException"


-- | This exception is thrown when the provided SNS topic name is not valid.
--
--
_InvalidSNSTopicNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicNameException =
  _MatchServiceError cloudTrail "InvalidSnsTopicNameException"


-- | This exception is thrown when the specified resource type is not supported by CloudTrail.
--
--
_ResourceTypeNotSupportedException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceTypeNotSupportedException =
  _MatchServiceError cloudTrail "ResourceTypeNotSupportedException"


-- | Cannot set a CloudWatch Logs delivery for this region.
--
--
_CloudWatchLogsDeliveryUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_CloudWatchLogsDeliveryUnavailableException =
  _MatchServiceError cloudTrail "CloudWatchLogsDeliveryUnavailableException"


-- | This exception is thrown when the KMS key does not exist, or when the S3 bucket and the KMS key are not in the same region.
--
--
_KMSKeyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSKeyNotFoundException =
  _MatchServiceError cloudTrail "KmsKeyNotFoundException"


-- | This exception is thrown when the trail with the given name is not found.
--
--
_TrailNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TrailNotFoundException = _MatchServiceError cloudTrail "TrailNotFoundException"


-- | This exception is thrown when the @PutEventSelectors@ operation is called with an invalid number of event selectors, data resources, or an invalid value for a parameter:
--
--
--     * Specify a valid number of event selectors (1 to 5) for a trail.
--
--     * Specify a valid number of data resources (1 to 250) for an event selector.
--
--     * Specify a valid value for a parameter. For example, specifying the @ReadWriteType@ parameter with a value of @read-only@ is invalid.
--
--
--
_InvalidEventSelectorsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEventSelectorsException =
  _MatchServiceError cloudTrail "InvalidEventSelectorsException"


-- | This exception is deprecated.
--
--
_TrailNotProvidedException :: AsError a => Getting (First ServiceError) a ServiceError
_TrailNotProvidedException =
  _MatchServiceError cloudTrail "TrailNotProvidedException"


-- | This exception is thrown when the provided S3 bucket name is not valid.
--
--
_InvalidS3BucketNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3BucketNameException =
  _MatchServiceError cloudTrail "InvalidS3BucketNameException"


-- | This exception is thrown when the provided CloudWatch log group is not valid.
--
--
_InvalidCloudWatchLogsLogGroupARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCloudWatchLogsLogGroupARNException =
  _MatchServiceError cloudTrail "InvalidCloudWatchLogsLogGroupArnException"


-- | This exception is thrown when there is an issue with the specified KMS key and the trail can’t be updated.
--
--
_KMSException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSException = _MatchServiceError cloudTrail "KmsException"


-- | This exception is thrown when the specified S3 bucket does not exist.
--
--
_S3BucketDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_S3BucketDoesNotExistException =
  _MatchServiceError cloudTrail "S3BucketDoesNotExistException"


-- | Invalid token or token that was previously used in a request with different parameters. This exception is thrown if the token is invalid.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError cloudTrail "InvalidNextTokenException"


-- | This exception is thrown when the key or value specified for the tag does not match the regular expression @^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-@]*)$@ .
--
--
_InvalidTagParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagParameterException =
  _MatchServiceError cloudTrail "InvalidTagParameterException"


-- | This exception is thrown when the requested operation is not permitted.
--
--
_OperationNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedException =
  _MatchServiceError cloudTrail "OperationNotPermittedException"


-- | Reserved for future use.
--
--
_InvalidTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTokenException = _MatchServiceError cloudTrail "InvalidTokenException"


-- | This exception is thrown if the limit specified is invalid.
--
--
_InvalidMaxResultsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMaxResultsException =
  _MatchServiceError cloudTrail "InvalidMaxResultsException"


-- | This exception is thrown when the specified trail already exists.
--
--
_TrailAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_TrailAlreadyExistsException =
  _MatchServiceError cloudTrail "TrailAlreadyExistsException"


-- | This exception is thrown when the provided S3 prefix is not valid.
--
--
_InvalidS3PrefixException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3PrefixException =
  _MatchServiceError cloudTrail "InvalidS3PrefixException"


-- | This exception is thrown when the specified resource is not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError cloudTrail "ResourceNotFoundException"


-- | This exception is thrown when the combination of parameters provided is not valid.
--
--
_InvalidParameterCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException =
  _MatchServiceError cloudTrail "InvalidParameterCombinationException"


-- | This exception is thrown when the KMS key ARN is invalid.
--
--
_InvalidKMSKeyIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidKMSKeyIdException =
  _MatchServiceError cloudTrail "InvalidKmsKeyIdException"


-- | This exception is thrown when an operation is called on a trail from a region other than the region in which the trail was created.
--
--
_InvalidHomeRegionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidHomeRegionException =
  _MatchServiceError cloudTrail "InvalidHomeRegionException"

