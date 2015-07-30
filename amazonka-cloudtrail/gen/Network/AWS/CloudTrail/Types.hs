{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types
    (
    -- * Service
      CloudTrail

    -- * Errors
    , _InvalidTimeRangeException
    , _InsufficientS3BucketPolicyException
    , _MaximumNumberOfTrailsExceededException
    , _InsufficientSNSTopicPolicyException
    , _InvalidCloudWatchLogsRoleARNException
    , _InvalidTrailNameException
    , _InvalidLookupAttributesException
    , _TrailNotFoundException
    , _CloudWatchLogsDeliveryUnavailableException
    , _InvalidSNSTopicNameException
    , _InvalidCloudWatchLogsLogGroupARNException
    , _InvalidS3BucketNameException
    , _InvalidNextTokenException
    , _S3BucketDoesNotExistException
    , _InvalidMaxResultsException
    , _TrailAlreadyExistsException
    , _InvalidS3PrefixException

    -- * LookupAttributeKey
    , LookupAttributeKey (..)

    -- * Event
    , Event
    , event
    , eUsername
    , eEventTime
    , eResources
    , eCloudTrailEvent
    , eEventName
    , eEventId

    -- * LookupAttribute
    , LookupAttribute
    , lookupAttribute
    , laAttributeKey
    , laAttributeValue

    -- * Resource
    , Resource
    , resource
    , rResourceType
    , rResourceName

    -- * Trail
    , Trail
    , trail
    , tS3KeyPrefix
    , tSNSTopicName
    , tCloudWatchLogsLogGroupARN
    , tName
    , tIncludeGlobalServiceEvents
    , tCloudWatchLogsRoleARN
    , tS3BucketName
    ) where

import           Network.AWS.CloudTrail.Types.Product
import           Network.AWS.CloudTrail.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2013-11-01@ of the Amazon CloudTrail SDK.
data CloudTrail

instance AWSService CloudTrail where
    type Sg CloudTrail = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CloudTrail"
            , _svcPrefix = "cloudtrail"
            , _svcVersion = "2013-11-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Occurs if the timestamp values are invalid. Either the start time occurs
-- after the end time or the time range is outside the range of possible
-- values.
_InvalidTimeRangeException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidTimeRangeException =
    _ServiceError . hasStatus 400 . hasCode "InvalidTimeRange"

-- | This exception is thrown when the policy on the S3 bucket is not
-- sufficient.
_InsufficientS3BucketPolicyException :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientS3BucketPolicyException =
    _ServiceError . hasStatus 403 . hasCode "InsufficientS3BucketPolicy"

-- | This exception is thrown when the maximum number of trails is reached.
_MaximumNumberOfTrailsExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_MaximumNumberOfTrailsExceededException =
    _ServiceError . hasStatus 403 . hasCode "MaximumNumberOfTrailsExceeded"

-- | This exception is thrown when the policy on the SNS topic is not
-- sufficient.
_InsufficientSNSTopicPolicyException :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientSNSTopicPolicyException =
    _ServiceError . hasStatus 403 . hasCode "InsufficientSnsTopicPolicy"

-- | This exception is thrown when the provided role is not valid.
_InvalidCloudWatchLogsRoleARNException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCloudWatchLogsRoleARNException =
    _ServiceError . hasStatus 400 . hasCode "InvalidCloudWatchLogsRoleArn"

-- | This exception is thrown when the provided trail name is not valid.
_InvalidTrailNameException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidTrailNameException =
    _ServiceError . hasStatus 400 . hasCode "InvalidTrailName"

-- | Occurs when an invalid lookup attribute is specified.
_InvalidLookupAttributesException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidLookupAttributesException =
    _ServiceError . hasStatus 400 . hasCode "InvalidLookupAttributes"

-- | This exception is thrown when the trail with the given name is not
-- found.
_TrailNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_TrailNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "TrailNotFound"

-- | Cannot set a CloudWatch Logs delivery for this region.
_CloudWatchLogsDeliveryUnavailableException :: AWSError a => Getting (First ServiceError) a ServiceError
_CloudWatchLogsDeliveryUnavailableException =
    _ServiceError . hasStatus 400 . hasCode "CloudWatchLogsDeliveryUnavailable"

-- | This exception is thrown when the provided SNS topic name is not valid.
_InvalidSNSTopicNameException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicNameException =
    _ServiceError . hasStatus 400 . hasCode "InvalidSnsTopicName"

-- | This exception is thrown when the provided CloudWatch log group is not
-- valid.
_InvalidCloudWatchLogsLogGroupARNException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCloudWatchLogsLogGroupARNException =
    _ServiceError . hasStatus 400 . hasCode "InvalidCloudWatchLogsLogGroupArn"

-- | This exception is thrown when the provided S3 bucket name is not valid.
_InvalidS3BucketNameException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidS3BucketNameException =
    _ServiceError . hasStatus 400 . hasCode "InvalidS3BucketName"

-- | Invalid token or token that was previously used in a request with
-- different parameters. This exception is thrown if the token is invalid.
_InvalidNextTokenException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
    _ServiceError . hasStatus 400 . hasCode "InvalidNextToken"

-- | This exception is thrown when the specified S3 bucket does not exist.
_S3BucketDoesNotExistException :: AWSError a => Getting (First ServiceError) a ServiceError
_S3BucketDoesNotExistException =
    _ServiceError . hasStatus 404 . hasCode "S3BucketDoesNotExist"

-- | This exception is thrown if the limit specified is invalid.
_InvalidMaxResultsException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidMaxResultsException =
    _ServiceError . hasStatus 400 . hasCode "InvalidMaxResults"

-- | This exception is thrown when the specified trail already exists.
_TrailAlreadyExistsException :: AWSError a => Getting (First ServiceError) a ServiceError
_TrailAlreadyExistsException =
    _ServiceError . hasStatus 400 . hasCode "TrailAlreadyExists"

-- | This exception is thrown when the provided S3 prefix is not valid.
_InvalidS3PrefixException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidS3PrefixException =
    _ServiceError . hasStatus 400 . hasCode "InvalidS3Prefix"
