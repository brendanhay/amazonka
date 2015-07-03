{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudTrail.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , eveUsername
    , eveEventTime
    , eveResources
    , eveCloudTrailEvent
    , eveEventName
    , eveEventId

    -- * LookupAttribute
    , LookupAttribute
    , lookupAttribute
    , laAttributeKey
    , laAttributeValue

    -- * Resource
    , Resource
    , resource
    , resResourceType
    , resResourceName

    -- * Trail
    , Trail
    , trail
    , traS3KeyPrefix
    , traSNSTopicName
    , traCloudWatchLogsLogGroupARN
    , traName
    , traIncludeGlobalServiceEvents
    , traCloudWatchLogsRoleARN
    , traS3BucketName
    ) where

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
            , _svcTimeout = Just 70000000
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

data LookupAttributeKey
    = ResourceType
    | ResourceName
    | Username
    | EventName
    | EventId
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText LookupAttributeKey where
    parser = takeLowerText >>= \case
        "eventid" -> pure EventId
        "eventname" -> pure EventName
        "resourcename" -> pure ResourceName
        "resourcetype" -> pure ResourceType
        "username" -> pure Username
        e -> fromTextError $ "Failure parsing LookupAttributeKey from value: '" <> e
           <> "'. Accepted values: eventid, eventname, resourcename, resourcetype, username"

instance ToText LookupAttributeKey where
    toText = \case
        EventId -> "eventid"
        EventName -> "eventname"
        ResourceName -> "resourcename"
        ResourceType -> "resourcetype"
        Username -> "username"

instance Hashable LookupAttributeKey where
    hashWithSalt = hashUsing fromEnum

instance ToQuery LookupAttributeKey
instance ToHeader LookupAttributeKey

instance ToJSON LookupAttributeKey where
    toJSON = toJSONText

-- | Contains information about an event that was returned by a lookup
-- request. The result includes a representation of a CloudTrail event.
--
-- /See:/ 'event' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eveUsername'
--
-- * 'eveEventTime'
--
-- * 'eveResources'
--
-- * 'eveCloudTrailEvent'
--
-- * 'eveEventName'
--
-- * 'eveEventId'
data Event = Event'
    { _eveUsername        :: !(Maybe Text)
    , _eveEventTime       :: !(Maybe POSIX)
    , _eveResources       :: !(Maybe [Resource])
    , _eveCloudTrailEvent :: !(Maybe Text)
    , _eveEventName       :: !(Maybe Text)
    , _eveEventId         :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Event' smart constructor.
event :: Event
event =
    Event'
    { _eveUsername = Nothing
    , _eveEventTime = Nothing
    , _eveResources = Nothing
    , _eveCloudTrailEvent = Nothing
    , _eveEventName = Nothing
    , _eveEventId = Nothing
    }

-- | A user name or role name of the requester that called the API in the
-- event returned.
eveUsername :: Lens' Event (Maybe Text)
eveUsername = lens _eveUsername (\ s a -> s{_eveUsername = a});

-- | The date and time of the event returned.
eveEventTime :: Lens' Event (Maybe UTCTime)
eveEventTime = lens _eveEventTime (\ s a -> s{_eveEventTime = a}) . mapping _Time;

-- | A list of resources referenced by the event returned.
eveResources :: Lens' Event [Resource]
eveResources = lens _eveResources (\ s a -> s{_eveResources = a}) . _Default;

-- | A JSON string that contains a representation of the event returned.
eveCloudTrailEvent :: Lens' Event (Maybe Text)
eveCloudTrailEvent = lens _eveCloudTrailEvent (\ s a -> s{_eveCloudTrailEvent = a});

-- | The name of the event returned.
eveEventName :: Lens' Event (Maybe Text)
eveEventName = lens _eveEventName (\ s a -> s{_eveEventName = a});

-- | The CloudTrail ID of the event returned.
eveEventId :: Lens' Event (Maybe Text)
eveEventId = lens _eveEventId (\ s a -> s{_eveEventId = a});

instance FromJSON Event where
        parseJSON
          = withObject "Event"
              (\ x ->
                 Event' <$>
                   (x .:? "Username") <*> (x .:? "EventTime") <*>
                     (x .:? "Resources" .!= mempty)
                     <*> (x .:? "CloudTrailEvent")
                     <*> (x .:? "EventName")
                     <*> (x .:? "EventId"))

-- | Specifies an attribute and value that filter the events returned.
--
-- /See:/ 'lookupAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laAttributeKey'
--
-- * 'laAttributeValue'
data LookupAttribute = LookupAttribute'
    { _laAttributeKey   :: !LookupAttributeKey
    , _laAttributeValue :: !Text
    } deriving (Eq,Read,Show)

-- | 'LookupAttribute' smart constructor.
lookupAttribute :: LookupAttributeKey -> Text -> LookupAttribute
lookupAttribute pAttributeKey pAttributeValue =
    LookupAttribute'
    { _laAttributeKey = pAttributeKey
    , _laAttributeValue = pAttributeValue
    }

-- | Specifies an attribute on which to filter the events returned.
laAttributeKey :: Lens' LookupAttribute LookupAttributeKey
laAttributeKey = lens _laAttributeKey (\ s a -> s{_laAttributeKey = a});

-- | Specifies a value for the specified AttributeKey.
laAttributeValue :: Lens' LookupAttribute Text
laAttributeValue = lens _laAttributeValue (\ s a -> s{_laAttributeValue = a});

instance ToJSON LookupAttribute where
        toJSON LookupAttribute'{..}
          = object
              ["AttributeKey" .= _laAttributeKey,
               "AttributeValue" .= _laAttributeValue]

-- | Specifies the type and name of a resource referenced by an event.
--
-- /See:/ 'resource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'resResourceType'
--
-- * 'resResourceName'
data Resource = Resource'
    { _resResourceType :: !(Maybe Text)
    , _resResourceName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Resource' smart constructor.
resource :: Resource
resource =
    Resource'
    { _resResourceType = Nothing
    , _resResourceName = Nothing
    }

-- | The type of a resource referenced by the event returned. When the
-- resource type cannot be determined, null is returned. Some examples of
-- resource types are: __Instance__ for EC2, __Trail__ for CloudTrail,
-- __DBInstance__ for RDS, and __AccessKey__ for IAM. For a list of
-- resource types supported for event lookup, see
-- <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/lookup_supported_resourcetypes.html Resource Types Supported for Event Lookup>.
resResourceType :: Lens' Resource (Maybe Text)
resResourceType = lens _resResourceType (\ s a -> s{_resResourceType = a});

-- | The name of the resource referenced by the event returned. These are
-- user-created names whose values will depend on the environment. For
-- example, the resource name might be \"auto-scaling-test-group\" for an
-- Auto Scaling Group or \"i-1234567\" for an EC2 Instance.
resResourceName :: Lens' Resource (Maybe Text)
resResourceName = lens _resResourceName (\ s a -> s{_resResourceName = a});

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x ->
                 Resource' <$>
                   (x .:? "ResourceType") <*> (x .:? "ResourceName"))

-- | The settings for a trail.
--
-- /See:/ 'trail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'traS3KeyPrefix'
--
-- * 'traSNSTopicName'
--
-- * 'traCloudWatchLogsLogGroupARN'
--
-- * 'traName'
--
-- * 'traIncludeGlobalServiceEvents'
--
-- * 'traCloudWatchLogsRoleARN'
--
-- * 'traS3BucketName'
data Trail = Trail'
    { _traS3KeyPrefix                :: !(Maybe Text)
    , _traSNSTopicName               :: !(Maybe Text)
    , _traCloudWatchLogsLogGroupARN  :: !(Maybe Text)
    , _traName                       :: !(Maybe Text)
    , _traIncludeGlobalServiceEvents :: !(Maybe Bool)
    , _traCloudWatchLogsRoleARN      :: !(Maybe Text)
    , _traS3BucketName               :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Trail' smart constructor.
trail :: Trail
trail =
    Trail'
    { _traS3KeyPrefix = Nothing
    , _traSNSTopicName = Nothing
    , _traCloudWatchLogsLogGroupARN = Nothing
    , _traName = Nothing
    , _traIncludeGlobalServiceEvents = Nothing
    , _traCloudWatchLogsRoleARN = Nothing
    , _traS3BucketName = Nothing
    }

-- | Value of the Amazon S3 prefix.
traS3KeyPrefix :: Lens' Trail (Maybe Text)
traS3KeyPrefix = lens _traS3KeyPrefix (\ s a -> s{_traS3KeyPrefix = a});

-- | Name of the existing Amazon SNS topic that CloudTrail uses to notify the
-- account owner when new CloudTrail log files have been delivered.
traSNSTopicName :: Lens' Trail (Maybe Text)
traSNSTopicName = lens _traSNSTopicName (\ s a -> s{_traSNSTopicName = a});

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that
-- represents the log group to which CloudTrail logs will be delivered.
traCloudWatchLogsLogGroupARN :: Lens' Trail (Maybe Text)
traCloudWatchLogsLogGroupARN = lens _traCloudWatchLogsLogGroupARN (\ s a -> s{_traCloudWatchLogsLogGroupARN = a});

-- | Name of the trail set by calling CreateTrail.
traName :: Lens' Trail (Maybe Text)
traName = lens _traName (\ s a -> s{_traName = a});

-- | Set to __True__ to include AWS API calls from AWS global services such
-- as IAM. Otherwise, __False__.
traIncludeGlobalServiceEvents :: Lens' Trail (Maybe Bool)
traIncludeGlobalServiceEvents = lens _traIncludeGlobalServiceEvents (\ s a -> s{_traIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user’s log group.
traCloudWatchLogsRoleARN :: Lens' Trail (Maybe Text)
traCloudWatchLogsRoleARN = lens _traCloudWatchLogsRoleARN (\ s a -> s{_traCloudWatchLogsRoleARN = a});

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files.
traS3BucketName :: Lens' Trail (Maybe Text)
traS3BucketName = lens _traS3BucketName (\ s a -> s{_traS3BucketName = a});

instance FromJSON Trail where
        parseJSON
          = withObject "Trail"
              (\ x ->
                 Trail' <$>
                   (x .:? "S3KeyPrefix") <*> (x .:? "SnsTopicName") <*>
                     (x .:? "CloudWatchLogsLogGroupArn")
                     <*> (x .:? "Name")
                     <*> (x .:? "IncludeGlobalServiceEvents")
                     <*> (x .:? "CloudWatchLogsRoleArn")
                     <*> (x .:? "S3BucketName"))
