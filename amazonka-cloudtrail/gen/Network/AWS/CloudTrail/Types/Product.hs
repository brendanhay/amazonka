{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.Product where

import           Network.AWS.CloudTrail.Types.Sum
import           Network.AWS.Prelude

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
