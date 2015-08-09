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
-- Stability   : auto-generated
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
-- * 'eUsername'
--
-- * 'eEventTime'
--
-- * 'eResources'
--
-- * 'eCloudTrailEvent'
--
-- * 'eEventName'
--
-- * 'eEventId'
data Event = Event'
    { _eUsername        :: !(Maybe Text)
    , _eEventTime       :: !(Maybe POSIX)
    , _eResources       :: !(Maybe [Resource])
    , _eCloudTrailEvent :: !(Maybe Text)
    , _eEventName       :: !(Maybe Text)
    , _eEventId         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Event' smart constructor.
event :: Event
event =
    Event'
    { _eUsername = Nothing
    , _eEventTime = Nothing
    , _eResources = Nothing
    , _eCloudTrailEvent = Nothing
    , _eEventName = Nothing
    , _eEventId = Nothing
    }

-- | A user name or role name of the requester that called the API in the
-- event returned.
eUsername :: Lens' Event (Maybe Text)
eUsername = lens _eUsername (\ s a -> s{_eUsername = a});

-- | The date and time of the event returned.
eEventTime :: Lens' Event (Maybe UTCTime)
eEventTime = lens _eEventTime (\ s a -> s{_eEventTime = a}) . mapping _Time;

-- | A list of resources referenced by the event returned.
eResources :: Lens' Event [Resource]
eResources = lens _eResources (\ s a -> s{_eResources = a}) . _Default . _Coerce;

-- | A JSON string that contains a representation of the event returned.
eCloudTrailEvent :: Lens' Event (Maybe Text)
eCloudTrailEvent = lens _eCloudTrailEvent (\ s a -> s{_eCloudTrailEvent = a});

-- | The name of the event returned.
eEventName :: Lens' Event (Maybe Text)
eEventName = lens _eEventName (\ s a -> s{_eEventName = a});

-- | The CloudTrail ID of the event returned.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\ s a -> s{_eEventId = a});

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
lookupAttribute pAttributeKey_ pAttributeValue_ =
    LookupAttribute'
    { _laAttributeKey = pAttributeKey_
    , _laAttributeValue = pAttributeValue_
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
-- * 'rResourceType'
--
-- * 'rResourceName'
data Resource = Resource'
    { _rResourceType :: !(Maybe Text)
    , _rResourceName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Resource' smart constructor.
resource :: Resource
resource =
    Resource'
    { _rResourceType = Nothing
    , _rResourceName = Nothing
    }

-- | The type of a resource referenced by the event returned. When the
-- resource type cannot be determined, null is returned. Some examples of
-- resource types are: __Instance__ for EC2, __Trail__ for CloudTrail,
-- __DBInstance__ for RDS, and __AccessKey__ for IAM. For a list of
-- resource types supported for event lookup, see
-- <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/lookup_supported_resourcetypes.html Resource Types Supported for Event Lookup>.
rResourceType :: Lens' Resource (Maybe Text)
rResourceType = lens _rResourceType (\ s a -> s{_rResourceType = a});

-- | The name of the resource referenced by the event returned. These are
-- user-created names whose values will depend on the environment. For
-- example, the resource name might be \"auto-scaling-test-group\" for an
-- Auto Scaling Group or \"i-1234567\" for an EC2 Instance.
rResourceName :: Lens' Resource (Maybe Text)
rResourceName = lens _rResourceName (\ s a -> s{_rResourceName = a});

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
-- * 'tS3KeyPrefix'
--
-- * 'tSNSTopicName'
--
-- * 'tCloudWatchLogsLogGroupARN'
--
-- * 'tName'
--
-- * 'tIncludeGlobalServiceEvents'
--
-- * 'tCloudWatchLogsRoleARN'
--
-- * 'tS3BucketName'
data Trail = Trail'
    { _tS3KeyPrefix                :: !(Maybe Text)
    , _tSNSTopicName               :: !(Maybe Text)
    , _tCloudWatchLogsLogGroupARN  :: !(Maybe Text)
    , _tName                       :: !(Maybe Text)
    , _tIncludeGlobalServiceEvents :: !(Maybe Bool)
    , _tCloudWatchLogsRoleARN      :: !(Maybe Text)
    , _tS3BucketName               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Trail' smart constructor.
trail :: Trail
trail =
    Trail'
    { _tS3KeyPrefix = Nothing
    , _tSNSTopicName = Nothing
    , _tCloudWatchLogsLogGroupARN = Nothing
    , _tName = Nothing
    , _tIncludeGlobalServiceEvents = Nothing
    , _tCloudWatchLogsRoleARN = Nothing
    , _tS3BucketName = Nothing
    }

-- | Value of the Amazon S3 prefix.
tS3KeyPrefix :: Lens' Trail (Maybe Text)
tS3KeyPrefix = lens _tS3KeyPrefix (\ s a -> s{_tS3KeyPrefix = a});

-- | Name of the existing Amazon SNS topic that CloudTrail uses to notify the
-- account owner when new CloudTrail log files have been delivered.
tSNSTopicName :: Lens' Trail (Maybe Text)
tSNSTopicName = lens _tSNSTopicName (\ s a -> s{_tSNSTopicName = a});

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that
-- represents the log group to which CloudTrail logs will be delivered.
tCloudWatchLogsLogGroupARN :: Lens' Trail (Maybe Text)
tCloudWatchLogsLogGroupARN = lens _tCloudWatchLogsLogGroupARN (\ s a -> s{_tCloudWatchLogsLogGroupARN = a});

-- | Name of the trail set by calling CreateTrail.
tName :: Lens' Trail (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a});

-- | Set to __True__ to include AWS API calls from AWS global services such
-- as IAM. Otherwise, __False__.
tIncludeGlobalServiceEvents :: Lens' Trail (Maybe Bool)
tIncludeGlobalServiceEvents = lens _tIncludeGlobalServiceEvents (\ s a -> s{_tIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user’s log group.
tCloudWatchLogsRoleARN :: Lens' Trail (Maybe Text)
tCloudWatchLogsRoleARN = lens _tCloudWatchLogsRoleARN (\ s a -> s{_tCloudWatchLogsRoleARN = a});

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files.
tS3BucketName :: Lens' Trail (Maybe Text)
tS3BucketName = lens _tS3BucketName (\ s a -> s{_tS3BucketName = a});

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
