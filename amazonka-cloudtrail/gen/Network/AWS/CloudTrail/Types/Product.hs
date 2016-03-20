{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

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
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Contains information about an event that was returned by a lookup
-- request. The result includes a representation of a CloudTrail event.
--
-- /See:/ 'event' smart constructor.
data Event = Event'
    { _eUsername        :: !(Maybe Text)
    , _eResources       :: !(Maybe [Resource])
    , _eEventTime       :: !(Maybe POSIX)
    , _eCloudTrailEvent :: !(Maybe Text)
    , _eEventName       :: !(Maybe Text)
    , _eEventId         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eUsername'
--
-- * 'eResources'
--
-- * 'eEventTime'
--
-- * 'eCloudTrailEvent'
--
-- * 'eEventName'
--
-- * 'eEventId'
event
    :: Event
event =
    Event'
    { _eUsername = Nothing
    , _eResources = Nothing
    , _eEventTime = Nothing
    , _eCloudTrailEvent = Nothing
    , _eEventName = Nothing
    , _eEventId = Nothing
    }

-- | A user name or role name of the requester that called the API in the
-- event returned.
eUsername :: Lens' Event (Maybe Text)
eUsername = lens _eUsername (\ s a -> s{_eUsername = a});

-- | A list of resources referenced by the event returned.
eResources :: Lens' Event [Resource]
eResources = lens _eResources (\ s a -> s{_eResources = a}) . _Default . _Coerce;

-- | The date and time of the event returned.
eEventTime :: Lens' Event (Maybe UTCTime)
eEventTime = lens _eEventTime (\ s a -> s{_eEventTime = a}) . mapping _Time;

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
                   (x .:? "Username") <*> (x .:? "Resources" .!= mempty)
                     <*> (x .:? "EventTime")
                     <*> (x .:? "CloudTrailEvent")
                     <*> (x .:? "EventName")
                     <*> (x .:? "EventId"))

instance Hashable Event

-- | Specifies an attribute and value that filter the events returned.
--
-- /See:/ 'lookupAttribute' smart constructor.
data LookupAttribute = LookupAttribute'
    { _laAttributeKey   :: !LookupAttributeKey
    , _laAttributeValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LookupAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laAttributeKey'
--
-- * 'laAttributeValue'
lookupAttribute
    :: LookupAttributeKey -- ^ 'laAttributeKey'
    -> Text -- ^ 'laAttributeValue'
    -> LookupAttribute
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

instance Hashable LookupAttribute

instance ToJSON LookupAttribute where
        toJSON LookupAttribute'{..}
          = object
              (catMaybes
                 [Just ("AttributeKey" .= _laAttributeKey),
                  Just ("AttributeValue" .= _laAttributeValue)])

-- | Contains information about a returned public key.
--
-- /See:/ 'publicKey' smart constructor.
data PublicKey = PublicKey'
    { _pkFingerprint       :: !(Maybe Text)
    , _pkValidityEndTime   :: !(Maybe POSIX)
    , _pkValue             :: !(Maybe Base64)
    , _pkValidityStartTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pkFingerprint'
--
-- * 'pkValidityEndTime'
--
-- * 'pkValue'
--
-- * 'pkValidityStartTime'
publicKey
    :: PublicKey
publicKey =
    PublicKey'
    { _pkFingerprint = Nothing
    , _pkValidityEndTime = Nothing
    , _pkValue = Nothing
    , _pkValidityStartTime = Nothing
    }

-- | The fingerprint of the public key.
pkFingerprint :: Lens' PublicKey (Maybe Text)
pkFingerprint = lens _pkFingerprint (\ s a -> s{_pkFingerprint = a});

-- | The ending time of validity of the public key.
pkValidityEndTime :: Lens' PublicKey (Maybe UTCTime)
pkValidityEndTime = lens _pkValidityEndTime (\ s a -> s{_pkValidityEndTime = a}) . mapping _Time;

-- | The DER encoded public key value in PKCS#1 format.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
pkValue :: Lens' PublicKey (Maybe ByteString)
pkValue = lens _pkValue (\ s a -> s{_pkValue = a}) . mapping _Base64;

-- | The starting time of validity of the public key.
pkValidityStartTime :: Lens' PublicKey (Maybe UTCTime)
pkValidityStartTime = lens _pkValidityStartTime (\ s a -> s{_pkValidityStartTime = a}) . mapping _Time;

instance FromJSON PublicKey where
        parseJSON
          = withObject "PublicKey"
              (\ x ->
                 PublicKey' <$>
                   (x .:? "Fingerprint") <*> (x .:? "ValidityEndTime")
                     <*> (x .:? "Value")
                     <*> (x .:? "ValidityStartTime"))

instance Hashable PublicKey

-- | Specifies the type and name of a resource referenced by an event.
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
    { _rResourceType :: !(Maybe Text)
    , _rResourceName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceType'
--
-- * 'rResourceName'
resource
    :: Resource
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

instance Hashable Resource

-- | A resource tag.
--
-- /See:/ 'resourceTag' smart constructor.
data ResourceTag = ResourceTag'
    { _rResourceId :: !(Maybe Text)
    , _rTagsList   :: !(Maybe [Tag])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceId'
--
-- * 'rTagsList'
resourceTag
    :: ResourceTag
resourceTag =
    ResourceTag'
    { _rResourceId = Nothing
    , _rTagsList = Nothing
    }

-- | Specifies the ARN of the resource.
rResourceId :: Lens' ResourceTag (Maybe Text)
rResourceId = lens _rResourceId (\ s a -> s{_rResourceId = a});

-- | Undocumented member.
rTagsList :: Lens' ResourceTag [Tag]
rTagsList = lens _rTagsList (\ s a -> s{_rTagsList = a}) . _Default . _Coerce;

instance FromJSON ResourceTag where
        parseJSON
          = withObject "ResourceTag"
              (\ x ->
                 ResourceTag' <$>
                   (x .:? "ResourceId") <*>
                     (x .:? "TagsList" .!= mempty))

instance Hashable ResourceTag

-- | A custom key-value pair associated with a resource such as a CloudTrail
-- trail.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue'
--
-- * 'tagKey'
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ =
    Tag'
    { _tagValue = Nothing
    , _tagKey = pKey_
    }

-- | The value in a key-value pair of a tag. The value must be no longer than
-- 256 Unicode characters.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key in a key-value pair. The key must be must be no longer than 128
-- Unicode characters. The key must be unique for the resource to which it
-- applies.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .: "Key"))

instance Hashable Tag

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue,
                  Just ("Key" .= _tagKey)])

-- | The settings for a trail.
--
-- /See:/ 'trail' smart constructor.
data Trail = Trail'
    { _tLogFileValidationEnabled   :: !(Maybe Bool)
    , _tTrailARN                   :: !(Maybe Text)
    , _tS3KeyPrefix                :: !(Maybe Text)
    , _tSNSTopicName               :: !(Maybe Text)
    , _tCloudWatchLogsLogGroupARN  :: !(Maybe Text)
    , _tKMSKeyId                   :: !(Maybe Text)
    , _tHomeRegion                 :: !(Maybe Text)
    , _tName                       :: !(Maybe Text)
    , _tIncludeGlobalServiceEvents :: !(Maybe Bool)
    , _tCloudWatchLogsRoleARN      :: !(Maybe Text)
    , _tS3BucketName               :: !(Maybe Text)
    , _tIsMultiRegionTrail         :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Trail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tLogFileValidationEnabled'
--
-- * 'tTrailARN'
--
-- * 'tS3KeyPrefix'
--
-- * 'tSNSTopicName'
--
-- * 'tCloudWatchLogsLogGroupARN'
--
-- * 'tKMSKeyId'
--
-- * 'tHomeRegion'
--
-- * 'tName'
--
-- * 'tIncludeGlobalServiceEvents'
--
-- * 'tCloudWatchLogsRoleARN'
--
-- * 'tS3BucketName'
--
-- * 'tIsMultiRegionTrail'
trail
    :: Trail
trail =
    Trail'
    { _tLogFileValidationEnabled = Nothing
    , _tTrailARN = Nothing
    , _tS3KeyPrefix = Nothing
    , _tSNSTopicName = Nothing
    , _tCloudWatchLogsLogGroupARN = Nothing
    , _tKMSKeyId = Nothing
    , _tHomeRegion = Nothing
    , _tName = Nothing
    , _tIncludeGlobalServiceEvents = Nothing
    , _tCloudWatchLogsRoleARN = Nothing
    , _tS3BucketName = Nothing
    , _tIsMultiRegionTrail = Nothing
    }

-- | Specifies whether log file validation is enabled.
tLogFileValidationEnabled :: Lens' Trail (Maybe Bool)
tLogFileValidationEnabled = lens _tLogFileValidationEnabled (\ s a -> s{_tLogFileValidationEnabled = a});

-- | The Amazon Resource Name of the trail. The 'TrailARN' format is
-- 'arn:aws:cloudtrail:us-east-1:123456789012:trail\/MyTrail'.
tTrailARN :: Lens' Trail (Maybe Text)
tTrailARN = lens _tTrailARN (\ s a -> s{_tTrailARN = a});

-- | Specifies the Amazon S3 key prefix that comes after the name of the
-- bucket you have designated for log file delivery. For more information,
-- see
-- <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files>.The
-- maximum length is 200 characters.
tS3KeyPrefix :: Lens' Trail (Maybe Text)
tS3KeyPrefix = lens _tS3KeyPrefix (\ s a -> s{_tS3KeyPrefix = a});

-- | Name of the existing Amazon SNS topic that CloudTrail uses to notify the
-- account owner when new CloudTrail log files have been delivered. The
-- maximum length is 256 characters.
tSNSTopicName :: Lens' Trail (Maybe Text)
tSNSTopicName = lens _tSNSTopicName (\ s a -> s{_tSNSTopicName = a});

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that
-- represents the log group to which CloudTrail logs will be delivered.
tCloudWatchLogsLogGroupARN :: Lens' Trail (Maybe Text)
tCloudWatchLogsLogGroupARN = lens _tCloudWatchLogsLogGroupARN (\ s a -> s{_tCloudWatchLogsLogGroupARN = a});

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail.
-- The value is a fully specified ARN to a KMS key in the format:
--
-- 'arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012'
tKMSKeyId :: Lens' Trail (Maybe Text)
tKMSKeyId = lens _tKMSKeyId (\ s a -> s{_tKMSKeyId = a});

-- | The region in which the trail was created.
tHomeRegion :: Lens' Trail (Maybe Text)
tHomeRegion = lens _tHomeRegion (\ s a -> s{_tHomeRegion = a});

-- | Name of the trail set by calling < CreateTrail>. The maximum length is
-- 128 characters.
tName :: Lens' Trail (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a});

-- | Set to __True__ to include AWS API calls from AWS global services such
-- as IAM. Otherwise, __False__.
tIncludeGlobalServiceEvents :: Lens' Trail (Maybe Bool)
tIncludeGlobalServiceEvents = lens _tIncludeGlobalServiceEvents (\ s a -> s{_tIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user\'s log group.
tCloudWatchLogsRoleARN :: Lens' Trail (Maybe Text)
tCloudWatchLogsRoleARN = lens _tCloudWatchLogsRoleARN (\ s a -> s{_tCloudWatchLogsRoleARN = a});

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files. See
-- <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements>.
tS3BucketName :: Lens' Trail (Maybe Text)
tS3BucketName = lens _tS3BucketName (\ s a -> s{_tS3BucketName = a});

-- | Specifies whether the trail belongs only to one region or exists in all
-- regions.
tIsMultiRegionTrail :: Lens' Trail (Maybe Bool)
tIsMultiRegionTrail = lens _tIsMultiRegionTrail (\ s a -> s{_tIsMultiRegionTrail = a});

instance FromJSON Trail where
        parseJSON
          = withObject "Trail"
              (\ x ->
                 Trail' <$>
                   (x .:? "LogFileValidationEnabled") <*>
                     (x .:? "TrailARN")
                     <*> (x .:? "S3KeyPrefix")
                     <*> (x .:? "SnsTopicName")
                     <*> (x .:? "CloudWatchLogsLogGroupArn")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "HomeRegion")
                     <*> (x .:? "Name")
                     <*> (x .:? "IncludeGlobalServiceEvents")
                     <*> (x .:? "CloudWatchLogsRoleArn")
                     <*> (x .:? "S3BucketName")
                     <*> (x .:? "IsMultiRegionTrail"))

instance Hashable Trail
