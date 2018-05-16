{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.Product where

import Network.AWS.CloudTrail.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon S3 objects that you specify in your event selectors for your trail to log data events. Data events are object-level API operations that access S3 objects, such as @GetObject@ , @DeleteObject@ , and @PutObject@ . You can specify up to 250 S3 buckets and object prefixes for a trail.
--
--
-- Example
--
--     * You create an event selector for a trail and specify an S3 bucket and an empty prefix, such as @arn:aws:s3:::bucket-1/@ .
--
--     * You upload an image file to @bucket-1@ .
--
--     * The @PutObject@ API operation occurs on an object in the S3 bucket that you specified in the event selector. The trail processes and logs the event.
--
--     * You upload another image file to a different S3 bucket named @arn:aws:s3:::bucket-2@ .
--
--     * The event occurs on an object in an S3 bucket that you didn't specify in the event selector. The trail doesn’t log the event.
--
--
--
--
-- /See:/ 'dataResource' smart constructor.
data DataResource = DataResource'
  { _drValues :: !(Maybe [Text])
  , _drType   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DataResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drValues' - A list of ARN-like strings for the specified S3 objects. To log data events for all objects in an S3 bucket, specify the bucket and an empty object prefix such as @arn:aws:s3:::bucket-1/@ . The trail logs data events for all objects in this S3 bucket. To log data events for specific objects, specify the S3 bucket and object prefix such as @arn:aws:s3:::bucket-1/example-images@ . The trail logs data events for objects in this S3 bucket that match the prefix.
--
-- * 'drType' - The resource type in which you want to log data events. You can specify only the following value: @AWS::S3::Object@ .
dataResource
    :: DataResource
dataResource = DataResource' {_drValues = Nothing, _drType = Nothing}


-- | A list of ARN-like strings for the specified S3 objects. To log data events for all objects in an S3 bucket, specify the bucket and an empty object prefix such as @arn:aws:s3:::bucket-1/@ . The trail logs data events for all objects in this S3 bucket. To log data events for specific objects, specify the S3 bucket and object prefix such as @arn:aws:s3:::bucket-1/example-images@ . The trail logs data events for objects in this S3 bucket that match the prefix.
drValues :: Lens' DataResource [Text]
drValues = lens _drValues (\ s a -> s{_drValues = a}) . _Default . _Coerce

-- | The resource type in which you want to log data events. You can specify only the following value: @AWS::S3::Object@ .
drType :: Lens' DataResource (Maybe Text)
drType = lens _drType (\ s a -> s{_drType = a})

instance FromJSON DataResource where
        parseJSON
          = withObject "DataResource"
              (\ x ->
                 DataResource' <$>
                   (x .:? "Values" .!= mempty) <*> (x .:? "Type"))

instance Hashable DataResource where

instance NFData DataResource where

instance ToJSON DataResource where
        toJSON DataResource'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _drValues,
                  ("Type" .=) <$> _drType])

-- | Contains information about an event that was returned by a lookup request. The result includes a representation of a CloudTrail event.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eUsername        :: !(Maybe Text)
  , _eResources       :: !(Maybe [Resource])
  , _eEventTime       :: !(Maybe POSIX)
  , _eCloudTrailEvent :: !(Maybe Text)
  , _eEventName       :: !(Maybe Text)
  , _eEventSource     :: !(Maybe Text)
  , _eEventId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eUsername' - A user name or role name of the requester that called the API in the event returned.
--
-- * 'eResources' - A list of resources referenced by the event returned.
--
-- * 'eEventTime' - The date and time of the event returned.
--
-- * 'eCloudTrailEvent' - A JSON string that contains a representation of the event returned.
--
-- * 'eEventName' - The name of the event returned.
--
-- * 'eEventSource' - The AWS service that the request was made to.
--
-- * 'eEventId' - The CloudTrail ID of the event returned.
event
    :: Event
event =
  Event'
    { _eUsername = Nothing
    , _eResources = Nothing
    , _eEventTime = Nothing
    , _eCloudTrailEvent = Nothing
    , _eEventName = Nothing
    , _eEventSource = Nothing
    , _eEventId = Nothing
    }


-- | A user name or role name of the requester that called the API in the event returned.
eUsername :: Lens' Event (Maybe Text)
eUsername = lens _eUsername (\ s a -> s{_eUsername = a})

-- | A list of resources referenced by the event returned.
eResources :: Lens' Event [Resource]
eResources = lens _eResources (\ s a -> s{_eResources = a}) . _Default . _Coerce

-- | The date and time of the event returned.
eEventTime :: Lens' Event (Maybe UTCTime)
eEventTime = lens _eEventTime (\ s a -> s{_eEventTime = a}) . mapping _Time

-- | A JSON string that contains a representation of the event returned.
eCloudTrailEvent :: Lens' Event (Maybe Text)
eCloudTrailEvent = lens _eCloudTrailEvent (\ s a -> s{_eCloudTrailEvent = a})

-- | The name of the event returned.
eEventName :: Lens' Event (Maybe Text)
eEventName = lens _eEventName (\ s a -> s{_eEventName = a})

-- | The AWS service that the request was made to.
eEventSource :: Lens' Event (Maybe Text)
eEventSource = lens _eEventSource (\ s a -> s{_eEventSource = a})

-- | The CloudTrail ID of the event returned.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\ s a -> s{_eEventId = a})

instance FromJSON Event where
        parseJSON
          = withObject "Event"
              (\ x ->
                 Event' <$>
                   (x .:? "Username") <*> (x .:? "Resources" .!= mempty)
                     <*> (x .:? "EventTime")
                     <*> (x .:? "CloudTrailEvent")
                     <*> (x .:? "EventName")
                     <*> (x .:? "EventSource")
                     <*> (x .:? "EventId"))

instance Hashable Event where

instance NFData Event where

-- | Use event selectors to specify whether you want your trail to log management and/or data events. When an event occurs in your account, CloudTrail evaluates the event selector for all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event.
--
--
-- You can configure up to five event selectors for a trail.
--
--
-- /See:/ 'eventSelector' smart constructor.
data EventSelector = EventSelector'
  { _esDataResources           :: !(Maybe [DataResource])
  , _esReadWriteType           :: !(Maybe ReadWriteType)
  , _esIncludeManagementEvents :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esDataResources' - CloudTrail supports logging only data events for S3 objects. You can specify up to 250 S3 buckets and object prefixes for a trail. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-data-events Data Events> in the /AWS CloudTrail User Guide/ .
--
-- * 'esReadWriteType' - Specify if you want your trail to log read-only events, write-only events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only API operation and @RunInstances@ is a write-only API operation. By default, the value is @All@ .
--
-- * 'esIncludeManagementEvents' - Specify if you want your event selector to include management events for your trail. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-management-events Management Events> in the /AWS CloudTrail User Guide/ . By default, the value is @true@ .
eventSelector
    :: EventSelector
eventSelector =
  EventSelector'
    { _esDataResources = Nothing
    , _esReadWriteType = Nothing
    , _esIncludeManagementEvents = Nothing
    }


-- | CloudTrail supports logging only data events for S3 objects. You can specify up to 250 S3 buckets and object prefixes for a trail. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-data-events Data Events> in the /AWS CloudTrail User Guide/ .
esDataResources :: Lens' EventSelector [DataResource]
esDataResources = lens _esDataResources (\ s a -> s{_esDataResources = a}) . _Default . _Coerce

-- | Specify if you want your trail to log read-only events, write-only events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only API operation and @RunInstances@ is a write-only API operation. By default, the value is @All@ .
esReadWriteType :: Lens' EventSelector (Maybe ReadWriteType)
esReadWriteType = lens _esReadWriteType (\ s a -> s{_esReadWriteType = a})

-- | Specify if you want your event selector to include management events for your trail. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-management-events Management Events> in the /AWS CloudTrail User Guide/ . By default, the value is @true@ .
esIncludeManagementEvents :: Lens' EventSelector (Maybe Bool)
esIncludeManagementEvents = lens _esIncludeManagementEvents (\ s a -> s{_esIncludeManagementEvents = a})

instance FromJSON EventSelector where
        parseJSON
          = withObject "EventSelector"
              (\ x ->
                 EventSelector' <$>
                   (x .:? "DataResources" .!= mempty) <*>
                     (x .:? "ReadWriteType")
                     <*> (x .:? "IncludeManagementEvents"))

instance Hashable EventSelector where

instance NFData EventSelector where

instance ToJSON EventSelector where
        toJSON EventSelector'{..}
          = object
              (catMaybes
                 [("DataResources" .=) <$> _esDataResources,
                  ("ReadWriteType" .=) <$> _esReadWriteType,
                  ("IncludeManagementEvents" .=) <$>
                    _esIncludeManagementEvents])

-- | Specifies an attribute and value that filter the events returned.
--
--
--
-- /See:/ 'lookupAttribute' smart constructor.
data LookupAttribute = LookupAttribute'
  { _laAttributeKey   :: !LookupAttributeKey
  , _laAttributeValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LookupAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laAttributeKey' - Specifies an attribute on which to filter the events returned.
--
-- * 'laAttributeValue' - Specifies a value for the specified AttributeKey.
lookupAttribute
    :: LookupAttributeKey -- ^ 'laAttributeKey'
    -> Text -- ^ 'laAttributeValue'
    -> LookupAttribute
lookupAttribute pAttributeKey_ pAttributeValue_ =
  LookupAttribute'
    {_laAttributeKey = pAttributeKey_, _laAttributeValue = pAttributeValue_}


-- | Specifies an attribute on which to filter the events returned.
laAttributeKey :: Lens' LookupAttribute LookupAttributeKey
laAttributeKey = lens _laAttributeKey (\ s a -> s{_laAttributeKey = a})

-- | Specifies a value for the specified AttributeKey.
laAttributeValue :: Lens' LookupAttribute Text
laAttributeValue = lens _laAttributeValue (\ s a -> s{_laAttributeValue = a})

instance Hashable LookupAttribute where

instance NFData LookupAttribute where

instance ToJSON LookupAttribute where
        toJSON LookupAttribute'{..}
          = object
              (catMaybes
                 [Just ("AttributeKey" .= _laAttributeKey),
                  Just ("AttributeValue" .= _laAttributeValue)])

-- | Contains information about a returned public key.
--
--
--
-- /See:/ 'publicKey' smart constructor.
data PublicKey = PublicKey'
  { _pkFingerprint       :: !(Maybe Text)
  , _pkValidityEndTime   :: !(Maybe POSIX)
  , _pkValue             :: !(Maybe Base64)
  , _pkValidityStartTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pkFingerprint' - The fingerprint of the public key.
--
-- * 'pkValidityEndTime' - The ending time of validity of the public key.
--
-- * 'pkValue' - The DER encoded public key value in PKCS#1 format.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'pkValidityStartTime' - The starting time of validity of the public key.
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
pkFingerprint = lens _pkFingerprint (\ s a -> s{_pkFingerprint = a})

-- | The ending time of validity of the public key.
pkValidityEndTime :: Lens' PublicKey (Maybe UTCTime)
pkValidityEndTime = lens _pkValidityEndTime (\ s a -> s{_pkValidityEndTime = a}) . mapping _Time

-- | The DER encoded public key value in PKCS#1 format.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
pkValue :: Lens' PublicKey (Maybe ByteString)
pkValue = lens _pkValue (\ s a -> s{_pkValue = a}) . mapping _Base64

-- | The starting time of validity of the public key.
pkValidityStartTime :: Lens' PublicKey (Maybe UTCTime)
pkValidityStartTime = lens _pkValidityStartTime (\ s a -> s{_pkValidityStartTime = a}) . mapping _Time

instance FromJSON PublicKey where
        parseJSON
          = withObject "PublicKey"
              (\ x ->
                 PublicKey' <$>
                   (x .:? "Fingerprint") <*> (x .:? "ValidityEndTime")
                     <*> (x .:? "Value")
                     <*> (x .:? "ValidityStartTime"))

instance Hashable PublicKey where

instance NFData PublicKey where

-- | Specifies the type and name of a resource referenced by an event.
--
--
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rResourceType :: !(Maybe Text)
  , _rResourceName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceType' - The type of a resource referenced by the event returned. When the resource type cannot be determined, null is returned. Some examples of resource types are: __Instance__ for EC2, __Trail__ for CloudTrail, __DBInstance__ for RDS, and __AccessKey__ for IAM. For a list of resource types supported for event lookup, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/lookup_supported_resourcetypes.html Resource Types Supported for Event Lookup> .
--
-- * 'rResourceName' - The name of the resource referenced by the event returned. These are user-created names whose values will depend on the environment. For example, the resource name might be "auto-scaling-test-group" for an Auto Scaling Group or "i-1234567" for an EC2 Instance.
resource
    :: Resource
resource = Resource' {_rResourceType = Nothing, _rResourceName = Nothing}


-- | The type of a resource referenced by the event returned. When the resource type cannot be determined, null is returned. Some examples of resource types are: __Instance__ for EC2, __Trail__ for CloudTrail, __DBInstance__ for RDS, and __AccessKey__ for IAM. For a list of resource types supported for event lookup, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/lookup_supported_resourcetypes.html Resource Types Supported for Event Lookup> .
rResourceType :: Lens' Resource (Maybe Text)
rResourceType = lens _rResourceType (\ s a -> s{_rResourceType = a})

-- | The name of the resource referenced by the event returned. These are user-created names whose values will depend on the environment. For example, the resource name might be "auto-scaling-test-group" for an Auto Scaling Group or "i-1234567" for an EC2 Instance.
rResourceName :: Lens' Resource (Maybe Text)
rResourceName = lens _rResourceName (\ s a -> s{_rResourceName = a})

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x ->
                 Resource' <$>
                   (x .:? "ResourceType") <*> (x .:? "ResourceName"))

instance Hashable Resource where

instance NFData Resource where

-- | A resource tag.
--
--
--
-- /See:/ 'resourceTag' smart constructor.
data ResourceTag = ResourceTag'
  { _rResourceId :: !(Maybe Text)
  , _rTagsList   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceId' - Specifies the ARN of the resource.
--
-- * 'rTagsList' - A list of tags.
resourceTag
    :: ResourceTag
resourceTag = ResourceTag' {_rResourceId = Nothing, _rTagsList = Nothing}


-- | Specifies the ARN of the resource.
rResourceId :: Lens' ResourceTag (Maybe Text)
rResourceId = lens _rResourceId (\ s a -> s{_rResourceId = a})

-- | A list of tags.
rTagsList :: Lens' ResourceTag [Tag]
rTagsList = lens _rTagsList (\ s a -> s{_rTagsList = a}) . _Default . _Coerce

instance FromJSON ResourceTag where
        parseJSON
          = withObject "ResourceTag"
              (\ x ->
                 ResourceTag' <$>
                   (x .:? "ResourceId") <*>
                     (x .:? "TagsList" .!= mempty))

instance Hashable ResourceTag where

instance NFData ResourceTag where

-- | A custom key-value pair associated with a resource such as a CloudTrail trail.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value in a key-value pair of a tag. The value must be no longer than 256 Unicode characters.
--
-- * 'tagKey' - The key in a key-value pair. The key must be must be no longer than 128 Unicode characters. The key must be unique for the resource to which it applies.
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | The value in a key-value pair of a tag. The value must be no longer than 256 Unicode characters.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key in a key-value pair. The key must be must be no longer than 128 Unicode characters. The key must be unique for the resource to which it applies.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .: "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue,
                  Just ("Key" .= _tagKey)])

-- | The settings for a trail.
--
--
--
-- /See:/ 'trail' smart constructor.
data Trail = Trail'
  { _tLogFileValidationEnabled   :: !(Maybe Bool)
  , _tTrailARN                   :: !(Maybe Text)
  , _tS3KeyPrefix                :: !(Maybe Text)
  , _tSNSTopicARN                :: !(Maybe Text)
  , _tSNSTopicName               :: !(Maybe Text)
  , _tCloudWatchLogsLogGroupARN  :: !(Maybe Text)
  , _tKMSKeyId                   :: !(Maybe Text)
  , _tHomeRegion                 :: !(Maybe Text)
  , _tName                       :: !(Maybe Text)
  , _tIncludeGlobalServiceEvents :: !(Maybe Bool)
  , _tHasCustomEventSelectors    :: !(Maybe Bool)
  , _tCloudWatchLogsRoleARN      :: !(Maybe Text)
  , _tS3BucketName               :: !(Maybe Text)
  , _tIsMultiRegionTrail         :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Trail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tLogFileValidationEnabled' - Specifies whether log file validation is enabled.
--
-- * 'tTrailARN' - Specifies the ARN of the trail. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
--
-- * 'tS3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .The maximum length is 200 characters.
--
-- * 'tSNSTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is: @arn:aws:sns:us-east-1:123456789012:MyTopic@
--
-- * 'tSNSTopicName' - This field is deprecated. Use SnsTopicARN.
--
-- * 'tCloudWatchLogsLogGroupARN' - Specifies an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered.
--
-- * 'tKMSKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@
--
-- * 'tHomeRegion' - The region in which the trail was created.
--
-- * 'tName' - Name of the trail set by calling 'CreateTrail' . The maximum length is 128 characters.
--
-- * 'tIncludeGlobalServiceEvents' - Set to __True__ to include AWS API calls from AWS global services such as IAM. Otherwise, __False__ .
--
-- * 'tHasCustomEventSelectors' - Specifies if the trail has custom event selectors.
--
-- * 'tCloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- * 'tS3BucketName' - Name of the Amazon S3 bucket into which CloudTrail delivers your trail files. See <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
--
-- * 'tIsMultiRegionTrail' - Specifies whether the trail belongs only to one region or exists in all regions.
trail
    :: Trail
trail =
  Trail'
    { _tLogFileValidationEnabled = Nothing
    , _tTrailARN = Nothing
    , _tS3KeyPrefix = Nothing
    , _tSNSTopicARN = Nothing
    , _tSNSTopicName = Nothing
    , _tCloudWatchLogsLogGroupARN = Nothing
    , _tKMSKeyId = Nothing
    , _tHomeRegion = Nothing
    , _tName = Nothing
    , _tIncludeGlobalServiceEvents = Nothing
    , _tHasCustomEventSelectors = Nothing
    , _tCloudWatchLogsRoleARN = Nothing
    , _tS3BucketName = Nothing
    , _tIsMultiRegionTrail = Nothing
    }


-- | Specifies whether log file validation is enabled.
tLogFileValidationEnabled :: Lens' Trail (Maybe Bool)
tLogFileValidationEnabled = lens _tLogFileValidationEnabled (\ s a -> s{_tLogFileValidationEnabled = a})

-- | Specifies the ARN of the trail. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
tTrailARN :: Lens' Trail (Maybe Text)
tTrailARN = lens _tTrailARN (\ s a -> s{_tTrailARN = a})

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .The maximum length is 200 characters.
tS3KeyPrefix :: Lens' Trail (Maybe Text)
tS3KeyPrefix = lens _tS3KeyPrefix (\ s a -> s{_tS3KeyPrefix = a})

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is: @arn:aws:sns:us-east-1:123456789012:MyTopic@
tSNSTopicARN :: Lens' Trail (Maybe Text)
tSNSTopicARN = lens _tSNSTopicARN (\ s a -> s{_tSNSTopicARN = a})

-- | This field is deprecated. Use SnsTopicARN.
tSNSTopicName :: Lens' Trail (Maybe Text)
tSNSTopicName = lens _tSNSTopicName (\ s a -> s{_tSNSTopicName = a})

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered.
tCloudWatchLogsLogGroupARN :: Lens' Trail (Maybe Text)
tCloudWatchLogsLogGroupARN = lens _tCloudWatchLogsLogGroupARN (\ s a -> s{_tCloudWatchLogsLogGroupARN = a})

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@
tKMSKeyId :: Lens' Trail (Maybe Text)
tKMSKeyId = lens _tKMSKeyId (\ s a -> s{_tKMSKeyId = a})

-- | The region in which the trail was created.
tHomeRegion :: Lens' Trail (Maybe Text)
tHomeRegion = lens _tHomeRegion (\ s a -> s{_tHomeRegion = a})

-- | Name of the trail set by calling 'CreateTrail' . The maximum length is 128 characters.
tName :: Lens' Trail (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a})

-- | Set to __True__ to include AWS API calls from AWS global services such as IAM. Otherwise, __False__ .
tIncludeGlobalServiceEvents :: Lens' Trail (Maybe Bool)
tIncludeGlobalServiceEvents = lens _tIncludeGlobalServiceEvents (\ s a -> s{_tIncludeGlobalServiceEvents = a})

-- | Specifies if the trail has custom event selectors.
tHasCustomEventSelectors :: Lens' Trail (Maybe Bool)
tHasCustomEventSelectors = lens _tHasCustomEventSelectors (\ s a -> s{_tHasCustomEventSelectors = a})

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
tCloudWatchLogsRoleARN :: Lens' Trail (Maybe Text)
tCloudWatchLogsRoleARN = lens _tCloudWatchLogsRoleARN (\ s a -> s{_tCloudWatchLogsRoleARN = a})

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail files. See <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
tS3BucketName :: Lens' Trail (Maybe Text)
tS3BucketName = lens _tS3BucketName (\ s a -> s{_tS3BucketName = a})

-- | Specifies whether the trail belongs only to one region or exists in all regions.
tIsMultiRegionTrail :: Lens' Trail (Maybe Bool)
tIsMultiRegionTrail = lens _tIsMultiRegionTrail (\ s a -> s{_tIsMultiRegionTrail = a})

instance FromJSON Trail where
        parseJSON
          = withObject "Trail"
              (\ x ->
                 Trail' <$>
                   (x .:? "LogFileValidationEnabled") <*>
                     (x .:? "TrailARN")
                     <*> (x .:? "S3KeyPrefix")
                     <*> (x .:? "SnsTopicARN")
                     <*> (x .:? "SnsTopicName")
                     <*> (x .:? "CloudWatchLogsLogGroupArn")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "HomeRegion")
                     <*> (x .:? "Name")
                     <*> (x .:? "IncludeGlobalServiceEvents")
                     <*> (x .:? "HasCustomEventSelectors")
                     <*> (x .:? "CloudWatchLogsRoleArn")
                     <*> (x .:? "S3BucketName")
                     <*> (x .:? "IsMultiRegionTrail"))

instance Hashable Trail where

instance NFData Trail where
