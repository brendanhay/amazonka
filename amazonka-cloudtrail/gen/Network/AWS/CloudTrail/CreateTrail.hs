{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.CreateTrail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a trail that specifies the settings for delivery of log data to an Amazon S3 bucket. A maximum of five trails can exist in a region, irrespective of the region in which they were created.
--
--
module Network.AWS.CloudTrail.CreateTrail
    (
    -- * Creating a Request
      createTrail
    , CreateTrail
    -- * Request Lenses
    , ctS3KeyPrefix
    , ctSNSTopicName
    , ctEnableLogFileValidation
    , ctCloudWatchLogsLogGroupARN
    , ctKMSKeyId
    , ctIncludeGlobalServiceEvents
    , ctCloudWatchLogsRoleARN
    , ctIsMultiRegionTrail
    , ctName
    , ctS3BucketName

    -- * Destructuring the Response
    , createTrailResponse
    , CreateTrailResponse
    -- * Response Lenses
    , ctrsLogFileValidationEnabled
    , ctrsTrailARN
    , ctrsS3KeyPrefix
    , ctrsSNSTopicARN
    , ctrsSNSTopicName
    , ctrsCloudWatchLogsLogGroupARN
    , ctrsKMSKeyId
    , ctrsName
    , ctrsIncludeGlobalServiceEvents
    , ctrsCloudWatchLogsRoleARN
    , ctrsS3BucketName
    , ctrsIsMultiRegionTrail
    , ctrsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Specifies the settings for each trail.
--
--
--
-- /See:/ 'createTrail' smart constructor.
data CreateTrail = CreateTrail'
  { _ctS3KeyPrefix                :: !(Maybe Text)
  , _ctSNSTopicName               :: !(Maybe Text)
  , _ctEnableLogFileValidation    :: !(Maybe Bool)
  , _ctCloudWatchLogsLogGroupARN  :: !(Maybe Text)
  , _ctKMSKeyId                   :: !(Maybe Text)
  , _ctIncludeGlobalServiceEvents :: !(Maybe Bool)
  , _ctCloudWatchLogsRoleARN      :: !(Maybe Text)
  , _ctIsMultiRegionTrail         :: !(Maybe Bool)
  , _ctName                       :: !Text
  , _ctS3BucketName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctS3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
--
-- * 'ctSNSTopicName' - Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
--
-- * 'ctEnableLogFileValidation' - Specifies whether log file integrity validation is enabled. The default is false.
--
-- * 'ctCloudWatchLogsLogGroupARN' - Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
--
-- * 'ctKMSKeyId' - Specifies the KMS key ID to use to encrypt the logs delivered by CloudTrail. The value can be an alias name prefixed by "alias/", a fully specified ARN to an alias, a fully specified ARN to a key, or a globally unique identifier. Examples:     * alias/MyAliasName     * arn:aws:kms:us-east-1:123456789012:alias/MyAliasName     * arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * 12345678-1234-1234-1234-123456789012
--
-- * 'ctIncludeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- * 'ctCloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- * 'ctIsMultiRegionTrail' - Specifies whether the trail is created in the current region or in all regions. The default is false.
--
-- * 'ctName' - Specifies the name of the trail. The name must meet the following requirements:     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)     * Start with a letter or number, and end with a letter or number     * Be between 3 and 128 characters     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.     * Not be in IP address format (for example, 192.168.5.4)
--
-- * 'ctS3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log files. See <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
createTrail
    :: Text -- ^ 'ctName'
    -> Text -- ^ 'ctS3BucketName'
    -> CreateTrail
createTrail pName_ pS3BucketName_ =
  CreateTrail'
    { _ctS3KeyPrefix = Nothing
    , _ctSNSTopicName = Nothing
    , _ctEnableLogFileValidation = Nothing
    , _ctCloudWatchLogsLogGroupARN = Nothing
    , _ctKMSKeyId = Nothing
    , _ctIncludeGlobalServiceEvents = Nothing
    , _ctCloudWatchLogsRoleARN = Nothing
    , _ctIsMultiRegionTrail = Nothing
    , _ctName = pName_
    , _ctS3BucketName = pS3BucketName_
    }


-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
ctS3KeyPrefix :: Lens' CreateTrail (Maybe Text)
ctS3KeyPrefix = lens _ctS3KeyPrefix (\ s a -> s{_ctS3KeyPrefix = a})

-- | Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
ctSNSTopicName :: Lens' CreateTrail (Maybe Text)
ctSNSTopicName = lens _ctSNSTopicName (\ s a -> s{_ctSNSTopicName = a})

-- | Specifies whether log file integrity validation is enabled. The default is false.
ctEnableLogFileValidation :: Lens' CreateTrail (Maybe Bool)
ctEnableLogFileValidation = lens _ctEnableLogFileValidation (\ s a -> s{_ctEnableLogFileValidation = a})

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
ctCloudWatchLogsLogGroupARN :: Lens' CreateTrail (Maybe Text)
ctCloudWatchLogsLogGroupARN = lens _ctCloudWatchLogsLogGroupARN (\ s a -> s{_ctCloudWatchLogsLogGroupARN = a})

-- | Specifies the KMS key ID to use to encrypt the logs delivered by CloudTrail. The value can be an alias name prefixed by "alias/", a fully specified ARN to an alias, a fully specified ARN to a key, or a globally unique identifier. Examples:     * alias/MyAliasName     * arn:aws:kms:us-east-1:123456789012:alias/MyAliasName     * arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * 12345678-1234-1234-1234-123456789012
ctKMSKeyId :: Lens' CreateTrail (Maybe Text)
ctKMSKeyId = lens _ctKMSKeyId (\ s a -> s{_ctKMSKeyId = a})

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
ctIncludeGlobalServiceEvents :: Lens' CreateTrail (Maybe Bool)
ctIncludeGlobalServiceEvents = lens _ctIncludeGlobalServiceEvents (\ s a -> s{_ctIncludeGlobalServiceEvents = a})

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
ctCloudWatchLogsRoleARN :: Lens' CreateTrail (Maybe Text)
ctCloudWatchLogsRoleARN = lens _ctCloudWatchLogsRoleARN (\ s a -> s{_ctCloudWatchLogsRoleARN = a})

-- | Specifies whether the trail is created in the current region or in all regions. The default is false.
ctIsMultiRegionTrail :: Lens' CreateTrail (Maybe Bool)
ctIsMultiRegionTrail = lens _ctIsMultiRegionTrail (\ s a -> s{_ctIsMultiRegionTrail = a})

-- | Specifies the name of the trail. The name must meet the following requirements:     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)     * Start with a letter or number, and end with a letter or number     * Be between 3 and 128 characters     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.     * Not be in IP address format (for example, 192.168.5.4)
ctName :: Lens' CreateTrail Text
ctName = lens _ctName (\ s a -> s{_ctName = a})

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files. See <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
ctS3BucketName :: Lens' CreateTrail Text
ctS3BucketName = lens _ctS3BucketName (\ s a -> s{_ctS3BucketName = a})

instance AWSRequest CreateTrail where
        type Rs CreateTrail = CreateTrailResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 CreateTrailResponse' <$>
                   (x .?> "LogFileValidationEnabled") <*>
                     (x .?> "TrailARN")
                     <*> (x .?> "S3KeyPrefix")
                     <*> (x .?> "SnsTopicARN")
                     <*> (x .?> "SnsTopicName")
                     <*> (x .?> "CloudWatchLogsLogGroupArn")
                     <*> (x .?> "KmsKeyId")
                     <*> (x .?> "Name")
                     <*> (x .?> "IncludeGlobalServiceEvents")
                     <*> (x .?> "CloudWatchLogsRoleArn")
                     <*> (x .?> "S3BucketName")
                     <*> (x .?> "IsMultiRegionTrail")
                     <*> (pure (fromEnum s)))

instance Hashable CreateTrail where

instance NFData CreateTrail where

instance ToHeaders CreateTrail where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.CreateTrail"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateTrail where
        toJSON CreateTrail'{..}
          = object
              (catMaybes
                 [("S3KeyPrefix" .=) <$> _ctS3KeyPrefix,
                  ("SnsTopicName" .=) <$> _ctSNSTopicName,
                  ("EnableLogFileValidation" .=) <$>
                    _ctEnableLogFileValidation,
                  ("CloudWatchLogsLogGroupArn" .=) <$>
                    _ctCloudWatchLogsLogGroupARN,
                  ("KmsKeyId" .=) <$> _ctKMSKeyId,
                  ("IncludeGlobalServiceEvents" .=) <$>
                    _ctIncludeGlobalServiceEvents,
                  ("CloudWatchLogsRoleArn" .=) <$>
                    _ctCloudWatchLogsRoleARN,
                  ("IsMultiRegionTrail" .=) <$> _ctIsMultiRegionTrail,
                  Just ("Name" .= _ctName),
                  Just ("S3BucketName" .= _ctS3BucketName)])

instance ToPath CreateTrail where
        toPath = const "/"

instance ToQuery CreateTrail where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
--
--
-- /See:/ 'createTrailResponse' smart constructor.
data CreateTrailResponse = CreateTrailResponse'
  { _ctrsLogFileValidationEnabled   :: !(Maybe Bool)
  , _ctrsTrailARN                   :: !(Maybe Text)
  , _ctrsS3KeyPrefix                :: !(Maybe Text)
  , _ctrsSNSTopicARN                :: !(Maybe Text)
  , _ctrsSNSTopicName               :: !(Maybe Text)
  , _ctrsCloudWatchLogsLogGroupARN  :: !(Maybe Text)
  , _ctrsKMSKeyId                   :: !(Maybe Text)
  , _ctrsName                       :: !(Maybe Text)
  , _ctrsIncludeGlobalServiceEvents :: !(Maybe Bool)
  , _ctrsCloudWatchLogsRoleARN      :: !(Maybe Text)
  , _ctrsS3BucketName               :: !(Maybe Text)
  , _ctrsIsMultiRegionTrail         :: !(Maybe Bool)
  , _ctrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTrailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsLogFileValidationEnabled' - Specifies whether log file integrity validation is enabled.
--
-- * 'ctrsTrailARN' - Specifies the ARN of the trail that was created. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
--
-- * 'ctrsS3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
--
-- * 'ctrsSNSTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is: @arn:aws:sns:us-east-1:123456789012:MyTopic@
--
-- * 'ctrsSNSTopicName' - This field is deprecated. Use SnsTopicARN.
--
-- * 'ctrsCloudWatchLogsLogGroupARN' - Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
--
-- * 'ctrsKMSKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@
--
-- * 'ctrsName' - Specifies the name of the trail.
--
-- * 'ctrsIncludeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- * 'ctrsCloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- * 'ctrsS3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log files.
--
-- * 'ctrsIsMultiRegionTrail' - Specifies whether the trail exists in one region or in all regions.
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTrailResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateTrailResponse
createTrailResponse pResponseStatus_ =
  CreateTrailResponse'
    { _ctrsLogFileValidationEnabled = Nothing
    , _ctrsTrailARN = Nothing
    , _ctrsS3KeyPrefix = Nothing
    , _ctrsSNSTopicARN = Nothing
    , _ctrsSNSTopicName = Nothing
    , _ctrsCloudWatchLogsLogGroupARN = Nothing
    , _ctrsKMSKeyId = Nothing
    , _ctrsName = Nothing
    , _ctrsIncludeGlobalServiceEvents = Nothing
    , _ctrsCloudWatchLogsRoleARN = Nothing
    , _ctrsS3BucketName = Nothing
    , _ctrsIsMultiRegionTrail = Nothing
    , _ctrsResponseStatus = pResponseStatus_
    }


-- | Specifies whether log file integrity validation is enabled.
ctrsLogFileValidationEnabled :: Lens' CreateTrailResponse (Maybe Bool)
ctrsLogFileValidationEnabled = lens _ctrsLogFileValidationEnabled (\ s a -> s{_ctrsLogFileValidationEnabled = a})

-- | Specifies the ARN of the trail that was created. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
ctrsTrailARN :: Lens' CreateTrailResponse (Maybe Text)
ctrsTrailARN = lens _ctrsTrailARN (\ s a -> s{_ctrsTrailARN = a})

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
ctrsS3KeyPrefix :: Lens' CreateTrailResponse (Maybe Text)
ctrsS3KeyPrefix = lens _ctrsS3KeyPrefix (\ s a -> s{_ctrsS3KeyPrefix = a})

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is: @arn:aws:sns:us-east-1:123456789012:MyTopic@
ctrsSNSTopicARN :: Lens' CreateTrailResponse (Maybe Text)
ctrsSNSTopicARN = lens _ctrsSNSTopicARN (\ s a -> s{_ctrsSNSTopicARN = a})

-- | This field is deprecated. Use SnsTopicARN.
ctrsSNSTopicName :: Lens' CreateTrailResponse (Maybe Text)
ctrsSNSTopicName = lens _ctrsSNSTopicName (\ s a -> s{_ctrsSNSTopicName = a})

-- | Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
ctrsCloudWatchLogsLogGroupARN :: Lens' CreateTrailResponse (Maybe Text)
ctrsCloudWatchLogsLogGroupARN = lens _ctrsCloudWatchLogsLogGroupARN (\ s a -> s{_ctrsCloudWatchLogsLogGroupARN = a})

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@
ctrsKMSKeyId :: Lens' CreateTrailResponse (Maybe Text)
ctrsKMSKeyId = lens _ctrsKMSKeyId (\ s a -> s{_ctrsKMSKeyId = a})

-- | Specifies the name of the trail.
ctrsName :: Lens' CreateTrailResponse (Maybe Text)
ctrsName = lens _ctrsName (\ s a -> s{_ctrsName = a})

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
ctrsIncludeGlobalServiceEvents :: Lens' CreateTrailResponse (Maybe Bool)
ctrsIncludeGlobalServiceEvents = lens _ctrsIncludeGlobalServiceEvents (\ s a -> s{_ctrsIncludeGlobalServiceEvents = a})

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
ctrsCloudWatchLogsRoleARN :: Lens' CreateTrailResponse (Maybe Text)
ctrsCloudWatchLogsRoleARN = lens _ctrsCloudWatchLogsRoleARN (\ s a -> s{_ctrsCloudWatchLogsRoleARN = a})

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files.
ctrsS3BucketName :: Lens' CreateTrailResponse (Maybe Text)
ctrsS3BucketName = lens _ctrsS3BucketName (\ s a -> s{_ctrsS3BucketName = a})

-- | Specifies whether the trail exists in one region or in all regions.
ctrsIsMultiRegionTrail :: Lens' CreateTrailResponse (Maybe Bool)
ctrsIsMultiRegionTrail = lens _ctrsIsMultiRegionTrail (\ s a -> s{_ctrsIsMultiRegionTrail = a})

-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTrailResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a})

instance NFData CreateTrailResponse where
