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
-- Module      : Network.AWS.CloudTrail.UpdateTrail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings that specify delivery of log files. Changes to a trail do not require stopping the CloudTrail service. Use this action to designate an existing bucket for log delivery. If the existing bucket has previously been a target for CloudTrail log files, an IAM policy exists for the bucket. @UpdateTrail@ must be called from the region in which the trail was created; otherwise, an @InvalidHomeRegionException@ is thrown.
--
--
module Network.AWS.CloudTrail.UpdateTrail
    (
    -- * Creating a Request
      updateTrail
    , UpdateTrail
    -- * Request Lenses
    , utS3KeyPrefix
    , utSNSTopicName
    , utEnableLogFileValidation
    , utCloudWatchLogsLogGroupARN
    , utKMSKeyId
    , utIncludeGlobalServiceEvents
    , utCloudWatchLogsRoleARN
    , utS3BucketName
    , utIsMultiRegionTrail
    , utName

    -- * Destructuring the Response
    , updateTrailResponse
    , UpdateTrailResponse
    -- * Response Lenses
    , utrsLogFileValidationEnabled
    , utrsTrailARN
    , utrsS3KeyPrefix
    , utrsSNSTopicARN
    , utrsSNSTopicName
    , utrsCloudWatchLogsLogGroupARN
    , utrsKMSKeyId
    , utrsName
    , utrsIncludeGlobalServiceEvents
    , utrsCloudWatchLogsRoleARN
    , utrsS3BucketName
    , utrsIsMultiRegionTrail
    , utrsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Specifies settings to update for the trail.
--
--
--
-- /See:/ 'updateTrail' smart constructor.
data UpdateTrail = UpdateTrail'
  { _utS3KeyPrefix                :: !(Maybe Text)
  , _utSNSTopicName               :: !(Maybe Text)
  , _utEnableLogFileValidation    :: !(Maybe Bool)
  , _utCloudWatchLogsLogGroupARN  :: !(Maybe Text)
  , _utKMSKeyId                   :: !(Maybe Text)
  , _utIncludeGlobalServiceEvents :: !(Maybe Bool)
  , _utCloudWatchLogsRoleARN      :: !(Maybe Text)
  , _utS3BucketName               :: !(Maybe Text)
  , _utIsMultiRegionTrail         :: !(Maybe Bool)
  , _utName                       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTrail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utS3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
--
-- * 'utSNSTopicName' - Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
--
-- * 'utEnableLogFileValidation' - Specifies whether log file validation is enabled. The default is false.
--
-- * 'utCloudWatchLogsLogGroupARN' - Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
--
-- * 'utKMSKeyId' - Specifies the KMS key ID to use to encrypt the logs delivered by CloudTrail. The value can be an alias name prefixed by "alias/", a fully specified ARN to an alias, a fully specified ARN to a key, or a globally unique identifier. Examples:     * alias/MyAliasName     * arn:aws:kms:us-east-1:123456789012:alias/MyAliasName     * arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * 12345678-1234-1234-1234-123456789012
--
-- * 'utIncludeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- * 'utCloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- * 'utS3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log files. See <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
--
-- * 'utIsMultiRegionTrail' - Specifies whether the trail applies only to the current region or to all regions. The default is false. If the trail exists only in the current region and this value is set to true, shadow trails (replications of the trail) will be created in the other regions. If the trail exists in all regions and this value is set to false, the trail will remain in the region where it was created, and its shadow trails in other regions will be deleted.
--
-- * 'utName' - Specifies the name of the trail or trail ARN. If @Name@ is a trail name, the string must meet the following requirements:     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)     * Start with a letter or number, and end with a letter or number     * Be between 3 and 128 characters     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.     * Not be in IP address format (for example, 192.168.5.4) If @Name@ is a trail ARN, it must be in the format: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
updateTrail
    :: Text -- ^ 'utName'
    -> UpdateTrail
updateTrail pName_ =
  UpdateTrail'
    { _utS3KeyPrefix = Nothing
    , _utSNSTopicName = Nothing
    , _utEnableLogFileValidation = Nothing
    , _utCloudWatchLogsLogGroupARN = Nothing
    , _utKMSKeyId = Nothing
    , _utIncludeGlobalServiceEvents = Nothing
    , _utCloudWatchLogsRoleARN = Nothing
    , _utS3BucketName = Nothing
    , _utIsMultiRegionTrail = Nothing
    , _utName = pName_
    }


-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> . The maximum length is 200 characters.
utS3KeyPrefix :: Lens' UpdateTrail (Maybe Text)
utS3KeyPrefix = lens _utS3KeyPrefix (\ s a -> s{_utS3KeyPrefix = a})

-- | Specifies the name of the Amazon SNS topic defined for notification of log file delivery. The maximum length is 256 characters.
utSNSTopicName :: Lens' UpdateTrail (Maybe Text)
utSNSTopicName = lens _utSNSTopicName (\ s a -> s{_utSNSTopicName = a})

-- | Specifies whether log file validation is enabled. The default is false.
utEnableLogFileValidation :: Lens' UpdateTrail (Maybe Bool)
utEnableLogFileValidation = lens _utEnableLogFileValidation (\ s a -> s{_utEnableLogFileValidation = a})

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique identifier that represents the log group to which CloudTrail logs will be delivered. Not required unless you specify CloudWatchLogsRoleArn.
utCloudWatchLogsLogGroupARN :: Lens' UpdateTrail (Maybe Text)
utCloudWatchLogsLogGroupARN = lens _utCloudWatchLogsLogGroupARN (\ s a -> s{_utCloudWatchLogsLogGroupARN = a})

-- | Specifies the KMS key ID to use to encrypt the logs delivered by CloudTrail. The value can be an alias name prefixed by "alias/", a fully specified ARN to an alias, a fully specified ARN to a key, or a globally unique identifier. Examples:     * alias/MyAliasName     * arn:aws:kms:us-east-1:123456789012:alias/MyAliasName     * arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012     * 12345678-1234-1234-1234-123456789012
utKMSKeyId :: Lens' UpdateTrail (Maybe Text)
utKMSKeyId = lens _utKMSKeyId (\ s a -> s{_utKMSKeyId = a})

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
utIncludeGlobalServiceEvents :: Lens' UpdateTrail (Maybe Bool)
utIncludeGlobalServiceEvents = lens _utIncludeGlobalServiceEvents (\ s a -> s{_utIncludeGlobalServiceEvents = a})

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
utCloudWatchLogsRoleARN :: Lens' UpdateTrail (Maybe Text)
utCloudWatchLogsRoleARN = lens _utCloudWatchLogsRoleARN (\ s a -> s{_utCloudWatchLogsRoleARN = a})

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files. See <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/create_trail_naming_policy.html Amazon S3 Bucket Naming Requirements> .
utS3BucketName :: Lens' UpdateTrail (Maybe Text)
utS3BucketName = lens _utS3BucketName (\ s a -> s{_utS3BucketName = a})

-- | Specifies whether the trail applies only to the current region or to all regions. The default is false. If the trail exists only in the current region and this value is set to true, shadow trails (replications of the trail) will be created in the other regions. If the trail exists in all regions and this value is set to false, the trail will remain in the region where it was created, and its shadow trails in other regions will be deleted.
utIsMultiRegionTrail :: Lens' UpdateTrail (Maybe Bool)
utIsMultiRegionTrail = lens _utIsMultiRegionTrail (\ s a -> s{_utIsMultiRegionTrail = a})

-- | Specifies the name of the trail or trail ARN. If @Name@ is a trail name, the string must meet the following requirements:     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)     * Start with a letter or number, and end with a letter or number     * Be between 3 and 128 characters     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.     * Not be in IP address format (for example, 192.168.5.4) If @Name@ is a trail ARN, it must be in the format: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
utName :: Lens' UpdateTrail Text
utName = lens _utName (\ s a -> s{_utName = a})

instance AWSRequest UpdateTrail where
        type Rs UpdateTrail = UpdateTrailResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTrailResponse' <$>
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

instance Hashable UpdateTrail where

instance NFData UpdateTrail where

instance ToHeaders UpdateTrail where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.UpdateTrail"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateTrail where
        toJSON UpdateTrail'{..}
          = object
              (catMaybes
                 [("S3KeyPrefix" .=) <$> _utS3KeyPrefix,
                  ("SnsTopicName" .=) <$> _utSNSTopicName,
                  ("EnableLogFileValidation" .=) <$>
                    _utEnableLogFileValidation,
                  ("CloudWatchLogsLogGroupArn" .=) <$>
                    _utCloudWatchLogsLogGroupARN,
                  ("KmsKeyId" .=) <$> _utKMSKeyId,
                  ("IncludeGlobalServiceEvents" .=) <$>
                    _utIncludeGlobalServiceEvents,
                  ("CloudWatchLogsRoleArn" .=) <$>
                    _utCloudWatchLogsRoleARN,
                  ("S3BucketName" .=) <$> _utS3BucketName,
                  ("IsMultiRegionTrail" .=) <$> _utIsMultiRegionTrail,
                  Just ("Name" .= _utName)])

instance ToPath UpdateTrail where
        toPath = const "/"

instance ToQuery UpdateTrail where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
--
--
-- /See:/ 'updateTrailResponse' smart constructor.
data UpdateTrailResponse = UpdateTrailResponse'
  { _utrsLogFileValidationEnabled   :: !(Maybe Bool)
  , _utrsTrailARN                   :: !(Maybe Text)
  , _utrsS3KeyPrefix                :: !(Maybe Text)
  , _utrsSNSTopicARN                :: !(Maybe Text)
  , _utrsSNSTopicName               :: !(Maybe Text)
  , _utrsCloudWatchLogsLogGroupARN  :: !(Maybe Text)
  , _utrsKMSKeyId                   :: !(Maybe Text)
  , _utrsName                       :: !(Maybe Text)
  , _utrsIncludeGlobalServiceEvents :: !(Maybe Bool)
  , _utrsCloudWatchLogsRoleARN      :: !(Maybe Text)
  , _utrsS3BucketName               :: !(Maybe Text)
  , _utrsIsMultiRegionTrail         :: !(Maybe Bool)
  , _utrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTrailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrsLogFileValidationEnabled' - Specifies whether log file integrity validation is enabled.
--
-- * 'utrsTrailARN' - Specifies the ARN of the trail that was updated. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
--
-- * 'utrsS3KeyPrefix' - Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
--
-- * 'utrsSNSTopicARN' - Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is: @arn:aws:sns:us-east-1:123456789012:MyTopic@
--
-- * 'utrsSNSTopicName' - This field is deprecated. Use SnsTopicARN.
--
-- * 'utrsCloudWatchLogsLogGroupARN' - Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
--
-- * 'utrsKMSKeyId' - Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@
--
-- * 'utrsName' - Specifies the name of the trail.
--
-- * 'utrsIncludeGlobalServiceEvents' - Specifies whether the trail is publishing events from global services such as IAM to the log files.
--
-- * 'utrsCloudWatchLogsRoleARN' - Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
--
-- * 'utrsS3BucketName' - Specifies the name of the Amazon S3 bucket designated for publishing log files.
--
-- * 'utrsIsMultiRegionTrail' - Specifies whether the trail exists in one region or in all regions.
--
-- * 'utrsResponseStatus' - -- | The response status code.
updateTrailResponse
    :: Int -- ^ 'utrsResponseStatus'
    -> UpdateTrailResponse
updateTrailResponse pResponseStatus_ =
  UpdateTrailResponse'
    { _utrsLogFileValidationEnabled = Nothing
    , _utrsTrailARN = Nothing
    , _utrsS3KeyPrefix = Nothing
    , _utrsSNSTopicARN = Nothing
    , _utrsSNSTopicName = Nothing
    , _utrsCloudWatchLogsLogGroupARN = Nothing
    , _utrsKMSKeyId = Nothing
    , _utrsName = Nothing
    , _utrsIncludeGlobalServiceEvents = Nothing
    , _utrsCloudWatchLogsRoleARN = Nothing
    , _utrsS3BucketName = Nothing
    , _utrsIsMultiRegionTrail = Nothing
    , _utrsResponseStatus = pResponseStatus_
    }


-- | Specifies whether log file integrity validation is enabled.
utrsLogFileValidationEnabled :: Lens' UpdateTrailResponse (Maybe Bool)
utrsLogFileValidationEnabled = lens _utrsLogFileValidationEnabled (\ s a -> s{_utrsLogFileValidationEnabled = a})

-- | Specifies the ARN of the trail that was updated. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
utrsTrailARN :: Lens' UpdateTrailResponse (Maybe Text)
utrsTrailARN = lens _utrsTrailARN (\ s a -> s{_utrsTrailARN = a})

-- | Specifies the Amazon S3 key prefix that comes after the name of the bucket you have designated for log file delivery. For more information, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-find-log-files.html Finding Your CloudTrail Log Files> .
utrsS3KeyPrefix :: Lens' UpdateTrailResponse (Maybe Text)
utrsS3KeyPrefix = lens _utrsS3KeyPrefix (\ s a -> s{_utrsS3KeyPrefix = a})

-- | Specifies the ARN of the Amazon SNS topic that CloudTrail uses to send notifications when log files are delivered. The format of a topic ARN is: @arn:aws:sns:us-east-1:123456789012:MyTopic@
utrsSNSTopicARN :: Lens' UpdateTrailResponse (Maybe Text)
utrsSNSTopicARN = lens _utrsSNSTopicARN (\ s a -> s{_utrsSNSTopicARN = a})

-- | This field is deprecated. Use SnsTopicARN.
utrsSNSTopicName :: Lens' UpdateTrailResponse (Maybe Text)
utrsSNSTopicName = lens _utrsSNSTopicName (\ s a -> s{_utrsSNSTopicName = a})

-- | Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail logs will be delivered.
utrsCloudWatchLogsLogGroupARN :: Lens' UpdateTrailResponse (Maybe Text)
utrsCloudWatchLogsLogGroupARN = lens _utrsCloudWatchLogsLogGroupARN (\ s a -> s{_utrsCloudWatchLogsLogGroupARN = a})

-- | Specifies the KMS key ID that encrypts the logs delivered by CloudTrail. The value is a fully specified ARN to a KMS key in the format: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@
utrsKMSKeyId :: Lens' UpdateTrailResponse (Maybe Text)
utrsKMSKeyId = lens _utrsKMSKeyId (\ s a -> s{_utrsKMSKeyId = a})

-- | Specifies the name of the trail.
utrsName :: Lens' UpdateTrailResponse (Maybe Text)
utrsName = lens _utrsName (\ s a -> s{_utrsName = a})

-- | Specifies whether the trail is publishing events from global services such as IAM to the log files.
utrsIncludeGlobalServiceEvents :: Lens' UpdateTrailResponse (Maybe Bool)
utrsIncludeGlobalServiceEvents = lens _utrsIncludeGlobalServiceEvents (\ s a -> s{_utrsIncludeGlobalServiceEvents = a})

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a user's log group.
utrsCloudWatchLogsRoleARN :: Lens' UpdateTrailResponse (Maybe Text)
utrsCloudWatchLogsRoleARN = lens _utrsCloudWatchLogsRoleARN (\ s a -> s{_utrsCloudWatchLogsRoleARN = a})

-- | Specifies the name of the Amazon S3 bucket designated for publishing log files.
utrsS3BucketName :: Lens' UpdateTrailResponse (Maybe Text)
utrsS3BucketName = lens _utrsS3BucketName (\ s a -> s{_utrsS3BucketName = a})

-- | Specifies whether the trail exists in one region or in all regions.
utrsIsMultiRegionTrail :: Lens' UpdateTrailResponse (Maybe Bool)
utrsIsMultiRegionTrail = lens _utrsIsMultiRegionTrail (\ s a -> s{_utrsIsMultiRegionTrail = a})

-- | -- | The response status code.
utrsResponseStatus :: Lens' UpdateTrailResponse Int
utrsResponseStatus = lens _utrsResponseStatus (\ s a -> s{_utrsResponseStatus = a})

instance NFData UpdateTrailResponse where
