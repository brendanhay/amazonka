{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudTrail.UpdateTrail
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | From the command line, use @update-subscription@.
--
-- Updates the settings that specify delivery of log files. Changes to a
-- trail do not require stopping the CloudTrail service. Use this action to
-- designate an existing bucket for log delivery. If the existing bucket
-- has previously been a target for CloudTrail log files, an IAM policy
-- exists for the bucket.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_UpdateTrail.html>
module Network.AWS.CloudTrail.UpdateTrail
    (
    -- * Request
      UpdateTrail
    -- ** Request constructor
    , updateTrail
    -- ** Request lenses
    , utS3KeyPrefix
    , utSNSTopicName
    , utCloudWatchLogsLogGroupARN
    , utIncludeGlobalServiceEvents
    , utCloudWatchLogsRoleARN
    , utS3BucketName
    , utName

    -- * Response
    , UpdateTrailResponse
    -- ** Response constructor
    , updateTrailResponse
    -- ** Response lenses
    , utrS3KeyPrefix
    , utrSNSTopicName
    , utrCloudWatchLogsLogGroupARN
    , utrName
    , utrIncludeGlobalServiceEvents
    , utrCloudWatchLogsRoleARN
    , utrS3BucketName
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTrail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utS3KeyPrefix'
--
-- * 'utSNSTopicName'
--
-- * 'utCloudWatchLogsLogGroupARN'
--
-- * 'utIncludeGlobalServiceEvents'
--
-- * 'utCloudWatchLogsRoleARN'
--
-- * 'utS3BucketName'
--
-- * 'utName'
data UpdateTrail = UpdateTrail'{_utS3KeyPrefix :: Maybe Text, _utSNSTopicName :: Maybe Text, _utCloudWatchLogsLogGroupARN :: Maybe Text, _utIncludeGlobalServiceEvents :: Maybe Bool, _utCloudWatchLogsRoleARN :: Maybe Text, _utS3BucketName :: Maybe Text, _utName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateTrail' smart constructor.
updateTrail :: Text -> UpdateTrail
updateTrail pName = UpdateTrail'{_utS3KeyPrefix = Nothing, _utSNSTopicName = Nothing, _utCloudWatchLogsLogGroupARN = Nothing, _utIncludeGlobalServiceEvents = Nothing, _utCloudWatchLogsRoleARN = Nothing, _utS3BucketName = Nothing, _utName = pName};

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket
-- you have designated for log file delivery.
utS3KeyPrefix :: Lens' UpdateTrail (Maybe Text)
utS3KeyPrefix = lens _utS3KeyPrefix (\ s a -> s{_utS3KeyPrefix = a});

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery.
utSNSTopicName :: Lens' UpdateTrail (Maybe Text)
utSNSTopicName = lens _utSNSTopicName (\ s a -> s{_utSNSTopicName = a});

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs will
-- be delivered. Not required unless you specify CloudWatchLogsRoleArn.
utCloudWatchLogsLogGroupARN :: Lens' UpdateTrail (Maybe Text)
utCloudWatchLogsLogGroupARN = lens _utCloudWatchLogsLogGroupARN (\ s a -> s{_utCloudWatchLogsLogGroupARN = a});

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
utIncludeGlobalServiceEvents :: Lens' UpdateTrail (Maybe Bool)
utIncludeGlobalServiceEvents = lens _utIncludeGlobalServiceEvents (\ s a -> s{_utIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user’s log group.
utCloudWatchLogsRoleARN :: Lens' UpdateTrail (Maybe Text)
utCloudWatchLogsRoleARN = lens _utCloudWatchLogsRoleARN (\ s a -> s{_utCloudWatchLogsRoleARN = a});

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utS3BucketName :: Lens' UpdateTrail (Maybe Text)
utS3BucketName = lens _utS3BucketName (\ s a -> s{_utS3BucketName = a});

-- | Specifies the name of the trail.
utName :: Lens' UpdateTrail Text
utName = lens _utName (\ s a -> s{_utName = a});

instance AWSRequest UpdateTrail where
        type Sv UpdateTrail = CloudTrail
        type Rs UpdateTrail = UpdateTrailResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTrailResponse' <$>
                   (x .?> "S3KeyPrefix") <*> (x .?> "SnsTopicName") <*>
                     (x .?> "CloudWatchLogsLogGroupArn")
                     <*> (x .?> "Name")
                     <*> (x .?> "IncludeGlobalServiceEvents")
                     <*> (x .?> "CloudWatchLogsRoleArn")
                     <*> (x .?> "S3BucketName"))

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
              ["S3KeyPrefix" .= _utS3KeyPrefix,
               "SnsTopicName" .= _utSNSTopicName,
               "CloudWatchLogsLogGroupArn" .=
                 _utCloudWatchLogsLogGroupARN,
               "IncludeGlobalServiceEvents" .=
                 _utIncludeGlobalServiceEvents,
               "CloudWatchLogsRoleArn" .= _utCloudWatchLogsRoleARN,
               "S3BucketName" .= _utS3BucketName, "Name" .= _utName]

instance ToPath UpdateTrail where
        toPath = const "/"

instance ToQuery UpdateTrail where
        toQuery = const mempty

-- | /See:/ 'updateTrailResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utrS3KeyPrefix'
--
-- * 'utrSNSTopicName'
--
-- * 'utrCloudWatchLogsLogGroupARN'
--
-- * 'utrName'
--
-- * 'utrIncludeGlobalServiceEvents'
--
-- * 'utrCloudWatchLogsRoleARN'
--
-- * 'utrS3BucketName'
data UpdateTrailResponse = UpdateTrailResponse'{_utrS3KeyPrefix :: Maybe Text, _utrSNSTopicName :: Maybe Text, _utrCloudWatchLogsLogGroupARN :: Maybe Text, _utrName :: Maybe Text, _utrIncludeGlobalServiceEvents :: Maybe Bool, _utrCloudWatchLogsRoleARN :: Maybe Text, _utrS3BucketName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'UpdateTrailResponse' smart constructor.
updateTrailResponse :: UpdateTrailResponse
updateTrailResponse = UpdateTrailResponse'{_utrS3KeyPrefix = Nothing, _utrSNSTopicName = Nothing, _utrCloudWatchLogsLogGroupARN = Nothing, _utrName = Nothing, _utrIncludeGlobalServiceEvents = Nothing, _utrCloudWatchLogsRoleARN = Nothing, _utrS3BucketName = Nothing};

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket
-- you have designated for log file delivery.
utrS3KeyPrefix :: Lens' UpdateTrailResponse (Maybe Text)
utrS3KeyPrefix = lens _utrS3KeyPrefix (\ s a -> s{_utrS3KeyPrefix = a});

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery.
utrSNSTopicName :: Lens' UpdateTrailResponse (Maybe Text)
utrSNSTopicName = lens _utrSNSTopicName (\ s a -> s{_utrSNSTopicName = a});

-- | Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs will be delivered.
utrCloudWatchLogsLogGroupARN :: Lens' UpdateTrailResponse (Maybe Text)
utrCloudWatchLogsLogGroupARN = lens _utrCloudWatchLogsLogGroupARN (\ s a -> s{_utrCloudWatchLogsLogGroupARN = a});

-- | Specifies the name of the trail.
utrName :: Lens' UpdateTrailResponse (Maybe Text)
utrName = lens _utrName (\ s a -> s{_utrName = a});

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
utrIncludeGlobalServiceEvents :: Lens' UpdateTrailResponse (Maybe Bool)
utrIncludeGlobalServiceEvents = lens _utrIncludeGlobalServiceEvents (\ s a -> s{_utrIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user’s log group.
utrCloudWatchLogsRoleARN :: Lens' UpdateTrailResponse (Maybe Text)
utrCloudWatchLogsRoleARN = lens _utrCloudWatchLogsRoleARN (\ s a -> s{_utrCloudWatchLogsRoleARN = a});

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utrS3BucketName :: Lens' UpdateTrailResponse (Maybe Text)
utrS3BucketName = lens _utrS3BucketName (\ s a -> s{_utrS3BucketName = a});
