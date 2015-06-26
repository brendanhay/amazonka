{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudTrail.CreateTrail
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

-- | From the command line, use @create-subscription@.
--
-- Creates a trail that specifies the settings for delivery of log data to
-- an Amazon S3 bucket.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_CreateTrail.html>
module Network.AWS.CloudTrail.CreateTrail
    (
    -- * Request
      CreateTrail
    -- ** Request constructor
    , createTrail
    -- ** Request lenses
    , ctS3KeyPrefix
    , ctSNSTopicName
    , ctCloudWatchLogsLogGroupARN
    , ctIncludeGlobalServiceEvents
    , ctCloudWatchLogsRoleARN
    , ctName
    , ctS3BucketName

    -- * Response
    , CreateTrailResponse
    -- ** Response constructor
    , createTrailResponse
    -- ** Response lenses
    , ctrS3KeyPrefix
    , ctrSNSTopicName
    , ctrCloudWatchLogsLogGroupARN
    , ctrName
    , ctrIncludeGlobalServiceEvents
    , ctrCloudWatchLogsRoleARN
    , ctrS3BucketName
    , ctrStatusCode
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Specifies the settings for each trail.
--
-- /See:/ 'createTrail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctS3KeyPrefix'
--
-- * 'ctSNSTopicName'
--
-- * 'ctCloudWatchLogsLogGroupARN'
--
-- * 'ctIncludeGlobalServiceEvents'
--
-- * 'ctCloudWatchLogsRoleARN'
--
-- * 'ctName'
--
-- * 'ctS3BucketName'
data CreateTrail = CreateTrail'{_ctS3KeyPrefix :: Maybe Text, _ctSNSTopicName :: Maybe Text, _ctCloudWatchLogsLogGroupARN :: Maybe Text, _ctIncludeGlobalServiceEvents :: Maybe Bool, _ctCloudWatchLogsRoleARN :: Maybe Text, _ctName :: Text, _ctS3BucketName :: Text} deriving (Eq, Read, Show)

-- | 'CreateTrail' smart constructor.
createTrail :: Text -> Text -> CreateTrail
createTrail pName pS3BucketName = CreateTrail'{_ctS3KeyPrefix = Nothing, _ctSNSTopicName = Nothing, _ctCloudWatchLogsLogGroupARN = Nothing, _ctIncludeGlobalServiceEvents = Nothing, _ctCloudWatchLogsRoleARN = Nothing, _ctName = pName, _ctS3BucketName = pS3BucketName};

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket
-- you have designated for log file delivery.
ctS3KeyPrefix :: Lens' CreateTrail (Maybe Text)
ctS3KeyPrefix = lens _ctS3KeyPrefix (\ s a -> s{_ctS3KeyPrefix = a});

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery.
ctSNSTopicName :: Lens' CreateTrail (Maybe Text)
ctSNSTopicName = lens _ctSNSTopicName (\ s a -> s{_ctSNSTopicName = a});

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs will
-- be delivered. Not required unless you specify CloudWatchLogsRoleArn.
ctCloudWatchLogsLogGroupARN :: Lens' CreateTrail (Maybe Text)
ctCloudWatchLogsLogGroupARN = lens _ctCloudWatchLogsLogGroupARN (\ s a -> s{_ctCloudWatchLogsLogGroupARN = a});

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
ctIncludeGlobalServiceEvents :: Lens' CreateTrail (Maybe Bool)
ctIncludeGlobalServiceEvents = lens _ctIncludeGlobalServiceEvents (\ s a -> s{_ctIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user’s log group.
ctCloudWatchLogsRoleARN :: Lens' CreateTrail (Maybe Text)
ctCloudWatchLogsRoleARN = lens _ctCloudWatchLogsRoleARN (\ s a -> s{_ctCloudWatchLogsRoleARN = a});

-- | Specifies the name of the trail.
ctName :: Lens' CreateTrail Text
ctName = lens _ctName (\ s a -> s{_ctName = a});

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctS3BucketName :: Lens' CreateTrail Text
ctS3BucketName = lens _ctS3BucketName (\ s a -> s{_ctS3BucketName = a});

instance AWSRequest CreateTrail where
        type Sv CreateTrail = CloudTrail
        type Rs CreateTrail = CreateTrailResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateTrailResponse' <$>
                   (x .?> "S3KeyPrefix") <*> (x .?> "SnsTopicName") <*>
                     (x .?> "CloudWatchLogsLogGroupArn")
                     <*> (x .?> "Name")
                     <*> (x .?> "IncludeGlobalServiceEvents")
                     <*> (x .?> "CloudWatchLogsRoleArn")
                     <*> (x .?> "S3BucketName")
                     <*> (pure (fromEnum s)))

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
              ["S3KeyPrefix" .= _ctS3KeyPrefix,
               "SnsTopicName" .= _ctSNSTopicName,
               "CloudWatchLogsLogGroupArn" .=
                 _ctCloudWatchLogsLogGroupARN,
               "IncludeGlobalServiceEvents" .=
                 _ctIncludeGlobalServiceEvents,
               "CloudWatchLogsRoleArn" .= _ctCloudWatchLogsRoleARN,
               "Name" .= _ctName, "S3BucketName" .= _ctS3BucketName]

instance ToPath CreateTrail where
        toPath = const "/"

instance ToQuery CreateTrail where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'createTrailResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrS3KeyPrefix'
--
-- * 'ctrSNSTopicName'
--
-- * 'ctrCloudWatchLogsLogGroupARN'
--
-- * 'ctrName'
--
-- * 'ctrIncludeGlobalServiceEvents'
--
-- * 'ctrCloudWatchLogsRoleARN'
--
-- * 'ctrS3BucketName'
--
-- * 'ctrStatusCode'
data CreateTrailResponse = CreateTrailResponse'{_ctrS3KeyPrefix :: Maybe Text, _ctrSNSTopicName :: Maybe Text, _ctrCloudWatchLogsLogGroupARN :: Maybe Text, _ctrName :: Maybe Text, _ctrIncludeGlobalServiceEvents :: Maybe Bool, _ctrCloudWatchLogsRoleARN :: Maybe Text, _ctrS3BucketName :: Maybe Text, _ctrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'CreateTrailResponse' smart constructor.
createTrailResponse :: Int -> CreateTrailResponse
createTrailResponse pStatusCode = CreateTrailResponse'{_ctrS3KeyPrefix = Nothing, _ctrSNSTopicName = Nothing, _ctrCloudWatchLogsLogGroupARN = Nothing, _ctrName = Nothing, _ctrIncludeGlobalServiceEvents = Nothing, _ctrCloudWatchLogsRoleARN = Nothing, _ctrS3BucketName = Nothing, _ctrStatusCode = pStatusCode};

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket
-- you have designated for log file delivery.
ctrS3KeyPrefix :: Lens' CreateTrailResponse (Maybe Text)
ctrS3KeyPrefix = lens _ctrS3KeyPrefix (\ s a -> s{_ctrS3KeyPrefix = a});

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery.
ctrSNSTopicName :: Lens' CreateTrailResponse (Maybe Text)
ctrSNSTopicName = lens _ctrSNSTopicName (\ s a -> s{_ctrSNSTopicName = a});

-- | Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs will be delivered.
ctrCloudWatchLogsLogGroupARN :: Lens' CreateTrailResponse (Maybe Text)
ctrCloudWatchLogsLogGroupARN = lens _ctrCloudWatchLogsLogGroupARN (\ s a -> s{_ctrCloudWatchLogsLogGroupARN = a});

-- | Specifies the name of the trail.
ctrName :: Lens' CreateTrailResponse (Maybe Text)
ctrName = lens _ctrName (\ s a -> s{_ctrName = a});

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
ctrIncludeGlobalServiceEvents :: Lens' CreateTrailResponse (Maybe Bool)
ctrIncludeGlobalServiceEvents = lens _ctrIncludeGlobalServiceEvents (\ s a -> s{_ctrIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user’s log group.
ctrCloudWatchLogsRoleARN :: Lens' CreateTrailResponse (Maybe Text)
ctrCloudWatchLogsRoleARN = lens _ctrCloudWatchLogsRoleARN (\ s a -> s{_ctrCloudWatchLogsRoleARN = a});

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctrS3BucketName :: Lens' CreateTrailResponse (Maybe Text)
ctrS3BucketName = lens _ctrS3BucketName (\ s a -> s{_ctrS3BucketName = a});

-- | FIXME: Undocumented member.
ctrStatusCode :: Lens' CreateTrailResponse Int
ctrStatusCode = lens _ctrStatusCode (\ s a -> s{_ctrStatusCode = a});
