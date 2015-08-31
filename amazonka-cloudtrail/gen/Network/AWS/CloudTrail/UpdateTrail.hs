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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From the command line, use 'update-subscription'.
--
-- Updates the settings that specify delivery of log files. Changes to a
-- trail do not require stopping the CloudTrail service. Use this action to
-- designate an existing bucket for log delivery. If the existing bucket
-- has previously been a target for CloudTrail log files, an IAM policy
-- exists for the bucket.
--
-- /See:/ <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_UpdateTrail.html AWS API Reference> for UpdateTrail.
module Network.AWS.CloudTrail.UpdateTrail
    (
    -- * Creating a Request
      updateTrail
    , UpdateTrail
    -- * Request Lenses
    , utS3KeyPrefix
    , utSNSTopicName
    , utCloudWatchLogsLogGroupARN
    , utIncludeGlobalServiceEvents
    , utCloudWatchLogsRoleARN
    , utS3BucketName
    , utName

    -- * Destructuring the Response
    , updateTrailResponse
    , UpdateTrailResponse
    -- * Response Lenses
    , utrsS3KeyPrefix
    , utrsSNSTopicName
    , utrsCloudWatchLogsLogGroupARN
    , utrsName
    , utrsIncludeGlobalServiceEvents
    , utrsCloudWatchLogsRoleARN
    , utrsS3BucketName
    , utrsResponseStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.CloudTrail.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Specifies settings to update for the trail.
--
-- /See:/ 'updateTrail' smart constructor.
data UpdateTrail = UpdateTrail'
    { _utS3KeyPrefix                :: !(Maybe Text)
    , _utSNSTopicName               :: !(Maybe Text)
    , _utCloudWatchLogsLogGroupARN  :: !(Maybe Text)
    , _utIncludeGlobalServiceEvents :: !(Maybe Bool)
    , _utCloudWatchLogsRoleARN      :: !(Maybe Text)
    , _utS3BucketName               :: !(Maybe Text)
    , _utName                       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateTrail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
updateTrail
    :: Text -- ^ 'utName'
    -> UpdateTrail
updateTrail pName_ =
    UpdateTrail'
    { _utS3KeyPrefix = Nothing
    , _utSNSTopicName = Nothing
    , _utCloudWatchLogsLogGroupARN = Nothing
    , _utIncludeGlobalServiceEvents = Nothing
    , _utCloudWatchLogsRoleARN = Nothing
    , _utS3BucketName = Nothing
    , _utName = pName_
    }

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
        type Rs UpdateTrail = UpdateTrailResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTrailResponse' <$>
                   (x .?> "S3KeyPrefix") <*> (x .?> "SnsTopicName") <*>
                     (x .?> "CloudWatchLogsLogGroupArn")
                     <*> (x .?> "Name")
                     <*> (x .?> "IncludeGlobalServiceEvents")
                     <*> (x .?> "CloudWatchLogsRoleArn")
                     <*> (x .?> "S3BucketName")
                     <*> (pure (fromEnum s)))

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
                  ("CloudWatchLogsLogGroupArn" .=) <$>
                    _utCloudWatchLogsLogGroupARN,
                  ("IncludeGlobalServiceEvents" .=) <$>
                    _utIncludeGlobalServiceEvents,
                  ("CloudWatchLogsRoleArn" .=) <$>
                    _utCloudWatchLogsRoleARN,
                  ("S3BucketName" .=) <$> _utS3BucketName,
                  Just ("Name" .= _utName)])

instance ToPath UpdateTrail where
        toPath = const "/"

instance ToQuery UpdateTrail where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'updateTrailResponse' smart constructor.
data UpdateTrailResponse = UpdateTrailResponse'
    { _utrsS3KeyPrefix                :: !(Maybe Text)
    , _utrsSNSTopicName               :: !(Maybe Text)
    , _utrsCloudWatchLogsLogGroupARN  :: !(Maybe Text)
    , _utrsName                       :: !(Maybe Text)
    , _utrsIncludeGlobalServiceEvents :: !(Maybe Bool)
    , _utrsCloudWatchLogsRoleARN      :: !(Maybe Text)
    , _utrsS3BucketName               :: !(Maybe Text)
    , _utrsResponseStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateTrailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrsS3KeyPrefix'
--
-- * 'utrsSNSTopicName'
--
-- * 'utrsCloudWatchLogsLogGroupARN'
--
-- * 'utrsName'
--
-- * 'utrsIncludeGlobalServiceEvents'
--
-- * 'utrsCloudWatchLogsRoleARN'
--
-- * 'utrsS3BucketName'
--
-- * 'utrsResponseStatus'
updateTrailResponse
    :: Int -- ^ 'utrsResponseStatus'
    -> UpdateTrailResponse
updateTrailResponse pResponseStatus_ =
    UpdateTrailResponse'
    { _utrsS3KeyPrefix = Nothing
    , _utrsSNSTopicName = Nothing
    , _utrsCloudWatchLogsLogGroupARN = Nothing
    , _utrsName = Nothing
    , _utrsIncludeGlobalServiceEvents = Nothing
    , _utrsCloudWatchLogsRoleARN = Nothing
    , _utrsS3BucketName = Nothing
    , _utrsResponseStatus = pResponseStatus_
    }

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket
-- you have designated for log file delivery.
utrsS3KeyPrefix :: Lens' UpdateTrailResponse (Maybe Text)
utrsS3KeyPrefix = lens _utrsS3KeyPrefix (\ s a -> s{_utrsS3KeyPrefix = a});

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery.
utrsSNSTopicName :: Lens' UpdateTrailResponse (Maybe Text)
utrsSNSTopicName = lens _utrsSNSTopicName (\ s a -> s{_utrsSNSTopicName = a});

-- | Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs will be delivered.
utrsCloudWatchLogsLogGroupARN :: Lens' UpdateTrailResponse (Maybe Text)
utrsCloudWatchLogsLogGroupARN = lens _utrsCloudWatchLogsLogGroupARN (\ s a -> s{_utrsCloudWatchLogsLogGroupARN = a});

-- | Specifies the name of the trail.
utrsName :: Lens' UpdateTrailResponse (Maybe Text)
utrsName = lens _utrsName (\ s a -> s{_utrsName = a});

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
utrsIncludeGlobalServiceEvents :: Lens' UpdateTrailResponse (Maybe Bool)
utrsIncludeGlobalServiceEvents = lens _utrsIncludeGlobalServiceEvents (\ s a -> s{_utrsIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user’s log group.
utrsCloudWatchLogsRoleARN :: Lens' UpdateTrailResponse (Maybe Text)
utrsCloudWatchLogsRoleARN = lens _utrsCloudWatchLogsRoleARN (\ s a -> s{_utrsCloudWatchLogsRoleARN = a});

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utrsS3BucketName :: Lens' UpdateTrailResponse (Maybe Text)
utrsS3BucketName = lens _utrsS3BucketName (\ s a -> s{_utrsS3BucketName = a});

-- | The response status code.
utrsResponseStatus :: Lens' UpdateTrailResponse Int
utrsResponseStatus = lens _utrsResponseStatus (\ s a -> s{_utrsResponseStatus = a});
