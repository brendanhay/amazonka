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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From the command line, use 'create-subscription'.
--
-- Creates a trail that specifies the settings for delivery of log data to
-- an Amazon S3 bucket.
--
-- /See:/ <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_CreateTrail.html AWS API Reference> for CreateTrail.
module Network.AWS.CloudTrail.CreateTrail
    (
    -- * Creating a Request
      createTrail
    , CreateTrail
    -- * Request Lenses
    , ctS3KeyPrefix
    , ctSNSTopicName
    , ctCloudWatchLogsLogGroupARN
    , ctIncludeGlobalServiceEvents
    , ctCloudWatchLogsRoleARN
    , ctName
    , ctS3BucketName

    -- * Destructuring the Response
    , createTrailResponse
    , CreateTrailResponse
    -- * Response Lenses
    , ctrsS3KeyPrefix
    , ctrsSNSTopicName
    , ctrsCloudWatchLogsLogGroupARN
    , ctrsName
    , ctrsIncludeGlobalServiceEvents
    , ctrsCloudWatchLogsRoleARN
    , ctrsS3BucketName
    , ctrsResponseStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.CloudTrail.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Specifies the settings for each trail.
--
-- /See:/ 'createTrail' smart constructor.
data CreateTrail = CreateTrail'
    { _ctS3KeyPrefix                :: !(Maybe Text)
    , _ctSNSTopicName               :: !(Maybe Text)
    , _ctCloudWatchLogsLogGroupARN  :: !(Maybe Text)
    , _ctIncludeGlobalServiceEvents :: !(Maybe Bool)
    , _ctCloudWatchLogsRoleARN      :: !(Maybe Text)
    , _ctName                       :: !Text
    , _ctS3BucketName               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateTrail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
createTrail
    :: Text -- ^ 'ctName'
    -> Text -- ^ 'ctS3BucketName'
    -> CreateTrail
createTrail pName_ pS3BucketName_ =
    CreateTrail'
    { _ctS3KeyPrefix = Nothing
    , _ctSNSTopicName = Nothing
    , _ctCloudWatchLogsLogGroupARN = Nothing
    , _ctIncludeGlobalServiceEvents = Nothing
    , _ctCloudWatchLogsRoleARN = Nothing
    , _ctName = pName_
    , _ctS3BucketName = pS3BucketName_
    }

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
        type Rs CreateTrail = CreateTrailResponse
        request = postJSON cloudTrail
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
              (catMaybes
                 [("S3KeyPrefix" .=) <$> _ctS3KeyPrefix,
                  ("SnsTopicName" .=) <$> _ctSNSTopicName,
                  ("CloudWatchLogsLogGroupArn" .=) <$>
                    _ctCloudWatchLogsLogGroupARN,
                  ("IncludeGlobalServiceEvents" .=) <$>
                    _ctIncludeGlobalServiceEvents,
                  ("CloudWatchLogsRoleArn" .=) <$>
                    _ctCloudWatchLogsRoleARN,
                  Just ("Name" .= _ctName),
                  Just ("S3BucketName" .= _ctS3BucketName)])

instance ToPath CreateTrail where
        toPath = const "/"

instance ToQuery CreateTrail where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'createTrailResponse' smart constructor.
data CreateTrailResponse = CreateTrailResponse'
    { _ctrsS3KeyPrefix                :: !(Maybe Text)
    , _ctrsSNSTopicName               :: !(Maybe Text)
    , _ctrsCloudWatchLogsLogGroupARN  :: !(Maybe Text)
    , _ctrsName                       :: !(Maybe Text)
    , _ctrsIncludeGlobalServiceEvents :: !(Maybe Bool)
    , _ctrsCloudWatchLogsRoleARN      :: !(Maybe Text)
    , _ctrsS3BucketName               :: !(Maybe Text)
    , _ctrsResponseStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateTrailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsS3KeyPrefix'
--
-- * 'ctrsSNSTopicName'
--
-- * 'ctrsCloudWatchLogsLogGroupARN'
--
-- * 'ctrsName'
--
-- * 'ctrsIncludeGlobalServiceEvents'
--
-- * 'ctrsCloudWatchLogsRoleARN'
--
-- * 'ctrsS3BucketName'
--
-- * 'ctrsResponseStatus'
createTrailResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateTrailResponse
createTrailResponse pResponseStatus_ =
    CreateTrailResponse'
    { _ctrsS3KeyPrefix = Nothing
    , _ctrsSNSTopicName = Nothing
    , _ctrsCloudWatchLogsLogGroupARN = Nothing
    , _ctrsName = Nothing
    , _ctrsIncludeGlobalServiceEvents = Nothing
    , _ctrsCloudWatchLogsRoleARN = Nothing
    , _ctrsS3BucketName = Nothing
    , _ctrsResponseStatus = pResponseStatus_
    }

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket
-- you have designated for log file delivery.
ctrsS3KeyPrefix :: Lens' CreateTrailResponse (Maybe Text)
ctrsS3KeyPrefix = lens _ctrsS3KeyPrefix (\ s a -> s{_ctrsS3KeyPrefix = a});

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery.
ctrsSNSTopicName :: Lens' CreateTrailResponse (Maybe Text)
ctrsSNSTopicName = lens _ctrsSNSTopicName (\ s a -> s{_ctrsSNSTopicName = a});

-- | Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs will be delivered.
ctrsCloudWatchLogsLogGroupARN :: Lens' CreateTrailResponse (Maybe Text)
ctrsCloudWatchLogsLogGroupARN = lens _ctrsCloudWatchLogsLogGroupARN (\ s a -> s{_ctrsCloudWatchLogsLogGroupARN = a});

-- | Specifies the name of the trail.
ctrsName :: Lens' CreateTrailResponse (Maybe Text)
ctrsName = lens _ctrsName (\ s a -> s{_ctrsName = a});

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
ctrsIncludeGlobalServiceEvents :: Lens' CreateTrailResponse (Maybe Bool)
ctrsIncludeGlobalServiceEvents = lens _ctrsIncludeGlobalServiceEvents (\ s a -> s{_ctrsIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user’s log group.
ctrsCloudWatchLogsRoleARN :: Lens' CreateTrailResponse (Maybe Text)
ctrsCloudWatchLogsRoleARN = lens _ctrsCloudWatchLogsRoleARN (\ s a -> s{_ctrsCloudWatchLogsRoleARN = a});

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctrsS3BucketName :: Lens' CreateTrailResponse (Maybe Text)
ctrsS3BucketName = lens _ctrsS3BucketName (\ s a -> s{_ctrsS3BucketName = a});

-- | The response status code.
ctrsResponseStatus :: Lens' CreateTrailResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a});
