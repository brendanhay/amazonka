{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.CreateTrail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- From the command line, use @create-subscription@.
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
    , ctrqS3KeyPrefix
    , ctrqSNSTopicName
    , ctrqCloudWatchLogsLogGroupARN
    , ctrqIncludeGlobalServiceEvents
    , ctrqCloudWatchLogsRoleARN
    , ctrqName
    , ctrqS3BucketName

    -- * Response
    , CreateTrailResponse
    -- ** Response constructor
    , createTrailResponse
    -- ** Response lenses
    , ctrsS3KeyPrefix
    , ctrsSNSTopicName
    , ctrsCloudWatchLogsLogGroupARN
    , ctrsName
    , ctrsIncludeGlobalServiceEvents
    , ctrsCloudWatchLogsRoleARN
    , ctrsS3BucketName
    , ctrsStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Specifies the settings for each trail.
--
-- /See:/ 'createTrail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrqS3KeyPrefix'
--
-- * 'ctrqSNSTopicName'
--
-- * 'ctrqCloudWatchLogsLogGroupARN'
--
-- * 'ctrqIncludeGlobalServiceEvents'
--
-- * 'ctrqCloudWatchLogsRoleARN'
--
-- * 'ctrqName'
--
-- * 'ctrqS3BucketName'
data CreateTrail = CreateTrail'
    { _ctrqS3KeyPrefix                :: !(Maybe Text)
    , _ctrqSNSTopicName               :: !(Maybe Text)
    , _ctrqCloudWatchLogsLogGroupARN  :: !(Maybe Text)
    , _ctrqIncludeGlobalServiceEvents :: !(Maybe Bool)
    , _ctrqCloudWatchLogsRoleARN      :: !(Maybe Text)
    , _ctrqName                       :: !Text
    , _ctrqS3BucketName               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTrail' smart constructor.
createTrail :: Text -> Text -> CreateTrail
createTrail pName_ pS3BucketName_ =
    CreateTrail'
    { _ctrqS3KeyPrefix = Nothing
    , _ctrqSNSTopicName = Nothing
    , _ctrqCloudWatchLogsLogGroupARN = Nothing
    , _ctrqIncludeGlobalServiceEvents = Nothing
    , _ctrqCloudWatchLogsRoleARN = Nothing
    , _ctrqName = pName_
    , _ctrqS3BucketName = pS3BucketName_
    }

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket
-- you have designated for log file delivery.
ctrqS3KeyPrefix :: Lens' CreateTrail (Maybe Text)
ctrqS3KeyPrefix = lens _ctrqS3KeyPrefix (\ s a -> s{_ctrqS3KeyPrefix = a});

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery.
ctrqSNSTopicName :: Lens' CreateTrail (Maybe Text)
ctrqSNSTopicName = lens _ctrqSNSTopicName (\ s a -> s{_ctrqSNSTopicName = a});

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs will
-- be delivered. Not required unless you specify CloudWatchLogsRoleArn.
ctrqCloudWatchLogsLogGroupARN :: Lens' CreateTrail (Maybe Text)
ctrqCloudWatchLogsLogGroupARN = lens _ctrqCloudWatchLogsLogGroupARN (\ s a -> s{_ctrqCloudWatchLogsLogGroupARN = a});

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
ctrqIncludeGlobalServiceEvents :: Lens' CreateTrail (Maybe Bool)
ctrqIncludeGlobalServiceEvents = lens _ctrqIncludeGlobalServiceEvents (\ s a -> s{_ctrqIncludeGlobalServiceEvents = a});

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write
-- to a user’s log group.
ctrqCloudWatchLogsRoleARN :: Lens' CreateTrail (Maybe Text)
ctrqCloudWatchLogsRoleARN = lens _ctrqCloudWatchLogsRoleARN (\ s a -> s{_ctrqCloudWatchLogsRoleARN = a});

-- | Specifies the name of the trail.
ctrqName :: Lens' CreateTrail Text
ctrqName = lens _ctrqName (\ s a -> s{_ctrqName = a});

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctrqS3BucketName :: Lens' CreateTrail Text
ctrqS3BucketName = lens _ctrqS3BucketName (\ s a -> s{_ctrqS3BucketName = a});

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
              ["S3KeyPrefix" .= _ctrqS3KeyPrefix,
               "SnsTopicName" .= _ctrqSNSTopicName,
               "CloudWatchLogsLogGroupArn" .=
                 _ctrqCloudWatchLogsLogGroupARN,
               "IncludeGlobalServiceEvents" .=
                 _ctrqIncludeGlobalServiceEvents,
               "CloudWatchLogsRoleArn" .=
                 _ctrqCloudWatchLogsRoleARN,
               "Name" .= _ctrqName,
               "S3BucketName" .= _ctrqS3BucketName]

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
-- * 'ctrsStatus'
data CreateTrailResponse = CreateTrailResponse'
    { _ctrsS3KeyPrefix                :: !(Maybe Text)
    , _ctrsSNSTopicName               :: !(Maybe Text)
    , _ctrsCloudWatchLogsLogGroupARN  :: !(Maybe Text)
    , _ctrsName                       :: !(Maybe Text)
    , _ctrsIncludeGlobalServiceEvents :: !(Maybe Bool)
    , _ctrsCloudWatchLogsRoleARN      :: !(Maybe Text)
    , _ctrsS3BucketName               :: !(Maybe Text)
    , _ctrsStatus                     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateTrailResponse' smart constructor.
createTrailResponse :: Int -> CreateTrailResponse
createTrailResponse pStatus_ =
    CreateTrailResponse'
    { _ctrsS3KeyPrefix = Nothing
    , _ctrsSNSTopicName = Nothing
    , _ctrsCloudWatchLogsLogGroupARN = Nothing
    , _ctrsName = Nothing
    , _ctrsIncludeGlobalServiceEvents = Nothing
    , _ctrsCloudWatchLogsRoleARN = Nothing
    , _ctrsS3BucketName = Nothing
    , _ctrsStatus = pStatus_
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

-- | FIXME: Undocumented member.
ctrsStatus :: Lens' CreateTrailResponse Int
ctrsStatus = lens _ctrsStatus (\ s a -> s{_ctrsStatus = a});
