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
-- Module      : Network.AWS.CloudTrail.GetTrailStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a JSON-formatted list of information about the specified trail. Fields include information on delivery errors, Amazon SNS and Amazon S3 errors, and start and stop logging times for each trail. This operation returns trail status from a single region. To return trail status from all regions, you must call the operation on each region.
--
--
module Network.AWS.CloudTrail.GetTrailStatus
    (
    -- * Creating a Request
      getTrailStatus
    , GetTrailStatus
    -- * Request Lenses
    , gtsName

    -- * Destructuring the Response
    , getTrailStatusResponse
    , GetTrailStatusResponse
    -- * Response Lenses
    , gtsrsTimeLoggingStopped
    , gtsrsLatestDeliveryError
    , gtsrsLatestDigestDeliveryTime
    , gtsrsLatestNotificationAttemptSucceeded
    , gtsrsStartLoggingTime
    , gtsrsLatestNotificationError
    , gtsrsLatestDeliveryAttemptSucceeded
    , gtsrsIsLogging
    , gtsrsTimeLoggingStarted
    , gtsrsLatestDigestDeliveryError
    , gtsrsLatestDeliveryAttemptTime
    , gtsrsLatestDeliveryTime
    , gtsrsLatestCloudWatchLogsDeliveryTime
    , gtsrsLatestCloudWatchLogsDeliveryError
    , gtsrsLatestNotificationTime
    , gtsrsLatestNotificationAttemptTime
    , gtsrsStopLoggingTime
    , gtsrsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The name of a trail about which you want the current status.
--
--
--
-- /See:/ 'getTrailStatus' smart constructor.
newtype GetTrailStatus = GetTrailStatus'
  { _gtsName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTrailStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsName' - Specifies the name or the CloudTrail ARN of the trail for which you are requesting status. To get the status of a shadow trail (a replication of the trail in another region), you must specify its ARN. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
getTrailStatus
    :: Text -- ^ 'gtsName'
    -> GetTrailStatus
getTrailStatus pName_ = GetTrailStatus' {_gtsName = pName_}


-- | Specifies the name or the CloudTrail ARN of the trail for which you are requesting status. To get the status of a shadow trail (a replication of the trail in another region), you must specify its ARN. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
gtsName :: Lens' GetTrailStatus Text
gtsName = lens _gtsName (\ s a -> s{_gtsName = a})

instance AWSRequest GetTrailStatus where
        type Rs GetTrailStatus = GetTrailStatusResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 GetTrailStatusResponse' <$>
                   (x .?> "TimeLoggingStopped") <*>
                     (x .?> "LatestDeliveryError")
                     <*> (x .?> "LatestDigestDeliveryTime")
                     <*> (x .?> "LatestNotificationAttemptSucceeded")
                     <*> (x .?> "StartLoggingTime")
                     <*> (x .?> "LatestNotificationError")
                     <*> (x .?> "LatestDeliveryAttemptSucceeded")
                     <*> (x .?> "IsLogging")
                     <*> (x .?> "TimeLoggingStarted")
                     <*> (x .?> "LatestDigestDeliveryError")
                     <*> (x .?> "LatestDeliveryAttemptTime")
                     <*> (x .?> "LatestDeliveryTime")
                     <*> (x .?> "LatestCloudWatchLogsDeliveryTime")
                     <*> (x .?> "LatestCloudWatchLogsDeliveryError")
                     <*> (x .?> "LatestNotificationTime")
                     <*> (x .?> "LatestNotificationAttemptTime")
                     <*> (x .?> "StopLoggingTime")
                     <*> (pure (fromEnum s)))

instance Hashable GetTrailStatus where

instance NFData GetTrailStatus where

instance ToHeaders GetTrailStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetTrailStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTrailStatus where
        toJSON GetTrailStatus'{..}
          = object (catMaybes [Just ("Name" .= _gtsName)])

instance ToPath GetTrailStatus where
        toPath = const "/"

instance ToQuery GetTrailStatus where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
--
--
-- /See:/ 'getTrailStatusResponse' smart constructor.
data GetTrailStatusResponse = GetTrailStatusResponse'
  { _gtsrsTimeLoggingStopped                 :: !(Maybe Text)
  , _gtsrsLatestDeliveryError                :: !(Maybe Text)
  , _gtsrsLatestDigestDeliveryTime           :: !(Maybe POSIX)
  , _gtsrsLatestNotificationAttemptSucceeded :: !(Maybe Text)
  , _gtsrsStartLoggingTime                   :: !(Maybe POSIX)
  , _gtsrsLatestNotificationError            :: !(Maybe Text)
  , _gtsrsLatestDeliveryAttemptSucceeded     :: !(Maybe Text)
  , _gtsrsIsLogging                          :: !(Maybe Bool)
  , _gtsrsTimeLoggingStarted                 :: !(Maybe Text)
  , _gtsrsLatestDigestDeliveryError          :: !(Maybe Text)
  , _gtsrsLatestDeliveryAttemptTime          :: !(Maybe Text)
  , _gtsrsLatestDeliveryTime                 :: !(Maybe POSIX)
  , _gtsrsLatestCloudWatchLogsDeliveryTime   :: !(Maybe POSIX)
  , _gtsrsLatestCloudWatchLogsDeliveryError  :: !(Maybe Text)
  , _gtsrsLatestNotificationTime             :: !(Maybe POSIX)
  , _gtsrsLatestNotificationAttemptTime      :: !(Maybe Text)
  , _gtsrsStopLoggingTime                    :: !(Maybe POSIX)
  , _gtsrsResponseStatus                     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTrailStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsrsTimeLoggingStopped' - This field is deprecated.
--
-- * 'gtsrsLatestDeliveryError' - Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver log files to the designated bucket. For more information see the topic <http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
--
-- * 'gtsrsLatestDigestDeliveryTime' - Specifies the date and time that CloudTrail last delivered a digest file to an account's Amazon S3 bucket.
--
-- * 'gtsrsLatestNotificationAttemptSucceeded' - This field is deprecated.
--
-- * 'gtsrsStartLoggingTime' - Specifies the most recent date and time when CloudTrail started recording API calls for an AWS account.
--
-- * 'gtsrsLatestNotificationError' - Displays any Amazon SNS error that CloudTrail encountered when attempting to send a notification. For more information about Amazon SNS errors, see the <http://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide> .
--
-- * 'gtsrsLatestDeliveryAttemptSucceeded' - This field is deprecated.
--
-- * 'gtsrsIsLogging' - Whether the CloudTrail is currently logging AWS API calls.
--
-- * 'gtsrsTimeLoggingStarted' - This field is deprecated.
--
-- * 'gtsrsLatestDigestDeliveryError' - Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver a digest file to the designated bucket. For more information see the topic <http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
--
-- * 'gtsrsLatestDeliveryAttemptTime' - This field is deprecated.
--
-- * 'gtsrsLatestDeliveryTime' - Specifies the date and time that CloudTrail last delivered log files to an account's Amazon S3 bucket.
--
-- * 'gtsrsLatestCloudWatchLogsDeliveryTime' - Displays the most recent date and time when CloudTrail delivered logs to CloudWatch Logs.
--
-- * 'gtsrsLatestCloudWatchLogsDeliveryError' - Displays any CloudWatch Logs error that CloudTrail encountered when attempting to deliver logs to CloudWatch Logs.
--
-- * 'gtsrsLatestNotificationTime' - Specifies the date and time of the most recent Amazon SNS notification that CloudTrail has written a new log file to an account's Amazon S3 bucket.
--
-- * 'gtsrsLatestNotificationAttemptTime' - This field is deprecated.
--
-- * 'gtsrsStopLoggingTime' - Specifies the most recent date and time when CloudTrail stopped recording API calls for an AWS account.
--
-- * 'gtsrsResponseStatus' - -- | The response status code.
getTrailStatusResponse
    :: Int -- ^ 'gtsrsResponseStatus'
    -> GetTrailStatusResponse
getTrailStatusResponse pResponseStatus_ =
  GetTrailStatusResponse'
    { _gtsrsTimeLoggingStopped = Nothing
    , _gtsrsLatestDeliveryError = Nothing
    , _gtsrsLatestDigestDeliveryTime = Nothing
    , _gtsrsLatestNotificationAttemptSucceeded = Nothing
    , _gtsrsStartLoggingTime = Nothing
    , _gtsrsLatestNotificationError = Nothing
    , _gtsrsLatestDeliveryAttemptSucceeded = Nothing
    , _gtsrsIsLogging = Nothing
    , _gtsrsTimeLoggingStarted = Nothing
    , _gtsrsLatestDigestDeliveryError = Nothing
    , _gtsrsLatestDeliveryAttemptTime = Nothing
    , _gtsrsLatestDeliveryTime = Nothing
    , _gtsrsLatestCloudWatchLogsDeliveryTime = Nothing
    , _gtsrsLatestCloudWatchLogsDeliveryError = Nothing
    , _gtsrsLatestNotificationTime = Nothing
    , _gtsrsLatestNotificationAttemptTime = Nothing
    , _gtsrsStopLoggingTime = Nothing
    , _gtsrsResponseStatus = pResponseStatus_
    }


-- | This field is deprecated.
gtsrsTimeLoggingStopped :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsTimeLoggingStopped = lens _gtsrsTimeLoggingStopped (\ s a -> s{_gtsrsTimeLoggingStopped = a})

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver log files to the designated bucket. For more information see the topic <http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
gtsrsLatestDeliveryError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsLatestDeliveryError = lens _gtsrsLatestDeliveryError (\ s a -> s{_gtsrsLatestDeliveryError = a})

-- | Specifies the date and time that CloudTrail last delivered a digest file to an account's Amazon S3 bucket.
gtsrsLatestDigestDeliveryTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrsLatestDigestDeliveryTime = lens _gtsrsLatestDigestDeliveryTime (\ s a -> s{_gtsrsLatestDigestDeliveryTime = a}) . mapping _Time

-- | This field is deprecated.
gtsrsLatestNotificationAttemptSucceeded :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsLatestNotificationAttemptSucceeded = lens _gtsrsLatestNotificationAttemptSucceeded (\ s a -> s{_gtsrsLatestNotificationAttemptSucceeded = a})

-- | Specifies the most recent date and time when CloudTrail started recording API calls for an AWS account.
gtsrsStartLoggingTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrsStartLoggingTime = lens _gtsrsStartLoggingTime (\ s a -> s{_gtsrsStartLoggingTime = a}) . mapping _Time

-- | Displays any Amazon SNS error that CloudTrail encountered when attempting to send a notification. For more information about Amazon SNS errors, see the <http://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide> .
gtsrsLatestNotificationError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsLatestNotificationError = lens _gtsrsLatestNotificationError (\ s a -> s{_gtsrsLatestNotificationError = a})

-- | This field is deprecated.
gtsrsLatestDeliveryAttemptSucceeded :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsLatestDeliveryAttemptSucceeded = lens _gtsrsLatestDeliveryAttemptSucceeded (\ s a -> s{_gtsrsLatestDeliveryAttemptSucceeded = a})

-- | Whether the CloudTrail is currently logging AWS API calls.
gtsrsIsLogging :: Lens' GetTrailStatusResponse (Maybe Bool)
gtsrsIsLogging = lens _gtsrsIsLogging (\ s a -> s{_gtsrsIsLogging = a})

-- | This field is deprecated.
gtsrsTimeLoggingStarted :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsTimeLoggingStarted = lens _gtsrsTimeLoggingStarted (\ s a -> s{_gtsrsTimeLoggingStarted = a})

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting to deliver a digest file to the designated bucket. For more information see the topic <http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
gtsrsLatestDigestDeliveryError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsLatestDigestDeliveryError = lens _gtsrsLatestDigestDeliveryError (\ s a -> s{_gtsrsLatestDigestDeliveryError = a})

-- | This field is deprecated.
gtsrsLatestDeliveryAttemptTime :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsLatestDeliveryAttemptTime = lens _gtsrsLatestDeliveryAttemptTime (\ s a -> s{_gtsrsLatestDeliveryAttemptTime = a})

-- | Specifies the date and time that CloudTrail last delivered log files to an account's Amazon S3 bucket.
gtsrsLatestDeliveryTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrsLatestDeliveryTime = lens _gtsrsLatestDeliveryTime (\ s a -> s{_gtsrsLatestDeliveryTime = a}) . mapping _Time

-- | Displays the most recent date and time when CloudTrail delivered logs to CloudWatch Logs.
gtsrsLatestCloudWatchLogsDeliveryTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrsLatestCloudWatchLogsDeliveryTime = lens _gtsrsLatestCloudWatchLogsDeliveryTime (\ s a -> s{_gtsrsLatestCloudWatchLogsDeliveryTime = a}) . mapping _Time

-- | Displays any CloudWatch Logs error that CloudTrail encountered when attempting to deliver logs to CloudWatch Logs.
gtsrsLatestCloudWatchLogsDeliveryError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsLatestCloudWatchLogsDeliveryError = lens _gtsrsLatestCloudWatchLogsDeliveryError (\ s a -> s{_gtsrsLatestCloudWatchLogsDeliveryError = a})

-- | Specifies the date and time of the most recent Amazon SNS notification that CloudTrail has written a new log file to an account's Amazon S3 bucket.
gtsrsLatestNotificationTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrsLatestNotificationTime = lens _gtsrsLatestNotificationTime (\ s a -> s{_gtsrsLatestNotificationTime = a}) . mapping _Time

-- | This field is deprecated.
gtsrsLatestNotificationAttemptTime :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrsLatestNotificationAttemptTime = lens _gtsrsLatestNotificationAttemptTime (\ s a -> s{_gtsrsLatestNotificationAttemptTime = a})

-- | Specifies the most recent date and time when CloudTrail stopped recording API calls for an AWS account.
gtsrsStopLoggingTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrsStopLoggingTime = lens _gtsrsStopLoggingTime (\ s a -> s{_gtsrsStopLoggingTime = a}) . mapping _Time

-- | -- | The response status code.
gtsrsResponseStatus :: Lens' GetTrailStatusResponse Int
gtsrsResponseStatus = lens _gtsrsResponseStatus (\ s a -> s{_gtsrsResponseStatus = a})

instance NFData GetTrailStatusResponse where
