{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudTrail.GetTrailStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a JSON-formatted list of information about the specified trail.
-- Fields include information on delivery errors, Amazon SNS and Amazon S3
-- errors, and start and stop logging times for each trail.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_GetTrailStatus.html>
module Network.AWS.CloudTrail.GetTrailStatus
    (
    -- * Request
      GetTrailStatus
    -- ** Request constructor
    , getTrailStatus
    -- ** Request lenses
    , gtsName

    -- * Response
    , GetTrailStatusResponse
    -- ** Response constructor
    , getTrailStatusResponse
    -- ** Response lenses
    , gtsrLatestDeliveryError
    , gtsrStartLoggingTime
    , gtsrLatestNotificationError
    , gtsrIsLogging
    , gtsrLatestDeliveryTime
    , gtsrLatestCloudWatchLogsDeliveryTime
    , gtsrLatestCloudWatchLogsDeliveryError
    , gtsrLatestNotificationTime
    , gtsrStopLoggingTime
    , gtsrStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The name of a trail about which you want the current status.
--
-- /See:/ 'getTrailStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtsName'
newtype GetTrailStatus = GetTrailStatus'
    { _gtsName :: Text
    } deriving (Eq,Read,Show)

-- | 'GetTrailStatus' smart constructor.
getTrailStatus :: Text -> GetTrailStatus
getTrailStatus pName =
    GetTrailStatus'
    { _gtsName = pName
    }

-- | The name of the trail for which you are requesting the current status.
gtsName :: Lens' GetTrailStatus Text
gtsName = lens _gtsName (\ s a -> s{_gtsName = a});

instance AWSRequest GetTrailStatus where
        type Sv GetTrailStatus = CloudTrail
        type Rs GetTrailStatus = GetTrailStatusResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetTrailStatusResponse' <$>
                   (x .?> "LatestDeliveryError") <*>
                     (x .?> "StartLoggingTime")
                     <*> (x .?> "LatestNotificationError")
                     <*> (x .?> "IsLogging")
                     <*> (x .?> "LatestDeliveryTime")
                     <*> (x .?> "LatestCloudWatchLogsDeliveryTime")
                     <*> (x .?> "LatestCloudWatchLogsDeliveryError")
                     <*> (x .?> "LatestNotificationTime")
                     <*> (x .?> "StopLoggingTime")
                     <*> (pure (fromEnum s)))

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
          = object ["Name" .= _gtsName]

instance ToPath GetTrailStatus where
        toPath = const "/"

instance ToQuery GetTrailStatus where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'getTrailStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtsrLatestDeliveryError'
--
-- * 'gtsrStartLoggingTime'
--
-- * 'gtsrLatestNotificationError'
--
-- * 'gtsrIsLogging'
--
-- * 'gtsrLatestDeliveryTime'
--
-- * 'gtsrLatestCloudWatchLogsDeliveryTime'
--
-- * 'gtsrLatestCloudWatchLogsDeliveryError'
--
-- * 'gtsrLatestNotificationTime'
--
-- * 'gtsrStopLoggingTime'
--
-- * 'gtsrStatus'
data GetTrailStatusResponse = GetTrailStatusResponse'
    { _gtsrLatestDeliveryError               :: !(Maybe Text)
    , _gtsrStartLoggingTime                  :: !(Maybe POSIX)
    , _gtsrLatestNotificationError           :: !(Maybe Text)
    , _gtsrIsLogging                         :: !(Maybe Bool)
    , _gtsrLatestDeliveryTime                :: !(Maybe POSIX)
    , _gtsrLatestCloudWatchLogsDeliveryTime  :: !(Maybe POSIX)
    , _gtsrLatestCloudWatchLogsDeliveryError :: !(Maybe Text)
    , _gtsrLatestNotificationTime            :: !(Maybe POSIX)
    , _gtsrStopLoggingTime                   :: !(Maybe POSIX)
    , _gtsrStatus                            :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetTrailStatusResponse' smart constructor.
getTrailStatusResponse :: Int -> GetTrailStatusResponse
getTrailStatusResponse pStatus =
    GetTrailStatusResponse'
    { _gtsrLatestDeliveryError = Nothing
    , _gtsrStartLoggingTime = Nothing
    , _gtsrLatestNotificationError = Nothing
    , _gtsrIsLogging = Nothing
    , _gtsrLatestDeliveryTime = Nothing
    , _gtsrLatestCloudWatchLogsDeliveryTime = Nothing
    , _gtsrLatestCloudWatchLogsDeliveryError = Nothing
    , _gtsrLatestNotificationTime = Nothing
    , _gtsrStopLoggingTime = Nothing
    , _gtsrStatus = pStatus
    }

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting
-- to deliver log files to the designated bucket. For more information see
-- the topic
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses>
-- in the Amazon S3 API Reference.
gtsrLatestDeliveryError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrLatestDeliveryError = lens _gtsrLatestDeliveryError (\ s a -> s{_gtsrLatestDeliveryError = a});

-- | Specifies the most recent date and time when CloudTrail started
-- recording API calls for an AWS account.
gtsrStartLoggingTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrStartLoggingTime = lens _gtsrStartLoggingTime (\ s a -> s{_gtsrStartLoggingTime = a}) . mapping _Time;

-- | Displays any Amazon SNS error that CloudTrail encountered when
-- attempting to send a notification. For more information about Amazon SNS
-- errors, see the
-- <http://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide>.
gtsrLatestNotificationError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrLatestNotificationError = lens _gtsrLatestNotificationError (\ s a -> s{_gtsrLatestNotificationError = a});

-- | Whether the CloudTrail is currently logging AWS API calls.
gtsrIsLogging :: Lens' GetTrailStatusResponse (Maybe Bool)
gtsrIsLogging = lens _gtsrIsLogging (\ s a -> s{_gtsrIsLogging = a});

-- | Specifies the date and time that CloudTrail last delivered log files to
-- an account\'s Amazon S3 bucket.
gtsrLatestDeliveryTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrLatestDeliveryTime = lens _gtsrLatestDeliveryTime (\ s a -> s{_gtsrLatestDeliveryTime = a}) . mapping _Time;

-- | Displays the most recent date and time when CloudTrail delivered logs to
-- CloudWatch Logs.
gtsrLatestCloudWatchLogsDeliveryTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrLatestCloudWatchLogsDeliveryTime = lens _gtsrLatestCloudWatchLogsDeliveryTime (\ s a -> s{_gtsrLatestCloudWatchLogsDeliveryTime = a}) . mapping _Time;

-- | Displays any CloudWatch Logs error that CloudTrail encountered when
-- attempting to deliver logs to CloudWatch Logs.
gtsrLatestCloudWatchLogsDeliveryError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrLatestCloudWatchLogsDeliveryError = lens _gtsrLatestCloudWatchLogsDeliveryError (\ s a -> s{_gtsrLatestCloudWatchLogsDeliveryError = a});

-- | Specifies the date and time of the most recent Amazon SNS notification
-- that CloudTrail has written a new log file to an account\'s Amazon S3
-- bucket.
gtsrLatestNotificationTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrLatestNotificationTime = lens _gtsrLatestNotificationTime (\ s a -> s{_gtsrLatestNotificationTime = a}) . mapping _Time;

-- | Specifies the most recent date and time when CloudTrail stopped
-- recording API calls for an AWS account.
gtsrStopLoggingTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrStopLoggingTime = lens _gtsrStopLoggingTime (\ s a -> s{_gtsrStopLoggingTime = a}) . mapping _Time;

-- | FIXME: Undocumented member.
gtsrStatus :: Lens' GetTrailStatusResponse Int
gtsrStatus = lens _gtsrStatus (\ s a -> s{_gtsrStatus = a});
