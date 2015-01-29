{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.GetTrailStatus
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
    , gtsrIsLogging
    , gtsrLatestCloudWatchLogsDeliveryError
    , gtsrLatestCloudWatchLogsDeliveryTime
    , gtsrLatestDeliveryError
    , gtsrLatestDeliveryTime
    , gtsrLatestNotificationError
    , gtsrLatestNotificationTime
    , gtsrStartLoggingTime
    , gtsrStopLoggingTime
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.Types
import qualified GHC.Exts

newtype GetTrailStatus = GetTrailStatus
    { _gtsName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetTrailStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtsName' @::@ 'Text'
--
getTrailStatus :: Text -- ^ 'gtsName'
               -> GetTrailStatus
getTrailStatus p1 = GetTrailStatus
    { _gtsName = p1
    }

-- | The name of the trail for which you are requesting the current status.
gtsName :: Lens' GetTrailStatus Text
gtsName = lens _gtsName (\s a -> s { _gtsName = a })

data GetTrailStatusResponse = GetTrailStatusResponse
    { _gtsrIsLogging                         :: Maybe Bool
    , _gtsrLatestCloudWatchLogsDeliveryError :: Maybe Text
    , _gtsrLatestCloudWatchLogsDeliveryTime  :: Maybe POSIX
    , _gtsrLatestDeliveryError               :: Maybe Text
    , _gtsrLatestDeliveryTime                :: Maybe POSIX
    , _gtsrLatestNotificationError           :: Maybe Text
    , _gtsrLatestNotificationTime            :: Maybe POSIX
    , _gtsrStartLoggingTime                  :: Maybe POSIX
    , _gtsrStopLoggingTime                   :: Maybe POSIX
    } deriving (Eq, Ord, Read, Show)

-- | 'GetTrailStatusResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtsrIsLogging' @::@ 'Maybe' 'Bool'
--
-- * 'gtsrLatestCloudWatchLogsDeliveryError' @::@ 'Maybe' 'Text'
--
-- * 'gtsrLatestCloudWatchLogsDeliveryTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'gtsrLatestDeliveryError' @::@ 'Maybe' 'Text'
--
-- * 'gtsrLatestDeliveryTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'gtsrLatestNotificationError' @::@ 'Maybe' 'Text'
--
-- * 'gtsrLatestNotificationTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'gtsrStartLoggingTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'gtsrStopLoggingTime' @::@ 'Maybe' 'UTCTime'
--
getTrailStatusResponse :: GetTrailStatusResponse
getTrailStatusResponse = GetTrailStatusResponse
    { _gtsrIsLogging                         = Nothing
    , _gtsrLatestDeliveryError               = Nothing
    , _gtsrLatestNotificationError           = Nothing
    , _gtsrLatestDeliveryTime                = Nothing
    , _gtsrLatestNotificationTime            = Nothing
    , _gtsrStartLoggingTime                  = Nothing
    , _gtsrStopLoggingTime                   = Nothing
    , _gtsrLatestCloudWatchLogsDeliveryError = Nothing
    , _gtsrLatestCloudWatchLogsDeliveryTime  = Nothing
    }

-- | Whether the CloudTrail is currently logging AWS API calls.
gtsrIsLogging :: Lens' GetTrailStatusResponse (Maybe Bool)
gtsrIsLogging = lens _gtsrIsLogging (\s a -> s { _gtsrIsLogging = a })

-- | Displays any CloudWatch Logs error that CloudTrail encountered when
-- attempting to deliver logs to CloudWatch Logs.
gtsrLatestCloudWatchLogsDeliveryError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrLatestCloudWatchLogsDeliveryError =
    lens _gtsrLatestCloudWatchLogsDeliveryError
        (\s a -> s { _gtsrLatestCloudWatchLogsDeliveryError = a })

-- | Displays the most recent date and time when CloudTrail delivered logs to
-- CloudWatch Logs.
gtsrLatestCloudWatchLogsDeliveryTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrLatestCloudWatchLogsDeliveryTime =
    lens _gtsrLatestCloudWatchLogsDeliveryTime
        (\s a -> s { _gtsrLatestCloudWatchLogsDeliveryTime = a })
            . mapping _Time

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting to
-- deliver log files to the designated bucket. For more information see the
-- topic <http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html Error Responses> in the Amazon S3 API Reference.
gtsrLatestDeliveryError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrLatestDeliveryError =
    lens _gtsrLatestDeliveryError (\s a -> s { _gtsrLatestDeliveryError = a })

-- | Specifies the date and time that CloudTrail last delivered log files to an
-- account's Amazon S3 bucket.
gtsrLatestDeliveryTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrLatestDeliveryTime =
    lens _gtsrLatestDeliveryTime (\s a -> s { _gtsrLatestDeliveryTime = a })
        . mapping _Time

-- | Displays any Amazon SNS error that CloudTrail encountered when attempting to
-- send a notification. For more information about Amazon SNS errors, see the <http://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon SNS Developer Guide>.
gtsrLatestNotificationError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrLatestNotificationError =
    lens _gtsrLatestNotificationError
        (\s a -> s { _gtsrLatestNotificationError = a })

-- | Specifies the date and time of the most recent Amazon SNS notification that
-- CloudTrail has written a new log file to an account's Amazon S3 bucket.
gtsrLatestNotificationTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrLatestNotificationTime =
    lens _gtsrLatestNotificationTime
        (\s a -> s { _gtsrLatestNotificationTime = a })
            . mapping _Time

-- | Specifies the most recent date and time when CloudTrail started recording API
-- calls for an AWS account.
gtsrStartLoggingTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrStartLoggingTime =
    lens _gtsrStartLoggingTime (\s a -> s { _gtsrStartLoggingTime = a })
        . mapping _Time

-- | Specifies the most recent date and time when CloudTrail stopped recording API
-- calls for an AWS account.
gtsrStopLoggingTime :: Lens' GetTrailStatusResponse (Maybe UTCTime)
gtsrStopLoggingTime =
    lens _gtsrStopLoggingTime (\s a -> s { _gtsrStopLoggingTime = a })
        . mapping _Time

instance ToPath GetTrailStatus where
    toPath = const "/"

instance ToQuery GetTrailStatus where
    toQuery = const mempty

instance ToHeaders GetTrailStatus

instance ToJSON GetTrailStatus where
    toJSON GetTrailStatus{..} = object
        [ "Name" .= _gtsName
        ]

instance AWSRequest GetTrailStatus where
    type Sv GetTrailStatus = CloudTrail
    type Rs GetTrailStatus = GetTrailStatusResponse

    request  = post "GetTrailStatus"
    response = jsonResponse

instance FromJSON GetTrailStatusResponse where
    parseJSON = withObject "GetTrailStatusResponse" $ \o -> GetTrailStatusResponse
        <$> o .:? "IsLogging"
        <*> o .:? "LatestCloudWatchLogsDeliveryError"
        <*> o .:? "LatestCloudWatchLogsDeliveryTime"
        <*> o .:? "LatestDeliveryError"
        <*> o .:? "LatestDeliveryTime"
        <*> o .:? "LatestNotificationError"
        <*> o .:? "LatestNotificationTime"
        <*> o .:? "StartLoggingTime"
        <*> o .:? "StopLoggingTime"
