{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.GetTrailStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a JSON-formatted list of information about the specified trail.
-- Fields include information on delivery errors, Amazon SNS and Amazon S3
-- errors, and start and stop logging times for each trail.
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
    , gtsrLatestDeliveryError
    , gtsrLatestNotificationError
    , gtsrLatestDeliveryTime
    , gtsrLatestNotificationTime
    , gtsrStartLoggingTime
    , gtsrStopLoggingTime
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The name of a trail about which you want the current status.
newtype GetTrailStatus = GetTrailStatus
    { _gtsName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetTrailStatus' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
getTrailStatus :: Text -- ^ 'gtsName'
               -> GetTrailStatus
getTrailStatus p1 = GetTrailStatus
    { _gtsName = p1
    }

-- | The name of the trail for which you are requesting the current status.
gtsName :: Lens' GetTrailStatus Text
gtsName = lens _gtsName (\s a -> s { _gtsName = a })

instance ToPath GetTrailStatus

instance ToQuery GetTrailStatus

instance ToHeaders GetTrailStatus

instance ToJSON GetTrailStatus

-- | Returns the objects or data listed below if successful. Otherwise, returns
-- an error.
data GetTrailStatusResponse = GetTrailStatusResponse
    { _gtsrIsLogging :: Maybe Bool
    , _gtsrLatestDeliveryError :: Maybe Text
    , _gtsrLatestNotificationError :: Maybe Text
    , _gtsrLatestDeliveryTime :: Maybe ISO8601
    , _gtsrLatestNotificationTime :: Maybe ISO8601
    , _gtsrStartLoggingTime :: Maybe ISO8601
    , _gtsrStopLoggingTime :: Maybe ISO8601
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetTrailStatusResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IsLogging ::@ @Maybe Bool@
--
-- * @LatestDeliveryError ::@ @Maybe Text@
--
-- * @LatestNotificationError ::@ @Maybe Text@
--
-- * @LatestDeliveryTime ::@ @Maybe ISO8601@
--
-- * @LatestNotificationTime ::@ @Maybe ISO8601@
--
-- * @StartLoggingTime ::@ @Maybe ISO8601@
--
-- * @StopLoggingTime ::@ @Maybe ISO8601@
--
getTrailStatusResponse :: GetTrailStatusResponse
getTrailStatusResponse = GetTrailStatusResponse
    { _gtsrIsLogging = Nothing
    , _gtsrLatestDeliveryError = Nothing
    , _gtsrLatestNotificationError = Nothing
    , _gtsrLatestDeliveryTime = Nothing
    , _gtsrLatestNotificationTime = Nothing
    , _gtsrStartLoggingTime = Nothing
    , _gtsrStopLoggingTime = Nothing
    }

-- | Whether the CloudTrail is currently logging AWS API calls.
gtsrIsLogging :: Lens' GetTrailStatusResponse (Maybe Bool)
gtsrIsLogging = lens _gtsrIsLogging (\s a -> s { _gtsrIsLogging = a })

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting to
-- deliver log files to the designated bucket. For more information see the
-- topic Error Responses in the Amazon S3 API Reference.
gtsrLatestDeliveryError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrLatestDeliveryError =
    lens _gtsrLatestDeliveryError
         (\s a -> s { _gtsrLatestDeliveryError = a })

-- | Displays any Amazon SNS error that CloudTrail encountered when attempting
-- to send a notification. For more information about Amazon SNS errors, see
-- the Amazon SNS Developer Guide.
gtsrLatestNotificationError :: Lens' GetTrailStatusResponse (Maybe Text)
gtsrLatestNotificationError =
    lens _gtsrLatestNotificationError
         (\s a -> s { _gtsrLatestNotificationError = a })

-- | Specifies the date and time that CloudTrail last delivered log files to an
-- account's Amazon S3 bucket.
gtsrLatestDeliveryTime :: Lens' GetTrailStatusResponse (Maybe ISO8601)
gtsrLatestDeliveryTime =
    lens _gtsrLatestDeliveryTime (\s a -> s { _gtsrLatestDeliveryTime = a })

-- | Specifies the date and time of the most recent Amazon SNS notification that
-- CloudTrail has written a new log file to an account's Amazon S3 bucket.
gtsrLatestNotificationTime :: Lens' GetTrailStatusResponse (Maybe ISO8601)
gtsrLatestNotificationTime =
    lens _gtsrLatestNotificationTime
         (\s a -> s { _gtsrLatestNotificationTime = a })

-- | Specifies the most recent date and time when CloudTrail started recording
-- API calls for an AWS account.
gtsrStartLoggingTime :: Lens' GetTrailStatusResponse (Maybe ISO8601)
gtsrStartLoggingTime =
    lens _gtsrStartLoggingTime (\s a -> s { _gtsrStartLoggingTime = a })

-- | Specifies the most recent date and time when CloudTrail stopped recording
-- API calls for an AWS account.
gtsrStopLoggingTime :: Lens' GetTrailStatusResponse (Maybe ISO8601)
gtsrStopLoggingTime =
    lens _gtsrStopLoggingTime (\s a -> s { _gtsrStopLoggingTime = a })

instance FromJSON GetTrailStatusResponse

instance AWSRequest GetTrailStatus where
    type Sv GetTrailStatus = CloudTrail
    type Rs GetTrailStatus = GetTrailStatusResponse

    request = get
    response _ = jsonResponse
