{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.GetTrailStatus
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
module Network.AWS.CloudTrail.V2013_11_01.GetTrailStatus
    (
    -- * Request
      GetTrailStatus
    -- ** Request constructor
    , getTrailStatus
    -- ** Request lenses
    , gtsrName

    -- * Response
    , GetTrailStatusResponse
    -- ** Response lenses
    , gtssIsLogging
    , gtssLatestDeliveryTime
    , gtssLatestNotificationTime
    , gtssStartLoggingTime
    , gtssStopLoggingTime
    , gtssLatestDeliveryError
    , gtssLatestNotificationError
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'GetTrailStatus' request.
getTrailStatus :: Text -- ^ 'gtsrName'
               -> GetTrailStatus
getTrailStatus p1 = GetTrailStatus
    { _gtsrName = p1
    }
{-# INLINE getTrailStatus #-}

data GetTrailStatus = GetTrailStatus
    { _gtsrName :: Text
      -- ^ The name of the trail for which you are requesting the current
      -- status.
    } deriving (Show, Generic)

-- | The name of the trail for which you are requesting the current status.
gtsrName :: Lens' GetTrailStatus (Text)
gtsrName f x =
    f (_gtsrName x)
        <&> \y -> x { _gtsrName = y }
{-# INLINE gtsrName #-}

instance ToPath GetTrailStatus

instance ToQuery GetTrailStatus

instance ToHeaders GetTrailStatus

instance ToJSON GetTrailStatus

data GetTrailStatusResponse = GetTrailStatusResponse
    { _gtssIsLogging :: Maybe Bool
      -- ^ Whether the CloudTrail is currently logging AWS API calls.
    , _gtssLatestDeliveryTime :: Maybe ISO8601
      -- ^ Specifies the date and time that CloudTrail last delivered log
      -- files to an account's Amazon S3 bucket.
    , _gtssLatestNotificationTime :: Maybe ISO8601
      -- ^ Specifies the date and time of the most recent Amazon SNS
      -- notification that CloudTrail has written a new log file to an
      -- account's Amazon S3 bucket.
    , _gtssStartLoggingTime :: Maybe ISO8601
      -- ^ Specifies the most recent date and time when CloudTrail started
      -- recording API calls for an AWS account.
    , _gtssStopLoggingTime :: Maybe ISO8601
      -- ^ Specifies the most recent date and time when CloudTrail stopped
      -- recording API calls for an AWS account.
    , _gtssLatestDeliveryError :: Maybe Text
      -- ^ Displays any Amazon S3 error that CloudTrail encountered when
      -- attempting to deliver log files to the designated bucket. For
      -- more information see the topic Error Responses in the Amazon S3
      -- API Reference.
    , _gtssLatestNotificationError :: Maybe Text
      -- ^ Displays any Amazon SNS error that CloudTrail encountered when
      -- attempting to send a notification. For more information about
      -- Amazon SNS errors, see the Amazon SNS Developer Guide.
    } deriving (Show, Generic)

-- | Whether the CloudTrail is currently logging AWS API calls.
gtssIsLogging :: Lens' GetTrailStatusResponse (Maybe Bool)
gtssIsLogging f x =
    f (_gtssIsLogging x)
        <&> \y -> x { _gtssIsLogging = y }
{-# INLINE gtssIsLogging #-}

-- | Specifies the date and time that CloudTrail last delivered log files to an
-- account's Amazon S3 bucket.
gtssLatestDeliveryTime :: Lens' GetTrailStatusResponse (Maybe ISO8601)
gtssLatestDeliveryTime f x =
    f (_gtssLatestDeliveryTime x)
        <&> \y -> x { _gtssLatestDeliveryTime = y }
{-# INLINE gtssLatestDeliveryTime #-}

-- | Specifies the date and time of the most recent Amazon SNS notification that
-- CloudTrail has written a new log file to an account's Amazon S3 bucket.
gtssLatestNotificationTime :: Lens' GetTrailStatusResponse (Maybe ISO8601)
gtssLatestNotificationTime f x =
    f (_gtssLatestNotificationTime x)
        <&> \y -> x { _gtssLatestNotificationTime = y }
{-# INLINE gtssLatestNotificationTime #-}

-- | Specifies the most recent date and time when CloudTrail started recording
-- API calls for an AWS account.
gtssStartLoggingTime :: Lens' GetTrailStatusResponse (Maybe ISO8601)
gtssStartLoggingTime f x =
    f (_gtssStartLoggingTime x)
        <&> \y -> x { _gtssStartLoggingTime = y }
{-# INLINE gtssStartLoggingTime #-}

-- | Specifies the most recent date and time when CloudTrail stopped recording
-- API calls for an AWS account.
gtssStopLoggingTime :: Lens' GetTrailStatusResponse (Maybe ISO8601)
gtssStopLoggingTime f x =
    f (_gtssStopLoggingTime x)
        <&> \y -> x { _gtssStopLoggingTime = y }
{-# INLINE gtssStopLoggingTime #-}

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting to
-- deliver log files to the designated bucket. For more information see the
-- topic Error Responses in the Amazon S3 API Reference.
gtssLatestDeliveryError :: Lens' GetTrailStatusResponse (Maybe Text)
gtssLatestDeliveryError f x =
    f (_gtssLatestDeliveryError x)
        <&> \y -> x { _gtssLatestDeliveryError = y }
{-# INLINE gtssLatestDeliveryError #-}

-- | Displays any Amazon SNS error that CloudTrail encountered when attempting
-- to send a notification. For more information about Amazon SNS errors, see
-- the Amazon SNS Developer Guide.
gtssLatestNotificationError :: Lens' GetTrailStatusResponse (Maybe Text)
gtssLatestNotificationError f x =
    f (_gtssLatestNotificationError x)
        <&> \y -> x { _gtssLatestNotificationError = y }
{-# INLINE gtssLatestNotificationError #-}

instance FromJSON GetTrailStatusResponse

instance AWSRequest GetTrailStatus where
    type Sv GetTrailStatus = CloudTrail
    type Rs GetTrailStatus = GetTrailStatusResponse

    request = get
    response _ = jsonResponse
