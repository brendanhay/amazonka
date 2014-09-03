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

data GetTrailStatus = GetTrailStatus
    { _gtsrName :: Text
      -- ^ The name of the trail for which you are requesting the current
      -- status.
    } deriving (Show, Generic)

-- | The name of the trail for which you are requesting the current status.
gtsrName
    :: Functor f
    => (Text
    -> f (Text))
    -> GetTrailStatus
    -> f GetTrailStatus
gtsrName f x =
    (\y -> x { _gtsrName = y })
       <$> f (_gtsrName x)
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
gtssIsLogging
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> GetTrailStatusResponse
    -> f GetTrailStatusResponse
gtssIsLogging f x =
    (\y -> x { _gtssIsLogging = y })
       <$> f (_gtssIsLogging x)
{-# INLINE gtssIsLogging #-}

-- | Specifies the date and time that CloudTrail last delivered log files to an
-- account's Amazon S3 bucket.
gtssLatestDeliveryTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> GetTrailStatusResponse
    -> f GetTrailStatusResponse
gtssLatestDeliveryTime f x =
    (\y -> x { _gtssLatestDeliveryTime = y })
       <$> f (_gtssLatestDeliveryTime x)
{-# INLINE gtssLatestDeliveryTime #-}

-- | Specifies the date and time of the most recent Amazon SNS notification that
-- CloudTrail has written a new log file to an account's Amazon S3 bucket.
gtssLatestNotificationTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> GetTrailStatusResponse
    -> f GetTrailStatusResponse
gtssLatestNotificationTime f x =
    (\y -> x { _gtssLatestNotificationTime = y })
       <$> f (_gtssLatestNotificationTime x)
{-# INLINE gtssLatestNotificationTime #-}

-- | Specifies the most recent date and time when CloudTrail started recording
-- API calls for an AWS account.
gtssStartLoggingTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> GetTrailStatusResponse
    -> f GetTrailStatusResponse
gtssStartLoggingTime f x =
    (\y -> x { _gtssStartLoggingTime = y })
       <$> f (_gtssStartLoggingTime x)
{-# INLINE gtssStartLoggingTime #-}

-- | Specifies the most recent date and time when CloudTrail stopped recording
-- API calls for an AWS account.
gtssStopLoggingTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> GetTrailStatusResponse
    -> f GetTrailStatusResponse
gtssStopLoggingTime f x =
    (\y -> x { _gtssStopLoggingTime = y })
       <$> f (_gtssStopLoggingTime x)
{-# INLINE gtssStopLoggingTime #-}

-- | Displays any Amazon S3 error that CloudTrail encountered when attempting to
-- deliver log files to the designated bucket. For more information see the
-- topic Error Responses in the Amazon S3 API Reference.
gtssLatestDeliveryError
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetTrailStatusResponse
    -> f GetTrailStatusResponse
gtssLatestDeliveryError f x =
    (\y -> x { _gtssLatestDeliveryError = y })
       <$> f (_gtssLatestDeliveryError x)
{-# INLINE gtssLatestDeliveryError #-}

-- | Displays any Amazon SNS error that CloudTrail encountered when attempting
-- to send a notification. For more information about Amazon SNS errors, see
-- the Amazon SNS Developer Guide.
gtssLatestNotificationError
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetTrailStatusResponse
    -> f GetTrailStatusResponse
gtssLatestNotificationError f x =
    (\y -> x { _gtssLatestNotificationError = y })
       <$> f (_gtssLatestNotificationError x)
{-# INLINE gtssLatestNotificationError #-}

instance FromJSON GetTrailStatusResponse

instance AWSRequest GetTrailStatus where
    type Sv GetTrailStatus = CloudTrail
    type Rs GetTrailStatus = GetTrailStatusResponse

    request = get
    response _ = jsonResponse
