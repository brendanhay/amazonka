{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
-- errors, and start and stop logging times for each trail. The CloudTrail API
-- is currently undergoing revision. This action currently returns both new
-- fields and fields slated for removal from the API. The following lists
-- indicate the plans for each field: List of Members Planned for Ongoing
-- Support IsLogging LatestDeliveryTime LatestNotificationTime
-- StartLoggingTime StopLoggingTime LatestNotificationError
-- LatestDeliveryError List of Members Scheduled for Removal
-- LatestDeliveryAttemptTime: Use LatestDeliveryTime instead.
-- LatestNotificationAttemptTime: Use LatestNotificationTime instead.
-- LatestDeliveryAttemptSucceeded: No replacement. See the note following this
-- list. LatestNotificationAttemptSucceeded: No replacement. See the note
-- following this list. TimeLoggingStarted: Use StartLoggingTime instead.
-- TimeLoggingStopped: Use StopLoggingtime instead. No replacements have been
-- created for LatestDeliveryAttemptSucceeded and
-- LatestNotificationAttemptSucceeded. Use LatestDeliveryError and
-- LatestNotificationError to evaluate success or failure of log delivery or
-- notification. Empty values returned for these fields indicate success. An
-- error in LatestDeliveryError generally indicates either a missing bucket or
-- insufficient permissions to write to the bucket. Similarly, an error in
-- LatestNotificationError indicates either a missing topic or insufficient
-- permissions.
module Network.AWS.CloudTrail.V2013_11_01.GetTrailStatus where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude

data GetTrailStatus = GetTrailStatus
    { _gtsrName :: Text
      -- ^ The name of the trail for which you are requesting the current
      -- status.
    } deriving (Generic)

makeLenses ''GetTrailStatus

instance ToPath GetTrailStatus

instance ToQuery GetTrailStatus

instance ToHeaders GetTrailStatus

instance ToJSON GetTrailStatus

data GetTrailStatusResponse = GetTrailStatusResponse
    { _gtssIsLogging :: Maybe Bool
      -- ^ Whether the CloudTrail is currently logging AWS API calls.
    , _gtssStartLoggingTime :: Maybe ISO8601
      -- ^ Specifies the most recent date and time when CloudTrail started
      -- recording API calls for an AWS account.
    , _gtssLatestDeliveryTime :: Maybe ISO8601
      -- ^ Specifies the date and time that CloudTrail last delivered log
      -- files to an account's Amazon S3 bucket.
    , _gtssLatestNotificationTime :: Maybe ISO8601
      -- ^ Specifies the date and time of the most recent Amazon SNS
      -- notification that CloudTrail has written a new log file to an
      -- account's Amazon S3 bucket.
    , _gtssStopLoggingTime :: Maybe ISO8601
      -- ^ Specifies the most recent date and time when CloudTrail stopped
      -- recording API calls for an AWS account.
    , _gtssTimeLoggingStopped :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , _gtssLatestDeliveryError :: Maybe Text
      -- ^ Displays any Amazon S3 error that CloudTrail encountered when
      -- attempting to deliver log files to the designated bucket. For
      -- more information see the topic Error Responses in the Amazon S3
      -- API Reference.
    , _gtssLatestNotificationAttemptSucceeded :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , _gtssLatestNotificationError :: Maybe Text
      -- ^ Displays any Amazon SNS error that CloudTrail encountered when
      -- attempting to send a notification. For more information about
      -- Amazon SNS errors, see the Amazon SNS Developer Guide.
    , _gtssLatestDeliveryAttemptSucceeded :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , _gtssTimeLoggingStarted :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , _gtssLatestDeliveryAttemptTime :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    , _gtssLatestNotificationAttemptTime :: Maybe Text
      -- ^ Scheduled for removal as early as February 15, 2014.
    } deriving (Generic)

makeLenses ''GetTrailStatusResponse

instance FromJSON GetTrailStatusResponse

instance AWSRequest GetTrailStatus where
    type Sv GetTrailStatus = CloudTrail
    type Rs GetTrailStatus = GetTrailStatusResponse

    request = get
    response _ = jsonResponse
