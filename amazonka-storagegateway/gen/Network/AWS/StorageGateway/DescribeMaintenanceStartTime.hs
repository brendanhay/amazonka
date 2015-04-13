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

-- Module      : Network.AWS.StorageGateway.DescribeMaintenanceStartTime
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns your gateway's weekly maintenance start time including
-- the day and time of the week. Note that values are in terms of the gateway's
-- time zone.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeMaintenanceStartTime.html>
module Network.AWS.StorageGateway.DescribeMaintenanceStartTime
    (
    -- * Request
      DescribeMaintenanceStartTime
    -- ** Request constructor
    , describeMaintenanceStartTime
    -- ** Request lenses
    , dmstGatewayARN

    -- * Response
    , DescribeMaintenanceStartTimeResponse
    -- ** Response constructor
    , describeMaintenanceStartTimeResponse
    -- ** Response lenses
    , dmstrDayOfWeek
    , dmstrGatewayARN
    , dmstrHourOfDay
    , dmstrMinuteOfHour
    , dmstrTimezone
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DescribeMaintenanceStartTime = DescribeMaintenanceStartTime
    { _dmstGatewayARN :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DescribeMaintenanceStartTime' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmstGatewayARN' @::@ 'Text'
--
describeMaintenanceStartTime :: Text -- ^ 'dmstGatewayARN'
                             -> DescribeMaintenanceStartTime
describeMaintenanceStartTime p1 = DescribeMaintenanceStartTime
    { _dmstGatewayARN = p1
    }

dmstGatewayARN :: Lens' DescribeMaintenanceStartTime Text
dmstGatewayARN = lens _dmstGatewayARN (\s a -> s { _dmstGatewayARN = a })

data DescribeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse
    { _dmstrDayOfWeek    :: Maybe Nat
    , _dmstrGatewayARN   :: Maybe Text
    , _dmstrHourOfDay    :: Maybe Nat
    , _dmstrMinuteOfHour :: Maybe Nat
    , _dmstrTimezone     :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeMaintenanceStartTimeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmstrDayOfWeek' @::@ 'Maybe' 'Natural'
--
-- * 'dmstrGatewayARN' @::@ 'Maybe' 'Text'
--
-- * 'dmstrHourOfDay' @::@ 'Maybe' 'Natural'
--
-- * 'dmstrMinuteOfHour' @::@ 'Maybe' 'Natural'
--
-- * 'dmstrTimezone' @::@ 'Maybe' 'Text'
--
describeMaintenanceStartTimeResponse :: DescribeMaintenanceStartTimeResponse
describeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse
    { _dmstrGatewayARN   = Nothing
    , _dmstrHourOfDay    = Nothing
    , _dmstrMinuteOfHour = Nothing
    , _dmstrDayOfWeek    = Nothing
    , _dmstrTimezone     = Nothing
    }

dmstrDayOfWeek :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrDayOfWeek = lens _dmstrDayOfWeek (\s a -> s { _dmstrDayOfWeek = a }) . mapping _Nat

dmstrGatewayARN :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrGatewayARN = lens _dmstrGatewayARN (\s a -> s { _dmstrGatewayARN = a })

dmstrHourOfDay :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrHourOfDay = lens _dmstrHourOfDay (\s a -> s { _dmstrHourOfDay = a }) . mapping _Nat

dmstrMinuteOfHour :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Natural)
dmstrMinuteOfHour =
    lens _dmstrMinuteOfHour (\s a -> s { _dmstrMinuteOfHour = a })
        . mapping _Nat

dmstrTimezone :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrTimezone = lens _dmstrTimezone (\s a -> s { _dmstrTimezone = a })

instance ToPath DescribeMaintenanceStartTime where
    toPath = const "/"

instance ToQuery DescribeMaintenanceStartTime where
    toQuery = const mempty

instance ToHeaders DescribeMaintenanceStartTime

instance ToJSON DescribeMaintenanceStartTime where
    toJSON DescribeMaintenanceStartTime{..} = object
        [ "GatewayARN" .= _dmstGatewayARN
        ]

instance AWSRequest DescribeMaintenanceStartTime where
    type Sv DescribeMaintenanceStartTime = StorageGateway
    type Rs DescribeMaintenanceStartTime = DescribeMaintenanceStartTimeResponse

    request  = post "DescribeMaintenanceStartTime"
    response = jsonResponse

instance FromJSON DescribeMaintenanceStartTimeResponse where
    parseJSON = withObject "DescribeMaintenanceStartTimeResponse" $ \o -> DescribeMaintenanceStartTimeResponse
        <$> o .:? "DayOfWeek"
        <*> o .:? "GatewayARN"
        <*> o .:? "HourOfDay"
        <*> o .:? "MinuteOfHour"
        <*> o .:? "Timezone"
