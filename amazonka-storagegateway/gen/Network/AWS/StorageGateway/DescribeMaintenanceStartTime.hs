{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | This operation returns your gateway's weekly maintenance start time
-- including the day and time of the week. Note that values are in terms of
-- the gateway's time zone. Example Request The following example shows a
-- request that describes a gateway's maintenance window. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeMaintenanceStartTime { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 136 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "HourOfDay": 15, "MinuteOfHour": 35, "DayOfWeek": 2, "Timezone": "GMT+7:00"
-- }.
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
    , dmstrGatewayARN
    , dmstrHourOfDay
    , dmstrMinuteOfHour
    , dmstrDayOfWeek
    , dmstrTimezone
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing the of the gateway.
newtype DescribeMaintenanceStartTime = DescribeMaintenanceStartTime
    { _dmstGatewayARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeMaintenanceStartTime' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
describeMaintenanceStartTime :: Text -- ^ 'dmstGatewayARN'
                             -> DescribeMaintenanceStartTime
describeMaintenanceStartTime p1 = DescribeMaintenanceStartTime
    { _dmstGatewayARN = p1
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dmstGatewayARN :: Lens' DescribeMaintenanceStartTime Text
dmstGatewayARN = lens _dmstGatewayARN (\s a -> s { _dmstGatewayARN = a })

instance ToPath DescribeMaintenanceStartTime

instance ToQuery DescribeMaintenanceStartTime

instance ToHeaders DescribeMaintenanceStartTime

instance ToJSON DescribeMaintenanceStartTime

data DescribeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse
    { _dmstrGatewayARN :: Maybe Text
    , _dmstrHourOfDay :: Maybe Integer
    , _dmstrMinuteOfHour :: Maybe Integer
    , _dmstrDayOfWeek :: Maybe Integer
    , _dmstrTimezone :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeMaintenanceStartTimeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
-- * @HourOfDay ::@ @Maybe Integer@
--
-- * @MinuteOfHour ::@ @Maybe Integer@
--
-- * @DayOfWeek ::@ @Maybe Integer@
--
-- * @Timezone ::@ @Maybe Text@
--
describeMaintenanceStartTimeResponse :: DescribeMaintenanceStartTimeResponse
describeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse
    { _dmstrGatewayARN = Nothing
    , _dmstrHourOfDay = Nothing
    , _dmstrMinuteOfHour = Nothing
    , _dmstrDayOfWeek = Nothing
    , _dmstrTimezone = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dmstrGatewayARN :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrGatewayARN = lens _dmstrGatewayARN (\s a -> s { _dmstrGatewayARN = a })

dmstrHourOfDay :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Integer)
dmstrHourOfDay = lens _dmstrHourOfDay (\s a -> s { _dmstrHourOfDay = a })

dmstrMinuteOfHour :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Integer)
dmstrMinuteOfHour =
    lens _dmstrMinuteOfHour (\s a -> s { _dmstrMinuteOfHour = a })

dmstrDayOfWeek :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Integer)
dmstrDayOfWeek = lens _dmstrDayOfWeek (\s a -> s { _dmstrDayOfWeek = a })

dmstrTimezone :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstrTimezone = lens _dmstrTimezone (\s a -> s { _dmstrTimezone = a })

instance FromJSON DescribeMaintenanceStartTimeResponse

instance AWSRequest DescribeMaintenanceStartTime where
    type Sv DescribeMaintenanceStartTime = StorageGateway
    type Rs DescribeMaintenanceStartTime = DescribeMaintenanceStartTimeResponse

    request = get
    response _ = jsonResponse
