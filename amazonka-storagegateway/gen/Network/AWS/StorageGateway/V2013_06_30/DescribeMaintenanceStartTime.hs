{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeMaintenanceStartTime
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
module Network.AWS.StorageGateway.V2013_06_30.DescribeMaintenanceStartTime
    (
    -- * Request
      DescribeMaintenanceStartTime
    -- ** Request constructor
    , mkDescribeMaintenanceStartTimeInput
    -- ** Request lenses
    , dmstiGatewayARN

    -- * Response
    , DescribeMaintenanceStartTimeResponse
    -- ** Response lenses
    , dmstoGatewayARN
    , dmstoHourOfDay
    , dmstoMinuteOfHour
    , dmstoDayOfWeek
    , dmstoTimezone
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeMaintenanceStartTime' request.
mkDescribeMaintenanceStartTimeInput :: Text -- ^ 'dmstiGatewayARN'
                                    -> DescribeMaintenanceStartTime
mkDescribeMaintenanceStartTimeInput p1 = DescribeMaintenanceStartTime
    { _dmstiGatewayARN = p1
    }
{-# INLINE mkDescribeMaintenanceStartTimeInput #-}

newtype DescribeMaintenanceStartTime = DescribeMaintenanceStartTime
    { _dmstiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dmstiGatewayARN :: Lens' DescribeMaintenanceStartTime (Text)
dmstiGatewayARN = lens _dmstiGatewayARN (\s a -> s { _dmstiGatewayARN = a })
{-# INLINE dmstiGatewayARN #-}

instance ToPath DescribeMaintenanceStartTime

instance ToQuery DescribeMaintenanceStartTime

instance ToHeaders DescribeMaintenanceStartTime

instance ToJSON DescribeMaintenanceStartTime

data DescribeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse
    { _dmstoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dmstoHourOfDay :: Maybe Integer
    , _dmstoMinuteOfHour :: Maybe Integer
    , _dmstoDayOfWeek :: Maybe Integer
    , _dmstoTimezone :: Maybe Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dmstoGatewayARN :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstoGatewayARN = lens _dmstoGatewayARN (\s a -> s { _dmstoGatewayARN = a })
{-# INLINE dmstoGatewayARN #-}

dmstoHourOfDay :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Integer)
dmstoHourOfDay = lens _dmstoHourOfDay (\s a -> s { _dmstoHourOfDay = a })
{-# INLINE dmstoHourOfDay #-}

dmstoMinuteOfHour :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Integer)
dmstoMinuteOfHour = lens _dmstoMinuteOfHour (\s a -> s { _dmstoMinuteOfHour = a })
{-# INLINE dmstoMinuteOfHour #-}

dmstoDayOfWeek :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Integer)
dmstoDayOfWeek = lens _dmstoDayOfWeek (\s a -> s { _dmstoDayOfWeek = a })
{-# INLINE dmstoDayOfWeek #-}

dmstoTimezone :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstoTimezone = lens _dmstoTimezone (\s a -> s { _dmstoTimezone = a })
{-# INLINE dmstoTimezone #-}

instance FromJSON DescribeMaintenanceStartTimeResponse

instance AWSRequest DescribeMaintenanceStartTime where
    type Sv DescribeMaintenanceStartTime = StorageGateway
    type Rs DescribeMaintenanceStartTime = DescribeMaintenanceStartTimeResponse

    request = get
    response _ = jsonResponse
