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
    , describeMaintenanceStartTime
    -- ** Request lenses
    , dmstiGatewayARN

    -- * Response
    , DescribeMaintenanceStartTimeResponse
    -- ** Response lenses
    , dmstoDayOfWeek
    , dmstoGatewayARN
    , dmstoTimezone
    , dmstoHourOfDay
    , dmstoMinuteOfHour
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeMaintenanceStartTime' request.
describeMaintenanceStartTime :: Text -- ^ 'dmstiGatewayARN'
                             -> DescribeMaintenanceStartTime
describeMaintenanceStartTime p1 = DescribeMaintenanceStartTime
    { _dmstiGatewayARN = p1
    }
{-# INLINE describeMaintenanceStartTime #-}

data DescribeMaintenanceStartTime = DescribeMaintenanceStartTime
    { _dmstiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dmstiGatewayARN :: Lens' DescribeMaintenanceStartTime (Text)
dmstiGatewayARN f x =
    f (_dmstiGatewayARN x)
        <&> \y -> x { _dmstiGatewayARN = y }
{-# INLINE dmstiGatewayARN #-}

instance ToPath DescribeMaintenanceStartTime

instance ToQuery DescribeMaintenanceStartTime

instance ToHeaders DescribeMaintenanceStartTime

instance ToJSON DescribeMaintenanceStartTime

data DescribeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse
    { _dmstoDayOfWeek :: Maybe Integer
    , _dmstoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dmstoTimezone :: Maybe Text
    , _dmstoHourOfDay :: Maybe Integer
    , _dmstoMinuteOfHour :: Maybe Integer
    } deriving (Show, Generic)

dmstoDayOfWeek :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Integer)
dmstoDayOfWeek f x =
    f (_dmstoDayOfWeek x)
        <&> \y -> x { _dmstoDayOfWeek = y }
{-# INLINE dmstoDayOfWeek #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dmstoGatewayARN :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstoGatewayARN f x =
    f (_dmstoGatewayARN x)
        <&> \y -> x { _dmstoGatewayARN = y }
{-# INLINE dmstoGatewayARN #-}

dmstoTimezone :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Text)
dmstoTimezone f x =
    f (_dmstoTimezone x)
        <&> \y -> x { _dmstoTimezone = y }
{-# INLINE dmstoTimezone #-}

dmstoHourOfDay :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Integer)
dmstoHourOfDay f x =
    f (_dmstoHourOfDay x)
        <&> \y -> x { _dmstoHourOfDay = y }
{-# INLINE dmstoHourOfDay #-}

dmstoMinuteOfHour :: Lens' DescribeMaintenanceStartTimeResponse (Maybe Integer)
dmstoMinuteOfHour f x =
    f (_dmstoMinuteOfHour x)
        <&> \y -> x { _dmstoMinuteOfHour = y }
{-# INLINE dmstoMinuteOfHour #-}

instance FromJSON DescribeMaintenanceStartTimeResponse

instance AWSRequest DescribeMaintenanceStartTime where
    type Sv DescribeMaintenanceStartTime = StorageGateway
    type Rs DescribeMaintenanceStartTime = DescribeMaintenanceStartTimeResponse

    request = get
    response _ = jsonResponse
