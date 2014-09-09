{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.UpdateMaintenanceStartTime
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates a gateway's weekly maintenance start time
-- information, including day and time of the week. The maintenance time is
-- the time in your gateway's time zone. Example Request The following example
-- shows a request that updates the maintenance start time of mygateway. POST
-- / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateMaintenanceStartTime { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "HourOfDay": 0, "MinuteOfHour": 30, "DayOfWeek": 2 } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 80 { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.V2013_06_30.UpdateMaintenanceStartTime
    (
    -- * Request
      UpdateMaintenanceStartTime
    -- ** Request constructor
    , mkUpdateMaintenanceStartTime
    -- ** Request lenses
    , umstGatewayARN
    , umstHourOfDay
    , umstMinuteOfHour
    , umstDayOfWeek

    -- * Response
    , UpdateMaintenanceStartTimeResponse
    -- ** Response constructor
    , mkUpdateMaintenanceStartTimeResponse
    -- ** Response lenses
    , umstrGatewayARN
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing the following fields:
-- UpdateMaintenanceStartTimeInput$DayOfWeek
-- UpdateMaintenanceStartTimeInput$HourOfDay
-- UpdateMaintenanceStartTimeInput$MinuteOfHour.
data UpdateMaintenanceStartTime = UpdateMaintenanceStartTime
    { _umstGatewayARN :: Text
    , _umstHourOfDay :: Integer
    , _umstMinuteOfHour :: Integer
    , _umstDayOfWeek :: Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateMaintenanceStartTime' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
-- * @HourOfDay ::@ @Integer@
--
-- * @MinuteOfHour ::@ @Integer@
--
-- * @DayOfWeek ::@ @Integer@
--
mkUpdateMaintenanceStartTime :: Text -- ^ 'umstGatewayARN'
                             -> Integer -- ^ 'umstHourOfDay'
                             -> Integer -- ^ 'umstMinuteOfHour'
                             -> Integer -- ^ 'umstDayOfWeek'
                             -> UpdateMaintenanceStartTime
mkUpdateMaintenanceStartTime p1 p2 p3 p4 = UpdateMaintenanceStartTime
    { _umstGatewayARN = p1
    , _umstHourOfDay = p2
    , _umstMinuteOfHour = p3
    , _umstDayOfWeek = p4
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
umstGatewayARN :: Lens' UpdateMaintenanceStartTime Text
umstGatewayARN = lens _umstGatewayARN (\s a -> s { _umstGatewayARN = a })

-- | The hour component of the maintenance start time represented as hh, where
-- hh is the hour (00 to 23). The hour of the day is in the time zone of the
-- gateway.
umstHourOfDay :: Lens' UpdateMaintenanceStartTime Integer
umstHourOfDay = lens _umstHourOfDay (\s a -> s { _umstHourOfDay = a })

-- | The minute component of the maintenance start time represented as mm, where
-- mm is the minute (00 to 59). The minute of the hour is in the time zone of
-- the gateway.
umstMinuteOfHour :: Lens' UpdateMaintenanceStartTime Integer
umstMinuteOfHour =
    lens _umstMinuteOfHour (\s a -> s { _umstMinuteOfHour = a })

-- | The maintenance start time day of the week.
umstDayOfWeek :: Lens' UpdateMaintenanceStartTime Integer
umstDayOfWeek = lens _umstDayOfWeek (\s a -> s { _umstDayOfWeek = a })

instance ToPath UpdateMaintenanceStartTime

instance ToQuery UpdateMaintenanceStartTime

instance ToHeaders UpdateMaintenanceStartTime

instance ToJSON UpdateMaintenanceStartTime

-- | A JSON object containing the of the gateway whose maintenance start time is
-- updated.
newtype UpdateMaintenanceStartTimeResponse = UpdateMaintenanceStartTimeResponse
    { _umstrGatewayARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateMaintenanceStartTimeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
mkUpdateMaintenanceStartTimeResponse :: Text -- ^ 'umstrGatewayARN'
                                     -> UpdateMaintenanceStartTimeResponse
mkUpdateMaintenanceStartTimeResponse p1 = UpdateMaintenanceStartTimeResponse
    { _umstrGatewayARN = p1
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
umstrGatewayARN :: Lens' UpdateMaintenanceStartTimeResponse Text
umstrGatewayARN = lens _umstrGatewayARN (\s a -> s { _umstrGatewayARN = a })

instance FromJSON UpdateMaintenanceStartTimeResponse

instance AWSRequest UpdateMaintenanceStartTime where
    type Sv UpdateMaintenanceStartTime = StorageGateway
    type Rs UpdateMaintenanceStartTime = UpdateMaintenanceStartTimeResponse

    request = get
    response _ = jsonResponse
