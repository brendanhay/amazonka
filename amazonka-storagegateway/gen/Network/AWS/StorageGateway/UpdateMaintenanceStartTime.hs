{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.StorageGateway.UpdateMaintenanceStartTime
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
-- the time in your gateway's time zone.
module Network.AWS.StorageGateway.UpdateMaintenanceStartTime
    (
    -- * Request
      UpdateMaintenanceStartTime
    -- ** Request constructor
    , updateMaintenanceStartTime
    -- ** Request lenses
    , umstDayOfWeek
    , umstGatewayARN
    , umstHourOfDay
    , umstMinuteOfHour

    -- * Response
    , UpdateMaintenanceStartTimeResponse
    -- ** Response constructor
    , updateMaintenanceStartTimeResponse
    -- ** Response lenses
    , umstrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

data UpdateMaintenanceStartTime = UpdateMaintenanceStartTime
    { _umstDayOfWeek    :: Natural
    , _umstGatewayARN   :: Text
    , _umstHourOfDay    :: Natural
    , _umstMinuteOfHour :: Natural
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateMaintenanceStartTime' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umstDayOfWeek' @::@ 'Natural'
--
-- * 'umstGatewayARN' @::@ 'Text'
--
-- * 'umstHourOfDay' @::@ 'Natural'
--
-- * 'umstMinuteOfHour' @::@ 'Natural'
--
updateMaintenanceStartTime :: Text -- ^ 'umstGatewayARN'
                           -> Natural -- ^ 'umstHourOfDay'
                           -> Natural -- ^ 'umstMinuteOfHour'
                           -> Natural -- ^ 'umstDayOfWeek'
                           -> UpdateMaintenanceStartTime
updateMaintenanceStartTime p1 p2 p3 p4 = UpdateMaintenanceStartTime
    { _umstGatewayARN   = p1
    , _umstHourOfDay    = p2
    , _umstMinuteOfHour = p3
    , _umstDayOfWeek    = p4
    }

-- | The maintenance start time day of the week.
umstDayOfWeek :: Lens' UpdateMaintenanceStartTime Natural
umstDayOfWeek = lens _umstDayOfWeek (\s a -> s { _umstDayOfWeek = a })

umstGatewayARN :: Lens' UpdateMaintenanceStartTime Text
umstGatewayARN = lens _umstGatewayARN (\s a -> s { _umstGatewayARN = a })

-- | The hour component of the maintenance start time represented as hh, where
-- hh is the hour (00 to 23). The hour of the day is in the time zone of the
-- gateway.
umstHourOfDay :: Lens' UpdateMaintenanceStartTime Natural
umstHourOfDay = lens _umstHourOfDay (\s a -> s { _umstHourOfDay = a })

-- | The minute component of the maintenance start time represented as mm,
-- where mm is the minute (00 to 59). The minute of the hour is in the time
-- zone of the gateway.
umstMinuteOfHour :: Lens' UpdateMaintenanceStartTime Natural
umstMinuteOfHour = lens _umstMinuteOfHour (\s a -> s { _umstMinuteOfHour = a })

instance ToPath UpdateMaintenanceStartTime where
    toPath = const "/"

instance ToQuery UpdateMaintenanceStartTime where
    toQuery = const mempty

instance ToHeaders UpdateMaintenanceStartTime

instance ToBody UpdateMaintenanceStartTime where
    toBody = toBody . encode . _umstGatewayARN

newtype UpdateMaintenanceStartTimeResponse = UpdateMaintenanceStartTimeResponse
    { _umstrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'UpdateMaintenanceStartTimeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umstrGatewayARN' @::@ 'Maybe' 'Text'
--
updateMaintenanceStartTimeResponse :: UpdateMaintenanceStartTimeResponse
updateMaintenanceStartTimeResponse = UpdateMaintenanceStartTimeResponse
    { _umstrGatewayARN = Nothing
    }

umstrGatewayARN :: Lens' UpdateMaintenanceStartTimeResponse (Maybe Text)
umstrGatewayARN = lens _umstrGatewayARN (\s a -> s { _umstrGatewayARN = a })

instance AWSRequest UpdateMaintenanceStartTime where
    type Sv UpdateMaintenanceStartTime = StorageGateway
    type Rs UpdateMaintenanceStartTime = UpdateMaintenanceStartTimeResponse

    request  = post
    response = jsonResponse $ \h o -> UpdateMaintenanceStartTimeResponse
        <$> o .: "GatewayARN"
