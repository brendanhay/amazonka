{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateMaintenanceStartTime
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation updates a gateway\'s weekly maintenance start time
-- information, including day and time of the week. The maintenance time is
-- the time in your gateway\'s time zone.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateMaintenanceStartTime.html>
module Network.AWS.StorageGateway.UpdateMaintenanceStartTime
    (
    -- * Request
      UpdateMaintenanceStartTime
    -- ** Request constructor
    , updateMaintenanceStartTime
    -- ** Request lenses
    , umstrqGatewayARN
    , umstrqHourOfDay
    , umstrqMinuteOfHour
    , umstrqDayOfWeek

    -- * Response
    , UpdateMaintenanceStartTimeResponse
    -- ** Response constructor
    , updateMaintenanceStartTimeResponse
    -- ** Response lenses
    , umstrsGatewayARN
    , umstrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the following fields:
--
-- -   UpdateMaintenanceStartTimeInput$DayOfWeek
-- -   UpdateMaintenanceStartTimeInput$HourOfDay
-- -   UpdateMaintenanceStartTimeInput$MinuteOfHour
--
-- /See:/ 'updateMaintenanceStartTime' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umstrqGatewayARN'
--
-- * 'umstrqHourOfDay'
--
-- * 'umstrqMinuteOfHour'
--
-- * 'umstrqDayOfWeek'
data UpdateMaintenanceStartTime = UpdateMaintenanceStartTime'
    { _umstrqGatewayARN   :: !Text
    , _umstrqHourOfDay    :: !Nat
    , _umstrqMinuteOfHour :: !Nat
    , _umstrqDayOfWeek    :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateMaintenanceStartTime' smart constructor.
updateMaintenanceStartTime :: Text -> Natural -> Natural -> Natural -> UpdateMaintenanceStartTime
updateMaintenanceStartTime pGatewayARN pHourOfDay pMinuteOfHour pDayOfWeek =
    UpdateMaintenanceStartTime'
    { _umstrqGatewayARN = pGatewayARN
    , _umstrqHourOfDay = _Nat # pHourOfDay
    , _umstrqMinuteOfHour = _Nat # pMinuteOfHour
    , _umstrqDayOfWeek = _Nat # pDayOfWeek
    }

-- | FIXME: Undocumented member.
umstrqGatewayARN :: Lens' UpdateMaintenanceStartTime Text
umstrqGatewayARN = lens _umstrqGatewayARN (\ s a -> s{_umstrqGatewayARN = a});

-- | The hour component of the maintenance start time represented as hh,
-- where /hh/ is the hour (00 to 23). The hour of the day is in the time
-- zone of the gateway.
umstrqHourOfDay :: Lens' UpdateMaintenanceStartTime Natural
umstrqHourOfDay = lens _umstrqHourOfDay (\ s a -> s{_umstrqHourOfDay = a}) . _Nat;

-- | The minute component of the maintenance start time represented as /mm/,
-- where /mm/ is the minute (00 to 59). The minute of the hour is in the
-- time zone of the gateway.
umstrqMinuteOfHour :: Lens' UpdateMaintenanceStartTime Natural
umstrqMinuteOfHour = lens _umstrqMinuteOfHour (\ s a -> s{_umstrqMinuteOfHour = a}) . _Nat;

-- | The maintenance start time day of the week.
umstrqDayOfWeek :: Lens' UpdateMaintenanceStartTime Natural
umstrqDayOfWeek = lens _umstrqDayOfWeek (\ s a -> s{_umstrqDayOfWeek = a}) . _Nat;

instance AWSRequest UpdateMaintenanceStartTime where
        type Sv UpdateMaintenanceStartTime = StorageGateway
        type Rs UpdateMaintenanceStartTime =
             UpdateMaintenanceStartTimeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateMaintenanceStartTimeResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance ToHeaders UpdateMaintenanceStartTime where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.UpdateMaintenanceStartTime"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateMaintenanceStartTime where
        toJSON UpdateMaintenanceStartTime'{..}
          = object
              ["GatewayARN" .= _umstrqGatewayARN,
               "HourOfDay" .= _umstrqHourOfDay,
               "MinuteOfHour" .= _umstrqMinuteOfHour,
               "DayOfWeek" .= _umstrqDayOfWeek]

instance ToPath UpdateMaintenanceStartTime where
        toPath = const "/"

instance ToQuery UpdateMaintenanceStartTime where
        toQuery = const mempty

-- | A JSON object containing the of the gateway whose maintenance start time
-- is updated.
--
-- /See:/ 'updateMaintenanceStartTimeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umstrsGatewayARN'
--
-- * 'umstrsStatus'
data UpdateMaintenanceStartTimeResponse = UpdateMaintenanceStartTimeResponse'
    { _umstrsGatewayARN :: !(Maybe Text)
    , _umstrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateMaintenanceStartTimeResponse' smart constructor.
updateMaintenanceStartTimeResponse :: Int -> UpdateMaintenanceStartTimeResponse
updateMaintenanceStartTimeResponse pStatus =
    UpdateMaintenanceStartTimeResponse'
    { _umstrsGatewayARN = Nothing
    , _umstrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
umstrsGatewayARN :: Lens' UpdateMaintenanceStartTimeResponse (Maybe Text)
umstrsGatewayARN = lens _umstrsGatewayARN (\ s a -> s{_umstrsGatewayARN = a});

-- | FIXME: Undocumented member.
umstrsStatus :: Lens' UpdateMaintenanceStartTimeResponse Int
umstrsStatus = lens _umstrsStatus (\ s a -> s{_umstrsStatus = a});
