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
    , umstGatewayARN
    , umstHourOfDay
    , umstMinuteOfHour
    , umstDayOfWeek

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
-- * 'umstGatewayARN'
--
-- * 'umstHourOfDay'
--
-- * 'umstMinuteOfHour'
--
-- * 'umstDayOfWeek'
data UpdateMaintenanceStartTime = UpdateMaintenanceStartTime'
    { _umstGatewayARN   :: !Text
    , _umstHourOfDay    :: !Nat
    , _umstMinuteOfHour :: !Nat
    , _umstDayOfWeek    :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateMaintenanceStartTime' smart constructor.
updateMaintenanceStartTime :: Text -> Natural -> Natural -> Natural -> UpdateMaintenanceStartTime
updateMaintenanceStartTime pGatewayARN_ pHourOfDay_ pMinuteOfHour_ pDayOfWeek_ =
    UpdateMaintenanceStartTime'
    { _umstGatewayARN = pGatewayARN_
    , _umstHourOfDay = _Nat # pHourOfDay_
    , _umstMinuteOfHour = _Nat # pMinuteOfHour_
    , _umstDayOfWeek = _Nat # pDayOfWeek_
    }

-- | FIXME: Undocumented member.
umstGatewayARN :: Lens' UpdateMaintenanceStartTime Text
umstGatewayARN = lens _umstGatewayARN (\ s a -> s{_umstGatewayARN = a});

-- | The hour component of the maintenance start time represented as hh,
-- where /hh/ is the hour (00 to 23). The hour of the day is in the time
-- zone of the gateway.
umstHourOfDay :: Lens' UpdateMaintenanceStartTime Natural
umstHourOfDay = lens _umstHourOfDay (\ s a -> s{_umstHourOfDay = a}) . _Nat;

-- | The minute component of the maintenance start time represented as /mm/,
-- where /mm/ is the minute (00 to 59). The minute of the hour is in the
-- time zone of the gateway.
umstMinuteOfHour :: Lens' UpdateMaintenanceStartTime Natural
umstMinuteOfHour = lens _umstMinuteOfHour (\ s a -> s{_umstMinuteOfHour = a}) . _Nat;

-- | The maintenance start time day of the week.
umstDayOfWeek :: Lens' UpdateMaintenanceStartTime Natural
umstDayOfWeek = lens _umstDayOfWeek (\ s a -> s{_umstDayOfWeek = a}) . _Nat;

instance AWSRequest UpdateMaintenanceStartTime where
        type Sv UpdateMaintenanceStartTime = StorageGateway
        type Rs UpdateMaintenanceStartTime =
             UpdateMaintenanceStartTimeResponse
        request = postJSON "UpdateMaintenanceStartTime"
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
              ["GatewayARN" .= _umstGatewayARN,
               "HourOfDay" .= _umstHourOfDay,
               "MinuteOfHour" .= _umstMinuteOfHour,
               "DayOfWeek" .= _umstDayOfWeek]

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
updateMaintenanceStartTimeResponse pStatus_ =
    UpdateMaintenanceStartTimeResponse'
    { _umstrsGatewayARN = Nothing
    , _umstrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
umstrsGatewayARN :: Lens' UpdateMaintenanceStartTimeResponse (Maybe Text)
umstrsGatewayARN = lens _umstrsGatewayARN (\ s a -> s{_umstrsGatewayARN = a});

-- | FIXME: Undocumented member.
umstrsStatus :: Lens' UpdateMaintenanceStartTimeResponse Int
umstrsStatus = lens _umstrsStatus (\ s a -> s{_umstrsStatus = a});
