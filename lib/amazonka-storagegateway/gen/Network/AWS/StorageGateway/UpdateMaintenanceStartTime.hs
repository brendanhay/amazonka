{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateMaintenanceStartTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway's weekly maintenance start time information, including day and time of the week. The maintenance time is the time in your gateway's time zone.
module Network.AWS.StorageGateway.UpdateMaintenanceStartTime
  ( -- * Creating a Request
    updateMaintenanceStartTime,
    UpdateMaintenanceStartTime,

    -- * Request Lenses
    umstDayOfMonth,
    umstDayOfWeek,
    umstGatewayARN,
    umstHourOfDay,
    umstMinuteOfHour,

    -- * Destructuring the Response
    updateMaintenanceStartTimeResponse,
    UpdateMaintenanceStartTimeResponse,

    -- * Response Lenses
    umstrsGatewayARN,
    umstrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the following fields:
--
--
--     * 'UpdateMaintenanceStartTimeInput$DayOfMonth'
--
--     * 'UpdateMaintenanceStartTimeInput$DayOfWeek'
--
--     * 'UpdateMaintenanceStartTimeInput$HourOfDay'
--
--     * 'UpdateMaintenanceStartTimeInput$MinuteOfHour'
--
--
--
--
-- /See:/ 'updateMaintenanceStartTime' smart constructor.
data UpdateMaintenanceStartTime = UpdateMaintenanceStartTime'
  { _umstDayOfMonth ::
      !(Maybe Nat),
    _umstDayOfWeek :: !(Maybe Nat),
    _umstGatewayARN :: !Text,
    _umstHourOfDay :: !Nat,
    _umstMinuteOfHour :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMaintenanceStartTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umstDayOfMonth' - The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
--
-- * 'umstDayOfWeek' - The day of the week component of the maintenance start time week represented as an ordinal number from 0 to 6, where 0 represents Sunday and 6 Saturday.
--
-- * 'umstGatewayARN' - Undocumented member.
--
-- * 'umstHourOfDay' - The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (00 to 23). The hour of the day is in the time zone of the gateway.
--
-- * 'umstMinuteOfHour' - The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (00 to 59). The minute of the hour is in the time zone of the gateway.
updateMaintenanceStartTime ::
  -- | 'umstGatewayARN'
  Text ->
  -- | 'umstHourOfDay'
  Natural ->
  -- | 'umstMinuteOfHour'
  Natural ->
  UpdateMaintenanceStartTime
updateMaintenanceStartTime pGatewayARN_ pHourOfDay_ pMinuteOfHour_ =
  UpdateMaintenanceStartTime'
    { _umstDayOfMonth = Nothing,
      _umstDayOfWeek = Nothing,
      _umstGatewayARN = pGatewayARN_,
      _umstHourOfDay = _Nat # pHourOfDay_,
      _umstMinuteOfHour = _Nat # pMinuteOfHour_
    }

-- | The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
umstDayOfMonth :: Lens' UpdateMaintenanceStartTime (Maybe Natural)
umstDayOfMonth = lens _umstDayOfMonth (\s a -> s {_umstDayOfMonth = a}) . mapping _Nat

-- | The day of the week component of the maintenance start time week represented as an ordinal number from 0 to 6, where 0 represents Sunday and 6 Saturday.
umstDayOfWeek :: Lens' UpdateMaintenanceStartTime (Maybe Natural)
umstDayOfWeek = lens _umstDayOfWeek (\s a -> s {_umstDayOfWeek = a}) . mapping _Nat

-- | Undocumented member.
umstGatewayARN :: Lens' UpdateMaintenanceStartTime Text
umstGatewayARN = lens _umstGatewayARN (\s a -> s {_umstGatewayARN = a})

-- | The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (00 to 23). The hour of the day is in the time zone of the gateway.
umstHourOfDay :: Lens' UpdateMaintenanceStartTime Natural
umstHourOfDay = lens _umstHourOfDay (\s a -> s {_umstHourOfDay = a}) . _Nat

-- | The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (00 to 59). The minute of the hour is in the time zone of the gateway.
umstMinuteOfHour :: Lens' UpdateMaintenanceStartTime Natural
umstMinuteOfHour = lens _umstMinuteOfHour (\s a -> s {_umstMinuteOfHour = a}) . _Nat

instance AWSRequest UpdateMaintenanceStartTime where
  type
    Rs UpdateMaintenanceStartTime =
      UpdateMaintenanceStartTimeResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          UpdateMaintenanceStartTimeResponse'
            <$> (x .?> "GatewayARN") <*> (pure (fromEnum s))
      )

instance Hashable UpdateMaintenanceStartTime

instance NFData UpdateMaintenanceStartTime

instance ToHeaders UpdateMaintenanceStartTime where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StorageGateway_20130630.UpdateMaintenanceStartTime" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateMaintenanceStartTime where
  toJSON UpdateMaintenanceStartTime' {..} =
    object
      ( catMaybes
          [ ("DayOfMonth" .=) <$> _umstDayOfMonth,
            ("DayOfWeek" .=) <$> _umstDayOfWeek,
            Just ("GatewayARN" .= _umstGatewayARN),
            Just ("HourOfDay" .= _umstHourOfDay),
            Just ("MinuteOfHour" .= _umstMinuteOfHour)
          ]
      )

instance ToPath UpdateMaintenanceStartTime where
  toPath = const "/"

instance ToQuery UpdateMaintenanceStartTime where
  toQuery = const mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway whose maintenance start time is updated.
--
--
--
-- /See:/ 'updateMaintenanceStartTimeResponse' smart constructor.
data UpdateMaintenanceStartTimeResponse = UpdateMaintenanceStartTimeResponse'
  { _umstrsGatewayARN ::
      !(Maybe Text),
    _umstrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMaintenanceStartTimeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umstrsGatewayARN' - Undocumented member.
--
-- * 'umstrsResponseStatus' - -- | The response status code.
updateMaintenanceStartTimeResponse ::
  -- | 'umstrsResponseStatus'
  Int ->
  UpdateMaintenanceStartTimeResponse
updateMaintenanceStartTimeResponse pResponseStatus_ =
  UpdateMaintenanceStartTimeResponse'
    { _umstrsGatewayARN = Nothing,
      _umstrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
umstrsGatewayARN :: Lens' UpdateMaintenanceStartTimeResponse (Maybe Text)
umstrsGatewayARN = lens _umstrsGatewayARN (\s a -> s {_umstrsGatewayARN = a})

-- | -- | The response status code.
umstrsResponseStatus :: Lens' UpdateMaintenanceStartTimeResponse Int
umstrsResponseStatus = lens _umstrsResponseStatus (\s a -> s {_umstrsResponseStatus = a})

instance NFData UpdateMaintenanceStartTimeResponse
