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
-- Module      : Network.AWS.SageMaker.StopMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a previously started monitoring schedule.
module Network.AWS.SageMaker.StopMonitoringSchedule
  ( -- * Creating a Request
    stopMonitoringSchedule,
    StopMonitoringSchedule,

    -- * Request Lenses
    smsMonitoringScheduleName,

    -- * Destructuring the Response
    stopMonitoringScheduleResponse,
    StopMonitoringScheduleResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'stopMonitoringSchedule' smart constructor.
newtype StopMonitoringSchedule = StopMonitoringSchedule'
  { _smsMonitoringScheduleName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopMonitoringSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smsMonitoringScheduleName' - The name of the schedule to stop.
stopMonitoringSchedule ::
  -- | 'smsMonitoringScheduleName'
  Text ->
  StopMonitoringSchedule
stopMonitoringSchedule pMonitoringScheduleName_ =
  StopMonitoringSchedule'
    { _smsMonitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | The name of the schedule to stop.
smsMonitoringScheduleName :: Lens' StopMonitoringSchedule Text
smsMonitoringScheduleName = lens _smsMonitoringScheduleName (\s a -> s {_smsMonitoringScheduleName = a})

instance AWSRequest StopMonitoringSchedule where
  type Rs StopMonitoringSchedule = StopMonitoringScheduleResponse
  request = postJSON sageMaker
  response = receiveNull StopMonitoringScheduleResponse'

instance Hashable StopMonitoringSchedule

instance NFData StopMonitoringSchedule

instance ToHeaders StopMonitoringSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.StopMonitoringSchedule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopMonitoringSchedule where
  toJSON StopMonitoringSchedule' {..} =
    object
      ( catMaybes
          [Just ("MonitoringScheduleName" .= _smsMonitoringScheduleName)]
      )

instance ToPath StopMonitoringSchedule where
  toPath = const "/"

instance ToQuery StopMonitoringSchedule where
  toQuery = const mempty

-- | /See:/ 'stopMonitoringScheduleResponse' smart constructor.
data StopMonitoringScheduleResponse = StopMonitoringScheduleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopMonitoringScheduleResponse' with the minimum fields required to make a request.
stopMonitoringScheduleResponse ::
  StopMonitoringScheduleResponse
stopMonitoringScheduleResponse = StopMonitoringScheduleResponse'

instance NFData StopMonitoringScheduleResponse
