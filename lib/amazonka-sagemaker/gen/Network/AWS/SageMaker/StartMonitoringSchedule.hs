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
-- Module      : Network.AWS.SageMaker.StartMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a previously stopped monitoring schedule.
module Network.AWS.SageMaker.StartMonitoringSchedule
  ( -- * Creating a Request
    startMonitoringSchedule,
    StartMonitoringSchedule,

    -- * Request Lenses
    sMonitoringScheduleName,

    -- * Destructuring the Response
    startMonitoringScheduleResponse,
    StartMonitoringScheduleResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'startMonitoringSchedule' smart constructor.
newtype StartMonitoringSchedule = StartMonitoringSchedule'
  { _sMonitoringScheduleName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMonitoringSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sMonitoringScheduleName' - The name of the schedule to start.
startMonitoringSchedule ::
  -- | 'sMonitoringScheduleName'
  Text ->
  StartMonitoringSchedule
startMonitoringSchedule pMonitoringScheduleName_ =
  StartMonitoringSchedule'
    { _sMonitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | The name of the schedule to start.
sMonitoringScheduleName :: Lens' StartMonitoringSchedule Text
sMonitoringScheduleName = lens _sMonitoringScheduleName (\s a -> s {_sMonitoringScheduleName = a})

instance AWSRequest StartMonitoringSchedule where
  type Rs StartMonitoringSchedule = StartMonitoringScheduleResponse
  request = postJSON sageMaker
  response = receiveNull StartMonitoringScheduleResponse'

instance Hashable StartMonitoringSchedule

instance NFData StartMonitoringSchedule

instance ToHeaders StartMonitoringSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.StartMonitoringSchedule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartMonitoringSchedule where
  toJSON StartMonitoringSchedule' {..} =
    object
      ( catMaybes
          [Just ("MonitoringScheduleName" .= _sMonitoringScheduleName)]
      )

instance ToPath StartMonitoringSchedule where
  toPath = const "/"

instance ToQuery StartMonitoringSchedule where
  toQuery = const mempty

-- | /See:/ 'startMonitoringScheduleResponse' smart constructor.
data StartMonitoringScheduleResponse = StartMonitoringScheduleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMonitoringScheduleResponse' with the minimum fields required to make a request.
startMonitoringScheduleResponse ::
  StartMonitoringScheduleResponse
startMonitoringScheduleResponse = StartMonitoringScheduleResponse'

instance NFData StartMonitoringScheduleResponse
