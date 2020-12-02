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
-- Module      : Network.AWS.SageMaker.UpdateMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created schedule.
module Network.AWS.SageMaker.UpdateMonitoringSchedule
  ( -- * Creating a Request
    updateMonitoringSchedule,
    UpdateMonitoringSchedule,

    -- * Request Lenses
    umsMonitoringScheduleName,
    umsMonitoringScheduleConfig,

    -- * Destructuring the Response
    updateMonitoringScheduleResponse,
    UpdateMonitoringScheduleResponse,

    -- * Response Lenses
    umsrsResponseStatus,
    umsrsMonitoringScheduleARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'updateMonitoringSchedule' smart constructor.
data UpdateMonitoringSchedule = UpdateMonitoringSchedule'
  { _umsMonitoringScheduleName ::
      !Text,
    _umsMonitoringScheduleConfig ::
      !MonitoringScheduleConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMonitoringSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umsMonitoringScheduleName' - The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
--
-- * 'umsMonitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and defines the monitoring job.
updateMonitoringSchedule ::
  -- | 'umsMonitoringScheduleName'
  Text ->
  -- | 'umsMonitoringScheduleConfig'
  MonitoringScheduleConfig ->
  UpdateMonitoringSchedule
updateMonitoringSchedule
  pMonitoringScheduleName_
  pMonitoringScheduleConfig_ =
    UpdateMonitoringSchedule'
      { _umsMonitoringScheduleName =
          pMonitoringScheduleName_,
        _umsMonitoringScheduleConfig = pMonitoringScheduleConfig_
      }

-- | The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
umsMonitoringScheduleName :: Lens' UpdateMonitoringSchedule Text
umsMonitoringScheduleName = lens _umsMonitoringScheduleName (\s a -> s {_umsMonitoringScheduleName = a})

-- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
umsMonitoringScheduleConfig :: Lens' UpdateMonitoringSchedule MonitoringScheduleConfig
umsMonitoringScheduleConfig = lens _umsMonitoringScheduleConfig (\s a -> s {_umsMonitoringScheduleConfig = a})

instance AWSRequest UpdateMonitoringSchedule where
  type Rs UpdateMonitoringSchedule = UpdateMonitoringScheduleResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          UpdateMonitoringScheduleResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "MonitoringScheduleArn")
      )

instance Hashable UpdateMonitoringSchedule

instance NFData UpdateMonitoringSchedule

instance ToHeaders UpdateMonitoringSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.UpdateMonitoringSchedule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateMonitoringSchedule where
  toJSON UpdateMonitoringSchedule' {..} =
    object
      ( catMaybes
          [ Just ("MonitoringScheduleName" .= _umsMonitoringScheduleName),
            Just ("MonitoringScheduleConfig" .= _umsMonitoringScheduleConfig)
          ]
      )

instance ToPath UpdateMonitoringSchedule where
  toPath = const "/"

instance ToQuery UpdateMonitoringSchedule where
  toQuery = const mempty

-- | /See:/ 'updateMonitoringScheduleResponse' smart constructor.
data UpdateMonitoringScheduleResponse = UpdateMonitoringScheduleResponse'
  { _umsrsResponseStatus ::
      !Int,
    _umsrsMonitoringScheduleARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMonitoringScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umsrsResponseStatus' - -- | The response status code.
--
-- * 'umsrsMonitoringScheduleARN' - The Amazon Resource Name (ARN) of the monitoring schedule.
updateMonitoringScheduleResponse ::
  -- | 'umsrsResponseStatus'
  Int ->
  -- | 'umsrsMonitoringScheduleARN'
  Text ->
  UpdateMonitoringScheduleResponse
updateMonitoringScheduleResponse
  pResponseStatus_
  pMonitoringScheduleARN_ =
    UpdateMonitoringScheduleResponse'
      { _umsrsResponseStatus =
          pResponseStatus_,
        _umsrsMonitoringScheduleARN = pMonitoringScheduleARN_
      }

-- | -- | The response status code.
umsrsResponseStatus :: Lens' UpdateMonitoringScheduleResponse Int
umsrsResponseStatus = lens _umsrsResponseStatus (\s a -> s {_umsrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
umsrsMonitoringScheduleARN :: Lens' UpdateMonitoringScheduleResponse Text
umsrsMonitoringScheduleARN = lens _umsrsMonitoringScheduleARN (\s a -> s {_umsrsMonitoringScheduleARN = a})

instance NFData UpdateMonitoringScheduleResponse
