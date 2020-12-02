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
-- Module      : Network.AWS.SageMaker.DeleteMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a monitoring schedule. Also stops the schedule had not already been stopped. This does not delete the job execution history of the monitoring schedule.
module Network.AWS.SageMaker.DeleteMonitoringSchedule
  ( -- * Creating a Request
    deleteMonitoringSchedule,
    DeleteMonitoringSchedule,

    -- * Request Lenses
    dMonitoringScheduleName,

    -- * Destructuring the Response
    deleteMonitoringScheduleResponse,
    DeleteMonitoringScheduleResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteMonitoringSchedule' smart constructor.
newtype DeleteMonitoringSchedule = DeleteMonitoringSchedule'
  { _dMonitoringScheduleName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMonitoringSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMonitoringScheduleName' - The name of the monitoring schedule to delete.
deleteMonitoringSchedule ::
  -- | 'dMonitoringScheduleName'
  Text ->
  DeleteMonitoringSchedule
deleteMonitoringSchedule pMonitoringScheduleName_ =
  DeleteMonitoringSchedule'
    { _dMonitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | The name of the monitoring schedule to delete.
dMonitoringScheduleName :: Lens' DeleteMonitoringSchedule Text
dMonitoringScheduleName = lens _dMonitoringScheduleName (\s a -> s {_dMonitoringScheduleName = a})

instance AWSRequest DeleteMonitoringSchedule where
  type Rs DeleteMonitoringSchedule = DeleteMonitoringScheduleResponse
  request = postJSON sageMaker
  response = receiveNull DeleteMonitoringScheduleResponse'

instance Hashable DeleteMonitoringSchedule

instance NFData DeleteMonitoringSchedule

instance ToHeaders DeleteMonitoringSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DeleteMonitoringSchedule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteMonitoringSchedule where
  toJSON DeleteMonitoringSchedule' {..} =
    object
      ( catMaybes
          [Just ("MonitoringScheduleName" .= _dMonitoringScheduleName)]
      )

instance ToPath DeleteMonitoringSchedule where
  toPath = const "/"

instance ToQuery DeleteMonitoringSchedule where
  toQuery = const mempty

-- | /See:/ 'deleteMonitoringScheduleResponse' smart constructor.
data DeleteMonitoringScheduleResponse = DeleteMonitoringScheduleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMonitoringScheduleResponse' with the minimum fields required to make a request.
deleteMonitoringScheduleResponse ::
  DeleteMonitoringScheduleResponse
deleteMonitoringScheduleResponse =
  DeleteMonitoringScheduleResponse'

instance NFData DeleteMonitoringScheduleResponse
