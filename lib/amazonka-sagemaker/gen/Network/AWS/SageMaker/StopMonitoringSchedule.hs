{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    StopMonitoringSchedule (..),
    mkStopMonitoringSchedule,

    -- ** Request lenses
    smsMonitoringScheduleName,

    -- * Destructuring the response
    StopMonitoringScheduleResponse (..),
    mkStopMonitoringScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStopMonitoringSchedule' smart constructor.
newtype StopMonitoringSchedule = StopMonitoringSchedule'
  { monitoringScheduleName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopMonitoringSchedule' with the minimum fields required to make a request.
--
-- * 'monitoringScheduleName' - The name of the schedule to stop.
mkStopMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Lude.Text ->
  StopMonitoringSchedule
mkStopMonitoringSchedule pMonitoringScheduleName_ =
  StopMonitoringSchedule'
    { monitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | The name of the schedule to stop.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsMonitoringScheduleName :: Lens.Lens' StopMonitoringSchedule Lude.Text
smsMonitoringScheduleName = Lens.lens (monitoringScheduleName :: StopMonitoringSchedule -> Lude.Text) (\s a -> s {monitoringScheduleName = a} :: StopMonitoringSchedule)
{-# DEPRECATED smsMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

instance Lude.AWSRequest StopMonitoringSchedule where
  type Rs StopMonitoringSchedule = StopMonitoringScheduleResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StopMonitoringScheduleResponse'

instance Lude.ToHeaders StopMonitoringSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StopMonitoringSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopMonitoringSchedule where
  toJSON StopMonitoringSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("MonitoringScheduleName" Lude..= monitoringScheduleName)
          ]
      )

instance Lude.ToPath StopMonitoringSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery StopMonitoringSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopMonitoringScheduleResponse' smart constructor.
data StopMonitoringScheduleResponse = StopMonitoringScheduleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopMonitoringScheduleResponse' with the minimum fields required to make a request.
mkStopMonitoringScheduleResponse ::
  StopMonitoringScheduleResponse
mkStopMonitoringScheduleResponse = StopMonitoringScheduleResponse'
