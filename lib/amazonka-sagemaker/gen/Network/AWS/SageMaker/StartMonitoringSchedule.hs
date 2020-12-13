{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    StartMonitoringSchedule (..),
    mkStartMonitoringSchedule,

    -- ** Request lenses
    sMonitoringScheduleName,

    -- * Destructuring the response
    StartMonitoringScheduleResponse (..),
    mkStartMonitoringScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStartMonitoringSchedule' smart constructor.
newtype StartMonitoringSchedule = StartMonitoringSchedule'
  { -- | The name of the schedule to start.
    monitoringScheduleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMonitoringSchedule' with the minimum fields required to make a request.
--
-- * 'monitoringScheduleName' - The name of the schedule to start.
mkStartMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Lude.Text ->
  StartMonitoringSchedule
mkStartMonitoringSchedule pMonitoringScheduleName_ =
  StartMonitoringSchedule'
    { monitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | The name of the schedule to start.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMonitoringScheduleName :: Lens.Lens' StartMonitoringSchedule Lude.Text
sMonitoringScheduleName = Lens.lens (monitoringScheduleName :: StartMonitoringSchedule -> Lude.Text) (\s a -> s {monitoringScheduleName = a} :: StartMonitoringSchedule)
{-# DEPRECATED sMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

instance Lude.AWSRequest StartMonitoringSchedule where
  type Rs StartMonitoringSchedule = StartMonitoringScheduleResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StartMonitoringScheduleResponse'

instance Lude.ToHeaders StartMonitoringSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StartMonitoringSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMonitoringSchedule where
  toJSON StartMonitoringSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("MonitoringScheduleName" Lude..= monitoringScheduleName)
          ]
      )

instance Lude.ToPath StartMonitoringSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMonitoringSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartMonitoringScheduleResponse' smart constructor.
data StartMonitoringScheduleResponse = StartMonitoringScheduleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMonitoringScheduleResponse' with the minimum fields required to make a request.
mkStartMonitoringScheduleResponse ::
  StartMonitoringScheduleResponse
mkStartMonitoringScheduleResponse =
  StartMonitoringScheduleResponse'
