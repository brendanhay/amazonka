{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    DeleteMonitoringSchedule (..),
    mkDeleteMonitoringSchedule,

    -- ** Request lenses
    dMonitoringScheduleName,

    -- * Destructuring the response
    DeleteMonitoringScheduleResponse (..),
    mkDeleteMonitoringScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteMonitoringSchedule' smart constructor.
newtype DeleteMonitoringSchedule = DeleteMonitoringSchedule'
  { -- | The name of the monitoring schedule to delete.
    monitoringScheduleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMonitoringSchedule' with the minimum fields required to make a request.
--
-- * 'monitoringScheduleName' - The name of the monitoring schedule to delete.
mkDeleteMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Lude.Text ->
  DeleteMonitoringSchedule
mkDeleteMonitoringSchedule pMonitoringScheduleName_ =
  DeleteMonitoringSchedule'
    { monitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | The name of the monitoring schedule to delete.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMonitoringScheduleName :: Lens.Lens' DeleteMonitoringSchedule Lude.Text
dMonitoringScheduleName = Lens.lens (monitoringScheduleName :: DeleteMonitoringSchedule -> Lude.Text) (\s a -> s {monitoringScheduleName = a} :: DeleteMonitoringSchedule)
{-# DEPRECATED dMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

instance Lude.AWSRequest DeleteMonitoringSchedule where
  type Rs DeleteMonitoringSchedule = DeleteMonitoringScheduleResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteMonitoringScheduleResponse'

instance Lude.ToHeaders DeleteMonitoringSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteMonitoringSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMonitoringSchedule where
  toJSON DeleteMonitoringSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("MonitoringScheduleName" Lude..= monitoringScheduleName)
          ]
      )

instance Lude.ToPath DeleteMonitoringSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMonitoringSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMonitoringScheduleResponse' smart constructor.
data DeleteMonitoringScheduleResponse = DeleteMonitoringScheduleResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMonitoringScheduleResponse' with the minimum fields required to make a request.
mkDeleteMonitoringScheduleResponse ::
  DeleteMonitoringScheduleResponse
mkDeleteMonitoringScheduleResponse =
  DeleteMonitoringScheduleResponse'
