{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetMaintenanceWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about a specific a maintenance window execution.
module Network.AWS.SSM.GetMaintenanceWindowExecution
  ( -- * Creating a request
    GetMaintenanceWindowExecution (..),
    mkGetMaintenanceWindowExecution,

    -- ** Request lenses
    gmweWindowExecutionId,

    -- * Destructuring the response
    GetMaintenanceWindowExecutionResponse (..),
    mkGetMaintenanceWindowExecutionResponse,

    -- ** Response lenses
    gmwersStatus,
    gmwersStartTime,
    gmwersWindowExecutionId,
    gmwersStatusDetails,
    gmwersEndTime,
    gmwersTaskIds,
    gmwersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetMaintenanceWindowExecution' smart constructor.
newtype GetMaintenanceWindowExecution = GetMaintenanceWindowExecution'
  { windowExecutionId ::
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

-- | Creates a value of 'GetMaintenanceWindowExecution' with the minimum fields required to make a request.
--
-- * 'windowExecutionId' - The ID of the maintenance window execution that includes the task.
mkGetMaintenanceWindowExecution ::
  -- | 'windowExecutionId'
  Lude.Text ->
  GetMaintenanceWindowExecution
mkGetMaintenanceWindowExecution pWindowExecutionId_ =
  GetMaintenanceWindowExecution'
    { windowExecutionId =
        pWindowExecutionId_
    }

-- | The ID of the maintenance window execution that includes the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmweWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecution Lude.Text
gmweWindowExecutionId = Lens.lens (windowExecutionId :: GetMaintenanceWindowExecution -> Lude.Text) (\s a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecution)
{-# DEPRECATED gmweWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

instance Lude.AWSRequest GetMaintenanceWindowExecution where
  type
    Rs GetMaintenanceWindowExecution =
      GetMaintenanceWindowExecutionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowExecutionResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "StartTime")
            Lude.<*> (x Lude..?> "WindowExecutionId")
            Lude.<*> (x Lude..?> "StatusDetails")
            Lude.<*> (x Lude..?> "EndTime")
            Lude.<*> (x Lude..?> "TaskIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMaintenanceWindowExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetMaintenanceWindowExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMaintenanceWindowExecution where
  toJSON GetMaintenanceWindowExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WindowExecutionId" Lude..= windowExecutionId)]
      )

instance Lude.ToPath GetMaintenanceWindowExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMaintenanceWindowExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMaintenanceWindowExecutionResponse' smart constructor.
data GetMaintenanceWindowExecutionResponse = GetMaintenanceWindowExecutionResponse'
  { status ::
      Lude.Maybe
        MaintenanceWindowExecutionStatus,
    startTime ::
      Lude.Maybe
        Lude.Timestamp,
    windowExecutionId ::
      Lude.Maybe
        Lude.Text,
    statusDetails ::
      Lude.Maybe
        Lude.Text,
    endTime ::
      Lude.Maybe
        Lude.Timestamp,
    taskIds ::
      Lude.Maybe
        [Lude.Text],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMaintenanceWindowExecutionResponse' with the minimum fields required to make a request.
--
-- * 'endTime' - The time the maintenance window finished running.
-- * 'responseStatus' - The response status code.
-- * 'startTime' - The time the maintenance window started running.
-- * 'status' - The status of the maintenance window execution.
-- * 'statusDetails' - The details explaining the Status. Only available for certain status values.
-- * 'taskIds' - The ID of the task executions from the maintenance window execution.
-- * 'windowExecutionId' - The ID of the maintenance window execution.
mkGetMaintenanceWindowExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMaintenanceWindowExecutionResponse
mkGetMaintenanceWindowExecutionResponse pResponseStatus_ =
  GetMaintenanceWindowExecutionResponse'
    { status = Lude.Nothing,
      startTime = Lude.Nothing,
      windowExecutionId = Lude.Nothing,
      statusDetails = Lude.Nothing,
      endTime = Lude.Nothing,
      taskIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the maintenance window execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwersStatus :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Lude.Maybe MaintenanceWindowExecutionStatus)
gmwersStatus = Lens.lens (status :: GetMaintenanceWindowExecutionResponse -> Lude.Maybe MaintenanceWindowExecutionStatus) (\s a -> s {status = a} :: GetMaintenanceWindowExecutionResponse)
{-# DEPRECATED gmwersStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time the maintenance window started running.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwersStartTime :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Lude.Maybe Lude.Timestamp)
gmwersStartTime = Lens.lens (startTime :: GetMaintenanceWindowExecutionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: GetMaintenanceWindowExecutionResponse)
{-# DEPRECATED gmwersStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ID of the maintenance window execution.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwersWindowExecutionId :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Lude.Maybe Lude.Text)
gmwersWindowExecutionId = Lens.lens (windowExecutionId :: GetMaintenanceWindowExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowExecutionId = a} :: GetMaintenanceWindowExecutionResponse)
{-# DEPRECATED gmwersWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The details explaining the Status. Only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwersStatusDetails :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Lude.Maybe Lude.Text)
gmwersStatusDetails = Lens.lens (statusDetails :: GetMaintenanceWindowExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: GetMaintenanceWindowExecutionResponse)
{-# DEPRECATED gmwersStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The time the maintenance window finished running.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwersEndTime :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Lude.Maybe Lude.Timestamp)
gmwersEndTime = Lens.lens (endTime :: GetMaintenanceWindowExecutionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: GetMaintenanceWindowExecutionResponse)
{-# DEPRECATED gmwersEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The ID of the task executions from the maintenance window execution.
--
-- /Note:/ Consider using 'taskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwersTaskIds :: Lens.Lens' GetMaintenanceWindowExecutionResponse (Lude.Maybe [Lude.Text])
gmwersTaskIds = Lens.lens (taskIds :: GetMaintenanceWindowExecutionResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {taskIds = a} :: GetMaintenanceWindowExecutionResponse)
{-# DEPRECATED gmwersTaskIds "Use generic-lens or generic-optics with 'taskIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwersResponseStatus :: Lens.Lens' GetMaintenanceWindowExecutionResponse Lude.Int
gmwersResponseStatus = Lens.lens (responseStatus :: GetMaintenanceWindowExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMaintenanceWindowExecutionResponse)
{-# DEPRECATED gmwersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
