{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuditTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender audit.
module Network.AWS.IoT.DescribeAuditTask
  ( -- * Creating a request
    DescribeAuditTask (..),
    mkDescribeAuditTask,

    -- ** Request lenses
    datTaskId,

    -- * Destructuring the response
    DescribeAuditTaskResponse (..),
    mkDescribeAuditTaskResponse,

    -- ** Response lenses
    datrsAuditDetails,
    datrsTaskType,
    datrsTaskStartTime,
    datrsTaskStatistics,
    datrsScheduledAuditName,
    datrsTaskStatus,
    datrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAuditTask' smart constructor.
newtype DescribeAuditTask = DescribeAuditTask'
  { -- | The ID of the audit whose information you want to get.
    taskId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAuditTask' with the minimum fields required to make a request.
--
-- * 'taskId' - The ID of the audit whose information you want to get.
mkDescribeAuditTask ::
  -- | 'taskId'
  Lude.Text ->
  DescribeAuditTask
mkDescribeAuditTask pTaskId_ =
  DescribeAuditTask' {taskId = pTaskId_}

-- | The ID of the audit whose information you want to get.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datTaskId :: Lens.Lens' DescribeAuditTask Lude.Text
datTaskId = Lens.lens (taskId :: DescribeAuditTask -> Lude.Text) (\s a -> s {taskId = a} :: DescribeAuditTask)
{-# DEPRECATED datTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest DescribeAuditTask where
  type Rs DescribeAuditTask = DescribeAuditTaskResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAuditTaskResponse'
            Lude.<$> (x Lude..?> "auditDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "taskType")
            Lude.<*> (x Lude..?> "taskStartTime")
            Lude.<*> (x Lude..?> "taskStatistics")
            Lude.<*> (x Lude..?> "scheduledAuditName")
            Lude.<*> (x Lude..?> "taskStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAuditTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAuditTask where
  toPath DescribeAuditTask' {..} =
    Lude.mconcat ["/audit/tasks/", Lude.toBS taskId]

instance Lude.ToQuery DescribeAuditTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAuditTaskResponse' smart constructor.
data DescribeAuditTaskResponse = DescribeAuditTaskResponse'
  { -- | Detailed information about each check performed during this audit.
    auditDetails :: Lude.Maybe (Lude.HashMap Lude.Text (AuditCheckDetails)),
    -- | The type of audit: "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
    taskType :: Lude.Maybe AuditTaskType,
    -- | The time the audit started.
    taskStartTime :: Lude.Maybe Lude.Timestamp,
    -- | Statistical information about the audit.
    taskStatistics :: Lude.Maybe TaskStatistics,
    -- | The name of the scheduled audit (only if the audit was a scheduled audit).
    scheduledAuditName :: Lude.Maybe Lude.Text,
    -- | The status of the audit: one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
    taskStatus :: Lude.Maybe AuditTaskStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAuditTaskResponse' with the minimum fields required to make a request.
--
-- * 'auditDetails' - Detailed information about each check performed during this audit.
-- * 'taskType' - The type of audit: "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
-- * 'taskStartTime' - The time the audit started.
-- * 'taskStatistics' - Statistical information about the audit.
-- * 'scheduledAuditName' - The name of the scheduled audit (only if the audit was a scheduled audit).
-- * 'taskStatus' - The status of the audit: one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
-- * 'responseStatus' - The response status code.
mkDescribeAuditTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAuditTaskResponse
mkDescribeAuditTaskResponse pResponseStatus_ =
  DescribeAuditTaskResponse'
    { auditDetails = Lude.Nothing,
      taskType = Lude.Nothing,
      taskStartTime = Lude.Nothing,
      taskStatistics = Lude.Nothing,
      scheduledAuditName = Lude.Nothing,
      taskStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Detailed information about each check performed during this audit.
--
-- /Note:/ Consider using 'auditDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsAuditDetails :: Lens.Lens' DescribeAuditTaskResponse (Lude.Maybe (Lude.HashMap Lude.Text (AuditCheckDetails)))
datrsAuditDetails = Lens.lens (auditDetails :: DescribeAuditTaskResponse -> Lude.Maybe (Lude.HashMap Lude.Text (AuditCheckDetails))) (\s a -> s {auditDetails = a} :: DescribeAuditTaskResponse)
{-# DEPRECATED datrsAuditDetails "Use generic-lens or generic-optics with 'auditDetails' instead." #-}

-- | The type of audit: "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsTaskType :: Lens.Lens' DescribeAuditTaskResponse (Lude.Maybe AuditTaskType)
datrsTaskType = Lens.lens (taskType :: DescribeAuditTaskResponse -> Lude.Maybe AuditTaskType) (\s a -> s {taskType = a} :: DescribeAuditTaskResponse)
{-# DEPRECATED datrsTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The time the audit started.
--
-- /Note:/ Consider using 'taskStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsTaskStartTime :: Lens.Lens' DescribeAuditTaskResponse (Lude.Maybe Lude.Timestamp)
datrsTaskStartTime = Lens.lens (taskStartTime :: DescribeAuditTaskResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {taskStartTime = a} :: DescribeAuditTaskResponse)
{-# DEPRECATED datrsTaskStartTime "Use generic-lens or generic-optics with 'taskStartTime' instead." #-}

-- | Statistical information about the audit.
--
-- /Note:/ Consider using 'taskStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsTaskStatistics :: Lens.Lens' DescribeAuditTaskResponse (Lude.Maybe TaskStatistics)
datrsTaskStatistics = Lens.lens (taskStatistics :: DescribeAuditTaskResponse -> Lude.Maybe TaskStatistics) (\s a -> s {taskStatistics = a} :: DescribeAuditTaskResponse)
{-# DEPRECATED datrsTaskStatistics "Use generic-lens or generic-optics with 'taskStatistics' instead." #-}

-- | The name of the scheduled audit (only if the audit was a scheduled audit).
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsScheduledAuditName :: Lens.Lens' DescribeAuditTaskResponse (Lude.Maybe Lude.Text)
datrsScheduledAuditName = Lens.lens (scheduledAuditName :: DescribeAuditTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduledAuditName = a} :: DescribeAuditTaskResponse)
{-# DEPRECATED datrsScheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead." #-}

-- | The status of the audit: one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsTaskStatus :: Lens.Lens' DescribeAuditTaskResponse (Lude.Maybe AuditTaskStatus)
datrsTaskStatus = Lens.lens (taskStatus :: DescribeAuditTaskResponse -> Lude.Maybe AuditTaskStatus) (\s a -> s {taskStatus = a} :: DescribeAuditTaskResponse)
{-# DEPRECATED datrsTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsResponseStatus :: Lens.Lens' DescribeAuditTaskResponse Lude.Int
datrsResponseStatus = Lens.lens (responseStatus :: DescribeAuditTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAuditTaskResponse)
{-# DEPRECATED datrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
