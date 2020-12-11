{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuditMitigationActionsTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an audit mitigation task that is used to apply mitigation actions to a set of audit findings. Properties include the actions being applied, the audit checks to which they're being applied, the task status, and aggregated task statistics.
module Network.AWS.IoT.DescribeAuditMitigationActionsTask
  ( -- * Creating a request
    DescribeAuditMitigationActionsTask (..),
    mkDescribeAuditMitigationActionsTask,

    -- ** Request lenses
    damatTaskId,

    -- * Destructuring the response
    DescribeAuditMitigationActionsTaskResponse (..),
    mkDescribeAuditMitigationActionsTaskResponse,

    -- ** Response lenses
    damatrsStartTime,
    damatrsTaskStatistics,
    damatrsActionsDefinition,
    damatrsAuditCheckToActionsMapping,
    damatrsEndTime,
    damatrsTarget,
    damatrsTaskStatus,
    damatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAuditMitigationActionsTask' smart constructor.
newtype DescribeAuditMitigationActionsTask = DescribeAuditMitigationActionsTask'
  { taskId ::
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

-- | Creates a value of 'DescribeAuditMitigationActionsTask' with the minimum fields required to make a request.
--
-- * 'taskId' - The unique identifier for the audit mitigation task.
mkDescribeAuditMitigationActionsTask ::
  -- | 'taskId'
  Lude.Text ->
  DescribeAuditMitigationActionsTask
mkDescribeAuditMitigationActionsTask pTaskId_ =
  DescribeAuditMitigationActionsTask' {taskId = pTaskId_}

-- | The unique identifier for the audit mitigation task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatTaskId :: Lens.Lens' DescribeAuditMitigationActionsTask Lude.Text
damatTaskId = Lens.lens (taskId :: DescribeAuditMitigationActionsTask -> Lude.Text) (\s a -> s {taskId = a} :: DescribeAuditMitigationActionsTask)
{-# DEPRECATED damatTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest DescribeAuditMitigationActionsTask where
  type
    Rs DescribeAuditMitigationActionsTask =
      DescribeAuditMitigationActionsTaskResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAuditMitigationActionsTaskResponse'
            Lude.<$> (x Lude..?> "startTime")
            Lude.<*> (x Lude..?> "taskStatistics" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "actionsDefinition" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "auditCheckToActionsMapping" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "endTime")
            Lude.<*> (x Lude..?> "target")
            Lude.<*> (x Lude..?> "taskStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAuditMitigationActionsTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAuditMitigationActionsTask where
  toPath DescribeAuditMitigationActionsTask' {..} =
    Lude.mconcat
      ["/audit/mitigationactions/tasks/", Lude.toBS taskId]

instance Lude.ToQuery DescribeAuditMitigationActionsTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAuditMitigationActionsTaskResponse' smart constructor.
data DescribeAuditMitigationActionsTaskResponse = DescribeAuditMitigationActionsTaskResponse'
  { startTime ::
      Lude.Maybe
        Lude.Timestamp,
    taskStatistics ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (TaskStatisticsForAuditCheck)
        ),
    actionsDefinition ::
      Lude.Maybe
        [MitigationAction],
    auditCheckToActionsMapping ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            ( Lude.NonEmpty
                Lude.Text
            )
        ),
    endTime ::
      Lude.Maybe
        Lude.Timestamp,
    target ::
      Lude.Maybe
        AuditMitigationActionsTaskTarget,
    taskStatus ::
      Lude.Maybe
        AuditMitigationActionsTaskStatus,
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

-- | Creates a value of 'DescribeAuditMitigationActionsTaskResponse' with the minimum fields required to make a request.
--
-- * 'actionsDefinition' - Specifies the mitigation actions and their parameters that are applied as part of this task.
-- * 'auditCheckToActionsMapping' - Specifies the mitigation actions that should be applied to specific audit checks.
-- * 'endTime' - The date and time when the task was completed or canceled.
-- * 'responseStatus' - The response status code.
-- * 'startTime' - The date and time when the task was started.
-- * 'target' - Identifies the findings to which the mitigation actions are applied. This can be by audit checks, by audit task, or a set of findings.
-- * 'taskStatistics' - Aggregate counts of the results when the mitigation tasks were applied to the findings for this audit mitigation actions task.
-- * 'taskStatus' - The current status of the task.
mkDescribeAuditMitigationActionsTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAuditMitigationActionsTaskResponse
mkDescribeAuditMitigationActionsTaskResponse pResponseStatus_ =
  DescribeAuditMitigationActionsTaskResponse'
    { startTime =
        Lude.Nothing,
      taskStatistics = Lude.Nothing,
      actionsDefinition = Lude.Nothing,
      auditCheckToActionsMapping = Lude.Nothing,
      endTime = Lude.Nothing,
      target = Lude.Nothing,
      taskStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time when the task was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrsStartTime :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Lude.Maybe Lude.Timestamp)
damatrsStartTime = Lens.lens (startTime :: DescribeAuditMitigationActionsTaskResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DescribeAuditMitigationActionsTaskResponse)
{-# DEPRECATED damatrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Aggregate counts of the results when the mitigation tasks were applied to the findings for this audit mitigation actions task.
--
-- /Note:/ Consider using 'taskStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrsTaskStatistics :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Lude.Maybe (Lude.HashMap Lude.Text (TaskStatisticsForAuditCheck)))
damatrsTaskStatistics = Lens.lens (taskStatistics :: DescribeAuditMitigationActionsTaskResponse -> Lude.Maybe (Lude.HashMap Lude.Text (TaskStatisticsForAuditCheck))) (\s a -> s {taskStatistics = a} :: DescribeAuditMitigationActionsTaskResponse)
{-# DEPRECATED damatrsTaskStatistics "Use generic-lens or generic-optics with 'taskStatistics' instead." #-}

-- | Specifies the mitigation actions and their parameters that are applied as part of this task.
--
-- /Note:/ Consider using 'actionsDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrsActionsDefinition :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Lude.Maybe [MitigationAction])
damatrsActionsDefinition = Lens.lens (actionsDefinition :: DescribeAuditMitigationActionsTaskResponse -> Lude.Maybe [MitigationAction]) (\s a -> s {actionsDefinition = a} :: DescribeAuditMitigationActionsTaskResponse)
{-# DEPRECATED damatrsActionsDefinition "Use generic-lens or generic-optics with 'actionsDefinition' instead." #-}

-- | Specifies the mitigation actions that should be applied to specific audit checks.
--
-- /Note:/ Consider using 'auditCheckToActionsMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrsAuditCheckToActionsMapping :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text)))
damatrsAuditCheckToActionsMapping = Lens.lens (auditCheckToActionsMapping :: DescribeAuditMitigationActionsTaskResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text))) (\s a -> s {auditCheckToActionsMapping = a} :: DescribeAuditMitigationActionsTaskResponse)
{-# DEPRECATED damatrsAuditCheckToActionsMapping "Use generic-lens or generic-optics with 'auditCheckToActionsMapping' instead." #-}

-- | The date and time when the task was completed or canceled.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrsEndTime :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Lude.Maybe Lude.Timestamp)
damatrsEndTime = Lens.lens (endTime :: DescribeAuditMitigationActionsTaskResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DescribeAuditMitigationActionsTaskResponse)
{-# DEPRECATED damatrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Identifies the findings to which the mitigation actions are applied. This can be by audit checks, by audit task, or a set of findings.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrsTarget :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Lude.Maybe AuditMitigationActionsTaskTarget)
damatrsTarget = Lens.lens (target :: DescribeAuditMitigationActionsTaskResponse -> Lude.Maybe AuditMitigationActionsTaskTarget) (\s a -> s {target = a} :: DescribeAuditMitigationActionsTaskResponse)
{-# DEPRECATED damatrsTarget "Use generic-lens or generic-optics with 'target' instead." #-}

-- | The current status of the task.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrsTaskStatus :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse (Lude.Maybe AuditMitigationActionsTaskStatus)
damatrsTaskStatus = Lens.lens (taskStatus :: DescribeAuditMitigationActionsTaskResponse -> Lude.Maybe AuditMitigationActionsTaskStatus) (\s a -> s {taskStatus = a} :: DescribeAuditMitigationActionsTaskResponse)
{-# DEPRECATED damatrsTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damatrsResponseStatus :: Lens.Lens' DescribeAuditMitigationActionsTaskResponse Lude.Int
damatrsResponseStatus = Lens.lens (responseStatus :: DescribeAuditMitigationActionsTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAuditMitigationActionsTaskResponse)
{-# DEPRECATED damatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
