{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.StartOnDemandAuditTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand Device Defender audit.
module Network.AWS.IoT.StartOnDemandAuditTask
  ( -- * Creating a request
    StartOnDemandAuditTask (..),
    mkStartOnDemandAuditTask,

    -- ** Request lenses
    sodatTargetCheckNames,

    -- * Destructuring the response
    StartOnDemandAuditTaskResponse (..),
    mkStartOnDemandAuditTaskResponse,

    -- ** Response lenses
    sodatrsTaskId,
    sodatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartOnDemandAuditTask' smart constructor.
newtype StartOnDemandAuditTask = StartOnDemandAuditTask'
  { targetCheckNames ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartOnDemandAuditTask' with the minimum fields required to make a request.
--
-- * 'targetCheckNames' - Which checks are performed during the audit. The checks you specify must be enabled for your account or an exception occurs. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or @UpdateAccountAuditConfiguration@ to select which checks are enabled.
mkStartOnDemandAuditTask ::
  StartOnDemandAuditTask
mkStartOnDemandAuditTask =
  StartOnDemandAuditTask' {targetCheckNames = Lude.mempty}

-- | Which checks are performed during the audit. The checks you specify must be enabled for your account or an exception occurs. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or @UpdateAccountAuditConfiguration@ to select which checks are enabled.
--
-- /Note:/ Consider using 'targetCheckNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodatTargetCheckNames :: Lens.Lens' StartOnDemandAuditTask [Lude.Text]
sodatTargetCheckNames = Lens.lens (targetCheckNames :: StartOnDemandAuditTask -> [Lude.Text]) (\s a -> s {targetCheckNames = a} :: StartOnDemandAuditTask)
{-# DEPRECATED sodatTargetCheckNames "Use generic-lens or generic-optics with 'targetCheckNames' instead." #-}

instance Lude.AWSRequest StartOnDemandAuditTask where
  type Rs StartOnDemandAuditTask = StartOnDemandAuditTaskResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartOnDemandAuditTaskResponse'
            Lude.<$> (x Lude..?> "taskId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartOnDemandAuditTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON StartOnDemandAuditTask where
  toJSON StartOnDemandAuditTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("targetCheckNames" Lude..= targetCheckNames)]
      )

instance Lude.ToPath StartOnDemandAuditTask where
  toPath = Lude.const "/audit/tasks"

instance Lude.ToQuery StartOnDemandAuditTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartOnDemandAuditTaskResponse' smart constructor.
data StartOnDemandAuditTaskResponse = StartOnDemandAuditTaskResponse'
  { taskId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartOnDemandAuditTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'taskId' - The ID of the on-demand audit you started.
mkStartOnDemandAuditTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartOnDemandAuditTaskResponse
mkStartOnDemandAuditTaskResponse pResponseStatus_ =
  StartOnDemandAuditTaskResponse'
    { taskId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the on-demand audit you started.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodatrsTaskId :: Lens.Lens' StartOnDemandAuditTaskResponse (Lude.Maybe Lude.Text)
sodatrsTaskId = Lens.lens (taskId :: StartOnDemandAuditTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: StartOnDemandAuditTaskResponse)
{-# DEPRECATED sodatrsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodatrsResponseStatus :: Lens.Lens' StartOnDemandAuditTaskResponse Lude.Int
sodatrsResponseStatus = Lens.lens (responseStatus :: StartOnDemandAuditTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartOnDemandAuditTaskResponse)
{-# DEPRECATED sodatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
