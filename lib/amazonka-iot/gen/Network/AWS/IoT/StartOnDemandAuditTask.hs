{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    sodatrrsTaskId,
    sodatrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartOnDemandAuditTask' smart constructor.
newtype StartOnDemandAuditTask = StartOnDemandAuditTask'
  { -- | Which checks are performed during the audit. The checks you specify must be enabled for your account or an exception occurs. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or @UpdateAccountAuditConfiguration@ to select which checks are enabled.
    targetCheckNames :: [Types.AuditCheckName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartOnDemandAuditTask' value with any optional fields omitted.
mkStartOnDemandAuditTask ::
  StartOnDemandAuditTask
mkStartOnDemandAuditTask =
  StartOnDemandAuditTask' {targetCheckNames = Core.mempty}

-- | Which checks are performed during the audit. The checks you specify must be enabled for your account or an exception occurs. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or @UpdateAccountAuditConfiguration@ to select which checks are enabled.
--
-- /Note:/ Consider using 'targetCheckNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodatTargetCheckNames :: Lens.Lens' StartOnDemandAuditTask [Types.AuditCheckName]
sodatTargetCheckNames = Lens.field @"targetCheckNames"
{-# DEPRECATED sodatTargetCheckNames "Use generic-lens or generic-optics with 'targetCheckNames' instead." #-}

instance Core.FromJSON StartOnDemandAuditTask where
  toJSON StartOnDemandAuditTask {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("targetCheckNames" Core..= targetCheckNames)]
      )

instance Core.AWSRequest StartOnDemandAuditTask where
  type Rs StartOnDemandAuditTask = StartOnDemandAuditTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/audit/tasks",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartOnDemandAuditTaskResponse'
            Core.<$> (x Core..:? "taskId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartOnDemandAuditTaskResponse' smart constructor.
data StartOnDemandAuditTaskResponse = StartOnDemandAuditTaskResponse'
  { -- | The ID of the on-demand audit you started.
    taskId :: Core.Maybe Types.AuditTaskId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartOnDemandAuditTaskResponse' value with any optional fields omitted.
mkStartOnDemandAuditTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartOnDemandAuditTaskResponse
mkStartOnDemandAuditTaskResponse responseStatus =
  StartOnDemandAuditTaskResponse'
    { taskId = Core.Nothing,
      responseStatus
    }

-- | The ID of the on-demand audit you started.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodatrrsTaskId :: Lens.Lens' StartOnDemandAuditTaskResponse (Core.Maybe Types.AuditTaskId)
sodatrrsTaskId = Lens.field @"taskId"
{-# DEPRECATED sodatrrsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sodatrrsResponseStatus :: Lens.Lens' StartOnDemandAuditTaskResponse Core.Int
sodatrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sodatrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
