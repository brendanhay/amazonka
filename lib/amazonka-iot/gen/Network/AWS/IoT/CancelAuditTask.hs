{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CancelAuditTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an audit that is in progress. The audit can be either scheduled or on-demand. If the audit is not in progress, an "InvalidRequestException" occurs.
module Network.AWS.IoT.CancelAuditTask
  ( -- * Creating a request
    CancelAuditTask (..),
    mkCancelAuditTask,

    -- ** Request lenses
    catTaskId,

    -- * Destructuring the response
    CancelAuditTaskResponse (..),
    mkCancelAuditTaskResponse,

    -- ** Response lenses
    catrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelAuditTask' smart constructor.
newtype CancelAuditTask = CancelAuditTask'
  { -- | The ID of the audit you want to cancel. You can only cancel an audit that is "IN_PROGRESS".
    taskId :: Types.AuditTaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelAuditTask' value with any optional fields omitted.
mkCancelAuditTask ::
  -- | 'taskId'
  Types.AuditTaskId ->
  CancelAuditTask
mkCancelAuditTask taskId = CancelAuditTask' {taskId}

-- | The ID of the audit you want to cancel. You can only cancel an audit that is "IN_PROGRESS".
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
catTaskId :: Lens.Lens' CancelAuditTask Types.AuditTaskId
catTaskId = Lens.field @"taskId"
{-# DEPRECATED catTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Core.FromJSON CancelAuditTask where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CancelAuditTask where
  type Rs CancelAuditTask = CancelAuditTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/audit/tasks/" Core.<> (Core.toText taskId) Core.<> ("/cancel")),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelAuditTaskResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCancelAuditTaskResponse' smart constructor.
newtype CancelAuditTaskResponse = CancelAuditTaskResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelAuditTaskResponse' value with any optional fields omitted.
mkCancelAuditTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelAuditTaskResponse
mkCancelAuditTaskResponse responseStatus =
  CancelAuditTaskResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
catrrsResponseStatus :: Lens.Lens' CancelAuditTaskResponse Core.Int
catrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED catrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
