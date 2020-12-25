{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StopReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the replication task.
module Network.AWS.DMS.StopReplicationTask
  ( -- * Creating a request
    StopReplicationTask (..),
    mkStopReplicationTask,

    -- ** Request lenses
    sReplicationTaskArn,

    -- * Destructuring the response
    StopReplicationTaskResponse (..),
    mkStopReplicationTaskResponse,

    -- ** Response lenses
    srsReplicationTask,
    srsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkStopReplicationTask' smart constructor.
newtype StopReplicationTask = StopReplicationTask'
  { -- | The Amazon Resource Name(ARN) of the replication task to be stopped.
    replicationTaskArn :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopReplicationTask' value with any optional fields omitted.
mkStopReplicationTask ::
  -- | 'replicationTaskArn'
  Types.String ->
  StopReplicationTask
mkStopReplicationTask replicationTaskArn =
  StopReplicationTask' {replicationTaskArn}

-- | The Amazon Resource Name(ARN) of the replication task to be stopped.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationTaskArn :: Lens.Lens' StopReplicationTask Types.String
sReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# DEPRECATED sReplicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead." #-}

instance Core.FromJSON StopReplicationTask where
  toJSON StopReplicationTask {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn)]
      )

instance Core.AWSRequest StopReplicationTask where
  type Rs StopReplicationTask = StopReplicationTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.StopReplicationTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopReplicationTaskResponse'
            Core.<$> (x Core..:? "ReplicationTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkStopReplicationTaskResponse' smart constructor.
data StopReplicationTaskResponse = StopReplicationTaskResponse'
  { -- | The replication task stopped.
    replicationTask :: Core.Maybe Types.ReplicationTask,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StopReplicationTaskResponse' value with any optional fields omitted.
mkStopReplicationTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopReplicationTaskResponse
mkStopReplicationTaskResponse responseStatus =
  StopReplicationTaskResponse'
    { replicationTask = Core.Nothing,
      responseStatus
    }

-- | The replication task stopped.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsReplicationTask :: Lens.Lens' StopReplicationTaskResponse (Core.Maybe Types.ReplicationTask)
srsReplicationTask = Lens.field @"replicationTask"
{-# DEPRECATED srsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopReplicationTaskResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
