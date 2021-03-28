{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StopReplicationTask (..)
    , mkStopReplicationTask
    -- ** Request lenses
    , sReplicationTaskArn

    -- * Destructuring the response
    , StopReplicationTaskResponse (..)
    , mkStopReplicationTaskResponse
    -- ** Response lenses
    , srsReplicationTask
    , srsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkStopReplicationTask' smart constructor.
newtype StopReplicationTask = StopReplicationTask'
  { replicationTaskArn :: Core.Text
    -- ^ The Amazon Resource Name(ARN) of the replication task to be stopped.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopReplicationTask' value with any optional fields omitted.
mkStopReplicationTask
    :: Core.Text -- ^ 'replicationTaskArn'
    -> StopReplicationTask
mkStopReplicationTask replicationTaskArn
  = StopReplicationTask'{replicationTaskArn}

-- | The Amazon Resource Name(ARN) of the replication task to be stopped.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationTaskArn :: Lens.Lens' StopReplicationTask Core.Text
sReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE sReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

instance Core.ToQuery StopReplicationTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopReplicationTask where
        toHeaders StopReplicationTask{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.StopReplicationTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopReplicationTask where
        toJSON StopReplicationTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn)])

instance Core.AWSRequest StopReplicationTask where
        type Rs StopReplicationTask = StopReplicationTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopReplicationTaskResponse' Core.<$>
                   (x Core..:? "ReplicationTask") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkStopReplicationTaskResponse' smart constructor.
data StopReplicationTaskResponse = StopReplicationTaskResponse'
  { replicationTask :: Core.Maybe Types.ReplicationTask
    -- ^ The replication task stopped.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopReplicationTaskResponse' value with any optional fields omitted.
mkStopReplicationTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopReplicationTaskResponse
mkStopReplicationTaskResponse responseStatus
  = StopReplicationTaskResponse'{replicationTask = Core.Nothing,
                                 responseStatus}

-- | The replication task stopped.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsReplicationTask :: Lens.Lens' StopReplicationTaskResponse (Core.Maybe Types.ReplicationTask)
srsReplicationTask = Lens.field @"replicationTask"
{-# INLINEABLE srsReplicationTask #-}
{-# DEPRECATED replicationTask "Use generic-lens or generic-optics with 'replicationTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopReplicationTaskResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
