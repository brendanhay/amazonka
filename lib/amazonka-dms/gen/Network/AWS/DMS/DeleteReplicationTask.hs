{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication task.
module Network.AWS.DMS.DeleteReplicationTask
    (
    -- * Creating a request
      DeleteReplicationTask (..)
    , mkDeleteReplicationTask
    -- ** Request lenses
    , drtReplicationTaskArn

    -- * Destructuring the response
    , DeleteReplicationTaskResponse (..)
    , mkDeleteReplicationTaskResponse
    -- ** Response lenses
    , drtrrsReplicationTask
    , drtrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteReplicationTask' smart constructor.
newtype DeleteReplicationTask = DeleteReplicationTask'
  { replicationTaskArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication task to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationTask' value with any optional fields omitted.
mkDeleteReplicationTask
    :: Core.Text -- ^ 'replicationTaskArn'
    -> DeleteReplicationTask
mkDeleteReplicationTask replicationTaskArn
  = DeleteReplicationTask'{replicationTaskArn}

-- | The Amazon Resource Name (ARN) of the replication task to be deleted.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtReplicationTaskArn :: Lens.Lens' DeleteReplicationTask Core.Text
drtReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE drtReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

instance Core.ToQuery DeleteReplicationTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteReplicationTask where
        toHeaders DeleteReplicationTask{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DeleteReplicationTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteReplicationTask where
        toJSON DeleteReplicationTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn)])

instance Core.AWSRequest DeleteReplicationTask where
        type Rs DeleteReplicationTask = DeleteReplicationTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteReplicationTaskResponse' Core.<$>
                   (x Core..:? "ReplicationTask") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDeleteReplicationTaskResponse' smart constructor.
data DeleteReplicationTaskResponse = DeleteReplicationTaskResponse'
  { replicationTask :: Core.Maybe Types.ReplicationTask
    -- ^ The deleted replication task.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteReplicationTaskResponse' value with any optional fields omitted.
mkDeleteReplicationTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReplicationTaskResponse
mkDeleteReplicationTaskResponse responseStatus
  = DeleteReplicationTaskResponse'{replicationTask = Core.Nothing,
                                   responseStatus}

-- | The deleted replication task.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrrsReplicationTask :: Lens.Lens' DeleteReplicationTaskResponse (Core.Maybe Types.ReplicationTask)
drtrrsReplicationTask = Lens.field @"replicationTask"
{-# INLINEABLE drtrrsReplicationTask #-}
{-# DEPRECATED replicationTask "Use generic-lens or generic-optics with 'replicationTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrrsResponseStatus :: Lens.Lens' DeleteReplicationTaskResponse Core.Int
drtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
