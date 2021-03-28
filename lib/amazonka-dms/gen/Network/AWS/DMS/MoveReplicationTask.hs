{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.MoveReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves a replication task from its current replication instance to a different target replication instance using the specified parameters. The target replication instance must be created with the same or later AWS DMS version as the current replication instance.
module Network.AWS.DMS.MoveReplicationTask
    (
    -- * Creating a request
      MoveReplicationTask (..)
    , mkMoveReplicationTask
    -- ** Request lenses
    , mrtReplicationTaskArn
    , mrtTargetReplicationInstanceArn

    -- * Destructuring the response
    , MoveReplicationTaskResponse (..)
    , mkMoveReplicationTaskResponse
    -- ** Response lenses
    , mrtrrsReplicationTask
    , mrtrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkMoveReplicationTask' smart constructor.
data MoveReplicationTask = MoveReplicationTask'
  { replicationTaskArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the task that you want to move.
  , targetReplicationInstanceArn :: Core.Text
    -- ^ The ARN of the replication instance where you want to move the task to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MoveReplicationTask' value with any optional fields omitted.
mkMoveReplicationTask
    :: Core.Text -- ^ 'replicationTaskArn'
    -> Core.Text -- ^ 'targetReplicationInstanceArn'
    -> MoveReplicationTask
mkMoveReplicationTask replicationTaskArn
  targetReplicationInstanceArn
  = MoveReplicationTask'{replicationTaskArn,
                         targetReplicationInstanceArn}

-- | The Amazon Resource Name (ARN) of the task that you want to move.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrtReplicationTaskArn :: Lens.Lens' MoveReplicationTask Core.Text
mrtReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE mrtReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

-- | The ARN of the replication instance where you want to move the task to.
--
-- /Note:/ Consider using 'targetReplicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrtTargetReplicationInstanceArn :: Lens.Lens' MoveReplicationTask Core.Text
mrtTargetReplicationInstanceArn = Lens.field @"targetReplicationInstanceArn"
{-# INLINEABLE mrtTargetReplicationInstanceArn #-}
{-# DEPRECATED targetReplicationInstanceArn "Use generic-lens or generic-optics with 'targetReplicationInstanceArn' instead"  #-}

instance Core.ToQuery MoveReplicationTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders MoveReplicationTask where
        toHeaders MoveReplicationTask{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.MoveReplicationTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON MoveReplicationTask where
        toJSON MoveReplicationTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn),
                  Core.Just
                    ("TargetReplicationInstanceArn" Core..=
                       targetReplicationInstanceArn)])

instance Core.AWSRequest MoveReplicationTask where
        type Rs MoveReplicationTask = MoveReplicationTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 MoveReplicationTaskResponse' Core.<$>
                   (x Core..:? "ReplicationTask") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkMoveReplicationTaskResponse' smart constructor.
data MoveReplicationTaskResponse = MoveReplicationTaskResponse'
  { replicationTask :: Core.Maybe Types.ReplicationTask
    -- ^ The replication task that was moved.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MoveReplicationTaskResponse' value with any optional fields omitted.
mkMoveReplicationTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> MoveReplicationTaskResponse
mkMoveReplicationTaskResponse responseStatus
  = MoveReplicationTaskResponse'{replicationTask = Core.Nothing,
                                 responseStatus}

-- | The replication task that was moved.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrtrrsReplicationTask :: Lens.Lens' MoveReplicationTaskResponse (Core.Maybe Types.ReplicationTask)
mrtrrsReplicationTask = Lens.field @"replicationTask"
{-# INLINEABLE mrtrrsReplicationTask #-}
{-# DEPRECATED replicationTask "Use generic-lens or generic-optics with 'replicationTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrtrrsResponseStatus :: Lens.Lens' MoveReplicationTaskResponse Core.Int
mrtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mrtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
