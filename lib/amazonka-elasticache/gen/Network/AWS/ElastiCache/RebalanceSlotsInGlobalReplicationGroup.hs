{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Redistribute slots to ensure uniform distribution across existing shards in the cluster.
module Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
    (
    -- * Creating a request
      RebalanceSlotsInGlobalReplicationGroup (..)
    , mkRebalanceSlotsInGlobalReplicationGroup
    -- ** Request lenses
    , rsigrgGlobalReplicationGroupId
    , rsigrgApplyImmediately

    -- * Destructuring the response
    , RebalanceSlotsInGlobalReplicationGroupResponse (..)
    , mkRebalanceSlotsInGlobalReplicationGroupResponse
    -- ** Response lenses
    , rsigrgrrsGlobalReplicationGroup
    , rsigrgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRebalanceSlotsInGlobalReplicationGroup' smart constructor.
data RebalanceSlotsInGlobalReplicationGroup = RebalanceSlotsInGlobalReplicationGroup'
  { globalReplicationGroupId :: Core.Text
    -- ^ The name of the Global Datastore
  , applyImmediately :: Core.Bool
    -- ^ If @True@ , redistribution is applied immediately.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebalanceSlotsInGlobalReplicationGroup' value with any optional fields omitted.
mkRebalanceSlotsInGlobalReplicationGroup
    :: Core.Text -- ^ 'globalReplicationGroupId'
    -> Core.Bool -- ^ 'applyImmediately'
    -> RebalanceSlotsInGlobalReplicationGroup
mkRebalanceSlotsInGlobalReplicationGroup globalReplicationGroupId
  applyImmediately
  = RebalanceSlotsInGlobalReplicationGroup'{globalReplicationGroupId,
                                            applyImmediately}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsigrgGlobalReplicationGroupId :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroup Core.Text
rsigrgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# INLINEABLE rsigrgGlobalReplicationGroupId #-}
{-# DEPRECATED globalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead"  #-}

-- | If @True@ , redistribution is applied immediately.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsigrgApplyImmediately :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroup Core.Bool
rsigrgApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE rsigrgApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

instance Core.ToQuery RebalanceSlotsInGlobalReplicationGroup where
        toQuery RebalanceSlotsInGlobalReplicationGroup{..}
          = Core.toQueryPair "Action"
              ("RebalanceSlotsInGlobalReplicationGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "GlobalReplicationGroupId"
                globalReplicationGroupId
              Core.<> Core.toQueryPair "ApplyImmediately" applyImmediately

instance Core.ToHeaders RebalanceSlotsInGlobalReplicationGroup
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RebalanceSlotsInGlobalReplicationGroup
         where
        type Rs RebalanceSlotsInGlobalReplicationGroup =
             RebalanceSlotsInGlobalReplicationGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "RebalanceSlotsInGlobalReplicationGroupResult"
              (\ s h x ->
                 RebalanceSlotsInGlobalReplicationGroupResponse' Core.<$>
                   (x Core..@? "GlobalReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRebalanceSlotsInGlobalReplicationGroupResponse' smart constructor.
data RebalanceSlotsInGlobalReplicationGroupResponse = RebalanceSlotsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebalanceSlotsInGlobalReplicationGroupResponse' value with any optional fields omitted.
mkRebalanceSlotsInGlobalReplicationGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RebalanceSlotsInGlobalReplicationGroupResponse
mkRebalanceSlotsInGlobalReplicationGroupResponse responseStatus
  = RebalanceSlotsInGlobalReplicationGroupResponse'{globalReplicationGroup
                                                      = Core.Nothing,
                                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsigrgrrsGlobalReplicationGroup :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
rsigrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# INLINEABLE rsigrgrrsGlobalReplicationGroup #-}
{-# DEPRECATED globalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsigrgrrsResponseStatus :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroupResponse Core.Int
rsigrgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rsigrgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
