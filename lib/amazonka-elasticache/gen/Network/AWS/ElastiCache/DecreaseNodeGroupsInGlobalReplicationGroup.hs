{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decreases the number of node groups in a Global Datastore
module Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
    (
    -- * Creating a request
      DecreaseNodeGroupsInGlobalReplicationGroup (..)
    , mkDecreaseNodeGroupsInGlobalReplicationGroup
    -- ** Request lenses
    , dngigrgGlobalReplicationGroupId
    , dngigrgNodeGroupCount
    , dngigrgApplyImmediately
    , dngigrgGlobalNodeGroupsToRemove
    , dngigrgGlobalNodeGroupsToRetain

    -- * Destructuring the response
    , DecreaseNodeGroupsInGlobalReplicationGroupResponse (..)
    , mkDecreaseNodeGroupsInGlobalReplicationGroupResponse
    -- ** Response lenses
    , dngigrgrrsGlobalReplicationGroup
    , dngigrgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDecreaseNodeGroupsInGlobalReplicationGroup' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroup = DecreaseNodeGroupsInGlobalReplicationGroup'
  { globalReplicationGroupId :: Core.Text
    -- ^ The name of the Global Datastore
  , nodeGroupCount :: Core.Int
    -- ^ The number of node groups (shards) that results from the modification of the shard configuration
  , applyImmediately :: Core.Bool
    -- ^ Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is true. 
  , globalNodeGroupsToRemove :: Core.Maybe [Core.Text]
    -- ^ If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster. 
  , globalNodeGroupsToRetain :: Core.Maybe [Core.Text]
    -- ^ If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecreaseNodeGroupsInGlobalReplicationGroup' value with any optional fields omitted.
mkDecreaseNodeGroupsInGlobalReplicationGroup
    :: Core.Text -- ^ 'globalReplicationGroupId'
    -> Core.Int -- ^ 'nodeGroupCount'
    -> Core.Bool -- ^ 'applyImmediately'
    -> DecreaseNodeGroupsInGlobalReplicationGroup
mkDecreaseNodeGroupsInGlobalReplicationGroup
  globalReplicationGroupId nodeGroupCount applyImmediately
  = DecreaseNodeGroupsInGlobalReplicationGroup'{globalReplicationGroupId,
                                                nodeGroupCount, applyImmediately,
                                                globalNodeGroupsToRemove = Core.Nothing,
                                                globalNodeGroupsToRetain = Core.Nothing}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgGlobalReplicationGroupId :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Core.Text
dngigrgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# INLINEABLE dngigrgGlobalReplicationGroupId #-}
{-# DEPRECATED globalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead"  #-}

-- | The number of node groups (shards) that results from the modification of the shard configuration
--
-- /Note:/ Consider using 'nodeGroupCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgNodeGroupCount :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Core.Int
dngigrgNodeGroupCount = Lens.field @"nodeGroupCount"
{-# INLINEABLE dngigrgNodeGroupCount #-}
{-# DEPRECATED nodeGroupCount "Use generic-lens or generic-optics with 'nodeGroupCount' instead"  #-}

-- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is true. 
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgApplyImmediately :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Core.Bool
dngigrgApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE dngigrgApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

-- | If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster. 
--
-- /Note:/ Consider using 'globalNodeGroupsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgGlobalNodeGroupsToRemove :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup (Core.Maybe [Core.Text])
dngigrgGlobalNodeGroupsToRemove = Lens.field @"globalNodeGroupsToRemove"
{-# INLINEABLE dngigrgGlobalNodeGroupsToRemove #-}
{-# DEPRECATED globalNodeGroupsToRemove "Use generic-lens or generic-optics with 'globalNodeGroupsToRemove' instead"  #-}

-- | If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster. 
--
-- /Note:/ Consider using 'globalNodeGroupsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgGlobalNodeGroupsToRetain :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup (Core.Maybe [Core.Text])
dngigrgGlobalNodeGroupsToRetain = Lens.field @"globalNodeGroupsToRetain"
{-# INLINEABLE dngigrgGlobalNodeGroupsToRetain #-}
{-# DEPRECATED globalNodeGroupsToRetain "Use generic-lens or generic-optics with 'globalNodeGroupsToRetain' instead"  #-}

instance Core.ToQuery DecreaseNodeGroupsInGlobalReplicationGroup
         where
        toQuery DecreaseNodeGroupsInGlobalReplicationGroup{..}
          = Core.toQueryPair "Action"
              ("DecreaseNodeGroupsInGlobalReplicationGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "GlobalReplicationGroupId"
                globalReplicationGroupId
              Core.<> Core.toQueryPair "NodeGroupCount" nodeGroupCount
              Core.<> Core.toQueryPair "ApplyImmediately" applyImmediately
              Core.<>
              Core.toQueryPair "GlobalNodeGroupsToRemove"
                (Core.maybe Core.mempty (Core.toQueryList "GlobalNodeGroupId")
                   globalNodeGroupsToRemove)
              Core.<>
              Core.toQueryPair "GlobalNodeGroupsToRetain"
                (Core.maybe Core.mempty (Core.toQueryList "GlobalNodeGroupId")
                   globalNodeGroupsToRetain)

instance Core.ToHeaders DecreaseNodeGroupsInGlobalReplicationGroup
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DecreaseNodeGroupsInGlobalReplicationGroup
         where
        type Rs DecreaseNodeGroupsInGlobalReplicationGroup =
             DecreaseNodeGroupsInGlobalReplicationGroupResponse
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
              "DecreaseNodeGroupsInGlobalReplicationGroupResult"
              (\ s h x ->
                 DecreaseNodeGroupsInGlobalReplicationGroupResponse' Core.<$>
                   (x Core..@? "GlobalReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDecreaseNodeGroupsInGlobalReplicationGroupResponse' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroupResponse = DecreaseNodeGroupsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecreaseNodeGroupsInGlobalReplicationGroupResponse' value with any optional fields omitted.
mkDecreaseNodeGroupsInGlobalReplicationGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DecreaseNodeGroupsInGlobalReplicationGroupResponse
mkDecreaseNodeGroupsInGlobalReplicationGroupResponse responseStatus
  = DecreaseNodeGroupsInGlobalReplicationGroupResponse'{globalReplicationGroup
                                                          = Core.Nothing,
                                                        responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgrrsGlobalReplicationGroup :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
dngigrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# INLINEABLE dngigrgrrsGlobalReplicationGroup #-}
{-# DEPRECATED globalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgrrsResponseStatus :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse Core.Int
dngigrgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dngigrgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
