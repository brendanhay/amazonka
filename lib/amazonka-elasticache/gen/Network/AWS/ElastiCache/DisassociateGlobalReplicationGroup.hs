{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a secondary cluster from the Global Datastore using the Global Datastore name. The secondary cluster will no longer receive updates from the primary cluster, but will remain as a standalone cluster in that AWS region.
module Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
    (
    -- * Creating a request
      DisassociateGlobalReplicationGroup (..)
    , mkDisassociateGlobalReplicationGroup
    -- ** Request lenses
    , dgrgGlobalReplicationGroupId
    , dgrgReplicationGroupId
    , dgrgReplicationGroupRegion

    -- * Destructuring the response
    , DisassociateGlobalReplicationGroupResponse (..)
    , mkDisassociateGlobalReplicationGroupResponse
    -- ** Response lenses
    , dgrgrrsGlobalReplicationGroup
    , dgrgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateGlobalReplicationGroup' smart constructor.
data DisassociateGlobalReplicationGroup = DisassociateGlobalReplicationGroup'
  { globalReplicationGroupId :: Core.Text
    -- ^ The name of the Global Datastore
  , replicationGroupId :: Core.Text
    -- ^ The name of the secondary cluster you wish to remove from the Global Datastore
  , replicationGroupRegion :: Core.Text
    -- ^ The AWS region of secondary cluster you wish to remove from the Global Datastore
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateGlobalReplicationGroup' value with any optional fields omitted.
mkDisassociateGlobalReplicationGroup
    :: Core.Text -- ^ 'globalReplicationGroupId'
    -> Core.Text -- ^ 'replicationGroupId'
    -> Core.Text -- ^ 'replicationGroupRegion'
    -> DisassociateGlobalReplicationGroup
mkDisassociateGlobalReplicationGroup globalReplicationGroupId
  replicationGroupId replicationGroupRegion
  = DisassociateGlobalReplicationGroup'{globalReplicationGroupId,
                                        replicationGroupId, replicationGroupRegion}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgGlobalReplicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Core.Text
dgrgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# INLINEABLE dgrgGlobalReplicationGroupId #-}
{-# DEPRECATED globalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead"  #-}

-- | The name of the secondary cluster you wish to remove from the Global Datastore
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgReplicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Core.Text
dgrgReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE dgrgReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | The AWS region of secondary cluster you wish to remove from the Global Datastore
--
-- /Note:/ Consider using 'replicationGroupRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgReplicationGroupRegion :: Lens.Lens' DisassociateGlobalReplicationGroup Core.Text
dgrgReplicationGroupRegion = Lens.field @"replicationGroupRegion"
{-# INLINEABLE dgrgReplicationGroupRegion #-}
{-# DEPRECATED replicationGroupRegion "Use generic-lens or generic-optics with 'replicationGroupRegion' instead"  #-}

instance Core.ToQuery DisassociateGlobalReplicationGroup where
        toQuery DisassociateGlobalReplicationGroup{..}
          = Core.toQueryPair "Action"
              ("DisassociateGlobalReplicationGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "GlobalReplicationGroupId"
                globalReplicationGroupId
              Core.<> Core.toQueryPair "ReplicationGroupId" replicationGroupId
              Core.<>
              Core.toQueryPair "ReplicationGroupRegion" replicationGroupRegion

instance Core.ToHeaders DisassociateGlobalReplicationGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateGlobalReplicationGroup where
        type Rs DisassociateGlobalReplicationGroup =
             DisassociateGlobalReplicationGroupResponse
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
              "DisassociateGlobalReplicationGroupResult"
              (\ s h x ->
                 DisassociateGlobalReplicationGroupResponse' Core.<$>
                   (x Core..@? "GlobalReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateGlobalReplicationGroupResponse' smart constructor.
data DisassociateGlobalReplicationGroupResponse = DisassociateGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateGlobalReplicationGroupResponse' value with any optional fields omitted.
mkDisassociateGlobalReplicationGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateGlobalReplicationGroupResponse
mkDisassociateGlobalReplicationGroupResponse responseStatus
  = DisassociateGlobalReplicationGroupResponse'{globalReplicationGroup
                                                  = Core.Nothing,
                                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrrsGlobalReplicationGroup :: Lens.Lens' DisassociateGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
dgrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# INLINEABLE dgrgrrsGlobalReplicationGroup #-}
{-# DEPRECATED globalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrrsResponseStatus :: Lens.Lens' DisassociateGlobalReplicationGroupResponse Core.Int
dgrgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgrgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
