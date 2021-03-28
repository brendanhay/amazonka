{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deleting a Global Datastore is a two-step process: 
--
--
--     * First, you must 'DisassociateGlobalReplicationGroup' to remove the secondary clusters in the Global Datastore.
--
--
--     * Once the Global Datastore contains only the primary cluster, you can use DeleteGlobalReplicationGroup API to delete the Global Datastore while retainining the primary cluster using Retain…= true.
--
--
-- Since the Global Datastore has only a primary cluster, you can delete the Global Datastore while retaining the primary by setting @RetainPrimaryCluster=true@ .
-- When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the selected resources; you cannot cancel or revert this operation.
module Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
    (
    -- * Creating a request
      DeleteGlobalReplicationGroup (..)
    , mkDeleteGlobalReplicationGroup
    -- ** Request lenses
    , dGlobalReplicationGroupId
    , dRetainPrimaryReplicationGroup

    -- * Destructuring the response
    , DeleteGlobalReplicationGroupResponse (..)
    , mkDeleteGlobalReplicationGroupResponse
    -- ** Response lenses
    , dgrgrgrsGlobalReplicationGroup
    , dgrgrgrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGlobalReplicationGroup' smart constructor.
data DeleteGlobalReplicationGroup = DeleteGlobalReplicationGroup'
  { globalReplicationGroupId :: Core.Text
    -- ^ The name of the Global Datastore
  , retainPrimaryReplicationGroup :: Core.Bool
    -- ^ The primary replication group is retained as a standalone replication group. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGlobalReplicationGroup' value with any optional fields omitted.
mkDeleteGlobalReplicationGroup
    :: Core.Text -- ^ 'globalReplicationGroupId'
    -> Core.Bool -- ^ 'retainPrimaryReplicationGroup'
    -> DeleteGlobalReplicationGroup
mkDeleteGlobalReplicationGroup globalReplicationGroupId
  retainPrimaryReplicationGroup
  = DeleteGlobalReplicationGroup'{globalReplicationGroupId,
                                  retainPrimaryReplicationGroup}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGlobalReplicationGroupId :: Lens.Lens' DeleteGlobalReplicationGroup Core.Text
dGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# INLINEABLE dGlobalReplicationGroupId #-}
{-# DEPRECATED globalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead"  #-}

-- | The primary replication group is retained as a standalone replication group. 
--
-- /Note:/ Consider using 'retainPrimaryReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRetainPrimaryReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroup Core.Bool
dRetainPrimaryReplicationGroup = Lens.field @"retainPrimaryReplicationGroup"
{-# INLINEABLE dRetainPrimaryReplicationGroup #-}
{-# DEPRECATED retainPrimaryReplicationGroup "Use generic-lens or generic-optics with 'retainPrimaryReplicationGroup' instead"  #-}

instance Core.ToQuery DeleteGlobalReplicationGroup where
        toQuery DeleteGlobalReplicationGroup{..}
          = Core.toQueryPair "Action"
              ("DeleteGlobalReplicationGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "GlobalReplicationGroupId"
                globalReplicationGroupId
              Core.<>
              Core.toQueryPair "RetainPrimaryReplicationGroup"
                retainPrimaryReplicationGroup

instance Core.ToHeaders DeleteGlobalReplicationGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteGlobalReplicationGroup where
        type Rs DeleteGlobalReplicationGroup =
             DeleteGlobalReplicationGroupResponse
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
          = Response.receiveXMLWrapper "DeleteGlobalReplicationGroupResult"
              (\ s h x ->
                 DeleteGlobalReplicationGroupResponse' Core.<$>
                   (x Core..@? "GlobalReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGlobalReplicationGroupResponse' smart constructor.
data DeleteGlobalReplicationGroupResponse = DeleteGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGlobalReplicationGroupResponse' value with any optional fields omitted.
mkDeleteGlobalReplicationGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteGlobalReplicationGroupResponse
mkDeleteGlobalReplicationGroupResponse responseStatus
  = DeleteGlobalReplicationGroupResponse'{globalReplicationGroup =
                                            Core.Nothing,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrgrsGlobalReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
dgrgrgrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# INLINEABLE dgrgrgrsGlobalReplicationGroup #-}
{-# DEPRECATED globalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrgrsResponseStatus :: Lens.Lens' DeleteGlobalReplicationGroupResponse Core.Int
dgrgrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgrgrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
