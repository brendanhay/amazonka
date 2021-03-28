{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to failover the primary region to a selected secondary region. The selected secondary region will become primary, and all other clusters will become secondary.
module Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
    (
    -- * Creating a request
      FailoverGlobalReplicationGroup (..)
    , mkFailoverGlobalReplicationGroup
    -- ** Request lenses
    , fgrgGlobalReplicationGroupId
    , fgrgPrimaryRegion
    , fgrgPrimaryReplicationGroupId

    -- * Destructuring the response
    , FailoverGlobalReplicationGroupResponse (..)
    , mkFailoverGlobalReplicationGroupResponse
    -- ** Response lenses
    , fgrgrrsGlobalReplicationGroup
    , fgrgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkFailoverGlobalReplicationGroup' smart constructor.
data FailoverGlobalReplicationGroup = FailoverGlobalReplicationGroup'
  { globalReplicationGroupId :: Core.Text
    -- ^ The name of the Global Datastore
  , primaryRegion :: Core.Text
    -- ^ The AWS region of the primary cluster of the Global Datastore
  , primaryReplicationGroupId :: Core.Text
    -- ^ The name of the primary replication group
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailoverGlobalReplicationGroup' value with any optional fields omitted.
mkFailoverGlobalReplicationGroup
    :: Core.Text -- ^ 'globalReplicationGroupId'
    -> Core.Text -- ^ 'primaryRegion'
    -> Core.Text -- ^ 'primaryReplicationGroupId'
    -> FailoverGlobalReplicationGroup
mkFailoverGlobalReplicationGroup globalReplicationGroupId
  primaryRegion primaryReplicationGroupId
  = FailoverGlobalReplicationGroup'{globalReplicationGroupId,
                                    primaryRegion, primaryReplicationGroupId}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgGlobalReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Core.Text
fgrgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# INLINEABLE fgrgGlobalReplicationGroupId #-}
{-# DEPRECATED globalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead"  #-}

-- | The AWS region of the primary cluster of the Global Datastore
--
-- /Note:/ Consider using 'primaryRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgPrimaryRegion :: Lens.Lens' FailoverGlobalReplicationGroup Core.Text
fgrgPrimaryRegion = Lens.field @"primaryRegion"
{-# INLINEABLE fgrgPrimaryRegion #-}
{-# DEPRECATED primaryRegion "Use generic-lens or generic-optics with 'primaryRegion' instead"  #-}

-- | The name of the primary replication group
--
-- /Note:/ Consider using 'primaryReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgPrimaryReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Core.Text
fgrgPrimaryReplicationGroupId = Lens.field @"primaryReplicationGroupId"
{-# INLINEABLE fgrgPrimaryReplicationGroupId #-}
{-# DEPRECATED primaryReplicationGroupId "Use generic-lens or generic-optics with 'primaryReplicationGroupId' instead"  #-}

instance Core.ToQuery FailoverGlobalReplicationGroup where
        toQuery FailoverGlobalReplicationGroup{..}
          = Core.toQueryPair "Action"
              ("FailoverGlobalReplicationGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "GlobalReplicationGroupId"
                globalReplicationGroupId
              Core.<> Core.toQueryPair "PrimaryRegion" primaryRegion
              Core.<>
              Core.toQueryPair "PrimaryReplicationGroupId"
                primaryReplicationGroupId

instance Core.ToHeaders FailoverGlobalReplicationGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest FailoverGlobalReplicationGroup where
        type Rs FailoverGlobalReplicationGroup =
             FailoverGlobalReplicationGroupResponse
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
          = Response.receiveXMLWrapper "FailoverGlobalReplicationGroupResult"
              (\ s h x ->
                 FailoverGlobalReplicationGroupResponse' Core.<$>
                   (x Core..@? "GlobalReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkFailoverGlobalReplicationGroupResponse' smart constructor.
data FailoverGlobalReplicationGroupResponse = FailoverGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailoverGlobalReplicationGroupResponse' value with any optional fields omitted.
mkFailoverGlobalReplicationGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> FailoverGlobalReplicationGroupResponse
mkFailoverGlobalReplicationGroupResponse responseStatus
  = FailoverGlobalReplicationGroupResponse'{globalReplicationGroup =
                                              Core.Nothing,
                                            responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgrrsGlobalReplicationGroup :: Lens.Lens' FailoverGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
fgrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# INLINEABLE fgrgrrsGlobalReplicationGroup #-}
{-# DEPRECATED globalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgrrsResponseStatus :: Lens.Lens' FailoverGlobalReplicationGroupResponse Core.Int
fgrgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE fgrgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
