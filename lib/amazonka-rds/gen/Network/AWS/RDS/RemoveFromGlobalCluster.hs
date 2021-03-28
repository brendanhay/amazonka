{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveFromGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an Aurora secondary cluster from an Aurora global database cluster. The cluster becomes a standalone cluster with read-write capability instead of being read-only and receiving data from a primary cluster in a different region. 
module Network.AWS.RDS.RemoveFromGlobalCluster
    (
    -- * Creating a request
      RemoveFromGlobalCluster (..)
    , mkRemoveFromGlobalCluster
    -- ** Request lenses
    , rfgcDbClusterIdentifier
    , rfgcGlobalClusterIdentifier

    -- * Destructuring the response
    , RemoveFromGlobalClusterResponse (..)
    , mkRemoveFromGlobalClusterResponse
    -- ** Response lenses
    , rfgcrrsGlobalCluster
    , rfgcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveFromGlobalCluster' smart constructor.
data RemoveFromGlobalCluster = RemoveFromGlobalCluster'
  { dbClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) identifying the cluster that was detached from the Aurora global database cluster. 
  , globalClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The cluster identifier to detach from the Aurora global database cluster. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveFromGlobalCluster' value with any optional fields omitted.
mkRemoveFromGlobalCluster
    :: RemoveFromGlobalCluster
mkRemoveFromGlobalCluster
  = RemoveFromGlobalCluster'{dbClusterIdentifier = Core.Nothing,
                             globalClusterIdentifier = Core.Nothing}

-- | The Amazon Resource Name (ARN) identifying the cluster that was detached from the Aurora global database cluster. 
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcDbClusterIdentifier :: Lens.Lens' RemoveFromGlobalCluster (Core.Maybe Core.Text)
rfgcDbClusterIdentifier = Lens.field @"dbClusterIdentifier"
{-# INLINEABLE rfgcDbClusterIdentifier #-}
{-# DEPRECATED dbClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead"  #-}

-- | The cluster identifier to detach from the Aurora global database cluster. 
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcGlobalClusterIdentifier :: Lens.Lens' RemoveFromGlobalCluster (Core.Maybe Core.Text)
rfgcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# INLINEABLE rfgcGlobalClusterIdentifier #-}
{-# DEPRECATED globalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead"  #-}

instance Core.ToQuery RemoveFromGlobalCluster where
        toQuery RemoveFromGlobalCluster{..}
          = Core.toQueryPair "Action"
              ("RemoveFromGlobalCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DbClusterIdentifier")
                dbClusterIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GlobalClusterIdentifier")
                globalClusterIdentifier

instance Core.ToHeaders RemoveFromGlobalCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RemoveFromGlobalCluster where
        type Rs RemoveFromGlobalCluster = RemoveFromGlobalClusterResponse
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
          = Response.receiveXMLWrapper "RemoveFromGlobalClusterResult"
              (\ s h x ->
                 RemoveFromGlobalClusterResponse' Core.<$>
                   (x Core..@? "GlobalCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveFromGlobalClusterResponse' smart constructor.
data RemoveFromGlobalClusterResponse = RemoveFromGlobalClusterResponse'
  { globalCluster :: Core.Maybe Types.GlobalCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveFromGlobalClusterResponse' value with any optional fields omitted.
mkRemoveFromGlobalClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveFromGlobalClusterResponse
mkRemoveFromGlobalClusterResponse responseStatus
  = RemoveFromGlobalClusterResponse'{globalCluster = Core.Nothing,
                                     responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcrrsGlobalCluster :: Lens.Lens' RemoveFromGlobalClusterResponse (Core.Maybe Types.GlobalCluster)
rfgcrrsGlobalCluster = Lens.field @"globalCluster"
{-# INLINEABLE rfgcrrsGlobalCluster #-}
{-# DEPRECATED globalCluster "Use generic-lens or generic-optics with 'globalCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcrrsResponseStatus :: Lens.Lens' RemoveFromGlobalClusterResponse Core.Int
rfgcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rfgcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
