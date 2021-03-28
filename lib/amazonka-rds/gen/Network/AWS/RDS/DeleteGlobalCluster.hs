{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a global database cluster. The primary and secondary clusters must already be detached or destroyed first. 
module Network.AWS.RDS.DeleteGlobalCluster
    (
    -- * Creating a request
      DeleteGlobalCluster (..)
    , mkDeleteGlobalCluster
    -- ** Request lenses
    , dgcGlobalClusterIdentifier

    -- * Destructuring the response
    , DeleteGlobalClusterResponse (..)
    , mkDeleteGlobalClusterResponse
    -- ** Response lenses
    , dgcrrsGlobalCluster
    , dgcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGlobalCluster' smart constructor.
newtype DeleteGlobalCluster = DeleteGlobalCluster'
  { globalClusterIdentifier :: Core.Text
    -- ^ The cluster identifier of the global database cluster being deleted. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGlobalCluster' value with any optional fields omitted.
mkDeleteGlobalCluster
    :: Core.Text -- ^ 'globalClusterIdentifier'
    -> DeleteGlobalCluster
mkDeleteGlobalCluster globalClusterIdentifier
  = DeleteGlobalCluster'{globalClusterIdentifier}

-- | The cluster identifier of the global database cluster being deleted. 
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcGlobalClusterIdentifier :: Lens.Lens' DeleteGlobalCluster Core.Text
dgcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# INLINEABLE dgcGlobalClusterIdentifier #-}
{-# DEPRECATED globalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead"  #-}

instance Core.ToQuery DeleteGlobalCluster where
        toQuery DeleteGlobalCluster{..}
          = Core.toQueryPair "Action" ("DeleteGlobalCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "GlobalClusterIdentifier" globalClusterIdentifier

instance Core.ToHeaders DeleteGlobalCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteGlobalCluster where
        type Rs DeleteGlobalCluster = DeleteGlobalClusterResponse
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
          = Response.receiveXMLWrapper "DeleteGlobalClusterResult"
              (\ s h x ->
                 DeleteGlobalClusterResponse' Core.<$>
                   (x Core..@? "GlobalCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGlobalClusterResponse' smart constructor.
data DeleteGlobalClusterResponse = DeleteGlobalClusterResponse'
  { globalCluster :: Core.Maybe Types.GlobalCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGlobalClusterResponse' value with any optional fields omitted.
mkDeleteGlobalClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteGlobalClusterResponse
mkDeleteGlobalClusterResponse responseStatus
  = DeleteGlobalClusterResponse'{globalCluster = Core.Nothing,
                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrrsGlobalCluster :: Lens.Lens' DeleteGlobalClusterResponse (Core.Maybe Types.GlobalCluster)
dgcrrsGlobalCluster = Lens.field @"globalCluster"
{-# INLINEABLE dgcrrsGlobalCluster #-}
{-# DEPRECATED globalCluster "Use generic-lens or generic-optics with 'globalCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrrsResponseStatus :: Lens.Lens' DeleteGlobalClusterResponse Core.Int
dgcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
