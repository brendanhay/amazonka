{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.PauseCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pauses a cluster.
module Network.AWS.Redshift.PauseCluster
    (
    -- * Creating a request
      PauseCluster (..)
    , mkPauseCluster
    -- ** Request lenses
    , pcClusterIdentifier

    -- * Destructuring the response
    , PauseClusterResponse (..)
    , mkPauseClusterResponse
    -- ** Response lenses
    , pcrrsCluster
    , pcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a pause cluster operation. For example, a scheduled action to run the @PauseCluster@ API operation. 
--
-- /See:/ 'mkPauseCluster' smart constructor.
newtype PauseCluster = PauseCluster'
  { clusterIdentifier :: Core.Text
    -- ^ The identifier of the cluster to be paused.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PauseCluster' value with any optional fields omitted.
mkPauseCluster
    :: Core.Text -- ^ 'clusterIdentifier'
    -> PauseCluster
mkPauseCluster clusterIdentifier = PauseCluster'{clusterIdentifier}

-- | The identifier of the cluster to be paused.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcClusterIdentifier :: Lens.Lens' PauseCluster Core.Text
pcClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE pcClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

instance Core.ToQuery PauseCluster where
        toQuery PauseCluster{..}
          = Core.toQueryPair "Action" ("PauseCluster" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier

instance Core.ToHeaders PauseCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PauseCluster where
        type Rs PauseCluster = PauseClusterResponse
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
          = Response.receiveXMLWrapper "PauseClusterResult"
              (\ s h x ->
                 PauseClusterResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPauseClusterResponse' smart constructor.
data PauseClusterResponse = PauseClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PauseClusterResponse' value with any optional fields omitted.
mkPauseClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PauseClusterResponse
mkPauseClusterResponse responseStatus
  = PauseClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsCluster :: Lens.Lens' PauseClusterResponse (Core.Maybe Types.Cluster)
pcrrsCluster = Lens.field @"cluster"
{-# INLINEABLE pcrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsResponseStatus :: Lens.Lens' PauseClusterResponse Core.Int
pcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
