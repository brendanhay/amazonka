{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster. The cluster will transition to the @INACTIVE@ state. Clusters with an @INACTIVE@ status may remain discoverable in your account for a period of time. However, this behavior is subject to change in the future, so you should not rely on @INACTIVE@ clusters persisting.
--
-- You must deregister all container instances from this cluster before you may delete it. You can list the container instances in a cluster with 'ListContainerInstances' and deregister them with 'DeregisterContainerInstance' .
module Network.AWS.ECS.DeleteCluster
    (
    -- * Creating a request
      DeleteCluster (..)
    , mkDeleteCluster
    -- ** Request lenses
    , dcCluster

    -- * Destructuring the response
    , DeleteClusterResponse (..)
    , mkDeleteClusterResponse
    -- ** Response lenses
    , drsCluster
    , drsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCluster' smart constructor.
newtype DeleteCluster = DeleteCluster'
  { cluster :: Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCluster' value with any optional fields omitted.
mkDeleteCluster
    :: Core.Text -- ^ 'cluster'
    -> DeleteCluster
mkDeleteCluster cluster = DeleteCluster'{cluster}

-- | The short name or full Amazon Resource Name (ARN) of the cluster to delete.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCluster :: Lens.Lens' DeleteCluster Core.Text
dcCluster = Lens.field @"cluster"
{-# INLINEABLE dcCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

instance Core.ToQuery DeleteCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCluster where
        toHeaders DeleteCluster{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DeleteCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCluster where
        toJSON DeleteCluster{..}
          = Core.object
              (Core.catMaybes [Core.Just ("cluster" Core..= cluster)])

instance Core.AWSRequest DeleteCluster where
        type Rs DeleteCluster = DeleteClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteClusterResponse' Core.<$>
                   (x Core..:? "cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
    -- ^ The full description of the deleted cluster.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterResponse' value with any optional fields omitted.
mkDeleteClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteClusterResponse
mkDeleteClusterResponse responseStatus
  = DeleteClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | The full description of the deleted cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCluster :: Lens.Lens' DeleteClusterResponse (Core.Maybe Types.Cluster)
drsCluster = Lens.field @"cluster"
{-# INLINEABLE drsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteClusterResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
