{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.DeleteCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS CloudHSM cluster. Before you can delete a cluster, you must delete all HSMs in the cluster. To see if the cluster contains any HSMs, use 'DescribeClusters' . To delete an HSM, use 'DeleteHsm' .
module Network.AWS.CloudHSMv2.DeleteCluster
    (
    -- * Creating a request
      DeleteCluster (..)
    , mkDeleteCluster
    -- ** Request lenses
    , dcClusterId

    -- * Destructuring the response
    , DeleteClusterResponse (..)
    , mkDeleteClusterResponse
    -- ** Response lenses
    , drsCluster
    , drsResponseStatus
    ) where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCluster' smart constructor.
newtype DeleteCluster = DeleteCluster'
  { clusterId :: Types.ClusterId
    -- ^ The identifier (ID) of the cluster that you are deleting. To find the cluster ID, use 'DescribeClusters' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCluster' value with any optional fields omitted.
mkDeleteCluster
    :: Types.ClusterId -- ^ 'clusterId'
    -> DeleteCluster
mkDeleteCluster clusterId = DeleteCluster'{clusterId}

-- | The identifier (ID) of the cluster that you are deleting. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterId :: Lens.Lens' DeleteCluster Types.ClusterId
dcClusterId = Lens.field @"clusterId"
{-# INLINEABLE dcClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

instance Core.ToQuery DeleteCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCluster where
        toHeaders DeleteCluster{..}
          = Core.pure ("X-Amz-Target", "BaldrApiService.DeleteCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCluster where
        toJSON DeleteCluster{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ClusterId" Core..= clusterId)])

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
                   (x Core..:? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
    -- ^ Information about the cluster that was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteClusterResponse' value with any optional fields omitted.
mkDeleteClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteClusterResponse
mkDeleteClusterResponse responseStatus
  = DeleteClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | Information about the cluster that was deleted.
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
