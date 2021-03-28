{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DeleteCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned DAX cluster. /DeleteCluster/ deletes all associated nodes, node endpoints and the DAX cluster itself. When you receive a successful response from this action, DAX immediately begins deleting the cluster; you cannot cancel or revert this action.
module Network.AWS.DAX.DeleteCluster
    (
    -- * Creating a request
      DeleteCluster (..)
    , mkDeleteCluster
    -- ** Request lenses
    , dcClusterName

    -- * Destructuring the response
    , DeleteClusterResponse (..)
    , mkDeleteClusterResponse
    -- ** Response lenses
    , drsCluster
    , drsResponseStatus
    ) where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCluster' smart constructor.
newtype DeleteCluster = DeleteCluster'
  { clusterName :: Core.Text
    -- ^ The name of the cluster to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCluster' value with any optional fields omitted.
mkDeleteCluster
    :: Core.Text -- ^ 'clusterName'
    -> DeleteCluster
mkDeleteCluster clusterName = DeleteCluster'{clusterName}

-- | The name of the cluster to be deleted.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterName :: Lens.Lens' DeleteCluster Core.Text
dcClusterName = Lens.field @"clusterName"
{-# INLINEABLE dcClusterName #-}
{-# DEPRECATED clusterName "Use generic-lens or generic-optics with 'clusterName' instead"  #-}

instance Core.ToQuery DeleteCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCluster where
        toHeaders DeleteCluster{..}
          = Core.pure ("X-Amz-Target", "AmazonDAXV3.DeleteCluster") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCluster where
        toJSON DeleteCluster{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ClusterName" Core..= clusterName)])

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
    -- ^ A description of the DAX cluster that is being deleted.
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

-- | A description of the DAX cluster that is being deleted.
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
