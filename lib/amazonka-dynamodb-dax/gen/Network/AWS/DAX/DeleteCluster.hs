{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteCluster (..),
    mkDeleteCluster,

    -- ** Request lenses
    dcClusterName,

    -- * Destructuring the response
    DeleteClusterResponse (..),
    mkDeleteClusterResponse,

    -- ** Response lenses
    drsCluster,
    drsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCluster' smart constructor.
newtype DeleteCluster = DeleteCluster'
  { -- | The name of the cluster to be deleted.
    clusterName :: Types.ClusterName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCluster' value with any optional fields omitted.
mkDeleteCluster ::
  -- | 'clusterName'
  Types.ClusterName ->
  DeleteCluster
mkDeleteCluster clusterName = DeleteCluster' {clusterName}

-- | The name of the cluster to be deleted.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterName :: Lens.Lens' DeleteCluster Types.ClusterName
dcClusterName = Lens.field @"clusterName"
{-# DEPRECATED dcClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

instance Core.FromJSON DeleteCluster where
  toJSON DeleteCluster {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ClusterName" Core..= clusterName)])

instance Core.AWSRequest DeleteCluster where
  type Rs DeleteCluster = DeleteClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.DeleteCluster")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteClusterResponse'
            Core.<$> (x Core..:? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { -- | A description of the DAX cluster that is being deleted.
    cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteClusterResponse' value with any optional fields omitted.
mkDeleteClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteClusterResponse
mkDeleteClusterResponse responseStatus =
  DeleteClusterResponse' {cluster = Core.Nothing, responseStatus}

-- | A description of the DAX cluster that is being deleted.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCluster :: Lens.Lens' DeleteClusterResponse (Core.Maybe Types.Cluster)
drsCluster = Lens.field @"cluster"
{-# DEPRECATED drsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteClusterResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
