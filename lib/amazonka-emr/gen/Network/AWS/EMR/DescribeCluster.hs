{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster-level details including status, hardware and software configuration, VPC settings, and so on.
module Network.AWS.EMR.DescribeCluster
  ( -- * Creating a request
    DescribeCluster (..),
    mkDescribeCluster,

    -- ** Request lenses
    dcClusterId,

    -- * Destructuring the response
    DescribeClusterResponse (..),
    mkDescribeClusterResponse,

    -- ** Response lenses
    dcrrsCluster,
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which cluster to describe.
--
-- /See:/ 'mkDescribeCluster' smart constructor.
newtype DescribeCluster = DescribeCluster'
  { -- | The identifier of the cluster to describe.
    clusterId :: Types.ClusterId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCluster' value with any optional fields omitted.
mkDescribeCluster ::
  -- | 'clusterId'
  Types.ClusterId ->
  DescribeCluster
mkDescribeCluster clusterId = DescribeCluster' {clusterId}

-- | The identifier of the cluster to describe.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterId :: Lens.Lens' DescribeCluster Types.ClusterId
dcClusterId = Lens.field @"clusterId"
{-# DEPRECATED dcClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Core.FromJSON DescribeCluster where
  toJSON DescribeCluster {..} =
    Core.object
      (Core.catMaybes [Core.Just ("ClusterId" Core..= clusterId)])

instance Core.AWSRequest DescribeCluster where
  type Rs DescribeCluster = DescribeClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.DescribeCluster")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClusterResponse'
            Core.<$> (x Core..: "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | This output contains the description of the cluster.
--
-- /See:/ 'mkDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { -- | This output contains the details for the requested cluster.
    cluster :: Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeClusterResponse' value with any optional fields omitted.
mkDescribeClusterResponse ::
  -- | 'cluster'
  Types.Cluster ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeClusterResponse
mkDescribeClusterResponse cluster responseStatus =
  DescribeClusterResponse' {cluster, responseStatus}

-- | This output contains the details for the requested cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCluster :: Lens.Lens' DescribeClusterResponse Types.Cluster
dcrrsCluster = Lens.field @"cluster"
{-# DEPRECATED dcrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeClusterResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
