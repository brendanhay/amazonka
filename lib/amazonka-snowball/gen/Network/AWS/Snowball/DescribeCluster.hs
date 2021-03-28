{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific cluster including shipping information, cluster status, and other important metadata.
module Network.AWS.Snowball.DescribeCluster
    (
    -- * Creating a request
      DescribeCluster (..)
    , mkDescribeCluster
    -- ** Request lenses
    , dcClusterId

    -- * Destructuring the response
    , DescribeClusterResponse (..)
    , mkDescribeClusterResponse
    -- ** Response lenses
    , dcrrsClusterMetadata
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkDescribeCluster' smart constructor.
newtype DescribeCluster = DescribeCluster'
  { clusterId :: Types.ClusterId
    -- ^ The automatically generated ID for a cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCluster' value with any optional fields omitted.
mkDescribeCluster
    :: Types.ClusterId -- ^ 'clusterId'
    -> DescribeCluster
mkDescribeCluster clusterId = DescribeCluster'{clusterId}

-- | The automatically generated ID for a cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterId :: Lens.Lens' DescribeCluster Types.ClusterId
dcClusterId = Lens.field @"clusterId"
{-# INLINEABLE dcClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

instance Core.ToQuery DescribeCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCluster where
        toHeaders DescribeCluster{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSIESnowballJobManagementService.DescribeCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCluster where
        toJSON DescribeCluster{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ClusterId" Core..= clusterId)])

instance Core.AWSRequest DescribeCluster where
        type Rs DescribeCluster = DescribeClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeClusterResponse' Core.<$>
                   (x Core..:? "ClusterMetadata") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { clusterMetadata :: Core.Maybe Types.ClusterMetadata
    -- ^ Information about a specific cluster, including shipping information, cluster status, and other important metadata.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeClusterResponse' value with any optional fields omitted.
mkDescribeClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClusterResponse
mkDescribeClusterResponse responseStatus
  = DescribeClusterResponse'{clusterMetadata = Core.Nothing,
                             responseStatus}

-- | Information about a specific cluster, including shipping information, cluster status, and other important metadata.
--
-- /Note:/ Consider using 'clusterMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsClusterMetadata :: Lens.Lens' DescribeClusterResponse (Core.Maybe Types.ClusterMetadata)
dcrrsClusterMetadata = Lens.field @"clusterMetadata"
{-# INLINEABLE dcrrsClusterMetadata #-}
{-# DEPRECATED clusterMetadata "Use generic-lens or generic-optics with 'clusterMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeClusterResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
