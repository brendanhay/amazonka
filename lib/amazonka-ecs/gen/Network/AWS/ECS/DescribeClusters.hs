{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your clusters.
module Network.AWS.ECS.DescribeClusters
    (
    -- * Creating a request
      DescribeClusters (..)
    , mkDescribeClusters
    -- ** Request lenses
    , dcClusters
    , dcInclude

    -- * Destructuring the response
    , DescribeClustersResponse (..)
    , mkDescribeClustersResponse
    -- ** Response lenses
    , dcrrsClusters
    , dcrrsFailures
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { clusters :: Core.Maybe [Core.Text]
    -- ^ A list of up to 100 cluster names or full cluster Amazon Resource Name (ARN) entries. If you do not specify a cluster, the default cluster is assumed.
  , include :: Core.Maybe [Types.ClusterField]
    -- ^ Whether to include additional information about your clusters in the response. If this field is omitted, the attachments, statistics, and tags are not included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container instances or tasks within the cluster are included.
-- If @SETTINGS@ is specified, the settings for the cluster are included.
-- If @STATISTICS@ is specified, the following additional information, separated by launch type, is included:
--
--     * runningEC2TasksCount
--
--
--     * runningFargateTasksCount
--
--
--     * pendingEC2TasksCount
--
--
--     * pendingFargateTasksCount
--
--
--     * activeEC2ServiceCount
--
--
--     * activeFargateServiceCount
--
--
--     * drainingEC2ServiceCount
--
--
--     * drainingFargateServiceCount
--
--
-- If @TAGS@ is specified, the metadata tags associated with the cluster are included.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusters' value with any optional fields omitted.
mkDescribeClusters
    :: DescribeClusters
mkDescribeClusters
  = DescribeClusters'{clusters = Core.Nothing,
                      include = Core.Nothing}

-- | A list of up to 100 cluster names or full cluster Amazon Resource Name (ARN) entries. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusters :: Lens.Lens' DescribeClusters (Core.Maybe [Core.Text])
dcClusters = Lens.field @"clusters"
{-# INLINEABLE dcClusters #-}
{-# DEPRECATED clusters "Use generic-lens or generic-optics with 'clusters' instead"  #-}

-- | Whether to include additional information about your clusters in the response. If this field is omitted, the attachments, statistics, and tags are not included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container instances or tasks within the cluster are included.
-- If @SETTINGS@ is specified, the settings for the cluster are included.
-- If @STATISTICS@ is specified, the following additional information, separated by launch type, is included:
--
--     * runningEC2TasksCount
--
--
--     * runningFargateTasksCount
--
--
--     * pendingEC2TasksCount
--
--
--     * pendingFargateTasksCount
--
--
--     * activeEC2ServiceCount
--
--
--     * activeFargateServiceCount
--
--
--     * drainingEC2ServiceCount
--
--
--     * drainingFargateServiceCount
--
--
-- If @TAGS@ is specified, the metadata tags associated with the cluster are included.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcInclude :: Lens.Lens' DescribeClusters (Core.Maybe [Types.ClusterField])
dcInclude = Lens.field @"include"
{-# INLINEABLE dcInclude #-}
{-# DEPRECATED include "Use generic-lens or generic-optics with 'include' instead"  #-}

instance Core.ToQuery DescribeClusters where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeClusters where
        toHeaders DescribeClusters{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DescribeClusters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeClusters where
        toJSON DescribeClusters{..}
          = Core.object
              (Core.catMaybes
                 [("clusters" Core..=) Core.<$> clusters,
                  ("include" Core..=) Core.<$> include])

instance Core.AWSRequest DescribeClusters where
        type Rs DescribeClusters = DescribeClustersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeClustersResponse' Core.<$>
                   (x Core..:? "clusters") Core.<*> x Core..:? "failures" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { clusters :: Core.Maybe [Types.Cluster]
    -- ^ The list of clusters.
  , failures :: Core.Maybe [Types.Failure]
    -- ^ Any failures associated with the call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClustersResponse' value with any optional fields omitted.
mkDescribeClustersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClustersResponse
mkDescribeClustersResponse responseStatus
  = DescribeClustersResponse'{clusters = Core.Nothing,
                              failures = Core.Nothing, responseStatus}

-- | The list of clusters.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsClusters :: Lens.Lens' DescribeClustersResponse (Core.Maybe [Types.Cluster])
dcrrsClusters = Lens.field @"clusters"
{-# INLINEABLE dcrrsClusters #-}
{-# DEPRECATED clusters "Use generic-lens or generic-optics with 'clusters' instead"  #-}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsFailures :: Lens.Lens' DescribeClustersResponse (Core.Maybe [Types.Failure])
dcrrsFailures = Lens.field @"failures"
{-# INLINEABLE dcrrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeClustersResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
