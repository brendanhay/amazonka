{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeContainerInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon Elastic Container Service container instances. Returns metadata about registered and remaining resources on each container instance requested.
module Network.AWS.ECS.DescribeContainerInstances
    (
    -- * Creating a request
      DescribeContainerInstances (..)
    , mkDescribeContainerInstances
    -- ** Request lenses
    , dciContainerInstances
    , dciCluster
    , dciInclude

    -- * Destructuring the response
    , DescribeContainerInstancesResponse (..)
    , mkDescribeContainerInstancesResponse
    -- ** Response lenses
    , dcirfrsContainerInstances
    , dcirfrsFailures
    , dcirfrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeContainerInstances' smart constructor.
data DescribeContainerInstances = DescribeContainerInstances'
  { containerInstances :: [Core.Text]
    -- ^ A list of up to 100 container instance IDs or full Amazon Resource Name (ARN) entries.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the container instance or container instances you are describing were launched in any cluster other than the default cluster.
  , include :: Core.Maybe [Types.ContainerInstanceField]
    -- ^ Specifies whether you want to see the resource tags for the container instance. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContainerInstances' value with any optional fields omitted.
mkDescribeContainerInstances
    :: DescribeContainerInstances
mkDescribeContainerInstances
  = DescribeContainerInstances'{containerInstances = Core.mempty,
                                cluster = Core.Nothing, include = Core.Nothing}

-- | A list of up to 100 container instance IDs or full Amazon Resource Name (ARN) entries.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciContainerInstances :: Lens.Lens' DescribeContainerInstances [Core.Text]
dciContainerInstances = Lens.field @"containerInstances"
{-# INLINEABLE dciContainerInstances #-}
{-# DEPRECATED containerInstances "Use generic-lens or generic-optics with 'containerInstances' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the container instance or container instances you are describing were launched in any cluster other than the default cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciCluster :: Lens.Lens' DescribeContainerInstances (Core.Maybe Core.Text)
dciCluster = Lens.field @"cluster"
{-# INLINEABLE dciCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | Specifies whether you want to see the resource tags for the container instance. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciInclude :: Lens.Lens' DescribeContainerInstances (Core.Maybe [Types.ContainerInstanceField])
dciInclude = Lens.field @"include"
{-# INLINEABLE dciInclude #-}
{-# DEPRECATED include "Use generic-lens or generic-optics with 'include' instead"  #-}

instance Core.ToQuery DescribeContainerInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeContainerInstances where
        toHeaders DescribeContainerInstances{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DescribeContainerInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeContainerInstances where
        toJSON DescribeContainerInstances{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("containerInstances" Core..= containerInstances),
                  ("cluster" Core..=) Core.<$> cluster,
                  ("include" Core..=) Core.<$> include])

instance Core.AWSRequest DescribeContainerInstances where
        type Rs DescribeContainerInstances =
             DescribeContainerInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeContainerInstancesResponse' Core.<$>
                   (x Core..:? "containerInstances") Core.<*> x Core..:? "failures"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeContainerInstancesResponse' smart constructor.
data DescribeContainerInstancesResponse = DescribeContainerInstancesResponse'
  { containerInstances :: Core.Maybe [Types.ContainerInstance]
    -- ^ The list of container instances.
  , failures :: Core.Maybe [Types.Failure]
    -- ^ Any failures associated with the call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeContainerInstancesResponse' value with any optional fields omitted.
mkDescribeContainerInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeContainerInstancesResponse
mkDescribeContainerInstancesResponse responseStatus
  = DescribeContainerInstancesResponse'{containerInstances =
                                          Core.Nothing,
                                        failures = Core.Nothing, responseStatus}

-- | The list of container instances.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirfrsContainerInstances :: Lens.Lens' DescribeContainerInstancesResponse (Core.Maybe [Types.ContainerInstance])
dcirfrsContainerInstances = Lens.field @"containerInstances"
{-# INLINEABLE dcirfrsContainerInstances #-}
{-# DEPRECATED containerInstances "Use generic-lens or generic-optics with 'containerInstances' instead"  #-}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirfrsFailures :: Lens.Lens' DescribeContainerInstancesResponse (Core.Maybe [Types.Failure])
dcirfrsFailures = Lens.field @"failures"
{-# INLINEABLE dcirfrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirfrsResponseStatus :: Lens.Lens' DescribeContainerInstancesResponse Core.Int
dcirfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcirfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
