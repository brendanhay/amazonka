{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeContainerInstances (..),
    mkDescribeContainerInstances,

    -- ** Request lenses
    dciContainerInstances,
    dciCluster,
    dciInclude,

    -- * Destructuring the response
    DescribeContainerInstancesResponse (..),
    mkDescribeContainerInstancesResponse,

    -- ** Response lenses
    dcirfrsContainerInstances,
    dcirfrsFailures,
    dcirfrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeContainerInstances' smart constructor.
data DescribeContainerInstances = DescribeContainerInstances'
  { -- | A list of up to 100 container instance IDs or full Amazon Resource Name (ARN) entries.
    containerInstances :: [Types.String],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the container instance or container instances you are describing were launched in any cluster other than the default cluster.
    cluster :: Core.Maybe Types.String,
    -- | Specifies whether you want to see the resource tags for the container instance. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
    include :: Core.Maybe [Types.ContainerInstanceField]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContainerInstances' value with any optional fields omitted.
mkDescribeContainerInstances ::
  DescribeContainerInstances
mkDescribeContainerInstances =
  DescribeContainerInstances'
    { containerInstances = Core.mempty,
      cluster = Core.Nothing,
      include = Core.Nothing
    }

-- | A list of up to 100 container instance IDs or full Amazon Resource Name (ARN) entries.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciContainerInstances :: Lens.Lens' DescribeContainerInstances [Types.String]
dciContainerInstances = Lens.field @"containerInstances"
{-# DEPRECATED dciContainerInstances "Use generic-lens or generic-optics with 'containerInstances' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the container instance or container instances you are describing were launched in any cluster other than the default cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciCluster :: Lens.Lens' DescribeContainerInstances (Core.Maybe Types.String)
dciCluster = Lens.field @"cluster"
{-# DEPRECATED dciCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | Specifies whether you want to see the resource tags for the container instance. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciInclude :: Lens.Lens' DescribeContainerInstances (Core.Maybe [Types.ContainerInstanceField])
dciInclude = Lens.field @"include"
{-# DEPRECATED dciInclude "Use generic-lens or generic-optics with 'include' instead." #-}

instance Core.FromJSON DescribeContainerInstances where
  toJSON DescribeContainerInstances {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("containerInstances" Core..= containerInstances),
            ("cluster" Core..=) Core.<$> cluster,
            ("include" Core..=) Core.<$> include
          ]
      )

instance Core.AWSRequest DescribeContainerInstances where
  type
    Rs DescribeContainerInstances =
      DescribeContainerInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.DescribeContainerInstances"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContainerInstancesResponse'
            Core.<$> (x Core..:? "containerInstances")
            Core.<*> (x Core..:? "failures")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeContainerInstancesResponse' smart constructor.
data DescribeContainerInstancesResponse = DescribeContainerInstancesResponse'
  { -- | The list of container instances.
    containerInstances :: Core.Maybe [Types.ContainerInstance],
    -- | Any failures associated with the call.
    failures :: Core.Maybe [Types.Failure],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeContainerInstancesResponse' value with any optional fields omitted.
mkDescribeContainerInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeContainerInstancesResponse
mkDescribeContainerInstancesResponse responseStatus =
  DescribeContainerInstancesResponse'
    { containerInstances =
        Core.Nothing,
      failures = Core.Nothing,
      responseStatus
    }

-- | The list of container instances.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirfrsContainerInstances :: Lens.Lens' DescribeContainerInstancesResponse (Core.Maybe [Types.ContainerInstance])
dcirfrsContainerInstances = Lens.field @"containerInstances"
{-# DEPRECATED dcirfrsContainerInstances "Use generic-lens or generic-optics with 'containerInstances' instead." #-}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirfrsFailures :: Lens.Lens' DescribeContainerInstancesResponse (Core.Maybe [Types.Failure])
dcirfrsFailures = Lens.field @"failures"
{-# DEPRECATED dcirfrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirfrsResponseStatus :: Lens.Lens' DescribeContainerInstancesResponse Core.Int
dcirfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcirfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
