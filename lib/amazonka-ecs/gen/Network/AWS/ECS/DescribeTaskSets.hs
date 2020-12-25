{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeTaskSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the task sets in the specified cluster and service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.DescribeTaskSets
  ( -- * Creating a request
    DescribeTaskSets (..),
    mkDescribeTaskSets,

    -- ** Request lenses
    dtssCluster,
    dtssService,
    dtssInclude,
    dtssTaskSets,

    -- * Destructuring the response
    DescribeTaskSetsResponse (..),
    mkDescribeTaskSetsResponse,

    -- ** Response lenses
    dtsrfrsFailures,
    dtsrfrsTaskSets,
    dtsrfrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTaskSets' smart constructor.
data DescribeTaskSets = DescribeTaskSets'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task sets exist in.
    cluster :: Types.String,
    -- | The short name or full Amazon Resource Name (ARN) of the service that the task sets exist in.
    service :: Types.String,
    -- | Specifies whether to see the resource tags for the task set. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
    include :: Core.Maybe [Types.TaskSetField],
    -- | The ID or full Amazon Resource Name (ARN) of task sets to describe.
    taskSets :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTaskSets' value with any optional fields omitted.
mkDescribeTaskSets ::
  -- | 'cluster'
  Types.String ->
  -- | 'service'
  Types.String ->
  DescribeTaskSets
mkDescribeTaskSets cluster service =
  DescribeTaskSets'
    { cluster,
      service,
      include = Core.Nothing,
      taskSets = Core.Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task sets exist in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssCluster :: Lens.Lens' DescribeTaskSets Types.String
dtssCluster = Lens.field @"cluster"
{-# DEPRECATED dtssCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that the task sets exist in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssService :: Lens.Lens' DescribeTaskSets Types.String
dtssService = Lens.field @"service"
{-# DEPRECATED dtssService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | Specifies whether to see the resource tags for the task set. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssInclude :: Lens.Lens' DescribeTaskSets (Core.Maybe [Types.TaskSetField])
dtssInclude = Lens.field @"include"
{-# DEPRECATED dtssInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The ID or full Amazon Resource Name (ARN) of task sets to describe.
--
-- /Note:/ Consider using 'taskSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssTaskSets :: Lens.Lens' DescribeTaskSets (Core.Maybe [Types.String])
dtssTaskSets = Lens.field @"taskSets"
{-# DEPRECATED dtssTaskSets "Use generic-lens or generic-optics with 'taskSets' instead." #-}

instance Core.FromJSON DescribeTaskSets where
  toJSON DescribeTaskSets {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("cluster" Core..= cluster),
            Core.Just ("service" Core..= service),
            ("include" Core..=) Core.<$> include,
            ("taskSets" Core..=) Core.<$> taskSets
          ]
      )

instance Core.AWSRequest DescribeTaskSets where
  type Rs DescribeTaskSets = DescribeTaskSetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.DescribeTaskSets"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTaskSetsResponse'
            Core.<$> (x Core..:? "failures")
            Core.<*> (x Core..:? "taskSets")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTaskSetsResponse' smart constructor.
data DescribeTaskSetsResponse = DescribeTaskSetsResponse'
  { -- | Any failures associated with the call.
    failures :: Core.Maybe [Types.Failure],
    -- | The list of task sets described.
    taskSets :: Core.Maybe [Types.TaskSet],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTaskSetsResponse' value with any optional fields omitted.
mkDescribeTaskSetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTaskSetsResponse
mkDescribeTaskSetsResponse responseStatus =
  DescribeTaskSetsResponse'
    { failures = Core.Nothing,
      taskSets = Core.Nothing,
      responseStatus
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrfrsFailures :: Lens.Lens' DescribeTaskSetsResponse (Core.Maybe [Types.Failure])
dtsrfrsFailures = Lens.field @"failures"
{-# DEPRECATED dtsrfrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The list of task sets described.
--
-- /Note:/ Consider using 'taskSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrfrsTaskSets :: Lens.Lens' DescribeTaskSetsResponse (Core.Maybe [Types.TaskSet])
dtsrfrsTaskSets = Lens.field @"taskSets"
{-# DEPRECATED dtsrfrsTaskSets "Use generic-lens or generic-optics with 'taskSets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrfrsResponseStatus :: Lens.Lens' DescribeTaskSetsResponse Core.Int
dtsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
