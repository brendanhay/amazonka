{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified task set within a service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.DeleteTaskSet
  ( -- * Creating a request
    DeleteTaskSet (..),
    mkDeleteTaskSet,

    -- ** Request lenses
    dtsCluster,
    dtsService,
    dtsTaskSet,
    dtsForce,

    -- * Destructuring the response
    DeleteTaskSetResponse (..),
    mkDeleteTaskSetResponse,

    -- ** Response lenses
    dtsrrsTaskSet,
    dtsrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTaskSet' smart constructor.
data DeleteTaskSet = DeleteTaskSet'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in to delete.
    cluster :: Types.String,
    -- | The short name or full Amazon Resource Name (ARN) of the service that hosts the task set to delete.
    service :: Types.String,
    -- | The task set ID or full Amazon Resource Name (ARN) of the task set to delete.
    taskSet :: Types.String,
    -- | If @true@ , this allows you to delete a task set even if it hasn't been scaled down to zero.
    force :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTaskSet' value with any optional fields omitted.
mkDeleteTaskSet ::
  -- | 'cluster'
  Types.String ->
  -- | 'service'
  Types.String ->
  -- | 'taskSet'
  Types.String ->
  DeleteTaskSet
mkDeleteTaskSet cluster service taskSet =
  DeleteTaskSet' {cluster, service, taskSet, force = Core.Nothing}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in to delete.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsCluster :: Lens.Lens' DeleteTaskSet Types.String
dtsCluster = Lens.field @"cluster"
{-# DEPRECATED dtsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that hosts the task set to delete.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsService :: Lens.Lens' DeleteTaskSet Types.String
dtsService = Lens.field @"service"
{-# DEPRECATED dtsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The task set ID or full Amazon Resource Name (ARN) of the task set to delete.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsTaskSet :: Lens.Lens' DeleteTaskSet Types.String
dtsTaskSet = Lens.field @"taskSet"
{-# DEPRECATED dtsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | If @true@ , this allows you to delete a task set even if it hasn't been scaled down to zero.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsForce :: Lens.Lens' DeleteTaskSet (Core.Maybe Core.Bool)
dtsForce = Lens.field @"force"
{-# DEPRECATED dtsForce "Use generic-lens or generic-optics with 'force' instead." #-}

instance Core.FromJSON DeleteTaskSet where
  toJSON DeleteTaskSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("cluster" Core..= cluster),
            Core.Just ("service" Core..= service),
            Core.Just ("taskSet" Core..= taskSet),
            ("force" Core..=) Core.<$> force
          ]
      )

instance Core.AWSRequest DeleteTaskSet where
  type Rs DeleteTaskSet = DeleteTaskSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.DeleteTaskSet"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTaskSetResponse'
            Core.<$> (x Core..:? "taskSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteTaskSetResponse' smart constructor.
data DeleteTaskSetResponse = DeleteTaskSetResponse'
  { taskSet :: Core.Maybe Types.TaskSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteTaskSetResponse' value with any optional fields omitted.
mkDeleteTaskSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteTaskSetResponse
mkDeleteTaskSetResponse responseStatus =
  DeleteTaskSetResponse' {taskSet = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsTaskSet :: Lens.Lens' DeleteTaskSetResponse (Core.Maybe Types.TaskSet)
dtsrrsTaskSet = Lens.field @"taskSet"
{-# DEPRECATED dtsrrsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrrsResponseStatus :: Lens.Lens' DeleteTaskSetResponse Core.Int
dtsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
