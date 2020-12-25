{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task set. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateTaskSet
  ( -- * Creating a request
    UpdateTaskSet (..),
    mkUpdateTaskSet,

    -- ** Request lenses
    utsCluster,
    utsService,
    utsTaskSet,
    utsScale,

    -- * Destructuring the response
    UpdateTaskSetResponse (..),
    mkUpdateTaskSetResponse,

    -- ** Response lenses
    utsrrsTaskSet,
    utsrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTaskSet' smart constructor.
data UpdateTaskSet = UpdateTaskSet'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
    cluster :: Types.String,
    -- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
    service :: Types.String,
    -- | The short name or full Amazon Resource Name (ARN) of the task set to update.
    taskSet :: Types.String,
    scale :: Types.Scale
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTaskSet' value with any optional fields omitted.
mkUpdateTaskSet ::
  -- | 'cluster'
  Types.String ->
  -- | 'service'
  Types.String ->
  -- | 'taskSet'
  Types.String ->
  -- | 'scale'
  Types.Scale ->
  UpdateTaskSet
mkUpdateTaskSet cluster service taskSet scale =
  UpdateTaskSet' {cluster, service, taskSet, scale}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsCluster :: Lens.Lens' UpdateTaskSet Types.String
utsCluster = Lens.field @"cluster"
{-# DEPRECATED utsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsService :: Lens.Lens' UpdateTaskSet Types.String
utsService = Lens.field @"service"
{-# DEPRECATED utsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the task set to update.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsTaskSet :: Lens.Lens' UpdateTaskSet Types.String
utsTaskSet = Lens.field @"taskSet"
{-# DEPRECATED utsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsScale :: Lens.Lens' UpdateTaskSet Types.Scale
utsScale = Lens.field @"scale"
{-# DEPRECATED utsScale "Use generic-lens or generic-optics with 'scale' instead." #-}

instance Core.FromJSON UpdateTaskSet where
  toJSON UpdateTaskSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("cluster" Core..= cluster),
            Core.Just ("service" Core..= service),
            Core.Just ("taskSet" Core..= taskSet),
            Core.Just ("scale" Core..= scale)
          ]
      )

instance Core.AWSRequest UpdateTaskSet where
  type Rs UpdateTaskSet = UpdateTaskSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.UpdateTaskSet"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTaskSetResponse'
            Core.<$> (x Core..:? "taskSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateTaskSetResponse' smart constructor.
data UpdateTaskSetResponse = UpdateTaskSetResponse'
  { taskSet :: Core.Maybe Types.TaskSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateTaskSetResponse' value with any optional fields omitted.
mkUpdateTaskSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTaskSetResponse
mkUpdateTaskSetResponse responseStatus =
  UpdateTaskSetResponse' {taskSet = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrrsTaskSet :: Lens.Lens' UpdateTaskSetResponse (Core.Maybe Types.TaskSet)
utsrrsTaskSet = Lens.field @"taskSet"
{-# DEPRECATED utsrrsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrrsResponseStatus :: Lens.Lens' UpdateTaskSetResponse Core.Int
utsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
