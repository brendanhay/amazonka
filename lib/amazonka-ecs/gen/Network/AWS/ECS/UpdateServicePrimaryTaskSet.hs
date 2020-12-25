{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateServicePrimaryTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies which task set in a service is the primary task set. Any parameters that are updated on the primary task set in a service will transition to the service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateServicePrimaryTaskSet
  ( -- * Creating a request
    UpdateServicePrimaryTaskSet (..),
    mkUpdateServicePrimaryTaskSet,

    -- ** Request lenses
    usptsCluster,
    usptsService,
    usptsPrimaryTaskSet,

    -- * Destructuring the response
    UpdateServicePrimaryTaskSetResponse (..),
    mkUpdateServicePrimaryTaskSetResponse,

    -- ** Response lenses
    usptsrrsTaskSet,
    usptsrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateServicePrimaryTaskSet' smart constructor.
data UpdateServicePrimaryTaskSet = UpdateServicePrimaryTaskSet'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
    cluster :: Types.String,
    -- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
    service :: Types.String,
    -- | The short name or full Amazon Resource Name (ARN) of the task set to set as the primary task set in the deployment.
    primaryTaskSet :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServicePrimaryTaskSet' value with any optional fields omitted.
mkUpdateServicePrimaryTaskSet ::
  -- | 'cluster'
  Types.String ->
  -- | 'service'
  Types.String ->
  -- | 'primaryTaskSet'
  Types.String ->
  UpdateServicePrimaryTaskSet
mkUpdateServicePrimaryTaskSet cluster service primaryTaskSet =
  UpdateServicePrimaryTaskSet' {cluster, service, primaryTaskSet}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsCluster :: Lens.Lens' UpdateServicePrimaryTaskSet Types.String
usptsCluster = Lens.field @"cluster"
{-# DEPRECATED usptsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsService :: Lens.Lens' UpdateServicePrimaryTaskSet Types.String
usptsService = Lens.field @"service"
{-# DEPRECATED usptsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the task set to set as the primary task set in the deployment.
--
-- /Note:/ Consider using 'primaryTaskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsPrimaryTaskSet :: Lens.Lens' UpdateServicePrimaryTaskSet Types.String
usptsPrimaryTaskSet = Lens.field @"primaryTaskSet"
{-# DEPRECATED usptsPrimaryTaskSet "Use generic-lens or generic-optics with 'primaryTaskSet' instead." #-}

instance Core.FromJSON UpdateServicePrimaryTaskSet where
  toJSON UpdateServicePrimaryTaskSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("cluster" Core..= cluster),
            Core.Just ("service" Core..= service),
            Core.Just ("primaryTaskSet" Core..= primaryTaskSet)
          ]
      )

instance Core.AWSRequest UpdateServicePrimaryTaskSet where
  type
    Rs UpdateServicePrimaryTaskSet =
      UpdateServicePrimaryTaskSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.UpdateServicePrimaryTaskSet"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServicePrimaryTaskSetResponse'
            Core.<$> (x Core..:? "taskSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateServicePrimaryTaskSetResponse' smart constructor.
data UpdateServicePrimaryTaskSetResponse = UpdateServicePrimaryTaskSetResponse'
  { taskSet :: Core.Maybe Types.TaskSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateServicePrimaryTaskSetResponse' value with any optional fields omitted.
mkUpdateServicePrimaryTaskSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateServicePrimaryTaskSetResponse
mkUpdateServicePrimaryTaskSetResponse responseStatus =
  UpdateServicePrimaryTaskSetResponse'
    { taskSet = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsrrsTaskSet :: Lens.Lens' UpdateServicePrimaryTaskSetResponse (Core.Maybe Types.TaskSet)
usptsrrsTaskSet = Lens.field @"taskSet"
{-# DEPRECATED usptsrrsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsrrsResponseStatus :: Lens.Lens' UpdateServicePrimaryTaskSetResponse Core.Int
usptsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usptsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
