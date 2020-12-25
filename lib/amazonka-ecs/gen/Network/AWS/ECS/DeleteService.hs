{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service within a cluster. You can delete a service if you have no running tasks in it and the desired task count is zero. If the service is actively maintaining tasks, you cannot delete it, and you must update the service to a desired task count of zero. For more information, see 'UpdateService' .
--
-- /Important:/ If you attempt to create a new service with the same name as an existing service in either @ACTIVE@ or @DRAINING@ status, you receive an error.
module Network.AWS.ECS.DeleteService
  ( -- * Creating a request
    DeleteService (..),
    mkDeleteService,

    -- ** Request lenses
    dsService,
    dsCluster,
    dsForce,

    -- * Destructuring the response
    DeleteServiceResponse (..),
    mkDeleteServiceResponse,

    -- ** Response lenses
    dsrrsService,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteService' smart constructor.
data DeleteService = DeleteService'
  { -- | The name of the service to delete.
    service :: Types.String,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to delete. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Core.Maybe Types.String,
    -- | If @true@ , allows you to delete a service even if it has not been scaled down to zero tasks. It is only necessary to use this if the service is using the @REPLICA@ scheduling strategy.
    force :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteService' value with any optional fields omitted.
mkDeleteService ::
  -- | 'service'
  Types.String ->
  DeleteService
mkDeleteService service =
  DeleteService'
    { service,
      cluster = Core.Nothing,
      force = Core.Nothing
    }

-- | The name of the service to delete.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsService :: Lens.Lens' DeleteService Types.String
dsService = Lens.field @"service"
{-# DEPRECATED dsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to delete. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCluster :: Lens.Lens' DeleteService (Core.Maybe Types.String)
dsCluster = Lens.field @"cluster"
{-# DEPRECATED dsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | If @true@ , allows you to delete a service even if it has not been scaled down to zero tasks. It is only necessary to use this if the service is using the @REPLICA@ scheduling strategy.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsForce :: Lens.Lens' DeleteService (Core.Maybe Core.Bool)
dsForce = Lens.field @"force"
{-# DEPRECATED dsForce "Use generic-lens or generic-optics with 'force' instead." #-}

instance Core.FromJSON DeleteService where
  toJSON DeleteService {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("service" Core..= service),
            ("cluster" Core..=) Core.<$> cluster,
            ("force" Core..=) Core.<$> force
          ]
      )

instance Core.AWSRequest DeleteService where
  type Rs DeleteService = DeleteServiceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.DeleteService"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteServiceResponse'
            Core.<$> (x Core..:? "service") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteServiceResponse' smart constructor.
data DeleteServiceResponse = DeleteServiceResponse'
  { -- | The full description of the deleted service.
    service :: Core.Maybe Types.ContainerService,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteServiceResponse' value with any optional fields omitted.
mkDeleteServiceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteServiceResponse
mkDeleteServiceResponse responseStatus =
  DeleteServiceResponse' {service = Core.Nothing, responseStatus}

-- | The full description of the deleted service.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsService :: Lens.Lens' DeleteServiceResponse (Core.Maybe Types.ContainerService)
dsrrsService = Lens.field @"service"
{-# DEPRECATED dsrrsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteServiceResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
