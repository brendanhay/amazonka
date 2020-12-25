{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current health status (@Healthy@ , @Unhealthy@ , or @Unknown@ ) of one or more instances that are associated with a specified service.
module Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
  ( -- * Creating a request
    GetInstancesHealthStatus (..),
    mkGetInstancesHealthStatus,

    -- ** Request lenses
    gihsServiceId,
    gihsInstances,
    gihsMaxResults,
    gihsNextToken,

    -- * Destructuring the response
    GetInstancesHealthStatusResponse (..),
    mkGetInstancesHealthStatusResponse,

    -- ** Response lenses
    gihsrrsNextToken,
    gihsrrsStatus,
    gihsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkGetInstancesHealthStatus' smart constructor.
data GetInstancesHealthStatus = GetInstancesHealthStatus'
  { -- | The ID of the service that the instance is associated with.
    serviceId :: Types.ResourceId,
    -- | An array that contains the IDs of all the instances that you want to get the health status for.
    --
    -- If you omit @Instances@ , AWS Cloud Map returns the health status for all the instances that are associated with the specified service.
    instances :: Core.Maybe (Core.NonEmpty Types.ResourceId),
    -- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @GetInstancesHealthStatus@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
    maxResults :: Core.Maybe Core.Natural,
    -- | For the first @GetInstancesHealthStatus@ request, omit this value.
    --
    -- If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstancesHealthStatus' value with any optional fields omitted.
mkGetInstancesHealthStatus ::
  -- | 'serviceId'
  Types.ResourceId ->
  GetInstancesHealthStatus
mkGetInstancesHealthStatus serviceId =
  GetInstancesHealthStatus'
    { serviceId,
      instances = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the service that the instance is associated with.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsServiceId :: Lens.Lens' GetInstancesHealthStatus Types.ResourceId
gihsServiceId = Lens.field @"serviceId"
{-# DEPRECATED gihsServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | An array that contains the IDs of all the instances that you want to get the health status for.
--
-- If you omit @Instances@ , AWS Cloud Map returns the health status for all the instances that are associated with the specified service.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsInstances :: Lens.Lens' GetInstancesHealthStatus (Core.Maybe (Core.NonEmpty Types.ResourceId))
gihsInstances = Lens.field @"instances"
{-# DEPRECATED gihsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @GetInstancesHealthStatus@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsMaxResults :: Lens.Lens' GetInstancesHealthStatus (Core.Maybe Core.Natural)
gihsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gihsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | For the first @GetInstancesHealthStatus@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsNextToken :: Lens.Lens' GetInstancesHealthStatus (Core.Maybe Types.NextToken)
gihsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gihsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetInstancesHealthStatus where
  toJSON GetInstancesHealthStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ServiceId" Core..= serviceId),
            ("Instances" Core..=) Core.<$> instances,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetInstancesHealthStatus where
  type Rs GetInstancesHealthStatus = GetInstancesHealthStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Route53AutoNaming_v20170314.GetInstancesHealthStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstancesHealthStatusResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInstancesHealthStatusResponse' smart constructor.
data GetInstancesHealthStatusResponse = GetInstancesHealthStatusResponse'
  { -- | If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A complex type that contains the IDs and the health status of the instances that you specified in the @GetInstancesHealthStatus@ request.
    status :: Core.Maybe (Core.HashMap Types.ResourceId Types.HealthStatus),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstancesHealthStatusResponse' value with any optional fields omitted.
mkGetInstancesHealthStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInstancesHealthStatusResponse
mkGetInstancesHealthStatusResponse responseStatus =
  GetInstancesHealthStatusResponse'
    { nextToken = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsrrsNextToken :: Lens.Lens' GetInstancesHealthStatusResponse (Core.Maybe Types.NextToken)
gihsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gihsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A complex type that contains the IDs and the health status of the instances that you specified in the @GetInstancesHealthStatus@ request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsrrsStatus :: Lens.Lens' GetInstancesHealthStatusResponse (Core.Maybe (Core.HashMap Types.ResourceId Types.HealthStatus))
gihsrrsStatus = Lens.field @"status"
{-# DEPRECATED gihsrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gihsrrsResponseStatus :: Lens.Lens' GetInstancesHealthStatusResponse Core.Int
gihsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gihsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
