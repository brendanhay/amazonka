{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHealthCheckStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status of a specified health check.
module Network.AWS.Route53.GetHealthCheckStatus
  ( -- * Creating a request
    GetHealthCheckStatus (..),
    mkGetHealthCheckStatus,

    -- ** Request lenses
    ghcsHealthCheckId,

    -- * Destructuring the response
    GetHealthCheckStatusResponse (..),
    mkGetHealthCheckStatusResponse,

    -- ** Response lenses
    ghcsrrsHealthCheckObservations,
    ghcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to get the status for a health check.
--
-- /See:/ 'mkGetHealthCheckStatus' smart constructor.
newtype GetHealthCheckStatus = GetHealthCheckStatus'
  { -- | The ID for the health check that you want the current status for. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
    healthCheckId :: Types.HealthCheckId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetHealthCheckStatus' value with any optional fields omitted.
mkGetHealthCheckStatus ::
  -- | 'healthCheckId'
  Types.HealthCheckId ->
  GetHealthCheckStatus
mkGetHealthCheckStatus healthCheckId =
  GetHealthCheckStatus' {healthCheckId}

-- | The ID for the health check that you want the current status for. When you created the health check, @CreateHealthCheck@ returned the ID in the response, in the @HealthCheckId@ element.
--
-- /Note:/ Consider using 'healthCheckId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcsHealthCheckId :: Lens.Lens' GetHealthCheckStatus Types.HealthCheckId
ghcsHealthCheckId = Lens.field @"healthCheckId"
{-# DEPRECATED ghcsHealthCheckId "Use generic-lens or generic-optics with 'healthCheckId' instead." #-}

instance Core.AWSRequest GetHealthCheckStatus where
  type Rs GetHealthCheckStatus = GetHealthCheckStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2013-04-01/healthcheck/" Core.<> (Core.toText healthCheckId)
                Core.<> ("/status")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetHealthCheckStatusResponse'
            Core.<$> ( x Core..@? "HealthCheckObservations" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "HealthCheckObservation"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response to a @GetHealthCheck@ request.
--
-- /See:/ 'mkGetHealthCheckStatusResponse' smart constructor.
data GetHealthCheckStatusResponse = GetHealthCheckStatusResponse'
  { -- | A list that contains one @HealthCheckObservation@ element for each Amazon Route 53 health checker that is reporting a status about the health check endpoint.
    healthCheckObservations :: [Types.HealthCheckObservation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetHealthCheckStatusResponse' value with any optional fields omitted.
mkGetHealthCheckStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetHealthCheckStatusResponse
mkGetHealthCheckStatusResponse responseStatus =
  GetHealthCheckStatusResponse'
    { healthCheckObservations =
        Core.mempty,
      responseStatus
    }

-- | A list that contains one @HealthCheckObservation@ element for each Amazon Route 53 health checker that is reporting a status about the health check endpoint.
--
-- /Note:/ Consider using 'healthCheckObservations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcsrrsHealthCheckObservations :: Lens.Lens' GetHealthCheckStatusResponse [Types.HealthCheckObservation]
ghcsrrsHealthCheckObservations = Lens.field @"healthCheckObservations"
{-# DEPRECATED ghcsrrsHealthCheckObservations "Use generic-lens or generic-optics with 'healthCheckObservations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghcsrrsResponseStatus :: Lens.Lens' GetHealthCheckStatusResponse Core.Int
ghcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ghcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
