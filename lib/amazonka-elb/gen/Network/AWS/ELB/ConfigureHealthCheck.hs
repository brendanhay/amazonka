{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.ConfigureHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies the health check settings to use when evaluating the health state of your EC2 instances.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-healthchecks.html Configure Health Checks for Your Load Balancer> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.ConfigureHealthCheck
  ( -- * Creating a request
    ConfigureHealthCheck (..),
    mkConfigureHealthCheck,

    -- ** Request lenses
    chcLoadBalancerName,
    chcHealthCheck,

    -- * Destructuring the response
    ConfigureHealthCheckResponse (..),
    mkConfigureHealthCheckResponse,

    -- ** Response lenses
    chcrrsHealthCheck,
    chcrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ConfigureHealthCheck.
--
-- /See:/ 'mkConfigureHealthCheck' smart constructor.
data ConfigureHealthCheck = ConfigureHealthCheck'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.LoadBalancerName,
    -- | The configuration information.
    healthCheck :: Types.HealthCheck
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigureHealthCheck' value with any optional fields omitted.
mkConfigureHealthCheck ::
  -- | 'loadBalancerName'
  Types.LoadBalancerName ->
  -- | 'healthCheck'
  Types.HealthCheck ->
  ConfigureHealthCheck
mkConfigureHealthCheck loadBalancerName healthCheck =
  ConfigureHealthCheck' {loadBalancerName, healthCheck}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcLoadBalancerName :: Lens.Lens' ConfigureHealthCheck Types.LoadBalancerName
chcLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED chcLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The configuration information.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHealthCheck :: Lens.Lens' ConfigureHealthCheck Types.HealthCheck
chcHealthCheck = Lens.field @"healthCheck"
{-# DEPRECATED chcHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

instance Core.AWSRequest ConfigureHealthCheck where
  type Rs ConfigureHealthCheck = ConfigureHealthCheckResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ConfigureHealthCheck")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> (Core.toQueryValue "HealthCheck" healthCheck)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ConfigureHealthCheckResult"
      ( \s h x ->
          ConfigureHealthCheckResponse'
            Core.<$> (x Core..@? "HealthCheck") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of ConfigureHealthCheck.
--
-- /See:/ 'mkConfigureHealthCheckResponse' smart constructor.
data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse'
  { -- | The updated health check.
    healthCheck :: Core.Maybe Types.HealthCheck,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigureHealthCheckResponse' value with any optional fields omitted.
mkConfigureHealthCheckResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ConfigureHealthCheckResponse
mkConfigureHealthCheckResponse responseStatus =
  ConfigureHealthCheckResponse'
    { healthCheck = Core.Nothing,
      responseStatus
    }

-- | The updated health check.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrrsHealthCheck :: Lens.Lens' ConfigureHealthCheckResponse (Core.Maybe Types.HealthCheck)
chcrrsHealthCheck = Lens.field @"healthCheck"
{-# DEPRECATED chcrrsHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrrsResponseStatus :: Lens.Lens' ConfigureHealthCheckResponse Core.Int
chcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED chcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
