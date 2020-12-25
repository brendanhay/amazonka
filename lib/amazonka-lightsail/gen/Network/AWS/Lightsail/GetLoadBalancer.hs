{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Lightsail load balancer.
module Network.AWS.Lightsail.GetLoadBalancer
  ( -- * Creating a request
    GetLoadBalancer (..),
    mkGetLoadBalancer,

    -- ** Request lenses
    glbLoadBalancerName,

    -- * Destructuring the response
    GetLoadBalancerResponse (..),
    mkGetLoadBalancerResponse,

    -- ** Response lenses
    glbrrsLoadBalancer,
    glbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLoadBalancer' smart constructor.
newtype GetLoadBalancer = GetLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoadBalancer' value with any optional fields omitted.
mkGetLoadBalancer ::
  -- | 'loadBalancerName'
  Types.ResourceName ->
  GetLoadBalancer
mkGetLoadBalancer loadBalancerName =
  GetLoadBalancer' {loadBalancerName}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbLoadBalancerName :: Lens.Lens' GetLoadBalancer Types.ResourceName
glbLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED glbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Core.FromJSON GetLoadBalancer where
  toJSON GetLoadBalancer {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("loadBalancerName" Core..= loadBalancerName)]
      )

instance Core.AWSRequest GetLoadBalancer where
  type Rs GetLoadBalancer = GetLoadBalancerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetLoadBalancer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoadBalancerResponse'
            Core.<$> (x Core..:? "loadBalancer") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetLoadBalancerResponse' smart constructor.
data GetLoadBalancerResponse = GetLoadBalancerResponse'
  { -- | An object containing information about your load balancer.
    loadBalancer :: Core.Maybe Types.LoadBalancer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetLoadBalancerResponse' value with any optional fields omitted.
mkGetLoadBalancerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLoadBalancerResponse
mkGetLoadBalancerResponse responseStatus =
  GetLoadBalancerResponse'
    { loadBalancer = Core.Nothing,
      responseStatus
    }

-- | An object containing information about your load balancer.
--
-- /Note:/ Consider using 'loadBalancer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbrrsLoadBalancer :: Lens.Lens' GetLoadBalancerResponse (Core.Maybe Types.LoadBalancer)
glbrrsLoadBalancer = Lens.field @"loadBalancer"
{-# DEPRECATED glbrrsLoadBalancer "Use generic-lens or generic-optics with 'loadBalancer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbrrsResponseStatus :: Lens.Lens' GetLoadBalancerResponse Core.Int
glbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED glbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
