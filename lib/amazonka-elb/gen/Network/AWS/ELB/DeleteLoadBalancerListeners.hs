{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancerListeners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified listeners from the specified load balancer.
module Network.AWS.ELB.DeleteLoadBalancerListeners
  ( -- * Creating a request
    DeleteLoadBalancerListeners (..),
    mkDeleteLoadBalancerListeners,

    -- ** Request lenses
    dlblLoadBalancerName,
    dlblLoadBalancerPorts,

    -- * Destructuring the response
    DeleteLoadBalancerListenersResponse (..),
    mkDeleteLoadBalancerListenersResponse,

    -- ** Response lenses
    dlblrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteLoadBalancerListeners.
--
-- /See:/ 'mkDeleteLoadBalancerListeners' smart constructor.
data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName,
    -- | The client port numbers of the listeners.
    loadBalancerPorts :: [Core.Int]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoadBalancerListeners' value with any optional fields omitted.
mkDeleteLoadBalancerListeners ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  DeleteLoadBalancerListeners
mkDeleteLoadBalancerListeners loadBalancerName =
  DeleteLoadBalancerListeners'
    { loadBalancerName,
      loadBalancerPorts = Core.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlblLoadBalancerName :: Lens.Lens' DeleteLoadBalancerListeners Types.AccessPointName
dlblLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED dlblLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The client port numbers of the listeners.
--
-- /Note:/ Consider using 'loadBalancerPorts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlblLoadBalancerPorts :: Lens.Lens' DeleteLoadBalancerListeners [Core.Int]
dlblLoadBalancerPorts = Lens.field @"loadBalancerPorts"
{-# DEPRECATED dlblLoadBalancerPorts "Use generic-lens or generic-optics with 'loadBalancerPorts' instead." #-}

instance Core.AWSRequest DeleteLoadBalancerListeners where
  type
    Rs DeleteLoadBalancerListeners =
      DeleteLoadBalancerListenersResponse
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
            ( Core.pure ("Action", "DeleteLoadBalancerListeners")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> ( Core.toQueryValue
                            "LoadBalancerPorts"
                            (Core.toQueryList "member" loadBalancerPorts)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerListenersResult"
      ( \s h x ->
          DeleteLoadBalancerListenersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of DeleteLoadBalancerListeners.
--
-- /See:/ 'mkDeleteLoadBalancerListenersResponse' smart constructor.
newtype DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoadBalancerListenersResponse' value with any optional fields omitted.
mkDeleteLoadBalancerListenersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLoadBalancerListenersResponse
mkDeleteLoadBalancerListenersResponse responseStatus =
  DeleteLoadBalancerListenersResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlblrrsResponseStatus :: Lens.Lens' DeleteLoadBalancerListenersResponse Core.Int
dlblrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlblrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
