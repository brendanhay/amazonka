{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified load balancer.
--
-- If you are attempting to recreate a load balancer, you must reconfigure all settings. The DNS name associated with a deleted load balancer are no longer usable. The name and associated DNS record of the deleted load balancer no longer exist and traffic sent to any of its IP addresses is no longer delivered to your instances.
-- If the load balancer does not exist or has already been deleted, the call to @DeleteLoadBalancer@ still succeeds.
module Network.AWS.ELB.DeleteLoadBalancer
  ( -- * Creating a request
    DeleteLoadBalancer (..),
    mkDeleteLoadBalancer,

    -- ** Request lenses
    dlbLoadBalancerName,

    -- * Destructuring the response
    DeleteLoadBalancerResponse (..),
    mkDeleteLoadBalancerResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteLoadBalancer.
--
-- /See:/ 'mkDeleteLoadBalancer' smart constructor.
newtype DeleteLoadBalancer = DeleteLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoadBalancer' value with any optional fields omitted.
mkDeleteLoadBalancer ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  DeleteLoadBalancer
mkDeleteLoadBalancer loadBalancerName =
  DeleteLoadBalancer' {loadBalancerName}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerName :: Lens.Lens' DeleteLoadBalancer Types.AccessPointName
dlbLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED dlbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Core.AWSRequest DeleteLoadBalancer where
  type Rs DeleteLoadBalancer = DeleteLoadBalancerResponse
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
            ( Core.pure ("Action", "DeleteLoadBalancer")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerResult"
      ( \s h x ->
          DeleteLoadBalancerResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of DeleteLoadBalancer.
--
-- /See:/ 'mkDeleteLoadBalancerResponse' smart constructor.
newtype DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoadBalancerResponse' value with any optional fields omitted.
mkDeleteLoadBalancerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLoadBalancerResponse
mkDeleteLoadBalancerResponse responseStatus =
  DeleteLoadBalancerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteLoadBalancerResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
