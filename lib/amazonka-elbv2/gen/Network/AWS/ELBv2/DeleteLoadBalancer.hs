{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DeleteLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Application Load Balancer, Network Load Balancer, or Gateway Load Balancer. Deleting a load balancer also deletes its listeners.
--
-- You can't delete a load balancer if deletion protection is enabled. If the load balancer does not exist or has already been deleted, the call succeeds.
-- Deleting a load balancer does not affect its registered targets. For example, your EC2 instances continue to run and are still registered to their target groups. If you no longer need these EC2 instances, you can stop or terminate them.
module Network.AWS.ELBv2.DeleteLoadBalancer
  ( -- * Creating a request
    DeleteLoadBalancer (..),
    mkDeleteLoadBalancer,

    -- ** Request lenses
    dlbLoadBalancerArn,

    -- * Destructuring the response
    DeleteLoadBalancerResponse (..),
    mkDeleteLoadBalancerResponse,

    -- ** Response lenses
    dlbrfrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLoadBalancer' smart constructor.
newtype DeleteLoadBalancer = DeleteLoadBalancer'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Types.LoadBalancerArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoadBalancer' value with any optional fields omitted.
mkDeleteLoadBalancer ::
  -- | 'loadBalancerArn'
  Types.LoadBalancerArn ->
  DeleteLoadBalancer
mkDeleteLoadBalancer loadBalancerArn =
  DeleteLoadBalancer' {loadBalancerArn}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerArn :: Lens.Lens' DeleteLoadBalancer Types.LoadBalancerArn
dlbLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# DEPRECATED dlbLoadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead." #-}

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
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "LoadBalancerArn" loadBalancerArn)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerResult"
      ( \s h x ->
          DeleteLoadBalancerResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteLoadBalancerResponse' smart constructor.
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
dlbrfrsResponseStatus :: Lens.Lens' DeleteLoadBalancerResponse Core.Int
dlbrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
