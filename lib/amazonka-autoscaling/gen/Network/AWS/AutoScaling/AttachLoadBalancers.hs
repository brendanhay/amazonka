{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.AttachLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more Classic Load Balancers to the specified Auto Scaling group. Amazon EC2 Auto Scaling registers the running instances with these Classic Load Balancers.
--
-- To describe the load balancers for an Auto Scaling group, call the 'DescribeLoadBalancers' API. To detach the load balancer from the Auto Scaling group, call the 'DetachLoadBalancers' API.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.AttachLoadBalancers
  ( -- * Creating a request
    AttachLoadBalancers (..),
    mkAttachLoadBalancers,

    -- ** Request lenses
    albAutoScalingGroupName,
    albLoadBalancerNames,

    -- * Destructuring the response
    AttachLoadBalancersResponse (..),
    mkAttachLoadBalancersResponse,

    -- ** Response lenses
    albrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachLoadBalancers' smart constructor.
data AttachLoadBalancers = AttachLoadBalancers'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.AutoScalingGroupName,
    -- | The names of the load balancers. You can specify up to 10 load balancers.
    loadBalancerNames :: [Types.XmlStringMaxLen255]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancers' value with any optional fields omitted.
mkAttachLoadBalancers ::
  -- | 'autoScalingGroupName'
  Types.AutoScalingGroupName ->
  AttachLoadBalancers
mkAttachLoadBalancers autoScalingGroupName =
  AttachLoadBalancers'
    { autoScalingGroupName,
      loadBalancerNames = Core.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albAutoScalingGroupName :: Lens.Lens' AttachLoadBalancers Types.AutoScalingGroupName
albAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED albAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The names of the load balancers. You can specify up to 10 load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albLoadBalancerNames :: Lens.Lens' AttachLoadBalancers [Types.XmlStringMaxLen255]
albLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# DEPRECATED albLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

instance Core.AWSRequest AttachLoadBalancers where
  type Rs AttachLoadBalancers = AttachLoadBalancersResponse
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
            ( Core.pure ("Action", "AttachLoadBalancers")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "LoadBalancerNames"
                            (Core.toQueryList "member" loadBalancerNames)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "AttachLoadBalancersResult"
      ( \s h x ->
          AttachLoadBalancersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAttachLoadBalancersResponse' smart constructor.
newtype AttachLoadBalancersResponse = AttachLoadBalancersResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancersResponse' value with any optional fields omitted.
mkAttachLoadBalancersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AttachLoadBalancersResponse
mkAttachLoadBalancersResponse responseStatus =
  AttachLoadBalancersResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albrrsResponseStatus :: Lens.Lens' AttachLoadBalancersResponse Core.Int
albrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED albrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
