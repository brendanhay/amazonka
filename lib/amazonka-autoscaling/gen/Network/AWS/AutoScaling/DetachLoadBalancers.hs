{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches one or more Classic Load Balancers from the specified Auto Scaling group.
--
-- This operation detaches only Classic Load Balancers. If you have Application Load Balancers, Network Load Balancers, or Gateway Load Balancers, use the 'DetachLoadBalancerTargetGroups' API instead.
-- When you detach a load balancer, it enters the @Removing@ state while deregistering the instances in the group. When all instances are deregistered, then you can no longer describe the load balancer using the 'DescribeLoadBalancers' API call. The instances remain running.
module Network.AWS.AutoScaling.DetachLoadBalancers
  ( -- * Creating a request
    DetachLoadBalancers (..),
    mkDetachLoadBalancers,

    -- ** Request lenses
    dAutoScalingGroupName,
    dLoadBalancerNames,

    -- * Destructuring the response
    DetachLoadBalancersResponse (..),
    mkDetachLoadBalancersResponse,

    -- ** Response lenses
    dlbrfrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachLoadBalancers' smart constructor.
data DetachLoadBalancers = DetachLoadBalancers'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The names of the load balancers. You can specify up to 10 load balancers.
    loadBalancerNames :: [Types.XmlStringMaxLen255]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancers' value with any optional fields omitted.
mkDetachLoadBalancers ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  DetachLoadBalancers
mkDetachLoadBalancers autoScalingGroupName =
  DetachLoadBalancers'
    { autoScalingGroupName,
      loadBalancerNames = Core.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAutoScalingGroupName :: Lens.Lens' DetachLoadBalancers Types.ResourceName
dAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED dAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The names of the load balancers. You can specify up to 10 load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLoadBalancerNames :: Lens.Lens' DetachLoadBalancers [Types.XmlStringMaxLen255]
dLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# DEPRECATED dLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

instance Core.AWSRequest DetachLoadBalancers where
  type Rs DetachLoadBalancers = DetachLoadBalancersResponse
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
            ( Core.pure ("Action", "DetachLoadBalancers")
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
      "DetachLoadBalancersResult"
      ( \s h x ->
          DetachLoadBalancersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetachLoadBalancersResponse' smart constructor.
newtype DetachLoadBalancersResponse = DetachLoadBalancersResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancersResponse' value with any optional fields omitted.
mkDetachLoadBalancersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachLoadBalancersResponse
mkDetachLoadBalancersResponse responseStatus =
  DetachLoadBalancersResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrfrsResponseStatus :: Lens.Lens' DetachLoadBalancersResponse Core.Int
dlbrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
