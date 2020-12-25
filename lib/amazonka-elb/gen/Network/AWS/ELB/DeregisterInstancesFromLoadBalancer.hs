{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified instances from the specified load balancer. After the instance is deregistered, it no longer receives traffic from the load balancer.
--
-- You can use 'DescribeLoadBalancers' to verify that the instance is deregistered from the load balancer.
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html Register or De-Register EC2 Instances> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
  ( -- * Creating a request
    DeregisterInstancesFromLoadBalancer (..),
    mkDeregisterInstancesFromLoadBalancer,

    -- ** Request lenses
    diflbLoadBalancerName,
    diflbInstances,

    -- * Destructuring the response
    DeregisterInstancesFromLoadBalancerResponse (..),
    mkDeregisterInstancesFromLoadBalancerResponse,

    -- ** Response lenses
    diflbrrsInstances,
    diflbrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeregisterInstancesFromLoadBalancer.
--
-- /See:/ 'mkDeregisterInstancesFromLoadBalancer' smart constructor.
data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName,
    -- | The IDs of the instances.
    instances :: [Types.Instance]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterInstancesFromLoadBalancer' value with any optional fields omitted.
mkDeregisterInstancesFromLoadBalancer ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  DeregisterInstancesFromLoadBalancer
mkDeregisterInstancesFromLoadBalancer loadBalancerName =
  DeregisterInstancesFromLoadBalancer'
    { loadBalancerName,
      instances = Core.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbLoadBalancerName :: Lens.Lens' DeregisterInstancesFromLoadBalancer Types.AccessPointName
diflbLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED diflbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbInstances :: Lens.Lens' DeregisterInstancesFromLoadBalancer [Types.Instance]
diflbInstances = Lens.field @"instances"
{-# DEPRECATED diflbInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

instance Core.AWSRequest DeregisterInstancesFromLoadBalancer where
  type
    Rs DeregisterInstancesFromLoadBalancer =
      DeregisterInstancesFromLoadBalancerResponse
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
            ( Core.pure ("Action", "DeregisterInstancesFromLoadBalancer")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> ( Core.toQueryValue
                            "Instances"
                            (Core.toQueryList "member" instances)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeregisterInstancesFromLoadBalancerResult"
      ( \s h x ->
          DeregisterInstancesFromLoadBalancerResponse'
            Core.<$> (x Core..@? "Instances" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of DeregisterInstancesFromLoadBalancer.
--
-- /See:/ 'mkDeregisterInstancesFromLoadBalancerResponse' smart constructor.
data DeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse'
  { -- | The remaining instances registered with the load balancer.
    instances :: Core.Maybe [Types.Instance],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterInstancesFromLoadBalancerResponse' value with any optional fields omitted.
mkDeregisterInstancesFromLoadBalancerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterInstancesFromLoadBalancerResponse
mkDeregisterInstancesFromLoadBalancerResponse responseStatus =
  DeregisterInstancesFromLoadBalancerResponse'
    { instances =
        Core.Nothing,
      responseStatus
    }

-- | The remaining instances registered with the load balancer.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbrrsInstances :: Lens.Lens' DeregisterInstancesFromLoadBalancerResponse (Core.Maybe [Types.Instance])
diflbrrsInstances = Lens.field @"instances"
{-# DEPRECATED diflbrrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbrrsResponseStatus :: Lens.Lens' DeregisterInstancesFromLoadBalancerResponse Core.Int
diflbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diflbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
