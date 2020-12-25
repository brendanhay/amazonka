{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeInstanceHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state of the specified instances with respect to the specified load balancer. If no instances are specified, the call describes the state of all instances that are currently registered with the load balancer. If instances are specified, their state is returned even if they are no longer registered with the load balancer. The state of terminated instances is not returned.
module Network.AWS.ELB.DescribeInstanceHealth
  ( -- * Creating a request
    DescribeInstanceHealth (..),
    mkDescribeInstanceHealth,

    -- ** Request lenses
    dihLoadBalancerName,
    dihInstances,

    -- * Destructuring the response
    DescribeInstanceHealthResponse (..),
    mkDescribeInstanceHealthResponse,

    -- ** Response lenses
    dihrrsInstanceStates,
    dihrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeInstanceHealth.
--
-- /See:/ 'mkDescribeInstanceHealth' smart constructor.
data DescribeInstanceHealth = DescribeInstanceHealth'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName,
    -- | The IDs of the instances.
    instances :: Core.Maybe [Types.Instance]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceHealth' value with any optional fields omitted.
mkDescribeInstanceHealth ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  DescribeInstanceHealth
mkDescribeInstanceHealth loadBalancerName =
  DescribeInstanceHealth'
    { loadBalancerName,
      instances = Core.Nothing
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihLoadBalancerName :: Lens.Lens' DescribeInstanceHealth Types.AccessPointName
dihLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED dihLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihInstances :: Lens.Lens' DescribeInstanceHealth (Core.Maybe [Types.Instance])
dihInstances = Lens.field @"instances"
{-# DEPRECATED dihInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

instance Core.AWSRequest DescribeInstanceHealth where
  type Rs DescribeInstanceHealth = DescribeInstanceHealthResponse
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
            ( Core.pure ("Action", "DescribeInstanceHealth")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> ( Core.toQueryValue
                            "Instances"
                            (Core.toQueryList "member" Core.<$> instances)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeInstanceHealthResult"
      ( \s h x ->
          DescribeInstanceHealthResponse'
            Core.<$> (x Core..@? "InstanceStates" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output for DescribeInstanceHealth.
--
-- /See:/ 'mkDescribeInstanceHealthResponse' smart constructor.
data DescribeInstanceHealthResponse = DescribeInstanceHealthResponse'
  { -- | Information about the health of the instances.
    instanceStates :: Core.Maybe [Types.InstanceState],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceHealthResponse' value with any optional fields omitted.
mkDescribeInstanceHealthResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceHealthResponse
mkDescribeInstanceHealthResponse responseStatus =
  DescribeInstanceHealthResponse'
    { instanceStates = Core.Nothing,
      responseStatus
    }

-- | Information about the health of the instances.
--
-- /Note:/ Consider using 'instanceStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrrsInstanceStates :: Lens.Lens' DescribeInstanceHealthResponse (Core.Maybe [Types.InstanceState])
dihrrsInstanceStates = Lens.field @"instanceStates"
{-# DEPRECATED dihrrsInstanceStates "Use generic-lens or generic-optics with 'instanceStates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrrsResponseStatus :: Lens.Lens' DescribeInstanceHealthResponse Core.Int
dihrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dihrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
