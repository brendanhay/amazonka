{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified instances from a Lightsail load balancer.
--
-- This operation waits until the instances are no longer needed before they are detached from the load balancer.
-- The @detach instances from load balancer@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
  ( -- * Creating a request
    DetachInstancesFromLoadBalancer (..),
    mkDetachInstancesFromLoadBalancer,

    -- ** Request lenses
    diflbLoadBalancerName,
    diflbInstanceNames,

    -- * Destructuring the response
    DetachInstancesFromLoadBalancerResponse (..),
    mkDetachInstancesFromLoadBalancerResponse,

    -- ** Response lenses
    diflbrrsOperations,
    diflbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachInstancesFromLoadBalancer' smart constructor.
data DetachInstancesFromLoadBalancer = DetachInstancesFromLoadBalancer'
  { -- | The name of the Lightsail load balancer.
    loadBalancerName :: Types.ResourceName,
    -- | An array of strings containing the names of the instances you want to detach from the load balancer.
    instanceNames :: [Types.ResourceName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachInstancesFromLoadBalancer' value with any optional fields omitted.
mkDetachInstancesFromLoadBalancer ::
  -- | 'loadBalancerName'
  Types.ResourceName ->
  DetachInstancesFromLoadBalancer
mkDetachInstancesFromLoadBalancer loadBalancerName =
  DetachInstancesFromLoadBalancer'
    { loadBalancerName,
      instanceNames = Core.mempty
    }

-- | The name of the Lightsail load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbLoadBalancerName :: Lens.Lens' DetachInstancesFromLoadBalancer Types.ResourceName
diflbLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED diflbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | An array of strings containing the names of the instances you want to detach from the load balancer.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbInstanceNames :: Lens.Lens' DetachInstancesFromLoadBalancer [Types.ResourceName]
diflbInstanceNames = Lens.field @"instanceNames"
{-# DEPRECATED diflbInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

instance Core.FromJSON DetachInstancesFromLoadBalancer where
  toJSON DetachInstancesFromLoadBalancer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("loadBalancerName" Core..= loadBalancerName),
            Core.Just ("instanceNames" Core..= instanceNames)
          ]
      )

instance Core.AWSRequest DetachInstancesFromLoadBalancer where
  type
    Rs DetachInstancesFromLoadBalancer =
      DetachInstancesFromLoadBalancerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.DetachInstancesFromLoadBalancer"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachInstancesFromLoadBalancerResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDetachInstancesFromLoadBalancerResponse' smart constructor.
data DetachInstancesFromLoadBalancerResponse = DetachInstancesFromLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DetachInstancesFromLoadBalancerResponse' value with any optional fields omitted.
mkDetachInstancesFromLoadBalancerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachInstancesFromLoadBalancerResponse
mkDetachInstancesFromLoadBalancerResponse responseStatus =
  DetachInstancesFromLoadBalancerResponse'
    { operations =
        Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbrrsOperations :: Lens.Lens' DetachInstancesFromLoadBalancerResponse (Core.Maybe [Types.Operation])
diflbrrsOperations = Lens.field @"operations"
{-# DEPRECATED diflbrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diflbrrsResponseStatus :: Lens.Lens' DetachInstancesFromLoadBalancerResponse Core.Int
diflbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diflbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
