{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AttachInstancesToLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more Lightsail instances to a load balancer.
--
-- After some time, the instances are attached to the load balancer and the health check status is available.
-- The @attach instances to load balancer@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.AttachInstancesToLoadBalancer
  ( -- * Creating a request
    AttachInstancesToLoadBalancer (..),
    mkAttachInstancesToLoadBalancer,

    -- ** Request lenses
    aitlbLoadBalancerName,
    aitlbInstanceNames,

    -- * Destructuring the response
    AttachInstancesToLoadBalancerResponse (..),
    mkAttachInstancesToLoadBalancerResponse,

    -- ** Response lenses
    aitlbrrsOperations,
    aitlbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachInstancesToLoadBalancer' smart constructor.
data AttachInstancesToLoadBalancer = AttachInstancesToLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.ResourceName,
    -- | An array of strings representing the instance name(s) you want to attach to your load balancer.
    --
    -- An instance must be @running@ before you can attach it to your load balancer.
    -- There are no additional limits on the number of instances you can attach to your load balancer, aside from the limit of Lightsail instances you can create in your account (20).
    instanceNames :: [Types.ResourceName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachInstancesToLoadBalancer' value with any optional fields omitted.
mkAttachInstancesToLoadBalancer ::
  -- | 'loadBalancerName'
  Types.ResourceName ->
  AttachInstancesToLoadBalancer
mkAttachInstancesToLoadBalancer loadBalancerName =
  AttachInstancesToLoadBalancer'
    { loadBalancerName,
      instanceNames = Core.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aitlbLoadBalancerName :: Lens.Lens' AttachInstancesToLoadBalancer Types.ResourceName
aitlbLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED aitlbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | An array of strings representing the instance name(s) you want to attach to your load balancer.
--
-- An instance must be @running@ before you can attach it to your load balancer.
-- There are no additional limits on the number of instances you can attach to your load balancer, aside from the limit of Lightsail instances you can create in your account (20).
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aitlbInstanceNames :: Lens.Lens' AttachInstancesToLoadBalancer [Types.ResourceName]
aitlbInstanceNames = Lens.field @"instanceNames"
{-# DEPRECATED aitlbInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

instance Core.FromJSON AttachInstancesToLoadBalancer where
  toJSON AttachInstancesToLoadBalancer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("loadBalancerName" Core..= loadBalancerName),
            Core.Just ("instanceNames" Core..= instanceNames)
          ]
      )

instance Core.AWSRequest AttachInstancesToLoadBalancer where
  type
    Rs AttachInstancesToLoadBalancer =
      AttachInstancesToLoadBalancerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.AttachInstancesToLoadBalancer"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachInstancesToLoadBalancerResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAttachInstancesToLoadBalancerResponse' smart constructor.
data AttachInstancesToLoadBalancerResponse = AttachInstancesToLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AttachInstancesToLoadBalancerResponse' value with any optional fields omitted.
mkAttachInstancesToLoadBalancerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AttachInstancesToLoadBalancerResponse
mkAttachInstancesToLoadBalancerResponse responseStatus =
  AttachInstancesToLoadBalancerResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aitlbrrsOperations :: Lens.Lens' AttachInstancesToLoadBalancerResponse (Core.Maybe [Types.Operation])
aitlbrrsOperations = Lens.field @"operations"
{-# DEPRECATED aitlbrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aitlbrrsResponseStatus :: Lens.Lens' AttachInstancesToLoadBalancerResponse Core.Int
aitlbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aitlbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
