{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the current set of policies for the specified load balancer port with the specified set of policies.
--
-- To enable back-end server authentication, use 'SetLoadBalancerPoliciesForBackendServer' .
-- For more information about setting policies, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/ssl-config-update.html Update the SSL Negotiation Configuration> , <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration Duration-Based Session Stickiness> , and <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-application Application-Controlled Session Stickiness> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
  ( -- * Creating a request
    SetLoadBalancerPoliciesOfListener (..),
    mkSetLoadBalancerPoliciesOfListener,

    -- ** Request lenses
    slbpolLoadBalancerName,
    slbpolLoadBalancerPort,
    slbpolPolicyNames,

    -- * Destructuring the response
    SetLoadBalancerPoliciesOfListenerResponse (..),
    mkSetLoadBalancerPoliciesOfListenerResponse,

    -- ** Response lenses
    slbpolrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetLoadBalancePoliciesOfListener.
--
-- /See:/ 'mkSetLoadBalancerPoliciesOfListener' smart constructor.
data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName,
    -- | The external port of the load balancer.
    loadBalancerPort :: Core.Int,
    -- | The names of the policies. This list must include all policies to be enabled. If you omit a policy that is currently enabled, it is disabled. If the list is empty, all current policies are disabled.
    policyNames :: [Types.PolicyName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBalancerPoliciesOfListener' value with any optional fields omitted.
mkSetLoadBalancerPoliciesOfListener ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  -- | 'loadBalancerPort'
  Core.Int ->
  SetLoadBalancerPoliciesOfListener
mkSetLoadBalancerPoliciesOfListener
  loadBalancerName
  loadBalancerPort =
    SetLoadBalancerPoliciesOfListener'
      { loadBalancerName,
        loadBalancerPort,
        policyNames = Core.mempty
      }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpolLoadBalancerName :: Lens.Lens' SetLoadBalancerPoliciesOfListener Types.AccessPointName
slbpolLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED slbpolLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The external port of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpolLoadBalancerPort :: Lens.Lens' SetLoadBalancerPoliciesOfListener Core.Int
slbpolLoadBalancerPort = Lens.field @"loadBalancerPort"
{-# DEPRECATED slbpolLoadBalancerPort "Use generic-lens or generic-optics with 'loadBalancerPort' instead." #-}

-- | The names of the policies. This list must include all policies to be enabled. If you omit a policy that is currently enabled, it is disabled. If the list is empty, all current policies are disabled.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpolPolicyNames :: Lens.Lens' SetLoadBalancerPoliciesOfListener [Types.PolicyName]
slbpolPolicyNames = Lens.field @"policyNames"
{-# DEPRECATED slbpolPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

instance Core.AWSRequest SetLoadBalancerPoliciesOfListener where
  type
    Rs SetLoadBalancerPoliciesOfListener =
      SetLoadBalancerPoliciesOfListenerResponse
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
            ( Core.pure ("Action", "SetLoadBalancerPoliciesOfListener")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> (Core.toQueryValue "LoadBalancerPort" loadBalancerPort)
                Core.<> ( Core.toQueryValue
                            "PolicyNames"
                            (Core.toQueryList "member" policyNames)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetLoadBalancerPoliciesOfListenerResult"
      ( \s h x ->
          SetLoadBalancerPoliciesOfListenerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of SetLoadBalancePoliciesOfListener.
--
-- /See:/ 'mkSetLoadBalancerPoliciesOfListenerResponse' smart constructor.
newtype SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBalancerPoliciesOfListenerResponse' value with any optional fields omitted.
mkSetLoadBalancerPoliciesOfListenerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetLoadBalancerPoliciesOfListenerResponse
mkSetLoadBalancerPoliciesOfListenerResponse responseStatus =
  SetLoadBalancerPoliciesOfListenerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpolrrsResponseStatus :: Lens.Lens' SetLoadBalancerPoliciesOfListenerResponse Core.Int
slbpolrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED slbpolrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
