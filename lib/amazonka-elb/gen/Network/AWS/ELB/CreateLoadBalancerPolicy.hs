{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateLoadBalancerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy with the specified attributes for the specified load balancer.
--
-- Policies are settings that are saved for your load balancer and that can be applied to the listener or the application server, depending on the policy type.
module Network.AWS.ELB.CreateLoadBalancerPolicy
  ( -- * Creating a request
    CreateLoadBalancerPolicy (..),
    mkCreateLoadBalancerPolicy,

    -- ** Request lenses
    clbpLoadBalancerName,
    clbpPolicyName,
    clbpPolicyTypeName,
    clbpPolicyAttributes,

    -- * Destructuring the response
    CreateLoadBalancerPolicyResponse (..),
    mkCreateLoadBalancerPolicyResponse,

    -- ** Response lenses
    clbprrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateLoadBalancerPolicy.
--
-- /See:/ 'mkCreateLoadBalancerPolicy' smart constructor.
data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName,
    -- | The name of the load balancer policy to be created. This name must be unique within the set of policies for this load balancer.
    policyName :: Types.PolicyName,
    -- | The name of the base policy type. To get the list of policy types, use 'DescribeLoadBalancerPolicyTypes' .
    policyTypeName :: Types.PolicyTypeName,
    -- | The policy attributes.
    policyAttributes :: Core.Maybe [Types.PolicyAttribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancerPolicy' value with any optional fields omitted.
mkCreateLoadBalancerPolicy ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policyTypeName'
  Types.PolicyTypeName ->
  CreateLoadBalancerPolicy
mkCreateLoadBalancerPolicy
  loadBalancerName
  policyName
  policyTypeName =
    CreateLoadBalancerPolicy'
      { loadBalancerName,
        policyName,
        policyTypeName,
        policyAttributes = Core.Nothing
      }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpLoadBalancerName :: Lens.Lens' CreateLoadBalancerPolicy Types.AccessPointName
clbpLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED clbpLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The name of the load balancer policy to be created. This name must be unique within the set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpPolicyName :: Lens.Lens' CreateLoadBalancerPolicy Types.PolicyName
clbpPolicyName = Lens.field @"policyName"
{-# DEPRECATED clbpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name of the base policy type. To get the list of policy types, use 'DescribeLoadBalancerPolicyTypes' .
--
-- /Note:/ Consider using 'policyTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpPolicyTypeName :: Lens.Lens' CreateLoadBalancerPolicy Types.PolicyTypeName
clbpPolicyTypeName = Lens.field @"policyTypeName"
{-# DEPRECATED clbpPolicyTypeName "Use generic-lens or generic-optics with 'policyTypeName' instead." #-}

-- | The policy attributes.
--
-- /Note:/ Consider using 'policyAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpPolicyAttributes :: Lens.Lens' CreateLoadBalancerPolicy (Core.Maybe [Types.PolicyAttribute])
clbpPolicyAttributes = Lens.field @"policyAttributes"
{-# DEPRECATED clbpPolicyAttributes "Use generic-lens or generic-optics with 'policyAttributes' instead." #-}

instance Core.AWSRequest CreateLoadBalancerPolicy where
  type Rs CreateLoadBalancerPolicy = CreateLoadBalancerPolicyResponse
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
            ( Core.pure ("Action", "CreateLoadBalancerPolicy")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> (Core.toQueryValue "PolicyName" policyName)
                Core.<> (Core.toQueryValue "PolicyTypeName" policyTypeName)
                Core.<> ( Core.toQueryValue
                            "PolicyAttributes"
                            (Core.toQueryList "member" Core.<$> policyAttributes)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateLoadBalancerPolicyResult"
      ( \s h x ->
          CreateLoadBalancerPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of CreateLoadBalancerPolicy.
--
-- /See:/ 'mkCreateLoadBalancerPolicyResponse' smart constructor.
newtype CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancerPolicyResponse' value with any optional fields omitted.
mkCreateLoadBalancerPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateLoadBalancerPolicyResponse
mkCreateLoadBalancerPolicyResponse responseStatus =
  CreateLoadBalancerPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbprrsResponseStatus :: Lens.Lens' CreateLoadBalancerPolicyResponse Core.Int
clbprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED clbprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
