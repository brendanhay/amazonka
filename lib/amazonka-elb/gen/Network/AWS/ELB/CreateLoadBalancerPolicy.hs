{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateLoadBalancerPolicy (..)
    , mkCreateLoadBalancerPolicy
    -- ** Request lenses
    , clbpLoadBalancerName
    , clbpPolicyName
    , clbpPolicyTypeName
    , clbpPolicyAttributes

    -- * Destructuring the response
    , CreateLoadBalancerPolicyResponse (..)
    , mkCreateLoadBalancerPolicyResponse
    -- ** Response lenses
    , clbprrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateLoadBalancerPolicy.
--
-- /See:/ 'mkCreateLoadBalancerPolicy' smart constructor.
data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy'
  { loadBalancerName :: Types.AccessPointName
    -- ^ The name of the load balancer.
  , policyName :: Types.PolicyName
    -- ^ The name of the load balancer policy to be created. This name must be unique within the set of policies for this load balancer.
  , policyTypeName :: Types.PolicyTypeName
    -- ^ The name of the base policy type. To get the list of policy types, use 'DescribeLoadBalancerPolicyTypes' .
  , policyAttributes :: Core.Maybe [Types.PolicyAttribute]
    -- ^ The policy attributes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancerPolicy' value with any optional fields omitted.
mkCreateLoadBalancerPolicy
    :: Types.AccessPointName -- ^ 'loadBalancerName'
    -> Types.PolicyName -- ^ 'policyName'
    -> Types.PolicyTypeName -- ^ 'policyTypeName'
    -> CreateLoadBalancerPolicy
mkCreateLoadBalancerPolicy loadBalancerName policyName
  policyTypeName
  = CreateLoadBalancerPolicy'{loadBalancerName, policyName,
                              policyTypeName, policyAttributes = Core.Nothing}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpLoadBalancerName :: Lens.Lens' CreateLoadBalancerPolicy Types.AccessPointName
clbpLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE clbpLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The name of the load balancer policy to be created. This name must be unique within the set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpPolicyName :: Lens.Lens' CreateLoadBalancerPolicy Types.PolicyName
clbpPolicyName = Lens.field @"policyName"
{-# INLINEABLE clbpPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The name of the base policy type. To get the list of policy types, use 'DescribeLoadBalancerPolicyTypes' .
--
-- /Note:/ Consider using 'policyTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpPolicyTypeName :: Lens.Lens' CreateLoadBalancerPolicy Types.PolicyTypeName
clbpPolicyTypeName = Lens.field @"policyTypeName"
{-# INLINEABLE clbpPolicyTypeName #-}
{-# DEPRECATED policyTypeName "Use generic-lens or generic-optics with 'policyTypeName' instead"  #-}

-- | The policy attributes.
--
-- /Note:/ Consider using 'policyAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpPolicyAttributes :: Lens.Lens' CreateLoadBalancerPolicy (Core.Maybe [Types.PolicyAttribute])
clbpPolicyAttributes = Lens.field @"policyAttributes"
{-# INLINEABLE clbpPolicyAttributes #-}
{-# DEPRECATED policyAttributes "Use generic-lens or generic-optics with 'policyAttributes' instead"  #-}

instance Core.ToQuery CreateLoadBalancerPolicy where
        toQuery CreateLoadBalancerPolicy{..}
          = Core.toQueryPair "Action"
              ("CreateLoadBalancerPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<> Core.toQueryPair "PolicyName" policyName
              Core.<> Core.toQueryPair "PolicyTypeName" policyTypeName
              Core.<>
              Core.toQueryPair "PolicyAttributes"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   policyAttributes)

instance Core.ToHeaders CreateLoadBalancerPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateLoadBalancerPolicy where
        type Rs CreateLoadBalancerPolicy = CreateLoadBalancerPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateLoadBalancerPolicyResult"
              (\ s h x ->
                 CreateLoadBalancerPolicyResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CreateLoadBalancerPolicy.
--
-- /See:/ 'mkCreateLoadBalancerPolicyResponse' smart constructor.
newtype CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancerPolicyResponse' value with any optional fields omitted.
mkCreateLoadBalancerPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLoadBalancerPolicyResponse
mkCreateLoadBalancerPolicyResponse responseStatus
  = CreateLoadBalancerPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbprrsResponseStatus :: Lens.Lens' CreateLoadBalancerPolicyResponse Core.Int
clbprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clbprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
