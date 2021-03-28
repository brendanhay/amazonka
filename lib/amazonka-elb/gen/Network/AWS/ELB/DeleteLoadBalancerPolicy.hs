{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from the specified load balancer. This policy must not be enabled for any listeners.
module Network.AWS.ELB.DeleteLoadBalancerPolicy
    (
    -- * Creating a request
      DeleteLoadBalancerPolicy (..)
    , mkDeleteLoadBalancerPolicy
    -- ** Request lenses
    , dLoadBalancerName
    , dPolicyName

    -- * Destructuring the response
    , DeleteLoadBalancerPolicyResponse (..)
    , mkDeleteLoadBalancerPolicyResponse
    -- ** Response lenses
    , dlbprfrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteLoadBalancerPolicy.
--
-- /See:/ 'mkDeleteLoadBalancerPolicy' smart constructor.
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'
  { loadBalancerName :: Types.AccessPointName
    -- ^ The name of the load balancer.
  , policyName :: Types.PolicyName
    -- ^ The name of the policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoadBalancerPolicy' value with any optional fields omitted.
mkDeleteLoadBalancerPolicy
    :: Types.AccessPointName -- ^ 'loadBalancerName'
    -> Types.PolicyName -- ^ 'policyName'
    -> DeleteLoadBalancerPolicy
mkDeleteLoadBalancerPolicy loadBalancerName policyName
  = DeleteLoadBalancerPolicy'{loadBalancerName, policyName}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLoadBalancerName :: Lens.Lens' DeleteLoadBalancerPolicy Types.AccessPointName
dLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE dLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPolicyName :: Lens.Lens' DeleteLoadBalancerPolicy Types.PolicyName
dPolicyName = Lens.field @"policyName"
{-# INLINEABLE dPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery DeleteLoadBalancerPolicy where
        toQuery DeleteLoadBalancerPolicy{..}
          = Core.toQueryPair "Action"
              ("DeleteLoadBalancerPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<> Core.toQueryPair "PolicyName" policyName

instance Core.ToHeaders DeleteLoadBalancerPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteLoadBalancerPolicy where
        type Rs DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicyResponse
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
          = Response.receiveXMLWrapper "DeleteLoadBalancerPolicyResult"
              (\ s h x ->
                 DeleteLoadBalancerPolicyResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of DeleteLoadBalancerPolicy.
--
-- /See:/ 'mkDeleteLoadBalancerPolicyResponse' smart constructor.
newtype DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoadBalancerPolicyResponse' value with any optional fields omitted.
mkDeleteLoadBalancerPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLoadBalancerPolicyResponse
mkDeleteLoadBalancerPolicyResponse responseStatus
  = DeleteLoadBalancerPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbprfrsResponseStatus :: Lens.Lens' DeleteLoadBalancerPolicyResponse Core.Int
dlbprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlbprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
