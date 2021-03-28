{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateAppCookieStickinessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a stickiness policy with sticky session lifetimes that follow that of an application-generated cookie. This policy can be associated only with HTTP/HTTPS listeners.
--
-- This policy is similar to the policy created by 'CreateLBCookieStickinessPolicy' , except that the lifetime of the special Elastic Load Balancing cookie, @AWSELB@ , follows the lifetime of the application-generated cookie specified in the policy configuration. The load balancer only inserts a new stickiness cookie when the application response includes a new application cookie.
-- If the application cookie is explicitly removed or expires, the session stops being sticky until a new application cookie is issued.
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-application Application-Controlled Session Stickiness> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.CreateAppCookieStickinessPolicy
    (
    -- * Creating a request
      CreateAppCookieStickinessPolicy (..)
    , mkCreateAppCookieStickinessPolicy
    -- ** Request lenses
    , cacspLoadBalancerName
    , cacspPolicyName
    , cacspCookieName

    -- * Destructuring the response
    , CreateAppCookieStickinessPolicyResponse (..)
    , mkCreateAppCookieStickinessPolicyResponse
    -- ** Response lenses
    , cacsprrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateAppCookieStickinessPolicy.
--
-- /See:/ 'mkCreateAppCookieStickinessPolicy' smart constructor.
data CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicy'
  { loadBalancerName :: Types.AccessPointName
    -- ^ The name of the load balancer.
  , policyName :: Types.PolicyName
    -- ^ The name of the policy being created. Policy names must consist of alphanumeric characters and dashes (-). This name must be unique within the set of policies for this load balancer.
  , cookieName :: Types.CookieName
    -- ^ The name of the application cookie used for stickiness.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAppCookieStickinessPolicy' value with any optional fields omitted.
mkCreateAppCookieStickinessPolicy
    :: Types.AccessPointName -- ^ 'loadBalancerName'
    -> Types.PolicyName -- ^ 'policyName'
    -> Types.CookieName -- ^ 'cookieName'
    -> CreateAppCookieStickinessPolicy
mkCreateAppCookieStickinessPolicy loadBalancerName policyName
  cookieName
  = CreateAppCookieStickinessPolicy'{loadBalancerName, policyName,
                                     cookieName}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacspLoadBalancerName :: Lens.Lens' CreateAppCookieStickinessPolicy Types.AccessPointName
cacspLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE cacspLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The name of the policy being created. Policy names must consist of alphanumeric characters and dashes (-). This name must be unique within the set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacspPolicyName :: Lens.Lens' CreateAppCookieStickinessPolicy Types.PolicyName
cacspPolicyName = Lens.field @"policyName"
{-# INLINEABLE cacspPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The name of the application cookie used for stickiness.
--
-- /Note:/ Consider using 'cookieName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacspCookieName :: Lens.Lens' CreateAppCookieStickinessPolicy Types.CookieName
cacspCookieName = Lens.field @"cookieName"
{-# INLINEABLE cacspCookieName #-}
{-# DEPRECATED cookieName "Use generic-lens or generic-optics with 'cookieName' instead"  #-}

instance Core.ToQuery CreateAppCookieStickinessPolicy where
        toQuery CreateAppCookieStickinessPolicy{..}
          = Core.toQueryPair "Action"
              ("CreateAppCookieStickinessPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<> Core.toQueryPair "PolicyName" policyName
              Core.<> Core.toQueryPair "CookieName" cookieName

instance Core.ToHeaders CreateAppCookieStickinessPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateAppCookieStickinessPolicy where
        type Rs CreateAppCookieStickinessPolicy =
             CreateAppCookieStickinessPolicyResponse
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
          = Response.receiveXMLWrapper
              "CreateAppCookieStickinessPolicyResult"
              (\ s h x ->
                 CreateAppCookieStickinessPolicyResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output for CreateAppCookieStickinessPolicy.
--
-- /See:/ 'mkCreateAppCookieStickinessPolicyResponse' smart constructor.
newtype CreateAppCookieStickinessPolicyResponse = CreateAppCookieStickinessPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAppCookieStickinessPolicyResponse' value with any optional fields omitted.
mkCreateAppCookieStickinessPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAppCookieStickinessPolicyResponse
mkCreateAppCookieStickinessPolicyResponse responseStatus
  = CreateAppCookieStickinessPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacsprrsResponseStatus :: Lens.Lens' CreateAppCookieStickinessPolicyResponse Core.Int
cacsprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cacsprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
