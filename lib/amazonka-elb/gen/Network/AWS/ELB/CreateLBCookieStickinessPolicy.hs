{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateLBCookieStickinessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a stickiness policy with sticky session lifetimes controlled by the lifetime of the browser (user-agent) or a specified expiration period. This policy can be associated only with HTTP/HTTPS listeners.
--
-- When a load balancer implements this policy, the load balancer uses a special cookie to track the instance for each request. When the load balancer receives a request, it first checks to see if this cookie is present in the request. If so, the load balancer sends the request to the application server specified in the cookie. If not, the load balancer sends the request to a server that is chosen based on the existing load-balancing algorithm.
-- A cookie is inserted into the response for binding subsequent requests from the same user to that server. The validity of the cookie is based on the cookie expiration time, which is specified in the policy configuration.
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration Duration-Based Session Stickiness> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.CreateLBCookieStickinessPolicy
  ( -- * Creating a request
    CreateLBCookieStickinessPolicy (..),
    mkCreateLBCookieStickinessPolicy,

    -- ** Request lenses
    clbcspCookieExpirationPeriod,
    clbcspLoadBalancerName,
    clbcspPolicyName,

    -- * Destructuring the response
    CreateLBCookieStickinessPolicyResponse (..),
    mkCreateLBCookieStickinessPolicyResponse,

    -- ** Response lenses
    clbcsprsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateLBCookieStickinessPolicy.
--
-- /See:/ 'mkCreateLBCookieStickinessPolicy' smart constructor.
data CreateLBCookieStickinessPolicy = CreateLBCookieStickinessPolicy'
  { cookieExpirationPeriod ::
      Lude.Maybe Lude.Integer,
    loadBalancerName :: Lude.Text,
    policyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLBCookieStickinessPolicy' with the minimum fields required to make a request.
--
-- * 'cookieExpirationPeriod' - The time period, in seconds, after which the cookie should be considered stale. If you do not specify this parameter, the default value is 0, which indicates that the sticky session should last for the duration of the browser session.
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'policyName' - The name of the policy being created. Policy names must consist of alphanumeric characters and dashes (-). This name must be unique within the set of policies for this load balancer.
mkCreateLBCookieStickinessPolicy ::
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  CreateLBCookieStickinessPolicy
mkCreateLBCookieStickinessPolicy pLoadBalancerName_ pPolicyName_ =
  CreateLBCookieStickinessPolicy'
    { cookieExpirationPeriod =
        Lude.Nothing,
      loadBalancerName = pLoadBalancerName_,
      policyName = pPolicyName_
    }

-- | The time period, in seconds, after which the cookie should be considered stale. If you do not specify this parameter, the default value is 0, which indicates that the sticky session should last for the duration of the browser session.
--
-- /Note:/ Consider using 'cookieExpirationPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbcspCookieExpirationPeriod :: Lens.Lens' CreateLBCookieStickinessPolicy (Lude.Maybe Lude.Integer)
clbcspCookieExpirationPeriod = Lens.lens (cookieExpirationPeriod :: CreateLBCookieStickinessPolicy -> Lude.Maybe Lude.Integer) (\s a -> s {cookieExpirationPeriod = a} :: CreateLBCookieStickinessPolicy)
{-# DEPRECATED clbcspCookieExpirationPeriod "Use generic-lens or generic-optics with 'cookieExpirationPeriod' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbcspLoadBalancerName :: Lens.Lens' CreateLBCookieStickinessPolicy Lude.Text
clbcspLoadBalancerName = Lens.lens (loadBalancerName :: CreateLBCookieStickinessPolicy -> Lude.Text) (\s a -> s {loadBalancerName = a} :: CreateLBCookieStickinessPolicy)
{-# DEPRECATED clbcspLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The name of the policy being created. Policy names must consist of alphanumeric characters and dashes (-). This name must be unique within the set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbcspPolicyName :: Lens.Lens' CreateLBCookieStickinessPolicy Lude.Text
clbcspPolicyName = Lens.lens (policyName :: CreateLBCookieStickinessPolicy -> Lude.Text) (\s a -> s {policyName = a} :: CreateLBCookieStickinessPolicy)
{-# DEPRECATED clbcspPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest CreateLBCookieStickinessPolicy where
  type
    Rs CreateLBCookieStickinessPolicy =
      CreateLBCookieStickinessPolicyResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "CreateLBCookieStickinessPolicyResult"
      ( \s h x ->
          CreateLBCookieStickinessPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLBCookieStickinessPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLBCookieStickinessPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLBCookieStickinessPolicy where
  toQuery CreateLBCookieStickinessPolicy' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateLBCookieStickinessPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "CookieExpirationPeriod" Lude.=: cookieExpirationPeriod,
        "LoadBalancerName" Lude.=: loadBalancerName,
        "PolicyName" Lude.=: policyName
      ]

-- | Contains the output for CreateLBCookieStickinessPolicy.
--
-- /See:/ 'mkCreateLBCookieStickinessPolicyResponse' smart constructor.
newtype CreateLBCookieStickinessPolicyResponse = CreateLBCookieStickinessPolicyResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLBCookieStickinessPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateLBCookieStickinessPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLBCookieStickinessPolicyResponse
mkCreateLBCookieStickinessPolicyResponse pResponseStatus_ =
  CreateLBCookieStickinessPolicyResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbcsprsResponseStatus :: Lens.Lens' CreateLBCookieStickinessPolicyResponse Lude.Int
clbcsprsResponseStatus = Lens.lens (responseStatus :: CreateLBCookieStickinessPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLBCookieStickinessPolicyResponse)
{-# DEPRECATED clbcsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
