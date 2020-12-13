{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateAppCookieStickinessPolicy (..),
    mkCreateAppCookieStickinessPolicy,

    -- ** Request lenses
    cacspPolicyName,
    cacspLoadBalancerName,
    cacspCookieName,

    -- * Destructuring the response
    CreateAppCookieStickinessPolicyResponse (..),
    mkCreateAppCookieStickinessPolicyResponse,

    -- ** Response lenses
    cacsprsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateAppCookieStickinessPolicy.
--
-- /See:/ 'mkCreateAppCookieStickinessPolicy' smart constructor.
data CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicy'
  { -- | The name of the policy being created. Policy names must consist of alphanumeric characters and dashes (-). This name must be unique within the set of policies for this load balancer.
    policyName :: Lude.Text,
    -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The name of the application cookie used for stickiness.
    cookieName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAppCookieStickinessPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy being created. Policy names must consist of alphanumeric characters and dashes (-). This name must be unique within the set of policies for this load balancer.
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'cookieName' - The name of the application cookie used for stickiness.
mkCreateAppCookieStickinessPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'cookieName'
  Lude.Text ->
  CreateAppCookieStickinessPolicy
mkCreateAppCookieStickinessPolicy
  pPolicyName_
  pLoadBalancerName_
  pCookieName_ =
    CreateAppCookieStickinessPolicy'
      { policyName = pPolicyName_,
        loadBalancerName = pLoadBalancerName_,
        cookieName = pCookieName_
      }

-- | The name of the policy being created. Policy names must consist of alphanumeric characters and dashes (-). This name must be unique within the set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacspPolicyName :: Lens.Lens' CreateAppCookieStickinessPolicy Lude.Text
cacspPolicyName = Lens.lens (policyName :: CreateAppCookieStickinessPolicy -> Lude.Text) (\s a -> s {policyName = a} :: CreateAppCookieStickinessPolicy)
{-# DEPRECATED cacspPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacspLoadBalancerName :: Lens.Lens' CreateAppCookieStickinessPolicy Lude.Text
cacspLoadBalancerName = Lens.lens (loadBalancerName :: CreateAppCookieStickinessPolicy -> Lude.Text) (\s a -> s {loadBalancerName = a} :: CreateAppCookieStickinessPolicy)
{-# DEPRECATED cacspLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The name of the application cookie used for stickiness.
--
-- /Note:/ Consider using 'cookieName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacspCookieName :: Lens.Lens' CreateAppCookieStickinessPolicy Lude.Text
cacspCookieName = Lens.lens (cookieName :: CreateAppCookieStickinessPolicy -> Lude.Text) (\s a -> s {cookieName = a} :: CreateAppCookieStickinessPolicy)
{-# DEPRECATED cacspCookieName "Use generic-lens or generic-optics with 'cookieName' instead." #-}

instance Lude.AWSRequest CreateAppCookieStickinessPolicy where
  type
    Rs CreateAppCookieStickinessPolicy =
      CreateAppCookieStickinessPolicyResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "CreateAppCookieStickinessPolicyResult"
      ( \s h x ->
          CreateAppCookieStickinessPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAppCookieStickinessPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateAppCookieStickinessPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAppCookieStickinessPolicy where
  toQuery CreateAppCookieStickinessPolicy' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateAppCookieStickinessPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "PolicyName" Lude.=: policyName,
        "LoadBalancerName" Lude.=: loadBalancerName,
        "CookieName" Lude.=: cookieName
      ]

-- | Contains the output for CreateAppCookieStickinessPolicy.
--
-- /See:/ 'mkCreateAppCookieStickinessPolicyResponse' smart constructor.
newtype CreateAppCookieStickinessPolicyResponse = CreateAppCookieStickinessPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAppCookieStickinessPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateAppCookieStickinessPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAppCookieStickinessPolicyResponse
mkCreateAppCookieStickinessPolicyResponse pResponseStatus_ =
  CreateAppCookieStickinessPolicyResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacsprsResponseStatus :: Lens.Lens' CreateAppCookieStickinessPolicyResponse Lude.Int
cacsprsResponseStatus = Lens.lens (responseStatus :: CreateAppCookieStickinessPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAppCookieStickinessPolicyResponse)
{-# DEPRECATED cacsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
