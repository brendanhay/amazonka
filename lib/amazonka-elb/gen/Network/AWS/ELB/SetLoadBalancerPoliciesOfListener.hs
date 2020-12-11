{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    slbpolrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for SetLoadBalancePoliciesOfListener.
--
-- /See:/ 'mkSetLoadBalancerPoliciesOfListener' smart constructor.
data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener'
  { loadBalancerName ::
      Lude.Text,
    loadBalancerPort ::
      Lude.Int,
    policyNames ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLoadBalancerPoliciesOfListener' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'loadBalancerPort' - The external port of the load balancer.
-- * 'policyNames' - The names of the policies. This list must include all policies to be enabled. If you omit a policy that is currently enabled, it is disabled. If the list is empty, all current policies are disabled.
mkSetLoadBalancerPoliciesOfListener ::
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'loadBalancerPort'
  Lude.Int ->
  SetLoadBalancerPoliciesOfListener
mkSetLoadBalancerPoliciesOfListener
  pLoadBalancerName_
  pLoadBalancerPort_ =
    SetLoadBalancerPoliciesOfListener'
      { loadBalancerName =
          pLoadBalancerName_,
        loadBalancerPort = pLoadBalancerPort_,
        policyNames = Lude.mempty
      }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpolLoadBalancerName :: Lens.Lens' SetLoadBalancerPoliciesOfListener Lude.Text
slbpolLoadBalancerName = Lens.lens (loadBalancerName :: SetLoadBalancerPoliciesOfListener -> Lude.Text) (\s a -> s {loadBalancerName = a} :: SetLoadBalancerPoliciesOfListener)
{-# DEPRECATED slbpolLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The external port of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpolLoadBalancerPort :: Lens.Lens' SetLoadBalancerPoliciesOfListener Lude.Int
slbpolLoadBalancerPort = Lens.lens (loadBalancerPort :: SetLoadBalancerPoliciesOfListener -> Lude.Int) (\s a -> s {loadBalancerPort = a} :: SetLoadBalancerPoliciesOfListener)
{-# DEPRECATED slbpolLoadBalancerPort "Use generic-lens or generic-optics with 'loadBalancerPort' instead." #-}

-- | The names of the policies. This list must include all policies to be enabled. If you omit a policy that is currently enabled, it is disabled. If the list is empty, all current policies are disabled.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpolPolicyNames :: Lens.Lens' SetLoadBalancerPoliciesOfListener [Lude.Text]
slbpolPolicyNames = Lens.lens (policyNames :: SetLoadBalancerPoliciesOfListener -> [Lude.Text]) (\s a -> s {policyNames = a} :: SetLoadBalancerPoliciesOfListener)
{-# DEPRECATED slbpolPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

instance Lude.AWSRequest SetLoadBalancerPoliciesOfListener where
  type
    Rs SetLoadBalancerPoliciesOfListener =
      SetLoadBalancerPoliciesOfListenerResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "SetLoadBalancerPoliciesOfListenerResult"
      ( \s h x ->
          SetLoadBalancerPoliciesOfListenerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetLoadBalancerPoliciesOfListener where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetLoadBalancerPoliciesOfListener where
  toPath = Lude.const "/"

instance Lude.ToQuery SetLoadBalancerPoliciesOfListener where
  toQuery SetLoadBalancerPoliciesOfListener' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SetLoadBalancerPoliciesOfListener" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "LoadBalancerPort" Lude.=: loadBalancerPort,
        "PolicyNames" Lude.=: Lude.toQueryList "member" policyNames
      ]

-- | Contains the output of SetLoadBalancePoliciesOfListener.
--
-- /See:/ 'mkSetLoadBalancerPoliciesOfListenerResponse' smart constructor.
newtype SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse'
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

-- | Creates a value of 'SetLoadBalancerPoliciesOfListenerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetLoadBalancerPoliciesOfListenerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetLoadBalancerPoliciesOfListenerResponse
mkSetLoadBalancerPoliciesOfListenerResponse pResponseStatus_ =
  SetLoadBalancerPoliciesOfListenerResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbpolrsResponseStatus :: Lens.Lens' SetLoadBalancerPoliciesOfListenerResponse Lude.Int
slbpolrsResponseStatus = Lens.lens (responseStatus :: SetLoadBalancerPoliciesOfListenerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetLoadBalancerPoliciesOfListenerResponse)
{-# DEPRECATED slbpolrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
