{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified policies.
--
-- If you specify a load balancer name, the action returns the descriptions of all policies created for the load balancer. If you specify a policy name associated with your load balancer, the action returns the description of that policy. If you don't specify a load balancer name, the action returns descriptions of the specified sample policies, or descriptions of all sample policies. The names of the sample policies have the @ELBSample-@ prefix.
module Network.AWS.ELB.DescribeLoadBalancerPolicies
  ( -- * Creating a request
    DescribeLoadBalancerPolicies (..),
    mkDescribeLoadBalancerPolicies,

    -- ** Request lenses
    dlbpPolicyNames,
    dlbpLoadBalancerName,

    -- * Destructuring the response
    DescribeLoadBalancerPoliciesResponse (..),
    mkDescribeLoadBalancerPoliciesResponse,

    -- ** Response lenses
    drsPolicyDescriptions,
    drsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeLoadBalancerPolicies.
--
-- /See:/ 'mkDescribeLoadBalancerPolicies' smart constructor.
data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies'
  { -- | The names of the policies.
    policyNames :: Lude.Maybe [Lude.Text],
    -- | The name of the load balancer.
    loadBalancerName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancerPolicies' with the minimum fields required to make a request.
--
-- * 'policyNames' - The names of the policies.
-- * 'loadBalancerName' - The name of the load balancer.
mkDescribeLoadBalancerPolicies ::
  DescribeLoadBalancerPolicies
mkDescribeLoadBalancerPolicies =
  DescribeLoadBalancerPolicies'
    { policyNames = Lude.Nothing,
      loadBalancerName = Lude.Nothing
    }

-- | The names of the policies.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbpPolicyNames :: Lens.Lens' DescribeLoadBalancerPolicies (Lude.Maybe [Lude.Text])
dlbpPolicyNames = Lens.lens (policyNames :: DescribeLoadBalancerPolicies -> Lude.Maybe [Lude.Text]) (\s a -> s {policyNames = a} :: DescribeLoadBalancerPolicies)
{-# DEPRECATED dlbpPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbpLoadBalancerName :: Lens.Lens' DescribeLoadBalancerPolicies (Lude.Maybe Lude.Text)
dlbpLoadBalancerName = Lens.lens (loadBalancerName :: DescribeLoadBalancerPolicies -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerName = a} :: DescribeLoadBalancerPolicies)
{-# DEPRECATED dlbpLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Lude.AWSRequest DescribeLoadBalancerPolicies where
  type
    Rs DescribeLoadBalancerPolicies =
      DescribeLoadBalancerPoliciesResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DescribeLoadBalancerPoliciesResult"
      ( \s h x ->
          DescribeLoadBalancerPoliciesResponse'
            Lude.<$> ( x Lude..@? "PolicyDescriptions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLoadBalancerPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLoadBalancerPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLoadBalancerPolicies where
  toQuery DescribeLoadBalancerPolicies' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeLoadBalancerPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "PolicyNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyNames),
        "LoadBalancerName" Lude.=: loadBalancerName
      ]

-- | Contains the output of DescribeLoadBalancerPolicies.
--
-- /See:/ 'mkDescribeLoadBalancerPoliciesResponse' smart constructor.
data DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse'
  { -- | Information about the policies.
    policyDescriptions :: Lude.Maybe [PolicyDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancerPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'policyDescriptions' - Information about the policies.
-- * 'responseStatus' - The response status code.
mkDescribeLoadBalancerPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoadBalancerPoliciesResponse
mkDescribeLoadBalancerPoliciesResponse pResponseStatus_ =
  DescribeLoadBalancerPoliciesResponse'
    { policyDescriptions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the policies.
--
-- /Note:/ Consider using 'policyDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPolicyDescriptions :: Lens.Lens' DescribeLoadBalancerPoliciesResponse (Lude.Maybe [PolicyDescription])
drsPolicyDescriptions = Lens.lens (policyDescriptions :: DescribeLoadBalancerPoliciesResponse -> Lude.Maybe [PolicyDescription]) (\s a -> s {policyDescriptions = a} :: DescribeLoadBalancerPoliciesResponse)
{-# DEPRECATED drsPolicyDescriptions "Use generic-lens or generic-optics with 'policyDescriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeLoadBalancerPoliciesResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeLoadBalancerPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoadBalancerPoliciesResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
