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
    clbpPolicyAttributes,
    clbpPolicyName,
    clbpLoadBalancerName,
    clbpPolicyTypeName,

    -- * Destructuring the response
    CreateLoadBalancerPolicyResponse (..),
    mkCreateLoadBalancerPolicyResponse,

    -- ** Response lenses
    clbprsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateLoadBalancerPolicy.
--
-- /See:/ 'mkCreateLoadBalancerPolicy' smart constructor.
data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy'
  { -- | The policy attributes.
    policyAttributes :: Lude.Maybe [PolicyAttribute],
    -- | The name of the load balancer policy to be created. This name must be unique within the set of policies for this load balancer.
    policyName :: Lude.Text,
    -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The name of the base policy type. To get the list of policy types, use 'DescribeLoadBalancerPolicyTypes' .
    policyTypeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancerPolicy' with the minimum fields required to make a request.
--
-- * 'policyAttributes' - The policy attributes.
-- * 'policyName' - The name of the load balancer policy to be created. This name must be unique within the set of policies for this load balancer.
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'policyTypeName' - The name of the base policy type. To get the list of policy types, use 'DescribeLoadBalancerPolicyTypes' .
mkCreateLoadBalancerPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'policyTypeName'
  Lude.Text ->
  CreateLoadBalancerPolicy
mkCreateLoadBalancerPolicy
  pPolicyName_
  pLoadBalancerName_
  pPolicyTypeName_ =
    CreateLoadBalancerPolicy'
      { policyAttributes = Lude.Nothing,
        policyName = pPolicyName_,
        loadBalancerName = pLoadBalancerName_,
        policyTypeName = pPolicyTypeName_
      }

-- | The policy attributes.
--
-- /Note:/ Consider using 'policyAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpPolicyAttributes :: Lens.Lens' CreateLoadBalancerPolicy (Lude.Maybe [PolicyAttribute])
clbpPolicyAttributes = Lens.lens (policyAttributes :: CreateLoadBalancerPolicy -> Lude.Maybe [PolicyAttribute]) (\s a -> s {policyAttributes = a} :: CreateLoadBalancerPolicy)
{-# DEPRECATED clbpPolicyAttributes "Use generic-lens or generic-optics with 'policyAttributes' instead." #-}

-- | The name of the load balancer policy to be created. This name must be unique within the set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpPolicyName :: Lens.Lens' CreateLoadBalancerPolicy Lude.Text
clbpPolicyName = Lens.lens (policyName :: CreateLoadBalancerPolicy -> Lude.Text) (\s a -> s {policyName = a} :: CreateLoadBalancerPolicy)
{-# DEPRECATED clbpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpLoadBalancerName :: Lens.Lens' CreateLoadBalancerPolicy Lude.Text
clbpLoadBalancerName = Lens.lens (loadBalancerName :: CreateLoadBalancerPolicy -> Lude.Text) (\s a -> s {loadBalancerName = a} :: CreateLoadBalancerPolicy)
{-# DEPRECATED clbpLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The name of the base policy type. To get the list of policy types, use 'DescribeLoadBalancerPolicyTypes' .
--
-- /Note:/ Consider using 'policyTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbpPolicyTypeName :: Lens.Lens' CreateLoadBalancerPolicy Lude.Text
clbpPolicyTypeName = Lens.lens (policyTypeName :: CreateLoadBalancerPolicy -> Lude.Text) (\s a -> s {policyTypeName = a} :: CreateLoadBalancerPolicy)
{-# DEPRECATED clbpPolicyTypeName "Use generic-lens or generic-optics with 'policyTypeName' instead." #-}

instance Lude.AWSRequest CreateLoadBalancerPolicy where
  type Rs CreateLoadBalancerPolicy = CreateLoadBalancerPolicyResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "CreateLoadBalancerPolicyResult"
      ( \s h x ->
          CreateLoadBalancerPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLoadBalancerPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLoadBalancerPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLoadBalancerPolicy where
  toQuery CreateLoadBalancerPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateLoadBalancerPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "PolicyAttributes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyAttributes),
        "PolicyName" Lude.=: policyName,
        "LoadBalancerName" Lude.=: loadBalancerName,
        "PolicyTypeName" Lude.=: policyTypeName
      ]

-- | Contains the output of CreateLoadBalancerPolicy.
--
-- /See:/ 'mkCreateLoadBalancerPolicyResponse' smart constructor.
newtype CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancerPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateLoadBalancerPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLoadBalancerPolicyResponse
mkCreateLoadBalancerPolicyResponse pResponseStatus_ =
  CreateLoadBalancerPolicyResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbprsResponseStatus :: Lens.Lens' CreateLoadBalancerPolicyResponse Lude.Int
clbprsResponseStatus = Lens.lens (responseStatus :: CreateLoadBalancerPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLoadBalancerPolicyResponse)
{-# DEPRECATED clbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
