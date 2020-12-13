{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified load balancer policy types or all load balancer policy types.
--
-- The description of each type indicates how it can be used. For example, some policies can be used only with layer 7 listeners, some policies can be used only with layer 4 listeners, and some policies can be used only with your EC2 instances.
-- You can use 'CreateLoadBalancerPolicy' to create a policy configuration for any of these policy types. Then, depending on the policy type, use either 'SetLoadBalancerPoliciesOfListener' or 'SetLoadBalancerPoliciesForBackendServer' to set the policy.
module Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
  ( -- * Creating a request
    DescribeLoadBalancerPolicyTypes (..),
    mkDescribeLoadBalancerPolicyTypes,

    -- ** Request lenses
    dlbptPolicyTypeNames,

    -- * Destructuring the response
    DescribeLoadBalancerPolicyTypesResponse (..),
    mkDescribeLoadBalancerPolicyTypesResponse,

    -- ** Response lenses
    dlbptrsPolicyTypeDescriptions,
    dlbptrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeLoadBalancerPolicyTypes.
--
-- /See:/ 'mkDescribeLoadBalancerPolicyTypes' smart constructor.
newtype DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes'
  { -- | The names of the policy types. If no names are specified, describes all policy types defined by Elastic Load Balancing.
    policyTypeNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancerPolicyTypes' with the minimum fields required to make a request.
--
-- * 'policyTypeNames' - The names of the policy types. If no names are specified, describes all policy types defined by Elastic Load Balancing.
mkDescribeLoadBalancerPolicyTypes ::
  DescribeLoadBalancerPolicyTypes
mkDescribeLoadBalancerPolicyTypes =
  DescribeLoadBalancerPolicyTypes' {policyTypeNames = Lude.Nothing}

-- | The names of the policy types. If no names are specified, describes all policy types defined by Elastic Load Balancing.
--
-- /Note:/ Consider using 'policyTypeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbptPolicyTypeNames :: Lens.Lens' DescribeLoadBalancerPolicyTypes (Lude.Maybe [Lude.Text])
dlbptPolicyTypeNames = Lens.lens (policyTypeNames :: DescribeLoadBalancerPolicyTypes -> Lude.Maybe [Lude.Text]) (\s a -> s {policyTypeNames = a} :: DescribeLoadBalancerPolicyTypes)
{-# DEPRECATED dlbptPolicyTypeNames "Use generic-lens or generic-optics with 'policyTypeNames' instead." #-}

instance Lude.AWSRequest DescribeLoadBalancerPolicyTypes where
  type
    Rs DescribeLoadBalancerPolicyTypes =
      DescribeLoadBalancerPolicyTypesResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DescribeLoadBalancerPolicyTypesResult"
      ( \s h x ->
          DescribeLoadBalancerPolicyTypesResponse'
            Lude.<$> ( x Lude..@? "PolicyTypeDescriptions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLoadBalancerPolicyTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLoadBalancerPolicyTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLoadBalancerPolicyTypes where
  toQuery DescribeLoadBalancerPolicyTypes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeLoadBalancerPolicyTypes" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "PolicyTypeNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyTypeNames)
      ]

-- | Contains the output of DescribeLoadBalancerPolicyTypes.
--
-- /See:/ 'mkDescribeLoadBalancerPolicyTypesResponse' smart constructor.
data DescribeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse'
  { -- | Information about the policy types.
    policyTypeDescriptions :: Lude.Maybe [PolicyTypeDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancerPolicyTypesResponse' with the minimum fields required to make a request.
--
-- * 'policyTypeDescriptions' - Information about the policy types.
-- * 'responseStatus' - The response status code.
mkDescribeLoadBalancerPolicyTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoadBalancerPolicyTypesResponse
mkDescribeLoadBalancerPolicyTypesResponse pResponseStatus_ =
  DescribeLoadBalancerPolicyTypesResponse'
    { policyTypeDescriptions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the policy types.
--
-- /Note:/ Consider using 'policyTypeDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbptrsPolicyTypeDescriptions :: Lens.Lens' DescribeLoadBalancerPolicyTypesResponse (Lude.Maybe [PolicyTypeDescription])
dlbptrsPolicyTypeDescriptions = Lens.lens (policyTypeDescriptions :: DescribeLoadBalancerPolicyTypesResponse -> Lude.Maybe [PolicyTypeDescription]) (\s a -> s {policyTypeDescriptions = a} :: DescribeLoadBalancerPolicyTypesResponse)
{-# DEPRECATED dlbptrsPolicyTypeDescriptions "Use generic-lens or generic-optics with 'policyTypeDescriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbptrsResponseStatus :: Lens.Lens' DescribeLoadBalancerPolicyTypesResponse Lude.Int
dlbptrsResponseStatus = Lens.lens (responseStatus :: DescribeLoadBalancerPolicyTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoadBalancerPolicyTypesResponse)
{-# DEPRECATED dlbptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
