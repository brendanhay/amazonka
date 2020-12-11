{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more security groups with your load balancer in a virtual private cloud (VPC). The specified security groups override the previously associated security groups.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-security-groups.html#elb-vpc-security-groups Security Groups for Load Balancers in a VPC> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
  ( -- * Creating a request
    ApplySecurityGroupsToLoadBalancer (..),
    mkApplySecurityGroupsToLoadBalancer,

    -- ** Request lenses
    asgtlbLoadBalancerName,
    asgtlbSecurityGroups,

    -- * Destructuring the response
    ApplySecurityGroupsToLoadBalancerResponse (..),
    mkApplySecurityGroupsToLoadBalancerResponse,

    -- ** Response lenses
    asgtlbrsSecurityGroups,
    asgtlbrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ApplySecurityGroupsToLoadBalancer.
--
-- /See:/ 'mkApplySecurityGroupsToLoadBalancer' smart constructor.
data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer'
  { loadBalancerName ::
      Lude.Text,
    securityGroups ::
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

-- | Creates a value of 'ApplySecurityGroupsToLoadBalancer' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'securityGroups' - The IDs of the security groups to associate with the load balancer. Note that you cannot specify the name of the security group.
mkApplySecurityGroupsToLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  ApplySecurityGroupsToLoadBalancer
mkApplySecurityGroupsToLoadBalancer pLoadBalancerName_ =
  ApplySecurityGroupsToLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      securityGroups = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtlbLoadBalancerName :: Lens.Lens' ApplySecurityGroupsToLoadBalancer Lude.Text
asgtlbLoadBalancerName = Lens.lens (loadBalancerName :: ApplySecurityGroupsToLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: ApplySecurityGroupsToLoadBalancer)
{-# DEPRECATED asgtlbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the security groups to associate with the load balancer. Note that you cannot specify the name of the security group.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtlbSecurityGroups :: Lens.Lens' ApplySecurityGroupsToLoadBalancer [Lude.Text]
asgtlbSecurityGroups = Lens.lens (securityGroups :: ApplySecurityGroupsToLoadBalancer -> [Lude.Text]) (\s a -> s {securityGroups = a} :: ApplySecurityGroupsToLoadBalancer)
{-# DEPRECATED asgtlbSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

instance Lude.AWSRequest ApplySecurityGroupsToLoadBalancer where
  type
    Rs ApplySecurityGroupsToLoadBalancer =
      ApplySecurityGroupsToLoadBalancerResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "ApplySecurityGroupsToLoadBalancerResult"
      ( \s h x ->
          ApplySecurityGroupsToLoadBalancerResponse'
            Lude.<$> ( x Lude..@? "SecurityGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ApplySecurityGroupsToLoadBalancer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ApplySecurityGroupsToLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery ApplySecurityGroupsToLoadBalancer where
  toQuery ApplySecurityGroupsToLoadBalancer' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ApplySecurityGroupsToLoadBalancer" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "SecurityGroups" Lude.=: Lude.toQueryList "member" securityGroups
      ]

-- | Contains the output of ApplySecurityGroupsToLoadBalancer.
--
-- /See:/ 'mkApplySecurityGroupsToLoadBalancerResponse' smart constructor.
data ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse'
  { securityGroups ::
      Lude.Maybe
        [Lude.Text],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplySecurityGroupsToLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'securityGroups' - The IDs of the security groups associated with the load balancer.
mkApplySecurityGroupsToLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ApplySecurityGroupsToLoadBalancerResponse
mkApplySecurityGroupsToLoadBalancerResponse pResponseStatus_ =
  ApplySecurityGroupsToLoadBalancerResponse'
    { securityGroups =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The IDs of the security groups associated with the load balancer.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtlbrsSecurityGroups :: Lens.Lens' ApplySecurityGroupsToLoadBalancerResponse (Lude.Maybe [Lude.Text])
asgtlbrsSecurityGroups = Lens.lens (securityGroups :: ApplySecurityGroupsToLoadBalancerResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: ApplySecurityGroupsToLoadBalancerResponse)
{-# DEPRECATED asgtlbrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtlbrsResponseStatus :: Lens.Lens' ApplySecurityGroupsToLoadBalancerResponse Lude.Int
asgtlbrsResponseStatus = Lens.lens (responseStatus :: ApplySecurityGroupsToLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ApplySecurityGroupsToLoadBalancerResponse)
{-# DEPRECATED asgtlbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
