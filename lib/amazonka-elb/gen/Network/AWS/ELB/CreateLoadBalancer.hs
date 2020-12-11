{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Classic Load Balancer.
--
-- You can add listeners, security groups, subnets, and tags when you create your load balancer, or you can add them later using 'CreateLoadBalancerListeners' , 'ApplySecurityGroupsToLoadBalancer' , 'AttachLoadBalancerToSubnets' , and 'AddTags' .
-- To describe your current load balancers, see 'DescribeLoadBalancers' . When you are finished with a load balancer, you can delete it using 'DeleteLoadBalancer' .
-- You can create up to 20 load balancers per region per account. You can request an increase for the number of load balancers for your account. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-limits.html Limits for Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.CreateLoadBalancer
  ( -- * Creating a request
    CreateLoadBalancer (..),
    mkCreateLoadBalancer,

    -- ** Request lenses
    clbSecurityGroups,
    clbSubnets,
    clbAvailabilityZones,
    clbScheme,
    clbTags,
    clbLoadBalancerName,
    clbListeners,

    -- * Destructuring the response
    CreateLoadBalancerResponse (..),
    mkCreateLoadBalancerResponse,

    -- ** Response lenses
    clbrsDNSName,
    clbrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateLoadBalancer.
--
-- /See:/ 'mkCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { securityGroups ::
      Lude.Maybe [Lude.Text],
    subnets :: Lude.Maybe [Lude.Text],
    availabilityZones :: Lude.Maybe [Lude.Text],
    scheme :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    loadBalancerName :: Lude.Text,
    listeners :: [Listener]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancer' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - One or more Availability Zones from the same region as the load balancer.
--
-- You must specify at least one Availability Zone.
-- You can add more Availability Zones after you create the load balancer using 'EnableAvailabilityZonesForLoadBalancer' .
-- * 'listeners' - The listeners.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
-- * 'loadBalancerName' - The name of the load balancer.
--
-- This name must be unique within your set of load balancers for the region, must have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and cannot begin or end with a hyphen.
-- * 'scheme' - The type of a load balancer. Valid only for load balancers in a VPC.
--
-- By default, Elastic Load Balancing creates an Internet-facing load balancer with a DNS name that resolves to public IP addresses. For more information about Internet-facing and Internal load balancers, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/how-elastic-load-balancing-works.html#load-balancer-scheme Load Balancer Scheme> in the /Elastic Load Balancing User Guide/ .
-- Specify @internal@ to create a load balancer with a DNS name that resolves to private IP addresses.
-- * 'securityGroups' - The IDs of the security groups to assign to the load balancer.
-- * 'subnets' - The IDs of the subnets in your VPC to attach to the load balancer. Specify one subnet per Availability Zone specified in @AvailabilityZones@ .
-- * 'tags' - A list of tags to assign to the load balancer.
--
-- For more information about tagging your load balancer, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
mkCreateLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  CreateLoadBalancer
mkCreateLoadBalancer pLoadBalancerName_ =
  CreateLoadBalancer'
    { securityGroups = Lude.Nothing,
      subnets = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      scheme = Lude.Nothing,
      tags = Lude.Nothing,
      loadBalancerName = pLoadBalancerName_,
      listeners = Lude.mempty
    }

-- | The IDs of the security groups to assign to the load balancer.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSecurityGroups :: Lens.Lens' CreateLoadBalancer (Lude.Maybe [Lude.Text])
clbSecurityGroups = Lens.lens (securityGroups :: CreateLoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: CreateLoadBalancer)
{-# DEPRECATED clbSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The IDs of the subnets in your VPC to attach to the load balancer. Specify one subnet per Availability Zone specified in @AvailabilityZones@ .
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSubnets :: Lens.Lens' CreateLoadBalancer (Lude.Maybe [Lude.Text])
clbSubnets = Lens.lens (subnets :: CreateLoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {subnets = a} :: CreateLoadBalancer)
{-# DEPRECATED clbSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | One or more Availability Zones from the same region as the load balancer.
--
-- You must specify at least one Availability Zone.
-- You can add more Availability Zones after you create the load balancer using 'EnableAvailabilityZonesForLoadBalancer' .
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbAvailabilityZones :: Lens.Lens' CreateLoadBalancer (Lude.Maybe [Lude.Text])
clbAvailabilityZones = Lens.lens (availabilityZones :: CreateLoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: CreateLoadBalancer)
{-# DEPRECATED clbAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The type of a load balancer. Valid only for load balancers in a VPC.
--
-- By default, Elastic Load Balancing creates an Internet-facing load balancer with a DNS name that resolves to public IP addresses. For more information about Internet-facing and Internal load balancers, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/how-elastic-load-balancing-works.html#load-balancer-scheme Load Balancer Scheme> in the /Elastic Load Balancing User Guide/ .
-- Specify @internal@ to create a load balancer with a DNS name that resolves to private IP addresses.
--
-- /Note:/ Consider using 'scheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbScheme :: Lens.Lens' CreateLoadBalancer (Lude.Maybe Lude.Text)
clbScheme = Lens.lens (scheme :: CreateLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {scheme = a} :: CreateLoadBalancer)
{-# DEPRECATED clbScheme "Use generic-lens or generic-optics with 'scheme' instead." #-}

-- | A list of tags to assign to the load balancer.
--
-- For more information about tagging your load balancer, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbTags :: Lens.Lens' CreateLoadBalancer (Lude.Maybe (Lude.NonEmpty Tag))
clbTags = Lens.lens (tags :: CreateLoadBalancer -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateLoadBalancer)
{-# DEPRECATED clbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the load balancer.
--
-- This name must be unique within your set of load balancers for the region, must have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and cannot begin or end with a hyphen.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbLoadBalancerName :: Lens.Lens' CreateLoadBalancer Lude.Text
clbLoadBalancerName = Lens.lens (loadBalancerName :: CreateLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: CreateLoadBalancer)
{-# DEPRECATED clbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The listeners.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbListeners :: Lens.Lens' CreateLoadBalancer [Listener]
clbListeners = Lens.lens (listeners :: CreateLoadBalancer -> [Listener]) (\s a -> s {listeners = a} :: CreateLoadBalancer)
{-# DEPRECATED clbListeners "Use generic-lens or generic-optics with 'listeners' instead." #-}

instance Lude.AWSRequest CreateLoadBalancer where
  type Rs CreateLoadBalancer = CreateLoadBalancerResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "CreateLoadBalancerResult"
      ( \s h x ->
          CreateLoadBalancerResponse'
            Lude.<$> (x Lude..@? "DNSName") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLoadBalancer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLoadBalancer where
  toQuery CreateLoadBalancer' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateLoadBalancer" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "SecurityGroups"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> securityGroups),
        "Subnets"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> subnets),
        "AvailabilityZones"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> availabilityZones),
        "Scheme" Lude.=: scheme,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "Listeners" Lude.=: Lude.toQueryList "member" listeners
      ]

-- | Contains the output for CreateLoadBalancer.
--
-- /See:/ 'mkCreateLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { dnsName ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'dnsName' - The DNS name of the load balancer.
-- * 'responseStatus' - The response status code.
mkCreateLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLoadBalancerResponse
mkCreateLoadBalancerResponse pResponseStatus_ =
  CreateLoadBalancerResponse'
    { dnsName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The DNS name of the load balancer.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrsDNSName :: Lens.Lens' CreateLoadBalancerResponse (Lude.Maybe Lude.Text)
clbrsDNSName = Lens.lens (dnsName :: CreateLoadBalancerResponse -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: CreateLoadBalancerResponse)
{-# DEPRECATED clbrsDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrsResponseStatus :: Lens.Lens' CreateLoadBalancerResponse Lude.Int
clbrsResponseStatus = Lens.lens (responseStatus :: CreateLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLoadBalancerResponse)
{-# DEPRECATED clbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
