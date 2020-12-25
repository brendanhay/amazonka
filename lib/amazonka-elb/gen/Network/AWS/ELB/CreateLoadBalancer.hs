{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    clbLoadBalancerName,
    clbListeners,
    clbAvailabilityZones,
    clbScheme,
    clbSecurityGroups,
    clbSubnets,
    clbTags,

    -- * Destructuring the response
    CreateLoadBalancerResponse (..),
    mkCreateLoadBalancerResponse,

    -- ** Response lenses
    clbrrsDNSName,
    clbrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateLoadBalancer.
--
-- /See:/ 'mkCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { -- | The name of the load balancer.
    --
    -- This name must be unique within your set of load balancers for the region, must have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and cannot begin or end with a hyphen.
    loadBalancerName :: Types.AccessPointName,
    -- | The listeners.
    --
    -- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
    listeners :: [Types.Listener],
    -- | One or more Availability Zones from the same region as the load balancer.
    --
    -- You must specify at least one Availability Zone.
    -- You can add more Availability Zones after you create the load balancer using 'EnableAvailabilityZonesForLoadBalancer' .
    availabilityZones :: Core.Maybe [Types.AvailabilityZone],
    -- | The type of a load balancer. Valid only for load balancers in a VPC.
    --
    -- By default, Elastic Load Balancing creates an Internet-facing load balancer with a DNS name that resolves to public IP addresses. For more information about Internet-facing and Internal load balancers, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/how-elastic-load-balancing-works.html#load-balancer-scheme Load Balancer Scheme> in the /Elastic Load Balancing User Guide/ .
    -- Specify @internal@ to create a load balancer with a DNS name that resolves to private IP addresses.
    scheme :: Core.Maybe Types.LoadBalancerScheme,
    -- | The IDs of the security groups to assign to the load balancer.
    securityGroups :: Core.Maybe [Types.SecurityGroupId],
    -- | The IDs of the subnets in your VPC to attach to the load balancer. Specify one subnet per Availability Zone specified in @AvailabilityZones@ .
    subnets :: Core.Maybe [Types.SubnetId],
    -- | A list of tags to assign to the load balancer.
    --
    -- For more information about tagging your load balancer, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancer' value with any optional fields omitted.
mkCreateLoadBalancer ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  CreateLoadBalancer
mkCreateLoadBalancer loadBalancerName =
  CreateLoadBalancer'
    { loadBalancerName,
      listeners = Core.mempty,
      availabilityZones = Core.Nothing,
      scheme = Core.Nothing,
      securityGroups = Core.Nothing,
      subnets = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the load balancer.
--
-- This name must be unique within your set of load balancers for the region, must have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and cannot begin or end with a hyphen.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbLoadBalancerName :: Lens.Lens' CreateLoadBalancer Types.AccessPointName
clbLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED clbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The listeners.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbListeners :: Lens.Lens' CreateLoadBalancer [Types.Listener]
clbListeners = Lens.field @"listeners"
{-# DEPRECATED clbListeners "Use generic-lens or generic-optics with 'listeners' instead." #-}

-- | One or more Availability Zones from the same region as the load balancer.
--
-- You must specify at least one Availability Zone.
-- You can add more Availability Zones after you create the load balancer using 'EnableAvailabilityZonesForLoadBalancer' .
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbAvailabilityZones :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Types.AvailabilityZone])
clbAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED clbAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The type of a load balancer. Valid only for load balancers in a VPC.
--
-- By default, Elastic Load Balancing creates an Internet-facing load balancer with a DNS name that resolves to public IP addresses. For more information about Internet-facing and Internal load balancers, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/how-elastic-load-balancing-works.html#load-balancer-scheme Load Balancer Scheme> in the /Elastic Load Balancing User Guide/ .
-- Specify @internal@ to create a load balancer with a DNS name that resolves to private IP addresses.
--
-- /Note:/ Consider using 'scheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbScheme :: Lens.Lens' CreateLoadBalancer (Core.Maybe Types.LoadBalancerScheme)
clbScheme = Lens.field @"scheme"
{-# DEPRECATED clbScheme "Use generic-lens or generic-optics with 'scheme' instead." #-}

-- | The IDs of the security groups to assign to the load balancer.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSecurityGroups :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Types.SecurityGroupId])
clbSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED clbSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The IDs of the subnets in your VPC to attach to the load balancer. Specify one subnet per Availability Zone specified in @AvailabilityZones@ .
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSubnets :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Types.SubnetId])
clbSubnets = Lens.field @"subnets"
{-# DEPRECATED clbSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | A list of tags to assign to the load balancer.
--
-- For more information about tagging your load balancer, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbTags :: Lens.Lens' CreateLoadBalancer (Core.Maybe (Core.NonEmpty Types.Tag))
clbTags = Lens.field @"tags"
{-# DEPRECATED clbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateLoadBalancer where
  type Rs CreateLoadBalancer = CreateLoadBalancerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateLoadBalancer")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> ( Core.toQueryValue
                            "Listeners"
                            (Core.toQueryList "member" listeners)
                        )
                Core.<> ( Core.toQueryValue
                            "AvailabilityZones"
                            (Core.toQueryList "member" Core.<$> availabilityZones)
                        )
                Core.<> (Core.toQueryValue "Scheme" Core.<$> scheme)
                Core.<> ( Core.toQueryValue
                            "SecurityGroups"
                            (Core.toQueryList "member" Core.<$> securityGroups)
                        )
                Core.<> ( Core.toQueryValue
                            "Subnets"
                            (Core.toQueryList "member" Core.<$> subnets)
                        )
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateLoadBalancerResult"
      ( \s h x ->
          CreateLoadBalancerResponse'
            Core.<$> (x Core..@? "DNSName") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output for CreateLoadBalancer.
--
-- /See:/ 'mkCreateLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { -- | The DNS name of the load balancer.
    dNSName :: Core.Maybe Types.DNSName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancerResponse' value with any optional fields omitted.
mkCreateLoadBalancerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateLoadBalancerResponse
mkCreateLoadBalancerResponse responseStatus =
  CreateLoadBalancerResponse'
    { dNSName = Core.Nothing,
      responseStatus
    }

-- | The DNS name of the load balancer.
--
-- /Note:/ Consider using 'dNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrrsDNSName :: Lens.Lens' CreateLoadBalancerResponse (Core.Maybe Types.DNSName)
clbrrsDNSName = Lens.field @"dNSName"
{-# DEPRECATED clbrrsDNSName "Use generic-lens or generic-optics with 'dNSName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrrsResponseStatus :: Lens.Lens' CreateLoadBalancerResponse Core.Int
clbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED clbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
