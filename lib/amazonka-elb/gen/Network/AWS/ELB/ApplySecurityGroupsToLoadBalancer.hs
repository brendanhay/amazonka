{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ApplySecurityGroupsToLoadBalancer (..)
    , mkApplySecurityGroupsToLoadBalancer
    -- ** Request lenses
    , asgtlbLoadBalancerName
    , asgtlbSecurityGroups

    -- * Destructuring the response
    , ApplySecurityGroupsToLoadBalancerResponse (..)
    , mkApplySecurityGroupsToLoadBalancerResponse
    -- ** Response lenses
    , asgtlbrrsSecurityGroups
    , asgtlbrrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ApplySecurityGroupsToLoadBalancer.
--
-- /See:/ 'mkApplySecurityGroupsToLoadBalancer' smart constructor.
data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer'
  { loadBalancerName :: Types.AccessPointName
    -- ^ The name of the load balancer.
  , securityGroups :: [Types.SecurityGroupId]
    -- ^ The IDs of the security groups to associate with the load balancer. Note that you cannot specify the name of the security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplySecurityGroupsToLoadBalancer' value with any optional fields omitted.
mkApplySecurityGroupsToLoadBalancer
    :: Types.AccessPointName -- ^ 'loadBalancerName'
    -> ApplySecurityGroupsToLoadBalancer
mkApplySecurityGroupsToLoadBalancer loadBalancerName
  = ApplySecurityGroupsToLoadBalancer'{loadBalancerName,
                                       securityGroups = Core.mempty}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtlbLoadBalancerName :: Lens.Lens' ApplySecurityGroupsToLoadBalancer Types.AccessPointName
asgtlbLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE asgtlbLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The IDs of the security groups to associate with the load balancer. Note that you cannot specify the name of the security group.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtlbSecurityGroups :: Lens.Lens' ApplySecurityGroupsToLoadBalancer [Types.SecurityGroupId]
asgtlbSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE asgtlbSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

instance Core.ToQuery ApplySecurityGroupsToLoadBalancer where
        toQuery ApplySecurityGroupsToLoadBalancer{..}
          = Core.toQueryPair "Action"
              ("ApplySecurityGroupsToLoadBalancer" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<>
              Core.toQueryPair "SecurityGroups"
                (Core.toQueryList "member" securityGroups)

instance Core.ToHeaders ApplySecurityGroupsToLoadBalancer where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ApplySecurityGroupsToLoadBalancer where
        type Rs ApplySecurityGroupsToLoadBalancer =
             ApplySecurityGroupsToLoadBalancerResponse
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
              "ApplySecurityGroupsToLoadBalancerResult"
              (\ s h x ->
                 ApplySecurityGroupsToLoadBalancerResponse' Core.<$>
                   (x Core..@? "SecurityGroups" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of ApplySecurityGroupsToLoadBalancer.
--
-- /See:/ 'mkApplySecurityGroupsToLoadBalancerResponse' smart constructor.
data ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse'
  { securityGroups :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of the security groups associated with the load balancer.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplySecurityGroupsToLoadBalancerResponse' value with any optional fields omitted.
mkApplySecurityGroupsToLoadBalancerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ApplySecurityGroupsToLoadBalancerResponse
mkApplySecurityGroupsToLoadBalancerResponse responseStatus
  = ApplySecurityGroupsToLoadBalancerResponse'{securityGroups =
                                                 Core.Nothing,
                                               responseStatus}

-- | The IDs of the security groups associated with the load balancer.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtlbrrsSecurityGroups :: Lens.Lens' ApplySecurityGroupsToLoadBalancerResponse (Core.Maybe [Types.SecurityGroupId])
asgtlbrrsSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE asgtlbrrsSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtlbrrsResponseStatus :: Lens.Lens' ApplySecurityGroupsToLoadBalancerResponse Core.Int
asgtlbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asgtlbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
