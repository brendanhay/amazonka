{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.SetSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified security groups with the specified Application Load Balancer. The specified security groups override the previously associated security groups.
--
-- You can't specify a security group for a Network Load Balancer or Gateway Load Balancer.
module Network.AWS.ELBv2.SetSecurityGroups
    (
    -- * Creating a request
      SetSecurityGroups (..)
    , mkSetSecurityGroups
    -- ** Request lenses
    , ssgLoadBalancerArn
    , ssgSecurityGroups

    -- * Destructuring the response
    , SetSecurityGroupsResponse (..)
    , mkSetSecurityGroupsResponse
    -- ** Response lenses
    , ssgrrsSecurityGroupIds
    , ssgrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetSecurityGroups' smart constructor.
data SetSecurityGroups = SetSecurityGroups'
  { loadBalancerArn :: Types.LoadBalancerArn
    -- ^ The Amazon Resource Name (ARN) of the load balancer.
  , securityGroups :: [Types.SecurityGroupId]
    -- ^ The IDs of the security groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSecurityGroups' value with any optional fields omitted.
mkSetSecurityGroups
    :: Types.LoadBalancerArn -- ^ 'loadBalancerArn'
    -> SetSecurityGroups
mkSetSecurityGroups loadBalancerArn
  = SetSecurityGroups'{loadBalancerArn, securityGroups = Core.mempty}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgLoadBalancerArn :: Lens.Lens' SetSecurityGroups Types.LoadBalancerArn
ssgLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# INLINEABLE ssgLoadBalancerArn #-}
{-# DEPRECATED loadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead"  #-}

-- | The IDs of the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgSecurityGroups :: Lens.Lens' SetSecurityGroups [Types.SecurityGroupId]
ssgSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE ssgSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

instance Core.ToQuery SetSecurityGroups where
        toQuery SetSecurityGroups{..}
          = Core.toQueryPair "Action" ("SetSecurityGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerArn" loadBalancerArn
              Core.<>
              Core.toQueryPair "SecurityGroups"
                (Core.toQueryList "member" securityGroups)

instance Core.ToHeaders SetSecurityGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetSecurityGroups where
        type Rs SetSecurityGroups = SetSecurityGroupsResponse
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
          = Response.receiveXMLWrapper "SetSecurityGroupsResult"
              (\ s h x ->
                 SetSecurityGroupsResponse' Core.<$>
                   (x Core..@? "SecurityGroupIds" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetSecurityGroupsResponse' smart constructor.
data SetSecurityGroupsResponse = SetSecurityGroupsResponse'
  { securityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of the security groups associated with the load balancer.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSecurityGroupsResponse' value with any optional fields omitted.
mkSetSecurityGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetSecurityGroupsResponse
mkSetSecurityGroupsResponse responseStatus
  = SetSecurityGroupsResponse'{securityGroupIds = Core.Nothing,
                               responseStatus}

-- | The IDs of the security groups associated with the load balancer.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgrrsSecurityGroupIds :: Lens.Lens' SetSecurityGroupsResponse (Core.Maybe [Types.SecurityGroupId])
ssgrrsSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE ssgrrsSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgrrsResponseStatus :: Lens.Lens' SetSecurityGroupsResponse Core.Int
ssgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ssgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
