{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches one or more Classic Load Balancers from the specified Auto Scaling group.
--
-- This operation detaches only Classic Load Balancers. If you have Application Load Balancers, Network Load Balancers, or Gateway Load Balancers, use the 'DetachLoadBalancerTargetGroups' API instead.
-- When you detach a load balancer, it enters the @Removing@ state while deregistering the instances in the group. When all instances are deregistered, then you can no longer describe the load balancer using the 'DescribeLoadBalancers' API call. The instances remain running.
module Network.AWS.AutoScaling.DetachLoadBalancers
    (
    -- * Creating a request
      DetachLoadBalancers (..)
    , mkDetachLoadBalancers
    -- ** Request lenses
    , dAutoScalingGroupName
    , dLoadBalancerNames

    -- * Destructuring the response
    , DetachLoadBalancersResponse (..)
    , mkDetachLoadBalancersResponse
    -- ** Response lenses
    , dlbrfrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachLoadBalancers' smart constructor.
data DetachLoadBalancers = DetachLoadBalancers'
  { autoScalingGroupName :: Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , loadBalancerNames :: [Types.XmlStringMaxLen255]
    -- ^ The names of the load balancers. You can specify up to 10 load balancers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancers' value with any optional fields omitted.
mkDetachLoadBalancers
    :: Types.ResourceName -- ^ 'autoScalingGroupName'
    -> DetachLoadBalancers
mkDetachLoadBalancers autoScalingGroupName
  = DetachLoadBalancers'{autoScalingGroupName,
                         loadBalancerNames = Core.mempty}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAutoScalingGroupName :: Lens.Lens' DetachLoadBalancers Types.ResourceName
dAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The names of the load balancers. You can specify up to 10 load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLoadBalancerNames :: Lens.Lens' DetachLoadBalancers [Types.XmlStringMaxLen255]
dLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# INLINEABLE dLoadBalancerNames #-}
{-# DEPRECATED loadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead"  #-}

instance Core.ToQuery DetachLoadBalancers where
        toQuery DetachLoadBalancers{..}
          = Core.toQueryPair "Action" ("DetachLoadBalancers" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "LoadBalancerNames"
                (Core.toQueryList "member" loadBalancerNames)

instance Core.ToHeaders DetachLoadBalancers where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DetachLoadBalancers where
        type Rs DetachLoadBalancers = DetachLoadBalancersResponse
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
          = Response.receiveXMLWrapper "DetachLoadBalancersResult"
              (\ s h x ->
                 DetachLoadBalancersResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachLoadBalancersResponse' smart constructor.
newtype DetachLoadBalancersResponse = DetachLoadBalancersResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancersResponse' value with any optional fields omitted.
mkDetachLoadBalancersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetachLoadBalancersResponse
mkDetachLoadBalancersResponse responseStatus
  = DetachLoadBalancersResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrfrsResponseStatus :: Lens.Lens' DetachLoadBalancersResponse Core.Int
dlbrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlbrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
