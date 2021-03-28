{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.AttachLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more Classic Load Balancers to the specified Auto Scaling group. Amazon EC2 Auto Scaling registers the running instances with these Classic Load Balancers.
--
-- To describe the load balancers for an Auto Scaling group, call the 'DescribeLoadBalancers' API. To detach the load balancer from the Auto Scaling group, call the 'DetachLoadBalancers' API.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ . 
module Network.AWS.AutoScaling.AttachLoadBalancers
    (
    -- * Creating a request
      AttachLoadBalancers (..)
    , mkAttachLoadBalancers
    -- ** Request lenses
    , albAutoScalingGroupName
    , albLoadBalancerNames

    -- * Destructuring the response
    , AttachLoadBalancersResponse (..)
    , mkAttachLoadBalancersResponse
    -- ** Response lenses
    , albrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachLoadBalancers' smart constructor.
data AttachLoadBalancers = AttachLoadBalancers'
  { autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , loadBalancerNames :: [Types.XmlStringMaxLen255]
    -- ^ The names of the load balancers. You can specify up to 10 load balancers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancers' value with any optional fields omitted.
mkAttachLoadBalancers
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> AttachLoadBalancers
mkAttachLoadBalancers autoScalingGroupName
  = AttachLoadBalancers'{autoScalingGroupName,
                         loadBalancerNames = Core.mempty}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albAutoScalingGroupName :: Lens.Lens' AttachLoadBalancers Types.AutoScalingGroupName
albAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE albAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The names of the load balancers. You can specify up to 10 load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albLoadBalancerNames :: Lens.Lens' AttachLoadBalancers [Types.XmlStringMaxLen255]
albLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# INLINEABLE albLoadBalancerNames #-}
{-# DEPRECATED loadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead"  #-}

instance Core.ToQuery AttachLoadBalancers where
        toQuery AttachLoadBalancers{..}
          = Core.toQueryPair "Action" ("AttachLoadBalancers" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "LoadBalancerNames"
                (Core.toQueryList "member" loadBalancerNames)

instance Core.ToHeaders AttachLoadBalancers where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AttachLoadBalancers where
        type Rs AttachLoadBalancers = AttachLoadBalancersResponse
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
          = Response.receiveXMLWrapper "AttachLoadBalancersResult"
              (\ s h x ->
                 AttachLoadBalancersResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachLoadBalancersResponse' smart constructor.
newtype AttachLoadBalancersResponse = AttachLoadBalancersResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancersResponse' value with any optional fields omitted.
mkAttachLoadBalancersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AttachLoadBalancersResponse
mkAttachLoadBalancersResponse responseStatus
  = AttachLoadBalancersResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albrrsResponseStatus :: Lens.Lens' AttachLoadBalancersResponse Core.Int
albrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE albrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
