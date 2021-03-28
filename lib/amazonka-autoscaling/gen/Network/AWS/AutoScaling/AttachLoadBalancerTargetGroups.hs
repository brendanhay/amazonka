{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more target groups to the specified Auto Scaling group.
--
-- This operation is used with the following load balancer types: 
--
--     * Application Load Balancer - Operates at the application layer (layer 7) and supports HTTP and HTTPS. 
--
--
--     * Network Load Balancer - Operates at the transport layer (layer 4) and supports TCP, TLS, and UDP. 
--
--
--     * Gateway Load Balancer - Operates at the network layer (layer 3).
--
--
-- To describe the target groups for an Auto Scaling group, call the 'DescribeLoadBalancerTargetGroups' API. To detach the target group from the Auto Scaling group, call the 'DetachLoadBalancerTargetGroups' API.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ . 
module Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups
    (
    -- * Creating a request
      AttachLoadBalancerTargetGroups (..)
    , mkAttachLoadBalancerTargetGroups
    -- ** Request lenses
    , albtgAutoScalingGroupName
    , albtgTargetGroupARNs

    -- * Destructuring the response
    , AttachLoadBalancerTargetGroupsResponse (..)
    , mkAttachLoadBalancerTargetGroupsResponse
    -- ** Response lenses
    , albtgrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachLoadBalancerTargetGroups' smart constructor.
data AttachLoadBalancerTargetGroups = AttachLoadBalancerTargetGroups'
  { autoScalingGroupName :: Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , targetGroupARNs :: [Types.XmlStringMaxLen511]
    -- ^ The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups. To get the ARN of a target group, use the Elastic Load Balancing <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups> API operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancerTargetGroups' value with any optional fields omitted.
mkAttachLoadBalancerTargetGroups
    :: Types.ResourceName -- ^ 'autoScalingGroupName'
    -> AttachLoadBalancerTargetGroups
mkAttachLoadBalancerTargetGroups autoScalingGroupName
  = AttachLoadBalancerTargetGroups'{autoScalingGroupName,
                                    targetGroupARNs = Core.mempty}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtgAutoScalingGroupName :: Lens.Lens' AttachLoadBalancerTargetGroups Types.ResourceName
albtgAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE albtgAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups. To get the ARN of a target group, use the Elastic Load Balancing <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups> API operation.
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtgTargetGroupARNs :: Lens.Lens' AttachLoadBalancerTargetGroups [Types.XmlStringMaxLen511]
albtgTargetGroupARNs = Lens.field @"targetGroupARNs"
{-# INLINEABLE albtgTargetGroupARNs #-}
{-# DEPRECATED targetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead"  #-}

instance Core.ToQuery AttachLoadBalancerTargetGroups where
        toQuery AttachLoadBalancerTargetGroups{..}
          = Core.toQueryPair "Action"
              ("AttachLoadBalancerTargetGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "TargetGroupARNs"
                (Core.toQueryList "member" targetGroupARNs)

instance Core.ToHeaders AttachLoadBalancerTargetGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AttachLoadBalancerTargetGroups where
        type Rs AttachLoadBalancerTargetGroups =
             AttachLoadBalancerTargetGroupsResponse
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
          = Response.receiveXMLWrapper "AttachLoadBalancerTargetGroupsResult"
              (\ s h x ->
                 AttachLoadBalancerTargetGroupsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachLoadBalancerTargetGroupsResponse' smart constructor.
newtype AttachLoadBalancerTargetGroupsResponse = AttachLoadBalancerTargetGroupsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancerTargetGroupsResponse' value with any optional fields omitted.
mkAttachLoadBalancerTargetGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AttachLoadBalancerTargetGroupsResponse
mkAttachLoadBalancerTargetGroupsResponse responseStatus
  = AttachLoadBalancerTargetGroupsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtgrrsResponseStatus :: Lens.Lens' AttachLoadBalancerTargetGroupsResponse Core.Int
albtgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE albtgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
