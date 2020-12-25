{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AttachLoadBalancerTargetGroups (..),
    mkAttachLoadBalancerTargetGroups,

    -- ** Request lenses
    albtgAutoScalingGroupName,
    albtgTargetGroupARNs,

    -- * Destructuring the response
    AttachLoadBalancerTargetGroupsResponse (..),
    mkAttachLoadBalancerTargetGroupsResponse,

    -- ** Response lenses
    albtgrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachLoadBalancerTargetGroups' smart constructor.
data AttachLoadBalancerTargetGroups = AttachLoadBalancerTargetGroups'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups. To get the ARN of a target group, use the Elastic Load Balancing <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups> API operation.
    targetGroupARNs :: [Types.XmlStringMaxLen511]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancerTargetGroups' value with any optional fields omitted.
mkAttachLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  AttachLoadBalancerTargetGroups
mkAttachLoadBalancerTargetGroups autoScalingGroupName =
  AttachLoadBalancerTargetGroups'
    { autoScalingGroupName,
      targetGroupARNs = Core.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtgAutoScalingGroupName :: Lens.Lens' AttachLoadBalancerTargetGroups Types.ResourceName
albtgAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED albtgAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups. To get the ARN of a target group, use the Elastic Load Balancing <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups> API operation.
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtgTargetGroupARNs :: Lens.Lens' AttachLoadBalancerTargetGroups [Types.XmlStringMaxLen511]
albtgTargetGroupARNs = Lens.field @"targetGroupARNs"
{-# DEPRECATED albtgTargetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead." #-}

instance Core.AWSRequest AttachLoadBalancerTargetGroups where
  type
    Rs AttachLoadBalancerTargetGroups =
      AttachLoadBalancerTargetGroupsResponse
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
            ( Core.pure ("Action", "AttachLoadBalancerTargetGroups")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "TargetGroupARNs"
                            (Core.toQueryList "member" targetGroupARNs)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "AttachLoadBalancerTargetGroupsResult"
      ( \s h x ->
          AttachLoadBalancerTargetGroupsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAttachLoadBalancerTargetGroupsResponse' smart constructor.
newtype AttachLoadBalancerTargetGroupsResponse = AttachLoadBalancerTargetGroupsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancerTargetGroupsResponse' value with any optional fields omitted.
mkAttachLoadBalancerTargetGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AttachLoadBalancerTargetGroupsResponse
mkAttachLoadBalancerTargetGroupsResponse responseStatus =
  AttachLoadBalancerTargetGroupsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtgrrsResponseStatus :: Lens.Lens' AttachLoadBalancerTargetGroupsResponse Core.Int
albtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED albtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
