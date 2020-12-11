{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    albtgrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachLoadBalancerTargetGroups' smart constructor.
data AttachLoadBalancerTargetGroups = AttachLoadBalancerTargetGroups'
  { autoScalingGroupName ::
      Lude.Text,
    targetGroupARNs ::
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

-- | Creates a value of 'AttachLoadBalancerTargetGroups' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups. To get the ARN of a target group, use the Elastic Load Balancing <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups> API operation.
mkAttachLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  AttachLoadBalancerTargetGroups
mkAttachLoadBalancerTargetGroups pAutoScalingGroupName_ =
  AttachLoadBalancerTargetGroups'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      targetGroupARNs = Lude.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtgAutoScalingGroupName :: Lens.Lens' AttachLoadBalancerTargetGroups Lude.Text
albtgAutoScalingGroupName = Lens.lens (autoScalingGroupName :: AttachLoadBalancerTargetGroups -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: AttachLoadBalancerTargetGroups)
{-# DEPRECATED albtgAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups. To get the ARN of a target group, use the Elastic Load Balancing <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups> API operation.
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtgTargetGroupARNs :: Lens.Lens' AttachLoadBalancerTargetGroups [Lude.Text]
albtgTargetGroupARNs = Lens.lens (targetGroupARNs :: AttachLoadBalancerTargetGroups -> [Lude.Text]) (\s a -> s {targetGroupARNs = a} :: AttachLoadBalancerTargetGroups)
{-# DEPRECATED albtgTargetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead." #-}

instance Lude.AWSRequest AttachLoadBalancerTargetGroups where
  type
    Rs AttachLoadBalancerTargetGroups =
      AttachLoadBalancerTargetGroupsResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "AttachLoadBalancerTargetGroupsResult"
      ( \s h x ->
          AttachLoadBalancerTargetGroupsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachLoadBalancerTargetGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachLoadBalancerTargetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachLoadBalancerTargetGroups where
  toQuery AttachLoadBalancerTargetGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AttachLoadBalancerTargetGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "TargetGroupARNs"
          Lude.=: Lude.toQueryList "member" targetGroupARNs
      ]

-- | /See:/ 'mkAttachLoadBalancerTargetGroupsResponse' smart constructor.
newtype AttachLoadBalancerTargetGroupsResponse = AttachLoadBalancerTargetGroupsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachLoadBalancerTargetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAttachLoadBalancerTargetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachLoadBalancerTargetGroupsResponse
mkAttachLoadBalancerTargetGroupsResponse pResponseStatus_ =
  AttachLoadBalancerTargetGroupsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtgrsResponseStatus :: Lens.Lens' AttachLoadBalancerTargetGroupsResponse Lude.Int
albtgrsResponseStatus = Lens.lens (responseStatus :: AttachLoadBalancerTargetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachLoadBalancerTargetGroupsResponse)
{-# DEPRECATED albtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
