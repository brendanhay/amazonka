{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches one or more target groups from the specified Auto Scaling group.
module Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
  ( -- * Creating a request
    DetachLoadBalancerTargetGroups (..),
    mkDetachLoadBalancerTargetGroups,

    -- ** Request lenses
    dlbtgAutoScalingGroupName,
    dlbtgTargetGroupARNs,

    -- * Destructuring the response
    DetachLoadBalancerTargetGroupsResponse (..),
    mkDetachLoadBalancerTargetGroupsResponse,

    -- ** Response lenses
    dlbtgrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachLoadBalancerTargetGroups' smart constructor.
data DetachLoadBalancerTargetGroups = DetachLoadBalancerTargetGroups'
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

-- | Creates a value of 'DetachLoadBalancerTargetGroups' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
mkDetachLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  DetachLoadBalancerTargetGroups
mkDetachLoadBalancerTargetGroups pAutoScalingGroupName_ =
  DetachLoadBalancerTargetGroups'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      targetGroupARNs = Lude.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgAutoScalingGroupName :: Lens.Lens' DetachLoadBalancerTargetGroups Lude.Text
dlbtgAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DetachLoadBalancerTargetGroups -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DetachLoadBalancerTargetGroups)
{-# DEPRECATED dlbtgAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgTargetGroupARNs :: Lens.Lens' DetachLoadBalancerTargetGroups [Lude.Text]
dlbtgTargetGroupARNs = Lens.lens (targetGroupARNs :: DetachLoadBalancerTargetGroups -> [Lude.Text]) (\s a -> s {targetGroupARNs = a} :: DetachLoadBalancerTargetGroups)
{-# DEPRECATED dlbtgTargetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead." #-}

instance Lude.AWSRequest DetachLoadBalancerTargetGroups where
  type
    Rs DetachLoadBalancerTargetGroups =
      DetachLoadBalancerTargetGroupsResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DetachLoadBalancerTargetGroupsResult"
      ( \s h x ->
          DetachLoadBalancerTargetGroupsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachLoadBalancerTargetGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachLoadBalancerTargetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachLoadBalancerTargetGroups where
  toQuery DetachLoadBalancerTargetGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DetachLoadBalancerTargetGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "TargetGroupARNs"
          Lude.=: Lude.toQueryList "member" targetGroupARNs
      ]

-- | /See:/ 'mkDetachLoadBalancerTargetGroupsResponse' smart constructor.
newtype DetachLoadBalancerTargetGroupsResponse = DetachLoadBalancerTargetGroupsResponse'
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

-- | Creates a value of 'DetachLoadBalancerTargetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDetachLoadBalancerTargetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachLoadBalancerTargetGroupsResponse
mkDetachLoadBalancerTargetGroupsResponse pResponseStatus_ =
  DetachLoadBalancerTargetGroupsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgrsResponseStatus :: Lens.Lens' DetachLoadBalancerTargetGroupsResponse Lude.Int
dlbtgrsResponseStatus = Lens.lens (responseStatus :: DetachLoadBalancerTargetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachLoadBalancerTargetGroupsResponse)
{-# DEPRECATED dlbtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
