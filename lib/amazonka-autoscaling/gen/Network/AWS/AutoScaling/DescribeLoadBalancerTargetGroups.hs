{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the target groups for the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
  ( -- * Creating a request
    DescribeLoadBalancerTargetGroups (..),
    mkDescribeLoadBalancerTargetGroups,

    -- ** Request lenses
    dlbtgsNextToken,
    dlbtgsMaxRecords,
    dlbtgsAutoScalingGroupName,

    -- * Destructuring the response
    DescribeLoadBalancerTargetGroupsResponse (..),
    mkDescribeLoadBalancerTargetGroupsResponse,

    -- ** Response lenses
    dlbtgsrsLoadBalancerTargetGroups,
    dlbtgsrsNextToken,
    dlbtgsrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLoadBalancerTargetGroups' smart constructor.
data DescribeLoadBalancerTargetGroups = DescribeLoadBalancerTargetGroups'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int,
    autoScalingGroupName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancerTargetGroups' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  DescribeLoadBalancerTargetGroups
mkDescribeLoadBalancerTargetGroups pAutoScalingGroupName_ =
  DescribeLoadBalancerTargetGroups'
    { nextToken = Lude.Nothing,
      maxRecords = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgsNextToken :: Lens.Lens' DescribeLoadBalancerTargetGroups (Lude.Maybe Lude.Text)
dlbtgsNextToken = Lens.lens (nextToken :: DescribeLoadBalancerTargetGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLoadBalancerTargetGroups)
{-# DEPRECATED dlbtgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgsMaxRecords :: Lens.Lens' DescribeLoadBalancerTargetGroups (Lude.Maybe Lude.Int)
dlbtgsMaxRecords = Lens.lens (maxRecords :: DescribeLoadBalancerTargetGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeLoadBalancerTargetGroups)
{-# DEPRECATED dlbtgsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgsAutoScalingGroupName :: Lens.Lens' DescribeLoadBalancerTargetGroups Lude.Text
dlbtgsAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DescribeLoadBalancerTargetGroups -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DescribeLoadBalancerTargetGroups)
{-# DEPRECATED dlbtgsAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Page.AWSPager DescribeLoadBalancerTargetGroups where
  page rq rs
    | Page.stop (rs Lens.^. dlbtgsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dlbtgsrsLoadBalancerTargetGroups) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlbtgsNextToken Lens..~ rs Lens.^. dlbtgsrsNextToken

instance Lude.AWSRequest DescribeLoadBalancerTargetGroups where
  type
    Rs DescribeLoadBalancerTargetGroups =
      DescribeLoadBalancerTargetGroupsResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeLoadBalancerTargetGroupsResult"
      ( \s h x ->
          DescribeLoadBalancerTargetGroupsResponse'
            Lude.<$> ( x Lude..@? "LoadBalancerTargetGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLoadBalancerTargetGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLoadBalancerTargetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLoadBalancerTargetGroups where
  toQuery DescribeLoadBalancerTargetGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeLoadBalancerTargetGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "MaxRecords" Lude.=: maxRecords,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkDescribeLoadBalancerTargetGroupsResponse' smart constructor.
data DescribeLoadBalancerTargetGroupsResponse = DescribeLoadBalancerTargetGroupsResponse'
  { loadBalancerTargetGroups ::
      Lude.Maybe
        [LoadBalancerTargetGroupState],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancerTargetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'loadBalancerTargetGroups' - Information about the target groups.
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'responseStatus' - The response status code.
mkDescribeLoadBalancerTargetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoadBalancerTargetGroupsResponse
mkDescribeLoadBalancerTargetGroupsResponse pResponseStatus_ =
  DescribeLoadBalancerTargetGroupsResponse'
    { loadBalancerTargetGroups =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the target groups.
--
-- /Note:/ Consider using 'loadBalancerTargetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgsrsLoadBalancerTargetGroups :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse (Lude.Maybe [LoadBalancerTargetGroupState])
dlbtgsrsLoadBalancerTargetGroups = Lens.lens (loadBalancerTargetGroups :: DescribeLoadBalancerTargetGroupsResponse -> Lude.Maybe [LoadBalancerTargetGroupState]) (\s a -> s {loadBalancerTargetGroups = a} :: DescribeLoadBalancerTargetGroupsResponse)
{-# DEPRECATED dlbtgsrsLoadBalancerTargetGroups "Use generic-lens or generic-optics with 'loadBalancerTargetGroups' instead." #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgsrsNextToken :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse (Lude.Maybe Lude.Text)
dlbtgsrsNextToken = Lens.lens (nextToken :: DescribeLoadBalancerTargetGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLoadBalancerTargetGroupsResponse)
{-# DEPRECATED dlbtgsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtgsrsResponseStatus :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse Lude.Int
dlbtgsrsResponseStatus = Lens.lens (responseStatus :: DescribeLoadBalancerTargetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoadBalancerTargetGroupsResponse)
{-# DEPRECATED dlbtgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
