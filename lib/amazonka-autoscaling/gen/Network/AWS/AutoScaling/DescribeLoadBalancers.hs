{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the load balancers for the specified Auto Scaling group.
--
-- This operation describes only Classic Load Balancers. If you have Application Load Balancers, Network Load Balancers, or Gateway Load Balancers, use the 'DescribeLoadBalancerTargetGroups' API instead.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeLoadBalancers
  ( -- * Creating a request
    DescribeLoadBalancers (..),
    mkDescribeLoadBalancers,

    -- ** Request lenses
    dlbNextToken,
    dlbAutoScalingGroupName,
    dlbMaxRecords,

    -- * Destructuring the response
    DescribeLoadBalancersResponse (..),
    mkDescribeLoadBalancersResponse,

    -- ** Response lenses
    dlbrsLoadBalancers,
    dlbrsNextToken,
    dlbrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text,
    -- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancers' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
mkDescribeLoadBalancers ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  DescribeLoadBalancers
mkDescribeLoadBalancers pAutoScalingGroupName_ =
  DescribeLoadBalancers'
    { nextToken = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_,
      maxRecords = Lude.Nothing
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbNextToken :: Lens.Lens' DescribeLoadBalancers (Lude.Maybe Lude.Text)
dlbNextToken = Lens.lens (nextToken :: DescribeLoadBalancers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbAutoScalingGroupName :: Lens.Lens' DescribeLoadBalancers Lude.Text
dlbAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DescribeLoadBalancers -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbMaxRecords :: Lens.Lens' DescribeLoadBalancers (Lude.Maybe Lude.Int)
dlbMaxRecords = Lens.lens (maxRecords :: DescribeLoadBalancers -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeLoadBalancers)
{-# DEPRECATED dlbMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeLoadBalancers where
  page rq rs
    | Page.stop (rs Lens.^. dlbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dlbrsLoadBalancers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlbNextToken Lens..~ rs Lens.^. dlbrsNextToken

instance Lude.AWSRequest DescribeLoadBalancers where
  type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeLoadBalancersResult"
      ( \s h x ->
          DescribeLoadBalancersResponse'
            Lude.<$> ( x Lude..@? "LoadBalancers" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLoadBalancers where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLoadBalancers where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLoadBalancers where
  toQuery DescribeLoadBalancers' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeLoadBalancers" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { -- | The load balancers.
    loadBalancers :: Lude.Maybe [LoadBalancerState],
    -- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancersResponse' with the minimum fields required to make a request.
--
-- * 'loadBalancers' - The load balancers.
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'responseStatus' - The response status code.
mkDescribeLoadBalancersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoadBalancersResponse
mkDescribeLoadBalancersResponse pResponseStatus_ =
  DescribeLoadBalancersResponse'
    { loadBalancers = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The load balancers.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrsLoadBalancers :: Lens.Lens' DescribeLoadBalancersResponse (Lude.Maybe [LoadBalancerState])
dlbrsLoadBalancers = Lens.lens (loadBalancers :: DescribeLoadBalancersResponse -> Lude.Maybe [LoadBalancerState]) (\s a -> s {loadBalancers = a} :: DescribeLoadBalancersResponse)
{-# DEPRECATED dlbrsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrsNextToken :: Lens.Lens' DescribeLoadBalancersResponse (Lude.Maybe Lude.Text)
dlbrsNextToken = Lens.lens (nextToken :: DescribeLoadBalancersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLoadBalancersResponse)
{-# DEPRECATED dlbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrsResponseStatus :: Lens.Lens' DescribeLoadBalancersResponse Lude.Int
dlbrsResponseStatus = Lens.lens (responseStatus :: DescribeLoadBalancersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoadBalancersResponse)
{-# DEPRECATED dlbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
