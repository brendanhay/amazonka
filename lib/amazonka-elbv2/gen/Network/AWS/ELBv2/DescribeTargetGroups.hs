{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified target groups or all of your target groups. By default, all target groups are described. Alternatively, you can specify one of the following to filter the results: the ARN of the load balancer, the names of one or more target groups, or the ARNs of one or more target groups.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeTargetGroups
  ( -- * Creating a request
    DescribeTargetGroups (..),
    mkDescribeTargetGroups,

    -- ** Request lenses
    dtgTargetGroupARNs,
    dtgNames,
    dtgLoadBalancerARN,
    dtgMarker,
    dtgPageSize,

    -- * Destructuring the response
    DescribeTargetGroupsResponse (..),
    mkDescribeTargetGroupsResponse,

    -- ** Response lenses
    dtgrsNextMarker,
    dtgrsTargetGroups,
    dtgrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTargetGroups' smart constructor.
data DescribeTargetGroups = DescribeTargetGroups'
  { -- | The Amazon Resource Names (ARN) of the target groups.
    targetGroupARNs :: Lude.Maybe [Lude.Text],
    -- | The names of the target groups.
    names :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerARN :: Lude.Maybe Lude.Text,
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return with this call.
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTargetGroups' with the minimum fields required to make a request.
--
-- * 'targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups.
-- * 'names' - The names of the target groups.
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'pageSize' - The maximum number of results to return with this call.
mkDescribeTargetGroups ::
  DescribeTargetGroups
mkDescribeTargetGroups =
  DescribeTargetGroups'
    { targetGroupARNs = Lude.Nothing,
      names = Lude.Nothing,
      loadBalancerARN = Lude.Nothing,
      marker = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The Amazon Resource Names (ARN) of the target groups.
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgTargetGroupARNs :: Lens.Lens' DescribeTargetGroups (Lude.Maybe [Lude.Text])
dtgTargetGroupARNs = Lens.lens (targetGroupARNs :: DescribeTargetGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {targetGroupARNs = a} :: DescribeTargetGroups)
{-# DEPRECATED dtgTargetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead." #-}

-- | The names of the target groups.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgNames :: Lens.Lens' DescribeTargetGroups (Lude.Maybe [Lude.Text])
dtgNames = Lens.lens (names :: DescribeTargetGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: DescribeTargetGroups)
{-# DEPRECATED dtgNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgLoadBalancerARN :: Lens.Lens' DescribeTargetGroups (Lude.Maybe Lude.Text)
dtgLoadBalancerARN = Lens.lens (loadBalancerARN :: DescribeTargetGroups -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerARN = a} :: DescribeTargetGroups)
{-# DEPRECATED dtgLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgMarker :: Lens.Lens' DescribeTargetGroups (Lude.Maybe Lude.Text)
dtgMarker = Lens.lens (marker :: DescribeTargetGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTargetGroups)
{-# DEPRECATED dtgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgPageSize :: Lens.Lens' DescribeTargetGroups (Lude.Maybe Lude.Natural)
dtgPageSize = Lens.lens (pageSize :: DescribeTargetGroups -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeTargetGroups)
{-# DEPRECATED dtgPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager DescribeTargetGroups where
  page rq rs
    | Page.stop (rs Lens.^. dtgrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dtgrsTargetGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtgMarker Lens..~ rs Lens.^. dtgrsNextMarker

instance Lude.AWSRequest DescribeTargetGroups where
  type Rs DescribeTargetGroups = DescribeTargetGroupsResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeTargetGroupsResult"
      ( \s h x ->
          DescribeTargetGroupsResponse'
            Lude.<$> (x Lude..@? "NextMarker")
            Lude.<*> ( x Lude..@? "TargetGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTargetGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTargetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTargetGroups where
  toQuery DescribeTargetGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeTargetGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "TargetGroupArns"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> targetGroupARNs),
        "Names"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> names),
        "LoadBalancerArn" Lude.=: loadBalancerARN,
        "Marker" Lude.=: marker,
        "PageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkDescribeTargetGroupsResponse' smart constructor.
data DescribeTargetGroupsResponse = DescribeTargetGroupsResponse'
  { -- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | Information about the target groups.
    targetGroups :: Lude.Maybe [TargetGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTargetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
-- * 'targetGroups' - Information about the target groups.
-- * 'responseStatus' - The response status code.
mkDescribeTargetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTargetGroupsResponse
mkDescribeTargetGroupsResponse pResponseStatus_ =
  DescribeTargetGroupsResponse'
    { nextMarker = Lude.Nothing,
      targetGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsNextMarker :: Lens.Lens' DescribeTargetGroupsResponse (Lude.Maybe Lude.Text)
dtgrsNextMarker = Lens.lens (nextMarker :: DescribeTargetGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeTargetGroupsResponse)
{-# DEPRECATED dtgrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Information about the target groups.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsTargetGroups :: Lens.Lens' DescribeTargetGroupsResponse (Lude.Maybe [TargetGroup])
dtgrsTargetGroups = Lens.lens (targetGroups :: DescribeTargetGroupsResponse -> Lude.Maybe [TargetGroup]) (\s a -> s {targetGroups = a} :: DescribeTargetGroupsResponse)
{-# DEPRECATED dtgrsTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrsResponseStatus :: Lens.Lens' DescribeTargetGroupsResponse Lude.Int
dtgrsResponseStatus = Lens.lens (responseStatus :: DescribeTargetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTargetGroupsResponse)
{-# DEPRECATED dtgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
