{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Auto Scaling groups.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeAutoScalingGroups
  ( -- * Creating a request
    DescribeAutoScalingGroups (..),
    mkDescribeAutoScalingGroups,

    -- ** Request lenses
    dasgAutoScalingGroupNames,
    dasgNextToken,
    dasgMaxRecords,

    -- * Destructuring the response
    DescribeAutoScalingGroupsResponse (..),
    mkDescribeAutoScalingGroupsResponse,

    -- ** Response lenses
    dasgrsNextToken,
    dasgrsResponseStatus,
    dasgrsAutoScalingGroups,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAutoScalingGroups' smart constructor.
data DescribeAutoScalingGroups = DescribeAutoScalingGroups'
  { autoScalingGroupNames ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAutoScalingGroups' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupNames' - The names of the Auto Scaling groups. By default, you can only specify up to 50 names. You can optionally increase this limit using the @MaxRecords@ parameter.
--
-- If you omit this parameter, all Auto Scaling groups are described.
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeAutoScalingGroups ::
  DescribeAutoScalingGroups
mkDescribeAutoScalingGroups =
  DescribeAutoScalingGroups'
    { autoScalingGroupNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The names of the Auto Scaling groups. By default, you can only specify up to 50 names. You can optionally increase this limit using the @MaxRecords@ parameter.
--
-- If you omit this parameter, all Auto Scaling groups are described.
--
-- /Note:/ Consider using 'autoScalingGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgAutoScalingGroupNames :: Lens.Lens' DescribeAutoScalingGroups (Lude.Maybe [Lude.Text])
dasgAutoScalingGroupNames = Lens.lens (autoScalingGroupNames :: DescribeAutoScalingGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {autoScalingGroupNames = a} :: DescribeAutoScalingGroups)
{-# DEPRECATED dasgAutoScalingGroupNames "Use generic-lens or generic-optics with 'autoScalingGroupNames' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgNextToken :: Lens.Lens' DescribeAutoScalingGroups (Lude.Maybe Lude.Text)
dasgNextToken = Lens.lens (nextToken :: DescribeAutoScalingGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAutoScalingGroups)
{-# DEPRECATED dasgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgMaxRecords :: Lens.Lens' DescribeAutoScalingGroups (Lude.Maybe Lude.Int)
dasgMaxRecords = Lens.lens (maxRecords :: DescribeAutoScalingGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeAutoScalingGroups)
{-# DEPRECATED dasgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeAutoScalingGroups where
  page rq rs
    | Page.stop (rs Lens.^. dasgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dasgrsAutoScalingGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dasgNextToken Lens..~ rs Lens.^. dasgrsNextToken

instance Lude.AWSRequest DescribeAutoScalingGroups where
  type
    Rs DescribeAutoScalingGroups =
      DescribeAutoScalingGroupsResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeAutoScalingGroupsResult"
      ( \s h x ->
          DescribeAutoScalingGroupsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "AutoScalingGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders DescribeAutoScalingGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAutoScalingGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAutoScalingGroups where
  toQuery DescribeAutoScalingGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAutoScalingGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupNames"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> autoScalingGroupNames),
        "NextToken" Lude.=: nextToken,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeAutoScalingGroupsResponse' smart constructor.
data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int,
    autoScalingGroups ::
      [AutoScalingGroup]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAutoScalingGroupsResponse' with the minimum fields required to make a request.
--
-- * 'autoScalingGroups' - The groups.
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'responseStatus' - The response status code.
mkDescribeAutoScalingGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAutoScalingGroupsResponse
mkDescribeAutoScalingGroupsResponse pResponseStatus_ =
  DescribeAutoScalingGroupsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      autoScalingGroups = Lude.mempty
    }

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgrsNextToken :: Lens.Lens' DescribeAutoScalingGroupsResponse (Lude.Maybe Lude.Text)
dasgrsNextToken = Lens.lens (nextToken :: DescribeAutoScalingGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAutoScalingGroupsResponse)
{-# DEPRECATED dasgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgrsResponseStatus :: Lens.Lens' DescribeAutoScalingGroupsResponse Lude.Int
dasgrsResponseStatus = Lens.lens (responseStatus :: DescribeAutoScalingGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAutoScalingGroupsResponse)
{-# DEPRECATED dasgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The groups.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgrsAutoScalingGroups :: Lens.Lens' DescribeAutoScalingGroupsResponse [AutoScalingGroup]
dasgrsAutoScalingGroups = Lens.lens (autoScalingGroups :: DescribeAutoScalingGroupsResponse -> [AutoScalingGroup]) (\s a -> s {autoScalingGroups = a} :: DescribeAutoScalingGroupsResponse)
{-# DEPRECATED dasgrsAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}
