{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more scaling activities for the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeScalingActivities
  ( -- * Creating a request
    DescribeScalingActivities (..),
    mkDescribeScalingActivities,

    -- ** Request lenses
    desNextToken,
    desAutoScalingGroupName,
    desMaxRecords,
    desActivityIds,

    -- * Destructuring the response
    DescribeScalingActivitiesResponse (..),
    mkDescribeScalingActivitiesResponse,

    -- ** Response lenses
    dsasrsNextToken,
    dsasrsResponseStatus,
    dsasrsActivities,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScalingActivities' smart constructor.
data DescribeScalingActivities = DescribeScalingActivities'
  { nextToken ::
      Lude.Maybe Lude.Text,
    autoScalingGroupName ::
      Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    activityIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingActivities' with the minimum fields required to make a request.
--
-- * 'activityIds' - The activity IDs of the desired scaling activities. You can specify up to 50 IDs. If you omit this parameter, all activities for the past six weeks are described. If unknown activities are requested, they are ignored with no error. If you specify an Auto Scaling group, the results are limited to that group.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeScalingActivities ::
  DescribeScalingActivities
mkDescribeScalingActivities =
  DescribeScalingActivities'
    { nextToken = Lude.Nothing,
      autoScalingGroupName = Lude.Nothing,
      maxRecords = Lude.Nothing,
      activityIds = Lude.Nothing
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desNextToken :: Lens.Lens' DescribeScalingActivities (Lude.Maybe Lude.Text)
desNextToken = Lens.lens (nextToken :: DescribeScalingActivities -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingActivities)
{-# DEPRECATED desNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desAutoScalingGroupName :: Lens.Lens' DescribeScalingActivities (Lude.Maybe Lude.Text)
desAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DescribeScalingActivities -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DescribeScalingActivities)
{-# DEPRECATED desAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desMaxRecords :: Lens.Lens' DescribeScalingActivities (Lude.Maybe Lude.Int)
desMaxRecords = Lens.lens (maxRecords :: DescribeScalingActivities -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeScalingActivities)
{-# DEPRECATED desMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The activity IDs of the desired scaling activities. You can specify up to 50 IDs. If you omit this parameter, all activities for the past six weeks are described. If unknown activities are requested, they are ignored with no error. If you specify an Auto Scaling group, the results are limited to that group.
--
-- /Note:/ Consider using 'activityIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desActivityIds :: Lens.Lens' DescribeScalingActivities (Lude.Maybe [Lude.Text])
desActivityIds = Lens.lens (activityIds :: DescribeScalingActivities -> Lude.Maybe [Lude.Text]) (\s a -> s {activityIds = a} :: DescribeScalingActivities)
{-# DEPRECATED desActivityIds "Use generic-lens or generic-optics with 'activityIds' instead." #-}

instance Page.AWSPager DescribeScalingActivities where
  page rq rs
    | Page.stop (rs Lens.^. dsasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsasrsActivities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& desNextToken Lens..~ rs Lens.^. dsasrsNextToken

instance Lude.AWSRequest DescribeScalingActivities where
  type
    Rs DescribeScalingActivities =
      DescribeScalingActivitiesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeScalingActivitiesResult"
      ( \s h x ->
          DescribeScalingActivitiesResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "Activities" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders DescribeScalingActivities where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeScalingActivities where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScalingActivities where
  toQuery DescribeScalingActivities' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeScalingActivities" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "MaxRecords" Lude.=: maxRecords,
        "ActivityIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> activityIds)
      ]

-- | /See:/ 'mkDescribeScalingActivitiesResponse' smart constructor.
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int,
    activities ::
      [Activity]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingActivitiesResponse' with the minimum fields required to make a request.
--
-- * 'activities' - The scaling activities. Activities are sorted by start time. Activities still in progress are described first.
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'responseStatus' - The response status code.
mkDescribeScalingActivitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScalingActivitiesResponse
mkDescribeScalingActivitiesResponse pResponseStatus_ =
  DescribeScalingActivitiesResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      activities = Lude.mempty
    }

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasrsNextToken :: Lens.Lens' DescribeScalingActivitiesResponse (Lude.Maybe Lude.Text)
dsasrsNextToken = Lens.lens (nextToken :: DescribeScalingActivitiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingActivitiesResponse)
{-# DEPRECATED dsasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasrsResponseStatus :: Lens.Lens' DescribeScalingActivitiesResponse Lude.Int
dsasrsResponseStatus = Lens.lens (responseStatus :: DescribeScalingActivitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScalingActivitiesResponse)
{-# DEPRECATED dsasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The scaling activities. Activities are sorted by start time. Activities still in progress are described first.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasrsActivities :: Lens.Lens' DescribeScalingActivitiesResponse [Activity]
dsasrsActivities = Lens.lens (activities :: DescribeScalingActivitiesResponse -> [Activity]) (\s a -> s {activities = a} :: DescribeScalingActivitiesResponse)
{-# DEPRECATED dsasrsActivities "Use generic-lens or generic-optics with 'activities' instead." #-}
