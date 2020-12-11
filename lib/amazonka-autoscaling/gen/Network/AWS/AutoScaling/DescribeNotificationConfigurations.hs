{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification actions associated with the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeNotificationConfigurations
  ( -- * Creating a request
    DescribeNotificationConfigurations (..),
    mkDescribeNotificationConfigurations,

    -- ** Request lenses
    dncAutoScalingGroupNames,
    dncNextToken,
    dncMaxRecords,

    -- * Destructuring the response
    DescribeNotificationConfigurationsResponse (..),
    mkDescribeNotificationConfigurationsResponse,

    -- ** Response lenses
    dncrsNextToken,
    dncrsResponseStatus,
    dncrsNotificationConfigurations,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeNotificationConfigurations' smart constructor.
data DescribeNotificationConfigurations = DescribeNotificationConfigurations'
  { autoScalingGroupNames ::
      Lude.Maybe
        [Lude.Text],
    nextToken ::
      Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotificationConfigurations' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupNames' - The name of the Auto Scaling group.
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeNotificationConfigurations ::
  DescribeNotificationConfigurations
mkDescribeNotificationConfigurations =
  DescribeNotificationConfigurations'
    { autoScalingGroupNames =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncAutoScalingGroupNames :: Lens.Lens' DescribeNotificationConfigurations (Lude.Maybe [Lude.Text])
dncAutoScalingGroupNames = Lens.lens (autoScalingGroupNames :: DescribeNotificationConfigurations -> Lude.Maybe [Lude.Text]) (\s a -> s {autoScalingGroupNames = a} :: DescribeNotificationConfigurations)
{-# DEPRECATED dncAutoScalingGroupNames "Use generic-lens or generic-optics with 'autoScalingGroupNames' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncNextToken :: Lens.Lens' DescribeNotificationConfigurations (Lude.Maybe Lude.Text)
dncNextToken = Lens.lens (nextToken :: DescribeNotificationConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNotificationConfigurations)
{-# DEPRECATED dncNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncMaxRecords :: Lens.Lens' DescribeNotificationConfigurations (Lude.Maybe Lude.Int)
dncMaxRecords = Lens.lens (maxRecords :: DescribeNotificationConfigurations -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeNotificationConfigurations)
{-# DEPRECATED dncMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeNotificationConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. dncrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dncrsNotificationConfigurations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dncNextToken Lens..~ rs Lens.^. dncrsNextToken

instance Lude.AWSRequest DescribeNotificationConfigurations where
  type
    Rs DescribeNotificationConfigurations =
      DescribeNotificationConfigurationsResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeNotificationConfigurationsResult"
      ( \s h x ->
          DescribeNotificationConfigurationsResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "NotificationConfigurations" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders DescribeNotificationConfigurations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeNotificationConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNotificationConfigurations where
  toQuery DescribeNotificationConfigurations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeNotificationConfigurations" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupNames"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> autoScalingGroupNames),
        "NextToken" Lude.=: nextToken,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeNotificationConfigurationsResponse' smart constructor.
data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    notificationConfigurations ::
      [NotificationConfiguration]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotificationConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'notificationConfigurations' - The notification configurations.
-- * 'responseStatus' - The response status code.
mkDescribeNotificationConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNotificationConfigurationsResponse
mkDescribeNotificationConfigurationsResponse pResponseStatus_ =
  DescribeNotificationConfigurationsResponse'
    { nextToken =
        Lude.Nothing,
      responseStatus = pResponseStatus_,
      notificationConfigurations = Lude.mempty
    }

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncrsNextToken :: Lens.Lens' DescribeNotificationConfigurationsResponse (Lude.Maybe Lude.Text)
dncrsNextToken = Lens.lens (nextToken :: DescribeNotificationConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNotificationConfigurationsResponse)
{-# DEPRECATED dncrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncrsResponseStatus :: Lens.Lens' DescribeNotificationConfigurationsResponse Lude.Int
dncrsResponseStatus = Lens.lens (responseStatus :: DescribeNotificationConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNotificationConfigurationsResponse)
{-# DEPRECATED dncrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The notification configurations.
--
-- /Note:/ Consider using 'notificationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncrsNotificationConfigurations :: Lens.Lens' DescribeNotificationConfigurationsResponse [NotificationConfiguration]
dncrsNotificationConfigurations = Lens.lens (notificationConfigurations :: DescribeNotificationConfigurationsResponse -> [NotificationConfiguration]) (\s a -> s {notificationConfigurations = a} :: DescribeNotificationConfigurationsResponse)
{-# DEPRECATED dncrsNotificationConfigurations "Use generic-lens or generic-optics with 'notificationConfigurations' instead." #-}
