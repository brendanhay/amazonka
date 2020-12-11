{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCEndpointConnectionNotifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connection notifications for VPC endpoints and VPC endpoint services.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCEndpointConnectionNotifications
  ( -- * Creating a request
    DescribeVPCEndpointConnectionNotifications (..),
    mkDescribeVPCEndpointConnectionNotifications,

    -- ** Request lenses
    dvpcecnFilters,
    dvpcecnNextToken,
    dvpcecnConnectionNotificationId,
    dvpcecnDryRun,
    dvpcecnMaxResults,

    -- * Destructuring the response
    DescribeVPCEndpointConnectionNotificationsResponse (..),
    mkDescribeVPCEndpointConnectionNotificationsResponse,

    -- ** Response lenses
    dvpcecnrsConnectionNotificationSet,
    dvpcecnrsNextToken,
    dvpcecnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCEndpointConnectionNotifications' smart constructor.
data DescribeVPCEndpointConnectionNotifications = DescribeVPCEndpointConnectionNotifications'
  { filters ::
      Lude.Maybe
        [Filter],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    connectionNotificationId ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    maxResults ::
      Lude.Maybe
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

-- | Creates a value of 'DescribeVPCEndpointConnectionNotifications' with the minimum fields required to make a request.
--
-- * 'connectionNotificationId' - The ID of the notification.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @connection-notification-arn@ - The ARN of the SNS topic for the notification.
--
--
--     * @connection-notification-id@ - The ID of the notification.
--
--
--     * @connection-notification-state@ - The state of the notification (@Enabled@ | @Disabled@ ).
--
--
--     * @connection-notification-type@ - The type of notification (@Topic@ ).
--
--
--     * @service-id@ - The ID of the endpoint service.
--
--
--     * @vpc-endpoint-id@ - The ID of the VPC endpoint.
--
--
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value.
-- * 'nextToken' - The token to request the next page of results.
mkDescribeVPCEndpointConnectionNotifications ::
  DescribeVPCEndpointConnectionNotifications
mkDescribeVPCEndpointConnectionNotifications =
  DescribeVPCEndpointConnectionNotifications'
    { filters =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      connectionNotificationId = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @connection-notification-arn@ - The ARN of the SNS topic for the notification.
--
--
--     * @connection-notification-id@ - The ID of the notification.
--
--
--     * @connection-notification-state@ - The state of the notification (@Enabled@ | @Disabled@ ).
--
--
--     * @connection-notification-type@ - The type of notification (@Topic@ ).
--
--
--     * @service-id@ - The ID of the endpoint service.
--
--
--     * @vpc-endpoint-id@ - The ID of the VPC endpoint.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnFilters :: Lens.Lens' DescribeVPCEndpointConnectionNotifications (Lude.Maybe [Filter])
dvpcecnFilters = Lens.lens (filters :: DescribeVPCEndpointConnectionNotifications -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPCEndpointConnectionNotifications)
{-# DEPRECATED dvpcecnFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnNextToken :: Lens.Lens' DescribeVPCEndpointConnectionNotifications (Lude.Maybe Lude.Text)
dvpcecnNextToken = Lens.lens (nextToken :: DescribeVPCEndpointConnectionNotifications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointConnectionNotifications)
{-# DEPRECATED dvpcecnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the notification.
--
-- /Note:/ Consider using 'connectionNotificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnConnectionNotificationId :: Lens.Lens' DescribeVPCEndpointConnectionNotifications (Lude.Maybe Lude.Text)
dvpcecnConnectionNotificationId = Lens.lens (connectionNotificationId :: DescribeVPCEndpointConnectionNotifications -> Lude.Maybe Lude.Text) (\s a -> s {connectionNotificationId = a} :: DescribeVPCEndpointConnectionNotifications)
{-# DEPRECATED dvpcecnConnectionNotificationId "Use generic-lens or generic-optics with 'connectionNotificationId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnDryRun :: Lens.Lens' DescribeVPCEndpointConnectionNotifications (Lude.Maybe Lude.Bool)
dvpcecnDryRun = Lens.lens (dryRun :: DescribeVPCEndpointConnectionNotifications -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCEndpointConnectionNotifications)
{-# DEPRECATED dvpcecnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnMaxResults :: Lens.Lens' DescribeVPCEndpointConnectionNotifications (Lude.Maybe Lude.Int)
dvpcecnMaxResults = Lens.lens (maxResults :: DescribeVPCEndpointConnectionNotifications -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeVPCEndpointConnectionNotifications)
{-# DEPRECATED dvpcecnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVPCEndpointConnectionNotifications where
  page rq rs
    | Page.stop (rs Lens.^. dvpcecnrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvpcecnrsConnectionNotificationSet) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvpcecnNextToken Lens..~ rs Lens.^. dvpcecnrsNextToken

instance Lude.AWSRequest DescribeVPCEndpointConnectionNotifications where
  type
    Rs DescribeVPCEndpointConnectionNotifications =
      DescribeVPCEndpointConnectionNotificationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCEndpointConnectionNotificationsResponse'
            Lude.<$> ( x Lude..@? "connectionNotificationSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCEndpointConnectionNotifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCEndpointConnectionNotifications where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCEndpointConnectionNotifications where
  toQuery DescribeVPCEndpointConnectionNotifications' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeVpcEndpointConnectionNotifications" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "ConnectionNotificationId" Lude.=: connectionNotificationId,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVPCEndpointConnectionNotificationsResponse' smart constructor.
data DescribeVPCEndpointConnectionNotificationsResponse = DescribeVPCEndpointConnectionNotificationsResponse'
  { connectionNotificationSet ::
      Lude.Maybe
        [ConnectionNotification],
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeVPCEndpointConnectionNotificationsResponse' with the minimum fields required to make a request.
--
-- * 'connectionNotificationSet' - One or more notifications.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeVPCEndpointConnectionNotificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCEndpointConnectionNotificationsResponse
mkDescribeVPCEndpointConnectionNotificationsResponse
  pResponseStatus_ =
    DescribeVPCEndpointConnectionNotificationsResponse'
      { connectionNotificationSet =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | One or more notifications.
--
-- /Note:/ Consider using 'connectionNotificationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnrsConnectionNotificationSet :: Lens.Lens' DescribeVPCEndpointConnectionNotificationsResponse (Lude.Maybe [ConnectionNotification])
dvpcecnrsConnectionNotificationSet = Lens.lens (connectionNotificationSet :: DescribeVPCEndpointConnectionNotificationsResponse -> Lude.Maybe [ConnectionNotification]) (\s a -> s {connectionNotificationSet = a} :: DescribeVPCEndpointConnectionNotificationsResponse)
{-# DEPRECATED dvpcecnrsConnectionNotificationSet "Use generic-lens or generic-optics with 'connectionNotificationSet' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnrsNextToken :: Lens.Lens' DescribeVPCEndpointConnectionNotificationsResponse (Lude.Maybe Lude.Text)
dvpcecnrsNextToken = Lens.lens (nextToken :: DescribeVPCEndpointConnectionNotificationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointConnectionNotificationsResponse)
{-# DEPRECATED dvpcecnrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnrsResponseStatus :: Lens.Lens' DescribeVPCEndpointConnectionNotificationsResponse Lude.Int
dvpcecnrsResponseStatus = Lens.lens (responseStatus :: DescribeVPCEndpointConnectionNotificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCEndpointConnectionNotificationsResponse)
{-# DEPRECATED dvpcecnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
