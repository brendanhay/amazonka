{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeNotificationSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified notification subscriptions.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeNotificationSubscriptions
  ( -- * Creating a request
    DescribeNotificationSubscriptions (..),
    mkDescribeNotificationSubscriptions,

    -- ** Request lenses
    dnssMarker,
    dnssLimit,
    dnssOrganizationId,

    -- * Destructuring the response
    DescribeNotificationSubscriptionsResponse (..),
    mkDescribeNotificationSubscriptionsResponse,

    -- ** Response lenses
    dnsrsMarker,
    dnsrsSubscriptions,
    dnsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDescribeNotificationSubscriptions' smart constructor.
data DescribeNotificationSubscriptions = DescribeNotificationSubscriptions'
  { -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    limit :: Lude.Maybe Lude.Natural,
    -- | The ID of the organization.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotificationSubscriptions' with the minimum fields required to make a request.
--
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'limit' - The maximum number of items to return with this call.
-- * 'organizationId' - The ID of the organization.
mkDescribeNotificationSubscriptions ::
  -- | 'organizationId'
  Lude.Text ->
  DescribeNotificationSubscriptions
mkDescribeNotificationSubscriptions pOrganizationId_ =
  DescribeNotificationSubscriptions'
    { marker = Lude.Nothing,
      limit = Lude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnssMarker :: Lens.Lens' DescribeNotificationSubscriptions (Lude.Maybe Lude.Text)
dnssMarker = Lens.lens (marker :: DescribeNotificationSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeNotificationSubscriptions)
{-# DEPRECATED dnssMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnssLimit :: Lens.Lens' DescribeNotificationSubscriptions (Lude.Maybe Lude.Natural)
dnssLimit = Lens.lens (limit :: DescribeNotificationSubscriptions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeNotificationSubscriptions)
{-# DEPRECATED dnssLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnssOrganizationId :: Lens.Lens' DescribeNotificationSubscriptions Lude.Text
dnssOrganizationId = Lens.lens (organizationId :: DescribeNotificationSubscriptions -> Lude.Text) (\s a -> s {organizationId = a} :: DescribeNotificationSubscriptions)
{-# DEPRECATED dnssOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Page.AWSPager DescribeNotificationSubscriptions where
  page rq rs
    | Page.stop (rs Lens.^. dnsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dnsrsSubscriptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dnssMarker Lens..~ rs Lens.^. dnsrsMarker

instance Lude.AWSRequest DescribeNotificationSubscriptions where
  type
    Rs DescribeNotificationSubscriptions =
      DescribeNotificationSubscriptionsResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeNotificationSubscriptionsResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "Subscriptions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNotificationSubscriptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeNotificationSubscriptions where
  toPath DescribeNotificationSubscriptions' {..} =
    Lude.mconcat
      [ "/api/v1/organizations/",
        Lude.toBS organizationId,
        "/subscriptions"
      ]

instance Lude.ToQuery DescribeNotificationSubscriptions where
  toQuery DescribeNotificationSubscriptions' {..} =
    Lude.mconcat ["marker" Lude.=: marker, "limit" Lude.=: limit]

-- | /See:/ 'mkDescribeNotificationSubscriptionsResponse' smart constructor.
data DescribeNotificationSubscriptionsResponse = DescribeNotificationSubscriptionsResponse'
  { -- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
    marker :: Lude.Maybe Lude.Text,
    -- | The subscriptions.
    subscriptions :: Lude.Maybe [Subscription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNotificationSubscriptionsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
-- * 'subscriptions' - The subscriptions.
-- * 'responseStatus' - The response status code.
mkDescribeNotificationSubscriptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNotificationSubscriptionsResponse
mkDescribeNotificationSubscriptionsResponse pResponseStatus_ =
  DescribeNotificationSubscriptionsResponse'
    { marker = Lude.Nothing,
      subscriptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsrsMarker :: Lens.Lens' DescribeNotificationSubscriptionsResponse (Lude.Maybe Lude.Text)
dnsrsMarker = Lens.lens (marker :: DescribeNotificationSubscriptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeNotificationSubscriptionsResponse)
{-# DEPRECATED dnsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsrsSubscriptions :: Lens.Lens' DescribeNotificationSubscriptionsResponse (Lude.Maybe [Subscription])
dnsrsSubscriptions = Lens.lens (subscriptions :: DescribeNotificationSubscriptionsResponse -> Lude.Maybe [Subscription]) (\s a -> s {subscriptions = a} :: DescribeNotificationSubscriptionsResponse)
{-# DEPRECATED dnsrsSubscriptions "Use generic-lens or generic-optics with 'subscriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsrsResponseStatus :: Lens.Lens' DescribeNotificationSubscriptionsResponse Lude.Int
dnsrsResponseStatus = Lens.lens (responseStatus :: DescribeNotificationSubscriptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNotificationSubscriptionsResponse)
{-# DEPRECATED dnsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
