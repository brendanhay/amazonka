{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeUsageReportSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more usage report subscriptions.
module Network.AWS.AppStream.DescribeUsageReportSubscriptions
  ( -- * Creating a request
    DescribeUsageReportSubscriptions (..),
    mkDescribeUsageReportSubscriptions,

    -- ** Request lenses
    dursNextToken,
    dursMaxResults,

    -- * Destructuring the response
    DescribeUsageReportSubscriptionsResponse (..),
    mkDescribeUsageReportSubscriptionsResponse,

    -- ** Response lenses
    dursrsUsageReportSubscriptions,
    dursrsNextToken,
    dursrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUsageReportSubscriptions' smart constructor.
data DescribeUsageReportSubscriptions = DescribeUsageReportSubscriptions'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum size of each page of results.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUsageReportSubscriptions' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
-- * 'maxResults' - The maximum size of each page of results.
mkDescribeUsageReportSubscriptions ::
  DescribeUsageReportSubscriptions
mkDescribeUsageReportSubscriptions =
  DescribeUsageReportSubscriptions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursNextToken :: Lens.Lens' DescribeUsageReportSubscriptions (Lude.Maybe Lude.Text)
dursNextToken = Lens.lens (nextToken :: DescribeUsageReportSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeUsageReportSubscriptions)
{-# DEPRECATED dursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursMaxResults :: Lens.Lens' DescribeUsageReportSubscriptions (Lude.Maybe Lude.Int)
dursMaxResults = Lens.lens (maxResults :: DescribeUsageReportSubscriptions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeUsageReportSubscriptions)
{-# DEPRECATED dursMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeUsageReportSubscriptions where
  type
    Rs DescribeUsageReportSubscriptions =
      DescribeUsageReportSubscriptionsResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUsageReportSubscriptionsResponse'
            Lude.<$> (x Lude..?> "UsageReportSubscriptions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUsageReportSubscriptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.DescribeUsageReportSubscriptions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUsageReportSubscriptions where
  toJSON DescribeUsageReportSubscriptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeUsageReportSubscriptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUsageReportSubscriptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUsageReportSubscriptionsResponse' smart constructor.
data DescribeUsageReportSubscriptionsResponse = DescribeUsageReportSubscriptionsResponse'
  { -- | Information about the usage report subscription.
    usageReportSubscriptions :: Lude.Maybe [UsageReportSubscription],
    -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUsageReportSubscriptionsResponse' with the minimum fields required to make a request.
--
-- * 'usageReportSubscriptions' - Information about the usage report subscription.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'responseStatus' - The response status code.
mkDescribeUsageReportSubscriptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUsageReportSubscriptionsResponse
mkDescribeUsageReportSubscriptionsResponse pResponseStatus_ =
  DescribeUsageReportSubscriptionsResponse'
    { usageReportSubscriptions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the usage report subscription.
--
-- /Note:/ Consider using 'usageReportSubscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursrsUsageReportSubscriptions :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Lude.Maybe [UsageReportSubscription])
dursrsUsageReportSubscriptions = Lens.lens (usageReportSubscriptions :: DescribeUsageReportSubscriptionsResponse -> Lude.Maybe [UsageReportSubscription]) (\s a -> s {usageReportSubscriptions = a} :: DescribeUsageReportSubscriptionsResponse)
{-# DEPRECATED dursrsUsageReportSubscriptions "Use generic-lens or generic-optics with 'usageReportSubscriptions' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursrsNextToken :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Lude.Maybe Lude.Text)
dursrsNextToken = Lens.lens (nextToken :: DescribeUsageReportSubscriptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeUsageReportSubscriptionsResponse)
{-# DEPRECATED dursrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursrsResponseStatus :: Lens.Lens' DescribeUsageReportSubscriptionsResponse Lude.Int
dursrsResponseStatus = Lens.lens (responseStatus :: DescribeUsageReportSubscriptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUsageReportSubscriptionsResponse)
{-# DEPRECATED dursrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
