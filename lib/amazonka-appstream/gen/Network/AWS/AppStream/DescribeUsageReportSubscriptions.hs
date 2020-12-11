{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    durssNextToken,
    durssMaxResults,

    -- * Destructuring the response
    DescribeUsageReportSubscriptionsResponse (..),
    mkDescribeUsageReportSubscriptionsResponse,

    -- ** Response lenses
    durssrsUsageReportSubscriptions,
    durssrsNextToken,
    durssrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUsageReportSubscriptions' smart constructor.
data DescribeUsageReportSubscriptions = DescribeUsageReportSubscriptions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
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

-- | Creates a value of 'DescribeUsageReportSubscriptions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum size of each page of results.
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
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
durssNextToken :: Lens.Lens' DescribeUsageReportSubscriptions (Lude.Maybe Lude.Text)
durssNextToken = Lens.lens (nextToken :: DescribeUsageReportSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeUsageReportSubscriptions)
{-# DEPRECATED durssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of each page of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durssMaxResults :: Lens.Lens' DescribeUsageReportSubscriptions (Lude.Maybe Lude.Int)
durssMaxResults = Lens.lens (maxResults :: DescribeUsageReportSubscriptions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeUsageReportSubscriptions)
{-# DEPRECATED durssMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

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
  { usageReportSubscriptions ::
      Lude.Maybe
        [UsageReportSubscription],
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

-- | Creates a value of 'DescribeUsageReportSubscriptionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
-- * 'responseStatus' - The response status code.
-- * 'usageReportSubscriptions' - Information about the usage report subscription.
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
durssrsUsageReportSubscriptions :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Lude.Maybe [UsageReportSubscription])
durssrsUsageReportSubscriptions = Lens.lens (usageReportSubscriptions :: DescribeUsageReportSubscriptionsResponse -> Lude.Maybe [UsageReportSubscription]) (\s a -> s {usageReportSubscriptions = a} :: DescribeUsageReportSubscriptionsResponse)
{-# DEPRECATED durssrsUsageReportSubscriptions "Use generic-lens or generic-optics with 'usageReportSubscriptions' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durssrsNextToken :: Lens.Lens' DescribeUsageReportSubscriptionsResponse (Lude.Maybe Lude.Text)
durssrsNextToken = Lens.lens (nextToken :: DescribeUsageReportSubscriptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeUsageReportSubscriptionsResponse)
{-# DEPRECATED durssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durssrsResponseStatus :: Lens.Lens' DescribeUsageReportSubscriptionsResponse Lude.Int
durssrsResponseStatus = Lens.lens (responseStatus :: DescribeUsageReportSubscriptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUsageReportSubscriptionsResponse)
{-# DEPRECATED durssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
