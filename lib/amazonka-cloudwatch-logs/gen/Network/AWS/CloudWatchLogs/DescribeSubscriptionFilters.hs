{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the subscription filters for the specified log group. You can list all the subscription filters or filter the results by prefix. The results are ASCII-sorted by filter name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
  ( -- * Creating a request
    DescribeSubscriptionFilters (..),
    mkDescribeSubscriptionFilters,

    -- ** Request lenses
    dsfFilterNamePrefix,
    dsfNextToken,
    dsfLimit,
    dsfLogGroupName,

    -- * Destructuring the response
    DescribeSubscriptionFiltersResponse (..),
    mkDescribeSubscriptionFiltersResponse,

    -- ** Response lenses
    dsfrsSubscriptionFilters,
    dsfrsNextToken,
    dsfrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSubscriptionFilters' smart constructor.
data DescribeSubscriptionFilters = DescribeSubscriptionFilters'
  { filterNamePrefix ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    logGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSubscriptionFilters' with the minimum fields required to make a request.
--
-- * 'filterNamePrefix' - The prefix to match. If you don't specify a value, no prefix filter is applied.
-- * 'limit' - The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
-- * 'logGroupName' - The name of the log group.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeSubscriptionFilters ::
  -- | 'logGroupName'
  Lude.Text ->
  DescribeSubscriptionFilters
mkDescribeSubscriptionFilters pLogGroupName_ =
  DescribeSubscriptionFilters'
    { filterNamePrefix = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      logGroupName = pLogGroupName_
    }

-- | The prefix to match. If you don't specify a value, no prefix filter is applied.
--
-- /Note:/ Consider using 'filterNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfFilterNamePrefix :: Lens.Lens' DescribeSubscriptionFilters (Lude.Maybe Lude.Text)
dsfFilterNamePrefix = Lens.lens (filterNamePrefix :: DescribeSubscriptionFilters -> Lude.Maybe Lude.Text) (\s a -> s {filterNamePrefix = a} :: DescribeSubscriptionFilters)
{-# DEPRECATED dsfFilterNamePrefix "Use generic-lens or generic-optics with 'filterNamePrefix' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfNextToken :: Lens.Lens' DescribeSubscriptionFilters (Lude.Maybe Lude.Text)
dsfNextToken = Lens.lens (nextToken :: DescribeSubscriptionFilters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSubscriptionFilters)
{-# DEPRECATED dsfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfLimit :: Lens.Lens' DescribeSubscriptionFilters (Lude.Maybe Lude.Natural)
dsfLimit = Lens.lens (limit :: DescribeSubscriptionFilters -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeSubscriptionFilters)
{-# DEPRECATED dsfLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfLogGroupName :: Lens.Lens' DescribeSubscriptionFilters Lude.Text
dsfLogGroupName = Lens.lens (logGroupName :: DescribeSubscriptionFilters -> Lude.Text) (\s a -> s {logGroupName = a} :: DescribeSubscriptionFilters)
{-# DEPRECATED dsfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Page.AWSPager DescribeSubscriptionFilters where
  page rq rs
    | Page.stop (rs Lens.^. dsfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsfrsSubscriptionFilters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsfNextToken Lens..~ rs Lens.^. dsfrsNextToken

instance Lude.AWSRequest DescribeSubscriptionFilters where
  type
    Rs DescribeSubscriptionFilters =
      DescribeSubscriptionFiltersResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSubscriptionFiltersResponse'
            Lude.<$> (x Lude..?> "subscriptionFilters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSubscriptionFilters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DescribeSubscriptionFilters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSubscriptionFilters where
  toJSON DescribeSubscriptionFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("filterNamePrefix" Lude..=) Lude.<$> filterNamePrefix,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("limit" Lude..=) Lude.<$> limit,
            Lude.Just ("logGroupName" Lude..= logGroupName)
          ]
      )

instance Lude.ToPath DescribeSubscriptionFilters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSubscriptionFilters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSubscriptionFiltersResponse' smart constructor.
data DescribeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse'
  { subscriptionFilters ::
      Lude.Maybe
        [SubscriptionFilter],
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

-- | Creates a value of 'DescribeSubscriptionFiltersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'subscriptionFilters' - The subscription filters.
mkDescribeSubscriptionFiltersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSubscriptionFiltersResponse
mkDescribeSubscriptionFiltersResponse pResponseStatus_ =
  DescribeSubscriptionFiltersResponse'
    { subscriptionFilters =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The subscription filters.
--
-- /Note:/ Consider using 'subscriptionFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsSubscriptionFilters :: Lens.Lens' DescribeSubscriptionFiltersResponse (Lude.Maybe [SubscriptionFilter])
dsfrsSubscriptionFilters = Lens.lens (subscriptionFilters :: DescribeSubscriptionFiltersResponse -> Lude.Maybe [SubscriptionFilter]) (\s a -> s {subscriptionFilters = a} :: DescribeSubscriptionFiltersResponse)
{-# DEPRECATED dsfrsSubscriptionFilters "Use generic-lens or generic-optics with 'subscriptionFilters' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsNextToken :: Lens.Lens' DescribeSubscriptionFiltersResponse (Lude.Maybe Lude.Text)
dsfrsNextToken = Lens.lens (nextToken :: DescribeSubscriptionFiltersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSubscriptionFiltersResponse)
{-# DEPRECATED dsfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsResponseStatus :: Lens.Lens' DescribeSubscriptionFiltersResponse Lude.Int
dsfrsResponseStatus = Lens.lens (responseStatus :: DescribeSubscriptionFiltersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSubscriptionFiltersResponse)
{-# DEPRECATED dsfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
