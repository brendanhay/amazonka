{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeMetricFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified metric filters. You can list all of the metric filters or filter the results by log name, prefix, metric name, or metric namespace. The results are ASCII-sorted by filter name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeMetricFilters
  ( -- * Creating a request
    DescribeMetricFilters (..),
    mkDescribeMetricFilters,

    -- ** Request lenses
    dmfFilterNamePrefix,
    dmfMetricName,
    dmfLogGroupName,
    dmfNextToken,
    dmfMetricNamespace,
    dmfLimit,

    -- * Destructuring the response
    DescribeMetricFiltersResponse (..),
    mkDescribeMetricFiltersResponse,

    -- ** Response lenses
    dmfrsNextToken,
    dmfrsMetricFilters,
    dmfrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMetricFilters' smart constructor.
data DescribeMetricFilters = DescribeMetricFilters'
  { filterNamePrefix ::
      Lude.Maybe Lude.Text,
    metricName :: Lude.Maybe Lude.Text,
    logGroupName :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    metricNamespace :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMetricFilters' with the minimum fields required to make a request.
--
-- * 'filterNamePrefix' - The prefix to match. CloudWatch Logs uses the value you set here only if you also include the @logGroupName@ parameter in your request.
-- * 'limit' - The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
-- * 'logGroupName' - The name of the log group.
-- * 'metricName' - Filters results to include only those with the specified metric name. If you include this parameter in your request, you must also include the @metricNamespace@ parameter.
-- * 'metricNamespace' - Filters results to include only those in the specified namespace. If you include this parameter in your request, you must also include the @metricName@ parameter.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeMetricFilters ::
  DescribeMetricFilters
mkDescribeMetricFilters =
  DescribeMetricFilters'
    { filterNamePrefix = Lude.Nothing,
      metricName = Lude.Nothing,
      logGroupName = Lude.Nothing,
      nextToken = Lude.Nothing,
      metricNamespace = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The prefix to match. CloudWatch Logs uses the value you set here only if you also include the @logGroupName@ parameter in your request.
--
-- /Note:/ Consider using 'filterNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfFilterNamePrefix :: Lens.Lens' DescribeMetricFilters (Lude.Maybe Lude.Text)
dmfFilterNamePrefix = Lens.lens (filterNamePrefix :: DescribeMetricFilters -> Lude.Maybe Lude.Text) (\s a -> s {filterNamePrefix = a} :: DescribeMetricFilters)
{-# DEPRECATED dmfFilterNamePrefix "Use generic-lens or generic-optics with 'filterNamePrefix' instead." #-}

-- | Filters results to include only those with the specified metric name. If you include this parameter in your request, you must also include the @metricNamespace@ parameter.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfMetricName :: Lens.Lens' DescribeMetricFilters (Lude.Maybe Lude.Text)
dmfMetricName = Lens.lens (metricName :: DescribeMetricFilters -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: DescribeMetricFilters)
{-# DEPRECATED dmfMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfLogGroupName :: Lens.Lens' DescribeMetricFilters (Lude.Maybe Lude.Text)
dmfLogGroupName = Lens.lens (logGroupName :: DescribeMetricFilters -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: DescribeMetricFilters)
{-# DEPRECATED dmfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfNextToken :: Lens.Lens' DescribeMetricFilters (Lude.Maybe Lude.Text)
dmfNextToken = Lens.lens (nextToken :: DescribeMetricFilters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMetricFilters)
{-# DEPRECATED dmfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters results to include only those in the specified namespace. If you include this parameter in your request, you must also include the @metricName@ parameter.
--
-- /Note:/ Consider using 'metricNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfMetricNamespace :: Lens.Lens' DescribeMetricFilters (Lude.Maybe Lude.Text)
dmfMetricNamespace = Lens.lens (metricNamespace :: DescribeMetricFilters -> Lude.Maybe Lude.Text) (\s a -> s {metricNamespace = a} :: DescribeMetricFilters)
{-# DEPRECATED dmfMetricNamespace "Use generic-lens or generic-optics with 'metricNamespace' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfLimit :: Lens.Lens' DescribeMetricFilters (Lude.Maybe Lude.Natural)
dmfLimit = Lens.lens (limit :: DescribeMetricFilters -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeMetricFilters)
{-# DEPRECATED dmfLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeMetricFilters where
  page rq rs
    | Page.stop (rs Lens.^. dmfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmfrsMetricFilters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmfNextToken Lens..~ rs Lens.^. dmfrsNextToken

instance Lude.AWSRequest DescribeMetricFilters where
  type Rs DescribeMetricFilters = DescribeMetricFiltersResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMetricFiltersResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "metricFilters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMetricFilters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DescribeMetricFilters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMetricFilters where
  toJSON DescribeMetricFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("filterNamePrefix" Lude..=) Lude.<$> filterNamePrefix,
            ("metricName" Lude..=) Lude.<$> metricName,
            ("logGroupName" Lude..=) Lude.<$> logGroupName,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("metricNamespace" Lude..=) Lude.<$> metricNamespace,
            ("limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeMetricFilters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMetricFilters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMetricFiltersResponse' smart constructor.
data DescribeMetricFiltersResponse = DescribeMetricFiltersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    metricFilters ::
      Lude.Maybe [MetricFilter],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMetricFiltersResponse' with the minimum fields required to make a request.
--
-- * 'metricFilters' - The metric filters.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeMetricFiltersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMetricFiltersResponse
mkDescribeMetricFiltersResponse pResponseStatus_ =
  DescribeMetricFiltersResponse'
    { nextToken = Lude.Nothing,
      metricFilters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsNextToken :: Lens.Lens' DescribeMetricFiltersResponse (Lude.Maybe Lude.Text)
dmfrsNextToken = Lens.lens (nextToken :: DescribeMetricFiltersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMetricFiltersResponse)
{-# DEPRECATED dmfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The metric filters.
--
-- /Note:/ Consider using 'metricFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsMetricFilters :: Lens.Lens' DescribeMetricFiltersResponse (Lude.Maybe [MetricFilter])
dmfrsMetricFilters = Lens.lens (metricFilters :: DescribeMetricFiltersResponse -> Lude.Maybe [MetricFilter]) (\s a -> s {metricFilters = a} :: DescribeMetricFiltersResponse)
{-# DEPRECATED dmfrsMetricFilters "Use generic-lens or generic-optics with 'metricFilters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmfrsResponseStatus :: Lens.Lens' DescribeMetricFiltersResponse Lude.Int
dmfrsResponseStatus = Lens.lens (responseStatus :: DescribeMetricFiltersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMetricFiltersResponse)
{-# DEPRECATED dmfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
