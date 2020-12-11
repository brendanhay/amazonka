{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCrawlerMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metrics about specified crawlers.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetCrawlerMetrics
  ( -- * Creating a request
    GetCrawlerMetrics (..),
    mkGetCrawlerMetrics,

    -- ** Request lenses
    gcmNextToken,
    gcmMaxResults,
    gcmCrawlerNameList,

    -- * Destructuring the response
    GetCrawlerMetricsResponse (..),
    mkGetCrawlerMetricsResponse,

    -- ** Response lenses
    gcmrsCrawlerMetricsList,
    gcmrsNextToken,
    gcmrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCrawlerMetrics' smart constructor.
data GetCrawlerMetrics = GetCrawlerMetrics'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    crawlerNameList :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCrawlerMetrics' with the minimum fields required to make a request.
--
-- * 'crawlerNameList' - A list of the names of crawlers about which to retrieve metrics.
-- * 'maxResults' - The maximum size of a list to return.
-- * 'nextToken' - A continuation token, if this is a continuation call.
mkGetCrawlerMetrics ::
  GetCrawlerMetrics
mkGetCrawlerMetrics =
  GetCrawlerMetrics'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      crawlerNameList = Lude.Nothing
    }

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmNextToken :: Lens.Lens' GetCrawlerMetrics (Lude.Maybe Lude.Text)
gcmNextToken = Lens.lens (nextToken :: GetCrawlerMetrics -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCrawlerMetrics)
{-# DEPRECATED gcmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmMaxResults :: Lens.Lens' GetCrawlerMetrics (Lude.Maybe Lude.Natural)
gcmMaxResults = Lens.lens (maxResults :: GetCrawlerMetrics -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetCrawlerMetrics)
{-# DEPRECATED gcmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A list of the names of crawlers about which to retrieve metrics.
--
-- /Note:/ Consider using 'crawlerNameList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmCrawlerNameList :: Lens.Lens' GetCrawlerMetrics (Lude.Maybe [Lude.Text])
gcmCrawlerNameList = Lens.lens (crawlerNameList :: GetCrawlerMetrics -> Lude.Maybe [Lude.Text]) (\s a -> s {crawlerNameList = a} :: GetCrawlerMetrics)
{-# DEPRECATED gcmCrawlerNameList "Use generic-lens or generic-optics with 'crawlerNameList' instead." #-}

instance Page.AWSPager GetCrawlerMetrics where
  page rq rs
    | Page.stop (rs Lens.^. gcmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gcmrsCrawlerMetricsList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcmNextToken Lens..~ rs Lens.^. gcmrsNextToken

instance Lude.AWSRequest GetCrawlerMetrics where
  type Rs GetCrawlerMetrics = GetCrawlerMetricsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCrawlerMetricsResponse'
            Lude.<$> (x Lude..?> "CrawlerMetricsList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCrawlerMetrics where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetCrawlerMetrics" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCrawlerMetrics where
  toJSON GetCrawlerMetrics' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("CrawlerNameList" Lude..=) Lude.<$> crawlerNameList
          ]
      )

instance Lude.ToPath GetCrawlerMetrics where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCrawlerMetrics where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCrawlerMetricsResponse' smart constructor.
data GetCrawlerMetricsResponse = GetCrawlerMetricsResponse'
  { crawlerMetricsList ::
      Lude.Maybe [CrawlerMetrics],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetCrawlerMetricsResponse' with the minimum fields required to make a request.
--
-- * 'crawlerMetricsList' - A list of metrics for the specified crawler.
-- * 'nextToken' - A continuation token, if the returned list does not contain the last metric available.
-- * 'responseStatus' - The response status code.
mkGetCrawlerMetricsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCrawlerMetricsResponse
mkGetCrawlerMetricsResponse pResponseStatus_ =
  GetCrawlerMetricsResponse'
    { crawlerMetricsList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of metrics for the specified crawler.
--
-- /Note:/ Consider using 'crawlerMetricsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsCrawlerMetricsList :: Lens.Lens' GetCrawlerMetricsResponse (Lude.Maybe [CrawlerMetrics])
gcmrsCrawlerMetricsList = Lens.lens (crawlerMetricsList :: GetCrawlerMetricsResponse -> Lude.Maybe [CrawlerMetrics]) (\s a -> s {crawlerMetricsList = a} :: GetCrawlerMetricsResponse)
{-# DEPRECATED gcmrsCrawlerMetricsList "Use generic-lens or generic-optics with 'crawlerMetricsList' instead." #-}

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsNextToken :: Lens.Lens' GetCrawlerMetricsResponse (Lude.Maybe Lude.Text)
gcmrsNextToken = Lens.lens (nextToken :: GetCrawlerMetricsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCrawlerMetricsResponse)
{-# DEPRECATED gcmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrsResponseStatus :: Lens.Lens' GetCrawlerMetricsResponse Lude.Int
gcmrsResponseStatus = Lens.lens (responseStatus :: GetCrawlerMetricsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCrawlerMetricsResponse)
{-# DEPRECATED gcmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
