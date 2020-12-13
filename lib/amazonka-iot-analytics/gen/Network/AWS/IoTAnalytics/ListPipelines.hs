{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.ListPipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of pipelines.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListPipelines
  ( -- * Creating a request
    ListPipelines (..),
    mkListPipelines,

    -- ** Request lenses
    lpNextToken,
    lpMaxResults,

    -- * Destructuring the response
    ListPipelinesResponse (..),
    mkListPipelinesResponse,

    -- ** Response lenses
    lprsPipelineSummaries,
    lprsNextToken,
    lprsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPipelines' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
mkListPipelines ::
  ListPipelines
mkListPipelines =
  ListPipelines'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPipelines (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListPipelines -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPipelines)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListPipelines (Lude.Maybe Lude.Natural)
lpMaxResults = Lens.lens (maxResults :: ListPipelines -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPipelines)
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListPipelines where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPipelineSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest ListPipelines where
  type Rs ListPipelines = ListPipelinesResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Lude.<$> (x Lude..?> "pipelineSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPipelines where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPipelines where
  toPath = Lude.const "/pipelines"

instance Lude.ToQuery ListPipelines where
  toQuery ListPipelines' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | A list of @PipelineSummary@ objects.
    pipelineSummaries :: Lude.Maybe [PipelineSummary],
    -- | The token to retrieve the next set of results, or @null@ if there are no more results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPipelinesResponse' with the minimum fields required to make a request.
--
-- * 'pipelineSummaries' - A list of @PipelineSummary@ objects.
-- * 'nextToken' - The token to retrieve the next set of results, or @null@ if there are no more results.
-- * 'responseStatus' - The response status code.
mkListPipelinesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPipelinesResponse
mkListPipelinesResponse pResponseStatus_ =
  ListPipelinesResponse'
    { pipelineSummaries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @PipelineSummary@ objects.
--
-- /Note:/ Consider using 'pipelineSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPipelineSummaries :: Lens.Lens' ListPipelinesResponse (Lude.Maybe [PipelineSummary])
lprsPipelineSummaries = Lens.lens (pipelineSummaries :: ListPipelinesResponse -> Lude.Maybe [PipelineSummary]) (\s a -> s {pipelineSummaries = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsPipelineSummaries "Use generic-lens or generic-optics with 'pipelineSummaries' instead." #-}

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListPipelinesResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListPipelinesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPipelinesResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPipelinesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
