{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.ListDatasets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about data sets.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatasets
  ( -- * Creating a request
    ListDatasets (..),
    mkListDatasets,

    -- ** Request lenses
    lNextToken,
    lMaxResults,

    -- * Destructuring the response
    ListDatasetsResponse (..),
    mkListDatasetsResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsDatasetSummaries,
    lrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDatasets' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
mkListDatasets ::
  ListDatasets
mkListDatasets =
  ListDatasets'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListDatasets (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListDatasets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDatasets)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListDatasets (Lude.Maybe Lude.Natural)
lMaxResults = Lens.lens (maxResults :: ListDatasets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDatasets)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDatasets where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsDatasetSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListDatasets where
  type Rs ListDatasets = ListDatasetsResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDatasetsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "datasetSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDatasets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDatasets where
  toPath = Lude.const "/datasets"

instance Lude.ToQuery ListDatasets where
  toQuery ListDatasets' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no more results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of @DatasetSummary@ objects.
    datasetSummaries :: Lude.Maybe [DatasetSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDatasetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to retrieve the next set of results, or @null@ if there are no more results.
-- * 'datasetSummaries' - A list of @DatasetSummary@ objects.
-- * 'responseStatus' - The response status code.
mkListDatasetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDatasetsResponse
mkListDatasetsResponse pResponseStatus_ =
  ListDatasetsResponse'
    { nextToken = Lude.Nothing,
      datasetSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListDatasetsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListDatasetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDatasetsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @DatasetSummary@ objects.
--
-- /Note:/ Consider using 'datasetSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsDatasetSummaries :: Lens.Lens' ListDatasetsResponse (Lude.Maybe [DatasetSummary])
lrsDatasetSummaries = Lens.lens (datasetSummaries :: ListDatasetsResponse -> Lude.Maybe [DatasetSummary]) (\s a -> s {datasetSummaries = a} :: ListDatasetsResponse)
{-# DEPRECATED lrsDatasetSummaries "Use generic-lens or generic-optics with 'datasetSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListDatasetsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListDatasetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDatasetsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
