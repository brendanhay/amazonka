{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListPipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPipelines operation gets a list of the pipelines associated with the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListPipelines
  ( -- * Creating a request
    ListPipelines (..),
    mkListPipelines,

    -- ** Request lenses
    lpAscending,
    lpPageToken,

    -- * Destructuring the response
    ListPipelinesResponse (..),
    mkListPipelinesResponse,

    -- ** Response lenses
    lprsNextPageToken,
    lprsPipelines,
    lprsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @ListPipelineRequest@ structure.
--
-- /See:/ 'mkListPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { -- | To list pipelines in chronological order by the date and time that they were created, enter @true@ . To list pipelines in reverse chronological order, enter @false@ .
    ascending :: Lude.Maybe Lude.Text,
    -- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPipelines' with the minimum fields required to make a request.
--
-- * 'ascending' - To list pipelines in chronological order by the date and time that they were created, enter @true@ . To list pipelines in reverse chronological order, enter @false@ .
-- * 'pageToken' - When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
mkListPipelines ::
  ListPipelines
mkListPipelines =
  ListPipelines'
    { ascending = Lude.Nothing,
      pageToken = Lude.Nothing
    }

-- | To list pipelines in chronological order by the date and time that they were created, enter @true@ . To list pipelines in reverse chronological order, enter @false@ .
--
-- /Note:/ Consider using 'ascending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpAscending :: Lens.Lens' ListPipelines (Lude.Maybe Lude.Text)
lpAscending = Lens.lens (ascending :: ListPipelines -> Lude.Maybe Lude.Text) (\s a -> s {ascending = a} :: ListPipelines)
{-# DEPRECATED lpAscending "Use generic-lens or generic-optics with 'ascending' instead." #-}

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPageToken :: Lens.Lens' ListPipelines (Lude.Maybe Lude.Text)
lpPageToken = Lens.lens (pageToken :: ListPipelines -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListPipelines)
{-# DEPRECATED lpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager ListPipelines where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPipelines) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpPageToken Lens..~ rs Lens.^. lprsNextPageToken

instance Lude.AWSRequest ListPipelines where
  type Rs ListPipelines = ListPipelinesResponse
  request = Req.get elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "Pipelines" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPipelines where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPipelines where
  toPath = Lude.const "/2012-09-25/pipelines"

instance Lude.ToQuery ListPipelines where
  toQuery ListPipelines' {..} =
    Lude.mconcat
      ["Ascending" Lude.=: ascending, "PageToken" Lude.=: pageToken]

-- | A list of the pipelines associated with the current AWS account.
--
-- /See:/ 'mkListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | A value that you use to access the second and subsequent pages of results, if any. When the pipelines fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of @Pipeline@ objects.
    pipelines :: Lude.Maybe [Pipeline],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPipelinesResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - A value that you use to access the second and subsequent pages of results, if any. When the pipelines fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
-- * 'pipelines' - An array of @Pipeline@ objects.
-- * 'responseStatus' - The response status code.
mkListPipelinesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPipelinesResponse
mkListPipelinesResponse pResponseStatus_ =
  ListPipelinesResponse'
    { nextPageToken = Lude.Nothing,
      pipelines = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you use to access the second and subsequent pages of results, if any. When the pipelines fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextPageToken :: Lens.Lens' ListPipelinesResponse (Lude.Maybe Lude.Text)
lprsNextPageToken = Lens.lens (nextPageToken :: ListPipelinesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of @Pipeline@ objects.
--
-- /Note:/ Consider using 'pipelines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPipelines :: Lens.Lens' ListPipelinesResponse (Lude.Maybe [Pipeline])
lprsPipelines = Lens.lens (pipelines :: ListPipelinesResponse -> Lude.Maybe [Pipeline]) (\s a -> s {pipelines = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsPipelines "Use generic-lens or generic-optics with 'pipelines' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPipelinesResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPipelinesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
