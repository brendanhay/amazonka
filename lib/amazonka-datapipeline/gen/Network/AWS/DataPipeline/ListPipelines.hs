{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ListPipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the pipeline identifiers for all active pipelines that you have permission to access.
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.ListPipelines
  ( -- * Creating a request
    ListPipelines (..),
    mkListPipelines,

    -- ** Request lenses
    lpMarker,

    -- * Destructuring the response
    ListPipelinesResponse (..),
    mkListPipelinesResponse,

    -- ** Response lenses
    lprsHasMoreResults,
    lprsPipelineIdList,
    lprsMarker,
    lprsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ListPipelines.
--
-- /See:/ 'mkListPipelines' smart constructor.
newtype ListPipelines = ListPipelines'
  { -- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @ListPipelines@ with the marker value from the previous call to retrieve the next set of results.
    marker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPipelines' with the minimum fields required to make a request.
--
-- * 'marker' - The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @ListPipelines@ with the marker value from the previous call to retrieve the next set of results.
mkListPipelines ::
  ListPipelines
mkListPipelines = ListPipelines' {marker = Lude.Nothing}

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @ListPipelines@ with the marker value from the previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMarker :: Lens.Lens' ListPipelines (Lude.Maybe Lude.Text)
lpMarker = Lens.lens (marker :: ListPipelines -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPipelines)
{-# DEPRECATED lpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Page.AWSPager ListPipelines where
  page rq rs
    | Page.stop (rs Lens.^. lprsHasMoreResults) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lpMarker Lens..~ rs Lens.^. lprsMarker

instance Lude.AWSRequest ListPipelines where
  type Rs ListPipelines = ListPipelinesResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Lude.<$> (x Lude..?> "hasMoreResults")
            Lude.<*> (x Lude..?> "pipelineIdList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPipelines where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.ListPipelines" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPipelines where
  toJSON ListPipelines' {..} =
    Lude.object (Lude.catMaybes [("marker" Lude..=) Lude.<$> marker])

instance Lude.ToPath ListPipelines where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPipelines where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of ListPipelines.
--
-- /See:/ 'mkListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | Indicates whether there are more results that can be obtained by a subsequent call.
    hasMoreResults :: Lude.Maybe Lude.Bool,
    -- | The pipeline identifiers. If you require additional information about the pipelines, you can use these identifiers to call 'DescribePipelines' and 'GetPipelineDefinition' .
    pipelineIdList :: [PipelineIdName],
    -- | The starting point for the next page of results. To view the next page of results, call @ListPipelinesOutput@ again with this marker value. If the value is null, there are no more results.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPipelinesResponse' with the minimum fields required to make a request.
--
-- * 'hasMoreResults' - Indicates whether there are more results that can be obtained by a subsequent call.
-- * 'pipelineIdList' - The pipeline identifiers. If you require additional information about the pipelines, you can use these identifiers to call 'DescribePipelines' and 'GetPipelineDefinition' .
-- * 'marker' - The starting point for the next page of results. To view the next page of results, call @ListPipelinesOutput@ again with this marker value. If the value is null, there are no more results.
-- * 'responseStatus' - The response status code.
mkListPipelinesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPipelinesResponse
mkListPipelinesResponse pResponseStatus_ =
  ListPipelinesResponse'
    { hasMoreResults = Lude.Nothing,
      pipelineIdList = Lude.mempty,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether there are more results that can be obtained by a subsequent call.
--
-- /Note:/ Consider using 'hasMoreResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsHasMoreResults :: Lens.Lens' ListPipelinesResponse (Lude.Maybe Lude.Bool)
lprsHasMoreResults = Lens.lens (hasMoreResults :: ListPipelinesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasMoreResults = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsHasMoreResults "Use generic-lens or generic-optics with 'hasMoreResults' instead." #-}

-- | The pipeline identifiers. If you require additional information about the pipelines, you can use these identifiers to call 'DescribePipelines' and 'GetPipelineDefinition' .
--
-- /Note:/ Consider using 'pipelineIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPipelineIdList :: Lens.Lens' ListPipelinesResponse [PipelineIdName]
lprsPipelineIdList = Lens.lens (pipelineIdList :: ListPipelinesResponse -> [PipelineIdName]) (\s a -> s {pipelineIdList = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsPipelineIdList "Use generic-lens or generic-optics with 'pipelineIdList' instead." #-}

-- | The starting point for the next page of results. To view the next page of results, call @ListPipelinesOutput@ again with this marker value. If the value is null, there are no more results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsMarker :: Lens.Lens' ListPipelinesResponse (Lude.Maybe Lude.Text)
lprsMarker = Lens.lens (marker :: ListPipelinesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPipelinesResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPipelinesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPipelinesResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
