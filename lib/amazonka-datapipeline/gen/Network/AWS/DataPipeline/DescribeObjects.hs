{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.DescribeObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the object definitions for a set of objects associated with the pipeline. Object definitions are composed of a set of fields that define the properties of the object.
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.DescribeObjects
  ( -- * Creating a request
    DescribeObjects (..),
    mkDescribeObjects,

    -- ** Request lenses
    doPipelineId,
    doEvaluateExpressions,
    doMarker,
    doObjectIds,

    -- * Destructuring the response
    DescribeObjectsResponse (..),
    mkDescribeObjectsResponse,

    -- ** Response lenses
    dorsPipelineObjects,
    dorsHasMoreResults,
    dorsMarker,
    dorsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeObjects.
--
-- /See:/ 'mkDescribeObjects' smart constructor.
data DescribeObjects = DescribeObjects'
  { -- | The ID of the pipeline that contains the object definitions.
    pipelineId :: Lude.Text,
    -- | Indicates whether any expressions in the object should be evaluated when the object descriptions are returned.
    evaluateExpressions :: Lude.Maybe Lude.Bool,
    -- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @DescribeObjects@ with the marker value from the previous call to retrieve the next set of results.
    marker :: Lude.Maybe Lude.Text,
    -- | The IDs of the pipeline objects that contain the definitions to be described. You can pass as many as 25 identifiers in a single call to @DescribeObjects@ .
    objectIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeObjects' with the minimum fields required to make a request.
--
-- * 'pipelineId' - The ID of the pipeline that contains the object definitions.
-- * 'evaluateExpressions' - Indicates whether any expressions in the object should be evaluated when the object descriptions are returned.
-- * 'marker' - The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @DescribeObjects@ with the marker value from the previous call to retrieve the next set of results.
-- * 'objectIds' - The IDs of the pipeline objects that contain the definitions to be described. You can pass as many as 25 identifiers in a single call to @DescribeObjects@ .
mkDescribeObjects ::
  -- | 'pipelineId'
  Lude.Text ->
  DescribeObjects
mkDescribeObjects pPipelineId_ =
  DescribeObjects'
    { pipelineId = pPipelineId_,
      evaluateExpressions = Lude.Nothing,
      marker = Lude.Nothing,
      objectIds = Lude.mempty
    }

-- | The ID of the pipeline that contains the object definitions.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doPipelineId :: Lens.Lens' DescribeObjects Lude.Text
doPipelineId = Lens.lens (pipelineId :: DescribeObjects -> Lude.Text) (\s a -> s {pipelineId = a} :: DescribeObjects)
{-# DEPRECATED doPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | Indicates whether any expressions in the object should be evaluated when the object descriptions are returned.
--
-- /Note:/ Consider using 'evaluateExpressions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doEvaluateExpressions :: Lens.Lens' DescribeObjects (Lude.Maybe Lude.Bool)
doEvaluateExpressions = Lens.lens (evaluateExpressions :: DescribeObjects -> Lude.Maybe Lude.Bool) (\s a -> s {evaluateExpressions = a} :: DescribeObjects)
{-# DEPRECATED doEvaluateExpressions "Use generic-lens or generic-optics with 'evaluateExpressions' instead." #-}

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @DescribeObjects@ with the marker value from the previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doMarker :: Lens.Lens' DescribeObjects (Lude.Maybe Lude.Text)
doMarker = Lens.lens (marker :: DescribeObjects -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeObjects)
{-# DEPRECATED doMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The IDs of the pipeline objects that contain the definitions to be described. You can pass as many as 25 identifiers in a single call to @DescribeObjects@ .
--
-- /Note:/ Consider using 'objectIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doObjectIds :: Lens.Lens' DescribeObjects [Lude.Text]
doObjectIds = Lens.lens (objectIds :: DescribeObjects -> [Lude.Text]) (\s a -> s {objectIds = a} :: DescribeObjects)
{-# DEPRECATED doObjectIds "Use generic-lens or generic-optics with 'objectIds' instead." #-}

instance Page.AWSPager DescribeObjects where
  page rq rs
    | Page.stop (rs Lens.^. dorsHasMoreResults) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. dorsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& doMarker Lens..~ rs Lens.^. dorsMarker

instance Lude.AWSRequest DescribeObjects where
  type Rs DescribeObjects = DescribeObjectsResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeObjectsResponse'
            Lude.<$> (x Lude..?> "pipelineObjects" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "hasMoreResults")
            Lude.<*> (x Lude..?> "marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeObjects where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.DescribeObjects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeObjects where
  toJSON DescribeObjects' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineId" Lude..= pipelineId),
            ("evaluateExpressions" Lude..=) Lude.<$> evaluateExpressions,
            ("marker" Lude..=) Lude.<$> marker,
            Lude.Just ("objectIds" Lude..= objectIds)
          ]
      )

instance Lude.ToPath DescribeObjects where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeObjects where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of DescribeObjects.
--
-- /See:/ 'mkDescribeObjectsResponse' smart constructor.
data DescribeObjectsResponse = DescribeObjectsResponse'
  { -- | An array of object definitions.
    pipelineObjects :: [PipelineObject],
    -- | Indicates whether there are more results to return.
    hasMoreResults :: Lude.Maybe Lude.Bool,
    -- | The starting point for the next page of results. To view the next page of results, call @DescribeObjects@ again with this marker value. If the value is null, there are no more results.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeObjectsResponse' with the minimum fields required to make a request.
--
-- * 'pipelineObjects' - An array of object definitions.
-- * 'hasMoreResults' - Indicates whether there are more results to return.
-- * 'marker' - The starting point for the next page of results. To view the next page of results, call @DescribeObjects@ again with this marker value. If the value is null, there are no more results.
-- * 'responseStatus' - The response status code.
mkDescribeObjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeObjectsResponse
mkDescribeObjectsResponse pResponseStatus_ =
  DescribeObjectsResponse'
    { pipelineObjects = Lude.mempty,
      hasMoreResults = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of object definitions.
--
-- /Note:/ Consider using 'pipelineObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsPipelineObjects :: Lens.Lens' DescribeObjectsResponse [PipelineObject]
dorsPipelineObjects = Lens.lens (pipelineObjects :: DescribeObjectsResponse -> [PipelineObject]) (\s a -> s {pipelineObjects = a} :: DescribeObjectsResponse)
{-# DEPRECATED dorsPipelineObjects "Use generic-lens or generic-optics with 'pipelineObjects' instead." #-}

-- | Indicates whether there are more results to return.
--
-- /Note:/ Consider using 'hasMoreResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsHasMoreResults :: Lens.Lens' DescribeObjectsResponse (Lude.Maybe Lude.Bool)
dorsHasMoreResults = Lens.lens (hasMoreResults :: DescribeObjectsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasMoreResults = a} :: DescribeObjectsResponse)
{-# DEPRECATED dorsHasMoreResults "Use generic-lens or generic-optics with 'hasMoreResults' instead." #-}

-- | The starting point for the next page of results. To view the next page of results, call @DescribeObjects@ again with this marker value. If the value is null, there are no more results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsMarker :: Lens.Lens' DescribeObjectsResponse (Lude.Maybe Lude.Text)
dorsMarker = Lens.lens (marker :: DescribeObjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeObjectsResponse)
{-# DEPRECATED dorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsResponseStatus :: Lens.Lens' DescribeObjectsResponse Lude.Int
dorsResponseStatus = Lens.lens (responseStatus :: DescribeObjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeObjectsResponse)
{-# DEPRECATED dorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
