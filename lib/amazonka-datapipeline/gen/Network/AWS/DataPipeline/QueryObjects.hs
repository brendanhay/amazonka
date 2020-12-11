{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.QueryObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries the specified pipeline for the names of objects that match the specified set of conditions.
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.QueryObjects
  ( -- * Creating a request
    QueryObjects (..),
    mkQueryObjects,

    -- ** Request lenses
    qoQuery,
    qoMarker,
    qoLimit,
    qoPipelineId,
    qoSphere,

    -- * Destructuring the response
    QueryObjectsResponse (..),
    mkQueryObjectsResponse,

    -- ** Response lenses
    qorsHasMoreResults,
    qorsIds,
    qorsMarker,
    qorsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for QueryObjects.
--
-- /See:/ 'mkQueryObjects' smart constructor.
data QueryObjects = QueryObjects'
  { query :: Lude.Maybe Query,
    marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Int,
    pipelineId :: Lude.Text,
    sphere :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryObjects' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of object names that @QueryObjects@ will return in a single call. The default value is 100.
-- * 'marker' - The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @QueryObjects@ with the marker value from the previous call to retrieve the next set of results.
-- * 'pipelineId' - The ID of the pipeline.
-- * 'query' - The query that defines the objects to be returned. The @Query@ object can contain a maximum of ten selectors. The conditions in the query are limited to top-level String fields in the object. These filters can be applied to components, instances, and attempts.
-- * 'sphere' - Indicates whether the query applies to components or instances. The possible values are: @COMPONENT@ , @INSTANCE@ , and @ATTEMPT@ .
mkQueryObjects ::
  -- | 'pipelineId'
  Lude.Text ->
  -- | 'sphere'
  Lude.Text ->
  QueryObjects
mkQueryObjects pPipelineId_ pSphere_ =
  QueryObjects'
    { query = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing,
      pipelineId = pPipelineId_,
      sphere = pSphere_
    }

-- | The query that defines the objects to be returned. The @Query@ object can contain a maximum of ten selectors. The conditions in the query are limited to top-level String fields in the object. These filters can be applied to components, instances, and attempts.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoQuery :: Lens.Lens' QueryObjects (Lude.Maybe Query)
qoQuery = Lens.lens (query :: QueryObjects -> Lude.Maybe Query) (\s a -> s {query = a} :: QueryObjects)
{-# DEPRECATED qoQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | The starting point for the results to be returned. For the first call, this value should be empty. As long as there are more results, continue to call @QueryObjects@ with the marker value from the previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoMarker :: Lens.Lens' QueryObjects (Lude.Maybe Lude.Text)
qoMarker = Lens.lens (marker :: QueryObjects -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: QueryObjects)
{-# DEPRECATED qoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of object names that @QueryObjects@ will return in a single call. The default value is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoLimit :: Lens.Lens' QueryObjects (Lude.Maybe Lude.Int)
qoLimit = Lens.lens (limit :: QueryObjects -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: QueryObjects)
{-# DEPRECATED qoLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoPipelineId :: Lens.Lens' QueryObjects Lude.Text
qoPipelineId = Lens.lens (pipelineId :: QueryObjects -> Lude.Text) (\s a -> s {pipelineId = a} :: QueryObjects)
{-# DEPRECATED qoPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | Indicates whether the query applies to components or instances. The possible values are: @COMPONENT@ , @INSTANCE@ , and @ATTEMPT@ .
--
-- /Note:/ Consider using 'sphere' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qoSphere :: Lens.Lens' QueryObjects Lude.Text
qoSphere = Lens.lens (sphere :: QueryObjects -> Lude.Text) (\s a -> s {sphere = a} :: QueryObjects)
{-# DEPRECATED qoSphere "Use generic-lens or generic-optics with 'sphere' instead." #-}

instance Page.AWSPager QueryObjects where
  page rq rs
    | Page.stop (rs Lens.^. qorsHasMoreResults) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. qorsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& qoMarker Lens..~ rs Lens.^. qorsMarker

instance Lude.AWSRequest QueryObjects where
  type Rs QueryObjects = QueryObjectsResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          QueryObjectsResponse'
            Lude.<$> (x Lude..?> "hasMoreResults")
            Lude.<*> (x Lude..?> "ids" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders QueryObjects where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.QueryObjects" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON QueryObjects where
  toJSON QueryObjects' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("query" Lude..=) Lude.<$> query,
            ("marker" Lude..=) Lude.<$> marker,
            ("limit" Lude..=) Lude.<$> limit,
            Lude.Just ("pipelineId" Lude..= pipelineId),
            Lude.Just ("sphere" Lude..= sphere)
          ]
      )

instance Lude.ToPath QueryObjects where
  toPath = Lude.const "/"

instance Lude.ToQuery QueryObjects where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of QueryObjects.
--
-- /See:/ 'mkQueryObjectsResponse' smart constructor.
data QueryObjectsResponse = QueryObjectsResponse'
  { hasMoreResults ::
      Lude.Maybe Lude.Bool,
    ids :: Lude.Maybe [Lude.Text],
    marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'QueryObjectsResponse' with the minimum fields required to make a request.
--
-- * 'hasMoreResults' - Indicates whether there are more results that can be obtained by a subsequent call.
-- * 'ids' - The identifiers that match the query selectors.
-- * 'marker' - The starting point for the next page of results. To view the next page of results, call @QueryObjects@ again with this marker value. If the value is null, there are no more results.
-- * 'responseStatus' - The response status code.
mkQueryObjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  QueryObjectsResponse
mkQueryObjectsResponse pResponseStatus_ =
  QueryObjectsResponse'
    { hasMoreResults = Lude.Nothing,
      ids = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether there are more results that can be obtained by a subsequent call.
--
-- /Note:/ Consider using 'hasMoreResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorsHasMoreResults :: Lens.Lens' QueryObjectsResponse (Lude.Maybe Lude.Bool)
qorsHasMoreResults = Lens.lens (hasMoreResults :: QueryObjectsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasMoreResults = a} :: QueryObjectsResponse)
{-# DEPRECATED qorsHasMoreResults "Use generic-lens or generic-optics with 'hasMoreResults' instead." #-}

-- | The identifiers that match the query selectors.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorsIds :: Lens.Lens' QueryObjectsResponse (Lude.Maybe [Lude.Text])
qorsIds = Lens.lens (ids :: QueryObjectsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {ids = a} :: QueryObjectsResponse)
{-# DEPRECATED qorsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | The starting point for the next page of results. To view the next page of results, call @QueryObjects@ again with this marker value. If the value is null, there are no more results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorsMarker :: Lens.Lens' QueryObjectsResponse (Lude.Maybe Lude.Text)
qorsMarker = Lens.lens (marker :: QueryObjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: QueryObjectsResponse)
{-# DEPRECATED qorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qorsResponseStatus :: Lens.Lens' QueryObjectsResponse Lude.Int
qorsResponseStatus = Lens.lens (responseStatus :: QueryObjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: QueryObjectsResponse)
{-# DEPRECATED qorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
