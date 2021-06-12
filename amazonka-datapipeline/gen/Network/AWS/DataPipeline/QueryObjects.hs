{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.QueryObjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries the specified pipeline for the names of objects that match the
-- specified set of conditions.
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.QueryObjects
  ( -- * Creating a Request
    QueryObjects (..),
    newQueryObjects,

    -- * Request Lenses
    queryObjects_query,
    queryObjects_limit,
    queryObjects_marker,
    queryObjects_pipelineId,
    queryObjects_sphere,

    -- * Destructuring the Response
    QueryObjectsResponse (..),
    newQueryObjectsResponse,

    -- * Response Lenses
    queryObjectsResponse_ids,
    queryObjectsResponse_hasMoreResults,
    queryObjectsResponse_marker,
    queryObjectsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for QueryObjects.
--
-- /See:/ 'newQueryObjects' smart constructor.
data QueryObjects = QueryObjects'
  { -- | The query that defines the objects to be returned. The @Query@ object
    -- can contain a maximum of ten selectors. The conditions in the query are
    -- limited to top-level String fields in the object. These filters can be
    -- applied to components, instances, and attempts.
    query :: Core.Maybe Query,
    -- | The maximum number of object names that @QueryObjects@ will return in a
    -- single call. The default value is 100.
    limit :: Core.Maybe Core.Int,
    -- | The starting point for the results to be returned. For the first call,
    -- this value should be empty. As long as there are more results, continue
    -- to call @QueryObjects@ with the marker value from the previous call to
    -- retrieve the next set of results.
    marker :: Core.Maybe Core.Text,
    -- | The ID of the pipeline.
    pipelineId :: Core.Text,
    -- | Indicates whether the query applies to components or instances. The
    -- possible values are: @COMPONENT@, @INSTANCE@, and @ATTEMPT@.
    sphere :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueryObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'query', 'queryObjects_query' - The query that defines the objects to be returned. The @Query@ object
-- can contain a maximum of ten selectors. The conditions in the query are
-- limited to top-level String fields in the object. These filters can be
-- applied to components, instances, and attempts.
--
-- 'limit', 'queryObjects_limit' - The maximum number of object names that @QueryObjects@ will return in a
-- single call. The default value is 100.
--
-- 'marker', 'queryObjects_marker' - The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @QueryObjects@ with the marker value from the previous call to
-- retrieve the next set of results.
--
-- 'pipelineId', 'queryObjects_pipelineId' - The ID of the pipeline.
--
-- 'sphere', 'queryObjects_sphere' - Indicates whether the query applies to components or instances. The
-- possible values are: @COMPONENT@, @INSTANCE@, and @ATTEMPT@.
newQueryObjects ::
  -- | 'pipelineId'
  Core.Text ->
  -- | 'sphere'
  Core.Text ->
  QueryObjects
newQueryObjects pPipelineId_ pSphere_ =
  QueryObjects'
    { query = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      pipelineId = pPipelineId_,
      sphere = pSphere_
    }

-- | The query that defines the objects to be returned. The @Query@ object
-- can contain a maximum of ten selectors. The conditions in the query are
-- limited to top-level String fields in the object. These filters can be
-- applied to components, instances, and attempts.
queryObjects_query :: Lens.Lens' QueryObjects (Core.Maybe Query)
queryObjects_query = Lens.lens (\QueryObjects' {query} -> query) (\s@QueryObjects' {} a -> s {query = a} :: QueryObjects)

-- | The maximum number of object names that @QueryObjects@ will return in a
-- single call. The default value is 100.
queryObjects_limit :: Lens.Lens' QueryObjects (Core.Maybe Core.Int)
queryObjects_limit = Lens.lens (\QueryObjects' {limit} -> limit) (\s@QueryObjects' {} a -> s {limit = a} :: QueryObjects)

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @QueryObjects@ with the marker value from the previous call to
-- retrieve the next set of results.
queryObjects_marker :: Lens.Lens' QueryObjects (Core.Maybe Core.Text)
queryObjects_marker = Lens.lens (\QueryObjects' {marker} -> marker) (\s@QueryObjects' {} a -> s {marker = a} :: QueryObjects)

-- | The ID of the pipeline.
queryObjects_pipelineId :: Lens.Lens' QueryObjects Core.Text
queryObjects_pipelineId = Lens.lens (\QueryObjects' {pipelineId} -> pipelineId) (\s@QueryObjects' {} a -> s {pipelineId = a} :: QueryObjects)

-- | Indicates whether the query applies to components or instances. The
-- possible values are: @COMPONENT@, @INSTANCE@, and @ATTEMPT@.
queryObjects_sphere :: Lens.Lens' QueryObjects Core.Text
queryObjects_sphere = Lens.lens (\QueryObjects' {sphere} -> sphere) (\s@QueryObjects' {} a -> s {sphere = a} :: QueryObjects)

instance Core.AWSPager QueryObjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? queryObjectsResponse_hasMoreResults
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? queryObjectsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& queryObjects_marker
          Lens..~ rs
          Lens.^? queryObjectsResponse_marker Core.. Lens._Just

instance Core.AWSRequest QueryObjects where
  type AWSResponse QueryObjects = QueryObjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryObjectsResponse'
            Core.<$> (x Core..?> "ids" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "hasMoreResults")
            Core.<*> (x Core..?> "marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable QueryObjects

instance Core.NFData QueryObjects

instance Core.ToHeaders QueryObjects where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DataPipeline.QueryObjects" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON QueryObjects where
  toJSON QueryObjects' {..} =
    Core.object
      ( Core.catMaybes
          [ ("query" Core..=) Core.<$> query,
            ("limit" Core..=) Core.<$> limit,
            ("marker" Core..=) Core.<$> marker,
            Core.Just ("pipelineId" Core..= pipelineId),
            Core.Just ("sphere" Core..= sphere)
          ]
      )

instance Core.ToPath QueryObjects where
  toPath = Core.const "/"

instance Core.ToQuery QueryObjects where
  toQuery = Core.const Core.mempty

-- | Contains the output of QueryObjects.
--
-- /See:/ 'newQueryObjectsResponse' smart constructor.
data QueryObjectsResponse = QueryObjectsResponse'
  { -- | The identifiers that match the query selectors.
    ids :: Core.Maybe [Core.Text],
    -- | Indicates whether there are more results that can be obtained by a
    -- subsequent call.
    hasMoreResults :: Core.Maybe Core.Bool,
    -- | The starting point for the next page of results. To view the next page
    -- of results, call @QueryObjects@ again with this marker value. If the
    -- value is null, there are no more results.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueryObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'queryObjectsResponse_ids' - The identifiers that match the query selectors.
--
-- 'hasMoreResults', 'queryObjectsResponse_hasMoreResults' - Indicates whether there are more results that can be obtained by a
-- subsequent call.
--
-- 'marker', 'queryObjectsResponse_marker' - The starting point for the next page of results. To view the next page
-- of results, call @QueryObjects@ again with this marker value. If the
-- value is null, there are no more results.
--
-- 'httpStatus', 'queryObjectsResponse_httpStatus' - The response's http status code.
newQueryObjectsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  QueryObjectsResponse
newQueryObjectsResponse pHttpStatus_ =
  QueryObjectsResponse'
    { ids = Core.Nothing,
      hasMoreResults = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifiers that match the query selectors.
queryObjectsResponse_ids :: Lens.Lens' QueryObjectsResponse (Core.Maybe [Core.Text])
queryObjectsResponse_ids = Lens.lens (\QueryObjectsResponse' {ids} -> ids) (\s@QueryObjectsResponse' {} a -> s {ids = a} :: QueryObjectsResponse) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether there are more results that can be obtained by a
-- subsequent call.
queryObjectsResponse_hasMoreResults :: Lens.Lens' QueryObjectsResponse (Core.Maybe Core.Bool)
queryObjectsResponse_hasMoreResults = Lens.lens (\QueryObjectsResponse' {hasMoreResults} -> hasMoreResults) (\s@QueryObjectsResponse' {} a -> s {hasMoreResults = a} :: QueryObjectsResponse)

-- | The starting point for the next page of results. To view the next page
-- of results, call @QueryObjects@ again with this marker value. If the
-- value is null, there are no more results.
queryObjectsResponse_marker :: Lens.Lens' QueryObjectsResponse (Core.Maybe Core.Text)
queryObjectsResponse_marker = Lens.lens (\QueryObjectsResponse' {marker} -> marker) (\s@QueryObjectsResponse' {} a -> s {marker = a} :: QueryObjectsResponse)

-- | The response's http status code.
queryObjectsResponse_httpStatus :: Lens.Lens' QueryObjectsResponse Core.Int
queryObjectsResponse_httpStatus = Lens.lens (\QueryObjectsResponse' {httpStatus} -> httpStatus) (\s@QueryObjectsResponse' {} a -> s {httpStatus = a} :: QueryObjectsResponse)

instance Core.NFData QueryObjectsResponse
