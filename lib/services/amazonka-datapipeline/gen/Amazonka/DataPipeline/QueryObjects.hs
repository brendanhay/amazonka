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
-- Module      : Amazonka.DataPipeline.QueryObjects
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries the specified pipeline for the names of objects that match the
-- specified set of conditions.
--
-- This operation returns paginated results.
module Amazonka.DataPipeline.QueryObjects
  ( -- * Creating a Request
    QueryObjects (..),
    newQueryObjects,

    -- * Request Lenses
    queryObjects_marker,
    queryObjects_limit,
    queryObjects_query,
    queryObjects_pipelineId,
    queryObjects_sphere,

    -- * Destructuring the Response
    QueryObjectsResponse (..),
    newQueryObjectsResponse,

    -- * Response Lenses
    queryObjectsResponse_marker,
    queryObjectsResponse_ids,
    queryObjectsResponse_hasMoreResults,
    queryObjectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for QueryObjects.
--
-- /See:/ 'newQueryObjects' smart constructor.
data QueryObjects = QueryObjects'
  { -- | The starting point for the results to be returned. For the first call,
    -- this value should be empty. As long as there are more results, continue
    -- to call @QueryObjects@ with the marker value from the previous call to
    -- retrieve the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of object names that @QueryObjects@ will return in a
    -- single call. The default value is 100.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The query that defines the objects to be returned. The @Query@ object
    -- can contain a maximum of ten selectors. The conditions in the query are
    -- limited to top-level String fields in the object. These filters can be
    -- applied to components, instances, and attempts.
    query :: Prelude.Maybe Query,
    -- | The ID of the pipeline.
    pipelineId :: Prelude.Text,
    -- | Indicates whether the query applies to components or instances. The
    -- possible values are: @COMPONENT@, @INSTANCE@, and @ATTEMPT@.
    sphere :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'queryObjects_marker' - The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @QueryObjects@ with the marker value from the previous call to
-- retrieve the next set of results.
--
-- 'limit', 'queryObjects_limit' - The maximum number of object names that @QueryObjects@ will return in a
-- single call. The default value is 100.
--
-- 'query', 'queryObjects_query' - The query that defines the objects to be returned. The @Query@ object
-- can contain a maximum of ten selectors. The conditions in the query are
-- limited to top-level String fields in the object. These filters can be
-- applied to components, instances, and attempts.
--
-- 'pipelineId', 'queryObjects_pipelineId' - The ID of the pipeline.
--
-- 'sphere', 'queryObjects_sphere' - Indicates whether the query applies to components or instances. The
-- possible values are: @COMPONENT@, @INSTANCE@, and @ATTEMPT@.
newQueryObjects ::
  -- | 'pipelineId'
  Prelude.Text ->
  -- | 'sphere'
  Prelude.Text ->
  QueryObjects
newQueryObjects pPipelineId_ pSphere_ =
  QueryObjects'
    { marker = Prelude.Nothing,
      limit = Prelude.Nothing,
      query = Prelude.Nothing,
      pipelineId = pPipelineId_,
      sphere = pSphere_
    }

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @QueryObjects@ with the marker value from the previous call to
-- retrieve the next set of results.
queryObjects_marker :: Lens.Lens' QueryObjects (Prelude.Maybe Prelude.Text)
queryObjects_marker = Lens.lens (\QueryObjects' {marker} -> marker) (\s@QueryObjects' {} a -> s {marker = a} :: QueryObjects)

-- | The maximum number of object names that @QueryObjects@ will return in a
-- single call. The default value is 100.
queryObjects_limit :: Lens.Lens' QueryObjects (Prelude.Maybe Prelude.Int)
queryObjects_limit = Lens.lens (\QueryObjects' {limit} -> limit) (\s@QueryObjects' {} a -> s {limit = a} :: QueryObjects)

-- | The query that defines the objects to be returned. The @Query@ object
-- can contain a maximum of ten selectors. The conditions in the query are
-- limited to top-level String fields in the object. These filters can be
-- applied to components, instances, and attempts.
queryObjects_query :: Lens.Lens' QueryObjects (Prelude.Maybe Query)
queryObjects_query = Lens.lens (\QueryObjects' {query} -> query) (\s@QueryObjects' {} a -> s {query = a} :: QueryObjects)

-- | The ID of the pipeline.
queryObjects_pipelineId :: Lens.Lens' QueryObjects Prelude.Text
queryObjects_pipelineId = Lens.lens (\QueryObjects' {pipelineId} -> pipelineId) (\s@QueryObjects' {} a -> s {pipelineId = a} :: QueryObjects)

-- | Indicates whether the query applies to components or instances. The
-- possible values are: @COMPONENT@, @INSTANCE@, and @ATTEMPT@.
queryObjects_sphere :: Lens.Lens' QueryObjects Prelude.Text
queryObjects_sphere = Lens.lens (\QueryObjects' {sphere} -> sphere) (\s@QueryObjects' {} a -> s {sphere = a} :: QueryObjects)

instance Core.AWSPager QueryObjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? queryObjectsResponse_hasMoreResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? queryObjectsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& queryObjects_marker
          Lens..~ rs
          Lens.^? queryObjectsResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest QueryObjects where
  type AWSResponse QueryObjects = QueryObjectsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryObjectsResponse'
            Prelude.<$> (x Core..?> "marker")
            Prelude.<*> (x Core..?> "ids" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "hasMoreResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable QueryObjects where
  hashWithSalt _salt QueryObjects' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` query
      `Prelude.hashWithSalt` pipelineId
      `Prelude.hashWithSalt` sphere

instance Prelude.NFData QueryObjects where
  rnf QueryObjects' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf query
      `Prelude.seq` Prelude.rnf pipelineId
      `Prelude.seq` Prelude.rnf sphere

instance Core.ToHeaders QueryObjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("DataPipeline.QueryObjects" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON QueryObjects where
  toJSON QueryObjects' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("marker" Core..=) Prelude.<$> marker,
            ("limit" Core..=) Prelude.<$> limit,
            ("query" Core..=) Prelude.<$> query,
            Prelude.Just ("pipelineId" Core..= pipelineId),
            Prelude.Just ("sphere" Core..= sphere)
          ]
      )

instance Core.ToPath QueryObjects where
  toPath = Prelude.const "/"

instance Core.ToQuery QueryObjects where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of QueryObjects.
--
-- /See:/ 'newQueryObjectsResponse' smart constructor.
data QueryObjectsResponse = QueryObjectsResponse'
  { -- | The starting point for the next page of results. To view the next page
    -- of results, call @QueryObjects@ again with this marker value. If the
    -- value is null, there are no more results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The identifiers that match the query selectors.
    ids :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether there are more results that can be obtained by a
    -- subsequent call.
    hasMoreResults :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'queryObjectsResponse_marker' - The starting point for the next page of results. To view the next page
-- of results, call @QueryObjects@ again with this marker value. If the
-- value is null, there are no more results.
--
-- 'ids', 'queryObjectsResponse_ids' - The identifiers that match the query selectors.
--
-- 'hasMoreResults', 'queryObjectsResponse_hasMoreResults' - Indicates whether there are more results that can be obtained by a
-- subsequent call.
--
-- 'httpStatus', 'queryObjectsResponse_httpStatus' - The response's http status code.
newQueryObjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  QueryObjectsResponse
newQueryObjectsResponse pHttpStatus_ =
  QueryObjectsResponse'
    { marker = Prelude.Nothing,
      ids = Prelude.Nothing,
      hasMoreResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The starting point for the next page of results. To view the next page
-- of results, call @QueryObjects@ again with this marker value. If the
-- value is null, there are no more results.
queryObjectsResponse_marker :: Lens.Lens' QueryObjectsResponse (Prelude.Maybe Prelude.Text)
queryObjectsResponse_marker = Lens.lens (\QueryObjectsResponse' {marker} -> marker) (\s@QueryObjectsResponse' {} a -> s {marker = a} :: QueryObjectsResponse)

-- | The identifiers that match the query selectors.
queryObjectsResponse_ids :: Lens.Lens' QueryObjectsResponse (Prelude.Maybe [Prelude.Text])
queryObjectsResponse_ids = Lens.lens (\QueryObjectsResponse' {ids} -> ids) (\s@QueryObjectsResponse' {} a -> s {ids = a} :: QueryObjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether there are more results that can be obtained by a
-- subsequent call.
queryObjectsResponse_hasMoreResults :: Lens.Lens' QueryObjectsResponse (Prelude.Maybe Prelude.Bool)
queryObjectsResponse_hasMoreResults = Lens.lens (\QueryObjectsResponse' {hasMoreResults} -> hasMoreResults) (\s@QueryObjectsResponse' {} a -> s {hasMoreResults = a} :: QueryObjectsResponse)

-- | The response's http status code.
queryObjectsResponse_httpStatus :: Lens.Lens' QueryObjectsResponse Prelude.Int
queryObjectsResponse_httpStatus = Lens.lens (\QueryObjectsResponse' {httpStatus} -> httpStatus) (\s@QueryObjectsResponse' {} a -> s {httpStatus = a} :: QueryObjectsResponse)

instance Prelude.NFData QueryObjectsResponse where
  rnf QueryObjectsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf ids
      `Prelude.seq` Prelude.rnf hasMoreResults
      `Prelude.seq` Prelude.rnf httpStatus
