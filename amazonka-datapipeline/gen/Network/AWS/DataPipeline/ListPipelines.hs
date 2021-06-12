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
-- Module      : Network.AWS.DataPipeline.ListPipelines
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the pipeline identifiers for all active pipelines that you have
-- permission to access.
--
-- This operation returns paginated results.
module Network.AWS.DataPipeline.ListPipelines
  ( -- * Creating a Request
    ListPipelines (..),
    newListPipelines,

    -- * Request Lenses
    listPipelines_marker,

    -- * Destructuring the Response
    ListPipelinesResponse (..),
    newListPipelinesResponse,

    -- * Response Lenses
    listPipelinesResponse_hasMoreResults,
    listPipelinesResponse_marker,
    listPipelinesResponse_httpStatus,
    listPipelinesResponse_pipelineIdList,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ListPipelines.
--
-- /See:/ 'newListPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { -- | The starting point for the results to be returned. For the first call,
    -- this value should be empty. As long as there are more results, continue
    -- to call @ListPipelines@ with the marker value from the previous call to
    -- retrieve the next set of results.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPipelines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listPipelines_marker' - The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @ListPipelines@ with the marker value from the previous call to
-- retrieve the next set of results.
newListPipelines ::
  ListPipelines
newListPipelines =
  ListPipelines' {marker = Core.Nothing}

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @ListPipelines@ with the marker value from the previous call to
-- retrieve the next set of results.
listPipelines_marker :: Lens.Lens' ListPipelines (Core.Maybe Core.Text)
listPipelines_marker = Lens.lens (\ListPipelines' {marker} -> marker) (\s@ListPipelines' {} a -> s {marker = a} :: ListPipelines)

instance Core.AWSPager ListPipelines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipelinesResponse_hasMoreResults
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listPipelinesResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPipelines_marker
          Lens..~ rs
          Lens.^? listPipelinesResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListPipelines where
  type
    AWSResponse ListPipelines =
      ListPipelinesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Core.<$> (x Core..?> "hasMoreResults")
            Core.<*> (x Core..?> "marker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "pipelineIdList" Core..!@ Core.mempty)
      )

instance Core.Hashable ListPipelines

instance Core.NFData ListPipelines

instance Core.ToHeaders ListPipelines where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DataPipeline.ListPipelines" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPipelines where
  toJSON ListPipelines' {..} =
    Core.object
      (Core.catMaybes [("marker" Core..=) Core.<$> marker])

instance Core.ToPath ListPipelines where
  toPath = Core.const "/"

instance Core.ToQuery ListPipelines where
  toQuery = Core.const Core.mempty

-- | Contains the output of ListPipelines.
--
-- /See:/ 'newListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | Indicates whether there are more results that can be obtained by a
    -- subsequent call.
    hasMoreResults :: Core.Maybe Core.Bool,
    -- | The starting point for the next page of results. To view the next page
    -- of results, call @ListPipelinesOutput@ again with this marker value. If
    -- the value is null, there are no more results.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The pipeline identifiers. If you require additional information about
    -- the pipelines, you can use these identifiers to call DescribePipelines
    -- and GetPipelineDefinition.
    pipelineIdList :: [PipelineIdName]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPipelinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hasMoreResults', 'listPipelinesResponse_hasMoreResults' - Indicates whether there are more results that can be obtained by a
-- subsequent call.
--
-- 'marker', 'listPipelinesResponse_marker' - The starting point for the next page of results. To view the next page
-- of results, call @ListPipelinesOutput@ again with this marker value. If
-- the value is null, there are no more results.
--
-- 'httpStatus', 'listPipelinesResponse_httpStatus' - The response's http status code.
--
-- 'pipelineIdList', 'listPipelinesResponse_pipelineIdList' - The pipeline identifiers. If you require additional information about
-- the pipelines, you can use these identifiers to call DescribePipelines
-- and GetPipelineDefinition.
newListPipelinesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPipelinesResponse
newListPipelinesResponse pHttpStatus_ =
  ListPipelinesResponse'
    { hasMoreResults =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_,
      pipelineIdList = Core.mempty
    }

-- | Indicates whether there are more results that can be obtained by a
-- subsequent call.
listPipelinesResponse_hasMoreResults :: Lens.Lens' ListPipelinesResponse (Core.Maybe Core.Bool)
listPipelinesResponse_hasMoreResults = Lens.lens (\ListPipelinesResponse' {hasMoreResults} -> hasMoreResults) (\s@ListPipelinesResponse' {} a -> s {hasMoreResults = a} :: ListPipelinesResponse)

-- | The starting point for the next page of results. To view the next page
-- of results, call @ListPipelinesOutput@ again with this marker value. If
-- the value is null, there are no more results.
listPipelinesResponse_marker :: Lens.Lens' ListPipelinesResponse (Core.Maybe Core.Text)
listPipelinesResponse_marker = Lens.lens (\ListPipelinesResponse' {marker} -> marker) (\s@ListPipelinesResponse' {} a -> s {marker = a} :: ListPipelinesResponse)

-- | The response's http status code.
listPipelinesResponse_httpStatus :: Lens.Lens' ListPipelinesResponse Core.Int
listPipelinesResponse_httpStatus = Lens.lens (\ListPipelinesResponse' {httpStatus} -> httpStatus) (\s@ListPipelinesResponse' {} a -> s {httpStatus = a} :: ListPipelinesResponse)

-- | The pipeline identifiers. If you require additional information about
-- the pipelines, you can use these identifiers to call DescribePipelines
-- and GetPipelineDefinition.
listPipelinesResponse_pipelineIdList :: Lens.Lens' ListPipelinesResponse [PipelineIdName]
listPipelinesResponse_pipelineIdList = Lens.lens (\ListPipelinesResponse' {pipelineIdList} -> pipelineIdList) (\s@ListPipelinesResponse' {} a -> s {pipelineIdList = a} :: ListPipelinesResponse) Core.. Lens._Coerce

instance Core.NFData ListPipelinesResponse
