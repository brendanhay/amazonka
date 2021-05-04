{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  ListPipelines' {marker = Prelude.Nothing}

-- | The starting point for the results to be returned. For the first call,
-- this value should be empty. As long as there are more results, continue
-- to call @ListPipelines@ with the marker value from the previous call to
-- retrieve the next set of results.
listPipelines_marker :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.Text)
listPipelines_marker = Lens.lens (\ListPipelines' {marker} -> marker) (\s@ListPipelines' {} a -> s {marker = a} :: ListPipelines)

instance Pager.AWSPager ListPipelines where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listPipelinesResponse_hasMoreResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listPipelinesResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listPipelines_marker
          Lens..~ rs
          Lens.^? listPipelinesResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest ListPipelines where
  type Rs ListPipelines = ListPipelinesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Prelude.<$> (x Prelude..?> "hasMoreResults")
            Prelude.<*> (x Prelude..?> "marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "pipelineIdList"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListPipelines

instance Prelude.NFData ListPipelines

instance Prelude.ToHeaders ListPipelines where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("DataPipeline.ListPipelines" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListPipelines where
  toJSON ListPipelines' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("marker" Prelude..=) Prelude.<$> marker]
      )

instance Prelude.ToPath ListPipelines where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListPipelines where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of ListPipelines.
--
-- /See:/ 'newListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | Indicates whether there are more results that can be obtained by a
    -- subsequent call.
    hasMoreResults :: Prelude.Maybe Prelude.Bool,
    -- | The starting point for the next page of results. To view the next page
    -- of results, call @ListPipelinesOutput@ again with this marker value. If
    -- the value is null, there are no more results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The pipeline identifiers. If you require additional information about
    -- the pipelines, you can use these identifiers to call DescribePipelines
    -- and GetPipelineDefinition.
    pipelineIdList :: [PipelineIdName]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListPipelinesResponse
newListPipelinesResponse pHttpStatus_ =
  ListPipelinesResponse'
    { hasMoreResults =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      pipelineIdList = Prelude.mempty
    }

-- | Indicates whether there are more results that can be obtained by a
-- subsequent call.
listPipelinesResponse_hasMoreResults :: Lens.Lens' ListPipelinesResponse (Prelude.Maybe Prelude.Bool)
listPipelinesResponse_hasMoreResults = Lens.lens (\ListPipelinesResponse' {hasMoreResults} -> hasMoreResults) (\s@ListPipelinesResponse' {} a -> s {hasMoreResults = a} :: ListPipelinesResponse)

-- | The starting point for the next page of results. To view the next page
-- of results, call @ListPipelinesOutput@ again with this marker value. If
-- the value is null, there are no more results.
listPipelinesResponse_marker :: Lens.Lens' ListPipelinesResponse (Prelude.Maybe Prelude.Text)
listPipelinesResponse_marker = Lens.lens (\ListPipelinesResponse' {marker} -> marker) (\s@ListPipelinesResponse' {} a -> s {marker = a} :: ListPipelinesResponse)

-- | The response's http status code.
listPipelinesResponse_httpStatus :: Lens.Lens' ListPipelinesResponse Prelude.Int
listPipelinesResponse_httpStatus = Lens.lens (\ListPipelinesResponse' {httpStatus} -> httpStatus) (\s@ListPipelinesResponse' {} a -> s {httpStatus = a} :: ListPipelinesResponse)

-- | The pipeline identifiers. If you require additional information about
-- the pipelines, you can use these identifiers to call DescribePipelines
-- and GetPipelineDefinition.
listPipelinesResponse_pipelineIdList :: Lens.Lens' ListPipelinesResponse [PipelineIdName]
listPipelinesResponse_pipelineIdList = Lens.lens (\ListPipelinesResponse' {pipelineIdList} -> pipelineIdList) (\s@ListPipelinesResponse' {} a -> s {pipelineIdList = a} :: ListPipelinesResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListPipelinesResponse
