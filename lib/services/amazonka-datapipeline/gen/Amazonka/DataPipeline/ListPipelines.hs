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
-- Module      : Amazonka.DataPipeline.ListPipelines
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the pipeline identifiers for all active pipelines that you have
-- permission to access.
--
-- This operation returns paginated results.
module Amazonka.DataPipeline.ListPipelines
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSPager ListPipelines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipelinesResponse_hasMoreResults
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listPipelinesResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPipelines_marker
              Lens..~ rs
              Lens.^? listPipelinesResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest ListPipelines where
  type
    AWSResponse ListPipelines =
      ListPipelinesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Prelude.<$> (x Data..?> "hasMoreResults")
            Prelude.<*> (x Data..?> "marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "pipelineIdList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListPipelines where
  hashWithSalt _salt ListPipelines' {..} =
    _salt `Prelude.hashWithSalt` marker

instance Prelude.NFData ListPipelines where
  rnf ListPipelines' {..} = Prelude.rnf marker

instance Data.ToHeaders ListPipelines where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("DataPipeline.ListPipelines" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPipelines where
  toJSON ListPipelines' {..} =
    Data.object
      ( Prelude.catMaybes
          [("marker" Data..=) Prelude.<$> marker]
      )

instance Data.ToPath ListPipelines where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPipelines where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listPipelinesResponse_pipelineIdList = Lens.lens (\ListPipelinesResponse' {pipelineIdList} -> pipelineIdList) (\s@ListPipelinesResponse' {} a -> s {pipelineIdList = a} :: ListPipelinesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPipelinesResponse where
  rnf ListPipelinesResponse' {..} =
    Prelude.rnf hasMoreResults `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf pipelineIdList
