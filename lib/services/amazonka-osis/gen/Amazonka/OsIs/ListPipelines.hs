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
-- Module      : Amazonka.OsIs.ListPipelines
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all OpenSearch Ingestion pipelines in the current Amazon Web
-- Services account and Region. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/list-pipeline.html Viewing Amazon OpenSearch Ingestion pipelines>.
module Amazonka.OsIs.ListPipelines
  ( -- * Creating a Request
    ListPipelines (..),
    newListPipelines,

    -- * Request Lenses
    listPipelines_maxResults,
    listPipelines_nextToken,

    -- * Destructuring the Response
    ListPipelinesResponse (..),
    newListPipelinesResponse,

    -- * Response Lenses
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your initial @ListPipelines@ operation returns a @nextToken@, you can
    -- include the returned @nextToken@ in subsequent @ListPipelines@
    -- operations, which returns results in the next page.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listPipelines_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'nextToken', 'listPipelines_nextToken' - If your initial @ListPipelines@ operation returns a @nextToken@, you can
-- include the returned @nextToken@ in subsequent @ListPipelines@
-- operations, which returns results in the next page.
newListPipelines ::
  ListPipelines
newListPipelines =
  ListPipelines'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
listPipelines_maxResults :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.Natural)
listPipelines_maxResults = Lens.lens (\ListPipelines' {maxResults} -> maxResults) (\s@ListPipelines' {} a -> s {maxResults = a} :: ListPipelines)

-- | If your initial @ListPipelines@ operation returns a @nextToken@, you can
-- include the returned @nextToken@ in subsequent @ListPipelines@
-- operations, which returns results in the next page.
listPipelines_nextToken :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.Text)
listPipelines_nextToken = Lens.lens (\ListPipelines' {nextToken} -> nextToken) (\s@ListPipelines' {} a -> s {nextToken = a} :: ListPipelines)

instance Core.AWSRequest ListPipelines where
  type
    AWSResponse ListPipelines =
      ListPipelinesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Pipelines" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPipelines where
  hashWithSalt _salt ListPipelines' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPipelines where
  rnf ListPipelines' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPipelines where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListPipelines where
  toPath =
    Prelude.const "/2022-01-01/osis/listPipelines"

instance Data.ToQuery ListPipelines where
  toQuery ListPipelines' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all existing Data Prepper pipelines.
    pipelines :: Prelude.Maybe [PipelineSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listPipelinesResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'pipelines', 'listPipelinesResponse_pipelines' - A list of all existing Data Prepper pipelines.
--
-- 'httpStatus', 'listPipelinesResponse_httpStatus' - The response's http status code.
newListPipelinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPipelinesResponse
newListPipelinesResponse pHttpStatus_ =
  ListPipelinesResponse'
    { nextToken = Prelude.Nothing,
      pipelines = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listPipelinesResponse_nextToken :: Lens.Lens' ListPipelinesResponse (Prelude.Maybe Prelude.Text)
listPipelinesResponse_nextToken = Lens.lens (\ListPipelinesResponse' {nextToken} -> nextToken) (\s@ListPipelinesResponse' {} a -> s {nextToken = a} :: ListPipelinesResponse)

-- | A list of all existing Data Prepper pipelines.
listPipelinesResponse_pipelines :: Lens.Lens' ListPipelinesResponse (Prelude.Maybe [PipelineSummary])
listPipelinesResponse_pipelines = Lens.lens (\ListPipelinesResponse' {pipelines} -> pipelines) (\s@ListPipelinesResponse' {} a -> s {pipelines = a} :: ListPipelinesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPipelinesResponse_httpStatus :: Lens.Lens' ListPipelinesResponse Prelude.Int
listPipelinesResponse_httpStatus = Lens.lens (\ListPipelinesResponse' {httpStatus} -> httpStatus) (\s@ListPipelinesResponse' {} a -> s {httpStatus = a} :: ListPipelinesResponse)

instance Prelude.NFData ListPipelinesResponse where
  rnf ListPipelinesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pipelines
      `Prelude.seq` Prelude.rnf httpStatus
