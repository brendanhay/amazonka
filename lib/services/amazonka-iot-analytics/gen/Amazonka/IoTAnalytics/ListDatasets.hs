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
-- Module      : Amazonka.IoTAnalytics.ListDatasets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about datasets.
--
-- This operation returns paginated results.
module Amazonka.IoTAnalytics.ListDatasets
  ( -- * Creating a Request
    ListDatasets (..),
    newListDatasets,

    -- * Request Lenses
    listDatasets_maxResults,
    listDatasets_nextToken,

    -- * Destructuring the Response
    ListDatasetsResponse (..),
    newListDatasetsResponse,

    -- * Response Lenses
    listDatasetsResponse_datasetSummaries,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDatasets_maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
--
-- 'nextToken', 'listDatasets_nextToken' - The token for the next set of results.
newListDatasets ::
  ListDatasets
newListDatasets =
  ListDatasets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
listDatasets_maxResults :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Natural)
listDatasets_maxResults = Lens.lens (\ListDatasets' {maxResults} -> maxResults) (\s@ListDatasets' {} a -> s {maxResults = a} :: ListDatasets)

-- | The token for the next set of results.
listDatasets_nextToken :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Text)
listDatasets_nextToken = Lens.lens (\ListDatasets' {nextToken} -> nextToken) (\s@ListDatasets' {} a -> s {nextToken = a} :: ListDatasets)

instance Core.AWSPager ListDatasets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetsResponse_datasetSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDatasets_nextToken
          Lens..~ rs
          Lens.^? listDatasetsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDatasets where
  type AWSResponse ListDatasets = ListDatasetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetsResponse'
            Prelude.<$> ( x
                            Data..?> "datasetSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasets where
  hashWithSalt _salt ListDatasets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDatasets where
  rnf ListDatasets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDatasets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDatasets where
  toPath = Prelude.const "/datasets"

instance Data.ToQuery ListDatasets where
  toQuery ListDatasets' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | A list of @DatasetSummary@ objects.
    datasetSummaries :: Prelude.Maybe [DatasetSummary],
    -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetSummaries', 'listDatasetsResponse_datasetSummaries' - A list of @DatasetSummary@ objects.
--
-- 'nextToken', 'listDatasetsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'httpStatus', 'listDatasetsResponse_httpStatus' - The response's http status code.
newListDatasetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetsResponse
newListDatasetsResponse pHttpStatus_ =
  ListDatasetsResponse'
    { datasetSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DatasetSummary@ objects.
listDatasetsResponse_datasetSummaries :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe [DatasetSummary])
listDatasetsResponse_datasetSummaries = Lens.lens (\ListDatasetsResponse' {datasetSummaries} -> datasetSummaries) (\s@ListDatasetsResponse' {} a -> s {datasetSummaries = a} :: ListDatasetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listDatasetsResponse_nextToken :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe Prelude.Text)
listDatasetsResponse_nextToken = Lens.lens (\ListDatasetsResponse' {nextToken} -> nextToken) (\s@ListDatasetsResponse' {} a -> s {nextToken = a} :: ListDatasetsResponse)

-- | The response's http status code.
listDatasetsResponse_httpStatus :: Lens.Lens' ListDatasetsResponse Prelude.Int
listDatasetsResponse_httpStatus = Lens.lens (\ListDatasetsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetsResponse' {} a -> s {httpStatus = a} :: ListDatasetsResponse)

instance Prelude.NFData ListDatasetsResponse where
  rnf ListDatasetsResponse' {..} =
    Prelude.rnf datasetSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
