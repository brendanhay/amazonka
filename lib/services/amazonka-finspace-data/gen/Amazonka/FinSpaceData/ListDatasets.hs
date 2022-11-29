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
-- Module      : Amazonka.FinSpaceData.ListDatasets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the active Datasets that a user has access to.
--
-- This operation returns paginated results.
module Amazonka.FinSpaceData.ListDatasets
  ( -- * Creating a Request
    ListDatasets (..),
    newListDatasets,

    -- * Request Lenses
    listDatasets_nextToken,
    listDatasets_maxResults,

    -- * Destructuring the Response
    ListDatasetsResponse (..),
    newListDatasetsResponse,

    -- * Response Lenses
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request for the ListDatasets operation.
--
-- /See:/ 'newListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results per page.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listDatasets_nextToken' - A token that indicates where a results page should begin.
--
-- 'maxResults', 'listDatasets_maxResults' - The maximum number of results per page.
newListDatasets ::
  ListDatasets
newListDatasets =
  ListDatasets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token that indicates where a results page should begin.
listDatasets_nextToken :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Text)
listDatasets_nextToken = Lens.lens (\ListDatasets' {nextToken} -> nextToken) (\s@ListDatasets' {} a -> s {nextToken = a} :: ListDatasets)

-- | The maximum number of results per page.
listDatasets_maxResults :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Natural)
listDatasets_maxResults = Lens.lens (\ListDatasets' {maxResults} -> maxResults) (\s@ListDatasets' {} a -> s {maxResults = a} :: ListDatasets)

instance Core.AWSPager ListDatasets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetsResponse_datasets Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDatasets_nextToken
          Lens..~ rs
          Lens.^? listDatasetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDatasets where
  type AWSResponse ListDatasets = ListDatasetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "datasets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasets where
  hashWithSalt _salt ListDatasets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDatasets where
  rnf ListDatasets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListDatasets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListDatasets where
  toPath = Prelude.const "/datasetsv2"

instance Core.ToQuery ListDatasets where
  toQuery ListDatasets' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Response for the ListDatasets operation
--
-- /See:/ 'newListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of Datasets.
    datasets :: Prelude.Maybe [Dataset],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetsResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'datasets', 'listDatasetsResponse_datasets' - List of Datasets.
--
-- 'httpStatus', 'listDatasetsResponse_httpStatus' - The response's http status code.
newListDatasetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetsResponse
newListDatasetsResponse pHttpStatus_ =
  ListDatasetsResponse'
    { nextToken = Prelude.Nothing,
      datasets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where a results page should begin.
listDatasetsResponse_nextToken :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe Prelude.Text)
listDatasetsResponse_nextToken = Lens.lens (\ListDatasetsResponse' {nextToken} -> nextToken) (\s@ListDatasetsResponse' {} a -> s {nextToken = a} :: ListDatasetsResponse)

-- | List of Datasets.
listDatasetsResponse_datasets :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe [Dataset])
listDatasetsResponse_datasets = Lens.lens (\ListDatasetsResponse' {datasets} -> datasets) (\s@ListDatasetsResponse' {} a -> s {datasets = a} :: ListDatasetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDatasetsResponse_httpStatus :: Lens.Lens' ListDatasetsResponse Prelude.Int
listDatasetsResponse_httpStatus = Lens.lens (\ListDatasetsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetsResponse' {} a -> s {httpStatus = a} :: ListDatasetsResponse)

instance Prelude.NFData ListDatasetsResponse where
  rnf ListDatasetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf datasets
      `Prelude.seq` Prelude.rnf httpStatus
