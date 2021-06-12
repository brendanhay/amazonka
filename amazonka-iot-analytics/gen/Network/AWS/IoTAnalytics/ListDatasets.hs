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
-- Module      : Network.AWS.IoTAnalytics.ListDatasets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about data sets.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatasets
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
    listDatasetsResponse_datasetSummaries,
    listDatasetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDatasets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasets_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listDatasets_maxResults' - The maximum number of results to return in this request.
--
-- The default value is 100.
newListDatasets ::
  ListDatasets
newListDatasets =
  ListDatasets'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results.
listDatasets_nextToken :: Lens.Lens' ListDatasets (Core.Maybe Core.Text)
listDatasets_nextToken = Lens.lens (\ListDatasets' {nextToken} -> nextToken) (\s@ListDatasets' {} a -> s {nextToken = a} :: ListDatasets)

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
listDatasets_maxResults :: Lens.Lens' ListDatasets (Core.Maybe Core.Natural)
listDatasets_maxResults = Lens.lens (\ListDatasets' {maxResults} -> maxResults) (\s@ListDatasets' {} a -> s {maxResults = a} :: ListDatasets)

instance Core.AWSPager ListDatasets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetsResponse_datasetSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDatasets_nextToken
          Lens..~ rs
          Lens.^? listDatasetsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDatasets where
  type AWSResponse ListDatasets = ListDatasetsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "datasetSummaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDatasets

instance Core.NFData ListDatasets

instance Core.ToHeaders ListDatasets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListDatasets where
  toPath = Core.const "/datasets"

instance Core.ToQuery ListDatasets where
  toQuery ListDatasets' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @DatasetSummary@ objects.
    datasetSummaries :: Core.Maybe [DatasetSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDatasetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'datasetSummaries', 'listDatasetsResponse_datasetSummaries' - A list of @DatasetSummary@ objects.
--
-- 'httpStatus', 'listDatasetsResponse_httpStatus' - The response's http status code.
newListDatasetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDatasetsResponse
newListDatasetsResponse pHttpStatus_ =
  ListDatasetsResponse'
    { nextToken = Core.Nothing,
      datasetSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listDatasetsResponse_nextToken :: Lens.Lens' ListDatasetsResponse (Core.Maybe Core.Text)
listDatasetsResponse_nextToken = Lens.lens (\ListDatasetsResponse' {nextToken} -> nextToken) (\s@ListDatasetsResponse' {} a -> s {nextToken = a} :: ListDatasetsResponse)

-- | A list of @DatasetSummary@ objects.
listDatasetsResponse_datasetSummaries :: Lens.Lens' ListDatasetsResponse (Core.Maybe [DatasetSummary])
listDatasetsResponse_datasetSummaries = Lens.lens (\ListDatasetsResponse' {datasetSummaries} -> datasetSummaries) (\s@ListDatasetsResponse' {} a -> s {datasetSummaries = a} :: ListDatasetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDatasetsResponse_httpStatus :: Lens.Lens' ListDatasetsResponse Core.Int
listDatasetsResponse_httpStatus = Lens.lens (\ListDatasetsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetsResponse' {} a -> s {httpStatus = a} :: ListDatasetsResponse)

instance Core.NFData ListDatasetsResponse
