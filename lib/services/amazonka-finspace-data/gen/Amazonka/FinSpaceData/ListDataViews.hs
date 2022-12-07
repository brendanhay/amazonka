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
-- Module      : Amazonka.FinSpaceData.ListDataViews
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available Dataviews for a Dataset.
--
-- This operation returns paginated results.
module Amazonka.FinSpaceData.ListDataViews
  ( -- * Creating a Request
    ListDataViews (..),
    newListDataViews,

    -- * Request Lenses
    listDataViews_nextToken,
    listDataViews_maxResults,
    listDataViews_datasetId,

    -- * Destructuring the Response
    ListDataViewsResponse (..),
    newListDataViewsResponse,

    -- * Response Lenses
    listDataViewsResponse_nextToken,
    listDataViewsResponse_dataViews,
    listDataViewsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request for a list data views.
--
-- /See:/ 'newListDataViews' smart constructor.
data ListDataViews = ListDataViews'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier of the Dataset for which to retrieve Dataviews.
    datasetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataViews' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataViews_nextToken' - A token that indicates where a results page should begin.
--
-- 'maxResults', 'listDataViews_maxResults' - The maximum number of results per page.
--
-- 'datasetId', 'listDataViews_datasetId' - The unique identifier of the Dataset for which to retrieve Dataviews.
newListDataViews ::
  -- | 'datasetId'
  Prelude.Text ->
  ListDataViews
newListDataViews pDatasetId_ =
  ListDataViews'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      datasetId = pDatasetId_
    }

-- | A token that indicates where a results page should begin.
listDataViews_nextToken :: Lens.Lens' ListDataViews (Prelude.Maybe Prelude.Text)
listDataViews_nextToken = Lens.lens (\ListDataViews' {nextToken} -> nextToken) (\s@ListDataViews' {} a -> s {nextToken = a} :: ListDataViews)

-- | The maximum number of results per page.
listDataViews_maxResults :: Lens.Lens' ListDataViews (Prelude.Maybe Prelude.Natural)
listDataViews_maxResults = Lens.lens (\ListDataViews' {maxResults} -> maxResults) (\s@ListDataViews' {} a -> s {maxResults = a} :: ListDataViews)

-- | The unique identifier of the Dataset for which to retrieve Dataviews.
listDataViews_datasetId :: Lens.Lens' ListDataViews Prelude.Text
listDataViews_datasetId = Lens.lens (\ListDataViews' {datasetId} -> datasetId) (\s@ListDataViews' {} a -> s {datasetId = a} :: ListDataViews)

instance Core.AWSPager ListDataViews where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataViewsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDataViewsResponse_dataViews Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDataViews_nextToken
          Lens..~ rs
          Lens.^? listDataViewsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDataViews where
  type
    AWSResponse ListDataViews =
      ListDataViewsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataViewsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "dataViews" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataViews where
  hashWithSalt _salt ListDataViews' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` datasetId

instance Prelude.NFData ListDataViews where
  rnf ListDataViews' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf datasetId

instance Data.ToHeaders ListDataViews where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDataViews where
  toPath ListDataViews' {..} =
    Prelude.mconcat
      ["/datasets/", Data.toBS datasetId, "/dataviewsv2"]

instance Data.ToQuery ListDataViews where
  toQuery ListDataViews' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListDataViewsResponse' smart constructor.
data ListDataViewsResponse = ListDataViewsResponse'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of Dataviews.
    dataViews :: Prelude.Maybe [DataViewSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataViewsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataViewsResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'dataViews', 'listDataViewsResponse_dataViews' - A list of Dataviews.
--
-- 'httpStatus', 'listDataViewsResponse_httpStatus' - The response's http status code.
newListDataViewsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataViewsResponse
newListDataViewsResponse pHttpStatus_ =
  ListDataViewsResponse'
    { nextToken = Prelude.Nothing,
      dataViews = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where a results page should begin.
listDataViewsResponse_nextToken :: Lens.Lens' ListDataViewsResponse (Prelude.Maybe Prelude.Text)
listDataViewsResponse_nextToken = Lens.lens (\ListDataViewsResponse' {nextToken} -> nextToken) (\s@ListDataViewsResponse' {} a -> s {nextToken = a} :: ListDataViewsResponse)

-- | A list of Dataviews.
listDataViewsResponse_dataViews :: Lens.Lens' ListDataViewsResponse (Prelude.Maybe [DataViewSummary])
listDataViewsResponse_dataViews = Lens.lens (\ListDataViewsResponse' {dataViews} -> dataViews) (\s@ListDataViewsResponse' {} a -> s {dataViews = a} :: ListDataViewsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDataViewsResponse_httpStatus :: Lens.Lens' ListDataViewsResponse Prelude.Int
listDataViewsResponse_httpStatus = Lens.lens (\ListDataViewsResponse' {httpStatus} -> httpStatus) (\s@ListDataViewsResponse' {} a -> s {httpStatus = a} :: ListDataViewsResponse)

instance Prelude.NFData ListDataViewsResponse where
  rnf ListDataViewsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dataViews
      `Prelude.seq` Prelude.rnf httpStatus
