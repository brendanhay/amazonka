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
-- Module      : Amazonka.QuickSight.SearchDataSources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @SearchDataSources@ operation to search for data sources that
-- belong to an account.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.SearchDataSources
  ( -- * Creating a Request
    SearchDataSources (..),
    newSearchDataSources,

    -- * Request Lenses
    searchDataSources_nextToken,
    searchDataSources_maxResults,
    searchDataSources_awsAccountId,
    searchDataSources_filters,

    -- * Destructuring the Response
    SearchDataSourcesResponse (..),
    newSearchDataSourcesResponse,

    -- * Response Lenses
    searchDataSourcesResponse_nextToken,
    searchDataSourcesResponse_requestId,
    searchDataSourcesResponse_dataSourceSummaries,
    searchDataSourcesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchDataSources' smart constructor.
data SearchDataSources = SearchDataSources'
  { -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The filters to apply to the search.
    filters :: Prelude.NonEmpty DataSourceSearchFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDataSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchDataSources_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'maxResults', 'searchDataSources_maxResults' - The maximum number of results to be returned per request.
--
-- 'awsAccountId', 'searchDataSources_awsAccountId' - The Amazon Web Services account ID.
--
-- 'filters', 'searchDataSources_filters' - The filters to apply to the search.
newSearchDataSources ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'filters'
  Prelude.NonEmpty DataSourceSearchFilter ->
  SearchDataSources
newSearchDataSources pAwsAccountId_ pFilters_ =
  SearchDataSources'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      filters = Lens.coerced Lens.# pFilters_
    }

-- | A pagination token that can be used in a subsequent request.
searchDataSources_nextToken :: Lens.Lens' SearchDataSources (Prelude.Maybe Prelude.Text)
searchDataSources_nextToken = Lens.lens (\SearchDataSources' {nextToken} -> nextToken) (\s@SearchDataSources' {} a -> s {nextToken = a} :: SearchDataSources)

-- | The maximum number of results to be returned per request.
searchDataSources_maxResults :: Lens.Lens' SearchDataSources (Prelude.Maybe Prelude.Natural)
searchDataSources_maxResults = Lens.lens (\SearchDataSources' {maxResults} -> maxResults) (\s@SearchDataSources' {} a -> s {maxResults = a} :: SearchDataSources)

-- | The Amazon Web Services account ID.
searchDataSources_awsAccountId :: Lens.Lens' SearchDataSources Prelude.Text
searchDataSources_awsAccountId = Lens.lens (\SearchDataSources' {awsAccountId} -> awsAccountId) (\s@SearchDataSources' {} a -> s {awsAccountId = a} :: SearchDataSources)

-- | The filters to apply to the search.
searchDataSources_filters :: Lens.Lens' SearchDataSources (Prelude.NonEmpty DataSourceSearchFilter)
searchDataSources_filters = Lens.lens (\SearchDataSources' {filters} -> filters) (\s@SearchDataSources' {} a -> s {filters = a} :: SearchDataSources) Prelude.. Lens.coerced

instance Core.AWSPager SearchDataSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchDataSourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchDataSourcesResponse_dataSourceSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchDataSources_nextToken
          Lens..~ rs
          Lens.^? searchDataSourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchDataSources where
  type
    AWSResponse SearchDataSources =
      SearchDataSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchDataSourcesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> ( x Core..?> "DataSourceSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchDataSources where
  hashWithSalt _salt SearchDataSources' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchDataSources where
  rnf SearchDataSources' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf filters

instance Core.ToHeaders SearchDataSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchDataSources where
  toJSON SearchDataSources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("Filters" Core..= filters)
          ]
      )

instance Core.ToPath SearchDataSources where
  toPath SearchDataSources' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/search/data-sources"
      ]

instance Core.ToQuery SearchDataSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchDataSourcesResponse' smart constructor.
data SearchDataSourcesResponse = SearchDataSourcesResponse'
  { -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A @DataSourceSummaries@ object that returns a summary of a data source.
    dataSourceSummaries :: Prelude.Maybe [DataSourceSummary],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDataSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchDataSourcesResponse_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'requestId', 'searchDataSourcesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'dataSourceSummaries', 'searchDataSourcesResponse_dataSourceSummaries' - A @DataSourceSummaries@ object that returns a summary of a data source.
--
-- 'status', 'searchDataSourcesResponse_status' - The HTTP status of the request.
newSearchDataSourcesResponse ::
  -- | 'status'
  Prelude.Int ->
  SearchDataSourcesResponse
newSearchDataSourcesResponse pStatus_ =
  SearchDataSourcesResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      dataSourceSummaries = Prelude.Nothing,
      status = pStatus_
    }

-- | A pagination token that can be used in a subsequent request.
searchDataSourcesResponse_nextToken :: Lens.Lens' SearchDataSourcesResponse (Prelude.Maybe Prelude.Text)
searchDataSourcesResponse_nextToken = Lens.lens (\SearchDataSourcesResponse' {nextToken} -> nextToken) (\s@SearchDataSourcesResponse' {} a -> s {nextToken = a} :: SearchDataSourcesResponse)

-- | The Amazon Web Services request ID for this operation.
searchDataSourcesResponse_requestId :: Lens.Lens' SearchDataSourcesResponse (Prelude.Maybe Prelude.Text)
searchDataSourcesResponse_requestId = Lens.lens (\SearchDataSourcesResponse' {requestId} -> requestId) (\s@SearchDataSourcesResponse' {} a -> s {requestId = a} :: SearchDataSourcesResponse)

-- | A @DataSourceSummaries@ object that returns a summary of a data source.
searchDataSourcesResponse_dataSourceSummaries :: Lens.Lens' SearchDataSourcesResponse (Prelude.Maybe [DataSourceSummary])
searchDataSourcesResponse_dataSourceSummaries = Lens.lens (\SearchDataSourcesResponse' {dataSourceSummaries} -> dataSourceSummaries) (\s@SearchDataSourcesResponse' {} a -> s {dataSourceSummaries = a} :: SearchDataSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
searchDataSourcesResponse_status :: Lens.Lens' SearchDataSourcesResponse Prelude.Int
searchDataSourcesResponse_status = Lens.lens (\SearchDataSourcesResponse' {status} -> status) (\s@SearchDataSourcesResponse' {} a -> s {status = a} :: SearchDataSourcesResponse)

instance Prelude.NFData SearchDataSourcesResponse where
  rnf SearchDataSourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf dataSourceSummaries
      `Prelude.seq` Prelude.rnf status
