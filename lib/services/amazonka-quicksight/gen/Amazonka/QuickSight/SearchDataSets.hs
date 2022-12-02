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
-- Module      : Amazonka.QuickSight.SearchDataSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @SearchDataSets@ operation to search for datasets that belong to
-- an account.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.SearchDataSets
  ( -- * Creating a Request
    SearchDataSets (..),
    newSearchDataSets,

    -- * Request Lenses
    searchDataSets_nextToken,
    searchDataSets_maxResults,
    searchDataSets_awsAccountId,
    searchDataSets_filters,

    -- * Destructuring the Response
    SearchDataSetsResponse (..),
    newSearchDataSetsResponse,

    -- * Response Lenses
    searchDataSetsResponse_nextToken,
    searchDataSetsResponse_dataSetSummaries,
    searchDataSetsResponse_requestId,
    searchDataSetsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchDataSets' smart constructor.
data SearchDataSets = SearchDataSets'
  { -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The filters to apply to the search.
    filters :: Prelude.NonEmpty DataSetSearchFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDataSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchDataSets_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'maxResults', 'searchDataSets_maxResults' - The maximum number of results to be returned per request.
--
-- 'awsAccountId', 'searchDataSets_awsAccountId' - The Amazon Web Services account ID.
--
-- 'filters', 'searchDataSets_filters' - The filters to apply to the search.
newSearchDataSets ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'filters'
  Prelude.NonEmpty DataSetSearchFilter ->
  SearchDataSets
newSearchDataSets pAwsAccountId_ pFilters_ =
  SearchDataSets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      filters = Lens.coerced Lens.# pFilters_
    }

-- | A pagination token that can be used in a subsequent request.
searchDataSets_nextToken :: Lens.Lens' SearchDataSets (Prelude.Maybe Prelude.Text)
searchDataSets_nextToken = Lens.lens (\SearchDataSets' {nextToken} -> nextToken) (\s@SearchDataSets' {} a -> s {nextToken = a} :: SearchDataSets)

-- | The maximum number of results to be returned per request.
searchDataSets_maxResults :: Lens.Lens' SearchDataSets (Prelude.Maybe Prelude.Natural)
searchDataSets_maxResults = Lens.lens (\SearchDataSets' {maxResults} -> maxResults) (\s@SearchDataSets' {} a -> s {maxResults = a} :: SearchDataSets)

-- | The Amazon Web Services account ID.
searchDataSets_awsAccountId :: Lens.Lens' SearchDataSets Prelude.Text
searchDataSets_awsAccountId = Lens.lens (\SearchDataSets' {awsAccountId} -> awsAccountId) (\s@SearchDataSets' {} a -> s {awsAccountId = a} :: SearchDataSets)

-- | The filters to apply to the search.
searchDataSets_filters :: Lens.Lens' SearchDataSets (Prelude.NonEmpty DataSetSearchFilter)
searchDataSets_filters = Lens.lens (\SearchDataSets' {filters} -> filters) (\s@SearchDataSets' {} a -> s {filters = a} :: SearchDataSets) Prelude.. Lens.coerced

instance Core.AWSPager SearchDataSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchDataSetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchDataSetsResponse_dataSetSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchDataSets_nextToken
          Lens..~ rs
          Lens.^? searchDataSetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest SearchDataSets where
  type
    AWSResponse SearchDataSets =
      SearchDataSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchDataSetsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "DataSetSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchDataSets where
  hashWithSalt _salt SearchDataSets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchDataSets where
  rnf SearchDataSets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders SearchDataSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchDataSets where
  toJSON SearchDataSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("Filters" Data..= filters)
          ]
      )

instance Data.ToPath SearchDataSets where
  toPath SearchDataSets' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/search/data-sets"
      ]

instance Data.ToQuery SearchDataSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchDataSetsResponse' smart constructor.
data SearchDataSetsResponse = SearchDataSetsResponse'
  { -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A @DataSetSummaries@ object that returns a summary of a dataset.
    dataSetSummaries :: Prelude.Maybe [DataSetSummary],
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDataSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchDataSetsResponse_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'dataSetSummaries', 'searchDataSetsResponse_dataSetSummaries' - A @DataSetSummaries@ object that returns a summary of a dataset.
--
-- 'requestId', 'searchDataSetsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'searchDataSetsResponse_status' - The HTTP status of the request.
newSearchDataSetsResponse ::
  -- | 'status'
  Prelude.Int ->
  SearchDataSetsResponse
newSearchDataSetsResponse pStatus_ =
  SearchDataSetsResponse'
    { nextToken =
        Prelude.Nothing,
      dataSetSummaries = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A pagination token that can be used in a subsequent request.
searchDataSetsResponse_nextToken :: Lens.Lens' SearchDataSetsResponse (Prelude.Maybe Prelude.Text)
searchDataSetsResponse_nextToken = Lens.lens (\SearchDataSetsResponse' {nextToken} -> nextToken) (\s@SearchDataSetsResponse' {} a -> s {nextToken = a} :: SearchDataSetsResponse)

-- | A @DataSetSummaries@ object that returns a summary of a dataset.
searchDataSetsResponse_dataSetSummaries :: Lens.Lens' SearchDataSetsResponse (Prelude.Maybe [DataSetSummary])
searchDataSetsResponse_dataSetSummaries = Lens.lens (\SearchDataSetsResponse' {dataSetSummaries} -> dataSetSummaries) (\s@SearchDataSetsResponse' {} a -> s {dataSetSummaries = a} :: SearchDataSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
searchDataSetsResponse_requestId :: Lens.Lens' SearchDataSetsResponse (Prelude.Maybe Prelude.Text)
searchDataSetsResponse_requestId = Lens.lens (\SearchDataSetsResponse' {requestId} -> requestId) (\s@SearchDataSetsResponse' {} a -> s {requestId = a} :: SearchDataSetsResponse)

-- | The HTTP status of the request.
searchDataSetsResponse_status :: Lens.Lens' SearchDataSetsResponse Prelude.Int
searchDataSetsResponse_status = Lens.lens (\SearchDataSetsResponse' {status} -> status) (\s@SearchDataSetsResponse' {} a -> s {status = a} :: SearchDataSetsResponse)

instance Prelude.NFData SearchDataSetsResponse where
  rnf SearchDataSetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dataSetSummaries
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
