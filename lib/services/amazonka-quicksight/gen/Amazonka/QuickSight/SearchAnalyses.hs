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
-- Module      : Amazonka.QuickSight.SearchAnalyses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for analyses that belong to the user specified in the filter.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.SearchAnalyses
  ( -- * Creating a Request
    SearchAnalyses (..),
    newSearchAnalyses,

    -- * Request Lenses
    searchAnalyses_maxResults,
    searchAnalyses_nextToken,
    searchAnalyses_awsAccountId,
    searchAnalyses_filters,

    -- * Destructuring the Response
    SearchAnalysesResponse (..),
    newSearchAnalysesResponse,

    -- * Response Lenses
    searchAnalysesResponse_analysisSummaryList,
    searchAnalysesResponse_nextToken,
    searchAnalysesResponse_requestId,
    searchAnalysesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchAnalyses' smart constructor.
data SearchAnalyses = SearchAnalyses'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the analyses
    -- that you\'re searching for.
    awsAccountId :: Prelude.Text,
    -- | The structure for the search filters that you want to apply to your
    -- search.
    filters :: Prelude.NonEmpty AnalysisSearchFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchAnalyses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchAnalyses_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'searchAnalyses_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'awsAccountId', 'searchAnalyses_awsAccountId' - The ID of the Amazon Web Services account that contains the analyses
-- that you\'re searching for.
--
-- 'filters', 'searchAnalyses_filters' - The structure for the search filters that you want to apply to your
-- search.
newSearchAnalyses ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'filters'
  Prelude.NonEmpty AnalysisSearchFilter ->
  SearchAnalyses
newSearchAnalyses pAwsAccountId_ pFilters_ =
  SearchAnalyses'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      filters = Lens.coerced Lens.# pFilters_
    }

-- | The maximum number of results to return.
searchAnalyses_maxResults :: Lens.Lens' SearchAnalyses (Prelude.Maybe Prelude.Natural)
searchAnalyses_maxResults = Lens.lens (\SearchAnalyses' {maxResults} -> maxResults) (\s@SearchAnalyses' {} a -> s {maxResults = a} :: SearchAnalyses)

-- | A pagination token that can be used in a subsequent request.
searchAnalyses_nextToken :: Lens.Lens' SearchAnalyses (Prelude.Maybe Prelude.Text)
searchAnalyses_nextToken = Lens.lens (\SearchAnalyses' {nextToken} -> nextToken) (\s@SearchAnalyses' {} a -> s {nextToken = a} :: SearchAnalyses)

-- | The ID of the Amazon Web Services account that contains the analyses
-- that you\'re searching for.
searchAnalyses_awsAccountId :: Lens.Lens' SearchAnalyses Prelude.Text
searchAnalyses_awsAccountId = Lens.lens (\SearchAnalyses' {awsAccountId} -> awsAccountId) (\s@SearchAnalyses' {} a -> s {awsAccountId = a} :: SearchAnalyses)

-- | The structure for the search filters that you want to apply to your
-- search.
searchAnalyses_filters :: Lens.Lens' SearchAnalyses (Prelude.NonEmpty AnalysisSearchFilter)
searchAnalyses_filters = Lens.lens (\SearchAnalyses' {filters} -> filters) (\s@SearchAnalyses' {} a -> s {filters = a} :: SearchAnalyses) Prelude.. Lens.coerced

instance Core.AWSPager SearchAnalyses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchAnalysesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchAnalysesResponse_analysisSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchAnalyses_nextToken
          Lens..~ rs
          Lens.^? searchAnalysesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest SearchAnalyses where
  type
    AWSResponse SearchAnalyses =
      SearchAnalysesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchAnalysesResponse'
            Prelude.<$> ( x Data..?> "AnalysisSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchAnalyses where
  hashWithSalt _salt SearchAnalyses' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchAnalyses where
  rnf SearchAnalyses' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders SearchAnalyses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchAnalyses where
  toJSON SearchAnalyses' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Filters" Data..= filters)
          ]
      )

instance Data.ToPath SearchAnalyses where
  toPath SearchAnalyses' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/search/analyses"
      ]

instance Data.ToQuery SearchAnalyses where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchAnalysesResponse' smart constructor.
data SearchAnalysesResponse = SearchAnalysesResponse'
  { -- | Metadata describing the analyses that you searched for.
    analysisSummaryList :: Prelude.Maybe [AnalysisSummary],
    -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchAnalysesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisSummaryList', 'searchAnalysesResponse_analysisSummaryList' - Metadata describing the analyses that you searched for.
--
-- 'nextToken', 'searchAnalysesResponse_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'requestId', 'searchAnalysesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'searchAnalysesResponse_status' - The HTTP status of the request.
newSearchAnalysesResponse ::
  -- | 'status'
  Prelude.Int ->
  SearchAnalysesResponse
newSearchAnalysesResponse pStatus_ =
  SearchAnalysesResponse'
    { analysisSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | Metadata describing the analyses that you searched for.
searchAnalysesResponse_analysisSummaryList :: Lens.Lens' SearchAnalysesResponse (Prelude.Maybe [AnalysisSummary])
searchAnalysesResponse_analysisSummaryList = Lens.lens (\SearchAnalysesResponse' {analysisSummaryList} -> analysisSummaryList) (\s@SearchAnalysesResponse' {} a -> s {analysisSummaryList = a} :: SearchAnalysesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that can be used in a subsequent request.
searchAnalysesResponse_nextToken :: Lens.Lens' SearchAnalysesResponse (Prelude.Maybe Prelude.Text)
searchAnalysesResponse_nextToken = Lens.lens (\SearchAnalysesResponse' {nextToken} -> nextToken) (\s@SearchAnalysesResponse' {} a -> s {nextToken = a} :: SearchAnalysesResponse)

-- | The Amazon Web Services request ID for this operation.
searchAnalysesResponse_requestId :: Lens.Lens' SearchAnalysesResponse (Prelude.Maybe Prelude.Text)
searchAnalysesResponse_requestId = Lens.lens (\SearchAnalysesResponse' {requestId} -> requestId) (\s@SearchAnalysesResponse' {} a -> s {requestId = a} :: SearchAnalysesResponse)

-- | The HTTP status of the request.
searchAnalysesResponse_status :: Lens.Lens' SearchAnalysesResponse Prelude.Int
searchAnalysesResponse_status = Lens.lens (\SearchAnalysesResponse' {status} -> status) (\s@SearchAnalysesResponse' {} a -> s {status = a} :: SearchAnalysesResponse)

instance Prelude.NFData SearchAnalysesResponse where
  rnf SearchAnalysesResponse' {..} =
    Prelude.rnf analysisSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
