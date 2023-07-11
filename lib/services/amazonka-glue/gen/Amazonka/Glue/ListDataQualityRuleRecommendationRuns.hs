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
-- Module      : Amazonka.Glue.ListDataQualityRuleRecommendationRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recommendation runs meeting the filter criteria.
module Amazonka.Glue.ListDataQualityRuleRecommendationRuns
  ( -- * Creating a Request
    ListDataQualityRuleRecommendationRuns (..),
    newListDataQualityRuleRecommendationRuns,

    -- * Request Lenses
    listDataQualityRuleRecommendationRuns_filter,
    listDataQualityRuleRecommendationRuns_maxResults,
    listDataQualityRuleRecommendationRuns_nextToken,

    -- * Destructuring the Response
    ListDataQualityRuleRecommendationRunsResponse (..),
    newListDataQualityRuleRecommendationRunsResponse,

    -- * Response Lenses
    listDataQualityRuleRecommendationRunsResponse_nextToken,
    listDataQualityRuleRecommendationRunsResponse_runs,
    listDataQualityRuleRecommendationRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataQualityRuleRecommendationRuns' smart constructor.
data ListDataQualityRuleRecommendationRuns = ListDataQualityRuleRecommendationRuns'
  { -- | The filter criteria.
    filter' :: Prelude.Maybe DataQualityRuleRecommendationRunFilter,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A paginated token to offset the results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataQualityRuleRecommendationRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listDataQualityRuleRecommendationRuns_filter' - The filter criteria.
--
-- 'maxResults', 'listDataQualityRuleRecommendationRuns_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listDataQualityRuleRecommendationRuns_nextToken' - A paginated token to offset the results.
newListDataQualityRuleRecommendationRuns ::
  ListDataQualityRuleRecommendationRuns
newListDataQualityRuleRecommendationRuns =
  ListDataQualityRuleRecommendationRuns'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The filter criteria.
listDataQualityRuleRecommendationRuns_filter :: Lens.Lens' ListDataQualityRuleRecommendationRuns (Prelude.Maybe DataQualityRuleRecommendationRunFilter)
listDataQualityRuleRecommendationRuns_filter = Lens.lens (\ListDataQualityRuleRecommendationRuns' {filter'} -> filter') (\s@ListDataQualityRuleRecommendationRuns' {} a -> s {filter' = a} :: ListDataQualityRuleRecommendationRuns)

-- | The maximum number of results to return.
listDataQualityRuleRecommendationRuns_maxResults :: Lens.Lens' ListDataQualityRuleRecommendationRuns (Prelude.Maybe Prelude.Natural)
listDataQualityRuleRecommendationRuns_maxResults = Lens.lens (\ListDataQualityRuleRecommendationRuns' {maxResults} -> maxResults) (\s@ListDataQualityRuleRecommendationRuns' {} a -> s {maxResults = a} :: ListDataQualityRuleRecommendationRuns)

-- | A paginated token to offset the results.
listDataQualityRuleRecommendationRuns_nextToken :: Lens.Lens' ListDataQualityRuleRecommendationRuns (Prelude.Maybe Prelude.Text)
listDataQualityRuleRecommendationRuns_nextToken = Lens.lens (\ListDataQualityRuleRecommendationRuns' {nextToken} -> nextToken) (\s@ListDataQualityRuleRecommendationRuns' {} a -> s {nextToken = a} :: ListDataQualityRuleRecommendationRuns)

instance
  Core.AWSRequest
    ListDataQualityRuleRecommendationRuns
  where
  type
    AWSResponse
      ListDataQualityRuleRecommendationRuns =
      ListDataQualityRuleRecommendationRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataQualityRuleRecommendationRunsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Runs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDataQualityRuleRecommendationRuns
  where
  hashWithSalt
    _salt
    ListDataQualityRuleRecommendationRuns' {..} =
      _salt
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListDataQualityRuleRecommendationRuns
  where
  rnf ListDataQualityRuleRecommendationRuns' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListDataQualityRuleRecommendationRuns
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.ListDataQualityRuleRecommendationRuns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListDataQualityRuleRecommendationRuns
  where
  toJSON ListDataQualityRuleRecommendationRuns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    ListDataQualityRuleRecommendationRuns
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListDataQualityRuleRecommendationRuns
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataQualityRuleRecommendationRunsResponse' smart constructor.
data ListDataQualityRuleRecommendationRunsResponse = ListDataQualityRuleRecommendationRunsResponse'
  { -- | A pagination token, if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @DataQualityRuleRecommendationRunDescription@ objects.
    runs :: Prelude.Maybe [DataQualityRuleRecommendationRunDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataQualityRuleRecommendationRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataQualityRuleRecommendationRunsResponse_nextToken' - A pagination token, if more results are available.
--
-- 'runs', 'listDataQualityRuleRecommendationRunsResponse_runs' - A list of @DataQualityRuleRecommendationRunDescription@ objects.
--
-- 'httpStatus', 'listDataQualityRuleRecommendationRunsResponse_httpStatus' - The response's http status code.
newListDataQualityRuleRecommendationRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataQualityRuleRecommendationRunsResponse
newListDataQualityRuleRecommendationRunsResponse
  pHttpStatus_ =
    ListDataQualityRuleRecommendationRunsResponse'
      { nextToken =
          Prelude.Nothing,
        runs = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A pagination token, if more results are available.
listDataQualityRuleRecommendationRunsResponse_nextToken :: Lens.Lens' ListDataQualityRuleRecommendationRunsResponse (Prelude.Maybe Prelude.Text)
listDataQualityRuleRecommendationRunsResponse_nextToken = Lens.lens (\ListDataQualityRuleRecommendationRunsResponse' {nextToken} -> nextToken) (\s@ListDataQualityRuleRecommendationRunsResponse' {} a -> s {nextToken = a} :: ListDataQualityRuleRecommendationRunsResponse)

-- | A list of @DataQualityRuleRecommendationRunDescription@ objects.
listDataQualityRuleRecommendationRunsResponse_runs :: Lens.Lens' ListDataQualityRuleRecommendationRunsResponse (Prelude.Maybe [DataQualityRuleRecommendationRunDescription])
listDataQualityRuleRecommendationRunsResponse_runs = Lens.lens (\ListDataQualityRuleRecommendationRunsResponse' {runs} -> runs) (\s@ListDataQualityRuleRecommendationRunsResponse' {} a -> s {runs = a} :: ListDataQualityRuleRecommendationRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDataQualityRuleRecommendationRunsResponse_httpStatus :: Lens.Lens' ListDataQualityRuleRecommendationRunsResponse Prelude.Int
listDataQualityRuleRecommendationRunsResponse_httpStatus = Lens.lens (\ListDataQualityRuleRecommendationRunsResponse' {httpStatus} -> httpStatus) (\s@ListDataQualityRuleRecommendationRunsResponse' {} a -> s {httpStatus = a} :: ListDataQualityRuleRecommendationRunsResponse)

instance
  Prelude.NFData
    ListDataQualityRuleRecommendationRunsResponse
  where
  rnf
    ListDataQualityRuleRecommendationRunsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf runs
        `Prelude.seq` Prelude.rnf httpStatus
