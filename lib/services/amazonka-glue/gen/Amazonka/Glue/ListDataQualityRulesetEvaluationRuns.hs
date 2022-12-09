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
-- Module      : Amazonka.Glue.ListDataQualityRulesetEvaluationRuns
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the runs meeting the filter criteria, where a ruleset is
-- evaluated against a data source.
module Amazonka.Glue.ListDataQualityRulesetEvaluationRuns
  ( -- * Creating a Request
    ListDataQualityRulesetEvaluationRuns (..),
    newListDataQualityRulesetEvaluationRuns,

    -- * Request Lenses
    listDataQualityRulesetEvaluationRuns_filter,
    listDataQualityRulesetEvaluationRuns_maxResults,
    listDataQualityRulesetEvaluationRuns_nextToken,

    -- * Destructuring the Response
    ListDataQualityRulesetEvaluationRunsResponse (..),
    newListDataQualityRulesetEvaluationRunsResponse,

    -- * Response Lenses
    listDataQualityRulesetEvaluationRunsResponse_nextToken,
    listDataQualityRulesetEvaluationRunsResponse_runs,
    listDataQualityRulesetEvaluationRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataQualityRulesetEvaluationRuns' smart constructor.
data ListDataQualityRulesetEvaluationRuns = ListDataQualityRulesetEvaluationRuns'
  { -- | The filter criteria.
    filter' :: Prelude.Maybe DataQualityRulesetEvaluationRunFilter,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A paginated token to offset the results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataQualityRulesetEvaluationRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listDataQualityRulesetEvaluationRuns_filter' - The filter criteria.
--
-- 'maxResults', 'listDataQualityRulesetEvaluationRuns_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listDataQualityRulesetEvaluationRuns_nextToken' - A paginated token to offset the results.
newListDataQualityRulesetEvaluationRuns ::
  ListDataQualityRulesetEvaluationRuns
newListDataQualityRulesetEvaluationRuns =
  ListDataQualityRulesetEvaluationRuns'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The filter criteria.
listDataQualityRulesetEvaluationRuns_filter :: Lens.Lens' ListDataQualityRulesetEvaluationRuns (Prelude.Maybe DataQualityRulesetEvaluationRunFilter)
listDataQualityRulesetEvaluationRuns_filter = Lens.lens (\ListDataQualityRulesetEvaluationRuns' {filter'} -> filter') (\s@ListDataQualityRulesetEvaluationRuns' {} a -> s {filter' = a} :: ListDataQualityRulesetEvaluationRuns)

-- | The maximum number of results to return.
listDataQualityRulesetEvaluationRuns_maxResults :: Lens.Lens' ListDataQualityRulesetEvaluationRuns (Prelude.Maybe Prelude.Natural)
listDataQualityRulesetEvaluationRuns_maxResults = Lens.lens (\ListDataQualityRulesetEvaluationRuns' {maxResults} -> maxResults) (\s@ListDataQualityRulesetEvaluationRuns' {} a -> s {maxResults = a} :: ListDataQualityRulesetEvaluationRuns)

-- | A paginated token to offset the results.
listDataQualityRulesetEvaluationRuns_nextToken :: Lens.Lens' ListDataQualityRulesetEvaluationRuns (Prelude.Maybe Prelude.Text)
listDataQualityRulesetEvaluationRuns_nextToken = Lens.lens (\ListDataQualityRulesetEvaluationRuns' {nextToken} -> nextToken) (\s@ListDataQualityRulesetEvaluationRuns' {} a -> s {nextToken = a} :: ListDataQualityRulesetEvaluationRuns)

instance
  Core.AWSRequest
    ListDataQualityRulesetEvaluationRuns
  where
  type
    AWSResponse ListDataQualityRulesetEvaluationRuns =
      ListDataQualityRulesetEvaluationRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataQualityRulesetEvaluationRunsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "Runs" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDataQualityRulesetEvaluationRuns
  where
  hashWithSalt
    _salt
    ListDataQualityRulesetEvaluationRuns' {..} =
      _salt `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListDataQualityRulesetEvaluationRuns
  where
  rnf ListDataQualityRulesetEvaluationRuns' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListDataQualityRulesetEvaluationRuns
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.ListDataQualityRulesetEvaluationRuns" ::
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
    ListDataQualityRulesetEvaluationRuns
  where
  toJSON ListDataQualityRulesetEvaluationRuns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    ListDataQualityRulesetEvaluationRuns
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListDataQualityRulesetEvaluationRuns
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataQualityRulesetEvaluationRunsResponse' smart constructor.
data ListDataQualityRulesetEvaluationRunsResponse = ListDataQualityRulesetEvaluationRunsResponse'
  { -- | A pagination token, if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @DataQualityRulesetEvaluationRunDescription@ objects
    -- representing data quality ruleset runs.
    runs :: Prelude.Maybe [DataQualityRulesetEvaluationRunDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataQualityRulesetEvaluationRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataQualityRulesetEvaluationRunsResponse_nextToken' - A pagination token, if more results are available.
--
-- 'runs', 'listDataQualityRulesetEvaluationRunsResponse_runs' - A list of @DataQualityRulesetEvaluationRunDescription@ objects
-- representing data quality ruleset runs.
--
-- 'httpStatus', 'listDataQualityRulesetEvaluationRunsResponse_httpStatus' - The response's http status code.
newListDataQualityRulesetEvaluationRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataQualityRulesetEvaluationRunsResponse
newListDataQualityRulesetEvaluationRunsResponse
  pHttpStatus_ =
    ListDataQualityRulesetEvaluationRunsResponse'
      { nextToken =
          Prelude.Nothing,
        runs = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A pagination token, if more results are available.
listDataQualityRulesetEvaluationRunsResponse_nextToken :: Lens.Lens' ListDataQualityRulesetEvaluationRunsResponse (Prelude.Maybe Prelude.Text)
listDataQualityRulesetEvaluationRunsResponse_nextToken = Lens.lens (\ListDataQualityRulesetEvaluationRunsResponse' {nextToken} -> nextToken) (\s@ListDataQualityRulesetEvaluationRunsResponse' {} a -> s {nextToken = a} :: ListDataQualityRulesetEvaluationRunsResponse)

-- | A list of @DataQualityRulesetEvaluationRunDescription@ objects
-- representing data quality ruleset runs.
listDataQualityRulesetEvaluationRunsResponse_runs :: Lens.Lens' ListDataQualityRulesetEvaluationRunsResponse (Prelude.Maybe [DataQualityRulesetEvaluationRunDescription])
listDataQualityRulesetEvaluationRunsResponse_runs = Lens.lens (\ListDataQualityRulesetEvaluationRunsResponse' {runs} -> runs) (\s@ListDataQualityRulesetEvaluationRunsResponse' {} a -> s {runs = a} :: ListDataQualityRulesetEvaluationRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDataQualityRulesetEvaluationRunsResponse_httpStatus :: Lens.Lens' ListDataQualityRulesetEvaluationRunsResponse Prelude.Int
listDataQualityRulesetEvaluationRunsResponse_httpStatus = Lens.lens (\ListDataQualityRulesetEvaluationRunsResponse' {httpStatus} -> httpStatus) (\s@ListDataQualityRulesetEvaluationRunsResponse' {} a -> s {httpStatus = a} :: ListDataQualityRulesetEvaluationRunsResponse)

instance
  Prelude.NFData
    ListDataQualityRulesetEvaluationRunsResponse
  where
  rnf ListDataQualityRulesetEvaluationRunsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf runs
      `Prelude.seq` Prelude.rnf httpStatus
