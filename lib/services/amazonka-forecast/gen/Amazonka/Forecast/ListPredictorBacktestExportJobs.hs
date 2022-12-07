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
-- Module      : Amazonka.Forecast.ListPredictorBacktestExportJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of predictor backtest export jobs created using the
-- CreatePredictorBacktestExportJob operation. This operation returns a
-- summary for each backtest export job. You can filter the list using an
-- array of Filter objects.
--
-- To retrieve the complete set of properties for a particular backtest
-- export job, use the ARN with the DescribePredictorBacktestExportJob
-- operation.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListPredictorBacktestExportJobs
  ( -- * Creating a Request
    ListPredictorBacktestExportJobs (..),
    newListPredictorBacktestExportJobs,

    -- * Request Lenses
    listPredictorBacktestExportJobs_nextToken,
    listPredictorBacktestExportJobs_filters,
    listPredictorBacktestExportJobs_maxResults,

    -- * Destructuring the Response
    ListPredictorBacktestExportJobsResponse (..),
    newListPredictorBacktestExportJobsResponse,

    -- * Response Lenses
    listPredictorBacktestExportJobsResponse_nextToken,
    listPredictorBacktestExportJobsResponse_predictorBacktestExportJobs,
    listPredictorBacktestExportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPredictorBacktestExportJobs' smart constructor.
data ListPredictorBacktestExportJobs = ListPredictorBacktestExportJobs'
  { -- | If the result of the previous request was truncated, the response
    -- includes a NextToken. To retrieve the next set of results, use the token
    -- in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of filters. For each filter, provide a condition and a match
    -- statement. The condition is either @IS@ or @IS_NOT@, which specifies
    -- whether to include or exclude the predictor backtest export jobs that
    -- match the statement from the list. The match statement consists of a key
    -- and a value.
    --
    -- __Filter properties__
    --
    -- -   @Condition@ - The condition to apply. Valid values are @IS@ and
    --     @IS_NOT@. To include the predictor backtest export jobs that match
    --     the statement, specify @IS@. To exclude matching predictor backtest
    --     export jobs, specify @IS_NOT@.
    --
    -- -   @Key@ - The name of the parameter to filter on. Valid values are
    --     @PredictorArn@ and @Status@.
    --
    -- -   @Value@ - The value to match.
    filters :: Prelude.Maybe [Filter],
    -- | The number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPredictorBacktestExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPredictorBacktestExportJobs_nextToken' - If the result of the previous request was truncated, the response
-- includes a NextToken. To retrieve the next set of results, use the token
-- in the next request. Tokens expire after 24 hours.
--
-- 'filters', 'listPredictorBacktestExportJobs_filters' - An array of filters. For each filter, provide a condition and a match
-- statement. The condition is either @IS@ or @IS_NOT@, which specifies
-- whether to include or exclude the predictor backtest export jobs that
-- match the statement from the list. The match statement consists of a key
-- and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the predictor backtest export jobs that match
--     the statement, specify @IS@. To exclude matching predictor backtest
--     export jobs, specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @PredictorArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- 'maxResults', 'listPredictorBacktestExportJobs_maxResults' - The number of items to return in the response.
newListPredictorBacktestExportJobs ::
  ListPredictorBacktestExportJobs
newListPredictorBacktestExportJobs =
  ListPredictorBacktestExportJobs'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the result of the previous request was truncated, the response
-- includes a NextToken. To retrieve the next set of results, use the token
-- in the next request. Tokens expire after 24 hours.
listPredictorBacktestExportJobs_nextToken :: Lens.Lens' ListPredictorBacktestExportJobs (Prelude.Maybe Prelude.Text)
listPredictorBacktestExportJobs_nextToken = Lens.lens (\ListPredictorBacktestExportJobs' {nextToken} -> nextToken) (\s@ListPredictorBacktestExportJobs' {} a -> s {nextToken = a} :: ListPredictorBacktestExportJobs)

-- | An array of filters. For each filter, provide a condition and a match
-- statement. The condition is either @IS@ or @IS_NOT@, which specifies
-- whether to include or exclude the predictor backtest export jobs that
-- match the statement from the list. The match statement consists of a key
-- and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the predictor backtest export jobs that match
--     the statement, specify @IS@. To exclude matching predictor backtest
--     export jobs, specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @PredictorArn@ and @Status@.
--
-- -   @Value@ - The value to match.
listPredictorBacktestExportJobs_filters :: Lens.Lens' ListPredictorBacktestExportJobs (Prelude.Maybe [Filter])
listPredictorBacktestExportJobs_filters = Lens.lens (\ListPredictorBacktestExportJobs' {filters} -> filters) (\s@ListPredictorBacktestExportJobs' {} a -> s {filters = a} :: ListPredictorBacktestExportJobs) Prelude.. Lens.mapping Lens.coerced

-- | The number of items to return in the response.
listPredictorBacktestExportJobs_maxResults :: Lens.Lens' ListPredictorBacktestExportJobs (Prelude.Maybe Prelude.Natural)
listPredictorBacktestExportJobs_maxResults = Lens.lens (\ListPredictorBacktestExportJobs' {maxResults} -> maxResults) (\s@ListPredictorBacktestExportJobs' {} a -> s {maxResults = a} :: ListPredictorBacktestExportJobs)

instance
  Core.AWSPager
    ListPredictorBacktestExportJobs
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPredictorBacktestExportJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPredictorBacktestExportJobsResponse_predictorBacktestExportJobs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPredictorBacktestExportJobs_nextToken
          Lens..~ rs
          Lens.^? listPredictorBacktestExportJobsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListPredictorBacktestExportJobs
  where
  type
    AWSResponse ListPredictorBacktestExportJobs =
      ListPredictorBacktestExportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPredictorBacktestExportJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "PredictorBacktestExportJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPredictorBacktestExportJobs
  where
  hashWithSalt
    _salt
    ListPredictorBacktestExportJobs' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListPredictorBacktestExportJobs
  where
  rnf ListPredictorBacktestExportJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance
  Data.ToHeaders
    ListPredictorBacktestExportJobs
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListPredictorBacktestExportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPredictorBacktestExportJobs where
  toJSON ListPredictorBacktestExportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListPredictorBacktestExportJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPredictorBacktestExportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPredictorBacktestExportJobsResponse' smart constructor.
data ListPredictorBacktestExportJobsResponse = ListPredictorBacktestExportJobsResponse'
  { -- | Returns this token if the response is truncated. To retrieve the next
    -- set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that summarize the properties of each predictor
    -- backtest export job.
    predictorBacktestExportJobs :: Prelude.Maybe [PredictorBacktestExportJobSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPredictorBacktestExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPredictorBacktestExportJobsResponse_nextToken' - Returns this token if the response is truncated. To retrieve the next
-- set of results, use the token in the next request.
--
-- 'predictorBacktestExportJobs', 'listPredictorBacktestExportJobsResponse_predictorBacktestExportJobs' - An array of objects that summarize the properties of each predictor
-- backtest export job.
--
-- 'httpStatus', 'listPredictorBacktestExportJobsResponse_httpStatus' - The response's http status code.
newListPredictorBacktestExportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPredictorBacktestExportJobsResponse
newListPredictorBacktestExportJobsResponse
  pHttpStatus_ =
    ListPredictorBacktestExportJobsResponse'
      { nextToken =
          Prelude.Nothing,
        predictorBacktestExportJobs =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns this token if the response is truncated. To retrieve the next
-- set of results, use the token in the next request.
listPredictorBacktestExportJobsResponse_nextToken :: Lens.Lens' ListPredictorBacktestExportJobsResponse (Prelude.Maybe Prelude.Text)
listPredictorBacktestExportJobsResponse_nextToken = Lens.lens (\ListPredictorBacktestExportJobsResponse' {nextToken} -> nextToken) (\s@ListPredictorBacktestExportJobsResponse' {} a -> s {nextToken = a} :: ListPredictorBacktestExportJobsResponse)

-- | An array of objects that summarize the properties of each predictor
-- backtest export job.
listPredictorBacktestExportJobsResponse_predictorBacktestExportJobs :: Lens.Lens' ListPredictorBacktestExportJobsResponse (Prelude.Maybe [PredictorBacktestExportJobSummary])
listPredictorBacktestExportJobsResponse_predictorBacktestExportJobs = Lens.lens (\ListPredictorBacktestExportJobsResponse' {predictorBacktestExportJobs} -> predictorBacktestExportJobs) (\s@ListPredictorBacktestExportJobsResponse' {} a -> s {predictorBacktestExportJobs = a} :: ListPredictorBacktestExportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPredictorBacktestExportJobsResponse_httpStatus :: Lens.Lens' ListPredictorBacktestExportJobsResponse Prelude.Int
listPredictorBacktestExportJobsResponse_httpStatus = Lens.lens (\ListPredictorBacktestExportJobsResponse' {httpStatus} -> httpStatus) (\s@ListPredictorBacktestExportJobsResponse' {} a -> s {httpStatus = a} :: ListPredictorBacktestExportJobsResponse)

instance
  Prelude.NFData
    ListPredictorBacktestExportJobsResponse
  where
  rnf ListPredictorBacktestExportJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf predictorBacktestExportJobs
      `Prelude.seq` Prelude.rnf httpStatus
