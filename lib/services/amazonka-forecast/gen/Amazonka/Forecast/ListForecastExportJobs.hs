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
-- Module      : Amazonka.Forecast.ListForecastExportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of forecast export jobs created using the
-- CreateForecastExportJob operation. For each forecast export job, this
-- operation returns a summary of its properties, including its Amazon
-- Resource Name (ARN). To retrieve the complete set of properties, use the
-- ARN with the DescribeForecastExportJob operation. You can filter the
-- list using an array of Filter objects.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListForecastExportJobs
  ( -- * Creating a Request
    ListForecastExportJobs (..),
    newListForecastExportJobs,

    -- * Request Lenses
    listForecastExportJobs_filters,
    listForecastExportJobs_maxResults,
    listForecastExportJobs_nextToken,

    -- * Destructuring the Response
    ListForecastExportJobsResponse (..),
    newListForecastExportJobsResponse,

    -- * Response Lenses
    listForecastExportJobsResponse_forecastExportJobs,
    listForecastExportJobsResponse_nextToken,
    listForecastExportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListForecastExportJobs' smart constructor.
data ListForecastExportJobs = ListForecastExportJobs'
  { -- | An array of filters. For each filter, you provide a condition and a
    -- match statement. The condition is either @IS@ or @IS_NOT@, which
    -- specifies whether to include or exclude the forecast export jobs that
    -- match the statement from the list, respectively. The match statement
    -- consists of a key and a value.
    --
    -- __Filter properties__
    --
    -- -   @Condition@ - The condition to apply. Valid values are @IS@ and
    --     @IS_NOT@. To include the forecast export jobs that match the
    --     statement, specify @IS@. To exclude matching forecast export jobs,
    --     specify @IS_NOT@.
    --
    -- -   @Key@ - The name of the parameter to filter on. Valid values are
    --     @ForecastArn@ and @Status@.
    --
    -- -   @Value@ - The value to match.
    --
    -- For example, to list all jobs that export a forecast named
    -- /electricityforecast/, specify the following filter:
    --
    -- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"ForecastArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityforecast\" } ]@
    filters :: Prelude.Maybe [Filter],
    -- | The number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListForecastExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listForecastExportJobs_filters' - An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the forecast export jobs that
-- match the statement from the list, respectively. The match statement
-- consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the forecast export jobs that match the
--     statement, specify @IS@. To exclude matching forecast export jobs,
--     specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @ForecastArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all jobs that export a forecast named
-- /electricityforecast/, specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"ForecastArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityforecast\" } ]@
--
-- 'maxResults', 'listForecastExportJobs_maxResults' - The number of items to return in the response.
--
-- 'nextToken', 'listForecastExportJobs_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
newListForecastExportJobs ::
  ListForecastExportJobs
newListForecastExportJobs =
  ListForecastExportJobs'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the forecast export jobs that
-- match the statement from the list, respectively. The match statement
-- consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the forecast export jobs that match the
--     statement, specify @IS@. To exclude matching forecast export jobs,
--     specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @ForecastArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all jobs that export a forecast named
-- /electricityforecast/, specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"ForecastArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityforecast\" } ]@
listForecastExportJobs_filters :: Lens.Lens' ListForecastExportJobs (Prelude.Maybe [Filter])
listForecastExportJobs_filters = Lens.lens (\ListForecastExportJobs' {filters} -> filters) (\s@ListForecastExportJobs' {} a -> s {filters = a} :: ListForecastExportJobs) Prelude.. Lens.mapping Lens.coerced

-- | The number of items to return in the response.
listForecastExportJobs_maxResults :: Lens.Lens' ListForecastExportJobs (Prelude.Maybe Prelude.Natural)
listForecastExportJobs_maxResults = Lens.lens (\ListForecastExportJobs' {maxResults} -> maxResults) (\s@ListForecastExportJobs' {} a -> s {maxResults = a} :: ListForecastExportJobs)

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listForecastExportJobs_nextToken :: Lens.Lens' ListForecastExportJobs (Prelude.Maybe Prelude.Text)
listForecastExportJobs_nextToken = Lens.lens (\ListForecastExportJobs' {nextToken} -> nextToken) (\s@ListForecastExportJobs' {} a -> s {nextToken = a} :: ListForecastExportJobs)

instance Core.AWSPager ListForecastExportJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listForecastExportJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listForecastExportJobsResponse_forecastExportJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listForecastExportJobs_nextToken
          Lens..~ rs
          Lens.^? listForecastExportJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListForecastExportJobs where
  type
    AWSResponse ListForecastExportJobs =
      ListForecastExportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListForecastExportJobsResponse'
            Prelude.<$> ( x
                            Data..?> "ForecastExportJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListForecastExportJobs where
  hashWithSalt _salt ListForecastExportJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListForecastExportJobs where
  rnf ListForecastExportJobs' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListForecastExportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListForecastExportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListForecastExportJobs where
  toJSON ListForecastExportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListForecastExportJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListForecastExportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListForecastExportJobsResponse' smart constructor.
data ListForecastExportJobsResponse = ListForecastExportJobsResponse'
  { -- | An array of objects that summarize each export job\'s properties.
    forecastExportJobs :: Prelude.Maybe [ForecastExportJobSummary],
    -- | If the response is truncated, Amazon Forecast returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListForecastExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastExportJobs', 'listForecastExportJobsResponse_forecastExportJobs' - An array of objects that summarize each export job\'s properties.
--
-- 'nextToken', 'listForecastExportJobsResponse_nextToken' - If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
--
-- 'httpStatus', 'listForecastExportJobsResponse_httpStatus' - The response's http status code.
newListForecastExportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListForecastExportJobsResponse
newListForecastExportJobsResponse pHttpStatus_ =
  ListForecastExportJobsResponse'
    { forecastExportJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that summarize each export job\'s properties.
listForecastExportJobsResponse_forecastExportJobs :: Lens.Lens' ListForecastExportJobsResponse (Prelude.Maybe [ForecastExportJobSummary])
listForecastExportJobsResponse_forecastExportJobs = Lens.lens (\ListForecastExportJobsResponse' {forecastExportJobs} -> forecastExportJobs) (\s@ListForecastExportJobsResponse' {} a -> s {forecastExportJobs = a} :: ListForecastExportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
listForecastExportJobsResponse_nextToken :: Lens.Lens' ListForecastExportJobsResponse (Prelude.Maybe Prelude.Text)
listForecastExportJobsResponse_nextToken = Lens.lens (\ListForecastExportJobsResponse' {nextToken} -> nextToken) (\s@ListForecastExportJobsResponse' {} a -> s {nextToken = a} :: ListForecastExportJobsResponse)

-- | The response's http status code.
listForecastExportJobsResponse_httpStatus :: Lens.Lens' ListForecastExportJobsResponse Prelude.Int
listForecastExportJobsResponse_httpStatus = Lens.lens (\ListForecastExportJobsResponse' {httpStatus} -> httpStatus) (\s@ListForecastExportJobsResponse' {} a -> s {httpStatus = a} :: ListForecastExportJobsResponse)

instance
  Prelude.NFData
    ListForecastExportJobsResponse
  where
  rnf ListForecastExportJobsResponse' {..} =
    Prelude.rnf forecastExportJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
