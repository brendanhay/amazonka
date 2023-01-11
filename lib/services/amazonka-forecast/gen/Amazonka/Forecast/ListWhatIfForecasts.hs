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
-- Module      : Amazonka.Forecast.ListWhatIfForecasts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of what-if forecasts created using the
-- CreateWhatIfForecast operation. For each what-if forecast, this
-- operation returns a summary of its properties, including its Amazon
-- Resource Name (ARN). You can retrieve the complete set of properties by
-- using the what-if forecast ARN with the DescribeWhatIfForecast
-- operation.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListWhatIfForecasts
  ( -- * Creating a Request
    ListWhatIfForecasts (..),
    newListWhatIfForecasts,

    -- * Request Lenses
    listWhatIfForecasts_filters,
    listWhatIfForecasts_maxResults,
    listWhatIfForecasts_nextToken,

    -- * Destructuring the Response
    ListWhatIfForecastsResponse (..),
    newListWhatIfForecastsResponse,

    -- * Response Lenses
    listWhatIfForecastsResponse_nextToken,
    listWhatIfForecastsResponse_whatIfForecasts,
    listWhatIfForecastsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWhatIfForecasts' smart constructor.
data ListWhatIfForecasts = ListWhatIfForecasts'
  { -- | An array of filters. For each filter, you provide a condition and a
    -- match statement. The condition is either @IS@ or @IS_NOT@, which
    -- specifies whether to include or exclude the what-if forecast export jobs
    -- that match the statement from the list, respectively. The match
    -- statement consists of a key and a value.
    --
    -- __Filter properties__
    --
    -- -   @Condition@ - The condition to apply. Valid values are @IS@ and
    --     @IS_NOT@. To include the forecast export jobs that match the
    --     statement, specify @IS@. To exclude matching forecast export jobs,
    --     specify @IS_NOT@.
    --
    -- -   @Key@ - The name of the parameter to filter on. Valid values are
    --     @WhatIfForecastArn@ and @Status@.
    --
    -- -   @Value@ - The value to match.
    --
    -- For example, to list all jobs that export a forecast named
    -- /electricityWhatIfForecast/, specify the following filter:
    --
    -- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"WhatIfForecastArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityWhatIfForecast\" } ]@
    filters :: Prelude.Maybe [Filter],
    -- | The number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next  request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWhatIfForecasts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listWhatIfForecasts_filters' - An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the what-if forecast export jobs
-- that match the statement from the list, respectively. The match
-- statement consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the forecast export jobs that match the
--     statement, specify @IS@. To exclude matching forecast export jobs,
--     specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @WhatIfForecastArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all jobs that export a forecast named
-- /electricityWhatIfForecast/, specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"WhatIfForecastArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityWhatIfForecast\" } ]@
--
-- 'maxResults', 'listWhatIfForecasts_maxResults' - The number of items to return in the response.
--
-- 'nextToken', 'listWhatIfForecasts_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next  request. Tokens expire after 24 hours.
newListWhatIfForecasts ::
  ListWhatIfForecasts
newListWhatIfForecasts =
  ListWhatIfForecasts'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the what-if forecast export jobs
-- that match the statement from the list, respectively. The match
-- statement consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the forecast export jobs that match the
--     statement, specify @IS@. To exclude matching forecast export jobs,
--     specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @WhatIfForecastArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all jobs that export a forecast named
-- /electricityWhatIfForecast/, specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"WhatIfForecastArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityWhatIfForecast\" } ]@
listWhatIfForecasts_filters :: Lens.Lens' ListWhatIfForecasts (Prelude.Maybe [Filter])
listWhatIfForecasts_filters = Lens.lens (\ListWhatIfForecasts' {filters} -> filters) (\s@ListWhatIfForecasts' {} a -> s {filters = a} :: ListWhatIfForecasts) Prelude.. Lens.mapping Lens.coerced

-- | The number of items to return in the response.
listWhatIfForecasts_maxResults :: Lens.Lens' ListWhatIfForecasts (Prelude.Maybe Prelude.Natural)
listWhatIfForecasts_maxResults = Lens.lens (\ListWhatIfForecasts' {maxResults} -> maxResults) (\s@ListWhatIfForecasts' {} a -> s {maxResults = a} :: ListWhatIfForecasts)

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next  request. Tokens expire after 24 hours.
listWhatIfForecasts_nextToken :: Lens.Lens' ListWhatIfForecasts (Prelude.Maybe Prelude.Text)
listWhatIfForecasts_nextToken = Lens.lens (\ListWhatIfForecasts' {nextToken} -> nextToken) (\s@ListWhatIfForecasts' {} a -> s {nextToken = a} :: ListWhatIfForecasts)

instance Core.AWSPager ListWhatIfForecasts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWhatIfForecastsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWhatIfForecastsResponse_whatIfForecasts
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWhatIfForecasts_nextToken
          Lens..~ rs
          Lens.^? listWhatIfForecastsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListWhatIfForecasts where
  type
    AWSResponse ListWhatIfForecasts =
      ListWhatIfForecastsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWhatIfForecastsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "WhatIfForecasts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWhatIfForecasts where
  hashWithSalt _salt ListWhatIfForecasts' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListWhatIfForecasts where
  rnf ListWhatIfForecasts' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListWhatIfForecasts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListWhatIfForecasts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWhatIfForecasts where
  toJSON ListWhatIfForecasts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListWhatIfForecasts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListWhatIfForecasts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWhatIfForecastsResponse' smart constructor.
data ListWhatIfForecastsResponse = ListWhatIfForecastsResponse'
  { -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next  request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @WhatIfForecasts@ objects that describe the matched
    -- forecasts.
    whatIfForecasts :: Prelude.Maybe [WhatIfForecastSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWhatIfForecastsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWhatIfForecastsResponse_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next  request. Tokens expire after 24 hours.
--
-- 'whatIfForecasts', 'listWhatIfForecastsResponse_whatIfForecasts' - An array of @WhatIfForecasts@ objects that describe the matched
-- forecasts.
--
-- 'httpStatus', 'listWhatIfForecastsResponse_httpStatus' - The response's http status code.
newListWhatIfForecastsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWhatIfForecastsResponse
newListWhatIfForecastsResponse pHttpStatus_ =
  ListWhatIfForecastsResponse'
    { nextToken =
        Prelude.Nothing,
      whatIfForecasts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next  request. Tokens expire after 24 hours.
listWhatIfForecastsResponse_nextToken :: Lens.Lens' ListWhatIfForecastsResponse (Prelude.Maybe Prelude.Text)
listWhatIfForecastsResponse_nextToken = Lens.lens (\ListWhatIfForecastsResponse' {nextToken} -> nextToken) (\s@ListWhatIfForecastsResponse' {} a -> s {nextToken = a} :: ListWhatIfForecastsResponse)

-- | An array of @WhatIfForecasts@ objects that describe the matched
-- forecasts.
listWhatIfForecastsResponse_whatIfForecasts :: Lens.Lens' ListWhatIfForecastsResponse (Prelude.Maybe [WhatIfForecastSummary])
listWhatIfForecastsResponse_whatIfForecasts = Lens.lens (\ListWhatIfForecastsResponse' {whatIfForecasts} -> whatIfForecasts) (\s@ListWhatIfForecastsResponse' {} a -> s {whatIfForecasts = a} :: ListWhatIfForecastsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWhatIfForecastsResponse_httpStatus :: Lens.Lens' ListWhatIfForecastsResponse Prelude.Int
listWhatIfForecastsResponse_httpStatus = Lens.lens (\ListWhatIfForecastsResponse' {httpStatus} -> httpStatus) (\s@ListWhatIfForecastsResponse' {} a -> s {httpStatus = a} :: ListWhatIfForecastsResponse)

instance Prelude.NFData ListWhatIfForecastsResponse where
  rnf ListWhatIfForecastsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf whatIfForecasts
      `Prelude.seq` Prelude.rnf httpStatus
