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
-- Module      : Amazonka.Forecast.ListForecasts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of forecasts created using the CreateForecast operation.
-- For each forecast, this operation returns a summary of its properties,
-- including its Amazon Resource Name (ARN). To retrieve the complete set
-- of properties, specify the ARN with the DescribeForecast operation. You
-- can filter the list using an array of Filter objects.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListForecasts
  ( -- * Creating a Request
    ListForecasts (..),
    newListForecasts,

    -- * Request Lenses
    listForecasts_filters,
    listForecasts_maxResults,
    listForecasts_nextToken,

    -- * Destructuring the Response
    ListForecastsResponse (..),
    newListForecastsResponse,

    -- * Response Lenses
    listForecastsResponse_forecasts,
    listForecastsResponse_nextToken,
    listForecastsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListForecasts' smart constructor.
data ListForecasts = ListForecasts'
  { -- | An array of filters. For each filter, you provide a condition and a
    -- match statement. The condition is either @IS@ or @IS_NOT@, which
    -- specifies whether to include or exclude the forecasts that match the
    -- statement from the list, respectively. The match statement consists of a
    -- key and a value.
    --
    -- __Filter properties__
    --
    -- -   @Condition@ - The condition to apply. Valid values are @IS@ and
    --     @IS_NOT@. To include the forecasts that match the statement, specify
    --     @IS@. To exclude matching forecasts, specify @IS_NOT@.
    --
    -- -   @Key@ - The name of the parameter to filter on. Valid values are
    --     @DatasetGroupArn@, @PredictorArn@, and @Status@.
    --
    -- -   @Value@ - The value to match.
    --
    -- For example, to list all forecasts whose status is not ACTIVE, you would
    -- specify:
    --
    -- @\"Filters\": [ { \"Condition\": \"IS_NOT\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
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
-- Create a value of 'ListForecasts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listForecasts_filters' - An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the forecasts that match the
-- statement from the list, respectively. The match statement consists of a
-- key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the forecasts that match the statement, specify
--     @IS@. To exclude matching forecasts, specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @DatasetGroupArn@, @PredictorArn@, and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all forecasts whose status is not ACTIVE, you would
-- specify:
--
-- @\"Filters\": [ { \"Condition\": \"IS_NOT\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
--
-- 'maxResults', 'listForecasts_maxResults' - The number of items to return in the response.
--
-- 'nextToken', 'listForecasts_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
newListForecasts ::
  ListForecasts
newListForecasts =
  ListForecasts'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the forecasts that match the
-- statement from the list, respectively. The match statement consists of a
-- key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the forecasts that match the statement, specify
--     @IS@. To exclude matching forecasts, specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @DatasetGroupArn@, @PredictorArn@, and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all forecasts whose status is not ACTIVE, you would
-- specify:
--
-- @\"Filters\": [ { \"Condition\": \"IS_NOT\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
listForecasts_filters :: Lens.Lens' ListForecasts (Prelude.Maybe [Filter])
listForecasts_filters = Lens.lens (\ListForecasts' {filters} -> filters) (\s@ListForecasts' {} a -> s {filters = a} :: ListForecasts) Prelude.. Lens.mapping Lens.coerced

-- | The number of items to return in the response.
listForecasts_maxResults :: Lens.Lens' ListForecasts (Prelude.Maybe Prelude.Natural)
listForecasts_maxResults = Lens.lens (\ListForecasts' {maxResults} -> maxResults) (\s@ListForecasts' {} a -> s {maxResults = a} :: ListForecasts)

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listForecasts_nextToken :: Lens.Lens' ListForecasts (Prelude.Maybe Prelude.Text)
listForecasts_nextToken = Lens.lens (\ListForecasts' {nextToken} -> nextToken) (\s@ListForecasts' {} a -> s {nextToken = a} :: ListForecasts)

instance Core.AWSPager ListForecasts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listForecastsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listForecastsResponse_forecasts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listForecasts_nextToken
          Lens..~ rs
          Lens.^? listForecastsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListForecasts where
  type
    AWSResponse ListForecasts =
      ListForecastsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListForecastsResponse'
            Prelude.<$> (x Data..?> "Forecasts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListForecasts where
  hashWithSalt _salt ListForecasts' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListForecasts where
  rnf ListForecasts' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListForecasts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListForecasts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListForecasts where
  toJSON ListForecasts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListForecasts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListForecasts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListForecastsResponse' smart constructor.
data ListForecastsResponse = ListForecastsResponse'
  { -- | An array of objects that summarize each forecast\'s properties.
    forecasts :: Prelude.Maybe [ForecastSummary],
    -- | If the response is truncated, Amazon Forecast returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListForecastsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecasts', 'listForecastsResponse_forecasts' - An array of objects that summarize each forecast\'s properties.
--
-- 'nextToken', 'listForecastsResponse_nextToken' - If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
--
-- 'httpStatus', 'listForecastsResponse_httpStatus' - The response's http status code.
newListForecastsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListForecastsResponse
newListForecastsResponse pHttpStatus_ =
  ListForecastsResponse'
    { forecasts = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that summarize each forecast\'s properties.
listForecastsResponse_forecasts :: Lens.Lens' ListForecastsResponse (Prelude.Maybe [ForecastSummary])
listForecastsResponse_forecasts = Lens.lens (\ListForecastsResponse' {forecasts} -> forecasts) (\s@ListForecastsResponse' {} a -> s {forecasts = a} :: ListForecastsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
listForecastsResponse_nextToken :: Lens.Lens' ListForecastsResponse (Prelude.Maybe Prelude.Text)
listForecastsResponse_nextToken = Lens.lens (\ListForecastsResponse' {nextToken} -> nextToken) (\s@ListForecastsResponse' {} a -> s {nextToken = a} :: ListForecastsResponse)

-- | The response's http status code.
listForecastsResponse_httpStatus :: Lens.Lens' ListForecastsResponse Prelude.Int
listForecastsResponse_httpStatus = Lens.lens (\ListForecastsResponse' {httpStatus} -> httpStatus) (\s@ListForecastsResponse' {} a -> s {httpStatus = a} :: ListForecastsResponse)

instance Prelude.NFData ListForecastsResponse where
  rnf ListForecastsResponse' {..} =
    Prelude.rnf forecasts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
