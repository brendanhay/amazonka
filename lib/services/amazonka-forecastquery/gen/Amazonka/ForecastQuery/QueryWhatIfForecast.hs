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
-- Module      : Amazonka.ForecastQuery.QueryWhatIfForecast
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a what-if forecast.
module Amazonka.ForecastQuery.QueryWhatIfForecast
  ( -- * Creating a Request
    QueryWhatIfForecast (..),
    newQueryWhatIfForecast,

    -- * Request Lenses
    queryWhatIfForecast_endDate,
    queryWhatIfForecast_nextToken,
    queryWhatIfForecast_startDate,
    queryWhatIfForecast_whatIfForecastArn,
    queryWhatIfForecast_filters,

    -- * Destructuring the Response
    QueryWhatIfForecastResponse (..),
    newQueryWhatIfForecastResponse,

    -- * Response Lenses
    queryWhatIfForecastResponse_forecast,
    queryWhatIfForecastResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ForecastQuery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newQueryWhatIfForecast' smart constructor.
data QueryWhatIfForecast = QueryWhatIfForecast'
  { -- | The end date for the what-if forecast. Specify the date using this
    -- format: yyyy-MM-dd\'T\'HH:mm:ss (ISO 8601 format). For example,
    -- 2015-01-01T20:00:00.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The start date for the what-if forecast. Specify the date using this
    -- format: yyyy-MM-dd\'T\'HH:mm:ss (ISO 8601 format). For example,
    -- 2015-01-01T08:00:00.
    startDate :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the what-if forecast to query.
    whatIfForecastArn :: Prelude.Text,
    -- | The filtering criteria to apply when retrieving the forecast. For
    -- example, to get the forecast for @client_21@ in the electricity usage
    -- dataset, specify the following:
    --
    -- @{\"item_id\" : \"client_21\"}@
    --
    -- To get the full what-if forecast, use the
    -- <https://docs.aws.amazon.com/en_us/forecast/latest/dg/API_CreateWhatIfForecastExport.html CreateForecastExportJob>
    -- operation.
    filters :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryWhatIfForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endDate', 'queryWhatIfForecast_endDate' - The end date for the what-if forecast. Specify the date using this
-- format: yyyy-MM-dd\'T\'HH:mm:ss (ISO 8601 format). For example,
-- 2015-01-01T20:00:00.
--
-- 'nextToken', 'queryWhatIfForecast_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
--
-- 'startDate', 'queryWhatIfForecast_startDate' - The start date for the what-if forecast. Specify the date using this
-- format: yyyy-MM-dd\'T\'HH:mm:ss (ISO 8601 format). For example,
-- 2015-01-01T08:00:00.
--
-- 'whatIfForecastArn', 'queryWhatIfForecast_whatIfForecastArn' - The Amazon Resource Name (ARN) of the what-if forecast to query.
--
-- 'filters', 'queryWhatIfForecast_filters' - The filtering criteria to apply when retrieving the forecast. For
-- example, to get the forecast for @client_21@ in the electricity usage
-- dataset, specify the following:
--
-- @{\"item_id\" : \"client_21\"}@
--
-- To get the full what-if forecast, use the
-- <https://docs.aws.amazon.com/en_us/forecast/latest/dg/API_CreateWhatIfForecastExport.html CreateForecastExportJob>
-- operation.
newQueryWhatIfForecast ::
  -- | 'whatIfForecastArn'
  Prelude.Text ->
  QueryWhatIfForecast
newQueryWhatIfForecast pWhatIfForecastArn_ =
  QueryWhatIfForecast'
    { endDate = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startDate = Prelude.Nothing,
      whatIfForecastArn = pWhatIfForecastArn_,
      filters = Prelude.mempty
    }

-- | The end date for the what-if forecast. Specify the date using this
-- format: yyyy-MM-dd\'T\'HH:mm:ss (ISO 8601 format). For example,
-- 2015-01-01T20:00:00.
queryWhatIfForecast_endDate :: Lens.Lens' QueryWhatIfForecast (Prelude.Maybe Prelude.Text)
queryWhatIfForecast_endDate = Lens.lens (\QueryWhatIfForecast' {endDate} -> endDate) (\s@QueryWhatIfForecast' {} a -> s {endDate = a} :: QueryWhatIfForecast)

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
queryWhatIfForecast_nextToken :: Lens.Lens' QueryWhatIfForecast (Prelude.Maybe Prelude.Text)
queryWhatIfForecast_nextToken = Lens.lens (\QueryWhatIfForecast' {nextToken} -> nextToken) (\s@QueryWhatIfForecast' {} a -> s {nextToken = a} :: QueryWhatIfForecast)

-- | The start date for the what-if forecast. Specify the date using this
-- format: yyyy-MM-dd\'T\'HH:mm:ss (ISO 8601 format). For example,
-- 2015-01-01T08:00:00.
queryWhatIfForecast_startDate :: Lens.Lens' QueryWhatIfForecast (Prelude.Maybe Prelude.Text)
queryWhatIfForecast_startDate = Lens.lens (\QueryWhatIfForecast' {startDate} -> startDate) (\s@QueryWhatIfForecast' {} a -> s {startDate = a} :: QueryWhatIfForecast)

-- | The Amazon Resource Name (ARN) of the what-if forecast to query.
queryWhatIfForecast_whatIfForecastArn :: Lens.Lens' QueryWhatIfForecast Prelude.Text
queryWhatIfForecast_whatIfForecastArn = Lens.lens (\QueryWhatIfForecast' {whatIfForecastArn} -> whatIfForecastArn) (\s@QueryWhatIfForecast' {} a -> s {whatIfForecastArn = a} :: QueryWhatIfForecast)

-- | The filtering criteria to apply when retrieving the forecast. For
-- example, to get the forecast for @client_21@ in the electricity usage
-- dataset, specify the following:
--
-- @{\"item_id\" : \"client_21\"}@
--
-- To get the full what-if forecast, use the
-- <https://docs.aws.amazon.com/en_us/forecast/latest/dg/API_CreateWhatIfForecastExport.html CreateForecastExportJob>
-- operation.
queryWhatIfForecast_filters :: Lens.Lens' QueryWhatIfForecast (Prelude.HashMap Prelude.Text Prelude.Text)
queryWhatIfForecast_filters = Lens.lens (\QueryWhatIfForecast' {filters} -> filters) (\s@QueryWhatIfForecast' {} a -> s {filters = a} :: QueryWhatIfForecast) Prelude.. Lens.coerced

instance Core.AWSRequest QueryWhatIfForecast where
  type
    AWSResponse QueryWhatIfForecast =
      QueryWhatIfForecastResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryWhatIfForecastResponse'
            Prelude.<$> (x Data..?> "Forecast")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable QueryWhatIfForecast where
  hashWithSalt _salt QueryWhatIfForecast' {..} =
    _salt
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` whatIfForecastArn
      `Prelude.hashWithSalt` filters

instance Prelude.NFData QueryWhatIfForecast where
  rnf QueryWhatIfForecast' {..} =
    Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf whatIfForecastArn
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders QueryWhatIfForecast where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecastRuntime.QueryWhatIfForecast" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON QueryWhatIfForecast where
  toJSON QueryWhatIfForecast' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndDate" Data..=) Prelude.<$> endDate,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StartDate" Data..=) Prelude.<$> startDate,
            Prelude.Just
              ("WhatIfForecastArn" Data..= whatIfForecastArn),
            Prelude.Just ("Filters" Data..= filters)
          ]
      )

instance Data.ToPath QueryWhatIfForecast where
  toPath = Prelude.const "/"

instance Data.ToQuery QueryWhatIfForecast where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newQueryWhatIfForecastResponse' smart constructor.
data QueryWhatIfForecastResponse = QueryWhatIfForecastResponse'
  { forecast :: Prelude.Maybe Forecast,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryWhatIfForecastResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecast', 'queryWhatIfForecastResponse_forecast' - Undocumented member.
--
-- 'httpStatus', 'queryWhatIfForecastResponse_httpStatus' - The response's http status code.
newQueryWhatIfForecastResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  QueryWhatIfForecastResponse
newQueryWhatIfForecastResponse pHttpStatus_ =
  QueryWhatIfForecastResponse'
    { forecast =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
queryWhatIfForecastResponse_forecast :: Lens.Lens' QueryWhatIfForecastResponse (Prelude.Maybe Forecast)
queryWhatIfForecastResponse_forecast = Lens.lens (\QueryWhatIfForecastResponse' {forecast} -> forecast) (\s@QueryWhatIfForecastResponse' {} a -> s {forecast = a} :: QueryWhatIfForecastResponse)

-- | The response's http status code.
queryWhatIfForecastResponse_httpStatus :: Lens.Lens' QueryWhatIfForecastResponse Prelude.Int
queryWhatIfForecastResponse_httpStatus = Lens.lens (\QueryWhatIfForecastResponse' {httpStatus} -> httpStatus) (\s@QueryWhatIfForecastResponse' {} a -> s {httpStatus = a} :: QueryWhatIfForecastResponse)

instance Prelude.NFData QueryWhatIfForecastResponse where
  rnf QueryWhatIfForecastResponse' {..} =
    Prelude.rnf forecast
      `Prelude.seq` Prelude.rnf httpStatus
