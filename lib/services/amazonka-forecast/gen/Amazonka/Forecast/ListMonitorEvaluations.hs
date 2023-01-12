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
-- Module      : Amazonka.Forecast.ListMonitorEvaluations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the monitoring evaluation results and predictor events
-- collected by the monitor resource during different windows of time.
--
-- For information about monitoring see predictor-monitoring. For more
-- information about retrieving monitoring results see
-- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring-results.html Viewing Monitoring Results>.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListMonitorEvaluations
  ( -- * Creating a Request
    ListMonitorEvaluations (..),
    newListMonitorEvaluations,

    -- * Request Lenses
    listMonitorEvaluations_filters,
    listMonitorEvaluations_maxResults,
    listMonitorEvaluations_nextToken,
    listMonitorEvaluations_monitorArn,

    -- * Destructuring the Response
    ListMonitorEvaluationsResponse (..),
    newListMonitorEvaluationsResponse,

    -- * Response Lenses
    listMonitorEvaluationsResponse_nextToken,
    listMonitorEvaluationsResponse_predictorMonitorEvaluations,
    listMonitorEvaluationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMonitorEvaluations' smart constructor.
data ListMonitorEvaluations = ListMonitorEvaluations'
  { -- | An array of filters. For each filter, provide a condition and a match
    -- statement. The condition is either @IS@ or @IS_NOT@, which specifies
    -- whether to include or exclude the resources that match the statement
    -- from the list. The match statement consists of a key and a value.
    --
    -- __Filter properties__
    --
    -- -   @Condition@ - The condition to apply. Valid values are @IS@ and
    --     @IS_NOT@.
    --
    -- -   @Key@ - The name of the parameter to filter on. The only valid value
    --     is @EvaluationState@.
    --
    -- -   @Value@ - The value to match. Valid values are only @SUCCESS@ or
    --     @FAILURE@.
    --
    -- For example, to list only successful monitor evaluations, you would
    -- specify:
    --
    -- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"EvaluationState\", \"Value\": \"SUCCESS\" } ]@
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of monitoring results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the monitor resource to get results
    -- from.
    monitorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitorEvaluations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listMonitorEvaluations_filters' - An array of filters. For each filter, provide a condition and a match
-- statement. The condition is either @IS@ or @IS_NOT@, which specifies
-- whether to include or exclude the resources that match the statement
-- from the list. The match statement consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. The only valid value
--     is @EvaluationState@.
--
-- -   @Value@ - The value to match. Valid values are only @SUCCESS@ or
--     @FAILURE@.
--
-- For example, to list only successful monitor evaluations, you would
-- specify:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"EvaluationState\", \"Value\": \"SUCCESS\" } ]@
--
-- 'maxResults', 'listMonitorEvaluations_maxResults' - The maximum number of monitoring results to return.
--
-- 'nextToken', 'listMonitorEvaluations_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
--
-- 'monitorArn', 'listMonitorEvaluations_monitorArn' - The Amazon Resource Name (ARN) of the monitor resource to get results
-- from.
newListMonitorEvaluations ::
  -- | 'monitorArn'
  Prelude.Text ->
  ListMonitorEvaluations
newListMonitorEvaluations pMonitorArn_ =
  ListMonitorEvaluations'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      monitorArn = pMonitorArn_
    }

-- | An array of filters. For each filter, provide a condition and a match
-- statement. The condition is either @IS@ or @IS_NOT@, which specifies
-- whether to include or exclude the resources that match the statement
-- from the list. The match statement consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. The only valid value
--     is @EvaluationState@.
--
-- -   @Value@ - The value to match. Valid values are only @SUCCESS@ or
--     @FAILURE@.
--
-- For example, to list only successful monitor evaluations, you would
-- specify:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"EvaluationState\", \"Value\": \"SUCCESS\" } ]@
listMonitorEvaluations_filters :: Lens.Lens' ListMonitorEvaluations (Prelude.Maybe [Filter])
listMonitorEvaluations_filters = Lens.lens (\ListMonitorEvaluations' {filters} -> filters) (\s@ListMonitorEvaluations' {} a -> s {filters = a} :: ListMonitorEvaluations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of monitoring results to return.
listMonitorEvaluations_maxResults :: Lens.Lens' ListMonitorEvaluations (Prelude.Maybe Prelude.Natural)
listMonitorEvaluations_maxResults = Lens.lens (\ListMonitorEvaluations' {maxResults} -> maxResults) (\s@ListMonitorEvaluations' {} a -> s {maxResults = a} :: ListMonitorEvaluations)

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listMonitorEvaluations_nextToken :: Lens.Lens' ListMonitorEvaluations (Prelude.Maybe Prelude.Text)
listMonitorEvaluations_nextToken = Lens.lens (\ListMonitorEvaluations' {nextToken} -> nextToken) (\s@ListMonitorEvaluations' {} a -> s {nextToken = a} :: ListMonitorEvaluations)

-- | The Amazon Resource Name (ARN) of the monitor resource to get results
-- from.
listMonitorEvaluations_monitorArn :: Lens.Lens' ListMonitorEvaluations Prelude.Text
listMonitorEvaluations_monitorArn = Lens.lens (\ListMonitorEvaluations' {monitorArn} -> monitorArn) (\s@ListMonitorEvaluations' {} a -> s {monitorArn = a} :: ListMonitorEvaluations)

instance Core.AWSPager ListMonitorEvaluations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMonitorEvaluationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMonitorEvaluationsResponse_predictorMonitorEvaluations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMonitorEvaluations_nextToken
          Lens..~ rs
          Lens.^? listMonitorEvaluationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMonitorEvaluations where
  type
    AWSResponse ListMonitorEvaluations =
      ListMonitorEvaluationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitorEvaluationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "PredictorMonitorEvaluations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMonitorEvaluations where
  hashWithSalt _salt ListMonitorEvaluations' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` monitorArn

instance Prelude.NFData ListMonitorEvaluations where
  rnf ListMonitorEvaluations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf monitorArn

instance Data.ToHeaders ListMonitorEvaluations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListMonitorEvaluations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMonitorEvaluations where
  toJSON ListMonitorEvaluations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("MonitorArn" Data..= monitorArn)
          ]
      )

instance Data.ToPath ListMonitorEvaluations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMonitorEvaluations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMonitorEvaluationsResponse' smart constructor.
data ListMonitorEvaluationsResponse = ListMonitorEvaluationsResponse'
  { -- | If the response is truncated, Amazon Forecast returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    -- Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The monitoring results and predictor events collected by the monitor
    -- resource during different windows of time.
    --
    -- For information about monitoring see
    -- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring-results.html Viewing Monitoring Results>.
    -- For more information about retrieving monitoring results see
    -- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring-results.html Viewing Monitoring Results>.
    predictorMonitorEvaluations :: Prelude.Maybe [PredictorMonitorEvaluation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitorEvaluationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMonitorEvaluationsResponse_nextToken' - If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
-- Tokens expire after 24 hours.
--
-- 'predictorMonitorEvaluations', 'listMonitorEvaluationsResponse_predictorMonitorEvaluations' - The monitoring results and predictor events collected by the monitor
-- resource during different windows of time.
--
-- For information about monitoring see
-- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring-results.html Viewing Monitoring Results>.
-- For more information about retrieving monitoring results see
-- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring-results.html Viewing Monitoring Results>.
--
-- 'httpStatus', 'listMonitorEvaluationsResponse_httpStatus' - The response's http status code.
newListMonitorEvaluationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMonitorEvaluationsResponse
newListMonitorEvaluationsResponse pHttpStatus_ =
  ListMonitorEvaluationsResponse'
    { nextToken =
        Prelude.Nothing,
      predictorMonitorEvaluations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
-- Tokens expire after 24 hours.
listMonitorEvaluationsResponse_nextToken :: Lens.Lens' ListMonitorEvaluationsResponse (Prelude.Maybe Prelude.Text)
listMonitorEvaluationsResponse_nextToken = Lens.lens (\ListMonitorEvaluationsResponse' {nextToken} -> nextToken) (\s@ListMonitorEvaluationsResponse' {} a -> s {nextToken = a} :: ListMonitorEvaluationsResponse)

-- | The monitoring results and predictor events collected by the monitor
-- resource during different windows of time.
--
-- For information about monitoring see
-- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring-results.html Viewing Monitoring Results>.
-- For more information about retrieving monitoring results see
-- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring-results.html Viewing Monitoring Results>.
listMonitorEvaluationsResponse_predictorMonitorEvaluations :: Lens.Lens' ListMonitorEvaluationsResponse (Prelude.Maybe [PredictorMonitorEvaluation])
listMonitorEvaluationsResponse_predictorMonitorEvaluations = Lens.lens (\ListMonitorEvaluationsResponse' {predictorMonitorEvaluations} -> predictorMonitorEvaluations) (\s@ListMonitorEvaluationsResponse' {} a -> s {predictorMonitorEvaluations = a} :: ListMonitorEvaluationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMonitorEvaluationsResponse_httpStatus :: Lens.Lens' ListMonitorEvaluationsResponse Prelude.Int
listMonitorEvaluationsResponse_httpStatus = Lens.lens (\ListMonitorEvaluationsResponse' {httpStatus} -> httpStatus) (\s@ListMonitorEvaluationsResponse' {} a -> s {httpStatus = a} :: ListMonitorEvaluationsResponse)

instance
  Prelude.NFData
    ListMonitorEvaluationsResponse
  where
  rnf ListMonitorEvaluationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf predictorMonitorEvaluations
      `Prelude.seq` Prelude.rnf httpStatus
