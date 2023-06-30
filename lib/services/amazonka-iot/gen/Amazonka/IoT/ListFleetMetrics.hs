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
-- Module      : Amazonka.IoT.ListFleetMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your fleet metrics.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListFleetMetrics>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListFleetMetrics
  ( -- * Creating a Request
    ListFleetMetrics (..),
    newListFleetMetrics,

    -- * Request Lenses
    listFleetMetrics_maxResults,
    listFleetMetrics_nextToken,

    -- * Destructuring the Response
    ListFleetMetricsResponse (..),
    newListFleetMetricsResponse,

    -- * Response Lenses
    listFleetMetricsResponse_fleetMetrics,
    listFleetMetricsResponse_nextToken,
    listFleetMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFleetMetrics' smart constructor.
data ListFleetMetrics = ListFleetMetrics'
  { -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise @null@ to receive the first set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleetMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFleetMetrics_maxResults' - The maximum number of results to return in this operation.
--
-- 'nextToken', 'listFleetMetrics_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise @null@ to receive the first set of results.
newListFleetMetrics ::
  ListFleetMetrics
newListFleetMetrics =
  ListFleetMetrics'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in this operation.
listFleetMetrics_maxResults :: Lens.Lens' ListFleetMetrics (Prelude.Maybe Prelude.Natural)
listFleetMetrics_maxResults = Lens.lens (\ListFleetMetrics' {maxResults} -> maxResults) (\s@ListFleetMetrics' {} a -> s {maxResults = a} :: ListFleetMetrics)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise @null@ to receive the first set of results.
listFleetMetrics_nextToken :: Lens.Lens' ListFleetMetrics (Prelude.Maybe Prelude.Text)
listFleetMetrics_nextToken = Lens.lens (\ListFleetMetrics' {nextToken} -> nextToken) (\s@ListFleetMetrics' {} a -> s {nextToken = a} :: ListFleetMetrics)

instance Core.AWSPager ListFleetMetrics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFleetMetricsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFleetMetricsResponse_fleetMetrics
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFleetMetrics_nextToken
          Lens..~ rs
          Lens.^? listFleetMetricsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFleetMetrics where
  type
    AWSResponse ListFleetMetrics =
      ListFleetMetricsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFleetMetricsResponse'
            Prelude.<$> (x Data..?> "fleetMetrics" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFleetMetrics where
  hashWithSalt _salt ListFleetMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFleetMetrics where
  rnf ListFleetMetrics' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListFleetMetrics where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListFleetMetrics where
  toPath = Prelude.const "/fleet-metrics"

instance Data.ToQuery ListFleetMetrics where
  toQuery ListFleetMetrics' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListFleetMetricsResponse' smart constructor.
data ListFleetMetricsResponse = ListFleetMetricsResponse'
  { -- | The list of fleet metrics objects.
    fleetMetrics :: Prelude.Maybe [FleetMetricNameAndArn],
    -- | The token for the next set of results. Will not be returned if the
    -- operation has returned all results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleetMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetMetrics', 'listFleetMetricsResponse_fleetMetrics' - The list of fleet metrics objects.
--
-- 'nextToken', 'listFleetMetricsResponse_nextToken' - The token for the next set of results. Will not be returned if the
-- operation has returned all results.
--
-- 'httpStatus', 'listFleetMetricsResponse_httpStatus' - The response's http status code.
newListFleetMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFleetMetricsResponse
newListFleetMetricsResponse pHttpStatus_ =
  ListFleetMetricsResponse'
    { fleetMetrics =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of fleet metrics objects.
listFleetMetricsResponse_fleetMetrics :: Lens.Lens' ListFleetMetricsResponse (Prelude.Maybe [FleetMetricNameAndArn])
listFleetMetricsResponse_fleetMetrics = Lens.lens (\ListFleetMetricsResponse' {fleetMetrics} -> fleetMetrics) (\s@ListFleetMetricsResponse' {} a -> s {fleetMetrics = a} :: ListFleetMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results. Will not be returned if the
-- operation has returned all results.
listFleetMetricsResponse_nextToken :: Lens.Lens' ListFleetMetricsResponse (Prelude.Maybe Prelude.Text)
listFleetMetricsResponse_nextToken = Lens.lens (\ListFleetMetricsResponse' {nextToken} -> nextToken) (\s@ListFleetMetricsResponse' {} a -> s {nextToken = a} :: ListFleetMetricsResponse)

-- | The response's http status code.
listFleetMetricsResponse_httpStatus :: Lens.Lens' ListFleetMetricsResponse Prelude.Int
listFleetMetricsResponse_httpStatus = Lens.lens (\ListFleetMetricsResponse' {httpStatus} -> httpStatus) (\s@ListFleetMetricsResponse' {} a -> s {httpStatus = a} :: ListFleetMetricsResponse)

instance Prelude.NFData ListFleetMetricsResponse where
  rnf ListFleetMetricsResponse' {..} =
    Prelude.rnf fleetMetrics
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
