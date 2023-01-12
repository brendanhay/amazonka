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
-- Module      : Amazonka.Rum.ListRumMetricsDestinations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of destinations that you have created to receive RUM
-- extended metrics, for the specified app monitor.
--
-- For more information about extended metrics, see
-- <https://docs.aws.amazon.com/cloudwatchrum/latest/APIReference/API_AddRumMetrcs.html AddRumMetrics>.
--
-- This operation returns paginated results.
module Amazonka.Rum.ListRumMetricsDestinations
  ( -- * Creating a Request
    ListRumMetricsDestinations (..),
    newListRumMetricsDestinations,

    -- * Request Lenses
    listRumMetricsDestinations_maxResults,
    listRumMetricsDestinations_nextToken,
    listRumMetricsDestinations_appMonitorName,

    -- * Destructuring the Response
    ListRumMetricsDestinationsResponse (..),
    newListRumMetricsDestinationsResponse,

    -- * Response Lenses
    listRumMetricsDestinationsResponse_destinations,
    listRumMetricsDestinationsResponse_nextToken,
    listRumMetricsDestinationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newListRumMetricsDestinations' smart constructor.
data ListRumMetricsDestinations = ListRumMetricsDestinations'
  { -- | The maximum number of results to return in one operation. The default is
    -- 50. The maximum that you can specify is 100.
    --
    -- To retrieve the remaining results, make another call with the returned
    -- @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Use the token returned by the previous operation to request the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the app monitor associated with the destinations that you
    -- want to retrieve.
    appMonitorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRumMetricsDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRumMetricsDestinations_maxResults' - The maximum number of results to return in one operation. The default is
-- 50. The maximum that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned
-- @NextToken@ value.
--
-- 'nextToken', 'listRumMetricsDestinations_nextToken' - Use the token returned by the previous operation to request the next
-- page of results.
--
-- 'appMonitorName', 'listRumMetricsDestinations_appMonitorName' - The name of the app monitor associated with the destinations that you
-- want to retrieve.
newListRumMetricsDestinations ::
  -- | 'appMonitorName'
  Prelude.Text ->
  ListRumMetricsDestinations
newListRumMetricsDestinations pAppMonitorName_ =
  ListRumMetricsDestinations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      appMonitorName = pAppMonitorName_
    }

-- | The maximum number of results to return in one operation. The default is
-- 50. The maximum that you can specify is 100.
--
-- To retrieve the remaining results, make another call with the returned
-- @NextToken@ value.
listRumMetricsDestinations_maxResults :: Lens.Lens' ListRumMetricsDestinations (Prelude.Maybe Prelude.Natural)
listRumMetricsDestinations_maxResults = Lens.lens (\ListRumMetricsDestinations' {maxResults} -> maxResults) (\s@ListRumMetricsDestinations' {} a -> s {maxResults = a} :: ListRumMetricsDestinations)

-- | Use the token returned by the previous operation to request the next
-- page of results.
listRumMetricsDestinations_nextToken :: Lens.Lens' ListRumMetricsDestinations (Prelude.Maybe Prelude.Text)
listRumMetricsDestinations_nextToken = Lens.lens (\ListRumMetricsDestinations' {nextToken} -> nextToken) (\s@ListRumMetricsDestinations' {} a -> s {nextToken = a} :: ListRumMetricsDestinations)

-- | The name of the app monitor associated with the destinations that you
-- want to retrieve.
listRumMetricsDestinations_appMonitorName :: Lens.Lens' ListRumMetricsDestinations Prelude.Text
listRumMetricsDestinations_appMonitorName = Lens.lens (\ListRumMetricsDestinations' {appMonitorName} -> appMonitorName) (\s@ListRumMetricsDestinations' {} a -> s {appMonitorName = a} :: ListRumMetricsDestinations)

instance Core.AWSPager ListRumMetricsDestinations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRumMetricsDestinationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRumMetricsDestinationsResponse_destinations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRumMetricsDestinations_nextToken
          Lens..~ rs
          Lens.^? listRumMetricsDestinationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRumMetricsDestinations where
  type
    AWSResponse ListRumMetricsDestinations =
      ListRumMetricsDestinationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRumMetricsDestinationsResponse'
            Prelude.<$> (x Data..?> "Destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRumMetricsDestinations where
  hashWithSalt _salt ListRumMetricsDestinations' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appMonitorName

instance Prelude.NFData ListRumMetricsDestinations where
  rnf ListRumMetricsDestinations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appMonitorName

instance Data.ToHeaders ListRumMetricsDestinations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRumMetricsDestinations where
  toPath ListRumMetricsDestinations' {..} =
    Prelude.mconcat
      [ "/rummetrics/",
        Data.toBS appMonitorName,
        "/metricsdestination"
      ]

instance Data.ToQuery ListRumMetricsDestinations where
  toQuery ListRumMetricsDestinations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListRumMetricsDestinationsResponse' smart constructor.
data ListRumMetricsDestinationsResponse = ListRumMetricsDestinationsResponse'
  { -- | The list of CloudWatch RUM extended metrics destinations associated with
    -- the app monitor that you specified.
    destinations :: Prelude.Maybe [MetricDestinationSummary],
    -- | A token that you can use in a subsequent operation to retrieve the next
    -- set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRumMetricsDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinations', 'listRumMetricsDestinationsResponse_destinations' - The list of CloudWatch RUM extended metrics destinations associated with
-- the app monitor that you specified.
--
-- 'nextToken', 'listRumMetricsDestinationsResponse_nextToken' - A token that you can use in a subsequent operation to retrieve the next
-- set of results.
--
-- 'httpStatus', 'listRumMetricsDestinationsResponse_httpStatus' - The response's http status code.
newListRumMetricsDestinationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRumMetricsDestinationsResponse
newListRumMetricsDestinationsResponse pHttpStatus_ =
  ListRumMetricsDestinationsResponse'
    { destinations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of CloudWatch RUM extended metrics destinations associated with
-- the app monitor that you specified.
listRumMetricsDestinationsResponse_destinations :: Lens.Lens' ListRumMetricsDestinationsResponse (Prelude.Maybe [MetricDestinationSummary])
listRumMetricsDestinationsResponse_destinations = Lens.lens (\ListRumMetricsDestinationsResponse' {destinations} -> destinations) (\s@ListRumMetricsDestinationsResponse' {} a -> s {destinations = a} :: ListRumMetricsDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that you can use in a subsequent operation to retrieve the next
-- set of results.
listRumMetricsDestinationsResponse_nextToken :: Lens.Lens' ListRumMetricsDestinationsResponse (Prelude.Maybe Prelude.Text)
listRumMetricsDestinationsResponse_nextToken = Lens.lens (\ListRumMetricsDestinationsResponse' {nextToken} -> nextToken) (\s@ListRumMetricsDestinationsResponse' {} a -> s {nextToken = a} :: ListRumMetricsDestinationsResponse)

-- | The response's http status code.
listRumMetricsDestinationsResponse_httpStatus :: Lens.Lens' ListRumMetricsDestinationsResponse Prelude.Int
listRumMetricsDestinationsResponse_httpStatus = Lens.lens (\ListRumMetricsDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListRumMetricsDestinationsResponse' {} a -> s {httpStatus = a} :: ListRumMetricsDestinationsResponse)

instance
  Prelude.NFData
    ListRumMetricsDestinationsResponse
  where
  rnf ListRumMetricsDestinationsResponse' {..} =
    Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
