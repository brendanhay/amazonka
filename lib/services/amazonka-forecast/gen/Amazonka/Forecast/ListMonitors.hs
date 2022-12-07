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
-- Module      : Amazonka.Forecast.ListMonitors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of monitors created with the CreateMonitor operation and
-- CreateAutoPredictor operation. For each monitor resource, this operation
-- returns of a summary of its properties, including its Amazon Resource
-- Name (ARN). You can retrieve a complete set of properties of a monitor
-- resource by specify the monitor\'s ARN in the DescribeMonitor operation.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListMonitors
  ( -- * Creating a Request
    ListMonitors (..),
    newListMonitors,

    -- * Request Lenses
    listMonitors_nextToken,
    listMonitors_filters,
    listMonitors_maxResults,

    -- * Destructuring the Response
    ListMonitorsResponse (..),
    newListMonitorsResponse,

    -- * Response Lenses
    listMonitorsResponse_nextToken,
    listMonitorsResponse_monitors,
    listMonitorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMonitors' smart constructor.
data ListMonitors = ListMonitors'
  { -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    --     is @Status@.
    --
    -- -   @Value@ - The value to match.
    --
    -- For example, to list all monitors who\'s status is ACTIVE, you would
    -- specify:
    --
    -- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of monitors to include in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMonitors_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
--
-- 'filters', 'listMonitors_filters' - An array of filters. For each filter, provide a condition and a match
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
--     is @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all monitors who\'s status is ACTIVE, you would
-- specify:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
--
-- 'maxResults', 'listMonitors_maxResults' - The maximum number of monitors to include in the response.
newListMonitors ::
  ListMonitors
newListMonitors =
  ListMonitors'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listMonitors_nextToken :: Lens.Lens' ListMonitors (Prelude.Maybe Prelude.Text)
listMonitors_nextToken = Lens.lens (\ListMonitors' {nextToken} -> nextToken) (\s@ListMonitors' {} a -> s {nextToken = a} :: ListMonitors)

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
--     is @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all monitors who\'s status is ACTIVE, you would
-- specify:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"Status\", \"Value\": \"ACTIVE\" } ]@
listMonitors_filters :: Lens.Lens' ListMonitors (Prelude.Maybe [Filter])
listMonitors_filters = Lens.lens (\ListMonitors' {filters} -> filters) (\s@ListMonitors' {} a -> s {filters = a} :: ListMonitors) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of monitors to include in the response.
listMonitors_maxResults :: Lens.Lens' ListMonitors (Prelude.Maybe Prelude.Natural)
listMonitors_maxResults = Lens.lens (\ListMonitors' {maxResults} -> maxResults) (\s@ListMonitors' {} a -> s {maxResults = a} :: ListMonitors)

instance Core.AWSPager ListMonitors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMonitorsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMonitorsResponse_monitors Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMonitors_nextToken
          Lens..~ rs
          Lens.^? listMonitorsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListMonitors where
  type AWSResponse ListMonitors = ListMonitorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitorsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Monitors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMonitors where
  hashWithSalt _salt ListMonitors' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListMonitors where
  rnf ListMonitors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListMonitors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListMonitors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMonitors where
  toJSON ListMonitors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListMonitors where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMonitors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMonitorsResponse' smart constructor.
data ListMonitorsResponse = ListMonitorsResponse'
  { -- | If the response is truncated, Amazon Forecast returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that summarize each monitor\'s properties.
    monitors :: Prelude.Maybe [MonitorSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMonitorsResponse_nextToken' - If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
--
-- 'monitors', 'listMonitorsResponse_monitors' - An array of objects that summarize each monitor\'s properties.
--
-- 'httpStatus', 'listMonitorsResponse_httpStatus' - The response's http status code.
newListMonitorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMonitorsResponse
newListMonitorsResponse pHttpStatus_ =
  ListMonitorsResponse'
    { nextToken = Prelude.Nothing,
      monitors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
listMonitorsResponse_nextToken :: Lens.Lens' ListMonitorsResponse (Prelude.Maybe Prelude.Text)
listMonitorsResponse_nextToken = Lens.lens (\ListMonitorsResponse' {nextToken} -> nextToken) (\s@ListMonitorsResponse' {} a -> s {nextToken = a} :: ListMonitorsResponse)

-- | An array of objects that summarize each monitor\'s properties.
listMonitorsResponse_monitors :: Lens.Lens' ListMonitorsResponse (Prelude.Maybe [MonitorSummary])
listMonitorsResponse_monitors = Lens.lens (\ListMonitorsResponse' {monitors} -> monitors) (\s@ListMonitorsResponse' {} a -> s {monitors = a} :: ListMonitorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMonitorsResponse_httpStatus :: Lens.Lens' ListMonitorsResponse Prelude.Int
listMonitorsResponse_httpStatus = Lens.lens (\ListMonitorsResponse' {httpStatus} -> httpStatus) (\s@ListMonitorsResponse' {} a -> s {httpStatus = a} :: ListMonitorsResponse)

instance Prelude.NFData ListMonitorsResponse where
  rnf ListMonitorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf monitors
      `Prelude.seq` Prelude.rnf httpStatus
