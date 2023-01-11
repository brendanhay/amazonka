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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listMonitors_filters,
    listMonitors_maxResults,
    listMonitors_nextToken,

    -- * Destructuring the Response
    ListMonitorsResponse (..),
    newListMonitorsResponse,

    -- * Response Lenses
    listMonitorsResponse_monitors,
    listMonitorsResponse_nextToken,
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
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text
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
--
-- 'nextToken', 'listMonitors_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
newListMonitors ::
  ListMonitors
newListMonitors =
  ListMonitors'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
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

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listMonitors_nextToken :: Lens.Lens' ListMonitors (Prelude.Maybe Prelude.Text)
listMonitors_nextToken = Lens.lens (\ListMonitors' {nextToken} -> nextToken) (\s@ListMonitors' {} a -> s {nextToken = a} :: ListMonitors)

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
            Prelude.<$> (x Data..?> "Monitors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMonitors where
  hashWithSalt _salt ListMonitors' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMonitors where
  rnf ListMonitors' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

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
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListMonitors where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMonitors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMonitorsResponse' smart constructor.
data ListMonitorsResponse = ListMonitorsResponse'
  { -- | An array of objects that summarize each monitor\'s properties.
    monitors :: Prelude.Maybe [MonitorSummary],
    -- | If the response is truncated, Amazon Forecast returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'monitors', 'listMonitorsResponse_monitors' - An array of objects that summarize each monitor\'s properties.
--
-- 'nextToken', 'listMonitorsResponse_nextToken' - If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
--
-- 'httpStatus', 'listMonitorsResponse_httpStatus' - The response's http status code.
newListMonitorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMonitorsResponse
newListMonitorsResponse pHttpStatus_ =
  ListMonitorsResponse'
    { monitors = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that summarize each monitor\'s properties.
listMonitorsResponse_monitors :: Lens.Lens' ListMonitorsResponse (Prelude.Maybe [MonitorSummary])
listMonitorsResponse_monitors = Lens.lens (\ListMonitorsResponse' {monitors} -> monitors) (\s@ListMonitorsResponse' {} a -> s {monitors = a} :: ListMonitorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
listMonitorsResponse_nextToken :: Lens.Lens' ListMonitorsResponse (Prelude.Maybe Prelude.Text)
listMonitorsResponse_nextToken = Lens.lens (\ListMonitorsResponse' {nextToken} -> nextToken) (\s@ListMonitorsResponse' {} a -> s {nextToken = a} :: ListMonitorsResponse)

-- | The response's http status code.
listMonitorsResponse_httpStatus :: Lens.Lens' ListMonitorsResponse Prelude.Int
listMonitorsResponse_httpStatus = Lens.lens (\ListMonitorsResponse' {httpStatus} -> httpStatus) (\s@ListMonitorsResponse' {} a -> s {httpStatus = a} :: ListMonitorsResponse)

instance Prelude.NFData ListMonitorsResponse where
  rnf ListMonitorsResponse' {..} =
    Prelude.rnf monitors
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
