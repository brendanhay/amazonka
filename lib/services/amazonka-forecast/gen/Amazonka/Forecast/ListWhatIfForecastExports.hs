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
-- Module      : Amazonka.Forecast.ListWhatIfForecastExports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of what-if forecast exports created using the
-- CreateWhatIfForecastExport operation. For each what-if forecast export,
-- this operation returns a summary of its properties, including its Amazon
-- Resource Name (ARN). You can retrieve the complete set of properties by
-- using the what-if forecast export ARN with the
-- DescribeWhatIfForecastExport operation.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListWhatIfForecastExports
  ( -- * Creating a Request
    ListWhatIfForecastExports (..),
    newListWhatIfForecastExports,

    -- * Request Lenses
    listWhatIfForecastExports_filters,
    listWhatIfForecastExports_maxResults,
    listWhatIfForecastExports_nextToken,

    -- * Destructuring the Response
    ListWhatIfForecastExportsResponse (..),
    newListWhatIfForecastExportsResponse,

    -- * Response Lenses
    listWhatIfForecastExportsResponse_nextToken,
    listWhatIfForecastExportsResponse_whatIfForecastExports,
    listWhatIfForecastExportsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWhatIfForecastExports' smart constructor.
data ListWhatIfForecastExports = ListWhatIfForecastExports'
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
    --     @WhatIfForecastExportArn@ and @Status@.
    --
    -- -   @Value@ - The value to match.
    --
    -- For example, to list all jobs that export a forecast named
    -- /electricityWIFExport/, specify the following filter:
    --
    -- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"WhatIfForecastExportArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityWIFExport\" } ]@
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
-- Create a value of 'ListWhatIfForecastExports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listWhatIfForecastExports_filters' - An array of filters. For each filter, you provide a condition and a
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
--     @WhatIfForecastExportArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all jobs that export a forecast named
-- /electricityWIFExport/, specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"WhatIfForecastExportArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityWIFExport\" } ]@
--
-- 'maxResults', 'listWhatIfForecastExports_maxResults' - The number of items to return in the response.
--
-- 'nextToken', 'listWhatIfForecastExports_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next  request. Tokens expire after 24 hours.
newListWhatIfForecastExports ::
  ListWhatIfForecastExports
newListWhatIfForecastExports =
  ListWhatIfForecastExports'
    { filters =
        Prelude.Nothing,
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
--     @WhatIfForecastExportArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all jobs that export a forecast named
-- /electricityWIFExport/, specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"WhatIfForecastExportArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityWIFExport\" } ]@
listWhatIfForecastExports_filters :: Lens.Lens' ListWhatIfForecastExports (Prelude.Maybe [Filter])
listWhatIfForecastExports_filters = Lens.lens (\ListWhatIfForecastExports' {filters} -> filters) (\s@ListWhatIfForecastExports' {} a -> s {filters = a} :: ListWhatIfForecastExports) Prelude.. Lens.mapping Lens.coerced

-- | The number of items to return in the response.
listWhatIfForecastExports_maxResults :: Lens.Lens' ListWhatIfForecastExports (Prelude.Maybe Prelude.Natural)
listWhatIfForecastExports_maxResults = Lens.lens (\ListWhatIfForecastExports' {maxResults} -> maxResults) (\s@ListWhatIfForecastExports' {} a -> s {maxResults = a} :: ListWhatIfForecastExports)

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next  request. Tokens expire after 24 hours.
listWhatIfForecastExports_nextToken :: Lens.Lens' ListWhatIfForecastExports (Prelude.Maybe Prelude.Text)
listWhatIfForecastExports_nextToken = Lens.lens (\ListWhatIfForecastExports' {nextToken} -> nextToken) (\s@ListWhatIfForecastExports' {} a -> s {nextToken = a} :: ListWhatIfForecastExports)

instance Core.AWSPager ListWhatIfForecastExports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWhatIfForecastExportsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWhatIfForecastExportsResponse_whatIfForecastExports
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listWhatIfForecastExports_nextToken
          Lens..~ rs
          Lens.^? listWhatIfForecastExportsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListWhatIfForecastExports where
  type
    AWSResponse ListWhatIfForecastExports =
      ListWhatIfForecastExportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWhatIfForecastExportsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "WhatIfForecastExports"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWhatIfForecastExports where
  hashWithSalt _salt ListWhatIfForecastExports' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListWhatIfForecastExports where
  rnf ListWhatIfForecastExports' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListWhatIfForecastExports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListWhatIfForecastExports" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWhatIfForecastExports where
  toJSON ListWhatIfForecastExports' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListWhatIfForecastExports where
  toPath = Prelude.const "/"

instance Data.ToQuery ListWhatIfForecastExports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWhatIfForecastExportsResponse' smart constructor.
data ListWhatIfForecastExportsResponse = ListWhatIfForecastExportsResponse'
  { -- | If the response is truncated, Forecast returns this token. To retrieve
    -- the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @WhatIfForecastExports@ objects that describe the matched
    -- forecast exports.
    whatIfForecastExports :: Prelude.Maybe [WhatIfForecastExportSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWhatIfForecastExportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWhatIfForecastExportsResponse_nextToken' - If the response is truncated, Forecast returns this token. To retrieve
-- the next set of results, use the token in the next request.
--
-- 'whatIfForecastExports', 'listWhatIfForecastExportsResponse_whatIfForecastExports' - An array of @WhatIfForecastExports@ objects that describe the matched
-- forecast exports.
--
-- 'httpStatus', 'listWhatIfForecastExportsResponse_httpStatus' - The response's http status code.
newListWhatIfForecastExportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWhatIfForecastExportsResponse
newListWhatIfForecastExportsResponse pHttpStatus_ =
  ListWhatIfForecastExportsResponse'
    { nextToken =
        Prelude.Nothing,
      whatIfForecastExports = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Forecast returns this token. To retrieve
-- the next set of results, use the token in the next request.
listWhatIfForecastExportsResponse_nextToken :: Lens.Lens' ListWhatIfForecastExportsResponse (Prelude.Maybe Prelude.Text)
listWhatIfForecastExportsResponse_nextToken = Lens.lens (\ListWhatIfForecastExportsResponse' {nextToken} -> nextToken) (\s@ListWhatIfForecastExportsResponse' {} a -> s {nextToken = a} :: ListWhatIfForecastExportsResponse)

-- | An array of @WhatIfForecastExports@ objects that describe the matched
-- forecast exports.
listWhatIfForecastExportsResponse_whatIfForecastExports :: Lens.Lens' ListWhatIfForecastExportsResponse (Prelude.Maybe [WhatIfForecastExportSummary])
listWhatIfForecastExportsResponse_whatIfForecastExports = Lens.lens (\ListWhatIfForecastExportsResponse' {whatIfForecastExports} -> whatIfForecastExports) (\s@ListWhatIfForecastExportsResponse' {} a -> s {whatIfForecastExports = a} :: ListWhatIfForecastExportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWhatIfForecastExportsResponse_httpStatus :: Lens.Lens' ListWhatIfForecastExportsResponse Prelude.Int
listWhatIfForecastExportsResponse_httpStatus = Lens.lens (\ListWhatIfForecastExportsResponse' {httpStatus} -> httpStatus) (\s@ListWhatIfForecastExportsResponse' {} a -> s {httpStatus = a} :: ListWhatIfForecastExportsResponse)

instance
  Prelude.NFData
    ListWhatIfForecastExportsResponse
  where
  rnf ListWhatIfForecastExportsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf whatIfForecastExports
      `Prelude.seq` Prelude.rnf httpStatus
