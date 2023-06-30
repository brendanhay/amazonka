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
-- Module      : Amazonka.Forecast.ListWhatIfAnalyses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of what-if analyses created using the
-- CreateWhatIfAnalysis operation. For each what-if analysis, this
-- operation returns a summary of its properties, including its Amazon
-- Resource Name (ARN). You can retrieve the complete set of properties by
-- using the what-if analysis ARN with the DescribeWhatIfAnalysis
-- operation.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListWhatIfAnalyses
  ( -- * Creating a Request
    ListWhatIfAnalyses (..),
    newListWhatIfAnalyses,

    -- * Request Lenses
    listWhatIfAnalyses_filters,
    listWhatIfAnalyses_maxResults,
    listWhatIfAnalyses_nextToken,

    -- * Destructuring the Response
    ListWhatIfAnalysesResponse (..),
    newListWhatIfAnalysesResponse,

    -- * Response Lenses
    listWhatIfAnalysesResponse_nextToken,
    listWhatIfAnalysesResponse_whatIfAnalyses,
    listWhatIfAnalysesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWhatIfAnalyses' smart constructor.
data ListWhatIfAnalyses = ListWhatIfAnalyses'
  { -- | An array of filters. For each filter, you provide a condition and a
    -- match statement. The condition is either @IS@ or @IS_NOT@, which
    -- specifies whether to include or exclude the what-if analysis jobs that
    -- match the statement from the list, respectively. The match statement
    -- consists of a key and a value.
    --
    -- __Filter properties__
    --
    -- -   @Condition@ - The condition to apply. Valid values are @IS@ and
    --     @IS_NOT@. To include the what-if analysis jobs that match the
    --     statement, specify @IS@. To exclude matching what-if analysis jobs,
    --     specify @IS_NOT@.
    --
    -- -   @Key@ - The name of the parameter to filter on. Valid values are
    --     @WhatIfAnalysisArn@ and @Status@.
    --
    -- -   @Value@ - The value to match.
    --
    -- For example, to list all jobs that export a forecast named
    -- /electricityWhatIf/, specify the following filter:
    --
    -- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"WhatIfAnalysisArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityWhatIf\" } ]@
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
-- Create a value of 'ListWhatIfAnalyses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listWhatIfAnalyses_filters' - An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the what-if analysis jobs that
-- match the statement from the list, respectively. The match statement
-- consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the what-if analysis jobs that match the
--     statement, specify @IS@. To exclude matching what-if analysis jobs,
--     specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @WhatIfAnalysisArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all jobs that export a forecast named
-- /electricityWhatIf/, specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"WhatIfAnalysisArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityWhatIf\" } ]@
--
-- 'maxResults', 'listWhatIfAnalyses_maxResults' - The number of items to return in the response.
--
-- 'nextToken', 'listWhatIfAnalyses_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
newListWhatIfAnalyses ::
  ListWhatIfAnalyses
newListWhatIfAnalyses =
  ListWhatIfAnalyses'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of filters. For each filter, you provide a condition and a
-- match statement. The condition is either @IS@ or @IS_NOT@, which
-- specifies whether to include or exclude the what-if analysis jobs that
-- match the statement from the list, respectively. The match statement
-- consists of a key and a value.
--
-- __Filter properties__
--
-- -   @Condition@ - The condition to apply. Valid values are @IS@ and
--     @IS_NOT@. To include the what-if analysis jobs that match the
--     statement, specify @IS@. To exclude matching what-if analysis jobs,
--     specify @IS_NOT@.
--
-- -   @Key@ - The name of the parameter to filter on. Valid values are
--     @WhatIfAnalysisArn@ and @Status@.
--
-- -   @Value@ - The value to match.
--
-- For example, to list all jobs that export a forecast named
-- /electricityWhatIf/, specify the following filter:
--
-- @\"Filters\": [ { \"Condition\": \"IS\", \"Key\": \"WhatIfAnalysisArn\", \"Value\": \"arn:aws:forecast:us-west-2:\<acct-id>:forecast\/electricityWhatIf\" } ]@
listWhatIfAnalyses_filters :: Lens.Lens' ListWhatIfAnalyses (Prelude.Maybe [Filter])
listWhatIfAnalyses_filters = Lens.lens (\ListWhatIfAnalyses' {filters} -> filters) (\s@ListWhatIfAnalyses' {} a -> s {filters = a} :: ListWhatIfAnalyses) Prelude.. Lens.mapping Lens.coerced

-- | The number of items to return in the response.
listWhatIfAnalyses_maxResults :: Lens.Lens' ListWhatIfAnalyses (Prelude.Maybe Prelude.Natural)
listWhatIfAnalyses_maxResults = Lens.lens (\ListWhatIfAnalyses' {maxResults} -> maxResults) (\s@ListWhatIfAnalyses' {} a -> s {maxResults = a} :: ListWhatIfAnalyses)

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listWhatIfAnalyses_nextToken :: Lens.Lens' ListWhatIfAnalyses (Prelude.Maybe Prelude.Text)
listWhatIfAnalyses_nextToken = Lens.lens (\ListWhatIfAnalyses' {nextToken} -> nextToken) (\s@ListWhatIfAnalyses' {} a -> s {nextToken = a} :: ListWhatIfAnalyses)

instance Core.AWSPager ListWhatIfAnalyses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWhatIfAnalysesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWhatIfAnalysesResponse_whatIfAnalyses
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listWhatIfAnalyses_nextToken
          Lens..~ rs
          Lens.^? listWhatIfAnalysesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListWhatIfAnalyses where
  type
    AWSResponse ListWhatIfAnalyses =
      ListWhatIfAnalysesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWhatIfAnalysesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "WhatIfAnalyses" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWhatIfAnalyses where
  hashWithSalt _salt ListWhatIfAnalyses' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListWhatIfAnalyses where
  rnf ListWhatIfAnalyses' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListWhatIfAnalyses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListWhatIfAnalyses" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWhatIfAnalyses where
  toJSON ListWhatIfAnalyses' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListWhatIfAnalyses where
  toPath = Prelude.const "/"

instance Data.ToQuery ListWhatIfAnalyses where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWhatIfAnalysesResponse' smart constructor.
data ListWhatIfAnalysesResponse = ListWhatIfAnalysesResponse'
  { -- | If the response is truncated, Forecast returns this token. To retrieve
    -- the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @WhatIfAnalysisSummary@ objects that describe the matched
    -- analyses.
    whatIfAnalyses :: Prelude.Maybe [WhatIfAnalysisSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWhatIfAnalysesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWhatIfAnalysesResponse_nextToken' - If the response is truncated, Forecast returns this token. To retrieve
-- the next set of results, use the token in the next request.
--
-- 'whatIfAnalyses', 'listWhatIfAnalysesResponse_whatIfAnalyses' - An array of @WhatIfAnalysisSummary@ objects that describe the matched
-- analyses.
--
-- 'httpStatus', 'listWhatIfAnalysesResponse_httpStatus' - The response's http status code.
newListWhatIfAnalysesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWhatIfAnalysesResponse
newListWhatIfAnalysesResponse pHttpStatus_ =
  ListWhatIfAnalysesResponse'
    { nextToken =
        Prelude.Nothing,
      whatIfAnalyses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Forecast returns this token. To retrieve
-- the next set of results, use the token in the next request.
listWhatIfAnalysesResponse_nextToken :: Lens.Lens' ListWhatIfAnalysesResponse (Prelude.Maybe Prelude.Text)
listWhatIfAnalysesResponse_nextToken = Lens.lens (\ListWhatIfAnalysesResponse' {nextToken} -> nextToken) (\s@ListWhatIfAnalysesResponse' {} a -> s {nextToken = a} :: ListWhatIfAnalysesResponse)

-- | An array of @WhatIfAnalysisSummary@ objects that describe the matched
-- analyses.
listWhatIfAnalysesResponse_whatIfAnalyses :: Lens.Lens' ListWhatIfAnalysesResponse (Prelude.Maybe [WhatIfAnalysisSummary])
listWhatIfAnalysesResponse_whatIfAnalyses = Lens.lens (\ListWhatIfAnalysesResponse' {whatIfAnalyses} -> whatIfAnalyses) (\s@ListWhatIfAnalysesResponse' {} a -> s {whatIfAnalyses = a} :: ListWhatIfAnalysesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWhatIfAnalysesResponse_httpStatus :: Lens.Lens' ListWhatIfAnalysesResponse Prelude.Int
listWhatIfAnalysesResponse_httpStatus = Lens.lens (\ListWhatIfAnalysesResponse' {httpStatus} -> httpStatus) (\s@ListWhatIfAnalysesResponse' {} a -> s {httpStatus = a} :: ListWhatIfAnalysesResponse)

instance Prelude.NFData ListWhatIfAnalysesResponse where
  rnf ListWhatIfAnalysesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf whatIfAnalyses
      `Prelude.seq` Prelude.rnf httpStatus
