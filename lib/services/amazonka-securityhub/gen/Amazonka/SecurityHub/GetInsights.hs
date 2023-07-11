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
-- Module      : Amazonka.SecurityHub.GetInsights
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists and describes insights for the specified insight ARNs.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.GetInsights
  ( -- * Creating a Request
    GetInsights (..),
    newGetInsights,

    -- * Request Lenses
    getInsights_insightArns,
    getInsights_maxResults,
    getInsights_nextToken,

    -- * Destructuring the Response
    GetInsightsResponse (..),
    newGetInsightsResponse,

    -- * Response Lenses
    getInsightsResponse_nextToken,
    getInsightsResponse_httpStatus,
    getInsightsResponse_insights,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newGetInsights' smart constructor.
data GetInsights = GetInsights'
  { -- | The ARNs of the insights to describe. If you do not provide any insight
    -- ARNs, then @GetInsights@ returns all of your custom insights. It does
    -- not return any managed insights.
    insightArns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that is required for pagination. On your first call to the
    -- @GetInsights@ operation, set the value of this parameter to @NULL@.
    --
    -- For subsequent calls to the operation, to continue listing data, set the
    -- value of this parameter to the value returned from the previous
    -- response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightArns', 'getInsights_insightArns' - The ARNs of the insights to describe. If you do not provide any insight
-- ARNs, then @GetInsights@ returns all of your custom insights. It does
-- not return any managed insights.
--
-- 'maxResults', 'getInsights_maxResults' - The maximum number of items to return in the response.
--
-- 'nextToken', 'getInsights_nextToken' - The token that is required for pagination. On your first call to the
-- @GetInsights@ operation, set the value of this parameter to @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
newGetInsights ::
  GetInsights
newGetInsights =
  GetInsights'
    { insightArns = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ARNs of the insights to describe. If you do not provide any insight
-- ARNs, then @GetInsights@ returns all of your custom insights. It does
-- not return any managed insights.
getInsights_insightArns :: Lens.Lens' GetInsights (Prelude.Maybe [Prelude.Text])
getInsights_insightArns = Lens.lens (\GetInsights' {insightArns} -> insightArns) (\s@GetInsights' {} a -> s {insightArns = a} :: GetInsights) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return in the response.
getInsights_maxResults :: Lens.Lens' GetInsights (Prelude.Maybe Prelude.Natural)
getInsights_maxResults = Lens.lens (\GetInsights' {maxResults} -> maxResults) (\s@GetInsights' {} a -> s {maxResults = a} :: GetInsights)

-- | The token that is required for pagination. On your first call to the
-- @GetInsights@ operation, set the value of this parameter to @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
getInsights_nextToken :: Lens.Lens' GetInsights (Prelude.Maybe Prelude.Text)
getInsights_nextToken = Lens.lens (\GetInsights' {nextToken} -> nextToken) (\s@GetInsights' {} a -> s {nextToken = a} :: GetInsights)

instance Core.AWSPager GetInsights where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInsightsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. getInsightsResponse_insights) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getInsights_nextToken
          Lens..~ rs
          Lens.^? getInsightsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetInsights where
  type AWSResponse GetInsights = GetInsightsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Insights" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetInsights where
  hashWithSalt _salt GetInsights' {..} =
    _salt
      `Prelude.hashWithSalt` insightArns
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetInsights where
  rnf GetInsights' {..} =
    Prelude.rnf insightArns
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInsights where
  toJSON GetInsights' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InsightArns" Data..=) Prelude.<$> insightArns,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetInsights where
  toPath = Prelude.const "/insights/get"

instance Data.ToQuery GetInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightsResponse' smart constructor.
data GetInsightsResponse = GetInsightsResponse'
  { -- | The pagination token to use to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The insights returned by the operation.
    insights :: [Insight]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInsightsResponse_nextToken' - The pagination token to use to request the next page of results.
--
-- 'httpStatus', 'getInsightsResponse_httpStatus' - The response's http status code.
--
-- 'insights', 'getInsightsResponse_insights' - The insights returned by the operation.
newGetInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInsightsResponse
newGetInsightsResponse pHttpStatus_ =
  GetInsightsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      insights = Prelude.mempty
    }

-- | The pagination token to use to request the next page of results.
getInsightsResponse_nextToken :: Lens.Lens' GetInsightsResponse (Prelude.Maybe Prelude.Text)
getInsightsResponse_nextToken = Lens.lens (\GetInsightsResponse' {nextToken} -> nextToken) (\s@GetInsightsResponse' {} a -> s {nextToken = a} :: GetInsightsResponse)

-- | The response's http status code.
getInsightsResponse_httpStatus :: Lens.Lens' GetInsightsResponse Prelude.Int
getInsightsResponse_httpStatus = Lens.lens (\GetInsightsResponse' {httpStatus} -> httpStatus) (\s@GetInsightsResponse' {} a -> s {httpStatus = a} :: GetInsightsResponse)

-- | The insights returned by the operation.
getInsightsResponse_insights :: Lens.Lens' GetInsightsResponse [Insight]
getInsightsResponse_insights = Lens.lens (\GetInsightsResponse' {insights} -> insights) (\s@GetInsightsResponse' {} a -> s {insights = a} :: GetInsightsResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetInsightsResponse where
  rnf GetInsightsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf insights
