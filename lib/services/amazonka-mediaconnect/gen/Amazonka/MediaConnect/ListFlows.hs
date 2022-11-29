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
-- Module      : Amazonka.MediaConnect.ListFlows
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of flows that are associated with this account. This
-- request returns a paginated result.
--
-- This operation returns paginated results.
module Amazonka.MediaConnect.ListFlows
  ( -- * Creating a Request
    ListFlows (..),
    newListFlows,

    -- * Request Lenses
    listFlows_nextToken,
    listFlows_maxResults,

    -- * Destructuring the Response
    ListFlowsResponse (..),
    newListFlowsResponse,

    -- * Response Lenses
    listFlowsResponse_nextToken,
    listFlowsResponse_flows,
    listFlowsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFlows' smart constructor.
data ListFlows = ListFlows'
  { -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListFlows request with MaxResults set at 5.
    -- The service returns the first batch of results (up to 5) and a NextToken
    -- value. To see the next batch of results, you can submit the ListFlows
    -- request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per API request. For example,
    -- you submit a ListFlows request with MaxResults set at 5. Although 20
    -- items match your request, the service returns no more than the first 5
    -- items. (The service also returns a NextToken value that you can use to
    -- fetch the next batch of results.) The service might return fewer results
    -- than the MaxResults value. If MaxResults is not included in the request,
    -- the service defaults to pagination with a maximum of 10 results per
    -- page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFlows_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListFlows request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListFlows
-- request a second time and specify the NextToken value.
--
-- 'maxResults', 'listFlows_maxResults' - The maximum number of results to return per API request. For example,
-- you submit a ListFlows request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
newListFlows ::
  ListFlows
newListFlows =
  ListFlows'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListFlows request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListFlows
-- request a second time and specify the NextToken value.
listFlows_nextToken :: Lens.Lens' ListFlows (Prelude.Maybe Prelude.Text)
listFlows_nextToken = Lens.lens (\ListFlows' {nextToken} -> nextToken) (\s@ListFlows' {} a -> s {nextToken = a} :: ListFlows)

-- | The maximum number of results to return per API request. For example,
-- you submit a ListFlows request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
listFlows_maxResults :: Lens.Lens' ListFlows (Prelude.Maybe Prelude.Natural)
listFlows_maxResults = Lens.lens (\ListFlows' {maxResults} -> maxResults) (\s@ListFlows' {} a -> s {maxResults = a} :: ListFlows)

instance Core.AWSPager ListFlows where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFlowsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFlowsResponse_flows Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFlows_nextToken
          Lens..~ rs
          Lens.^? listFlowsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFlows where
  type AWSResponse ListFlows = ListFlowsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFlowsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "flows" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFlows where
  hashWithSalt _salt ListFlows' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListFlows where
  rnf ListFlows' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListFlows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListFlows where
  toPath = Prelude.const "/v1/flows"

instance Core.ToQuery ListFlows where
  toQuery ListFlows' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListFlowsResponse' smart constructor.
data ListFlowsResponse = ListFlowsResponse'
  { -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListFlows request with MaxResults set at 5.
    -- The service returns the first batch of results (up to 5) and a NextToken
    -- value. To see the next batch of results, you can submit the ListFlows
    -- request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of flow summaries.
    flows :: Prelude.Maybe [ListedFlow],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFlowsResponse_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListFlows request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListFlows
-- request a second time and specify the NextToken value.
--
-- 'flows', 'listFlowsResponse_flows' - A list of flow summaries.
--
-- 'httpStatus', 'listFlowsResponse_httpStatus' - The response's http status code.
newListFlowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFlowsResponse
newListFlowsResponse pHttpStatus_ =
  ListFlowsResponse'
    { nextToken = Prelude.Nothing,
      flows = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListFlows request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListFlows
-- request a second time and specify the NextToken value.
listFlowsResponse_nextToken :: Lens.Lens' ListFlowsResponse (Prelude.Maybe Prelude.Text)
listFlowsResponse_nextToken = Lens.lens (\ListFlowsResponse' {nextToken} -> nextToken) (\s@ListFlowsResponse' {} a -> s {nextToken = a} :: ListFlowsResponse)

-- | A list of flow summaries.
listFlowsResponse_flows :: Lens.Lens' ListFlowsResponse (Prelude.Maybe [ListedFlow])
listFlowsResponse_flows = Lens.lens (\ListFlowsResponse' {flows} -> flows) (\s@ListFlowsResponse' {} a -> s {flows = a} :: ListFlowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFlowsResponse_httpStatus :: Lens.Lens' ListFlowsResponse Prelude.Int
listFlowsResponse_httpStatus = Lens.lens (\ListFlowsResponse' {httpStatus} -> httpStatus) (\s@ListFlowsResponse' {} a -> s {httpStatus = a} :: ListFlowsResponse)

instance Prelude.NFData ListFlowsResponse where
  rnf ListFlowsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf flows
      `Prelude.seq` Prelude.rnf httpStatus
