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
-- Module      : Amazonka.MediaConnect.ListGateways
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of gateways that are associated with this account. This
-- request returns a paginated result.
--
-- This operation returns paginated results.
module Amazonka.MediaConnect.ListGateways
  ( -- * Creating a Request
    ListGateways (..),
    newListGateways,

    -- * Request Lenses
    listGateways_maxResults,
    listGateways_nextToken,

    -- * Destructuring the Response
    ListGatewaysResponse (..),
    newListGatewaysResponse,

    -- * Response Lenses
    listGatewaysResponse_gateways,
    listGatewaysResponse_nextToken,
    listGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGateways' smart constructor.
data ListGateways = ListGateways'
  { -- | The maximum number of results to return per API request. For example,
    -- you submit a ListGateways request with MaxResults set at 5. Although 20
    -- items match your request, the service returns no more than the first 5
    -- items. (The service also returns a NextToken value that you can use to
    -- fetch the next batch of results.) The service might return fewer results
    -- than the MaxResults value. If MaxResults is not included in the request,
    -- the service defaults to pagination with a maximum of 10 results per
    -- page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListGateways request with MaxResults set at 5.
    -- The service returns the first batch of results (up to 5) and a NextToken
    -- value. To see the next batch of results, you can submit the ListGateways
    -- request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGateways_maxResults' - The maximum number of results to return per API request. For example,
-- you submit a ListGateways request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
--
-- 'nextToken', 'listGateways_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListGateways request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListGateways
-- request a second time and specify the NextToken value.
newListGateways ::
  ListGateways
newListGateways =
  ListGateways'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return per API request. For example,
-- you submit a ListGateways request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
listGateways_maxResults :: Lens.Lens' ListGateways (Prelude.Maybe Prelude.Natural)
listGateways_maxResults = Lens.lens (\ListGateways' {maxResults} -> maxResults) (\s@ListGateways' {} a -> s {maxResults = a} :: ListGateways)

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListGateways request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListGateways
-- request a second time and specify the NextToken value.
listGateways_nextToken :: Lens.Lens' ListGateways (Prelude.Maybe Prelude.Text)
listGateways_nextToken = Lens.lens (\ListGateways' {nextToken} -> nextToken) (\s@ListGateways' {} a -> s {nextToken = a} :: ListGateways)

instance Core.AWSPager ListGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGatewaysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGatewaysResponse_gateways
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listGateways_nextToken
          Lens..~ rs
          Lens.^? listGatewaysResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListGateways where
  type AWSResponse ListGateways = ListGatewaysResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            Prelude.<$> (x Data..?> "gateways" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGateways where
  hashWithSalt _salt ListGateways' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGateways where
  rnf ListGateways' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListGateways where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListGateways where
  toPath = Prelude.const "/v1/gateways"

instance Data.ToQuery ListGateways where
  toQuery ListGateways' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { -- | A list of gateway summaries.
    gateways :: Prelude.Maybe [ListedGateway],
    -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListGateways request with MaxResults set at 5.
    -- The service returns the first batch of results (up to 5) and a NextToken
    -- value. To see the next batch of results, you can submit the ListGateways
    -- request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gateways', 'listGatewaysResponse_gateways' - A list of gateway summaries.
--
-- 'nextToken', 'listGatewaysResponse_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListGateways request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListGateways
-- request a second time and specify the NextToken value.
--
-- 'httpStatus', 'listGatewaysResponse_httpStatus' - The response's http status code.
newListGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGatewaysResponse
newListGatewaysResponse pHttpStatus_ =
  ListGatewaysResponse'
    { gateways = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of gateway summaries.
listGatewaysResponse_gateways :: Lens.Lens' ListGatewaysResponse (Prelude.Maybe [ListedGateway])
listGatewaysResponse_gateways = Lens.lens (\ListGatewaysResponse' {gateways} -> gateways) (\s@ListGatewaysResponse' {} a -> s {gateways = a} :: ListGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListGateways request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListGateways
-- request a second time and specify the NextToken value.
listGatewaysResponse_nextToken :: Lens.Lens' ListGatewaysResponse (Prelude.Maybe Prelude.Text)
listGatewaysResponse_nextToken = Lens.lens (\ListGatewaysResponse' {nextToken} -> nextToken) (\s@ListGatewaysResponse' {} a -> s {nextToken = a} :: ListGatewaysResponse)

-- | The response's http status code.
listGatewaysResponse_httpStatus :: Lens.Lens' ListGatewaysResponse Prelude.Int
listGatewaysResponse_httpStatus = Lens.lens (\ListGatewaysResponse' {httpStatus} -> httpStatus) (\s@ListGatewaysResponse' {} a -> s {httpStatus = a} :: ListGatewaysResponse)

instance Prelude.NFData ListGatewaysResponse where
  rnf ListGatewaysResponse' {..} =
    Prelude.rnf gateways
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
