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
-- Module      : Amazonka.MediaConnect.ListBridges
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of bridges that are associated with this account and an
-- optionally specified Arn. This request returns a paginated result.
--
-- This operation returns paginated results.
module Amazonka.MediaConnect.ListBridges
  ( -- * Creating a Request
    ListBridges (..),
    newListBridges,

    -- * Request Lenses
    listBridges_filterArn,
    listBridges_maxResults,
    listBridges_nextToken,

    -- * Destructuring the Response
    ListBridgesResponse (..),
    newListBridgesResponse,

    -- * Response Lenses
    listBridgesResponse_bridges,
    listBridgesResponse_nextToken,
    listBridgesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBridges' smart constructor.
data ListBridges = ListBridges'
  { -- | Filter the list results to display only the bridges associated with the
    -- selected Amazon Resource Name (ARN).
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per API request. For example,
    -- you submit a ListBridges request with MaxResults set at 5. Although 20
    -- items match your request, the service returns no more than the first 5
    -- items. (The service also returns a NextToken value that you can use to
    -- fetch the next batch of results.) The service might return fewer results
    -- than the MaxResults value. If MaxResults is not included in the request,
    -- the service defaults to pagination with a maximum of 10 results per
    -- page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListBridges request with MaxResults set at 5.
    -- The service returns the first batch of results (up to 5) and a NextToken
    -- value. To see the next batch of results, you can submit the ListBridges
    -- request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBridges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterArn', 'listBridges_filterArn' - Filter the list results to display only the bridges associated with the
-- selected Amazon Resource Name (ARN).
--
-- 'maxResults', 'listBridges_maxResults' - The maximum number of results to return per API request. For example,
-- you submit a ListBridges request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
--
-- 'nextToken', 'listBridges_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListBridges request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListBridges
-- request a second time and specify the NextToken value.
newListBridges ::
  ListBridges
newListBridges =
  ListBridges'
    { filterArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filter the list results to display only the bridges associated with the
-- selected Amazon Resource Name (ARN).
listBridges_filterArn :: Lens.Lens' ListBridges (Prelude.Maybe Prelude.Text)
listBridges_filterArn = Lens.lens (\ListBridges' {filterArn} -> filterArn) (\s@ListBridges' {} a -> s {filterArn = a} :: ListBridges)

-- | The maximum number of results to return per API request. For example,
-- you submit a ListBridges request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
listBridges_maxResults :: Lens.Lens' ListBridges (Prelude.Maybe Prelude.Natural)
listBridges_maxResults = Lens.lens (\ListBridges' {maxResults} -> maxResults) (\s@ListBridges' {} a -> s {maxResults = a} :: ListBridges)

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListBridges request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListBridges
-- request a second time and specify the NextToken value.
listBridges_nextToken :: Lens.Lens' ListBridges (Prelude.Maybe Prelude.Text)
listBridges_nextToken = Lens.lens (\ListBridges' {nextToken} -> nextToken) (\s@ListBridges' {} a -> s {nextToken = a} :: ListBridges)

instance Core.AWSPager ListBridges where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBridgesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBridgesResponse_bridges
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listBridges_nextToken
          Lens..~ rs
          Lens.^? listBridgesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListBridges where
  type AWSResponse ListBridges = ListBridgesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBridgesResponse'
            Prelude.<$> (x Data..?> "bridges" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBridges where
  hashWithSalt _salt ListBridges' {..} =
    _salt
      `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListBridges where
  rnf ListBridges' {..} =
    Prelude.rnf filterArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListBridges where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBridges where
  toPath = Prelude.const "/v1/bridges"

instance Data.ToQuery ListBridges where
  toQuery ListBridges' {..} =
    Prelude.mconcat
      [ "filterArn" Data.=: filterArn,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListBridgesResponse' smart constructor.
data ListBridgesResponse = ListBridgesResponse'
  { -- | A list of bridge summaries.
    bridges :: Prelude.Maybe [ListedBridge],
    -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListBridges request with MaxResults set at 5.
    -- The service returns the first batch of results (up to 5) and a NextToken
    -- value. To see the next batch of results, you can submit the ListBridges
    -- request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBridgesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridges', 'listBridgesResponse_bridges' - A list of bridge summaries.
--
-- 'nextToken', 'listBridgesResponse_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListBridges request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListBridges
-- request a second time and specify the NextToken value.
--
-- 'httpStatus', 'listBridgesResponse_httpStatus' - The response's http status code.
newListBridgesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBridgesResponse
newListBridgesResponse pHttpStatus_ =
  ListBridgesResponse'
    { bridges = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of bridge summaries.
listBridgesResponse_bridges :: Lens.Lens' ListBridgesResponse (Prelude.Maybe [ListedBridge])
listBridgesResponse_bridges = Lens.lens (\ListBridgesResponse' {bridges} -> bridges) (\s@ListBridgesResponse' {} a -> s {bridges = a} :: ListBridgesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListBridges request with MaxResults set at 5.
-- The service returns the first batch of results (up to 5) and a NextToken
-- value. To see the next batch of results, you can submit the ListBridges
-- request a second time and specify the NextToken value.
listBridgesResponse_nextToken :: Lens.Lens' ListBridgesResponse (Prelude.Maybe Prelude.Text)
listBridgesResponse_nextToken = Lens.lens (\ListBridgesResponse' {nextToken} -> nextToken) (\s@ListBridgesResponse' {} a -> s {nextToken = a} :: ListBridgesResponse)

-- | The response's http status code.
listBridgesResponse_httpStatus :: Lens.Lens' ListBridgesResponse Prelude.Int
listBridgesResponse_httpStatus = Lens.lens (\ListBridgesResponse' {httpStatus} -> httpStatus) (\s@ListBridgesResponse' {} a -> s {httpStatus = a} :: ListBridgesResponse)

instance Prelude.NFData ListBridgesResponse where
  rnf ListBridgesResponse' {..} =
    Prelude.rnf bridges
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
