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
-- Module      : Amazonka.MediaConnect.ListEntitlements
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of all entitlements that have been granted to this
-- account. This request returns 20 results per page.
--
-- This operation returns paginated results.
module Amazonka.MediaConnect.ListEntitlements
  ( -- * Creating a Request
    ListEntitlements (..),
    newListEntitlements,

    -- * Request Lenses
    listEntitlements_maxResults,
    listEntitlements_nextToken,

    -- * Destructuring the Response
    ListEntitlementsResponse (..),
    newListEntitlementsResponse,

    -- * Response Lenses
    listEntitlementsResponse_entitlements,
    listEntitlementsResponse_nextToken,
    listEntitlementsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntitlements' smart constructor.
data ListEntitlements = ListEntitlements'
  { -- | The maximum number of results to return per API request. For example,
    -- you submit a ListEntitlements request with MaxResults set at 5. Although
    -- 20 items match your request, the service returns no more than the first
    -- 5 items. (The service also returns a NextToken value that you can use to
    -- fetch the next batch of results.) The service might return fewer results
    -- than the MaxResults value. If MaxResults is not included in the request,
    -- the service defaults to pagination with a maximum of 20 results per
    -- page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListEntitlements request with MaxResults set
    -- at 5. The service returns the first batch of results (up to 5) and a
    -- NextToken value. To see the next batch of results, you can submit the
    -- ListEntitlements request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitlements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEntitlements_maxResults' - The maximum number of results to return per API request. For example,
-- you submit a ListEntitlements request with MaxResults set at 5. Although
-- 20 items match your request, the service returns no more than the first
-- 5 items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 20 results per
-- page.
--
-- 'nextToken', 'listEntitlements_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListEntitlements request with MaxResults set
-- at 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListEntitlements request a second time and specify the NextToken value.
newListEntitlements ::
  ListEntitlements
newListEntitlements =
  ListEntitlements'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return per API request. For example,
-- you submit a ListEntitlements request with MaxResults set at 5. Although
-- 20 items match your request, the service returns no more than the first
-- 5 items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 20 results per
-- page.
listEntitlements_maxResults :: Lens.Lens' ListEntitlements (Prelude.Maybe Prelude.Natural)
listEntitlements_maxResults = Lens.lens (\ListEntitlements' {maxResults} -> maxResults) (\s@ListEntitlements' {} a -> s {maxResults = a} :: ListEntitlements)

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListEntitlements request with MaxResults set
-- at 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListEntitlements request a second time and specify the NextToken value.
listEntitlements_nextToken :: Lens.Lens' ListEntitlements (Prelude.Maybe Prelude.Text)
listEntitlements_nextToken = Lens.lens (\ListEntitlements' {nextToken} -> nextToken) (\s@ListEntitlements' {} a -> s {nextToken = a} :: ListEntitlements)

instance Core.AWSPager ListEntitlements where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEntitlementsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEntitlementsResponse_entitlements
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEntitlements_nextToken
          Lens..~ rs
          Lens.^? listEntitlementsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEntitlements where
  type
    AWSResponse ListEntitlements =
      ListEntitlementsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntitlementsResponse'
            Prelude.<$> (x Data..?> "entitlements" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntitlements where
  hashWithSalt _salt ListEntitlements' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEntitlements where
  rnf ListEntitlements' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEntitlements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEntitlements where
  toPath = Prelude.const "/v1/entitlements"

instance Data.ToQuery ListEntitlements where
  toQuery ListEntitlements' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEntitlementsResponse' smart constructor.
data ListEntitlementsResponse = ListEntitlementsResponse'
  { -- | A list of entitlements that have been granted to you from other AWS
    -- accounts.
    entitlements :: Prelude.Maybe [ListedEntitlement],
    -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListEntitlements request with MaxResults set
    -- at 5. The service returns the first batch of results (up to 5) and a
    -- NextToken value. To see the next batch of results, you can submit the
    -- ListEntitlements request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitlementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlements', 'listEntitlementsResponse_entitlements' - A list of entitlements that have been granted to you from other AWS
-- accounts.
--
-- 'nextToken', 'listEntitlementsResponse_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListEntitlements request with MaxResults set
-- at 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListEntitlements request a second time and specify the NextToken value.
--
-- 'httpStatus', 'listEntitlementsResponse_httpStatus' - The response's http status code.
newListEntitlementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntitlementsResponse
newListEntitlementsResponse pHttpStatus_ =
  ListEntitlementsResponse'
    { entitlements =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of entitlements that have been granted to you from other AWS
-- accounts.
listEntitlementsResponse_entitlements :: Lens.Lens' ListEntitlementsResponse (Prelude.Maybe [ListedEntitlement])
listEntitlementsResponse_entitlements = Lens.lens (\ListEntitlementsResponse' {entitlements} -> entitlements) (\s@ListEntitlementsResponse' {} a -> s {entitlements = a} :: ListEntitlementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListEntitlements request with MaxResults set
-- at 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListEntitlements request a second time and specify the NextToken value.
listEntitlementsResponse_nextToken :: Lens.Lens' ListEntitlementsResponse (Prelude.Maybe Prelude.Text)
listEntitlementsResponse_nextToken = Lens.lens (\ListEntitlementsResponse' {nextToken} -> nextToken) (\s@ListEntitlementsResponse' {} a -> s {nextToken = a} :: ListEntitlementsResponse)

-- | The response's http status code.
listEntitlementsResponse_httpStatus :: Lens.Lens' ListEntitlementsResponse Prelude.Int
listEntitlementsResponse_httpStatus = Lens.lens (\ListEntitlementsResponse' {httpStatus} -> httpStatus) (\s@ListEntitlementsResponse' {} a -> s {httpStatus = a} :: ListEntitlementsResponse)

instance Prelude.NFData ListEntitlementsResponse where
  rnf ListEntitlementsResponse' {..} =
    Prelude.rnf entitlements
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
