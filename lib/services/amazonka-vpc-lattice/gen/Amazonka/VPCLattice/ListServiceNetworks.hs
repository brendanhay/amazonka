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
-- Module      : Amazonka.VPCLattice.ListServiceNetworks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the service networks owned by the caller account or shared with
-- the caller account. Also includes the account ID in the ARN to show
-- which account owns the service network.
--
-- This operation returns paginated results.
module Amazonka.VPCLattice.ListServiceNetworks
  ( -- * Creating a Request
    ListServiceNetworks (..),
    newListServiceNetworks,

    -- * Request Lenses
    listServiceNetworks_maxResults,
    listServiceNetworks_nextToken,

    -- * Destructuring the Response
    ListServiceNetworksResponse (..),
    newListServiceNetworksResponse,

    -- * Response Lenses
    listServiceNetworksResponse_nextToken,
    listServiceNetworksResponse_httpStatus,
    listServiceNetworksResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newListServiceNetworks' smart constructor.
data ListServiceNetworks = ListServiceNetworks'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceNetworks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listServiceNetworks_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listServiceNetworks_nextToken' - A pagination token for the next page of results.
newListServiceNetworks ::
  ListServiceNetworks
newListServiceNetworks =
  ListServiceNetworks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return.
listServiceNetworks_maxResults :: Lens.Lens' ListServiceNetworks (Prelude.Maybe Prelude.Natural)
listServiceNetworks_maxResults = Lens.lens (\ListServiceNetworks' {maxResults} -> maxResults) (\s@ListServiceNetworks' {} a -> s {maxResults = a} :: ListServiceNetworks)

-- | A pagination token for the next page of results.
listServiceNetworks_nextToken :: Lens.Lens' ListServiceNetworks (Prelude.Maybe Prelude.Text)
listServiceNetworks_nextToken = Lens.lens (\ListServiceNetworks' {nextToken} -> nextToken) (\s@ListServiceNetworks' {} a -> s {nextToken = a} :: ListServiceNetworks)

instance Core.AWSPager ListServiceNetworks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceNetworksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listServiceNetworksResponse_items) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listServiceNetworks_nextToken
          Lens..~ rs
          Lens.^? listServiceNetworksResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListServiceNetworks where
  type
    AWSResponse ListServiceNetworks =
      ListServiceNetworksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceNetworksResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListServiceNetworks where
  hashWithSalt _salt ListServiceNetworks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListServiceNetworks where
  rnf ListServiceNetworks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListServiceNetworks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListServiceNetworks where
  toPath = Prelude.const "/servicenetworks"

instance Data.ToQuery ListServiceNetworks where
  toQuery ListServiceNetworks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListServiceNetworksResponse' smart constructor.
data ListServiceNetworksResponse = ListServiceNetworksResponse'
  { -- | If there are additional results, a pagination token for the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the service networks.
    items :: [ServiceNetworkSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceNetworksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceNetworksResponse_nextToken' - If there are additional results, a pagination token for the next page of
-- results.
--
-- 'httpStatus', 'listServiceNetworksResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listServiceNetworksResponse_items' - Information about the service networks.
newListServiceNetworksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceNetworksResponse
newListServiceNetworksResponse pHttpStatus_ =
  ListServiceNetworksResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | If there are additional results, a pagination token for the next page of
-- results.
listServiceNetworksResponse_nextToken :: Lens.Lens' ListServiceNetworksResponse (Prelude.Maybe Prelude.Text)
listServiceNetworksResponse_nextToken = Lens.lens (\ListServiceNetworksResponse' {nextToken} -> nextToken) (\s@ListServiceNetworksResponse' {} a -> s {nextToken = a} :: ListServiceNetworksResponse)

-- | The response's http status code.
listServiceNetworksResponse_httpStatus :: Lens.Lens' ListServiceNetworksResponse Prelude.Int
listServiceNetworksResponse_httpStatus = Lens.lens (\ListServiceNetworksResponse' {httpStatus} -> httpStatus) (\s@ListServiceNetworksResponse' {} a -> s {httpStatus = a} :: ListServiceNetworksResponse)

-- | Information about the service networks.
listServiceNetworksResponse_items :: Lens.Lens' ListServiceNetworksResponse [ServiceNetworkSummary]
listServiceNetworksResponse_items = Lens.lens (\ListServiceNetworksResponse' {items} -> items) (\s@ListServiceNetworksResponse' {} a -> s {items = a} :: ListServiceNetworksResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListServiceNetworksResponse where
  rnf ListServiceNetworksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
