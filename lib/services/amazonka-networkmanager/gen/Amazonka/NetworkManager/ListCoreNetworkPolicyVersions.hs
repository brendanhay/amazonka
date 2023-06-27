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
-- Module      : Amazonka.NetworkManager.ListCoreNetworkPolicyVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of core network policy versions.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.ListCoreNetworkPolicyVersions
  ( -- * Creating a Request
    ListCoreNetworkPolicyVersions (..),
    newListCoreNetworkPolicyVersions,

    -- * Request Lenses
    listCoreNetworkPolicyVersions_maxResults,
    listCoreNetworkPolicyVersions_nextToken,
    listCoreNetworkPolicyVersions_coreNetworkId,

    -- * Destructuring the Response
    ListCoreNetworkPolicyVersionsResponse (..),
    newListCoreNetworkPolicyVersionsResponse,

    -- * Response Lenses
    listCoreNetworkPolicyVersionsResponse_coreNetworkPolicyVersions,
    listCoreNetworkPolicyVersionsResponse_nextToken,
    listCoreNetworkPolicyVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCoreNetworkPolicyVersions' smart constructor.
data ListCoreNetworkPolicyVersions = ListCoreNetworkPolicyVersions'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoreNetworkPolicyVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCoreNetworkPolicyVersions_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listCoreNetworkPolicyVersions_nextToken' - The token for the next page of results.
--
-- 'coreNetworkId', 'listCoreNetworkPolicyVersions_coreNetworkId' - The ID of a core network.
newListCoreNetworkPolicyVersions ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  ListCoreNetworkPolicyVersions
newListCoreNetworkPolicyVersions pCoreNetworkId_ =
  ListCoreNetworkPolicyVersions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      coreNetworkId = pCoreNetworkId_
    }

-- | The maximum number of results to return.
listCoreNetworkPolicyVersions_maxResults :: Lens.Lens' ListCoreNetworkPolicyVersions (Prelude.Maybe Prelude.Natural)
listCoreNetworkPolicyVersions_maxResults = Lens.lens (\ListCoreNetworkPolicyVersions' {maxResults} -> maxResults) (\s@ListCoreNetworkPolicyVersions' {} a -> s {maxResults = a} :: ListCoreNetworkPolicyVersions)

-- | The token for the next page of results.
listCoreNetworkPolicyVersions_nextToken :: Lens.Lens' ListCoreNetworkPolicyVersions (Prelude.Maybe Prelude.Text)
listCoreNetworkPolicyVersions_nextToken = Lens.lens (\ListCoreNetworkPolicyVersions' {nextToken} -> nextToken) (\s@ListCoreNetworkPolicyVersions' {} a -> s {nextToken = a} :: ListCoreNetworkPolicyVersions)

-- | The ID of a core network.
listCoreNetworkPolicyVersions_coreNetworkId :: Lens.Lens' ListCoreNetworkPolicyVersions Prelude.Text
listCoreNetworkPolicyVersions_coreNetworkId = Lens.lens (\ListCoreNetworkPolicyVersions' {coreNetworkId} -> coreNetworkId) (\s@ListCoreNetworkPolicyVersions' {} a -> s {coreNetworkId = a} :: ListCoreNetworkPolicyVersions)

instance Core.AWSPager ListCoreNetworkPolicyVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoreNetworkPolicyVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCoreNetworkPolicyVersionsResponse_coreNetworkPolicyVersions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCoreNetworkPolicyVersions_nextToken
          Lens..~ rs
          Lens.^? listCoreNetworkPolicyVersionsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListCoreNetworkPolicyVersions
  where
  type
    AWSResponse ListCoreNetworkPolicyVersions =
      ListCoreNetworkPolicyVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoreNetworkPolicyVersionsResponse'
            Prelude.<$> ( x
                            Data..?> "CoreNetworkPolicyVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCoreNetworkPolicyVersions
  where
  hashWithSalt _salt ListCoreNetworkPolicyVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` coreNetworkId

instance Prelude.NFData ListCoreNetworkPolicyVersions where
  rnf ListCoreNetworkPolicyVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf coreNetworkId

instance Data.ToHeaders ListCoreNetworkPolicyVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCoreNetworkPolicyVersions where
  toPath ListCoreNetworkPolicyVersions' {..} =
    Prelude.mconcat
      [ "/core-networks/",
        Data.toBS coreNetworkId,
        "/core-network-policy-versions"
      ]

instance Data.ToQuery ListCoreNetworkPolicyVersions where
  toQuery ListCoreNetworkPolicyVersions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListCoreNetworkPolicyVersionsResponse' smart constructor.
data ListCoreNetworkPolicyVersionsResponse = ListCoreNetworkPolicyVersionsResponse'
  { -- | Describes core network policy versions.
    coreNetworkPolicyVersions :: Prelude.Maybe [CoreNetworkPolicyVersion],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoreNetworkPolicyVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkPolicyVersions', 'listCoreNetworkPolicyVersionsResponse_coreNetworkPolicyVersions' - Describes core network policy versions.
--
-- 'nextToken', 'listCoreNetworkPolicyVersionsResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'listCoreNetworkPolicyVersionsResponse_httpStatus' - The response's http status code.
newListCoreNetworkPolicyVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCoreNetworkPolicyVersionsResponse
newListCoreNetworkPolicyVersionsResponse pHttpStatus_ =
  ListCoreNetworkPolicyVersionsResponse'
    { coreNetworkPolicyVersions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes core network policy versions.
listCoreNetworkPolicyVersionsResponse_coreNetworkPolicyVersions :: Lens.Lens' ListCoreNetworkPolicyVersionsResponse (Prelude.Maybe [CoreNetworkPolicyVersion])
listCoreNetworkPolicyVersionsResponse_coreNetworkPolicyVersions = Lens.lens (\ListCoreNetworkPolicyVersionsResponse' {coreNetworkPolicyVersions} -> coreNetworkPolicyVersions) (\s@ListCoreNetworkPolicyVersionsResponse' {} a -> s {coreNetworkPolicyVersions = a} :: ListCoreNetworkPolicyVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
listCoreNetworkPolicyVersionsResponse_nextToken :: Lens.Lens' ListCoreNetworkPolicyVersionsResponse (Prelude.Maybe Prelude.Text)
listCoreNetworkPolicyVersionsResponse_nextToken = Lens.lens (\ListCoreNetworkPolicyVersionsResponse' {nextToken} -> nextToken) (\s@ListCoreNetworkPolicyVersionsResponse' {} a -> s {nextToken = a} :: ListCoreNetworkPolicyVersionsResponse)

-- | The response's http status code.
listCoreNetworkPolicyVersionsResponse_httpStatus :: Lens.Lens' ListCoreNetworkPolicyVersionsResponse Prelude.Int
listCoreNetworkPolicyVersionsResponse_httpStatus = Lens.lens (\ListCoreNetworkPolicyVersionsResponse' {httpStatus} -> httpStatus) (\s@ListCoreNetworkPolicyVersionsResponse' {} a -> s {httpStatus = a} :: ListCoreNetworkPolicyVersionsResponse)

instance
  Prelude.NFData
    ListCoreNetworkPolicyVersionsResponse
  where
  rnf ListCoreNetworkPolicyVersionsResponse' {..} =
    Prelude.rnf coreNetworkPolicyVersions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
