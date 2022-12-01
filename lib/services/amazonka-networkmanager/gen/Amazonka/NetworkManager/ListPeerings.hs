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
-- Module      : Amazonka.NetworkManager.ListPeerings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the peerings for a core network.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.ListPeerings
  ( -- * Creating a Request
    ListPeerings (..),
    newListPeerings,

    -- * Request Lenses
    listPeerings_coreNetworkId,
    listPeerings_nextToken,
    listPeerings_peeringType,
    listPeerings_state,
    listPeerings_edgeLocation,
    listPeerings_maxResults,

    -- * Destructuring the Response
    ListPeeringsResponse (..),
    newListPeeringsResponse,

    -- * Response Lenses
    listPeeringsResponse_nextToken,
    listPeeringsResponse_peerings,
    listPeeringsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPeerings' smart constructor.
data ListPeerings = ListPeerings'
  { -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of a peering requests.
    peeringType :: Prelude.Maybe PeeringType,
    -- | Returns a list of the peering request states.
    state :: Prelude.Maybe PeeringState,
    -- | Returns a list edge locations for the
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPeerings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkId', 'listPeerings_coreNetworkId' - The ID of a core network.
--
-- 'nextToken', 'listPeerings_nextToken' - The token for the next page of results.
--
-- 'peeringType', 'listPeerings_peeringType' - Returns a list of a peering requests.
--
-- 'state', 'listPeerings_state' - Returns a list of the peering request states.
--
-- 'edgeLocation', 'listPeerings_edgeLocation' - Returns a list edge locations for the
--
-- 'maxResults', 'listPeerings_maxResults' - The maximum number of results to return.
newListPeerings ::
  ListPeerings
newListPeerings =
  ListPeerings'
    { coreNetworkId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      peeringType = Prelude.Nothing,
      state = Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The ID of a core network.
listPeerings_coreNetworkId :: Lens.Lens' ListPeerings (Prelude.Maybe Prelude.Text)
listPeerings_coreNetworkId = Lens.lens (\ListPeerings' {coreNetworkId} -> coreNetworkId) (\s@ListPeerings' {} a -> s {coreNetworkId = a} :: ListPeerings)

-- | The token for the next page of results.
listPeerings_nextToken :: Lens.Lens' ListPeerings (Prelude.Maybe Prelude.Text)
listPeerings_nextToken = Lens.lens (\ListPeerings' {nextToken} -> nextToken) (\s@ListPeerings' {} a -> s {nextToken = a} :: ListPeerings)

-- | Returns a list of a peering requests.
listPeerings_peeringType :: Lens.Lens' ListPeerings (Prelude.Maybe PeeringType)
listPeerings_peeringType = Lens.lens (\ListPeerings' {peeringType} -> peeringType) (\s@ListPeerings' {} a -> s {peeringType = a} :: ListPeerings)

-- | Returns a list of the peering request states.
listPeerings_state :: Lens.Lens' ListPeerings (Prelude.Maybe PeeringState)
listPeerings_state = Lens.lens (\ListPeerings' {state} -> state) (\s@ListPeerings' {} a -> s {state = a} :: ListPeerings)

-- | Returns a list edge locations for the
listPeerings_edgeLocation :: Lens.Lens' ListPeerings (Prelude.Maybe Prelude.Text)
listPeerings_edgeLocation = Lens.lens (\ListPeerings' {edgeLocation} -> edgeLocation) (\s@ListPeerings' {} a -> s {edgeLocation = a} :: ListPeerings)

-- | The maximum number of results to return.
listPeerings_maxResults :: Lens.Lens' ListPeerings (Prelude.Maybe Prelude.Natural)
listPeerings_maxResults = Lens.lens (\ListPeerings' {maxResults} -> maxResults) (\s@ListPeerings' {} a -> s {maxResults = a} :: ListPeerings)

instance Core.AWSPager ListPeerings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPeeringsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPeeringsResponse_peerings Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPeerings_nextToken
          Lens..~ rs
          Lens.^? listPeeringsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListPeerings where
  type AWSResponse ListPeerings = ListPeeringsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPeeringsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Peerings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPeerings where
  hashWithSalt _salt ListPeerings' {..} =
    _salt `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` peeringType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPeerings where
  rnf ListPeerings' {..} =
    Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf peeringType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListPeerings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListPeerings where
  toPath = Prelude.const "/peerings"

instance Core.ToQuery ListPeerings where
  toQuery ListPeerings' {..} =
    Prelude.mconcat
      [ "coreNetworkId" Core.=: coreNetworkId,
        "nextToken" Core.=: nextToken,
        "peeringType" Core.=: peeringType,
        "state" Core.=: state,
        "edgeLocation" Core.=: edgeLocation,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListPeeringsResponse' smart constructor.
data ListPeeringsResponse = ListPeeringsResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists the transit gateway peerings for the @ListPeerings@ request.
    peerings :: Prelude.Maybe [Peering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPeeringsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPeeringsResponse_nextToken' - The token for the next page of results.
--
-- 'peerings', 'listPeeringsResponse_peerings' - Lists the transit gateway peerings for the @ListPeerings@ request.
--
-- 'httpStatus', 'listPeeringsResponse_httpStatus' - The response's http status code.
newListPeeringsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPeeringsResponse
newListPeeringsResponse pHttpStatus_ =
  ListPeeringsResponse'
    { nextToken = Prelude.Nothing,
      peerings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next page of results.
listPeeringsResponse_nextToken :: Lens.Lens' ListPeeringsResponse (Prelude.Maybe Prelude.Text)
listPeeringsResponse_nextToken = Lens.lens (\ListPeeringsResponse' {nextToken} -> nextToken) (\s@ListPeeringsResponse' {} a -> s {nextToken = a} :: ListPeeringsResponse)

-- | Lists the transit gateway peerings for the @ListPeerings@ request.
listPeeringsResponse_peerings :: Lens.Lens' ListPeeringsResponse (Prelude.Maybe [Peering])
listPeeringsResponse_peerings = Lens.lens (\ListPeeringsResponse' {peerings} -> peerings) (\s@ListPeeringsResponse' {} a -> s {peerings = a} :: ListPeeringsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPeeringsResponse_httpStatus :: Lens.Lens' ListPeeringsResponse Prelude.Int
listPeeringsResponse_httpStatus = Lens.lens (\ListPeeringsResponse' {httpStatus} -> httpStatus) (\s@ListPeeringsResponse' {} a -> s {httpStatus = a} :: ListPeeringsResponse)

instance Prelude.NFData ListPeeringsResponse where
  rnf ListPeeringsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf peerings
      `Prelude.seq` Prelude.rnf httpStatus
