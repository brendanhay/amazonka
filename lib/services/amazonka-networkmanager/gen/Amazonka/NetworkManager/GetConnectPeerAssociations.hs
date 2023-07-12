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
-- Module      : Amazonka.NetworkManager.GetConnectPeerAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a core network Connect peer associations.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetConnectPeerAssociations
  ( -- * Creating a Request
    GetConnectPeerAssociations (..),
    newGetConnectPeerAssociations,

    -- * Request Lenses
    getConnectPeerAssociations_connectPeerIds,
    getConnectPeerAssociations_maxResults,
    getConnectPeerAssociations_nextToken,
    getConnectPeerAssociations_globalNetworkId,

    -- * Destructuring the Response
    GetConnectPeerAssociationsResponse (..),
    newGetConnectPeerAssociationsResponse,

    -- * Response Lenses
    getConnectPeerAssociationsResponse_connectPeerAssociations,
    getConnectPeerAssociationsResponse_nextToken,
    getConnectPeerAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnectPeerAssociations' smart constructor.
data GetConnectPeerAssociations = GetConnectPeerAssociations'
  { -- | The IDs of the Connect peers.
    connectPeerIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectPeerAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeerIds', 'getConnectPeerAssociations_connectPeerIds' - The IDs of the Connect peers.
--
-- 'maxResults', 'getConnectPeerAssociations_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getConnectPeerAssociations_nextToken' - The token for the next page of results.
--
-- 'globalNetworkId', 'getConnectPeerAssociations_globalNetworkId' - The ID of the global network.
newGetConnectPeerAssociations ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetConnectPeerAssociations
newGetConnectPeerAssociations pGlobalNetworkId_ =
  GetConnectPeerAssociations'
    { connectPeerIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | The IDs of the Connect peers.
getConnectPeerAssociations_connectPeerIds :: Lens.Lens' GetConnectPeerAssociations (Prelude.Maybe [Prelude.Text])
getConnectPeerAssociations_connectPeerIds = Lens.lens (\GetConnectPeerAssociations' {connectPeerIds} -> connectPeerIds) (\s@GetConnectPeerAssociations' {} a -> s {connectPeerIds = a} :: GetConnectPeerAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
getConnectPeerAssociations_maxResults :: Lens.Lens' GetConnectPeerAssociations (Prelude.Maybe Prelude.Natural)
getConnectPeerAssociations_maxResults = Lens.lens (\GetConnectPeerAssociations' {maxResults} -> maxResults) (\s@GetConnectPeerAssociations' {} a -> s {maxResults = a} :: GetConnectPeerAssociations)

-- | The token for the next page of results.
getConnectPeerAssociations_nextToken :: Lens.Lens' GetConnectPeerAssociations (Prelude.Maybe Prelude.Text)
getConnectPeerAssociations_nextToken = Lens.lens (\GetConnectPeerAssociations' {nextToken} -> nextToken) (\s@GetConnectPeerAssociations' {} a -> s {nextToken = a} :: GetConnectPeerAssociations)

-- | The ID of the global network.
getConnectPeerAssociations_globalNetworkId :: Lens.Lens' GetConnectPeerAssociations Prelude.Text
getConnectPeerAssociations_globalNetworkId = Lens.lens (\GetConnectPeerAssociations' {globalNetworkId} -> globalNetworkId) (\s@GetConnectPeerAssociations' {} a -> s {globalNetworkId = a} :: GetConnectPeerAssociations)

instance Core.AWSPager GetConnectPeerAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getConnectPeerAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getConnectPeerAssociationsResponse_connectPeerAssociations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getConnectPeerAssociations_nextToken
          Lens..~ rs
          Lens.^? getConnectPeerAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetConnectPeerAssociations where
  type
    AWSResponse GetConnectPeerAssociations =
      GetConnectPeerAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectPeerAssociationsResponse'
            Prelude.<$> ( x
                            Data..?> "ConnectPeerAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectPeerAssociations where
  hashWithSalt _salt GetConnectPeerAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` connectPeerIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData GetConnectPeerAssociations where
  rnf GetConnectPeerAssociations' {..} =
    Prelude.rnf connectPeerIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf globalNetworkId

instance Data.ToHeaders GetConnectPeerAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConnectPeerAssociations where
  toPath GetConnectPeerAssociations' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/connect-peer-associations"
      ]

instance Data.ToQuery GetConnectPeerAssociations where
  toQuery GetConnectPeerAssociations' {..} =
    Prelude.mconcat
      [ "connectPeerIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> connectPeerIds
            ),
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetConnectPeerAssociationsResponse' smart constructor.
data GetConnectPeerAssociationsResponse = GetConnectPeerAssociationsResponse'
  { -- | Displays a list of Connect peer associations.
    connectPeerAssociations :: Prelude.Maybe [ConnectPeerAssociation],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectPeerAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeerAssociations', 'getConnectPeerAssociationsResponse_connectPeerAssociations' - Displays a list of Connect peer associations.
--
-- 'nextToken', 'getConnectPeerAssociationsResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'getConnectPeerAssociationsResponse_httpStatus' - The response's http status code.
newGetConnectPeerAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectPeerAssociationsResponse
newGetConnectPeerAssociationsResponse pHttpStatus_ =
  GetConnectPeerAssociationsResponse'
    { connectPeerAssociations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Displays a list of Connect peer associations.
getConnectPeerAssociationsResponse_connectPeerAssociations :: Lens.Lens' GetConnectPeerAssociationsResponse (Prelude.Maybe [ConnectPeerAssociation])
getConnectPeerAssociationsResponse_connectPeerAssociations = Lens.lens (\GetConnectPeerAssociationsResponse' {connectPeerAssociations} -> connectPeerAssociations) (\s@GetConnectPeerAssociationsResponse' {} a -> s {connectPeerAssociations = a} :: GetConnectPeerAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getConnectPeerAssociationsResponse_nextToken :: Lens.Lens' GetConnectPeerAssociationsResponse (Prelude.Maybe Prelude.Text)
getConnectPeerAssociationsResponse_nextToken = Lens.lens (\GetConnectPeerAssociationsResponse' {nextToken} -> nextToken) (\s@GetConnectPeerAssociationsResponse' {} a -> s {nextToken = a} :: GetConnectPeerAssociationsResponse)

-- | The response's http status code.
getConnectPeerAssociationsResponse_httpStatus :: Lens.Lens' GetConnectPeerAssociationsResponse Prelude.Int
getConnectPeerAssociationsResponse_httpStatus = Lens.lens (\GetConnectPeerAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetConnectPeerAssociationsResponse' {} a -> s {httpStatus = a} :: GetConnectPeerAssociationsResponse)

instance
  Prelude.NFData
    GetConnectPeerAssociationsResponse
  where
  rnf GetConnectPeerAssociationsResponse' {..} =
    Prelude.rnf connectPeerAssociations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
