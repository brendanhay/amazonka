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
-- Module      : Amazonka.NetworkManager.ListConnectPeers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of core network Connect peers.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.ListConnectPeers
  ( -- * Creating a Request
    ListConnectPeers (..),
    newListConnectPeers,

    -- * Request Lenses
    listConnectPeers_connectAttachmentId,
    listConnectPeers_coreNetworkId,
    listConnectPeers_maxResults,
    listConnectPeers_nextToken,

    -- * Destructuring the Response
    ListConnectPeersResponse (..),
    newListConnectPeersResponse,

    -- * Response Lenses
    listConnectPeersResponse_connectPeers,
    listConnectPeersResponse_nextToken,
    listConnectPeersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConnectPeers' smart constructor.
data ListConnectPeers = ListConnectPeers'
  { -- | The ID of the attachment.
    connectAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectPeers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectAttachmentId', 'listConnectPeers_connectAttachmentId' - The ID of the attachment.
--
-- 'coreNetworkId', 'listConnectPeers_coreNetworkId' - The ID of a core network.
--
-- 'maxResults', 'listConnectPeers_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listConnectPeers_nextToken' - The token for the next page of results.
newListConnectPeers ::
  ListConnectPeers
newListConnectPeers =
  ListConnectPeers'
    { connectAttachmentId =
        Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ID of the attachment.
listConnectPeers_connectAttachmentId :: Lens.Lens' ListConnectPeers (Prelude.Maybe Prelude.Text)
listConnectPeers_connectAttachmentId = Lens.lens (\ListConnectPeers' {connectAttachmentId} -> connectAttachmentId) (\s@ListConnectPeers' {} a -> s {connectAttachmentId = a} :: ListConnectPeers)

-- | The ID of a core network.
listConnectPeers_coreNetworkId :: Lens.Lens' ListConnectPeers (Prelude.Maybe Prelude.Text)
listConnectPeers_coreNetworkId = Lens.lens (\ListConnectPeers' {coreNetworkId} -> coreNetworkId) (\s@ListConnectPeers' {} a -> s {coreNetworkId = a} :: ListConnectPeers)

-- | The maximum number of results to return.
listConnectPeers_maxResults :: Lens.Lens' ListConnectPeers (Prelude.Maybe Prelude.Natural)
listConnectPeers_maxResults = Lens.lens (\ListConnectPeers' {maxResults} -> maxResults) (\s@ListConnectPeers' {} a -> s {maxResults = a} :: ListConnectPeers)

-- | The token for the next page of results.
listConnectPeers_nextToken :: Lens.Lens' ListConnectPeers (Prelude.Maybe Prelude.Text)
listConnectPeers_nextToken = Lens.lens (\ListConnectPeers' {nextToken} -> nextToken) (\s@ListConnectPeers' {} a -> s {nextToken = a} :: ListConnectPeers)

instance Core.AWSPager ListConnectPeers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConnectPeersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConnectPeersResponse_connectPeers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listConnectPeers_nextToken
          Lens..~ rs
          Lens.^? listConnectPeersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListConnectPeers where
  type
    AWSResponse ListConnectPeers =
      ListConnectPeersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectPeersResponse'
            Prelude.<$> (x Data..?> "ConnectPeers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConnectPeers where
  hashWithSalt _salt ListConnectPeers' {..} =
    _salt `Prelude.hashWithSalt` connectAttachmentId
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListConnectPeers where
  rnf ListConnectPeers' {..} =
    Prelude.rnf connectAttachmentId
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListConnectPeers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListConnectPeers where
  toPath = Prelude.const "/connect-peers"

instance Data.ToQuery ListConnectPeers where
  toQuery ListConnectPeers' {..} =
    Prelude.mconcat
      [ "connectAttachmentId" Data.=: connectAttachmentId,
        "coreNetworkId" Data.=: coreNetworkId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListConnectPeersResponse' smart constructor.
data ListConnectPeersResponse = ListConnectPeersResponse'
  { -- | Describes the Connect peers.
    connectPeers :: Prelude.Maybe [ConnectPeerSummary],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectPeersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeers', 'listConnectPeersResponse_connectPeers' - Describes the Connect peers.
--
-- 'nextToken', 'listConnectPeersResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'listConnectPeersResponse_httpStatus' - The response's http status code.
newListConnectPeersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectPeersResponse
newListConnectPeersResponse pHttpStatus_ =
  ListConnectPeersResponse'
    { connectPeers =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the Connect peers.
listConnectPeersResponse_connectPeers :: Lens.Lens' ListConnectPeersResponse (Prelude.Maybe [ConnectPeerSummary])
listConnectPeersResponse_connectPeers = Lens.lens (\ListConnectPeersResponse' {connectPeers} -> connectPeers) (\s@ListConnectPeersResponse' {} a -> s {connectPeers = a} :: ListConnectPeersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
listConnectPeersResponse_nextToken :: Lens.Lens' ListConnectPeersResponse (Prelude.Maybe Prelude.Text)
listConnectPeersResponse_nextToken = Lens.lens (\ListConnectPeersResponse' {nextToken} -> nextToken) (\s@ListConnectPeersResponse' {} a -> s {nextToken = a} :: ListConnectPeersResponse)

-- | The response's http status code.
listConnectPeersResponse_httpStatus :: Lens.Lens' ListConnectPeersResponse Prelude.Int
listConnectPeersResponse_httpStatus = Lens.lens (\ListConnectPeersResponse' {httpStatus} -> httpStatus) (\s@ListConnectPeersResponse' {} a -> s {httpStatus = a} :: ListConnectPeersResponse)

instance Prelude.NFData ListConnectPeersResponse where
  rnf ListConnectPeersResponse' {..} =
    Prelude.rnf connectPeers
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
