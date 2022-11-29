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
-- Module      : Amazonka.NetworkManager.GetConnectPeer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a core network Connect peer.
module Amazonka.NetworkManager.GetConnectPeer
  ( -- * Creating a Request
    GetConnectPeer (..),
    newGetConnectPeer,

    -- * Request Lenses
    getConnectPeer_connectPeerId,

    -- * Destructuring the Response
    GetConnectPeerResponse (..),
    newGetConnectPeerResponse,

    -- * Response Lenses
    getConnectPeerResponse_connectPeer,
    getConnectPeerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnectPeer' smart constructor.
data GetConnectPeer = GetConnectPeer'
  { -- | The ID of the Connect peer.
    connectPeerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeerId', 'getConnectPeer_connectPeerId' - The ID of the Connect peer.
newGetConnectPeer ::
  -- | 'connectPeerId'
  Prelude.Text ->
  GetConnectPeer
newGetConnectPeer pConnectPeerId_ =
  GetConnectPeer' {connectPeerId = pConnectPeerId_}

-- | The ID of the Connect peer.
getConnectPeer_connectPeerId :: Lens.Lens' GetConnectPeer Prelude.Text
getConnectPeer_connectPeerId = Lens.lens (\GetConnectPeer' {connectPeerId} -> connectPeerId) (\s@GetConnectPeer' {} a -> s {connectPeerId = a} :: GetConnectPeer)

instance Core.AWSRequest GetConnectPeer where
  type
    AWSResponse GetConnectPeer =
      GetConnectPeerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectPeerResponse'
            Prelude.<$> (x Core..?> "ConnectPeer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectPeer where
  hashWithSalt _salt GetConnectPeer' {..} =
    _salt `Prelude.hashWithSalt` connectPeerId

instance Prelude.NFData GetConnectPeer where
  rnf GetConnectPeer' {..} = Prelude.rnf connectPeerId

instance Core.ToHeaders GetConnectPeer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetConnectPeer where
  toPath GetConnectPeer' {..} =
    Prelude.mconcat
      ["/connect-peers/", Core.toBS connectPeerId]

instance Core.ToQuery GetConnectPeer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectPeerResponse' smart constructor.
data GetConnectPeerResponse = GetConnectPeerResponse'
  { -- | Returns information about a core network Connect peer.
    connectPeer :: Prelude.Maybe ConnectPeer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeer', 'getConnectPeerResponse_connectPeer' - Returns information about a core network Connect peer.
--
-- 'httpStatus', 'getConnectPeerResponse_httpStatus' - The response's http status code.
newGetConnectPeerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectPeerResponse
newGetConnectPeerResponse pHttpStatus_ =
  GetConnectPeerResponse'
    { connectPeer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns information about a core network Connect peer.
getConnectPeerResponse_connectPeer :: Lens.Lens' GetConnectPeerResponse (Prelude.Maybe ConnectPeer)
getConnectPeerResponse_connectPeer = Lens.lens (\GetConnectPeerResponse' {connectPeer} -> connectPeer) (\s@GetConnectPeerResponse' {} a -> s {connectPeer = a} :: GetConnectPeerResponse)

-- | The response's http status code.
getConnectPeerResponse_httpStatus :: Lens.Lens' GetConnectPeerResponse Prelude.Int
getConnectPeerResponse_httpStatus = Lens.lens (\GetConnectPeerResponse' {httpStatus} -> httpStatus) (\s@GetConnectPeerResponse' {} a -> s {httpStatus = a} :: GetConnectPeerResponse)

instance Prelude.NFData GetConnectPeerResponse where
  rnf GetConnectPeerResponse' {..} =
    Prelude.rnf connectPeer
      `Prelude.seq` Prelude.rnf httpStatus
