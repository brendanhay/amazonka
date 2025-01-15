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
-- Module      : Amazonka.NetworkManager.DisassociateConnectPeer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a core network Connect peer from a device and a link.
module Amazonka.NetworkManager.DisassociateConnectPeer
  ( -- * Creating a Request
    DisassociateConnectPeer (..),
    newDisassociateConnectPeer,

    -- * Request Lenses
    disassociateConnectPeer_globalNetworkId,
    disassociateConnectPeer_connectPeerId,

    -- * Destructuring the Response
    DisassociateConnectPeerResponse (..),
    newDisassociateConnectPeerResponse,

    -- * Response Lenses
    disassociateConnectPeerResponse_connectPeerAssociation,
    disassociateConnectPeerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateConnectPeer' smart constructor.
data DisassociateConnectPeer = DisassociateConnectPeer'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the Connect peer to disassociate from a device.
    connectPeerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'disassociateConnectPeer_globalNetworkId' - The ID of the global network.
--
-- 'connectPeerId', 'disassociateConnectPeer_connectPeerId' - The ID of the Connect peer to disassociate from a device.
newDisassociateConnectPeer ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'connectPeerId'
  Prelude.Text ->
  DisassociateConnectPeer
newDisassociateConnectPeer
  pGlobalNetworkId_
  pConnectPeerId_ =
    DisassociateConnectPeer'
      { globalNetworkId =
          pGlobalNetworkId_,
        connectPeerId = pConnectPeerId_
      }

-- | The ID of the global network.
disassociateConnectPeer_globalNetworkId :: Lens.Lens' DisassociateConnectPeer Prelude.Text
disassociateConnectPeer_globalNetworkId = Lens.lens (\DisassociateConnectPeer' {globalNetworkId} -> globalNetworkId) (\s@DisassociateConnectPeer' {} a -> s {globalNetworkId = a} :: DisassociateConnectPeer)

-- | The ID of the Connect peer to disassociate from a device.
disassociateConnectPeer_connectPeerId :: Lens.Lens' DisassociateConnectPeer Prelude.Text
disassociateConnectPeer_connectPeerId = Lens.lens (\DisassociateConnectPeer' {connectPeerId} -> connectPeerId) (\s@DisassociateConnectPeer' {} a -> s {connectPeerId = a} :: DisassociateConnectPeer)

instance Core.AWSRequest DisassociateConnectPeer where
  type
    AWSResponse DisassociateConnectPeer =
      DisassociateConnectPeerResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateConnectPeerResponse'
            Prelude.<$> (x Data..?> "ConnectPeerAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateConnectPeer where
  hashWithSalt _salt DisassociateConnectPeer' {..} =
    _salt
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` connectPeerId

instance Prelude.NFData DisassociateConnectPeer where
  rnf DisassociateConnectPeer' {..} =
    Prelude.rnf globalNetworkId `Prelude.seq`
      Prelude.rnf connectPeerId

instance Data.ToHeaders DisassociateConnectPeer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateConnectPeer where
  toPath DisassociateConnectPeer' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/connect-peer-associations/",
        Data.toBS connectPeerId
      ]

instance Data.ToQuery DisassociateConnectPeer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateConnectPeerResponse' smart constructor.
data DisassociateConnectPeerResponse = DisassociateConnectPeerResponse'
  { -- | Describes the Connect peer association.
    connectPeerAssociation :: Prelude.Maybe ConnectPeerAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateConnectPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeerAssociation', 'disassociateConnectPeerResponse_connectPeerAssociation' - Describes the Connect peer association.
--
-- 'httpStatus', 'disassociateConnectPeerResponse_httpStatus' - The response's http status code.
newDisassociateConnectPeerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateConnectPeerResponse
newDisassociateConnectPeerResponse pHttpStatus_ =
  DisassociateConnectPeerResponse'
    { connectPeerAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the Connect peer association.
disassociateConnectPeerResponse_connectPeerAssociation :: Lens.Lens' DisassociateConnectPeerResponse (Prelude.Maybe ConnectPeerAssociation)
disassociateConnectPeerResponse_connectPeerAssociation = Lens.lens (\DisassociateConnectPeerResponse' {connectPeerAssociation} -> connectPeerAssociation) (\s@DisassociateConnectPeerResponse' {} a -> s {connectPeerAssociation = a} :: DisassociateConnectPeerResponse)

-- | The response's http status code.
disassociateConnectPeerResponse_httpStatus :: Lens.Lens' DisassociateConnectPeerResponse Prelude.Int
disassociateConnectPeerResponse_httpStatus = Lens.lens (\DisassociateConnectPeerResponse' {httpStatus} -> httpStatus) (\s@DisassociateConnectPeerResponse' {} a -> s {httpStatus = a} :: DisassociateConnectPeerResponse)

instance
  Prelude.NFData
    DisassociateConnectPeerResponse
  where
  rnf DisassociateConnectPeerResponse' {..} =
    Prelude.rnf connectPeerAssociation `Prelude.seq`
      Prelude.rnf httpStatus
