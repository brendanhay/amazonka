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
-- Module      : Amazonka.NetworkManager.AssociateConnectPeer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a core network Connect peer with a device and optionally,
-- with a link.
--
-- If you specify a link, it must be associated with the specified device.
-- You can only associate core network Connect peers that have been created
-- on a core network Connect attachment on a core network.
module Amazonka.NetworkManager.AssociateConnectPeer
  ( -- * Creating a Request
    AssociateConnectPeer (..),
    newAssociateConnectPeer,

    -- * Request Lenses
    associateConnectPeer_linkId,
    associateConnectPeer_globalNetworkId,
    associateConnectPeer_connectPeerId,
    associateConnectPeer_deviceId,

    -- * Destructuring the Response
    AssociateConnectPeerResponse (..),
    newAssociateConnectPeerResponse,

    -- * Response Lenses
    associateConnectPeerResponse_connectPeerAssociation,
    associateConnectPeerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateConnectPeer' smart constructor.
data AssociateConnectPeer = AssociateConnectPeer'
  { -- | The ID of the link.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of your global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the Connect peer.
    connectPeerId :: Prelude.Text,
    -- | The ID of the device.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkId', 'associateConnectPeer_linkId' - The ID of the link.
--
-- 'globalNetworkId', 'associateConnectPeer_globalNetworkId' - The ID of your global network.
--
-- 'connectPeerId', 'associateConnectPeer_connectPeerId' - The ID of the Connect peer.
--
-- 'deviceId', 'associateConnectPeer_deviceId' - The ID of the device.
newAssociateConnectPeer ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'connectPeerId'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  AssociateConnectPeer
newAssociateConnectPeer
  pGlobalNetworkId_
  pConnectPeerId_
  pDeviceId_ =
    AssociateConnectPeer'
      { linkId = Prelude.Nothing,
        globalNetworkId = pGlobalNetworkId_,
        connectPeerId = pConnectPeerId_,
        deviceId = pDeviceId_
      }

-- | The ID of the link.
associateConnectPeer_linkId :: Lens.Lens' AssociateConnectPeer (Prelude.Maybe Prelude.Text)
associateConnectPeer_linkId = Lens.lens (\AssociateConnectPeer' {linkId} -> linkId) (\s@AssociateConnectPeer' {} a -> s {linkId = a} :: AssociateConnectPeer)

-- | The ID of your global network.
associateConnectPeer_globalNetworkId :: Lens.Lens' AssociateConnectPeer Prelude.Text
associateConnectPeer_globalNetworkId = Lens.lens (\AssociateConnectPeer' {globalNetworkId} -> globalNetworkId) (\s@AssociateConnectPeer' {} a -> s {globalNetworkId = a} :: AssociateConnectPeer)

-- | The ID of the Connect peer.
associateConnectPeer_connectPeerId :: Lens.Lens' AssociateConnectPeer Prelude.Text
associateConnectPeer_connectPeerId = Lens.lens (\AssociateConnectPeer' {connectPeerId} -> connectPeerId) (\s@AssociateConnectPeer' {} a -> s {connectPeerId = a} :: AssociateConnectPeer)

-- | The ID of the device.
associateConnectPeer_deviceId :: Lens.Lens' AssociateConnectPeer Prelude.Text
associateConnectPeer_deviceId = Lens.lens (\AssociateConnectPeer' {deviceId} -> deviceId) (\s@AssociateConnectPeer' {} a -> s {deviceId = a} :: AssociateConnectPeer)

instance Core.AWSRequest AssociateConnectPeer where
  type
    AWSResponse AssociateConnectPeer =
      AssociateConnectPeerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateConnectPeerResponse'
            Prelude.<$> (x Data..?> "ConnectPeerAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateConnectPeer where
  hashWithSalt _salt AssociateConnectPeer' {..} =
    _salt
      `Prelude.hashWithSalt` linkId
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` connectPeerId
      `Prelude.hashWithSalt` deviceId

instance Prelude.NFData AssociateConnectPeer where
  rnf AssociateConnectPeer' {..} =
    Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf connectPeerId
      `Prelude.seq` Prelude.rnf deviceId

instance Data.ToHeaders AssociateConnectPeer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateConnectPeer where
  toJSON AssociateConnectPeer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LinkId" Data..=) Prelude.<$> linkId,
            Prelude.Just ("ConnectPeerId" Data..= connectPeerId),
            Prelude.Just ("DeviceId" Data..= deviceId)
          ]
      )

instance Data.ToPath AssociateConnectPeer where
  toPath AssociateConnectPeer' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/connect-peer-associations"
      ]

instance Data.ToQuery AssociateConnectPeer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateConnectPeerResponse' smart constructor.
data AssociateConnectPeerResponse = AssociateConnectPeerResponse'
  { -- | The response to the Connect peer request.
    connectPeerAssociation :: Prelude.Maybe ConnectPeerAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateConnectPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeerAssociation', 'associateConnectPeerResponse_connectPeerAssociation' - The response to the Connect peer request.
--
-- 'httpStatus', 'associateConnectPeerResponse_httpStatus' - The response's http status code.
newAssociateConnectPeerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateConnectPeerResponse
newAssociateConnectPeerResponse pHttpStatus_ =
  AssociateConnectPeerResponse'
    { connectPeerAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response to the Connect peer request.
associateConnectPeerResponse_connectPeerAssociation :: Lens.Lens' AssociateConnectPeerResponse (Prelude.Maybe ConnectPeerAssociation)
associateConnectPeerResponse_connectPeerAssociation = Lens.lens (\AssociateConnectPeerResponse' {connectPeerAssociation} -> connectPeerAssociation) (\s@AssociateConnectPeerResponse' {} a -> s {connectPeerAssociation = a} :: AssociateConnectPeerResponse)

-- | The response's http status code.
associateConnectPeerResponse_httpStatus :: Lens.Lens' AssociateConnectPeerResponse Prelude.Int
associateConnectPeerResponse_httpStatus = Lens.lens (\AssociateConnectPeerResponse' {httpStatus} -> httpStatus) (\s@AssociateConnectPeerResponse' {} a -> s {httpStatus = a} :: AssociateConnectPeerResponse)

instance Prelude.NFData AssociateConnectPeerResponse where
  rnf AssociateConnectPeerResponse' {..} =
    Prelude.rnf connectPeerAssociation
      `Prelude.seq` Prelude.rnf httpStatus
