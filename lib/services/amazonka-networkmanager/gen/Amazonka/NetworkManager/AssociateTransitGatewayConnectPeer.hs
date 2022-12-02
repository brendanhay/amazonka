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
-- Module      : Amazonka.NetworkManager.AssociateTransitGatewayConnectPeer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a transit gateway Connect peer with a device, and optionally,
-- with a link. If you specify a link, it must be associated with the
-- specified device.
--
-- You can only associate transit gateway Connect peers that have been
-- created on a transit gateway that\'s registered in your global network.
--
-- You cannot associate a transit gateway Connect peer with more than one
-- device and link.
module Amazonka.NetworkManager.AssociateTransitGatewayConnectPeer
  ( -- * Creating a Request
    AssociateTransitGatewayConnectPeer (..),
    newAssociateTransitGatewayConnectPeer,

    -- * Request Lenses
    associateTransitGatewayConnectPeer_linkId,
    associateTransitGatewayConnectPeer_globalNetworkId,
    associateTransitGatewayConnectPeer_transitGatewayConnectPeerArn,
    associateTransitGatewayConnectPeer_deviceId,

    -- * Destructuring the Response
    AssociateTransitGatewayConnectPeerResponse (..),
    newAssociateTransitGatewayConnectPeerResponse,

    -- * Response Lenses
    associateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation,
    associateTransitGatewayConnectPeerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateTransitGatewayConnectPeer' smart constructor.
data AssociateTransitGatewayConnectPeer = AssociateTransitGatewayConnectPeer'
  { -- | The ID of the link.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Connect peer.
    transitGatewayConnectPeerArn :: Prelude.Text,
    -- | The ID of the device.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTransitGatewayConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkId', 'associateTransitGatewayConnectPeer_linkId' - The ID of the link.
--
-- 'globalNetworkId', 'associateTransitGatewayConnectPeer_globalNetworkId' - The ID of the global network.
--
-- 'transitGatewayConnectPeerArn', 'associateTransitGatewayConnectPeer_transitGatewayConnectPeerArn' - The Amazon Resource Name (ARN) of the Connect peer.
--
-- 'deviceId', 'associateTransitGatewayConnectPeer_deviceId' - The ID of the device.
newAssociateTransitGatewayConnectPeer ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'transitGatewayConnectPeerArn'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  AssociateTransitGatewayConnectPeer
newAssociateTransitGatewayConnectPeer
  pGlobalNetworkId_
  pTransitGatewayConnectPeerArn_
  pDeviceId_ =
    AssociateTransitGatewayConnectPeer'
      { linkId =
          Prelude.Nothing,
        globalNetworkId = pGlobalNetworkId_,
        transitGatewayConnectPeerArn =
          pTransitGatewayConnectPeerArn_,
        deviceId = pDeviceId_
      }

-- | The ID of the link.
associateTransitGatewayConnectPeer_linkId :: Lens.Lens' AssociateTransitGatewayConnectPeer (Prelude.Maybe Prelude.Text)
associateTransitGatewayConnectPeer_linkId = Lens.lens (\AssociateTransitGatewayConnectPeer' {linkId} -> linkId) (\s@AssociateTransitGatewayConnectPeer' {} a -> s {linkId = a} :: AssociateTransitGatewayConnectPeer)

-- | The ID of the global network.
associateTransitGatewayConnectPeer_globalNetworkId :: Lens.Lens' AssociateTransitGatewayConnectPeer Prelude.Text
associateTransitGatewayConnectPeer_globalNetworkId = Lens.lens (\AssociateTransitGatewayConnectPeer' {globalNetworkId} -> globalNetworkId) (\s@AssociateTransitGatewayConnectPeer' {} a -> s {globalNetworkId = a} :: AssociateTransitGatewayConnectPeer)

-- | The Amazon Resource Name (ARN) of the Connect peer.
associateTransitGatewayConnectPeer_transitGatewayConnectPeerArn :: Lens.Lens' AssociateTransitGatewayConnectPeer Prelude.Text
associateTransitGatewayConnectPeer_transitGatewayConnectPeerArn = Lens.lens (\AssociateTransitGatewayConnectPeer' {transitGatewayConnectPeerArn} -> transitGatewayConnectPeerArn) (\s@AssociateTransitGatewayConnectPeer' {} a -> s {transitGatewayConnectPeerArn = a} :: AssociateTransitGatewayConnectPeer)

-- | The ID of the device.
associateTransitGatewayConnectPeer_deviceId :: Lens.Lens' AssociateTransitGatewayConnectPeer Prelude.Text
associateTransitGatewayConnectPeer_deviceId = Lens.lens (\AssociateTransitGatewayConnectPeer' {deviceId} -> deviceId) (\s@AssociateTransitGatewayConnectPeer' {} a -> s {deviceId = a} :: AssociateTransitGatewayConnectPeer)

instance
  Core.AWSRequest
    AssociateTransitGatewayConnectPeer
  where
  type
    AWSResponse AssociateTransitGatewayConnectPeer =
      AssociateTransitGatewayConnectPeerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTransitGatewayConnectPeerResponse'
            Prelude.<$> (x Data..?> "TransitGatewayConnectPeerAssociation")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateTransitGatewayConnectPeer
  where
  hashWithSalt
    _salt
    AssociateTransitGatewayConnectPeer' {..} =
      _salt `Prelude.hashWithSalt` linkId
        `Prelude.hashWithSalt` globalNetworkId
        `Prelude.hashWithSalt` transitGatewayConnectPeerArn
        `Prelude.hashWithSalt` deviceId

instance
  Prelude.NFData
    AssociateTransitGatewayConnectPeer
  where
  rnf AssociateTransitGatewayConnectPeer' {..} =
    Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf transitGatewayConnectPeerArn
      `Prelude.seq` Prelude.rnf deviceId

instance
  Data.ToHeaders
    AssociateTransitGatewayConnectPeer
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    AssociateTransitGatewayConnectPeer
  where
  toJSON AssociateTransitGatewayConnectPeer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LinkId" Data..=) Prelude.<$> linkId,
            Prelude.Just
              ( "TransitGatewayConnectPeerArn"
                  Data..= transitGatewayConnectPeerArn
              ),
            Prelude.Just ("DeviceId" Data..= deviceId)
          ]
      )

instance
  Data.ToPath
    AssociateTransitGatewayConnectPeer
  where
  toPath AssociateTransitGatewayConnectPeer' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/transit-gateway-connect-peer-associations"
      ]

instance
  Data.ToQuery
    AssociateTransitGatewayConnectPeer
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateTransitGatewayConnectPeerResponse' smart constructor.
data AssociateTransitGatewayConnectPeerResponse = AssociateTransitGatewayConnectPeerResponse'
  { -- | The transit gateway Connect peer association.
    transitGatewayConnectPeerAssociation :: Prelude.Maybe TransitGatewayConnectPeerAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTransitGatewayConnectPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayConnectPeerAssociation', 'associateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation' - The transit gateway Connect peer association.
--
-- 'httpStatus', 'associateTransitGatewayConnectPeerResponse_httpStatus' - The response's http status code.
newAssociateTransitGatewayConnectPeerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateTransitGatewayConnectPeerResponse
newAssociateTransitGatewayConnectPeerResponse
  pHttpStatus_ =
    AssociateTransitGatewayConnectPeerResponse'
      { transitGatewayConnectPeerAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The transit gateway Connect peer association.
associateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation :: Lens.Lens' AssociateTransitGatewayConnectPeerResponse (Prelude.Maybe TransitGatewayConnectPeerAssociation)
associateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation = Lens.lens (\AssociateTransitGatewayConnectPeerResponse' {transitGatewayConnectPeerAssociation} -> transitGatewayConnectPeerAssociation) (\s@AssociateTransitGatewayConnectPeerResponse' {} a -> s {transitGatewayConnectPeerAssociation = a} :: AssociateTransitGatewayConnectPeerResponse)

-- | The response's http status code.
associateTransitGatewayConnectPeerResponse_httpStatus :: Lens.Lens' AssociateTransitGatewayConnectPeerResponse Prelude.Int
associateTransitGatewayConnectPeerResponse_httpStatus = Lens.lens (\AssociateTransitGatewayConnectPeerResponse' {httpStatus} -> httpStatus) (\s@AssociateTransitGatewayConnectPeerResponse' {} a -> s {httpStatus = a} :: AssociateTransitGatewayConnectPeerResponse)

instance
  Prelude.NFData
    AssociateTransitGatewayConnectPeerResponse
  where
  rnf AssociateTransitGatewayConnectPeerResponse' {..} =
    Prelude.rnf transitGatewayConnectPeerAssociation
      `Prelude.seq` Prelude.rnf httpStatus
