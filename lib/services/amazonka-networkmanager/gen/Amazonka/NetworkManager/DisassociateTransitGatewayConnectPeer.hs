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
-- Module      : Amazonka.NetworkManager.DisassociateTransitGatewayConnectPeer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a transit gateway Connect peer from a device and link.
module Amazonka.NetworkManager.DisassociateTransitGatewayConnectPeer
  ( -- * Creating a Request
    DisassociateTransitGatewayConnectPeer (..),
    newDisassociateTransitGatewayConnectPeer,

    -- * Request Lenses
    disassociateTransitGatewayConnectPeer_globalNetworkId,
    disassociateTransitGatewayConnectPeer_transitGatewayConnectPeerArn,

    -- * Destructuring the Response
    DisassociateTransitGatewayConnectPeerResponse (..),
    newDisassociateTransitGatewayConnectPeerResponse,

    -- * Response Lenses
    disassociateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation,
    disassociateTransitGatewayConnectPeerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateTransitGatewayConnectPeer' smart constructor.
data DisassociateTransitGatewayConnectPeer = DisassociateTransitGatewayConnectPeer'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the transit gateway Connect peer.
    transitGatewayConnectPeerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTransitGatewayConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'disassociateTransitGatewayConnectPeer_globalNetworkId' - The ID of the global network.
--
-- 'transitGatewayConnectPeerArn', 'disassociateTransitGatewayConnectPeer_transitGatewayConnectPeerArn' - The Amazon Resource Name (ARN) of the transit gateway Connect peer.
newDisassociateTransitGatewayConnectPeer ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'transitGatewayConnectPeerArn'
  Prelude.Text ->
  DisassociateTransitGatewayConnectPeer
newDisassociateTransitGatewayConnectPeer
  pGlobalNetworkId_
  pTransitGatewayConnectPeerArn_ =
    DisassociateTransitGatewayConnectPeer'
      { globalNetworkId =
          pGlobalNetworkId_,
        transitGatewayConnectPeerArn =
          pTransitGatewayConnectPeerArn_
      }

-- | The ID of the global network.
disassociateTransitGatewayConnectPeer_globalNetworkId :: Lens.Lens' DisassociateTransitGatewayConnectPeer Prelude.Text
disassociateTransitGatewayConnectPeer_globalNetworkId = Lens.lens (\DisassociateTransitGatewayConnectPeer' {globalNetworkId} -> globalNetworkId) (\s@DisassociateTransitGatewayConnectPeer' {} a -> s {globalNetworkId = a} :: DisassociateTransitGatewayConnectPeer)

-- | The Amazon Resource Name (ARN) of the transit gateway Connect peer.
disassociateTransitGatewayConnectPeer_transitGatewayConnectPeerArn :: Lens.Lens' DisassociateTransitGatewayConnectPeer Prelude.Text
disassociateTransitGatewayConnectPeer_transitGatewayConnectPeerArn = Lens.lens (\DisassociateTransitGatewayConnectPeer' {transitGatewayConnectPeerArn} -> transitGatewayConnectPeerArn) (\s@DisassociateTransitGatewayConnectPeer' {} a -> s {transitGatewayConnectPeerArn = a} :: DisassociateTransitGatewayConnectPeer)

instance
  Core.AWSRequest
    DisassociateTransitGatewayConnectPeer
  where
  type
    AWSResponse
      DisassociateTransitGatewayConnectPeer =
      DisassociateTransitGatewayConnectPeerResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateTransitGatewayConnectPeerResponse'
            Prelude.<$> (x Data..?> "TransitGatewayConnectPeerAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateTransitGatewayConnectPeer
  where
  hashWithSalt
    _salt
    DisassociateTransitGatewayConnectPeer' {..} =
      _salt
        `Prelude.hashWithSalt` globalNetworkId
        `Prelude.hashWithSalt` transitGatewayConnectPeerArn

instance
  Prelude.NFData
    DisassociateTransitGatewayConnectPeer
  where
  rnf DisassociateTransitGatewayConnectPeer' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf transitGatewayConnectPeerArn

instance
  Data.ToHeaders
    DisassociateTransitGatewayConnectPeer
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
  Data.ToPath
    DisassociateTransitGatewayConnectPeer
  where
  toPath DisassociateTransitGatewayConnectPeer' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/transit-gateway-connect-peer-associations/",
        Data.toBS transitGatewayConnectPeerArn
      ]

instance
  Data.ToQuery
    DisassociateTransitGatewayConnectPeer
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateTransitGatewayConnectPeerResponse' smart constructor.
data DisassociateTransitGatewayConnectPeerResponse = DisassociateTransitGatewayConnectPeerResponse'
  { -- | The transit gateway Connect peer association.
    transitGatewayConnectPeerAssociation :: Prelude.Maybe TransitGatewayConnectPeerAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTransitGatewayConnectPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayConnectPeerAssociation', 'disassociateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation' - The transit gateway Connect peer association.
--
-- 'httpStatus', 'disassociateTransitGatewayConnectPeerResponse_httpStatus' - The response's http status code.
newDisassociateTransitGatewayConnectPeerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateTransitGatewayConnectPeerResponse
newDisassociateTransitGatewayConnectPeerResponse
  pHttpStatus_ =
    DisassociateTransitGatewayConnectPeerResponse'
      { transitGatewayConnectPeerAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The transit gateway Connect peer association.
disassociateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation :: Lens.Lens' DisassociateTransitGatewayConnectPeerResponse (Prelude.Maybe TransitGatewayConnectPeerAssociation)
disassociateTransitGatewayConnectPeerResponse_transitGatewayConnectPeerAssociation = Lens.lens (\DisassociateTransitGatewayConnectPeerResponse' {transitGatewayConnectPeerAssociation} -> transitGatewayConnectPeerAssociation) (\s@DisassociateTransitGatewayConnectPeerResponse' {} a -> s {transitGatewayConnectPeerAssociation = a} :: DisassociateTransitGatewayConnectPeerResponse)

-- | The response's http status code.
disassociateTransitGatewayConnectPeerResponse_httpStatus :: Lens.Lens' DisassociateTransitGatewayConnectPeerResponse Prelude.Int
disassociateTransitGatewayConnectPeerResponse_httpStatus = Lens.lens (\DisassociateTransitGatewayConnectPeerResponse' {httpStatus} -> httpStatus) (\s@DisassociateTransitGatewayConnectPeerResponse' {} a -> s {httpStatus = a} :: DisassociateTransitGatewayConnectPeerResponse)

instance
  Prelude.NFData
    DisassociateTransitGatewayConnectPeerResponse
  where
  rnf
    DisassociateTransitGatewayConnectPeerResponse' {..} =
      Prelude.rnf transitGatewayConnectPeerAssociation
        `Prelude.seq` Prelude.rnf httpStatus
