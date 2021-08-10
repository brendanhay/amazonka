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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayConnectPeer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Connect peer.
module Network.AWS.EC2.DeleteTransitGatewayConnectPeer
  ( -- * Creating a Request
    DeleteTransitGatewayConnectPeer (..),
    newDeleteTransitGatewayConnectPeer,

    -- * Request Lenses
    deleteTransitGatewayConnectPeer_dryRun,
    deleteTransitGatewayConnectPeer_transitGatewayConnectPeerId,

    -- * Destructuring the Response
    DeleteTransitGatewayConnectPeerResponse (..),
    newDeleteTransitGatewayConnectPeerResponse,

    -- * Response Lenses
    deleteTransitGatewayConnectPeerResponse_transitGatewayConnectPeer,
    deleteTransitGatewayConnectPeerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTransitGatewayConnectPeer' smart constructor.
data DeleteTransitGatewayConnectPeer = DeleteTransitGatewayConnectPeer'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Connect peer.
    transitGatewayConnectPeerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayConnectPeer_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayConnectPeerId', 'deleteTransitGatewayConnectPeer_transitGatewayConnectPeerId' - The ID of the Connect peer.
newDeleteTransitGatewayConnectPeer ::
  -- | 'transitGatewayConnectPeerId'
  Prelude.Text ->
  DeleteTransitGatewayConnectPeer
newDeleteTransitGatewayConnectPeer
  pTransitGatewayConnectPeerId_ =
    DeleteTransitGatewayConnectPeer'
      { dryRun =
          Prelude.Nothing,
        transitGatewayConnectPeerId =
          pTransitGatewayConnectPeerId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayConnectPeer_dryRun :: Lens.Lens' DeleteTransitGatewayConnectPeer (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayConnectPeer_dryRun = Lens.lens (\DeleteTransitGatewayConnectPeer' {dryRun} -> dryRun) (\s@DeleteTransitGatewayConnectPeer' {} a -> s {dryRun = a} :: DeleteTransitGatewayConnectPeer)

-- | The ID of the Connect peer.
deleteTransitGatewayConnectPeer_transitGatewayConnectPeerId :: Lens.Lens' DeleteTransitGatewayConnectPeer Prelude.Text
deleteTransitGatewayConnectPeer_transitGatewayConnectPeerId = Lens.lens (\DeleteTransitGatewayConnectPeer' {transitGatewayConnectPeerId} -> transitGatewayConnectPeerId) (\s@DeleteTransitGatewayConnectPeer' {} a -> s {transitGatewayConnectPeerId = a} :: DeleteTransitGatewayConnectPeer)

instance
  Core.AWSRequest
    DeleteTransitGatewayConnectPeer
  where
  type
    AWSResponse DeleteTransitGatewayConnectPeer =
      DeleteTransitGatewayConnectPeerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayConnectPeerResponse'
            Prelude.<$> (x Core..@? "transitGatewayConnectPeer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayConnectPeer

instance
  Prelude.NFData
    DeleteTransitGatewayConnectPeer

instance
  Core.ToHeaders
    DeleteTransitGatewayConnectPeer
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteTransitGatewayConnectPeer where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteTransitGatewayConnectPeer where
  toQuery DeleteTransitGatewayConnectPeer' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteTransitGatewayConnectPeer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayConnectPeerId"
          Core.=: transitGatewayConnectPeerId
      ]

-- | /See:/ 'newDeleteTransitGatewayConnectPeerResponse' smart constructor.
data DeleteTransitGatewayConnectPeerResponse = DeleteTransitGatewayConnectPeerResponse'
  { -- | Information about the deleted Connect peer.
    transitGatewayConnectPeer :: Prelude.Maybe TransitGatewayConnectPeer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayConnectPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayConnectPeer', 'deleteTransitGatewayConnectPeerResponse_transitGatewayConnectPeer' - Information about the deleted Connect peer.
--
-- 'httpStatus', 'deleteTransitGatewayConnectPeerResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayConnectPeerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayConnectPeerResponse
newDeleteTransitGatewayConnectPeerResponse
  pHttpStatus_ =
    DeleteTransitGatewayConnectPeerResponse'
      { transitGatewayConnectPeer =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted Connect peer.
deleteTransitGatewayConnectPeerResponse_transitGatewayConnectPeer :: Lens.Lens' DeleteTransitGatewayConnectPeerResponse (Prelude.Maybe TransitGatewayConnectPeer)
deleteTransitGatewayConnectPeerResponse_transitGatewayConnectPeer = Lens.lens (\DeleteTransitGatewayConnectPeerResponse' {transitGatewayConnectPeer} -> transitGatewayConnectPeer) (\s@DeleteTransitGatewayConnectPeerResponse' {} a -> s {transitGatewayConnectPeer = a} :: DeleteTransitGatewayConnectPeerResponse)

-- | The response's http status code.
deleteTransitGatewayConnectPeerResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayConnectPeerResponse Prelude.Int
deleteTransitGatewayConnectPeerResponse_httpStatus = Lens.lens (\DeleteTransitGatewayConnectPeerResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayConnectPeerResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayConnectPeerResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayConnectPeerResponse
