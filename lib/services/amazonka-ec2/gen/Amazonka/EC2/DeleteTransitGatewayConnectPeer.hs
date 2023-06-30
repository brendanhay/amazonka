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
-- Module      : Amazonka.EC2.DeleteTransitGatewayConnectPeer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Connect peer.
module Amazonka.EC2.DeleteTransitGatewayConnectPeer
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayConnectPeerResponse'
            Prelude.<$> (x Data..@? "transitGatewayConnectPeer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayConnectPeer
  where
  hashWithSalt
    _salt
    DeleteTransitGatewayConnectPeer' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayConnectPeerId

instance
  Prelude.NFData
    DeleteTransitGatewayConnectPeer
  where
  rnf DeleteTransitGatewayConnectPeer' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayConnectPeerId

instance
  Data.ToHeaders
    DeleteTransitGatewayConnectPeer
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTransitGatewayConnectPeer where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTransitGatewayConnectPeer where
  toQuery DeleteTransitGatewayConnectPeer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteTransitGatewayConnectPeer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TransitGatewayConnectPeerId"
          Data.=: transitGatewayConnectPeerId
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
  where
  rnf DeleteTransitGatewayConnectPeerResponse' {..} =
    Prelude.rnf transitGatewayConnectPeer
      `Prelude.seq` Prelude.rnf httpStatus
