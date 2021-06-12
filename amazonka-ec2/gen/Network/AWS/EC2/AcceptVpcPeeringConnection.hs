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
-- Module      : Network.AWS.EC2.AcceptVpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept a VPC peering connection request. To accept a request, the VPC
-- peering connection must be in the @pending-acceptance@ state, and you
-- must be the owner of the peer VPC. Use DescribeVpcPeeringConnections to
-- view your outstanding VPC peering connection requests.
--
-- For an inter-Region VPC peering connection request, you must accept the
-- VPC peering connection in the Region of the accepter VPC.
module Network.AWS.EC2.AcceptVpcPeeringConnection
  ( -- * Creating a Request
    AcceptVpcPeeringConnection (..),
    newAcceptVpcPeeringConnection,

    -- * Request Lenses
    acceptVpcPeeringConnection_vpcPeeringConnectionId,
    acceptVpcPeeringConnection_dryRun,

    -- * Destructuring the Response
    AcceptVpcPeeringConnectionResponse (..),
    newAcceptVpcPeeringConnectionResponse,

    -- * Response Lenses
    acceptVpcPeeringConnectionResponse_vpcPeeringConnection,
    acceptVpcPeeringConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptVpcPeeringConnection' smart constructor.
data AcceptVpcPeeringConnection = AcceptVpcPeeringConnection'
  { -- | The ID of the VPC peering connection. You must specify this parameter in
    -- the request.
    vpcPeeringConnectionId :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptVpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringConnectionId', 'acceptVpcPeeringConnection_vpcPeeringConnectionId' - The ID of the VPC peering connection. You must specify this parameter in
-- the request.
--
-- 'dryRun', 'acceptVpcPeeringConnection_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newAcceptVpcPeeringConnection ::
  AcceptVpcPeeringConnection
newAcceptVpcPeeringConnection =
  AcceptVpcPeeringConnection'
    { vpcPeeringConnectionId =
        Core.Nothing,
      dryRun = Core.Nothing
    }

-- | The ID of the VPC peering connection. You must specify this parameter in
-- the request.
acceptVpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' AcceptVpcPeeringConnection (Core.Maybe Core.Text)
acceptVpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\AcceptVpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@AcceptVpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: AcceptVpcPeeringConnection)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptVpcPeeringConnection_dryRun :: Lens.Lens' AcceptVpcPeeringConnection (Core.Maybe Core.Bool)
acceptVpcPeeringConnection_dryRun = Lens.lens (\AcceptVpcPeeringConnection' {dryRun} -> dryRun) (\s@AcceptVpcPeeringConnection' {} a -> s {dryRun = a} :: AcceptVpcPeeringConnection)

instance Core.AWSRequest AcceptVpcPeeringConnection where
  type
    AWSResponse AcceptVpcPeeringConnection =
      AcceptVpcPeeringConnectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptVpcPeeringConnectionResponse'
            Core.<$> (x Core..@? "vpcPeeringConnection")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AcceptVpcPeeringConnection

instance Core.NFData AcceptVpcPeeringConnection

instance Core.ToHeaders AcceptVpcPeeringConnection where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AcceptVpcPeeringConnection where
  toPath = Core.const "/"

instance Core.ToQuery AcceptVpcPeeringConnection where
  toQuery AcceptVpcPeeringConnection' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AcceptVpcPeeringConnection" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "VpcPeeringConnectionId"
          Core.=: vpcPeeringConnectionId,
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newAcceptVpcPeeringConnectionResponse' smart constructor.
data AcceptVpcPeeringConnectionResponse = AcceptVpcPeeringConnectionResponse'
  { -- | Information about the VPC peering connection.
    vpcPeeringConnection :: Core.Maybe VpcPeeringConnection,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptVpcPeeringConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringConnection', 'acceptVpcPeeringConnectionResponse_vpcPeeringConnection' - Information about the VPC peering connection.
--
-- 'httpStatus', 'acceptVpcPeeringConnectionResponse_httpStatus' - The response's http status code.
newAcceptVpcPeeringConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptVpcPeeringConnectionResponse
newAcceptVpcPeeringConnectionResponse pHttpStatus_ =
  AcceptVpcPeeringConnectionResponse'
    { vpcPeeringConnection =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the VPC peering connection.
acceptVpcPeeringConnectionResponse_vpcPeeringConnection :: Lens.Lens' AcceptVpcPeeringConnectionResponse (Core.Maybe VpcPeeringConnection)
acceptVpcPeeringConnectionResponse_vpcPeeringConnection = Lens.lens (\AcceptVpcPeeringConnectionResponse' {vpcPeeringConnection} -> vpcPeeringConnection) (\s@AcceptVpcPeeringConnectionResponse' {} a -> s {vpcPeeringConnection = a} :: AcceptVpcPeeringConnectionResponse)

-- | The response's http status code.
acceptVpcPeeringConnectionResponse_httpStatus :: Lens.Lens' AcceptVpcPeeringConnectionResponse Core.Int
acceptVpcPeeringConnectionResponse_httpStatus = Lens.lens (\AcceptVpcPeeringConnectionResponse' {httpStatus} -> httpStatus) (\s@AcceptVpcPeeringConnectionResponse' {} a -> s {httpStatus = a} :: AcceptVpcPeeringConnectionResponse)

instance
  Core.NFData
    AcceptVpcPeeringConnectionResponse
