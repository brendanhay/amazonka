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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptVpcPeeringConnection' smart constructor.
data AcceptVpcPeeringConnection = AcceptVpcPeeringConnection'
  { -- | The ID of the VPC peering connection. You must specify this parameter in
    -- the request.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | The ID of the VPC peering connection. You must specify this parameter in
-- the request.
acceptVpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' AcceptVpcPeeringConnection (Prelude.Maybe Prelude.Text)
acceptVpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\AcceptVpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@AcceptVpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: AcceptVpcPeeringConnection)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptVpcPeeringConnection_dryRun :: Lens.Lens' AcceptVpcPeeringConnection (Prelude.Maybe Prelude.Bool)
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
            Prelude.<$> (x Core..@? "vpcPeeringConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptVpcPeeringConnection

instance Prelude.NFData AcceptVpcPeeringConnection

instance Core.ToHeaders AcceptVpcPeeringConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AcceptVpcPeeringConnection where
  toPath = Prelude.const "/"

instance Core.ToQuery AcceptVpcPeeringConnection where
  toQuery AcceptVpcPeeringConnection' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AcceptVpcPeeringConnection" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "VpcPeeringConnectionId"
          Core.=: vpcPeeringConnectionId,
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newAcceptVpcPeeringConnectionResponse' smart constructor.
data AcceptVpcPeeringConnectionResponse = AcceptVpcPeeringConnectionResponse'
  { -- | Information about the VPC peering connection.
    vpcPeeringConnection :: Prelude.Maybe VpcPeeringConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AcceptVpcPeeringConnectionResponse
newAcceptVpcPeeringConnectionResponse pHttpStatus_ =
  AcceptVpcPeeringConnectionResponse'
    { vpcPeeringConnection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the VPC peering connection.
acceptVpcPeeringConnectionResponse_vpcPeeringConnection :: Lens.Lens' AcceptVpcPeeringConnectionResponse (Prelude.Maybe VpcPeeringConnection)
acceptVpcPeeringConnectionResponse_vpcPeeringConnection = Lens.lens (\AcceptVpcPeeringConnectionResponse' {vpcPeeringConnection} -> vpcPeeringConnection) (\s@AcceptVpcPeeringConnectionResponse' {} a -> s {vpcPeeringConnection = a} :: AcceptVpcPeeringConnectionResponse)

-- | The response's http status code.
acceptVpcPeeringConnectionResponse_httpStatus :: Lens.Lens' AcceptVpcPeeringConnectionResponse Prelude.Int
acceptVpcPeeringConnectionResponse_httpStatus = Lens.lens (\AcceptVpcPeeringConnectionResponse' {httpStatus} -> httpStatus) (\s@AcceptVpcPeeringConnectionResponse' {} a -> s {httpStatus = a} :: AcceptVpcPeeringConnectionResponse)

instance
  Prelude.NFData
    AcceptVpcPeeringConnectionResponse
