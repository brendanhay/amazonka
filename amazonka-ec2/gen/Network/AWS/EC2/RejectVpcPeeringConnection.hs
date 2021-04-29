{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.RejectVpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a VPC peering connection request. The VPC peering connection
-- must be in the @pending-acceptance@ state. Use the
-- DescribeVpcPeeringConnections request to view your outstanding VPC
-- peering connection requests. To delete an active VPC peering connection,
-- or to delete a VPC peering connection request that you initiated, use
-- DeleteVpcPeeringConnection.
module Network.AWS.EC2.RejectVpcPeeringConnection
  ( -- * Creating a Request
    RejectVpcPeeringConnection (..),
    newRejectVpcPeeringConnection,

    -- * Request Lenses
    rejectVpcPeeringConnection_dryRun,
    rejectVpcPeeringConnection_vpcPeeringConnectionId,

    -- * Destructuring the Response
    RejectVpcPeeringConnectionResponse (..),
    newRejectVpcPeeringConnectionResponse,

    -- * Response Lenses
    rejectVpcPeeringConnectionResponse_return,
    rejectVpcPeeringConnectionResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectVpcPeeringConnection' smart constructor.
data RejectVpcPeeringConnection = RejectVpcPeeringConnection'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RejectVpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'rejectVpcPeeringConnection_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcPeeringConnectionId', 'rejectVpcPeeringConnection_vpcPeeringConnectionId' - The ID of the VPC peering connection.
newRejectVpcPeeringConnection ::
  -- | 'vpcPeeringConnectionId'
  Prelude.Text ->
  RejectVpcPeeringConnection
newRejectVpcPeeringConnection
  pVpcPeeringConnectionId_ =
    RejectVpcPeeringConnection'
      { dryRun =
          Prelude.Nothing,
        vpcPeeringConnectionId =
          pVpcPeeringConnectionId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rejectVpcPeeringConnection_dryRun :: Lens.Lens' RejectVpcPeeringConnection (Prelude.Maybe Prelude.Bool)
rejectVpcPeeringConnection_dryRun = Lens.lens (\RejectVpcPeeringConnection' {dryRun} -> dryRun) (\s@RejectVpcPeeringConnection' {} a -> s {dryRun = a} :: RejectVpcPeeringConnection)

-- | The ID of the VPC peering connection.
rejectVpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' RejectVpcPeeringConnection Prelude.Text
rejectVpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\RejectVpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@RejectVpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: RejectVpcPeeringConnection)

instance
  Prelude.AWSRequest
    RejectVpcPeeringConnection
  where
  type
    Rs RejectVpcPeeringConnection =
      RejectVpcPeeringConnectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RejectVpcPeeringConnectionResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectVpcPeeringConnection

instance Prelude.NFData RejectVpcPeeringConnection

instance Prelude.ToHeaders RejectVpcPeeringConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RejectVpcPeeringConnection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RejectVpcPeeringConnection where
  toQuery RejectVpcPeeringConnection' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RejectVpcPeeringConnection" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "VpcPeeringConnectionId"
          Prelude.=: vpcPeeringConnectionId
      ]

-- | /See:/ 'newRejectVpcPeeringConnectionResponse' smart constructor.
data RejectVpcPeeringConnectionResponse = RejectVpcPeeringConnectionResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RejectVpcPeeringConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'rejectVpcPeeringConnectionResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'rejectVpcPeeringConnectionResponse_httpStatus' - The response's http status code.
newRejectVpcPeeringConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectVpcPeeringConnectionResponse
newRejectVpcPeeringConnectionResponse pHttpStatus_ =
  RejectVpcPeeringConnectionResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
rejectVpcPeeringConnectionResponse_return :: Lens.Lens' RejectVpcPeeringConnectionResponse (Prelude.Maybe Prelude.Bool)
rejectVpcPeeringConnectionResponse_return = Lens.lens (\RejectVpcPeeringConnectionResponse' {return'} -> return') (\s@RejectVpcPeeringConnectionResponse' {} a -> s {return' = a} :: RejectVpcPeeringConnectionResponse)

-- | The response's http status code.
rejectVpcPeeringConnectionResponse_httpStatus :: Lens.Lens' RejectVpcPeeringConnectionResponse Prelude.Int
rejectVpcPeeringConnectionResponse_httpStatus = Lens.lens (\RejectVpcPeeringConnectionResponse' {httpStatus} -> httpStatus) (\s@RejectVpcPeeringConnectionResponse' {} a -> s {httpStatus = a} :: RejectVpcPeeringConnectionResponse)

instance
  Prelude.NFData
    RejectVpcPeeringConnectionResponse
