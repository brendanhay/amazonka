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
-- Module      : Network.AWS.EC2.DeleteVpnGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual private gateway. You must first detach the
-- virtual private gateway from the VPC. Note that you don\'t need to
-- delete the virtual private gateway if you plan to delete and recreate
-- the VPN connection between your VPC and your network.
module Network.AWS.EC2.DeleteVpnGateway
  ( -- * Creating a Request
    DeleteVpnGateway (..),
    newDeleteVpnGateway,

    -- * Request Lenses
    deleteVpnGateway_dryRun,
    deleteVpnGateway_vpnGatewayId,

    -- * Destructuring the Response
    DeleteVpnGatewayResponse (..),
    newDeleteVpnGatewayResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteVpnGateway.
--
-- /See:/ 'newDeleteVpnGateway' smart constructor.
data DeleteVpnGateway = DeleteVpnGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the virtual private gateway.
    vpnGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVpnGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteVpnGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpnGatewayId', 'deleteVpnGateway_vpnGatewayId' - The ID of the virtual private gateway.
newDeleteVpnGateway ::
  -- | 'vpnGatewayId'
  Core.Text ->
  DeleteVpnGateway
newDeleteVpnGateway pVpnGatewayId_ =
  DeleteVpnGateway'
    { dryRun = Core.Nothing,
      vpnGatewayId = pVpnGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVpnGateway_dryRun :: Lens.Lens' DeleteVpnGateway (Core.Maybe Core.Bool)
deleteVpnGateway_dryRun = Lens.lens (\DeleteVpnGateway' {dryRun} -> dryRun) (\s@DeleteVpnGateway' {} a -> s {dryRun = a} :: DeleteVpnGateway)

-- | The ID of the virtual private gateway.
deleteVpnGateway_vpnGatewayId :: Lens.Lens' DeleteVpnGateway Core.Text
deleteVpnGateway_vpnGatewayId = Lens.lens (\DeleteVpnGateway' {vpnGatewayId} -> vpnGatewayId) (\s@DeleteVpnGateway' {} a -> s {vpnGatewayId = a} :: DeleteVpnGateway)

instance Core.AWSRequest DeleteVpnGateway where
  type
    AWSResponse DeleteVpnGateway =
      DeleteVpnGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteVpnGatewayResponse'

instance Core.Hashable DeleteVpnGateway

instance Core.NFData DeleteVpnGateway

instance Core.ToHeaders DeleteVpnGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteVpnGateway where
  toPath = Core.const "/"

instance Core.ToQuery DeleteVpnGateway where
  toQuery DeleteVpnGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteVpnGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "VpnGatewayId" Core.=: vpnGatewayId
      ]

-- | /See:/ 'newDeleteVpnGatewayResponse' smart constructor.
data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVpnGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVpnGatewayResponse ::
  DeleteVpnGatewayResponse
newDeleteVpnGatewayResponse =
  DeleteVpnGatewayResponse'

instance Core.NFData DeleteVpnGatewayResponse
