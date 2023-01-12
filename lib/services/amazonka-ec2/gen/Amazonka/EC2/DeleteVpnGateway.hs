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
-- Module      : Amazonka.EC2.DeleteVpnGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual private gateway. You must first detach the
-- virtual private gateway from the VPC. Note that you don\'t need to
-- delete the virtual private gateway if you plan to delete and recreate
-- the VPN connection between your VPC and your network.
module Amazonka.EC2.DeleteVpnGateway
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DeleteVpnGateway.
--
-- /See:/ 'newDeleteVpnGateway' smart constructor.
data DeleteVpnGateway = DeleteVpnGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the virtual private gateway.
    vpnGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteVpnGateway
newDeleteVpnGateway pVpnGatewayId_ =
  DeleteVpnGateway'
    { dryRun = Prelude.Nothing,
      vpnGatewayId = pVpnGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVpnGateway_dryRun :: Lens.Lens' DeleteVpnGateway (Prelude.Maybe Prelude.Bool)
deleteVpnGateway_dryRun = Lens.lens (\DeleteVpnGateway' {dryRun} -> dryRun) (\s@DeleteVpnGateway' {} a -> s {dryRun = a} :: DeleteVpnGateway)

-- | The ID of the virtual private gateway.
deleteVpnGateway_vpnGatewayId :: Lens.Lens' DeleteVpnGateway Prelude.Text
deleteVpnGateway_vpnGatewayId = Lens.lens (\DeleteVpnGateway' {vpnGatewayId} -> vpnGatewayId) (\s@DeleteVpnGateway' {} a -> s {vpnGatewayId = a} :: DeleteVpnGateway)

instance Core.AWSRequest DeleteVpnGateway where
  type
    AWSResponse DeleteVpnGateway =
      DeleteVpnGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteVpnGatewayResponse'

instance Prelude.Hashable DeleteVpnGateway where
  hashWithSalt _salt DeleteVpnGateway' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpnGatewayId

instance Prelude.NFData DeleteVpnGateway where
  rnf DeleteVpnGateway' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf vpnGatewayId

instance Data.ToHeaders DeleteVpnGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVpnGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVpnGateway where
  toQuery DeleteVpnGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteVpnGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VpnGatewayId" Data.=: vpnGatewayId
      ]

-- | /See:/ 'newDeleteVpnGatewayResponse' smart constructor.
data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpnGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVpnGatewayResponse ::
  DeleteVpnGatewayResponse
newDeleteVpnGatewayResponse =
  DeleteVpnGatewayResponse'

instance Prelude.NFData DeleteVpnGatewayResponse where
  rnf _ = ()
