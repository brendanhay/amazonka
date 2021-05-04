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
-- Module      : Network.AWS.EC2.DeleteVpnConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPN connection.
--
-- If you\'re deleting the VPC and its associated components, we recommend
-- that you detach the virtual private gateway from the VPC and delete the
-- VPC before deleting the VPN connection. If you believe that the tunnel
-- credentials for your VPN connection have been compromised, you can
-- delete the VPN connection and create a new one that has new keys,
-- without needing to delete the VPC or virtual private gateway. If you
-- create a new VPN connection, you must reconfigure the customer gateway
-- device using the new configuration information returned with the new VPN
-- connection ID.
--
-- For certificate-based authentication, delete all AWS Certificate Manager
-- (ACM) private certificates used for the AWS-side tunnel endpoints for
-- the VPN connection before deleting the VPN connection.
module Network.AWS.EC2.DeleteVpnConnection
  ( -- * Creating a Request
    DeleteVpnConnection (..),
    newDeleteVpnConnection,

    -- * Request Lenses
    deleteVpnConnection_dryRun,
    deleteVpnConnection_vpnConnectionId,

    -- * Destructuring the Response
    DeleteVpnConnectionResponse (..),
    newDeleteVpnConnectionResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteVpnConnection.
--
-- /See:/ 'newDeleteVpnConnection' smart constructor.
data DeleteVpnConnection = DeleteVpnConnection'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPN connection.
    vpnConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpnConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteVpnConnection_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpnConnectionId', 'deleteVpnConnection_vpnConnectionId' - The ID of the VPN connection.
newDeleteVpnConnection ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  DeleteVpnConnection
newDeleteVpnConnection pVpnConnectionId_ =
  DeleteVpnConnection'
    { dryRun = Prelude.Nothing,
      vpnConnectionId = pVpnConnectionId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVpnConnection_dryRun :: Lens.Lens' DeleteVpnConnection (Prelude.Maybe Prelude.Bool)
deleteVpnConnection_dryRun = Lens.lens (\DeleteVpnConnection' {dryRun} -> dryRun) (\s@DeleteVpnConnection' {} a -> s {dryRun = a} :: DeleteVpnConnection)

-- | The ID of the VPN connection.
deleteVpnConnection_vpnConnectionId :: Lens.Lens' DeleteVpnConnection Prelude.Text
deleteVpnConnection_vpnConnectionId = Lens.lens (\DeleteVpnConnection' {vpnConnectionId} -> vpnConnectionId) (\s@DeleteVpnConnection' {} a -> s {vpnConnectionId = a} :: DeleteVpnConnection)

instance Prelude.AWSRequest DeleteVpnConnection where
  type
    Rs DeleteVpnConnection =
      DeleteVpnConnectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteVpnConnectionResponse'

instance Prelude.Hashable DeleteVpnConnection

instance Prelude.NFData DeleteVpnConnection

instance Prelude.ToHeaders DeleteVpnConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteVpnConnection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteVpnConnection where
  toQuery DeleteVpnConnection' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteVpnConnection" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "VpnConnectionId" Prelude.=: vpnConnectionId
      ]

-- | /See:/ 'newDeleteVpnConnectionResponse' smart constructor.
data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpnConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVpnConnectionResponse ::
  DeleteVpnConnectionResponse
newDeleteVpnConnectionResponse =
  DeleteVpnConnectionResponse'

instance Prelude.NFData DeleteVpnConnectionResponse
