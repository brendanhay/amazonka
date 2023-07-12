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
-- Module      : Amazonka.EC2.ModifyVpnTunnelOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the options for a VPN tunnel in an Amazon Web Services
-- Site-to-Site VPN connection. You can modify multiple options for a
-- tunnel in a single request, but you can only modify one tunnel at a
-- time. For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPNTunnels.html Site-to-Site VPN tunnel options for your Site-to-Site VPN connection>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
module Amazonka.EC2.ModifyVpnTunnelOptions
  ( -- * Creating a Request
    ModifyVpnTunnelOptions (..),
    newModifyVpnTunnelOptions,

    -- * Request Lenses
    modifyVpnTunnelOptions_dryRun,
    modifyVpnTunnelOptions_vpnConnectionId,
    modifyVpnTunnelOptions_vpnTunnelOutsideIpAddress,
    modifyVpnTunnelOptions_tunnelOptions,

    -- * Destructuring the Response
    ModifyVpnTunnelOptionsResponse (..),
    newModifyVpnTunnelOptionsResponse,

    -- * Response Lenses
    modifyVpnTunnelOptionsResponse_vpnConnection,
    modifyVpnTunnelOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpnTunnelOptions' smart constructor.
data ModifyVpnTunnelOptions = ModifyVpnTunnelOptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Site-to-Site VPN connection.
    vpnConnectionId :: Prelude.Text,
    -- | The external IP address of the VPN tunnel.
    vpnTunnelOutsideIpAddress :: Prelude.Text,
    -- | The tunnel options to modify.
    tunnelOptions :: ModifyVpnTunnelOptionsSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpnTunnelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyVpnTunnelOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpnConnectionId', 'modifyVpnTunnelOptions_vpnConnectionId' - The ID of the Amazon Web Services Site-to-Site VPN connection.
--
-- 'vpnTunnelOutsideIpAddress', 'modifyVpnTunnelOptions_vpnTunnelOutsideIpAddress' - The external IP address of the VPN tunnel.
--
-- 'tunnelOptions', 'modifyVpnTunnelOptions_tunnelOptions' - The tunnel options to modify.
newModifyVpnTunnelOptions ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  -- | 'vpnTunnelOutsideIpAddress'
  Prelude.Text ->
  -- | 'tunnelOptions'
  ModifyVpnTunnelOptionsSpecification ->
  ModifyVpnTunnelOptions
newModifyVpnTunnelOptions
  pVpnConnectionId_
  pVpnTunnelOutsideIpAddress_
  pTunnelOptions_ =
    ModifyVpnTunnelOptions'
      { dryRun = Prelude.Nothing,
        vpnConnectionId = pVpnConnectionId_,
        vpnTunnelOutsideIpAddress =
          pVpnTunnelOutsideIpAddress_,
        tunnelOptions = pTunnelOptions_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpnTunnelOptions_dryRun :: Lens.Lens' ModifyVpnTunnelOptions (Prelude.Maybe Prelude.Bool)
modifyVpnTunnelOptions_dryRun = Lens.lens (\ModifyVpnTunnelOptions' {dryRun} -> dryRun) (\s@ModifyVpnTunnelOptions' {} a -> s {dryRun = a} :: ModifyVpnTunnelOptions)

-- | The ID of the Amazon Web Services Site-to-Site VPN connection.
modifyVpnTunnelOptions_vpnConnectionId :: Lens.Lens' ModifyVpnTunnelOptions Prelude.Text
modifyVpnTunnelOptions_vpnConnectionId = Lens.lens (\ModifyVpnTunnelOptions' {vpnConnectionId} -> vpnConnectionId) (\s@ModifyVpnTunnelOptions' {} a -> s {vpnConnectionId = a} :: ModifyVpnTunnelOptions)

-- | The external IP address of the VPN tunnel.
modifyVpnTunnelOptions_vpnTunnelOutsideIpAddress :: Lens.Lens' ModifyVpnTunnelOptions Prelude.Text
modifyVpnTunnelOptions_vpnTunnelOutsideIpAddress = Lens.lens (\ModifyVpnTunnelOptions' {vpnTunnelOutsideIpAddress} -> vpnTunnelOutsideIpAddress) (\s@ModifyVpnTunnelOptions' {} a -> s {vpnTunnelOutsideIpAddress = a} :: ModifyVpnTunnelOptions)

-- | The tunnel options to modify.
modifyVpnTunnelOptions_tunnelOptions :: Lens.Lens' ModifyVpnTunnelOptions ModifyVpnTunnelOptionsSpecification
modifyVpnTunnelOptions_tunnelOptions = Lens.lens (\ModifyVpnTunnelOptions' {tunnelOptions} -> tunnelOptions) (\s@ModifyVpnTunnelOptions' {} a -> s {tunnelOptions = a} :: ModifyVpnTunnelOptions)

instance Core.AWSRequest ModifyVpnTunnelOptions where
  type
    AWSResponse ModifyVpnTunnelOptions =
      ModifyVpnTunnelOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpnTunnelOptionsResponse'
            Prelude.<$> (x Data..@? "vpnConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVpnTunnelOptions where
  hashWithSalt _salt ModifyVpnTunnelOptions' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpnConnectionId
      `Prelude.hashWithSalt` vpnTunnelOutsideIpAddress
      `Prelude.hashWithSalt` tunnelOptions

instance Prelude.NFData ModifyVpnTunnelOptions where
  rnf ModifyVpnTunnelOptions' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf vpnConnectionId
      `Prelude.seq` Prelude.rnf vpnTunnelOutsideIpAddress
      `Prelude.seq` Prelude.rnf tunnelOptions

instance Data.ToHeaders ModifyVpnTunnelOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVpnTunnelOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVpnTunnelOptions where
  toQuery ModifyVpnTunnelOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyVpnTunnelOptions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VpnConnectionId" Data.=: vpnConnectionId,
        "VpnTunnelOutsideIpAddress"
          Data.=: vpnTunnelOutsideIpAddress,
        "TunnelOptions" Data.=: tunnelOptions
      ]

-- | /See:/ 'newModifyVpnTunnelOptionsResponse' smart constructor.
data ModifyVpnTunnelOptionsResponse = ModifyVpnTunnelOptionsResponse'
  { -- | Information about the VPN connection.
    vpnConnection :: Prelude.Maybe VpnConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpnTunnelOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnConnection', 'modifyVpnTunnelOptionsResponse_vpnConnection' - Information about the VPN connection.
--
-- 'httpStatus', 'modifyVpnTunnelOptionsResponse_httpStatus' - The response's http status code.
newModifyVpnTunnelOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVpnTunnelOptionsResponse
newModifyVpnTunnelOptionsResponse pHttpStatus_ =
  ModifyVpnTunnelOptionsResponse'
    { vpnConnection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the VPN connection.
modifyVpnTunnelOptionsResponse_vpnConnection :: Lens.Lens' ModifyVpnTunnelOptionsResponse (Prelude.Maybe VpnConnection)
modifyVpnTunnelOptionsResponse_vpnConnection = Lens.lens (\ModifyVpnTunnelOptionsResponse' {vpnConnection} -> vpnConnection) (\s@ModifyVpnTunnelOptionsResponse' {} a -> s {vpnConnection = a} :: ModifyVpnTunnelOptionsResponse)

-- | The response's http status code.
modifyVpnTunnelOptionsResponse_httpStatus :: Lens.Lens' ModifyVpnTunnelOptionsResponse Prelude.Int
modifyVpnTunnelOptionsResponse_httpStatus = Lens.lens (\ModifyVpnTunnelOptionsResponse' {httpStatus} -> httpStatus) (\s@ModifyVpnTunnelOptionsResponse' {} a -> s {httpStatus = a} :: ModifyVpnTunnelOptionsResponse)

instance
  Prelude.NFData
    ModifyVpnTunnelOptionsResponse
  where
  rnf ModifyVpnTunnelOptionsResponse' {..} =
    Prelude.rnf vpnConnection
      `Prelude.seq` Prelude.rnf httpStatus
