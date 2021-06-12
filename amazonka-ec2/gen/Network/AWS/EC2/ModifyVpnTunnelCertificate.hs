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
-- Module      : Network.AWS.EC2.ModifyVpnTunnelCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the VPN tunnel endpoint certificate.
module Network.AWS.EC2.ModifyVpnTunnelCertificate
  ( -- * Creating a Request
    ModifyVpnTunnelCertificate (..),
    newModifyVpnTunnelCertificate,

    -- * Request Lenses
    modifyVpnTunnelCertificate_dryRun,
    modifyVpnTunnelCertificate_vpnConnectionId,
    modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress,

    -- * Destructuring the Response
    ModifyVpnTunnelCertificateResponse (..),
    newModifyVpnTunnelCertificateResponse,

    -- * Response Lenses
    modifyVpnTunnelCertificateResponse_vpnConnection,
    modifyVpnTunnelCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpnTunnelCertificate' smart constructor.
data ModifyVpnTunnelCertificate = ModifyVpnTunnelCertificate'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the AWS Site-to-Site VPN connection.
    vpnConnectionId :: Core.Text,
    -- | The external IP address of the VPN tunnel.
    vpnTunnelOutsideIpAddress :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpnTunnelCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyVpnTunnelCertificate_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpnConnectionId', 'modifyVpnTunnelCertificate_vpnConnectionId' - The ID of the AWS Site-to-Site VPN connection.
--
-- 'vpnTunnelOutsideIpAddress', 'modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress' - The external IP address of the VPN tunnel.
newModifyVpnTunnelCertificate ::
  -- | 'vpnConnectionId'
  Core.Text ->
  -- | 'vpnTunnelOutsideIpAddress'
  Core.Text ->
  ModifyVpnTunnelCertificate
newModifyVpnTunnelCertificate
  pVpnConnectionId_
  pVpnTunnelOutsideIpAddress_ =
    ModifyVpnTunnelCertificate'
      { dryRun = Core.Nothing,
        vpnConnectionId = pVpnConnectionId_,
        vpnTunnelOutsideIpAddress =
          pVpnTunnelOutsideIpAddress_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpnTunnelCertificate_dryRun :: Lens.Lens' ModifyVpnTunnelCertificate (Core.Maybe Core.Bool)
modifyVpnTunnelCertificate_dryRun = Lens.lens (\ModifyVpnTunnelCertificate' {dryRun} -> dryRun) (\s@ModifyVpnTunnelCertificate' {} a -> s {dryRun = a} :: ModifyVpnTunnelCertificate)

-- | The ID of the AWS Site-to-Site VPN connection.
modifyVpnTunnelCertificate_vpnConnectionId :: Lens.Lens' ModifyVpnTunnelCertificate Core.Text
modifyVpnTunnelCertificate_vpnConnectionId = Lens.lens (\ModifyVpnTunnelCertificate' {vpnConnectionId} -> vpnConnectionId) (\s@ModifyVpnTunnelCertificate' {} a -> s {vpnConnectionId = a} :: ModifyVpnTunnelCertificate)

-- | The external IP address of the VPN tunnel.
modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress :: Lens.Lens' ModifyVpnTunnelCertificate Core.Text
modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress = Lens.lens (\ModifyVpnTunnelCertificate' {vpnTunnelOutsideIpAddress} -> vpnTunnelOutsideIpAddress) (\s@ModifyVpnTunnelCertificate' {} a -> s {vpnTunnelOutsideIpAddress = a} :: ModifyVpnTunnelCertificate)

instance Core.AWSRequest ModifyVpnTunnelCertificate where
  type
    AWSResponse ModifyVpnTunnelCertificate =
      ModifyVpnTunnelCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpnTunnelCertificateResponse'
            Core.<$> (x Core..@? "vpnConnection")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyVpnTunnelCertificate

instance Core.NFData ModifyVpnTunnelCertificate

instance Core.ToHeaders ModifyVpnTunnelCertificate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyVpnTunnelCertificate where
  toPath = Core.const "/"

instance Core.ToQuery ModifyVpnTunnelCertificate where
  toQuery ModifyVpnTunnelCertificate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyVpnTunnelCertificate" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "VpnConnectionId" Core.=: vpnConnectionId,
        "VpnTunnelOutsideIpAddress"
          Core.=: vpnTunnelOutsideIpAddress
      ]

-- | /See:/ 'newModifyVpnTunnelCertificateResponse' smart constructor.
data ModifyVpnTunnelCertificateResponse = ModifyVpnTunnelCertificateResponse'
  { vpnConnection :: Core.Maybe VpnConnection,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpnTunnelCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnConnection', 'modifyVpnTunnelCertificateResponse_vpnConnection' - Undocumented member.
--
-- 'httpStatus', 'modifyVpnTunnelCertificateResponse_httpStatus' - The response's http status code.
newModifyVpnTunnelCertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyVpnTunnelCertificateResponse
newModifyVpnTunnelCertificateResponse pHttpStatus_ =
  ModifyVpnTunnelCertificateResponse'
    { vpnConnection =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyVpnTunnelCertificateResponse_vpnConnection :: Lens.Lens' ModifyVpnTunnelCertificateResponse (Core.Maybe VpnConnection)
modifyVpnTunnelCertificateResponse_vpnConnection = Lens.lens (\ModifyVpnTunnelCertificateResponse' {vpnConnection} -> vpnConnection) (\s@ModifyVpnTunnelCertificateResponse' {} a -> s {vpnConnection = a} :: ModifyVpnTunnelCertificateResponse)

-- | The response's http status code.
modifyVpnTunnelCertificateResponse_httpStatus :: Lens.Lens' ModifyVpnTunnelCertificateResponse Core.Int
modifyVpnTunnelCertificateResponse_httpStatus = Lens.lens (\ModifyVpnTunnelCertificateResponse' {httpStatus} -> httpStatus) (\s@ModifyVpnTunnelCertificateResponse' {} a -> s {httpStatus = a} :: ModifyVpnTunnelCertificateResponse)

instance
  Core.NFData
    ModifyVpnTunnelCertificateResponse
