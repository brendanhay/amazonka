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
-- Module      : Amazonka.EC2.ModifyVpnTunnelCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the VPN tunnel endpoint certificate.
module Amazonka.EC2.ModifyVpnTunnelCertificate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpnTunnelCertificate' smart constructor.
data ModifyVpnTunnelCertificate = ModifyVpnTunnelCertificate'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Site-to-Site VPN connection.
    vpnConnectionId :: Prelude.Text,
    -- | The external IP address of the VPN tunnel.
    vpnTunnelOutsideIpAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'vpnConnectionId', 'modifyVpnTunnelCertificate_vpnConnectionId' - The ID of the Amazon Web Services Site-to-Site VPN connection.
--
-- 'vpnTunnelOutsideIpAddress', 'modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress' - The external IP address of the VPN tunnel.
newModifyVpnTunnelCertificate ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  -- | 'vpnTunnelOutsideIpAddress'
  Prelude.Text ->
  ModifyVpnTunnelCertificate
newModifyVpnTunnelCertificate
  pVpnConnectionId_
  pVpnTunnelOutsideIpAddress_ =
    ModifyVpnTunnelCertificate'
      { dryRun =
          Prelude.Nothing,
        vpnConnectionId = pVpnConnectionId_,
        vpnTunnelOutsideIpAddress =
          pVpnTunnelOutsideIpAddress_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpnTunnelCertificate_dryRun :: Lens.Lens' ModifyVpnTunnelCertificate (Prelude.Maybe Prelude.Bool)
modifyVpnTunnelCertificate_dryRun = Lens.lens (\ModifyVpnTunnelCertificate' {dryRun} -> dryRun) (\s@ModifyVpnTunnelCertificate' {} a -> s {dryRun = a} :: ModifyVpnTunnelCertificate)

-- | The ID of the Amazon Web Services Site-to-Site VPN connection.
modifyVpnTunnelCertificate_vpnConnectionId :: Lens.Lens' ModifyVpnTunnelCertificate Prelude.Text
modifyVpnTunnelCertificate_vpnConnectionId = Lens.lens (\ModifyVpnTunnelCertificate' {vpnConnectionId} -> vpnConnectionId) (\s@ModifyVpnTunnelCertificate' {} a -> s {vpnConnectionId = a} :: ModifyVpnTunnelCertificate)

-- | The external IP address of the VPN tunnel.
modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress :: Lens.Lens' ModifyVpnTunnelCertificate Prelude.Text
modifyVpnTunnelCertificate_vpnTunnelOutsideIpAddress = Lens.lens (\ModifyVpnTunnelCertificate' {vpnTunnelOutsideIpAddress} -> vpnTunnelOutsideIpAddress) (\s@ModifyVpnTunnelCertificate' {} a -> s {vpnTunnelOutsideIpAddress = a} :: ModifyVpnTunnelCertificate)

instance Core.AWSRequest ModifyVpnTunnelCertificate where
  type
    AWSResponse ModifyVpnTunnelCertificate =
      ModifyVpnTunnelCertificateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpnTunnelCertificateResponse'
            Prelude.<$> (x Core..@? "vpnConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVpnTunnelCertificate where
  hashWithSalt _salt ModifyVpnTunnelCertificate' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpnConnectionId
      `Prelude.hashWithSalt` vpnTunnelOutsideIpAddress

instance Prelude.NFData ModifyVpnTunnelCertificate where
  rnf ModifyVpnTunnelCertificate' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf vpnConnectionId
      `Prelude.seq` Prelude.rnf vpnTunnelOutsideIpAddress

instance Core.ToHeaders ModifyVpnTunnelCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyVpnTunnelCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyVpnTunnelCertificate where
  toQuery ModifyVpnTunnelCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyVpnTunnelCertificate" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "VpnConnectionId" Core.=: vpnConnectionId,
        "VpnTunnelOutsideIpAddress"
          Core.=: vpnTunnelOutsideIpAddress
      ]

-- | /See:/ 'newModifyVpnTunnelCertificateResponse' smart constructor.
data ModifyVpnTunnelCertificateResponse = ModifyVpnTunnelCertificateResponse'
  { vpnConnection :: Prelude.Maybe VpnConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyVpnTunnelCertificateResponse
newModifyVpnTunnelCertificateResponse pHttpStatus_ =
  ModifyVpnTunnelCertificateResponse'
    { vpnConnection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyVpnTunnelCertificateResponse_vpnConnection :: Lens.Lens' ModifyVpnTunnelCertificateResponse (Prelude.Maybe VpnConnection)
modifyVpnTunnelCertificateResponse_vpnConnection = Lens.lens (\ModifyVpnTunnelCertificateResponse' {vpnConnection} -> vpnConnection) (\s@ModifyVpnTunnelCertificateResponse' {} a -> s {vpnConnection = a} :: ModifyVpnTunnelCertificateResponse)

-- | The response's http status code.
modifyVpnTunnelCertificateResponse_httpStatus :: Lens.Lens' ModifyVpnTunnelCertificateResponse Prelude.Int
modifyVpnTunnelCertificateResponse_httpStatus = Lens.lens (\ModifyVpnTunnelCertificateResponse' {httpStatus} -> httpStatus) (\s@ModifyVpnTunnelCertificateResponse' {} a -> s {httpStatus = a} :: ModifyVpnTunnelCertificateResponse)

instance
  Prelude.NFData
    ModifyVpnTunnelCertificateResponse
  where
  rnf ModifyVpnTunnelCertificateResponse' {..} =
    Prelude.rnf vpnConnection
      `Prelude.seq` Prelude.rnf httpStatus
