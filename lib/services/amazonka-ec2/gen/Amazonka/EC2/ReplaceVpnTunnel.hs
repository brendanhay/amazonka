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
-- Module      : Amazonka.EC2.ReplaceVpnTunnel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Trigger replacement of specified VPN tunnel.
module Amazonka.EC2.ReplaceVpnTunnel
  ( -- * Creating a Request
    ReplaceVpnTunnel (..),
    newReplaceVpnTunnel,

    -- * Request Lenses
    replaceVpnTunnel_applyPendingMaintenance,
    replaceVpnTunnel_dryRun,
    replaceVpnTunnel_vpnConnectionId,
    replaceVpnTunnel_vpnTunnelOutsideIpAddress,

    -- * Destructuring the Response
    ReplaceVpnTunnelResponse (..),
    newReplaceVpnTunnelResponse,

    -- * Response Lenses
    replaceVpnTunnelResponse_return,
    replaceVpnTunnelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReplaceVpnTunnel' smart constructor.
data ReplaceVpnTunnel = ReplaceVpnTunnel'
  { -- | Trigger pending tunnel endpoint maintenance.
    applyPendingMaintenance :: Prelude.Maybe Prelude.Bool,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Site-to-Site VPN connection.
    vpnConnectionId :: Prelude.Text,
    -- | The external IP address of the VPN tunnel.
    vpnTunnelOutsideIpAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceVpnTunnel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyPendingMaintenance', 'replaceVpnTunnel_applyPendingMaintenance' - Trigger pending tunnel endpoint maintenance.
--
-- 'dryRun', 'replaceVpnTunnel_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpnConnectionId', 'replaceVpnTunnel_vpnConnectionId' - The ID of the Site-to-Site VPN connection.
--
-- 'vpnTunnelOutsideIpAddress', 'replaceVpnTunnel_vpnTunnelOutsideIpAddress' - The external IP address of the VPN tunnel.
newReplaceVpnTunnel ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  -- | 'vpnTunnelOutsideIpAddress'
  Prelude.Text ->
  ReplaceVpnTunnel
newReplaceVpnTunnel
  pVpnConnectionId_
  pVpnTunnelOutsideIpAddress_ =
    ReplaceVpnTunnel'
      { applyPendingMaintenance =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        vpnConnectionId = pVpnConnectionId_,
        vpnTunnelOutsideIpAddress =
          pVpnTunnelOutsideIpAddress_
      }

-- | Trigger pending tunnel endpoint maintenance.
replaceVpnTunnel_applyPendingMaintenance :: Lens.Lens' ReplaceVpnTunnel (Prelude.Maybe Prelude.Bool)
replaceVpnTunnel_applyPendingMaintenance = Lens.lens (\ReplaceVpnTunnel' {applyPendingMaintenance} -> applyPendingMaintenance) (\s@ReplaceVpnTunnel' {} a -> s {applyPendingMaintenance = a} :: ReplaceVpnTunnel)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
replaceVpnTunnel_dryRun :: Lens.Lens' ReplaceVpnTunnel (Prelude.Maybe Prelude.Bool)
replaceVpnTunnel_dryRun = Lens.lens (\ReplaceVpnTunnel' {dryRun} -> dryRun) (\s@ReplaceVpnTunnel' {} a -> s {dryRun = a} :: ReplaceVpnTunnel)

-- | The ID of the Site-to-Site VPN connection.
replaceVpnTunnel_vpnConnectionId :: Lens.Lens' ReplaceVpnTunnel Prelude.Text
replaceVpnTunnel_vpnConnectionId = Lens.lens (\ReplaceVpnTunnel' {vpnConnectionId} -> vpnConnectionId) (\s@ReplaceVpnTunnel' {} a -> s {vpnConnectionId = a} :: ReplaceVpnTunnel)

-- | The external IP address of the VPN tunnel.
replaceVpnTunnel_vpnTunnelOutsideIpAddress :: Lens.Lens' ReplaceVpnTunnel Prelude.Text
replaceVpnTunnel_vpnTunnelOutsideIpAddress = Lens.lens (\ReplaceVpnTunnel' {vpnTunnelOutsideIpAddress} -> vpnTunnelOutsideIpAddress) (\s@ReplaceVpnTunnel' {} a -> s {vpnTunnelOutsideIpAddress = a} :: ReplaceVpnTunnel)

instance Core.AWSRequest ReplaceVpnTunnel where
  type
    AWSResponse ReplaceVpnTunnel =
      ReplaceVpnTunnelResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ReplaceVpnTunnelResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReplaceVpnTunnel where
  hashWithSalt _salt ReplaceVpnTunnel' {..} =
    _salt
      `Prelude.hashWithSalt` applyPendingMaintenance
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpnConnectionId
      `Prelude.hashWithSalt` vpnTunnelOutsideIpAddress

instance Prelude.NFData ReplaceVpnTunnel where
  rnf ReplaceVpnTunnel' {..} =
    Prelude.rnf applyPendingMaintenance
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf vpnConnectionId
      `Prelude.seq` Prelude.rnf vpnTunnelOutsideIpAddress

instance Data.ToHeaders ReplaceVpnTunnel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReplaceVpnTunnel where
  toPath = Prelude.const "/"

instance Data.ToQuery ReplaceVpnTunnel where
  toQuery ReplaceVpnTunnel' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ReplaceVpnTunnel" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ApplyPendingMaintenance"
          Data.=: applyPendingMaintenance,
        "DryRun" Data.=: dryRun,
        "VpnConnectionId" Data.=: vpnConnectionId,
        "VpnTunnelOutsideIpAddress"
          Data.=: vpnTunnelOutsideIpAddress
      ]

-- | /See:/ 'newReplaceVpnTunnelResponse' smart constructor.
data ReplaceVpnTunnelResponse = ReplaceVpnTunnelResponse'
  { -- | Confirmation of replace tunnel operation.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceVpnTunnelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'replaceVpnTunnelResponse_return' - Confirmation of replace tunnel operation.
--
-- 'httpStatus', 'replaceVpnTunnelResponse_httpStatus' - The response's http status code.
newReplaceVpnTunnelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReplaceVpnTunnelResponse
newReplaceVpnTunnelResponse pHttpStatus_ =
  ReplaceVpnTunnelResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Confirmation of replace tunnel operation.
replaceVpnTunnelResponse_return :: Lens.Lens' ReplaceVpnTunnelResponse (Prelude.Maybe Prelude.Bool)
replaceVpnTunnelResponse_return = Lens.lens (\ReplaceVpnTunnelResponse' {return'} -> return') (\s@ReplaceVpnTunnelResponse' {} a -> s {return' = a} :: ReplaceVpnTunnelResponse)

-- | The response's http status code.
replaceVpnTunnelResponse_httpStatus :: Lens.Lens' ReplaceVpnTunnelResponse Prelude.Int
replaceVpnTunnelResponse_httpStatus = Lens.lens (\ReplaceVpnTunnelResponse' {httpStatus} -> httpStatus) (\s@ReplaceVpnTunnelResponse' {} a -> s {httpStatus = a} :: ReplaceVpnTunnelResponse)

instance Prelude.NFData ReplaceVpnTunnelResponse where
  rnf ReplaceVpnTunnelResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
