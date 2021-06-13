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
-- Module      : Network.AWS.EC2.ModifyVpnConnectionOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the connection options for your Site-to-Site VPN connection.
--
-- When you modify the VPN connection options, the VPN endpoint IP
-- addresses on the AWS side do not change, and the tunnel options do not
-- change. Your VPN connection will be temporarily unavailable for a brief
-- period while the VPN connection is updated.
module Network.AWS.EC2.ModifyVpnConnectionOptions
  ( -- * Creating a Request
    ModifyVpnConnectionOptions (..),
    newModifyVpnConnectionOptions,

    -- * Request Lenses
    modifyVpnConnectionOptions_remoteIpv6NetworkCidr,
    modifyVpnConnectionOptions_dryRun,
    modifyVpnConnectionOptions_localIpv6NetworkCidr,
    modifyVpnConnectionOptions_remoteIpv4NetworkCidr,
    modifyVpnConnectionOptions_localIpv4NetworkCidr,
    modifyVpnConnectionOptions_vpnConnectionId,

    -- * Destructuring the Response
    ModifyVpnConnectionOptionsResponse (..),
    newModifyVpnConnectionOptionsResponse,

    -- * Response Lenses
    modifyVpnConnectionOptionsResponse_vpnConnection,
    modifyVpnConnectionOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpnConnectionOptions' smart constructor.
data ModifyVpnConnectionOptions = ModifyVpnConnectionOptions'
  { -- | The IPv6 CIDR on the AWS side of the VPN connection.
    --
    -- Default: @::\/0@
    remoteIpv6NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    --
    -- Default: @::\/0@
    localIpv6NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR on the AWS side of the VPN connection.
    --
    -- Default: @0.0.0.0\/0@
    remoteIpv4NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    --
    -- Default: @0.0.0.0\/0@
    localIpv4NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Site-to-Site VPN connection.
    vpnConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpnConnectionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteIpv6NetworkCidr', 'modifyVpnConnectionOptions_remoteIpv6NetworkCidr' - The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::\/0@
--
-- 'dryRun', 'modifyVpnConnectionOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'localIpv6NetworkCidr', 'modifyVpnConnectionOptions_localIpv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @::\/0@
--
-- 'remoteIpv4NetworkCidr', 'modifyVpnConnectionOptions_remoteIpv4NetworkCidr' - The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0\/0@
--
-- 'localIpv4NetworkCidr', 'modifyVpnConnectionOptions_localIpv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @0.0.0.0\/0@
--
-- 'vpnConnectionId', 'modifyVpnConnectionOptions_vpnConnectionId' - The ID of the Site-to-Site VPN connection.
newModifyVpnConnectionOptions ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  ModifyVpnConnectionOptions
newModifyVpnConnectionOptions pVpnConnectionId_ =
  ModifyVpnConnectionOptions'
    { remoteIpv6NetworkCidr =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      localIpv6NetworkCidr = Prelude.Nothing,
      remoteIpv4NetworkCidr = Prelude.Nothing,
      localIpv4NetworkCidr = Prelude.Nothing,
      vpnConnectionId = pVpnConnectionId_
    }

-- | The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::\/0@
modifyVpnConnectionOptions_remoteIpv6NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Text)
modifyVpnConnectionOptions_remoteIpv6NetworkCidr = Lens.lens (\ModifyVpnConnectionOptions' {remoteIpv6NetworkCidr} -> remoteIpv6NetworkCidr) (\s@ModifyVpnConnectionOptions' {} a -> s {remoteIpv6NetworkCidr = a} :: ModifyVpnConnectionOptions)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpnConnectionOptions_dryRun :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Bool)
modifyVpnConnectionOptions_dryRun = Lens.lens (\ModifyVpnConnectionOptions' {dryRun} -> dryRun) (\s@ModifyVpnConnectionOptions' {} a -> s {dryRun = a} :: ModifyVpnConnectionOptions)

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @::\/0@
modifyVpnConnectionOptions_localIpv6NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Text)
modifyVpnConnectionOptions_localIpv6NetworkCidr = Lens.lens (\ModifyVpnConnectionOptions' {localIpv6NetworkCidr} -> localIpv6NetworkCidr) (\s@ModifyVpnConnectionOptions' {} a -> s {localIpv6NetworkCidr = a} :: ModifyVpnConnectionOptions)

-- | The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0\/0@
modifyVpnConnectionOptions_remoteIpv4NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Text)
modifyVpnConnectionOptions_remoteIpv4NetworkCidr = Lens.lens (\ModifyVpnConnectionOptions' {remoteIpv4NetworkCidr} -> remoteIpv4NetworkCidr) (\s@ModifyVpnConnectionOptions' {} a -> s {remoteIpv4NetworkCidr = a} :: ModifyVpnConnectionOptions)

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @0.0.0.0\/0@
modifyVpnConnectionOptions_localIpv4NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Text)
modifyVpnConnectionOptions_localIpv4NetworkCidr = Lens.lens (\ModifyVpnConnectionOptions' {localIpv4NetworkCidr} -> localIpv4NetworkCidr) (\s@ModifyVpnConnectionOptions' {} a -> s {localIpv4NetworkCidr = a} :: ModifyVpnConnectionOptions)

-- | The ID of the Site-to-Site VPN connection.
modifyVpnConnectionOptions_vpnConnectionId :: Lens.Lens' ModifyVpnConnectionOptions Prelude.Text
modifyVpnConnectionOptions_vpnConnectionId = Lens.lens (\ModifyVpnConnectionOptions' {vpnConnectionId} -> vpnConnectionId) (\s@ModifyVpnConnectionOptions' {} a -> s {vpnConnectionId = a} :: ModifyVpnConnectionOptions)

instance Core.AWSRequest ModifyVpnConnectionOptions where
  type
    AWSResponse ModifyVpnConnectionOptions =
      ModifyVpnConnectionOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpnConnectionOptionsResponse'
            Prelude.<$> (x Core..@? "vpnConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVpnConnectionOptions

instance Prelude.NFData ModifyVpnConnectionOptions

instance Core.ToHeaders ModifyVpnConnectionOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyVpnConnectionOptions where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyVpnConnectionOptions where
  toQuery ModifyVpnConnectionOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyVpnConnectionOptions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "RemoteIpv6NetworkCidr"
          Core.=: remoteIpv6NetworkCidr,
        "DryRun" Core.=: dryRun,
        "LocalIpv6NetworkCidr" Core.=: localIpv6NetworkCidr,
        "RemoteIpv4NetworkCidr"
          Core.=: remoteIpv4NetworkCidr,
        "LocalIpv4NetworkCidr" Core.=: localIpv4NetworkCidr,
        "VpnConnectionId" Core.=: vpnConnectionId
      ]

-- | /See:/ 'newModifyVpnConnectionOptionsResponse' smart constructor.
data ModifyVpnConnectionOptionsResponse = ModifyVpnConnectionOptionsResponse'
  { vpnConnection :: Prelude.Maybe VpnConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpnConnectionOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnConnection', 'modifyVpnConnectionOptionsResponse_vpnConnection' - Undocumented member.
--
-- 'httpStatus', 'modifyVpnConnectionOptionsResponse_httpStatus' - The response's http status code.
newModifyVpnConnectionOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVpnConnectionOptionsResponse
newModifyVpnConnectionOptionsResponse pHttpStatus_ =
  ModifyVpnConnectionOptionsResponse'
    { vpnConnection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyVpnConnectionOptionsResponse_vpnConnection :: Lens.Lens' ModifyVpnConnectionOptionsResponse (Prelude.Maybe VpnConnection)
modifyVpnConnectionOptionsResponse_vpnConnection = Lens.lens (\ModifyVpnConnectionOptionsResponse' {vpnConnection} -> vpnConnection) (\s@ModifyVpnConnectionOptionsResponse' {} a -> s {vpnConnection = a} :: ModifyVpnConnectionOptionsResponse)

-- | The response's http status code.
modifyVpnConnectionOptionsResponse_httpStatus :: Lens.Lens' ModifyVpnConnectionOptionsResponse Prelude.Int
modifyVpnConnectionOptionsResponse_httpStatus = Lens.lens (\ModifyVpnConnectionOptionsResponse' {httpStatus} -> httpStatus) (\s@ModifyVpnConnectionOptionsResponse' {} a -> s {httpStatus = a} :: ModifyVpnConnectionOptionsResponse)

instance
  Prelude.NFData
    ModifyVpnConnectionOptionsResponse
