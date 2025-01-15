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
-- Module      : Amazonka.EC2.ModifyVpnConnectionOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the connection options for your Site-to-Site VPN connection.
--
-- When you modify the VPN connection options, the VPN endpoint IP
-- addresses on the Amazon Web Services side do not change, and the tunnel
-- options do not change. Your VPN connection will be temporarily
-- unavailable for a brief period while the VPN connection is updated.
module Amazonka.EC2.ModifyVpnConnectionOptions
  ( -- * Creating a Request
    ModifyVpnConnectionOptions (..),
    newModifyVpnConnectionOptions,

    -- * Request Lenses
    modifyVpnConnectionOptions_dryRun,
    modifyVpnConnectionOptions_localIpv4NetworkCidr,
    modifyVpnConnectionOptions_localIpv6NetworkCidr,
    modifyVpnConnectionOptions_remoteIpv4NetworkCidr,
    modifyVpnConnectionOptions_remoteIpv6NetworkCidr,
    modifyVpnConnectionOptions_vpnConnectionId,

    -- * Destructuring the Response
    ModifyVpnConnectionOptionsResponse (..),
    newModifyVpnConnectionOptionsResponse,

    -- * Response Lenses
    modifyVpnConnectionOptionsResponse_vpnConnection,
    modifyVpnConnectionOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpnConnectionOptions' smart constructor.
data ModifyVpnConnectionOptions = ModifyVpnConnectionOptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    --
    -- Default: @0.0.0.0\/0@
    localIpv4NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    --
    -- Default: @::\/0@
    localIpv6NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR on the Amazon Web Services side of the VPN connection.
    --
    -- Default: @0.0.0.0\/0@
    remoteIpv4NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR on the Amazon Web Services side of the VPN connection.
    --
    -- Default: @::\/0@
    remoteIpv6NetworkCidr :: Prelude.Maybe Prelude.Text,
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
-- 'dryRun', 'modifyVpnConnectionOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'localIpv4NetworkCidr', 'modifyVpnConnectionOptions_localIpv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @0.0.0.0\/0@
--
-- 'localIpv6NetworkCidr', 'modifyVpnConnectionOptions_localIpv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @::\/0@
--
-- 'remoteIpv4NetworkCidr', 'modifyVpnConnectionOptions_remoteIpv4NetworkCidr' - The IPv4 CIDR on the Amazon Web Services side of the VPN connection.
--
-- Default: @0.0.0.0\/0@
--
-- 'remoteIpv6NetworkCidr', 'modifyVpnConnectionOptions_remoteIpv6NetworkCidr' - The IPv6 CIDR on the Amazon Web Services side of the VPN connection.
--
-- Default: @::\/0@
--
-- 'vpnConnectionId', 'modifyVpnConnectionOptions_vpnConnectionId' - The ID of the Site-to-Site VPN connection.
newModifyVpnConnectionOptions ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  ModifyVpnConnectionOptions
newModifyVpnConnectionOptions pVpnConnectionId_ =
  ModifyVpnConnectionOptions'
    { dryRun =
        Prelude.Nothing,
      localIpv4NetworkCidr = Prelude.Nothing,
      localIpv6NetworkCidr = Prelude.Nothing,
      remoteIpv4NetworkCidr = Prelude.Nothing,
      remoteIpv6NetworkCidr = Prelude.Nothing,
      vpnConnectionId = pVpnConnectionId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpnConnectionOptions_dryRun :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Bool)
modifyVpnConnectionOptions_dryRun = Lens.lens (\ModifyVpnConnectionOptions' {dryRun} -> dryRun) (\s@ModifyVpnConnectionOptions' {} a -> s {dryRun = a} :: ModifyVpnConnectionOptions)

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @0.0.0.0\/0@
modifyVpnConnectionOptions_localIpv4NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Text)
modifyVpnConnectionOptions_localIpv4NetworkCidr = Lens.lens (\ModifyVpnConnectionOptions' {localIpv4NetworkCidr} -> localIpv4NetworkCidr) (\s@ModifyVpnConnectionOptions' {} a -> s {localIpv4NetworkCidr = a} :: ModifyVpnConnectionOptions)

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @::\/0@
modifyVpnConnectionOptions_localIpv6NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Text)
modifyVpnConnectionOptions_localIpv6NetworkCidr = Lens.lens (\ModifyVpnConnectionOptions' {localIpv6NetworkCidr} -> localIpv6NetworkCidr) (\s@ModifyVpnConnectionOptions' {} a -> s {localIpv6NetworkCidr = a} :: ModifyVpnConnectionOptions)

-- | The IPv4 CIDR on the Amazon Web Services side of the VPN connection.
--
-- Default: @0.0.0.0\/0@
modifyVpnConnectionOptions_remoteIpv4NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Text)
modifyVpnConnectionOptions_remoteIpv4NetworkCidr = Lens.lens (\ModifyVpnConnectionOptions' {remoteIpv4NetworkCidr} -> remoteIpv4NetworkCidr) (\s@ModifyVpnConnectionOptions' {} a -> s {remoteIpv4NetworkCidr = a} :: ModifyVpnConnectionOptions)

-- | The IPv6 CIDR on the Amazon Web Services side of the VPN connection.
--
-- Default: @::\/0@
modifyVpnConnectionOptions_remoteIpv6NetworkCidr :: Lens.Lens' ModifyVpnConnectionOptions (Prelude.Maybe Prelude.Text)
modifyVpnConnectionOptions_remoteIpv6NetworkCidr = Lens.lens (\ModifyVpnConnectionOptions' {remoteIpv6NetworkCidr} -> remoteIpv6NetworkCidr) (\s@ModifyVpnConnectionOptions' {} a -> s {remoteIpv6NetworkCidr = a} :: ModifyVpnConnectionOptions)

-- | The ID of the Site-to-Site VPN connection.
modifyVpnConnectionOptions_vpnConnectionId :: Lens.Lens' ModifyVpnConnectionOptions Prelude.Text
modifyVpnConnectionOptions_vpnConnectionId = Lens.lens (\ModifyVpnConnectionOptions' {vpnConnectionId} -> vpnConnectionId) (\s@ModifyVpnConnectionOptions' {} a -> s {vpnConnectionId = a} :: ModifyVpnConnectionOptions)

instance Core.AWSRequest ModifyVpnConnectionOptions where
  type
    AWSResponse ModifyVpnConnectionOptions =
      ModifyVpnConnectionOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpnConnectionOptionsResponse'
            Prelude.<$> (x Data..@? "vpnConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVpnConnectionOptions where
  hashWithSalt _salt ModifyVpnConnectionOptions' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` localIpv4NetworkCidr
      `Prelude.hashWithSalt` localIpv6NetworkCidr
      `Prelude.hashWithSalt` remoteIpv4NetworkCidr
      `Prelude.hashWithSalt` remoteIpv6NetworkCidr
      `Prelude.hashWithSalt` vpnConnectionId

instance Prelude.NFData ModifyVpnConnectionOptions where
  rnf ModifyVpnConnectionOptions' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf localIpv4NetworkCidr `Prelude.seq`
        Prelude.rnf localIpv6NetworkCidr `Prelude.seq`
          Prelude.rnf remoteIpv4NetworkCidr `Prelude.seq`
            Prelude.rnf remoteIpv6NetworkCidr `Prelude.seq`
              Prelude.rnf vpnConnectionId

instance Data.ToHeaders ModifyVpnConnectionOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVpnConnectionOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVpnConnectionOptions where
  toQuery ModifyVpnConnectionOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyVpnConnectionOptions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "LocalIpv4NetworkCidr" Data.=: localIpv4NetworkCidr,
        "LocalIpv6NetworkCidr" Data.=: localIpv6NetworkCidr,
        "RemoteIpv4NetworkCidr"
          Data.=: remoteIpv4NetworkCidr,
        "RemoteIpv6NetworkCidr"
          Data.=: remoteIpv6NetworkCidr,
        "VpnConnectionId" Data.=: vpnConnectionId
      ]

-- | /See:/ 'newModifyVpnConnectionOptionsResponse' smart constructor.
data ModifyVpnConnectionOptionsResponse = ModifyVpnConnectionOptionsResponse'
  { -- | Information about the VPN connection.
    vpnConnection :: Prelude.Maybe VpnConnection,
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
-- 'vpnConnection', 'modifyVpnConnectionOptionsResponse_vpnConnection' - Information about the VPN connection.
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

-- | Information about the VPN connection.
modifyVpnConnectionOptionsResponse_vpnConnection :: Lens.Lens' ModifyVpnConnectionOptionsResponse (Prelude.Maybe VpnConnection)
modifyVpnConnectionOptionsResponse_vpnConnection = Lens.lens (\ModifyVpnConnectionOptionsResponse' {vpnConnection} -> vpnConnection) (\s@ModifyVpnConnectionOptionsResponse' {} a -> s {vpnConnection = a} :: ModifyVpnConnectionOptionsResponse)

-- | The response's http status code.
modifyVpnConnectionOptionsResponse_httpStatus :: Lens.Lens' ModifyVpnConnectionOptionsResponse Prelude.Int
modifyVpnConnectionOptionsResponse_httpStatus = Lens.lens (\ModifyVpnConnectionOptionsResponse' {httpStatus} -> httpStatus) (\s@ModifyVpnConnectionOptionsResponse' {} a -> s {httpStatus = a} :: ModifyVpnConnectionOptionsResponse)

instance
  Prelude.NFData
    ModifyVpnConnectionOptionsResponse
  where
  rnf ModifyVpnConnectionOptionsResponse' {..} =
    Prelude.rnf vpnConnection `Prelude.seq`
      Prelude.rnf httpStatus
