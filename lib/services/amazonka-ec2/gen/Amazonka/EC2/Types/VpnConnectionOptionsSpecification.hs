{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.VpnConnectionOptionsSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpnConnectionOptionsSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TunnelInsideIpVersion
import Amazonka.EC2.Types.VpnTunnelOptionsSpecification
import qualified Amazonka.Prelude as Prelude

-- | Describes VPN connection options.
--
-- /See:/ 'newVpnConnectionOptionsSpecification' smart constructor.
data VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification'
  { -- | Indicate whether to enable acceleration for the VPN connection.
    --
    -- Default: @false@
    enableAcceleration :: Prelude.Maybe Prelude.Bool,
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
    -- | The type of IPv4 address assigned to the outside interface of the
    -- customer gateway device.
    --
    -- Valid values: @PrivateIpv4@ | @PublicIpv4@
    --
    -- Default: @PublicIpv4@
    outsideIpAddressType :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR on the Amazon Web Services side of the VPN connection.
    --
    -- Default: @0.0.0.0\/0@
    remoteIpv4NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR on the Amazon Web Services side of the VPN connection.
    --
    -- Default: @::\/0@
    remoteIpv6NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | Indicate whether the VPN connection uses static routes only. If you are
    -- creating a VPN connection for a device that does not support BGP, you
    -- must specify @true@. Use CreateVpnConnectionRoute to create a static
    -- route.
    --
    -- Default: @false@
    staticRoutesOnly :: Prelude.Maybe Prelude.Bool,
    -- | The transit gateway attachment ID to use for the VPN tunnel.
    --
    -- Required if @OutsideIpAddressType@ is set to @PrivateIpv4@.
    transportTransitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
    --
    -- Default: @ipv4@
    tunnelInsideIpVersion :: Prelude.Maybe TunnelInsideIpVersion,
    -- | The tunnel options for the VPN connection.
    tunnelOptions :: Prelude.Maybe [VpnTunnelOptionsSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpnConnectionOptionsSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableAcceleration', 'vpnConnectionOptionsSpecification_enableAcceleration' - Indicate whether to enable acceleration for the VPN connection.
--
-- Default: @false@
--
-- 'localIpv4NetworkCidr', 'vpnConnectionOptionsSpecification_localIpv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @0.0.0.0\/0@
--
-- 'localIpv6NetworkCidr', 'vpnConnectionOptionsSpecification_localIpv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @::\/0@
--
-- 'outsideIpAddressType', 'vpnConnectionOptionsSpecification_outsideIpAddressType' - The type of IPv4 address assigned to the outside interface of the
-- customer gateway device.
--
-- Valid values: @PrivateIpv4@ | @PublicIpv4@
--
-- Default: @PublicIpv4@
--
-- 'remoteIpv4NetworkCidr', 'vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr' - The IPv4 CIDR on the Amazon Web Services side of the VPN connection.
--
-- Default: @0.0.0.0\/0@
--
-- 'remoteIpv6NetworkCidr', 'vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr' - The IPv6 CIDR on the Amazon Web Services side of the VPN connection.
--
-- Default: @::\/0@
--
-- 'staticRoutesOnly', 'vpnConnectionOptionsSpecification_staticRoutesOnly' - Indicate whether the VPN connection uses static routes only. If you are
-- creating a VPN connection for a device that does not support BGP, you
-- must specify @true@. Use CreateVpnConnectionRoute to create a static
-- route.
--
-- Default: @false@
--
-- 'transportTransitGatewayAttachmentId', 'vpnConnectionOptionsSpecification_transportTransitGatewayAttachmentId' - The transit gateway attachment ID to use for the VPN tunnel.
--
-- Required if @OutsideIpAddressType@ is set to @PrivateIpv4@.
--
-- 'tunnelInsideIpVersion', 'vpnConnectionOptionsSpecification_tunnelInsideIpVersion' - Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- Default: @ipv4@
--
-- 'tunnelOptions', 'vpnConnectionOptionsSpecification_tunnelOptions' - The tunnel options for the VPN connection.
newVpnConnectionOptionsSpecification ::
  VpnConnectionOptionsSpecification
newVpnConnectionOptionsSpecification =
  VpnConnectionOptionsSpecification'
    { enableAcceleration =
        Prelude.Nothing,
      localIpv4NetworkCidr = Prelude.Nothing,
      localIpv6NetworkCidr = Prelude.Nothing,
      outsideIpAddressType = Prelude.Nothing,
      remoteIpv4NetworkCidr = Prelude.Nothing,
      remoteIpv6NetworkCidr = Prelude.Nothing,
      staticRoutesOnly = Prelude.Nothing,
      transportTransitGatewayAttachmentId =
        Prelude.Nothing,
      tunnelInsideIpVersion = Prelude.Nothing,
      tunnelOptions = Prelude.Nothing
    }

-- | Indicate whether to enable acceleration for the VPN connection.
--
-- Default: @false@
vpnConnectionOptionsSpecification_enableAcceleration :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Bool)
vpnConnectionOptionsSpecification_enableAcceleration = Lens.lens (\VpnConnectionOptionsSpecification' {enableAcceleration} -> enableAcceleration) (\s@VpnConnectionOptionsSpecification' {} a -> s {enableAcceleration = a} :: VpnConnectionOptionsSpecification)

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @0.0.0.0\/0@
vpnConnectionOptionsSpecification_localIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnConnectionOptionsSpecification_localIpv4NetworkCidr = Lens.lens (\VpnConnectionOptionsSpecification' {localIpv4NetworkCidr} -> localIpv4NetworkCidr) (\s@VpnConnectionOptionsSpecification' {} a -> s {localIpv4NetworkCidr = a} :: VpnConnectionOptionsSpecification)

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @::\/0@
vpnConnectionOptionsSpecification_localIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnConnectionOptionsSpecification_localIpv6NetworkCidr = Lens.lens (\VpnConnectionOptionsSpecification' {localIpv6NetworkCidr} -> localIpv6NetworkCidr) (\s@VpnConnectionOptionsSpecification' {} a -> s {localIpv6NetworkCidr = a} :: VpnConnectionOptionsSpecification)

-- | The type of IPv4 address assigned to the outside interface of the
-- customer gateway device.
--
-- Valid values: @PrivateIpv4@ | @PublicIpv4@
--
-- Default: @PublicIpv4@
vpnConnectionOptionsSpecification_outsideIpAddressType :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnConnectionOptionsSpecification_outsideIpAddressType = Lens.lens (\VpnConnectionOptionsSpecification' {outsideIpAddressType} -> outsideIpAddressType) (\s@VpnConnectionOptionsSpecification' {} a -> s {outsideIpAddressType = a} :: VpnConnectionOptionsSpecification)

-- | The IPv4 CIDR on the Amazon Web Services side of the VPN connection.
--
-- Default: @0.0.0.0\/0@
vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr = Lens.lens (\VpnConnectionOptionsSpecification' {remoteIpv4NetworkCidr} -> remoteIpv4NetworkCidr) (\s@VpnConnectionOptionsSpecification' {} a -> s {remoteIpv4NetworkCidr = a} :: VpnConnectionOptionsSpecification)

-- | The IPv6 CIDR on the Amazon Web Services side of the VPN connection.
--
-- Default: @::\/0@
vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr = Lens.lens (\VpnConnectionOptionsSpecification' {remoteIpv6NetworkCidr} -> remoteIpv6NetworkCidr) (\s@VpnConnectionOptionsSpecification' {} a -> s {remoteIpv6NetworkCidr = a} :: VpnConnectionOptionsSpecification)

-- | Indicate whether the VPN connection uses static routes only. If you are
-- creating a VPN connection for a device that does not support BGP, you
-- must specify @true@. Use CreateVpnConnectionRoute to create a static
-- route.
--
-- Default: @false@
vpnConnectionOptionsSpecification_staticRoutesOnly :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Bool)
vpnConnectionOptionsSpecification_staticRoutesOnly = Lens.lens (\VpnConnectionOptionsSpecification' {staticRoutesOnly} -> staticRoutesOnly) (\s@VpnConnectionOptionsSpecification' {} a -> s {staticRoutesOnly = a} :: VpnConnectionOptionsSpecification)

-- | The transit gateway attachment ID to use for the VPN tunnel.
--
-- Required if @OutsideIpAddressType@ is set to @PrivateIpv4@.
vpnConnectionOptionsSpecification_transportTransitGatewayAttachmentId :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnConnectionOptionsSpecification_transportTransitGatewayAttachmentId = Lens.lens (\VpnConnectionOptionsSpecification' {transportTransitGatewayAttachmentId} -> transportTransitGatewayAttachmentId) (\s@VpnConnectionOptionsSpecification' {} a -> s {transportTransitGatewayAttachmentId = a} :: VpnConnectionOptionsSpecification)

-- | Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- Default: @ipv4@
vpnConnectionOptionsSpecification_tunnelInsideIpVersion :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe TunnelInsideIpVersion)
vpnConnectionOptionsSpecification_tunnelInsideIpVersion = Lens.lens (\VpnConnectionOptionsSpecification' {tunnelInsideIpVersion} -> tunnelInsideIpVersion) (\s@VpnConnectionOptionsSpecification' {} a -> s {tunnelInsideIpVersion = a} :: VpnConnectionOptionsSpecification)

-- | The tunnel options for the VPN connection.
vpnConnectionOptionsSpecification_tunnelOptions :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe [VpnTunnelOptionsSpecification])
vpnConnectionOptionsSpecification_tunnelOptions = Lens.lens (\VpnConnectionOptionsSpecification' {tunnelOptions} -> tunnelOptions) (\s@VpnConnectionOptionsSpecification' {} a -> s {tunnelOptions = a} :: VpnConnectionOptionsSpecification) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    VpnConnectionOptionsSpecification
  where
  hashWithSalt
    _salt
    VpnConnectionOptionsSpecification' {..} =
      _salt `Prelude.hashWithSalt` enableAcceleration
        `Prelude.hashWithSalt` localIpv4NetworkCidr
        `Prelude.hashWithSalt` localIpv6NetworkCidr
        `Prelude.hashWithSalt` outsideIpAddressType
        `Prelude.hashWithSalt` remoteIpv4NetworkCidr
        `Prelude.hashWithSalt` remoteIpv6NetworkCidr
        `Prelude.hashWithSalt` staticRoutesOnly
        `Prelude.hashWithSalt` transportTransitGatewayAttachmentId
        `Prelude.hashWithSalt` tunnelInsideIpVersion
        `Prelude.hashWithSalt` tunnelOptions

instance
  Prelude.NFData
    VpnConnectionOptionsSpecification
  where
  rnf VpnConnectionOptionsSpecification' {..} =
    Prelude.rnf enableAcceleration
      `Prelude.seq` Prelude.rnf localIpv4NetworkCidr
      `Prelude.seq` Prelude.rnf localIpv6NetworkCidr
      `Prelude.seq` Prelude.rnf outsideIpAddressType
      `Prelude.seq` Prelude.rnf remoteIpv4NetworkCidr
      `Prelude.seq` Prelude.rnf remoteIpv6NetworkCidr
      `Prelude.seq` Prelude.rnf staticRoutesOnly
      `Prelude.seq` Prelude.rnf transportTransitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf tunnelInsideIpVersion
      `Prelude.seq` Prelude.rnf tunnelOptions

instance
  Data.ToQuery
    VpnConnectionOptionsSpecification
  where
  toQuery VpnConnectionOptionsSpecification' {..} =
    Prelude.mconcat
      [ "EnableAcceleration" Data.=: enableAcceleration,
        "LocalIpv4NetworkCidr" Data.=: localIpv4NetworkCidr,
        "LocalIpv6NetworkCidr" Data.=: localIpv6NetworkCidr,
        "OutsideIpAddressType" Data.=: outsideIpAddressType,
        "RemoteIpv4NetworkCidr"
          Data.=: remoteIpv4NetworkCidr,
        "RemoteIpv6NetworkCidr"
          Data.=: remoteIpv6NetworkCidr,
        "StaticRoutesOnly" Data.=: staticRoutesOnly,
        "TransportTransitGatewayAttachmentId"
          Data.=: transportTransitGatewayAttachmentId,
        "TunnelInsideIpVersion"
          Data.=: tunnelInsideIpVersion,
        Data.toQuery
          ( Data.toQueryList "TunnelOptions"
              Prelude.<$> tunnelOptions
          )
      ]
