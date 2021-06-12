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
-- Module      : Network.AWS.EC2.Types.VpnConnectionOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpnConnectionOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TunnelInsideIpVersion
import Network.AWS.EC2.Types.TunnelOption
import qualified Network.AWS.Lens as Lens

-- | Describes VPN connection options.
--
-- /See:/ 'newVpnConnectionOptions' smart constructor.
data VpnConnectionOptions = VpnConnectionOptions'
  { -- | The IPv6 CIDR on the AWS side of the VPN connection.
    remoteIpv6NetworkCidr :: Core.Maybe Core.Text,
    -- | Indicates whether the VPN connection uses static routes only. Static
    -- routes must be used for devices that don\'t support BGP.
    staticRoutesOnly :: Core.Maybe Core.Bool,
    -- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    localIpv6NetworkCidr :: Core.Maybe Core.Text,
    -- | Indicates whether acceleration is enabled for the VPN connection.
    enableAcceleration :: Core.Maybe Core.Bool,
    -- | Indicates the VPN tunnel options.
    tunnelOptions :: Core.Maybe [TunnelOption],
    -- | The IPv4 CIDR on the AWS side of the VPN connection.
    remoteIpv4NetworkCidr :: Core.Maybe Core.Text,
    -- | Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
    tunnelInsideIpVersion :: Core.Maybe TunnelInsideIpVersion,
    -- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    localIpv4NetworkCidr :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpnConnectionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteIpv6NetworkCidr', 'vpnConnectionOptions_remoteIpv6NetworkCidr' - The IPv6 CIDR on the AWS side of the VPN connection.
--
-- 'staticRoutesOnly', 'vpnConnectionOptions_staticRoutesOnly' - Indicates whether the VPN connection uses static routes only. Static
-- routes must be used for devices that don\'t support BGP.
--
-- 'localIpv6NetworkCidr', 'vpnConnectionOptions_localIpv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- 'enableAcceleration', 'vpnConnectionOptions_enableAcceleration' - Indicates whether acceleration is enabled for the VPN connection.
--
-- 'tunnelOptions', 'vpnConnectionOptions_tunnelOptions' - Indicates the VPN tunnel options.
--
-- 'remoteIpv4NetworkCidr', 'vpnConnectionOptions_remoteIpv4NetworkCidr' - The IPv4 CIDR on the AWS side of the VPN connection.
--
-- 'tunnelInsideIpVersion', 'vpnConnectionOptions_tunnelInsideIpVersion' - Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- 'localIpv4NetworkCidr', 'vpnConnectionOptions_localIpv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
newVpnConnectionOptions ::
  VpnConnectionOptions
newVpnConnectionOptions =
  VpnConnectionOptions'
    { remoteIpv6NetworkCidr =
        Core.Nothing,
      staticRoutesOnly = Core.Nothing,
      localIpv6NetworkCidr = Core.Nothing,
      enableAcceleration = Core.Nothing,
      tunnelOptions = Core.Nothing,
      remoteIpv4NetworkCidr = Core.Nothing,
      tunnelInsideIpVersion = Core.Nothing,
      localIpv4NetworkCidr = Core.Nothing
    }

-- | The IPv6 CIDR on the AWS side of the VPN connection.
vpnConnectionOptions_remoteIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Text)
vpnConnectionOptions_remoteIpv6NetworkCidr = Lens.lens (\VpnConnectionOptions' {remoteIpv6NetworkCidr} -> remoteIpv6NetworkCidr) (\s@VpnConnectionOptions' {} a -> s {remoteIpv6NetworkCidr = a} :: VpnConnectionOptions)

-- | Indicates whether the VPN connection uses static routes only. Static
-- routes must be used for devices that don\'t support BGP.
vpnConnectionOptions_staticRoutesOnly :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Bool)
vpnConnectionOptions_staticRoutesOnly = Lens.lens (\VpnConnectionOptions' {staticRoutesOnly} -> staticRoutesOnly) (\s@VpnConnectionOptions' {} a -> s {staticRoutesOnly = a} :: VpnConnectionOptions)

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
vpnConnectionOptions_localIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Text)
vpnConnectionOptions_localIpv6NetworkCidr = Lens.lens (\VpnConnectionOptions' {localIpv6NetworkCidr} -> localIpv6NetworkCidr) (\s@VpnConnectionOptions' {} a -> s {localIpv6NetworkCidr = a} :: VpnConnectionOptions)

-- | Indicates whether acceleration is enabled for the VPN connection.
vpnConnectionOptions_enableAcceleration :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Bool)
vpnConnectionOptions_enableAcceleration = Lens.lens (\VpnConnectionOptions' {enableAcceleration} -> enableAcceleration) (\s@VpnConnectionOptions' {} a -> s {enableAcceleration = a} :: VpnConnectionOptions)

-- | Indicates the VPN tunnel options.
vpnConnectionOptions_tunnelOptions :: Lens.Lens' VpnConnectionOptions (Core.Maybe [TunnelOption])
vpnConnectionOptions_tunnelOptions = Lens.lens (\VpnConnectionOptions' {tunnelOptions} -> tunnelOptions) (\s@VpnConnectionOptions' {} a -> s {tunnelOptions = a} :: VpnConnectionOptions) Core.. Lens.mapping Lens._Coerce

-- | The IPv4 CIDR on the AWS side of the VPN connection.
vpnConnectionOptions_remoteIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Text)
vpnConnectionOptions_remoteIpv4NetworkCidr = Lens.lens (\VpnConnectionOptions' {remoteIpv4NetworkCidr} -> remoteIpv4NetworkCidr) (\s@VpnConnectionOptions' {} a -> s {remoteIpv4NetworkCidr = a} :: VpnConnectionOptions)

-- | Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
vpnConnectionOptions_tunnelInsideIpVersion :: Lens.Lens' VpnConnectionOptions (Core.Maybe TunnelInsideIpVersion)
vpnConnectionOptions_tunnelInsideIpVersion = Lens.lens (\VpnConnectionOptions' {tunnelInsideIpVersion} -> tunnelInsideIpVersion) (\s@VpnConnectionOptions' {} a -> s {tunnelInsideIpVersion = a} :: VpnConnectionOptions)

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
vpnConnectionOptions_localIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Text)
vpnConnectionOptions_localIpv4NetworkCidr = Lens.lens (\VpnConnectionOptions' {localIpv4NetworkCidr} -> localIpv4NetworkCidr) (\s@VpnConnectionOptions' {} a -> s {localIpv4NetworkCidr = a} :: VpnConnectionOptions)

instance Core.FromXML VpnConnectionOptions where
  parseXML x =
    VpnConnectionOptions'
      Core.<$> (x Core..@? "remoteIpv6NetworkCidr")
      Core.<*> (x Core..@? "staticRoutesOnly")
      Core.<*> (x Core..@? "localIpv6NetworkCidr")
      Core.<*> (x Core..@? "enableAcceleration")
      Core.<*> ( x Core..@? "tunnelOptionSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "remoteIpv4NetworkCidr")
      Core.<*> (x Core..@? "tunnelInsideIpVersion")
      Core.<*> (x Core..@? "localIpv4NetworkCidr")

instance Core.Hashable VpnConnectionOptions

instance Core.NFData VpnConnectionOptions
