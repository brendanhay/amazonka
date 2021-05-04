{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.VpnConnectionOptionsSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpnConnectionOptionsSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TunnelInsideIpVersion
import Network.AWS.EC2.Types.VpnTunnelOptionsSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes VPN connection options.
--
-- /See:/ 'newVpnConnectionOptionsSpecification' smart constructor.
data VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification'
  { -- | The IPv6 CIDR on the AWS side of the VPN connection.
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
    -- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    --
    -- Default: @::\/0@
    localIpv6NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | Indicate whether to enable acceleration for the VPN connection.
    --
    -- Default: @false@
    enableAcceleration :: Prelude.Maybe Prelude.Bool,
    -- | The tunnel options for the VPN connection.
    tunnelOptions :: Prelude.Maybe [VpnTunnelOptionsSpecification],
    -- | The IPv4 CIDR on the AWS side of the VPN connection.
    --
    -- Default: @0.0.0.0\/0@
    remoteIpv4NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
    --
    -- Default: @ipv4@
    tunnelInsideIpVersion :: Prelude.Maybe TunnelInsideIpVersion,
    -- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    --
    -- Default: @0.0.0.0\/0@
    localIpv4NetworkCidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpnConnectionOptionsSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteIpv6NetworkCidr', 'vpnConnectionOptionsSpecification_remoteIpv6NetworkCidr' - The IPv6 CIDR on the AWS side of the VPN connection.
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
-- 'localIpv6NetworkCidr', 'vpnConnectionOptionsSpecification_localIpv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @::\/0@
--
-- 'enableAcceleration', 'vpnConnectionOptionsSpecification_enableAcceleration' - Indicate whether to enable acceleration for the VPN connection.
--
-- Default: @false@
--
-- 'tunnelOptions', 'vpnConnectionOptionsSpecification_tunnelOptions' - The tunnel options for the VPN connection.
--
-- 'remoteIpv4NetworkCidr', 'vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr' - The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0\/0@
--
-- 'tunnelInsideIpVersion', 'vpnConnectionOptionsSpecification_tunnelInsideIpVersion' - Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- Default: @ipv4@
--
-- 'localIpv4NetworkCidr', 'vpnConnectionOptionsSpecification_localIpv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @0.0.0.0\/0@
newVpnConnectionOptionsSpecification ::
  VpnConnectionOptionsSpecification
newVpnConnectionOptionsSpecification =
  VpnConnectionOptionsSpecification'
    { remoteIpv6NetworkCidr =
        Prelude.Nothing,
      staticRoutesOnly = Prelude.Nothing,
      localIpv6NetworkCidr = Prelude.Nothing,
      enableAcceleration = Prelude.Nothing,
      tunnelOptions = Prelude.Nothing,
      remoteIpv4NetworkCidr = Prelude.Nothing,
      tunnelInsideIpVersion = Prelude.Nothing,
      localIpv4NetworkCidr = Prelude.Nothing
    }

-- | The IPv6 CIDR on the AWS side of the VPN connection.
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

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @::\/0@
vpnConnectionOptionsSpecification_localIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnConnectionOptionsSpecification_localIpv6NetworkCidr = Lens.lens (\VpnConnectionOptionsSpecification' {localIpv6NetworkCidr} -> localIpv6NetworkCidr) (\s@VpnConnectionOptionsSpecification' {} a -> s {localIpv6NetworkCidr = a} :: VpnConnectionOptionsSpecification)

-- | Indicate whether to enable acceleration for the VPN connection.
--
-- Default: @false@
vpnConnectionOptionsSpecification_enableAcceleration :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Bool)
vpnConnectionOptionsSpecification_enableAcceleration = Lens.lens (\VpnConnectionOptionsSpecification' {enableAcceleration} -> enableAcceleration) (\s@VpnConnectionOptionsSpecification' {} a -> s {enableAcceleration = a} :: VpnConnectionOptionsSpecification)

-- | The tunnel options for the VPN connection.
vpnConnectionOptionsSpecification_tunnelOptions :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe [VpnTunnelOptionsSpecification])
vpnConnectionOptionsSpecification_tunnelOptions = Lens.lens (\VpnConnectionOptionsSpecification' {tunnelOptions} -> tunnelOptions) (\s@VpnConnectionOptionsSpecification' {} a -> s {tunnelOptions = a} :: VpnConnectionOptionsSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0\/0@
vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnConnectionOptionsSpecification_remoteIpv4NetworkCidr = Lens.lens (\VpnConnectionOptionsSpecification' {remoteIpv4NetworkCidr} -> remoteIpv4NetworkCidr) (\s@VpnConnectionOptionsSpecification' {} a -> s {remoteIpv4NetworkCidr = a} :: VpnConnectionOptionsSpecification)

-- | Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- Default: @ipv4@
vpnConnectionOptionsSpecification_tunnelInsideIpVersion :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe TunnelInsideIpVersion)
vpnConnectionOptionsSpecification_tunnelInsideIpVersion = Lens.lens (\VpnConnectionOptionsSpecification' {tunnelInsideIpVersion} -> tunnelInsideIpVersion) (\s@VpnConnectionOptionsSpecification' {} a -> s {tunnelInsideIpVersion = a} :: VpnConnectionOptionsSpecification)

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- Default: @0.0.0.0\/0@
vpnConnectionOptionsSpecification_localIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Prelude.Maybe Prelude.Text)
vpnConnectionOptionsSpecification_localIpv4NetworkCidr = Lens.lens (\VpnConnectionOptionsSpecification' {localIpv4NetworkCidr} -> localIpv4NetworkCidr) (\s@VpnConnectionOptionsSpecification' {} a -> s {localIpv4NetworkCidr = a} :: VpnConnectionOptionsSpecification)

instance
  Prelude.Hashable
    VpnConnectionOptionsSpecification

instance
  Prelude.NFData
    VpnConnectionOptionsSpecification

instance
  Prelude.ToQuery
    VpnConnectionOptionsSpecification
  where
  toQuery VpnConnectionOptionsSpecification' {..} =
    Prelude.mconcat
      [ "RemoteIpv6NetworkCidr"
          Prelude.=: remoteIpv6NetworkCidr,
        "StaticRoutesOnly" Prelude.=: staticRoutesOnly,
        "LocalIpv6NetworkCidr"
          Prelude.=: localIpv6NetworkCidr,
        "EnableAcceleration" Prelude.=: enableAcceleration,
        Prelude.toQuery
          ( Prelude.toQueryList "TunnelOptions"
              Prelude.<$> tunnelOptions
          ),
        "RemoteIpv4NetworkCidr"
          Prelude.=: remoteIpv4NetworkCidr,
        "TunnelInsideIpVersion"
          Prelude.=: tunnelInsideIpVersion,
        "LocalIpv4NetworkCidr"
          Prelude.=: localIpv4NetworkCidr
      ]
