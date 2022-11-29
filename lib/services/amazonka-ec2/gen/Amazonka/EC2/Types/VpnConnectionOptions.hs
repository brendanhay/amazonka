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
-- Module      : Amazonka.EC2.Types.VpnConnectionOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpnConnectionOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TunnelInsideIpVersion
import Amazonka.EC2.Types.TunnelOption
import qualified Amazonka.Prelude as Prelude

-- | Describes VPN connection options.
--
-- /See:/ 'newVpnConnectionOptions' smart constructor.
data VpnConnectionOptions = VpnConnectionOptions'
  { -- | The type of IPv4 address assigned to the outside interface of the
    -- customer gateway.
    --
    -- Valid values: @PrivateIpv4@ | @PublicIpv4@
    --
    -- Default: @PublicIpv4@
    outsideIpAddressType :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 CIDR on the Amazon Web Services side of the VPN connection.
    remoteIpv6NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    localIpv4NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | Indicates the VPN tunnel options.
    tunnelOptions :: Prelude.Maybe [TunnelOption],
    -- | Indicates whether the VPN connection uses static routes only. Static
    -- routes must be used for devices that don\'t support BGP.
    staticRoutesOnly :: Prelude.Maybe Prelude.Bool,
    -- | The IPv4 CIDR on the Amazon Web Services side of the VPN connection.
    remoteIpv4NetworkCidr :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
    tunnelInsideIpVersion :: Prelude.Maybe TunnelInsideIpVersion,
    -- | The transit gateway attachment ID in use for the VPN tunnel.
    transportTransitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether acceleration is enabled for the VPN connection.
    enableAcceleration :: Prelude.Maybe Prelude.Bool,
    -- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
    -- connection.
    localIpv6NetworkCidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpnConnectionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outsideIpAddressType', 'vpnConnectionOptions_outsideIpAddressType' - The type of IPv4 address assigned to the outside interface of the
-- customer gateway.
--
-- Valid values: @PrivateIpv4@ | @PublicIpv4@
--
-- Default: @PublicIpv4@
--
-- 'remoteIpv6NetworkCidr', 'vpnConnectionOptions_remoteIpv6NetworkCidr' - The IPv6 CIDR on the Amazon Web Services side of the VPN connection.
--
-- 'localIpv4NetworkCidr', 'vpnConnectionOptions_localIpv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
--
-- 'tunnelOptions', 'vpnConnectionOptions_tunnelOptions' - Indicates the VPN tunnel options.
--
-- 'staticRoutesOnly', 'vpnConnectionOptions_staticRoutesOnly' - Indicates whether the VPN connection uses static routes only. Static
-- routes must be used for devices that don\'t support BGP.
--
-- 'remoteIpv4NetworkCidr', 'vpnConnectionOptions_remoteIpv4NetworkCidr' - The IPv4 CIDR on the Amazon Web Services side of the VPN connection.
--
-- 'tunnelInsideIpVersion', 'vpnConnectionOptions_tunnelInsideIpVersion' - Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- 'transportTransitGatewayAttachmentId', 'vpnConnectionOptions_transportTransitGatewayAttachmentId' - The transit gateway attachment ID in use for the VPN tunnel.
--
-- 'enableAcceleration', 'vpnConnectionOptions_enableAcceleration' - Indicates whether acceleration is enabled for the VPN connection.
--
-- 'localIpv6NetworkCidr', 'vpnConnectionOptions_localIpv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
newVpnConnectionOptions ::
  VpnConnectionOptions
newVpnConnectionOptions =
  VpnConnectionOptions'
    { outsideIpAddressType =
        Prelude.Nothing,
      remoteIpv6NetworkCidr = Prelude.Nothing,
      localIpv4NetworkCidr = Prelude.Nothing,
      tunnelOptions = Prelude.Nothing,
      staticRoutesOnly = Prelude.Nothing,
      remoteIpv4NetworkCidr = Prelude.Nothing,
      tunnelInsideIpVersion = Prelude.Nothing,
      transportTransitGatewayAttachmentId =
        Prelude.Nothing,
      enableAcceleration = Prelude.Nothing,
      localIpv6NetworkCidr = Prelude.Nothing
    }

-- | The type of IPv4 address assigned to the outside interface of the
-- customer gateway.
--
-- Valid values: @PrivateIpv4@ | @PublicIpv4@
--
-- Default: @PublicIpv4@
vpnConnectionOptions_outsideIpAddressType :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe Prelude.Text)
vpnConnectionOptions_outsideIpAddressType = Lens.lens (\VpnConnectionOptions' {outsideIpAddressType} -> outsideIpAddressType) (\s@VpnConnectionOptions' {} a -> s {outsideIpAddressType = a} :: VpnConnectionOptions)

-- | The IPv6 CIDR on the Amazon Web Services side of the VPN connection.
vpnConnectionOptions_remoteIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe Prelude.Text)
vpnConnectionOptions_remoteIpv6NetworkCidr = Lens.lens (\VpnConnectionOptions' {remoteIpv6NetworkCidr} -> remoteIpv6NetworkCidr) (\s@VpnConnectionOptions' {} a -> s {remoteIpv6NetworkCidr = a} :: VpnConnectionOptions)

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
vpnConnectionOptions_localIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe Prelude.Text)
vpnConnectionOptions_localIpv4NetworkCidr = Lens.lens (\VpnConnectionOptions' {localIpv4NetworkCidr} -> localIpv4NetworkCidr) (\s@VpnConnectionOptions' {} a -> s {localIpv4NetworkCidr = a} :: VpnConnectionOptions)

-- | Indicates the VPN tunnel options.
vpnConnectionOptions_tunnelOptions :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe [TunnelOption])
vpnConnectionOptions_tunnelOptions = Lens.lens (\VpnConnectionOptions' {tunnelOptions} -> tunnelOptions) (\s@VpnConnectionOptions' {} a -> s {tunnelOptions = a} :: VpnConnectionOptions) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the VPN connection uses static routes only. Static
-- routes must be used for devices that don\'t support BGP.
vpnConnectionOptions_staticRoutesOnly :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe Prelude.Bool)
vpnConnectionOptions_staticRoutesOnly = Lens.lens (\VpnConnectionOptions' {staticRoutesOnly} -> staticRoutesOnly) (\s@VpnConnectionOptions' {} a -> s {staticRoutesOnly = a} :: VpnConnectionOptions)

-- | The IPv4 CIDR on the Amazon Web Services side of the VPN connection.
vpnConnectionOptions_remoteIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe Prelude.Text)
vpnConnectionOptions_remoteIpv4NetworkCidr = Lens.lens (\VpnConnectionOptions' {remoteIpv4NetworkCidr} -> remoteIpv4NetworkCidr) (\s@VpnConnectionOptions' {} a -> s {remoteIpv4NetworkCidr = a} :: VpnConnectionOptions)

-- | Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
vpnConnectionOptions_tunnelInsideIpVersion :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe TunnelInsideIpVersion)
vpnConnectionOptions_tunnelInsideIpVersion = Lens.lens (\VpnConnectionOptions' {tunnelInsideIpVersion} -> tunnelInsideIpVersion) (\s@VpnConnectionOptions' {} a -> s {tunnelInsideIpVersion = a} :: VpnConnectionOptions)

-- | The transit gateway attachment ID in use for the VPN tunnel.
vpnConnectionOptions_transportTransitGatewayAttachmentId :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe Prelude.Text)
vpnConnectionOptions_transportTransitGatewayAttachmentId = Lens.lens (\VpnConnectionOptions' {transportTransitGatewayAttachmentId} -> transportTransitGatewayAttachmentId) (\s@VpnConnectionOptions' {} a -> s {transportTransitGatewayAttachmentId = a} :: VpnConnectionOptions)

-- | Indicates whether acceleration is enabled for the VPN connection.
vpnConnectionOptions_enableAcceleration :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe Prelude.Bool)
vpnConnectionOptions_enableAcceleration = Lens.lens (\VpnConnectionOptions' {enableAcceleration} -> enableAcceleration) (\s@VpnConnectionOptions' {} a -> s {enableAcceleration = a} :: VpnConnectionOptions)

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN
-- connection.
vpnConnectionOptions_localIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptions (Prelude.Maybe Prelude.Text)
vpnConnectionOptions_localIpv6NetworkCidr = Lens.lens (\VpnConnectionOptions' {localIpv6NetworkCidr} -> localIpv6NetworkCidr) (\s@VpnConnectionOptions' {} a -> s {localIpv6NetworkCidr = a} :: VpnConnectionOptions)

instance Core.FromXML VpnConnectionOptions where
  parseXML x =
    VpnConnectionOptions'
      Prelude.<$> (x Core..@? "outsideIpAddressType")
      Prelude.<*> (x Core..@? "remoteIpv6NetworkCidr")
      Prelude.<*> (x Core..@? "localIpv4NetworkCidr")
      Prelude.<*> ( x Core..@? "tunnelOptionSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "staticRoutesOnly")
      Prelude.<*> (x Core..@? "remoteIpv4NetworkCidr")
      Prelude.<*> (x Core..@? "tunnelInsideIpVersion")
      Prelude.<*> (x Core..@? "transportTransitGatewayAttachmentId")
      Prelude.<*> (x Core..@? "enableAcceleration")
      Prelude.<*> (x Core..@? "localIpv6NetworkCidr")

instance Prelude.Hashable VpnConnectionOptions where
  hashWithSalt _salt VpnConnectionOptions' {..} =
    _salt `Prelude.hashWithSalt` outsideIpAddressType
      `Prelude.hashWithSalt` remoteIpv6NetworkCidr
      `Prelude.hashWithSalt` localIpv4NetworkCidr
      `Prelude.hashWithSalt` tunnelOptions
      `Prelude.hashWithSalt` staticRoutesOnly
      `Prelude.hashWithSalt` remoteIpv4NetworkCidr
      `Prelude.hashWithSalt` tunnelInsideIpVersion
      `Prelude.hashWithSalt` transportTransitGatewayAttachmentId
      `Prelude.hashWithSalt` enableAcceleration
      `Prelude.hashWithSalt` localIpv6NetworkCidr

instance Prelude.NFData VpnConnectionOptions where
  rnf VpnConnectionOptions' {..} =
    Prelude.rnf outsideIpAddressType
      `Prelude.seq` Prelude.rnf remoteIpv6NetworkCidr
      `Prelude.seq` Prelude.rnf localIpv4NetworkCidr
      `Prelude.seq` Prelude.rnf tunnelOptions
      `Prelude.seq` Prelude.rnf staticRoutesOnly
      `Prelude.seq` Prelude.rnf remoteIpv4NetworkCidr
      `Prelude.seq` Prelude.rnf tunnelInsideIpVersion
      `Prelude.seq` Prelude.rnf transportTransitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf enableAcceleration
      `Prelude.seq` Prelude.rnf localIpv6NetworkCidr
