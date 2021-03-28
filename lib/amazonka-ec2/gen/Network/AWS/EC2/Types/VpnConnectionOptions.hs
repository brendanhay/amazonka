{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpnConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpnConnectionOptions
  ( VpnConnectionOptions (..)
  -- * Smart constructor
  , mkVpnConnectionOptions
  -- * Lenses
  , vcoEnableAcceleration
  , vcoLocalIpv4NetworkCidr
  , vcoLocalIpv6NetworkCidr
  , vcoRemoteIpv4NetworkCidr
  , vcoRemoteIpv6NetworkCidr
  , vcoStaticRoutesOnly
  , vcoTunnelInsideIpVersion
  , vcoTunnelOptions
  ) where

import qualified Network.AWS.EC2.Types.TunnelInsideIpVersion as Types
import qualified Network.AWS.EC2.Types.TunnelOption as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes VPN connection options.
--
-- /See:/ 'mkVpnConnectionOptions' smart constructor.
data VpnConnectionOptions = VpnConnectionOptions'
  { enableAcceleration :: Core.Maybe Core.Bool
    -- ^ Indicates whether acceleration is enabled for the VPN connection.
  , localIpv4NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
  , localIpv6NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
  , remoteIpv4NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR on the AWS side of the VPN connection.
  , remoteIpv6NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR on the AWS side of the VPN connection.
  , staticRoutesOnly :: Core.Maybe Core.Bool
    -- ^ Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
  , tunnelInsideIpVersion :: Core.Maybe Types.TunnelInsideIpVersion
    -- ^ Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
  , tunnelOptions :: Core.Maybe [Types.TunnelOption]
    -- ^ Indicates the VPN tunnel options.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpnConnectionOptions' value with any optional fields omitted.
mkVpnConnectionOptions
    :: VpnConnectionOptions
mkVpnConnectionOptions
  = VpnConnectionOptions'{enableAcceleration = Core.Nothing,
                          localIpv4NetworkCidr = Core.Nothing,
                          localIpv6NetworkCidr = Core.Nothing,
                          remoteIpv4NetworkCidr = Core.Nothing,
                          remoteIpv6NetworkCidr = Core.Nothing,
                          staticRoutesOnly = Core.Nothing,
                          tunnelInsideIpVersion = Core.Nothing, tunnelOptions = Core.Nothing}

-- | Indicates whether acceleration is enabled for the VPN connection.
--
-- /Note:/ Consider using 'enableAcceleration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoEnableAcceleration :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Bool)
vcoEnableAcceleration = Lens.field @"enableAcceleration"
{-# INLINEABLE vcoEnableAcceleration #-}
{-# DEPRECATED enableAcceleration "Use generic-lens or generic-optics with 'enableAcceleration' instead"  #-}

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- /Note:/ Consider using 'localIpv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoLocalIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Text)
vcoLocalIpv4NetworkCidr = Lens.field @"localIpv4NetworkCidr"
{-# INLINEABLE vcoLocalIpv4NetworkCidr #-}
{-# DEPRECATED localIpv4NetworkCidr "Use generic-lens or generic-optics with 'localIpv4NetworkCidr' instead"  #-}

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- /Note:/ Consider using 'localIpv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoLocalIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Text)
vcoLocalIpv6NetworkCidr = Lens.field @"localIpv6NetworkCidr"
{-# INLINEABLE vcoLocalIpv6NetworkCidr #-}
{-# DEPRECATED localIpv6NetworkCidr "Use generic-lens or generic-optics with 'localIpv6NetworkCidr' instead"  #-}

-- | The IPv4 CIDR on the AWS side of the VPN connection.
--
-- /Note:/ Consider using 'remoteIpv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoRemoteIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Text)
vcoRemoteIpv4NetworkCidr = Lens.field @"remoteIpv4NetworkCidr"
{-# INLINEABLE vcoRemoteIpv4NetworkCidr #-}
{-# DEPRECATED remoteIpv4NetworkCidr "Use generic-lens or generic-optics with 'remoteIpv4NetworkCidr' instead"  #-}

-- | The IPv6 CIDR on the AWS side of the VPN connection.
--
-- /Note:/ Consider using 'remoteIpv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoRemoteIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Text)
vcoRemoteIpv6NetworkCidr = Lens.field @"remoteIpv6NetworkCidr"
{-# INLINEABLE vcoRemoteIpv6NetworkCidr #-}
{-# DEPRECATED remoteIpv6NetworkCidr "Use generic-lens or generic-optics with 'remoteIpv6NetworkCidr' instead"  #-}

-- | Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
--
-- /Note:/ Consider using 'staticRoutesOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoStaticRoutesOnly :: Lens.Lens' VpnConnectionOptions (Core.Maybe Core.Bool)
vcoStaticRoutesOnly = Lens.field @"staticRoutesOnly"
{-# INLINEABLE vcoStaticRoutesOnly #-}
{-# DEPRECATED staticRoutesOnly "Use generic-lens or generic-optics with 'staticRoutesOnly' instead"  #-}

-- | Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- /Note:/ Consider using 'tunnelInsideIpVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoTunnelInsideIpVersion :: Lens.Lens' VpnConnectionOptions (Core.Maybe Types.TunnelInsideIpVersion)
vcoTunnelInsideIpVersion = Lens.field @"tunnelInsideIpVersion"
{-# INLINEABLE vcoTunnelInsideIpVersion #-}
{-# DEPRECATED tunnelInsideIpVersion "Use generic-lens or generic-optics with 'tunnelInsideIpVersion' instead"  #-}

-- | Indicates the VPN tunnel options.
--
-- /Note:/ Consider using 'tunnelOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoTunnelOptions :: Lens.Lens' VpnConnectionOptions (Core.Maybe [Types.TunnelOption])
vcoTunnelOptions = Lens.field @"tunnelOptions"
{-# INLINEABLE vcoTunnelOptions #-}
{-# DEPRECATED tunnelOptions "Use generic-lens or generic-optics with 'tunnelOptions' instead"  #-}

instance Core.FromXML VpnConnectionOptions where
        parseXML x
          = VpnConnectionOptions' Core.<$>
              (x Core..@? "enableAcceleration") Core.<*>
                x Core..@? "localIpv4NetworkCidr"
                Core.<*> x Core..@? "localIpv6NetworkCidr"
                Core.<*> x Core..@? "remoteIpv4NetworkCidr"
                Core.<*> x Core..@? "remoteIpv6NetworkCidr"
                Core.<*> x Core..@? "staticRoutesOnly"
                Core.<*> x Core..@? "tunnelInsideIpVersion"
                Core.<*>
                x Core..@? "tunnelOptionSet" Core..<@> Core.parseXMLList "item"
