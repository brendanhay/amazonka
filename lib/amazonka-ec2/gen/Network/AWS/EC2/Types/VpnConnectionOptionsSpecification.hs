{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpnConnectionOptionsSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpnConnectionOptionsSpecification
  ( VpnConnectionOptionsSpecification (..)
  -- * Smart constructor
  , mkVpnConnectionOptionsSpecification
  -- * Lenses
  , vcosEnableAcceleration
  , vcosLocalIpv4NetworkCidr
  , vcosLocalIpv6NetworkCidr
  , vcosRemoteIpv4NetworkCidr
  , vcosRemoteIpv6NetworkCidr
  , vcosStaticRoutesOnly
  , vcosTunnelInsideIpVersion
  , vcosTunnelOptions
  ) where

import qualified Network.AWS.EC2.Types.TunnelInsideIpVersion as Types
import qualified Network.AWS.EC2.Types.VpnTunnelOptionsSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes VPN connection options.
--
-- /See:/ 'mkVpnConnectionOptionsSpecification' smart constructor.
data VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification'
  { enableAcceleration :: Core.Maybe Core.Bool
    -- ^ Indicate whether to enable acceleration for the VPN connection.
--
-- Default: @false@ 
  , localIpv4NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @0.0.0.0/0@ 
  , localIpv6NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @::/0@ 
  , remoteIpv4NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0/0@ 
  , remoteIpv6NetworkCidr :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::/0@ 
  , staticRoutesOnly :: Core.Maybe Core.Bool
    -- ^ Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route.
--
-- Default: @false@ 
  , tunnelInsideIpVersion :: Core.Maybe Types.TunnelInsideIpVersion
    -- ^ Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- Default: @ipv4@ 
  , tunnelOptions :: Core.Maybe [Types.VpnTunnelOptionsSpecification]
    -- ^ The tunnel options for the VPN connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpnConnectionOptionsSpecification' value with any optional fields omitted.
mkVpnConnectionOptionsSpecification
    :: VpnConnectionOptionsSpecification
mkVpnConnectionOptionsSpecification
  = VpnConnectionOptionsSpecification'{enableAcceleration =
                                         Core.Nothing,
                                       localIpv4NetworkCidr = Core.Nothing,
                                       localIpv6NetworkCidr = Core.Nothing,
                                       remoteIpv4NetworkCidr = Core.Nothing,
                                       remoteIpv6NetworkCidr = Core.Nothing,
                                       staticRoutesOnly = Core.Nothing,
                                       tunnelInsideIpVersion = Core.Nothing,
                                       tunnelOptions = Core.Nothing}

-- | Indicate whether to enable acceleration for the VPN connection.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'enableAcceleration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosEnableAcceleration :: Lens.Lens' VpnConnectionOptionsSpecification (Core.Maybe Core.Bool)
vcosEnableAcceleration = Lens.field @"enableAcceleration"
{-# INLINEABLE vcosEnableAcceleration #-}
{-# DEPRECATED enableAcceleration "Use generic-lens or generic-optics with 'enableAcceleration' instead"  #-}

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @0.0.0.0/0@ 
--
-- /Note:/ Consider using 'localIpv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosLocalIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Core.Maybe Core.Text)
vcosLocalIpv4NetworkCidr = Lens.field @"localIpv4NetworkCidr"
{-# INLINEABLE vcosLocalIpv4NetworkCidr #-}
{-# DEPRECATED localIpv4NetworkCidr "Use generic-lens or generic-optics with 'localIpv4NetworkCidr' instead"  #-}

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @::/0@ 
--
-- /Note:/ Consider using 'localIpv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosLocalIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Core.Maybe Core.Text)
vcosLocalIpv6NetworkCidr = Lens.field @"localIpv6NetworkCidr"
{-# INLINEABLE vcosLocalIpv6NetworkCidr #-}
{-# DEPRECATED localIpv6NetworkCidr "Use generic-lens or generic-optics with 'localIpv6NetworkCidr' instead"  #-}

-- | The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0/0@ 
--
-- /Note:/ Consider using 'remoteIpv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosRemoteIpv4NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Core.Maybe Core.Text)
vcosRemoteIpv4NetworkCidr = Lens.field @"remoteIpv4NetworkCidr"
{-# INLINEABLE vcosRemoteIpv4NetworkCidr #-}
{-# DEPRECATED remoteIpv4NetworkCidr "Use generic-lens or generic-optics with 'remoteIpv4NetworkCidr' instead"  #-}

-- | The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::/0@ 
--
-- /Note:/ Consider using 'remoteIpv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosRemoteIpv6NetworkCidr :: Lens.Lens' VpnConnectionOptionsSpecification (Core.Maybe Core.Text)
vcosRemoteIpv6NetworkCidr = Lens.field @"remoteIpv6NetworkCidr"
{-# INLINEABLE vcosRemoteIpv6NetworkCidr #-}
{-# DEPRECATED remoteIpv6NetworkCidr "Use generic-lens or generic-optics with 'remoteIpv6NetworkCidr' instead"  #-}

-- | Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'staticRoutesOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosStaticRoutesOnly :: Lens.Lens' VpnConnectionOptionsSpecification (Core.Maybe Core.Bool)
vcosStaticRoutesOnly = Lens.field @"staticRoutesOnly"
{-# INLINEABLE vcosStaticRoutesOnly #-}
{-# DEPRECATED staticRoutesOnly "Use generic-lens or generic-optics with 'staticRoutesOnly' instead"  #-}

-- | Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- Default: @ipv4@ 
--
-- /Note:/ Consider using 'tunnelInsideIpVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosTunnelInsideIpVersion :: Lens.Lens' VpnConnectionOptionsSpecification (Core.Maybe Types.TunnelInsideIpVersion)
vcosTunnelInsideIpVersion = Lens.field @"tunnelInsideIpVersion"
{-# INLINEABLE vcosTunnelInsideIpVersion #-}
{-# DEPRECATED tunnelInsideIpVersion "Use generic-lens or generic-optics with 'tunnelInsideIpVersion' instead"  #-}

-- | The tunnel options for the VPN connection.
--
-- /Note:/ Consider using 'tunnelOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosTunnelOptions :: Lens.Lens' VpnConnectionOptionsSpecification (Core.Maybe [Types.VpnTunnelOptionsSpecification])
vcosTunnelOptions = Lens.field @"tunnelOptions"
{-# INLINEABLE vcosTunnelOptions #-}
{-# DEPRECATED tunnelOptions "Use generic-lens or generic-optics with 'tunnelOptions' instead"  #-}

instance Core.ToQuery VpnConnectionOptionsSpecification where
        toQuery VpnConnectionOptionsSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "EnableAcceleration")
              enableAcceleration
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LocalIpv4NetworkCidr")
                localIpv4NetworkCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LocalIpv6NetworkCidr")
                localIpv6NetworkCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RemoteIpv4NetworkCidr")
                remoteIpv4NetworkCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RemoteIpv6NetworkCidr")
                remoteIpv6NetworkCidr
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StaticRoutesOnly")
                staticRoutesOnly
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TunnelInsideIpVersion")
                tunnelInsideIpVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TunnelOptions")
                tunnelOptions
