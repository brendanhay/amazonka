-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNConnectionOptionsSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNConnectionOptionsSpecification
  ( VPNConnectionOptionsSpecification (..),

    -- * Smart constructor
    mkVPNConnectionOptionsSpecification,

    -- * Lenses
    vcosTunnelInsideIPVersion,
    vcosRemoteIPv4NetworkCidr,
    vcosEnableAcceleration,
    vcosLocalIPv4NetworkCidr,
    vcosRemoteIPv6NetworkCidr,
    vcosTunnelOptions,
    vcosLocalIPv6NetworkCidr,
    vcosStaticRoutesOnly,
  )
where

import Network.AWS.EC2.Types.TunnelInsideIPVersion
import Network.AWS.EC2.Types.VPNTunnelOptionsSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes VPN connection options.
--
-- /See:/ 'mkVPNConnectionOptionsSpecification' smart constructor.
data VPNConnectionOptionsSpecification = VPNConnectionOptionsSpecification'
  { tunnelInsideIPVersion ::
      Lude.Maybe
        TunnelInsideIPVersion,
    remoteIPv4NetworkCidr ::
      Lude.Maybe Lude.Text,
    enableAcceleration ::
      Lude.Maybe Lude.Bool,
    localIPv4NetworkCidr ::
      Lude.Maybe Lude.Text,
    remoteIPv6NetworkCidr ::
      Lude.Maybe Lude.Text,
    tunnelOptions ::
      Lude.Maybe
        [VPNTunnelOptionsSpecification],
    localIPv6NetworkCidr ::
      Lude.Maybe Lude.Text,
    staticRoutesOnly ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPNConnectionOptionsSpecification' with the minimum fields required to make a request.
--
-- * 'enableAcceleration' - Indicate whether to enable acceleration for the VPN connection.
--
-- Default: @false@
-- * 'localIPv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @0.0.0.0/0@
-- * 'localIPv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @::/0@
-- * 'remoteIPv4NetworkCidr' - The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0/0@
-- * 'remoteIPv6NetworkCidr' - The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::/0@
-- * 'staticRoutesOnly' - Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route.
--
-- Default: @false@
-- * 'tunnelInsideIPVersion' - Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- Default: @ipv4@
-- * 'tunnelOptions' - The tunnel options for the VPN connection.
mkVPNConnectionOptionsSpecification ::
  VPNConnectionOptionsSpecification
mkVPNConnectionOptionsSpecification =
  VPNConnectionOptionsSpecification'
    { tunnelInsideIPVersion =
        Lude.Nothing,
      remoteIPv4NetworkCidr = Lude.Nothing,
      enableAcceleration = Lude.Nothing,
      localIPv4NetworkCidr = Lude.Nothing,
      remoteIPv6NetworkCidr = Lude.Nothing,
      tunnelOptions = Lude.Nothing,
      localIPv6NetworkCidr = Lude.Nothing,
      staticRoutesOnly = Lude.Nothing
    }

-- | Indicate whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- Default: @ipv4@
--
-- /Note:/ Consider using 'tunnelInsideIPVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosTunnelInsideIPVersion :: Lens.Lens' VPNConnectionOptionsSpecification (Lude.Maybe TunnelInsideIPVersion)
vcosTunnelInsideIPVersion = Lens.lens (tunnelInsideIPVersion :: VPNConnectionOptionsSpecification -> Lude.Maybe TunnelInsideIPVersion) (\s a -> s {tunnelInsideIPVersion = a} :: VPNConnectionOptionsSpecification)
{-# DEPRECATED vcosTunnelInsideIPVersion "Use generic-lens or generic-optics with 'tunnelInsideIPVersion' instead." #-}

-- | The IPv4 CIDR on the AWS side of the VPN connection.
--
-- Default: @0.0.0.0/0@
--
-- /Note:/ Consider using 'remoteIPv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosRemoteIPv4NetworkCidr :: Lens.Lens' VPNConnectionOptionsSpecification (Lude.Maybe Lude.Text)
vcosRemoteIPv4NetworkCidr = Lens.lens (remoteIPv4NetworkCidr :: VPNConnectionOptionsSpecification -> Lude.Maybe Lude.Text) (\s a -> s {remoteIPv4NetworkCidr = a} :: VPNConnectionOptionsSpecification)
{-# DEPRECATED vcosRemoteIPv4NetworkCidr "Use generic-lens or generic-optics with 'remoteIPv4NetworkCidr' instead." #-}

-- | Indicate whether to enable acceleration for the VPN connection.
--
-- Default: @false@
--
-- /Note:/ Consider using 'enableAcceleration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosEnableAcceleration :: Lens.Lens' VPNConnectionOptionsSpecification (Lude.Maybe Lude.Bool)
vcosEnableAcceleration = Lens.lens (enableAcceleration :: VPNConnectionOptionsSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {enableAcceleration = a} :: VPNConnectionOptionsSpecification)
{-# DEPRECATED vcosEnableAcceleration "Use generic-lens or generic-optics with 'enableAcceleration' instead." #-}

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @0.0.0.0/0@
--
-- /Note:/ Consider using 'localIPv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosLocalIPv4NetworkCidr :: Lens.Lens' VPNConnectionOptionsSpecification (Lude.Maybe Lude.Text)
vcosLocalIPv4NetworkCidr = Lens.lens (localIPv4NetworkCidr :: VPNConnectionOptionsSpecification -> Lude.Maybe Lude.Text) (\s a -> s {localIPv4NetworkCidr = a} :: VPNConnectionOptionsSpecification)
{-# DEPRECATED vcosLocalIPv4NetworkCidr "Use generic-lens or generic-optics with 'localIPv4NetworkCidr' instead." #-}

-- | The IPv6 CIDR on the AWS side of the VPN connection.
--
-- Default: @::/0@
--
-- /Note:/ Consider using 'remoteIPv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosRemoteIPv6NetworkCidr :: Lens.Lens' VPNConnectionOptionsSpecification (Lude.Maybe Lude.Text)
vcosRemoteIPv6NetworkCidr = Lens.lens (remoteIPv6NetworkCidr :: VPNConnectionOptionsSpecification -> Lude.Maybe Lude.Text) (\s a -> s {remoteIPv6NetworkCidr = a} :: VPNConnectionOptionsSpecification)
{-# DEPRECATED vcosRemoteIPv6NetworkCidr "Use generic-lens or generic-optics with 'remoteIPv6NetworkCidr' instead." #-}

-- | The tunnel options for the VPN connection.
--
-- /Note:/ Consider using 'tunnelOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosTunnelOptions :: Lens.Lens' VPNConnectionOptionsSpecification (Lude.Maybe [VPNTunnelOptionsSpecification])
vcosTunnelOptions = Lens.lens (tunnelOptions :: VPNConnectionOptionsSpecification -> Lude.Maybe [VPNTunnelOptionsSpecification]) (\s a -> s {tunnelOptions = a} :: VPNConnectionOptionsSpecification)
{-# DEPRECATED vcosTunnelOptions "Use generic-lens or generic-optics with 'tunnelOptions' instead." #-}

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- Default: @::/0@
--
-- /Note:/ Consider using 'localIPv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosLocalIPv6NetworkCidr :: Lens.Lens' VPNConnectionOptionsSpecification (Lude.Maybe Lude.Text)
vcosLocalIPv6NetworkCidr = Lens.lens (localIPv6NetworkCidr :: VPNConnectionOptionsSpecification -> Lude.Maybe Lude.Text) (\s a -> s {localIPv6NetworkCidr = a} :: VPNConnectionOptionsSpecification)
{-# DEPRECATED vcosLocalIPv6NetworkCidr "Use generic-lens or generic-optics with 'localIPv6NetworkCidr' instead." #-}

-- | Indicate whether the VPN connection uses static routes only. If you are creating a VPN connection for a device that does not support BGP, you must specify @true@ . Use 'CreateVpnConnectionRoute' to create a static route.
--
-- Default: @false@
--
-- /Note:/ Consider using 'staticRoutesOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcosStaticRoutesOnly :: Lens.Lens' VPNConnectionOptionsSpecification (Lude.Maybe Lude.Bool)
vcosStaticRoutesOnly = Lens.lens (staticRoutesOnly :: VPNConnectionOptionsSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {staticRoutesOnly = a} :: VPNConnectionOptionsSpecification)
{-# DEPRECATED vcosStaticRoutesOnly "Use generic-lens or generic-optics with 'staticRoutesOnly' instead." #-}

instance Lude.ToQuery VPNConnectionOptionsSpecification where
  toQuery VPNConnectionOptionsSpecification' {..} =
    Lude.mconcat
      [ "TunnelInsideIpVersion" Lude.=: tunnelInsideIPVersion,
        "RemoteIpv4NetworkCidr" Lude.=: remoteIPv4NetworkCidr,
        "EnableAcceleration" Lude.=: enableAcceleration,
        "LocalIpv4NetworkCidr" Lude.=: localIPv4NetworkCidr,
        "RemoteIpv6NetworkCidr" Lude.=: remoteIPv6NetworkCidr,
        Lude.toQuery
          (Lude.toQueryList "TunnelOptions" Lude.<$> tunnelOptions),
        "LocalIpv6NetworkCidr" Lude.=: localIPv6NetworkCidr,
        "StaticRoutesOnly" Lude.=: staticRoutesOnly
      ]
