-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNConnectionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNConnectionOptions
  ( VPNConnectionOptions (..),

    -- * Smart constructor
    mkVPNConnectionOptions,

    -- * Lenses
    vcoTunnelInsideIPVersion,
    vcoRemoteIPv4NetworkCidr,
    vcoEnableAcceleration,
    vcoLocalIPv4NetworkCidr,
    vcoRemoteIPv6NetworkCidr,
    vcoTunnelOptions,
    vcoLocalIPv6NetworkCidr,
    vcoStaticRoutesOnly,
  )
where

import Network.AWS.EC2.Types.TunnelInsideIPVersion
import Network.AWS.EC2.Types.TunnelOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes VPN connection options.
--
-- /See:/ 'mkVPNConnectionOptions' smart constructor.
data VPNConnectionOptions = VPNConnectionOptions'
  { tunnelInsideIPVersion ::
      Lude.Maybe TunnelInsideIPVersion,
    remoteIPv4NetworkCidr :: Lude.Maybe Lude.Text,
    enableAcceleration :: Lude.Maybe Lude.Bool,
    localIPv4NetworkCidr :: Lude.Maybe Lude.Text,
    remoteIPv6NetworkCidr :: Lude.Maybe Lude.Text,
    tunnelOptions :: Lude.Maybe [TunnelOption],
    localIPv6NetworkCidr :: Lude.Maybe Lude.Text,
    staticRoutesOnly :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPNConnectionOptions' with the minimum fields required to make a request.
--
-- * 'enableAcceleration' - Indicates whether acceleration is enabled for the VPN connection.
-- * 'localIPv4NetworkCidr' - The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
-- * 'localIPv6NetworkCidr' - The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
-- * 'remoteIPv4NetworkCidr' - The IPv4 CIDR on the AWS side of the VPN connection.
-- * 'remoteIPv6NetworkCidr' - The IPv6 CIDR on the AWS side of the VPN connection.
-- * 'staticRoutesOnly' - Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
-- * 'tunnelInsideIPVersion' - Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
-- * 'tunnelOptions' - Indicates the VPN tunnel options.
mkVPNConnectionOptions ::
  VPNConnectionOptions
mkVPNConnectionOptions =
  VPNConnectionOptions'
    { tunnelInsideIPVersion = Lude.Nothing,
      remoteIPv4NetworkCidr = Lude.Nothing,
      enableAcceleration = Lude.Nothing,
      localIPv4NetworkCidr = Lude.Nothing,
      remoteIPv6NetworkCidr = Lude.Nothing,
      tunnelOptions = Lude.Nothing,
      localIPv6NetworkCidr = Lude.Nothing,
      staticRoutesOnly = Lude.Nothing
    }

-- | Indicates whether the VPN tunnels process IPv4 or IPv6 traffic.
--
-- /Note:/ Consider using 'tunnelInsideIPVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoTunnelInsideIPVersion :: Lens.Lens' VPNConnectionOptions (Lude.Maybe TunnelInsideIPVersion)
vcoTunnelInsideIPVersion = Lens.lens (tunnelInsideIPVersion :: VPNConnectionOptions -> Lude.Maybe TunnelInsideIPVersion) (\s a -> s {tunnelInsideIPVersion = a} :: VPNConnectionOptions)
{-# DEPRECATED vcoTunnelInsideIPVersion "Use generic-lens or generic-optics with 'tunnelInsideIPVersion' instead." #-}

-- | The IPv4 CIDR on the AWS side of the VPN connection.
--
-- /Note:/ Consider using 'remoteIPv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoRemoteIPv4NetworkCidr :: Lens.Lens' VPNConnectionOptions (Lude.Maybe Lude.Text)
vcoRemoteIPv4NetworkCidr = Lens.lens (remoteIPv4NetworkCidr :: VPNConnectionOptions -> Lude.Maybe Lude.Text) (\s a -> s {remoteIPv4NetworkCidr = a} :: VPNConnectionOptions)
{-# DEPRECATED vcoRemoteIPv4NetworkCidr "Use generic-lens or generic-optics with 'remoteIPv4NetworkCidr' instead." #-}

-- | Indicates whether acceleration is enabled for the VPN connection.
--
-- /Note:/ Consider using 'enableAcceleration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoEnableAcceleration :: Lens.Lens' VPNConnectionOptions (Lude.Maybe Lude.Bool)
vcoEnableAcceleration = Lens.lens (enableAcceleration :: VPNConnectionOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enableAcceleration = a} :: VPNConnectionOptions)
{-# DEPRECATED vcoEnableAcceleration "Use generic-lens or generic-optics with 'enableAcceleration' instead." #-}

-- | The IPv4 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- /Note:/ Consider using 'localIPv4NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoLocalIPv4NetworkCidr :: Lens.Lens' VPNConnectionOptions (Lude.Maybe Lude.Text)
vcoLocalIPv4NetworkCidr = Lens.lens (localIPv4NetworkCidr :: VPNConnectionOptions -> Lude.Maybe Lude.Text) (\s a -> s {localIPv4NetworkCidr = a} :: VPNConnectionOptions)
{-# DEPRECATED vcoLocalIPv4NetworkCidr "Use generic-lens or generic-optics with 'localIPv4NetworkCidr' instead." #-}

-- | The IPv6 CIDR on the AWS side of the VPN connection.
--
-- /Note:/ Consider using 'remoteIPv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoRemoteIPv6NetworkCidr :: Lens.Lens' VPNConnectionOptions (Lude.Maybe Lude.Text)
vcoRemoteIPv6NetworkCidr = Lens.lens (remoteIPv6NetworkCidr :: VPNConnectionOptions -> Lude.Maybe Lude.Text) (\s a -> s {remoteIPv6NetworkCidr = a} :: VPNConnectionOptions)
{-# DEPRECATED vcoRemoteIPv6NetworkCidr "Use generic-lens or generic-optics with 'remoteIPv6NetworkCidr' instead." #-}

-- | Indicates the VPN tunnel options.
--
-- /Note:/ Consider using 'tunnelOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoTunnelOptions :: Lens.Lens' VPNConnectionOptions (Lude.Maybe [TunnelOption])
vcoTunnelOptions = Lens.lens (tunnelOptions :: VPNConnectionOptions -> Lude.Maybe [TunnelOption]) (\s a -> s {tunnelOptions = a} :: VPNConnectionOptions)
{-# DEPRECATED vcoTunnelOptions "Use generic-lens or generic-optics with 'tunnelOptions' instead." #-}

-- | The IPv6 CIDR on the customer gateway (on-premises) side of the VPN connection.
--
-- /Note:/ Consider using 'localIPv6NetworkCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoLocalIPv6NetworkCidr :: Lens.Lens' VPNConnectionOptions (Lude.Maybe Lude.Text)
vcoLocalIPv6NetworkCidr = Lens.lens (localIPv6NetworkCidr :: VPNConnectionOptions -> Lude.Maybe Lude.Text) (\s a -> s {localIPv6NetworkCidr = a} :: VPNConnectionOptions)
{-# DEPRECATED vcoLocalIPv6NetworkCidr "Use generic-lens or generic-optics with 'localIPv6NetworkCidr' instead." #-}

-- | Indicates whether the VPN connection uses static routes only. Static routes must be used for devices that don't support BGP.
--
-- /Note:/ Consider using 'staticRoutesOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcoStaticRoutesOnly :: Lens.Lens' VPNConnectionOptions (Lude.Maybe Lude.Bool)
vcoStaticRoutesOnly = Lens.lens (staticRoutesOnly :: VPNConnectionOptions -> Lude.Maybe Lude.Bool) (\s a -> s {staticRoutesOnly = a} :: VPNConnectionOptions)
{-# DEPRECATED vcoStaticRoutesOnly "Use generic-lens or generic-optics with 'staticRoutesOnly' instead." #-}

instance Lude.FromXML VPNConnectionOptions where
  parseXML x =
    VPNConnectionOptions'
      Lude.<$> (x Lude..@? "tunnelInsideIpVersion")
      Lude.<*> (x Lude..@? "remoteIpv4NetworkCidr")
      Lude.<*> (x Lude..@? "enableAcceleration")
      Lude.<*> (x Lude..@? "localIpv4NetworkCidr")
      Lude.<*> (x Lude..@? "remoteIpv6NetworkCidr")
      Lude.<*> ( x Lude..@? "tunnelOptionSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "localIpv6NetworkCidr")
      Lude.<*> (x Lude..@? "staticRoutesOnly")
