{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.NetworkInterface
  ( NetworkInterface (..),

    -- * Smart constructor
    mkNetworkInterface,

    -- * Lenses
    niIPv6Address,
    niMACAddress,
    niIPv4Address,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a gateway's network interface.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
    ipv6Address :: Lude.Maybe Lude.Text,
    -- | The Media Access Control (MAC) address of the interface.
    mACAddress :: Lude.Maybe Lude.Text,
    -- | The Internet Protocol version 4 (IPv4) address of the interface.
    ipv4Address :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- * 'ipv6Address' - The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
-- * 'mACAddress' - The Media Access Control (MAC) address of the interface.
-- * 'ipv4Address' - The Internet Protocol version 4 (IPv4) address of the interface.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { ipv6Address = Lude.Nothing,
      mACAddress = Lude.Nothing,
      ipv4Address = Lude.Nothing
    }

-- | The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIPv6Address :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niIPv6Address = Lens.lens (ipv6Address :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Address = a} :: NetworkInterface)
{-# DEPRECATED niIPv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

-- | The Media Access Control (MAC) address of the interface.
--
-- /Note:/ Consider using 'mACAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niMACAddress :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niMACAddress = Lens.lens (mACAddress :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {mACAddress = a} :: NetworkInterface)
{-# DEPRECATED niMACAddress "Use generic-lens or generic-optics with 'mACAddress' instead." #-}

-- | The Internet Protocol version 4 (IPv4) address of the interface.
--
-- /Note:/ Consider using 'ipv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIPv4Address :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niIPv4Address = Lens.lens (ipv4Address :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {ipv4Address = a} :: NetworkInterface)
{-# DEPRECATED niIPv4Address "Use generic-lens or generic-optics with 'ipv4Address' instead." #-}

instance Lude.FromJSON NetworkInterface where
  parseJSON =
    Lude.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Lude.<$> (x Lude..:? "Ipv6Address")
            Lude.<*> (x Lude..:? "MacAddress")
            Lude.<*> (x Lude..:? "Ipv4Address")
      )
