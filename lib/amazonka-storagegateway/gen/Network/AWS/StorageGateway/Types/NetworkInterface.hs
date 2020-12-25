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
    niIpv4Address,
    niIpv6Address,
    niMacAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.String as Types

-- | Describes a gateway's network interface.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The Internet Protocol version 4 (IPv4) address of the interface.
    ipv4Address :: Core.Maybe Types.String,
    -- | The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
    ipv6Address :: Core.Maybe Types.String,
    -- | The Media Access Control (MAC) address of the interface.
    macAddress :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterface' value with any optional fields omitted.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { ipv4Address = Core.Nothing,
      ipv6Address = Core.Nothing,
      macAddress = Core.Nothing
    }

-- | The Internet Protocol version 4 (IPv4) address of the interface.
--
-- /Note:/ Consider using 'ipv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv4Address :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niIpv4Address = Lens.field @"ipv4Address"
{-# DEPRECATED niIpv4Address "Use generic-lens or generic-optics with 'ipv4Address' instead." #-}

-- | The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6Address :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niIpv6Address = Lens.field @"ipv6Address"
{-# DEPRECATED niIpv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

-- | The Media Access Control (MAC) address of the interface.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niMacAddress :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niMacAddress = Lens.field @"macAddress"
{-# DEPRECATED niMacAddress "Use generic-lens or generic-optics with 'macAddress' instead." #-}

instance Core.FromJSON NetworkInterface where
  parseJSON =
    Core.withObject "NetworkInterface" Core.$
      \x ->
        NetworkInterface'
          Core.<$> (x Core..:? "Ipv4Address")
          Core.<*> (x Core..:? "Ipv6Address")
          Core.<*> (x Core..:? "MacAddress")
