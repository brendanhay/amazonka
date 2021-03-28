{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.NetworkInterface
  ( NetworkInterface (..)
  -- * Smart constructor
  , mkNetworkInterface
  -- * Lenses
  , niIpv4Address
  , niIpv6Address
  , niMacAddress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a gateway's network interface.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { ipv4Address :: Core.Maybe Core.Text
    -- ^ The Internet Protocol version 4 (IPv4) address of the interface.
  , ipv6Address :: Core.Maybe Core.Text
    -- ^ The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
  , macAddress :: Core.Maybe Core.Text
    -- ^ The Media Access Control (MAC) address of the interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterface' value with any optional fields omitted.
mkNetworkInterface
    :: NetworkInterface
mkNetworkInterface
  = NetworkInterface'{ipv4Address = Core.Nothing,
                      ipv6Address = Core.Nothing, macAddress = Core.Nothing}

-- | The Internet Protocol version 4 (IPv4) address of the interface.
--
-- /Note:/ Consider using 'ipv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv4Address :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
niIpv4Address = Lens.field @"ipv4Address"
{-# INLINEABLE niIpv4Address #-}
{-# DEPRECATED ipv4Address "Use generic-lens or generic-optics with 'ipv4Address' instead"  #-}

-- | The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6Address :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
niIpv6Address = Lens.field @"ipv6Address"
{-# INLINEABLE niIpv6Address #-}
{-# DEPRECATED ipv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead"  #-}

-- | The Media Access Control (MAC) address of the interface.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niMacAddress :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
niMacAddress = Lens.field @"macAddress"
{-# INLINEABLE niMacAddress #-}
{-# DEPRECATED macAddress "Use generic-lens or generic-optics with 'macAddress' instead"  #-}

instance Core.FromJSON NetworkInterface where
        parseJSON
          = Core.withObject "NetworkInterface" Core.$
              \ x ->
                NetworkInterface' Core.<$>
                  (x Core..:? "Ipv4Address") Core.<*> x Core..:? "Ipv6Address"
                    Core.<*> x Core..:? "MacAddress"
