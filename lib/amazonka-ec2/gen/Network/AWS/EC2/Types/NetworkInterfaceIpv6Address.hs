{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceIpv6Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfaceIpv6Address
  ( NetworkInterfaceIpv6Address (..)
  -- * Smart constructor
  , mkNetworkInterfaceIpv6Address
  -- * Lenses
  , niiaIpv6Address
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv6 address associated with a network interface.
--
-- /See:/ 'mkNetworkInterfaceIpv6Address' smart constructor.
newtype NetworkInterfaceIpv6Address = NetworkInterfaceIpv6Address'
  { ipv6Address :: Core.Maybe Core.Text
    -- ^ The IPv6 address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterfaceIpv6Address' value with any optional fields omitted.
mkNetworkInterfaceIpv6Address
    :: NetworkInterfaceIpv6Address
mkNetworkInterfaceIpv6Address
  = NetworkInterfaceIpv6Address'{ipv6Address = Core.Nothing}

-- | The IPv6 address.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niiaIpv6Address :: Lens.Lens' NetworkInterfaceIpv6Address (Core.Maybe Core.Text)
niiaIpv6Address = Lens.field @"ipv6Address"
{-# INLINEABLE niiaIpv6Address #-}
{-# DEPRECATED ipv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead"  #-}

instance Core.FromXML NetworkInterfaceIpv6Address where
        parseXML x
          = NetworkInterfaceIpv6Address' Core.<$> (x Core..@? "ipv6Address")
