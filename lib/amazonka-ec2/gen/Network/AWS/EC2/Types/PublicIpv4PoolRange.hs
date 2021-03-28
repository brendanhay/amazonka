{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PublicIpv4PoolRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PublicIpv4PoolRange
  ( PublicIpv4PoolRange (..)
  -- * Smart constructor
  , mkPublicIpv4PoolRange
  -- * Lenses
  , piprAddressCount
  , piprAvailableAddressCount
  , piprFirstAddress
  , piprLastAddress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an address range of an IPv4 address pool.
--
-- /See:/ 'mkPublicIpv4PoolRange' smart constructor.
data PublicIpv4PoolRange = PublicIpv4PoolRange'
  { addressCount :: Core.Maybe Core.Int
    -- ^ The number of addresses in the range.
  , availableAddressCount :: Core.Maybe Core.Int
    -- ^ The number of available addresses in the range.
  , firstAddress :: Core.Maybe Core.Text
    -- ^ The first IP address in the range.
  , lastAddress :: Core.Maybe Core.Text
    -- ^ The last IP address in the range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublicIpv4PoolRange' value with any optional fields omitted.
mkPublicIpv4PoolRange
    :: PublicIpv4PoolRange
mkPublicIpv4PoolRange
  = PublicIpv4PoolRange'{addressCount = Core.Nothing,
                         availableAddressCount = Core.Nothing, firstAddress = Core.Nothing,
                         lastAddress = Core.Nothing}

-- | The number of addresses in the range.
--
-- /Note:/ Consider using 'addressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprAddressCount :: Lens.Lens' PublicIpv4PoolRange (Core.Maybe Core.Int)
piprAddressCount = Lens.field @"addressCount"
{-# INLINEABLE piprAddressCount #-}
{-# DEPRECATED addressCount "Use generic-lens or generic-optics with 'addressCount' instead"  #-}

-- | The number of available addresses in the range.
--
-- /Note:/ Consider using 'availableAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprAvailableAddressCount :: Lens.Lens' PublicIpv4PoolRange (Core.Maybe Core.Int)
piprAvailableAddressCount = Lens.field @"availableAddressCount"
{-# INLINEABLE piprAvailableAddressCount #-}
{-# DEPRECATED availableAddressCount "Use generic-lens or generic-optics with 'availableAddressCount' instead"  #-}

-- | The first IP address in the range.
--
-- /Note:/ Consider using 'firstAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprFirstAddress :: Lens.Lens' PublicIpv4PoolRange (Core.Maybe Core.Text)
piprFirstAddress = Lens.field @"firstAddress"
{-# INLINEABLE piprFirstAddress #-}
{-# DEPRECATED firstAddress "Use generic-lens or generic-optics with 'firstAddress' instead"  #-}

-- | The last IP address in the range.
--
-- /Note:/ Consider using 'lastAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprLastAddress :: Lens.Lens' PublicIpv4PoolRange (Core.Maybe Core.Text)
piprLastAddress = Lens.field @"lastAddress"
{-# INLINEABLE piprLastAddress #-}
{-# DEPRECATED lastAddress "Use generic-lens or generic-optics with 'lastAddress' instead"  #-}

instance Core.FromXML PublicIpv4PoolRange where
        parseXML x
          = PublicIpv4PoolRange' Core.<$>
              (x Core..@? "addressCount") Core.<*>
                x Core..@? "availableAddressCount"
                Core.<*> x Core..@? "firstAddress"
                Core.<*> x Core..@? "lastAddress"
