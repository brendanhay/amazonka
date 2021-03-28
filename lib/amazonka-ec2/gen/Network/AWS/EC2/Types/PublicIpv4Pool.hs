{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PublicIpv4Pool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PublicIpv4Pool
  ( PublicIpv4Pool (..)
  -- * Smart constructor
  , mkPublicIpv4Pool
  -- * Lenses
  , pipDescription
  , pipNetworkBorderGroup
  , pipPoolAddressRanges
  , pipPoolId
  , pipTags
  , pipTotalAddressCount
  , pipTotalAvailableAddressCount
  ) where

import qualified Network.AWS.EC2.Types.PublicIpv4PoolRange as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv4 address pool.
--
-- /See:/ 'mkPublicIpv4Pool' smart constructor.
data PublicIpv4Pool = PublicIpv4Pool'
  { description :: Core.Maybe Core.Text
    -- ^ A description of the address pool.
  , networkBorderGroup :: Core.Maybe Core.Text
    -- ^ The name of the location from which the address pool is advertised. A network border group is a unique set of Availability Zones or Local Zones from where AWS advertises public IP addresses.
  , poolAddressRanges :: Core.Maybe [Types.PublicIpv4PoolRange]
    -- ^ The address ranges.
  , poolId :: Core.Maybe Core.Text
    -- ^ The ID of the address pool.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags for the address pool.
  , totalAddressCount :: Core.Maybe Core.Int
    -- ^ The total number of addresses.
  , totalAvailableAddressCount :: Core.Maybe Core.Int
    -- ^ The total number of available addresses.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublicIpv4Pool' value with any optional fields omitted.
mkPublicIpv4Pool
    :: PublicIpv4Pool
mkPublicIpv4Pool
  = PublicIpv4Pool'{description = Core.Nothing,
                    networkBorderGroup = Core.Nothing,
                    poolAddressRanges = Core.Nothing, poolId = Core.Nothing,
                    tags = Core.Nothing, totalAddressCount = Core.Nothing,
                    totalAvailableAddressCount = Core.Nothing}

-- | A description of the address pool.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipDescription :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Text)
pipDescription = Lens.field @"description"
{-# INLINEABLE pipDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the location from which the address pool is advertised. A network border group is a unique set of Availability Zones or Local Zones from where AWS advertises public IP addresses.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipNetworkBorderGroup :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Text)
pipNetworkBorderGroup = Lens.field @"networkBorderGroup"
{-# INLINEABLE pipNetworkBorderGroup #-}
{-# DEPRECATED networkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead"  #-}

-- | The address ranges.
--
-- /Note:/ Consider using 'poolAddressRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPoolAddressRanges :: Lens.Lens' PublicIpv4Pool (Core.Maybe [Types.PublicIpv4PoolRange])
pipPoolAddressRanges = Lens.field @"poolAddressRanges"
{-# INLINEABLE pipPoolAddressRanges #-}
{-# DEPRECATED poolAddressRanges "Use generic-lens or generic-optics with 'poolAddressRanges' instead"  #-}

-- | The ID of the address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPoolId :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Text)
pipPoolId = Lens.field @"poolId"
{-# INLINEABLE pipPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | Any tags for the address pool.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipTags :: Lens.Lens' PublicIpv4Pool (Core.Maybe [Types.Tag])
pipTags = Lens.field @"tags"
{-# INLINEABLE pipTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The total number of addresses.
--
-- /Note:/ Consider using 'totalAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipTotalAddressCount :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Int)
pipTotalAddressCount = Lens.field @"totalAddressCount"
{-# INLINEABLE pipTotalAddressCount #-}
{-# DEPRECATED totalAddressCount "Use generic-lens or generic-optics with 'totalAddressCount' instead"  #-}

-- | The total number of available addresses.
--
-- /Note:/ Consider using 'totalAvailableAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipTotalAvailableAddressCount :: Lens.Lens' PublicIpv4Pool (Core.Maybe Core.Int)
pipTotalAvailableAddressCount = Lens.field @"totalAvailableAddressCount"
{-# INLINEABLE pipTotalAvailableAddressCount #-}
{-# DEPRECATED totalAvailableAddressCount "Use generic-lens or generic-optics with 'totalAvailableAddressCount' instead"  #-}

instance Core.FromXML PublicIpv4Pool where
        parseXML x
          = PublicIpv4Pool' Core.<$>
              (x Core..@? "description") Core.<*> x Core..@? "networkBorderGroup"
                Core.<*>
                x Core..@? "poolAddressRangeSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "poolId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "totalAddressCount"
                Core.<*> x Core..@? "totalAvailableAddressCount"
