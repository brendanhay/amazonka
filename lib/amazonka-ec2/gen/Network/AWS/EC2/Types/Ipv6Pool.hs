{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Ipv6Pool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Ipv6Pool
  ( Ipv6Pool (..)
  -- * Smart constructor
  , mkIpv6Pool
  -- * Lenses
  , ipDescription
  , ipPoolCidrBlocks
  , ipPoolId
  , ipTags
  ) where

import qualified Network.AWS.EC2.Types.PoolCidrBlock as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an IPv6 address pool.
--
-- /See:/ 'mkIpv6Pool' smart constructor.
data Ipv6Pool = Ipv6Pool'
  { description :: Core.Maybe Core.Text
    -- ^ The description for the address pool.
  , poolCidrBlocks :: Core.Maybe [Types.PoolCidrBlock]
    -- ^ The CIDR blocks for the address pool.
  , poolId :: Core.Maybe Core.Text
    -- ^ The ID of the address pool.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags for the address pool.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ipv6Pool' value with any optional fields omitted.
mkIpv6Pool
    :: Ipv6Pool
mkIpv6Pool
  = Ipv6Pool'{description = Core.Nothing,
              poolCidrBlocks = Core.Nothing, poolId = Core.Nothing,
              tags = Core.Nothing}

-- | The description for the address pool.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipDescription :: Lens.Lens' Ipv6Pool (Core.Maybe Core.Text)
ipDescription = Lens.field @"description"
{-# INLINEABLE ipDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The CIDR blocks for the address pool.
--
-- /Note:/ Consider using 'poolCidrBlocks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipPoolCidrBlocks :: Lens.Lens' Ipv6Pool (Core.Maybe [Types.PoolCidrBlock])
ipPoolCidrBlocks = Lens.field @"poolCidrBlocks"
{-# INLINEABLE ipPoolCidrBlocks #-}
{-# DEPRECATED poolCidrBlocks "Use generic-lens or generic-optics with 'poolCidrBlocks' instead"  #-}

-- | The ID of the address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipPoolId :: Lens.Lens' Ipv6Pool (Core.Maybe Core.Text)
ipPoolId = Lens.field @"poolId"
{-# INLINEABLE ipPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | Any tags for the address pool.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipTags :: Lens.Lens' Ipv6Pool (Core.Maybe [Types.Tag])
ipTags = Lens.field @"tags"
{-# INLINEABLE ipTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML Ipv6Pool where
        parseXML x
          = Ipv6Pool' Core.<$>
              (x Core..@? "description") Core.<*>
                x Core..@? "poolCidrBlockSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "poolId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
