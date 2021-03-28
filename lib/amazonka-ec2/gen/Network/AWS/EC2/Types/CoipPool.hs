{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CoipPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CoipPool
  ( CoipPool (..)
  -- * Smart constructor
  , mkCoipPool
  -- * Lenses
  , cpLocalGatewayRouteTableId
  , cpPoolArn
  , cpPoolCidrs
  , cpPoolId
  , cpTags
  ) where

import qualified Network.AWS.EC2.Types.LocalGatewayRouteTableId as Types
import qualified Network.AWS.EC2.Types.PoolArn as Types
import qualified Network.AWS.EC2.Types.PoolId as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a customer-owned address pool.
--
-- /See:/ 'mkCoipPool' smart constructor.
data CoipPool = CoipPool'
  { localGatewayRouteTableId :: Core.Maybe Types.LocalGatewayRouteTableId
    -- ^ The ID of the local gateway route table.
  , poolArn :: Core.Maybe Types.PoolArn
    -- ^ The ARN of the address pool.
  , poolCidrs :: Core.Maybe [Core.Text]
    -- ^ The address ranges of the address pool.
  , poolId :: Core.Maybe Types.PoolId
    -- ^ The ID of the address pool.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CoipPool' value with any optional fields omitted.
mkCoipPool
    :: CoipPool
mkCoipPool
  = CoipPool'{localGatewayRouteTableId = Core.Nothing,
              poolArn = Core.Nothing, poolCidrs = Core.Nothing,
              poolId = Core.Nothing, tags = Core.Nothing}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLocalGatewayRouteTableId :: Lens.Lens' CoipPool (Core.Maybe Types.LocalGatewayRouteTableId)
cpLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# INLINEABLE cpLocalGatewayRouteTableId #-}
{-# DEPRECATED localGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead"  #-}

-- | The ARN of the address pool.
--
-- /Note:/ Consider using 'poolArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPoolArn :: Lens.Lens' CoipPool (Core.Maybe Types.PoolArn)
cpPoolArn = Lens.field @"poolArn"
{-# INLINEABLE cpPoolArn #-}
{-# DEPRECATED poolArn "Use generic-lens or generic-optics with 'poolArn' instead"  #-}

-- | The address ranges of the address pool.
--
-- /Note:/ Consider using 'poolCidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPoolCidrs :: Lens.Lens' CoipPool (Core.Maybe [Core.Text])
cpPoolCidrs = Lens.field @"poolCidrs"
{-# INLINEABLE cpPoolCidrs #-}
{-# DEPRECATED poolCidrs "Use generic-lens or generic-optics with 'poolCidrs' instead"  #-}

-- | The ID of the address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPoolId :: Lens.Lens' CoipPool (Core.Maybe Types.PoolId)
cpPoolId = Lens.field @"poolId"
{-# INLINEABLE cpPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CoipPool (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# INLINEABLE cpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML CoipPool where
        parseXML x
          = CoipPool' Core.<$>
              (x Core..@? "localGatewayRouteTableId") Core.<*>
                x Core..@? "poolArn"
                Core.<*>
                x Core..@? "poolCidrSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "poolId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
