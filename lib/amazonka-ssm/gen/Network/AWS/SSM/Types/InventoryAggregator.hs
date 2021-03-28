{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryAggregator
  ( InventoryAggregator (..)
  -- * Smart constructor
  , mkInventoryAggregator
  -- * Lenses
  , iaAggregators
  , iaExpression
  , iaGroups
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InventoryAggregatorExpression as Types
import qualified Network.AWS.SSM.Types.InventoryGroup as Types

-- | Specifies the inventory type and attribute for the aggregation execution.
--
-- /See:/ 'mkInventoryAggregator' smart constructor.
data InventoryAggregator = InventoryAggregator'
  { aggregators :: Core.Maybe (Core.NonEmpty InventoryAggregator)
    -- ^ Nested aggregators to further refine aggregation for an inventory type.
  , expression :: Core.Maybe Types.InventoryAggregatorExpression
    -- ^ The inventory type and attribute name for aggregation.
  , groups :: Core.Maybe (Core.NonEmpty Types.InventoryGroup)
    -- ^ A user-defined set of one or more filters on which to aggregate inventory data. Groups return a count of resources that match and don't match the specified criteria.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryAggregator' value with any optional fields omitted.
mkInventoryAggregator
    :: InventoryAggregator
mkInventoryAggregator
  = InventoryAggregator'{aggregators = Core.Nothing,
                         expression = Core.Nothing, groups = Core.Nothing}

-- | Nested aggregators to further refine aggregation for an inventory type.
--
-- /Note:/ Consider using 'aggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAggregators :: Lens.Lens' InventoryAggregator (Core.Maybe (Core.NonEmpty InventoryAggregator))
iaAggregators = Lens.field @"aggregators"
{-# INLINEABLE iaAggregators #-}
{-# DEPRECATED aggregators "Use generic-lens or generic-optics with 'aggregators' instead"  #-}

-- | The inventory type and attribute name for aggregation.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaExpression :: Lens.Lens' InventoryAggregator (Core.Maybe Types.InventoryAggregatorExpression)
iaExpression = Lens.field @"expression"
{-# INLINEABLE iaExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

-- | A user-defined set of one or more filters on which to aggregate inventory data. Groups return a count of resources that match and don't match the specified criteria.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaGroups :: Lens.Lens' InventoryAggregator (Core.Maybe (Core.NonEmpty Types.InventoryGroup))
iaGroups = Lens.field @"groups"
{-# INLINEABLE iaGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

instance Core.FromJSON InventoryAggregator where
        toJSON InventoryAggregator{..}
          = Core.object
              (Core.catMaybes
                 [("Aggregators" Core..=) Core.<$> aggregators,
                  ("Expression" Core..=) Core.<$> expression,
                  ("Groups" Core..=) Core.<$> groups])
