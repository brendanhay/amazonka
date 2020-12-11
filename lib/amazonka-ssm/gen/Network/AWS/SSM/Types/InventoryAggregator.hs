-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryAggregator
  ( InventoryAggregator (..),

    -- * Smart constructor
    mkInventoryAggregator,

    -- * Lenses
    iaGroups,
    iaAggregators,
    iaExpression,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InventoryGroup

-- | Specifies the inventory type and attribute for the aggregation execution.
--
-- /See:/ 'mkInventoryAggregator' smart constructor.
data InventoryAggregator = InventoryAggregator'
  { groups ::
      Lude.Maybe (Lude.NonEmpty InventoryGroup),
    aggregators ::
      Lude.Maybe (Lude.NonEmpty InventoryAggregator),
    expression :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryAggregator' with the minimum fields required to make a request.
--
-- * 'aggregators' - Nested aggregators to further refine aggregation for an inventory type.
-- * 'expression' - The inventory type and attribute name for aggregation.
-- * 'groups' - A user-defined set of one or more filters on which to aggregate inventory data. Groups return a count of resources that match and don't match the specified criteria.
mkInventoryAggregator ::
  InventoryAggregator
mkInventoryAggregator =
  InventoryAggregator'
    { groups = Lude.Nothing,
      aggregators = Lude.Nothing,
      expression = Lude.Nothing
    }

-- | A user-defined set of one or more filters on which to aggregate inventory data. Groups return a count of resources that match and don't match the specified criteria.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaGroups :: Lens.Lens' InventoryAggregator (Lude.Maybe (Lude.NonEmpty InventoryGroup))
iaGroups = Lens.lens (groups :: InventoryAggregator -> Lude.Maybe (Lude.NonEmpty InventoryGroup)) (\s a -> s {groups = a} :: InventoryAggregator)
{-# DEPRECATED iaGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Nested aggregators to further refine aggregation for an inventory type.
--
-- /Note:/ Consider using 'aggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaAggregators :: Lens.Lens' InventoryAggregator (Lude.Maybe (Lude.NonEmpty InventoryAggregator))
iaAggregators = Lens.lens (aggregators :: InventoryAggregator -> Lude.Maybe (Lude.NonEmpty InventoryAggregator)) (\s a -> s {aggregators = a} :: InventoryAggregator)
{-# DEPRECATED iaAggregators "Use generic-lens or generic-optics with 'aggregators' instead." #-}

-- | The inventory type and attribute name for aggregation.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaExpression :: Lens.Lens' InventoryAggregator (Lude.Maybe Lude.Text)
iaExpression = Lens.lens (expression :: InventoryAggregator -> Lude.Maybe Lude.Text) (\s a -> s {expression = a} :: InventoryAggregator)
{-# DEPRECATED iaExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

instance Lude.ToJSON InventoryAggregator where
  toJSON InventoryAggregator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Groups" Lude..=) Lude.<$> groups,
            ("Aggregators" Lude..=) Lude.<$> aggregators,
            ("Expression" Lude..=) Lude.<$> expression
          ]
      )
