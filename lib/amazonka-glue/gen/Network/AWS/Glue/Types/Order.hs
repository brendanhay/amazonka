{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Order
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Order
  ( Order (..),

    -- * Smart constructor
    mkOrder,

    -- * Lenses
    oColumn,
    oSortOrder,
  )
where

import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the sort order of a sorted column.
--
-- /See:/ 'mkOrder' smart constructor.
data Order = Order'
  { -- | The name of the column.
    column :: Types.NameString,
    -- | Indicates that the column is sorted in ascending order (@== 1@ ), or in descending order (@==0@ ).
    sortOrder :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Order' value with any optional fields omitted.
mkOrder ::
  -- | 'column'
  Types.NameString ->
  -- | 'sortOrder'
  Core.Natural ->
  Order
mkOrder column sortOrder = Order' {column, sortOrder}

-- | The name of the column.
--
-- /Note:/ Consider using 'column' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oColumn :: Lens.Lens' Order Types.NameString
oColumn = Lens.field @"column"
{-# DEPRECATED oColumn "Use generic-lens or generic-optics with 'column' instead." #-}

-- | Indicates that the column is sorted in ascending order (@== 1@ ), or in descending order (@==0@ ).
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oSortOrder :: Lens.Lens' Order Core.Natural
oSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED oSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON Order where
  toJSON Order {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Column" Core..= column),
            Core.Just ("SortOrder" Core..= sortOrder)
          ]
      )

instance Core.FromJSON Order where
  parseJSON =
    Core.withObject "Order" Core.$
      \x ->
        Order'
          Core.<$> (x Core..: "Column") Core.<*> (x Core..: "SortOrder")
