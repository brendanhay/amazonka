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
    oSortOrder,
    oColumn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the sort order of a sorted column.
--
-- /See:/ 'mkOrder' smart constructor.
data Order = Order'
  { -- | Indicates that the column is sorted in ascending order (@== 1@ ), or in descending order (@==0@ ).
    sortOrder :: Lude.Natural,
    -- | The name of the column.
    column :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Order' with the minimum fields required to make a request.
--
-- * 'sortOrder' - Indicates that the column is sorted in ascending order (@== 1@ ), or in descending order (@==0@ ).
-- * 'column' - The name of the column.
mkOrder ::
  -- | 'sortOrder'
  Lude.Natural ->
  -- | 'column'
  Lude.Text ->
  Order
mkOrder pSortOrder_ pColumn_ =
  Order' {sortOrder = pSortOrder_, column = pColumn_}

-- | Indicates that the column is sorted in ascending order (@== 1@ ), or in descending order (@==0@ ).
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oSortOrder :: Lens.Lens' Order Lude.Natural
oSortOrder = Lens.lens (sortOrder :: Order -> Lude.Natural) (\s a -> s {sortOrder = a} :: Order)
{-# DEPRECATED oSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The name of the column.
--
-- /Note:/ Consider using 'column' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oColumn :: Lens.Lens' Order Lude.Text
oColumn = Lens.lens (column :: Order -> Lude.Text) (\s a -> s {column = a} :: Order)
{-# DEPRECATED oColumn "Use generic-lens or generic-optics with 'column' instead." #-}

instance Lude.FromJSON Order where
  parseJSON =
    Lude.withObject
      "Order"
      ( \x ->
          Order'
            Lude.<$> (x Lude..: "SortOrder") Lude.<*> (x Lude..: "Column")
      )

instance Lude.ToJSON Order where
  toJSON Order' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SortOrder" Lude..= sortOrder),
            Lude.Just ("Column" Lude..= column)
          ]
      )
