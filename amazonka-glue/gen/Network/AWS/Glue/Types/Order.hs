{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Order
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Order where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the sort order of a sorted column.
--
-- /See:/ 'newOrder' smart constructor.
data Order = Order'
  { -- | The name of the column.
    column :: Core.Text,
    -- | Indicates that the column is sorted in ascending order (@== 1@), or in
    -- descending order (@==0@).
    sortOrder :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Order' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'order_column' - The name of the column.
--
-- 'sortOrder', 'order_sortOrder' - Indicates that the column is sorted in ascending order (@== 1@), or in
-- descending order (@==0@).
newOrder ::
  -- | 'column'
  Core.Text ->
  -- | 'sortOrder'
  Core.Natural ->
  Order
newOrder pColumn_ pSortOrder_ =
  Order' {column = pColumn_, sortOrder = pSortOrder_}

-- | The name of the column.
order_column :: Lens.Lens' Order Core.Text
order_column = Lens.lens (\Order' {column} -> column) (\s@Order' {} a -> s {column = a} :: Order)

-- | Indicates that the column is sorted in ascending order (@== 1@), or in
-- descending order (@==0@).
order_sortOrder :: Lens.Lens' Order Core.Natural
order_sortOrder = Lens.lens (\Order' {sortOrder} -> sortOrder) (\s@Order' {} a -> s {sortOrder = a} :: Order)

instance Core.FromJSON Order where
  parseJSON =
    Core.withObject
      "Order"
      ( \x ->
          Order'
            Core.<$> (x Core..: "Column")
            Core.<*> (x Core..: "SortOrder")
      )

instance Core.Hashable Order

instance Core.NFData Order

instance Core.ToJSON Order where
  toJSON Order' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Column" Core..= column),
            Core.Just ("SortOrder" Core..= sortOrder)
          ]
      )
