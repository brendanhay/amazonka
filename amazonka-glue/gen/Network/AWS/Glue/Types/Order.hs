{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the sort order of a sorted column.
--
-- /See:/ 'newOrder' smart constructor.
data Order = Order'
  { -- | The name of the column.
    column :: Prelude.Text,
    -- | Indicates that the column is sorted in ascending order (@== 1@), or in
    -- descending order (@==0@).
    sortOrder :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'sortOrder'
  Prelude.Natural ->
  Order
newOrder pColumn_ pSortOrder_ =
  Order' {column = pColumn_, sortOrder = pSortOrder_}

-- | The name of the column.
order_column :: Lens.Lens' Order Prelude.Text
order_column = Lens.lens (\Order' {column} -> column) (\s@Order' {} a -> s {column = a} :: Order)

-- | Indicates that the column is sorted in ascending order (@== 1@), or in
-- descending order (@==0@).
order_sortOrder :: Lens.Lens' Order Prelude.Natural
order_sortOrder = Lens.lens (\Order' {sortOrder} -> sortOrder) (\s@Order' {} a -> s {sortOrder = a} :: Order)

instance Prelude.FromJSON Order where
  parseJSON =
    Prelude.withObject
      "Order"
      ( \x ->
          Order'
            Prelude.<$> (x Prelude..: "Column")
            Prelude.<*> (x Prelude..: "SortOrder")
      )

instance Prelude.Hashable Order

instance Prelude.NFData Order

instance Prelude.ToJSON Order where
  toJSON Order' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Prelude..= column),
            Prelude.Just ("SortOrder" Prelude..= sortOrder)
          ]
      )
