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
-- Module      : Amazonka.Glue.Types.Order
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Order where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON Order where
  parseJSON =
    Data.withObject
      "Order"
      ( \x ->
          Order'
            Prelude.<$> (x Data..: "Column")
            Prelude.<*> (x Data..: "SortOrder")
      )

instance Prelude.Hashable Order where
  hashWithSalt _salt Order' {..} =
    _salt
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData Order where
  rnf Order' {..} =
    Prelude.rnf column
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON Order where
  toJSON Order' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Data..= column),
            Prelude.Just ("SortOrder" Data..= sortOrder)
          ]
      )
