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
-- Module      : Amazonka.QuickSight.Types.TooltipItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TooltipItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnTooltipItem
import Amazonka.QuickSight.Types.FieldTooltipItem

-- | The tooltip.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newTooltipItem' smart constructor.
data TooltipItem = TooltipItem'
  { -- | The tooltip item for the columns that are not part of a field well.
    columnTooltipItem :: Prelude.Maybe ColumnTooltipItem,
    -- | The tooltip item for the fields.
    fieldTooltipItem :: Prelude.Maybe FieldTooltipItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TooltipItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnTooltipItem', 'tooltipItem_columnTooltipItem' - The tooltip item for the columns that are not part of a field well.
--
-- 'fieldTooltipItem', 'tooltipItem_fieldTooltipItem' - The tooltip item for the fields.
newTooltipItem ::
  TooltipItem
newTooltipItem =
  TooltipItem'
    { columnTooltipItem = Prelude.Nothing,
      fieldTooltipItem = Prelude.Nothing
    }

-- | The tooltip item for the columns that are not part of a field well.
tooltipItem_columnTooltipItem :: Lens.Lens' TooltipItem (Prelude.Maybe ColumnTooltipItem)
tooltipItem_columnTooltipItem = Lens.lens (\TooltipItem' {columnTooltipItem} -> columnTooltipItem) (\s@TooltipItem' {} a -> s {columnTooltipItem = a} :: TooltipItem)

-- | The tooltip item for the fields.
tooltipItem_fieldTooltipItem :: Lens.Lens' TooltipItem (Prelude.Maybe FieldTooltipItem)
tooltipItem_fieldTooltipItem = Lens.lens (\TooltipItem' {fieldTooltipItem} -> fieldTooltipItem) (\s@TooltipItem' {} a -> s {fieldTooltipItem = a} :: TooltipItem)

instance Data.FromJSON TooltipItem where
  parseJSON =
    Data.withObject
      "TooltipItem"
      ( \x ->
          TooltipItem'
            Prelude.<$> (x Data..:? "ColumnTooltipItem")
            Prelude.<*> (x Data..:? "FieldTooltipItem")
      )

instance Prelude.Hashable TooltipItem where
  hashWithSalt _salt TooltipItem' {..} =
    _salt
      `Prelude.hashWithSalt` columnTooltipItem
      `Prelude.hashWithSalt` fieldTooltipItem

instance Prelude.NFData TooltipItem where
  rnf TooltipItem' {..} =
    Prelude.rnf columnTooltipItem
      `Prelude.seq` Prelude.rnf fieldTooltipItem

instance Data.ToJSON TooltipItem where
  toJSON TooltipItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnTooltipItem" Data..=)
              Prelude.<$> columnTooltipItem,
            ("FieldTooltipItem" Data..=)
              Prelude.<$> fieldTooltipItem
          ]
      )
