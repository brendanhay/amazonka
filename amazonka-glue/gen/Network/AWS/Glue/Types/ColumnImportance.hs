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
-- Module      : Network.AWS.Glue.Types.ColumnImportance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnImportance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure containing the column name and column importance score for a
-- column.
--
-- Column importance helps you understand how columns contribute to your
-- model, by identifying which columns in your records are more important
-- than others.
--
-- /See:/ 'newColumnImportance' smart constructor.
data ColumnImportance = ColumnImportance'
  { -- | The column importance score for the column, as a decimal.
    importance :: Core.Maybe Core.Double,
    -- | The name of a column.
    columnName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ColumnImportance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importance', 'columnImportance_importance' - The column importance score for the column, as a decimal.
--
-- 'columnName', 'columnImportance_columnName' - The name of a column.
newColumnImportance ::
  ColumnImportance
newColumnImportance =
  ColumnImportance'
    { importance = Core.Nothing,
      columnName = Core.Nothing
    }

-- | The column importance score for the column, as a decimal.
columnImportance_importance :: Lens.Lens' ColumnImportance (Core.Maybe Core.Double)
columnImportance_importance = Lens.lens (\ColumnImportance' {importance} -> importance) (\s@ColumnImportance' {} a -> s {importance = a} :: ColumnImportance)

-- | The name of a column.
columnImportance_columnName :: Lens.Lens' ColumnImportance (Core.Maybe Core.Text)
columnImportance_columnName = Lens.lens (\ColumnImportance' {columnName} -> columnName) (\s@ColumnImportance' {} a -> s {columnName = a} :: ColumnImportance)

instance Core.FromJSON ColumnImportance where
  parseJSON =
    Core.withObject
      "ColumnImportance"
      ( \x ->
          ColumnImportance'
            Core.<$> (x Core..:? "Importance")
            Core.<*> (x Core..:? "ColumnName")
      )

instance Core.Hashable ColumnImportance

instance Core.NFData ColumnImportance
