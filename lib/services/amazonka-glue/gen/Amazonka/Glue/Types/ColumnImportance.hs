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
-- Module      : Amazonka.Glue.Types.ColumnImportance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ColumnImportance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the column name and column importance score for a
-- column.
--
-- Column importance helps you understand how columns contribute to your
-- model, by identifying which columns in your records are more important
-- than others.
--
-- /See:/ 'newColumnImportance' smart constructor.
data ColumnImportance = ColumnImportance'
  { -- | The name of a column.
    columnName :: Prelude.Maybe Prelude.Text,
    -- | The column importance score for the column, as a decimal.
    importance :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnImportance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnName', 'columnImportance_columnName' - The name of a column.
--
-- 'importance', 'columnImportance_importance' - The column importance score for the column, as a decimal.
newColumnImportance ::
  ColumnImportance
newColumnImportance =
  ColumnImportance'
    { columnName = Prelude.Nothing,
      importance = Prelude.Nothing
    }

-- | The name of a column.
columnImportance_columnName :: Lens.Lens' ColumnImportance (Prelude.Maybe Prelude.Text)
columnImportance_columnName = Lens.lens (\ColumnImportance' {columnName} -> columnName) (\s@ColumnImportance' {} a -> s {columnName = a} :: ColumnImportance)

-- | The column importance score for the column, as a decimal.
columnImportance_importance :: Lens.Lens' ColumnImportance (Prelude.Maybe Prelude.Double)
columnImportance_importance = Lens.lens (\ColumnImportance' {importance} -> importance) (\s@ColumnImportance' {} a -> s {importance = a} :: ColumnImportance)

instance Data.FromJSON ColumnImportance where
  parseJSON =
    Data.withObject
      "ColumnImportance"
      ( \x ->
          ColumnImportance'
            Prelude.<$> (x Data..:? "ColumnName")
            Prelude.<*> (x Data..:? "Importance")
      )

instance Prelude.Hashable ColumnImportance where
  hashWithSalt _salt ColumnImportance' {..} =
    _salt `Prelude.hashWithSalt` columnName
      `Prelude.hashWithSalt` importance

instance Prelude.NFData ColumnImportance where
  rnf ColumnImportance' {..} =
    Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf importance
