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
-- Module      : Amazonka.QuickSight.Types.ColumnLevelPermissionRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnLevelPermissionRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A rule defined to grant access on one or more restricted columns. Each
-- dataset can have multiple rules. To create a restricted column, you add
-- it to one or more rules. Each rule must contain at least one column and
-- at least one user or group. To be able to see a restricted column, a
-- user or group needs to be added to a rule for that column.
--
-- /See:/ 'newColumnLevelPermissionRule' smart constructor.
data ColumnLevelPermissionRule = ColumnLevelPermissionRule'
  { -- | An array of column names.
    columnNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An array of Amazon Resource Names (ARNs) for Amazon QuickSight users or
    -- groups.
    principals :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnLevelPermissionRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnNames', 'columnLevelPermissionRule_columnNames' - An array of column names.
--
-- 'principals', 'columnLevelPermissionRule_principals' - An array of Amazon Resource Names (ARNs) for Amazon QuickSight users or
-- groups.
newColumnLevelPermissionRule ::
  ColumnLevelPermissionRule
newColumnLevelPermissionRule =
  ColumnLevelPermissionRule'
    { columnNames =
        Prelude.Nothing,
      principals = Prelude.Nothing
    }

-- | An array of column names.
columnLevelPermissionRule_columnNames :: Lens.Lens' ColumnLevelPermissionRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
columnLevelPermissionRule_columnNames = Lens.lens (\ColumnLevelPermissionRule' {columnNames} -> columnNames) (\s@ColumnLevelPermissionRule' {} a -> s {columnNames = a} :: ColumnLevelPermissionRule) Prelude.. Lens.mapping Lens.coerced

-- | An array of Amazon Resource Names (ARNs) for Amazon QuickSight users or
-- groups.
columnLevelPermissionRule_principals :: Lens.Lens' ColumnLevelPermissionRule (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
columnLevelPermissionRule_principals = Lens.lens (\ColumnLevelPermissionRule' {principals} -> principals) (\s@ColumnLevelPermissionRule' {} a -> s {principals = a} :: ColumnLevelPermissionRule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ColumnLevelPermissionRule where
  parseJSON =
    Data.withObject
      "ColumnLevelPermissionRule"
      ( \x ->
          ColumnLevelPermissionRule'
            Prelude.<$> (x Data..:? "ColumnNames")
            Prelude.<*> (x Data..:? "Principals")
      )

instance Prelude.Hashable ColumnLevelPermissionRule where
  hashWithSalt _salt ColumnLevelPermissionRule' {..} =
    _salt `Prelude.hashWithSalt` columnNames
      `Prelude.hashWithSalt` principals

instance Prelude.NFData ColumnLevelPermissionRule where
  rnf ColumnLevelPermissionRule' {..} =
    Prelude.rnf columnNames
      `Prelude.seq` Prelude.rnf principals

instance Data.ToJSON ColumnLevelPermissionRule where
  toJSON ColumnLevelPermissionRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnNames" Data..=) Prelude.<$> columnNames,
            ("Principals" Data..=) Prelude.<$> principals
          ]
      )
